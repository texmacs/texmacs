
/******************************************************************************
* MODULE     : QTMsockets.cpp
* DESCRIPTION: QT TeXmacs sockets manager
* COPYRIGHT  : (C) 2015 Denis RAUX
*                  2022 Gregoire LECERF
*                  2025 Robin WILS
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMSockets.hpp"
#include "scheme.hpp"
#include "iterator.hpp"
#include "analyze.hpp"
#include "server_log.hpp"
#include "gnutls.hpp"
#include <cctype>

#if defined(OS_MACOS)
  #include "MacOS/mac_utilities.h"
#endif

#ifndef OS_MINGW

#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdint.h>
#include <fcntl.h>
#include <arpa/inet.h>

#define CONNECT ::connect
#define CLOSE(a) close(a)
#define WRITE(a, b, c) ::write(a, b, c)
#define ERRNO errno
#define ERRSOC(a) a 
#define GETADDRINFO getaddrinfo
#define FREEADDRINFO freeaddrinfo
#define ADDRINFO addrinfo
#define SOCKET socket
#define GAI_STRERROR gai_strerror
#define BIND bind
#define LISTEN listen
#define ACCEPT accept
#define INET_NTOP inet_ntop
#define TM_FD_SET(fd, set) FD_SET(fd, set)

#else

#define CONNECT wsoc::connect
#define CLOSE(a) wsoc::closesocket(a)
#define WRITE(a, b, c) wsoc::send(a, b, c, 0) 
#define ERRNO wsoc::WSAGetLastError()
#define ERRSOC(a) WSA##a 
#define GETADDRINFO wsoc::getaddrinfo 
#define FREEADDRINFO wsoc::freeaddrinfo
#define ADDRINFO wsoc::addrinfo
#define SOCKET wsoc::socket
#define GAI_STRERROR wsoc::gai_strerrorA
#define BIND wsoc::bind
#define LISTEN wsoc::listen
#define ACCEPT wsoc::accept
#define INET_NTOP wsoc::inet_ntop
#undef FD_ISSET
#define FD_ISSET __WSAFDIsSet
#define TM_FD_SET(fd, set) FD_SET((u_int) fd, set)

#endif

/******************************************************************************
* Utilities
******************************************************************************/

static string
string_from_socket_address (SOCKADDR_STORAGE* sock) {
  static char tmp[128];
  if (sock->ss_family == AF_INET) {
#ifdef OS_MINGW
    return wsoc::inet_ntoa (((SOCKADDR_IN*) sock)->sin_addr);
#else
    if (inet_ntop (AF_INET, &(((sockaddr_in*) sock)->sin_addr),
        tmp, sizeof(tmp)) == NULL)
      return "";
    return tmp;
#endif
  }
  if (sock->ss_family == AF_INET6) {
#if !defined (OS_MINGW) || (_WIN32_WINNT >= 0x0600)
    if (INET_NTOP (AF_INET6, &(((SOCKADDR_IN6*) sock)->sin6_addr),
        tmp, sizeof(tmp)) == NULL)
      return "";
#else
    return "";
#endif
    return string ("[") * tmp * string ("]");
  }
  return "";
}

string
socket_debug_io_string (string s, int max) {
  int i, n= N(s);
  string r;
  for (i=0; i<n && i<max; i++) {
    unsigned char c= (unsigned char) s[i];
    if (c == DATA_BEGIN) r << "[BEGIN]";
    else if (c == DATA_END) r << "[END]";
    else if (c == DATA_COMMAND) r << "[COMMAND]";
    else if (c == DATA_ESCAPE) r << "[ESCAPE]";
    else if (std::isprint(c) || std::isspace(c)) r << s[i];
    else r << "\\x" << as_hexadecimal(c);
  }
  if (n > max) {
    r << "... (" << as_string (n) << " bytes truncated to "
      << as_string (max) << ")";
  }
  return r;
}

unsigned long long int qtmsocket_debug_counter= 0;

static int
safe_server_close (int fd) {
  int ret= CLOSE (fd);
  if (ret != 0) {
    server_log_write (log_error, string ("server socket, close error: ")
                      * strerror (errno));
    server_log_write (log_error, "cannot close server socket. Please exit TeXmacs in order to stop the server completely");
    io_error << "cannot close server socket. Please exit TeXmacs in order to stop the server completely" << LF;
  }
  return ret;
}

/******************************************************************************
* Socket initialization
******************************************************************************/

#ifndef OS_MINGW

bool
socket_present () {
  return true;
}

#else

static bool __socket_present= false;
static wsoc::WSADATA wsadata;

struct __WSA_initializer {
  __WSA_initializer () {
	using namespace wsoc;
    if (__socket_present) return;
    int e= WSAStartup (MAKEWORD (2,0), &wsadata);
    if (e == 0)
      __socket_present= true;
    else {
      io_error << "disabling sockets because 'WSAStartup' failed: "
	       << strerror (e) << LF; }
  }
  ~__WSA_initializer () {
    wsoc::WSACleanup ();
    __socket_present= false;
  }
};

bool
socket_present () {
  static __WSA_initializer __dummy;
  (void) __dummy;
  return __socket_present;
}

#endif

/******************************************************************************
* Socket link
******************************************************************************/

// to be created by the server
socket_link_rep::socket_link_rep (int fd, SOCKADDR_STORAGE* addr,
    tm_contact contact2):
  host (""), port (0), socket_id (fd), contact (contact2),
    read_notifier_ptr (NULL), write_notifier_ptr (NULL) {
  alive= false;
  memcpy (&address, addr, sizeof(address));
  host= string_from_socket_address (&address);
}

// to be created by clients
socket_link_rep::socket_link_rep (string host2, unsigned short port2,
    tm_contact contact2):
  host (host2), port (port2), socket_id (-1), contact (contact2),
    read_notifier_ptr (NULL), write_notifier_ptr (NULL) {
  alive= false;
}

socket_link_rep::~socket_link_rep () {
  DEBUG_SOCKET("'~socket_link_rep' is closing socket " << socket_id);
  if (socket_id >= 0)
    CLOSE(socket_id);
  input_buffer= "";
  output_buffer= "";
}

string
socket_link_rep::start () {
  if (!socket_present ())
    return "cannot use sockets";
  if (socket_id >= 0) { // used by the server
    if (!is_active (contact)) {
      string msg= string ("unexpected closed contact for socket") *
        as_string (socket_id);
      server_log_write (log_error, msg);
      return msg;
    }
  }
  else { // used for clients
    if (used_by_server ())
      return "internal error, inconsistent 'socket_link_rep'";
    c_string _host (host);
    c_string _port (as_string (port));
    struct ADDRINFO hints;
    struct ADDRINFO *result, *rp;
    memset (&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;
    hints.ai_protocol = 0;
    hints.ai_canonname = NULL;
    hints.ai_addr = NULL;
    hints.ai_next = NULL;
    int x= GETADDRINFO(_host, _port, &hints, &result);
    if (x)
      return string ("'getaddrinfo' error: ") * string (GAI_STRERROR(x));
    for (rp = result; rp != NULL; rp = rp->ai_next) {
      socket_id= SOCKET(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
      if (socket_id < 0)
        continue;
      if (CONNECT(socket_id, rp->ai_addr, rp->ai_addrlen) != -1)
        break;
      CLOSE(socket_id);
    }
    if (rp == NULL)
      return "cannot connect to hostname '" * host
        * "' at port " * as_string (port);
    FREEADDRINFO(result);
#ifndef OS_MINGW
    if (fcntl (socket_id, F_SETFL, O_NONBLOCK) == -1)
      return string ("'fnctl' error: ") * strerror (errno);
#else
    {
      using namespace wsoc;
      u_long flags = -1;
      if (ioctlsocket (socket_id, FIONBIO, &flags) == SOCKET_ERROR)
        return string ("'ioctlsocket' error: ") * strerror (errno);
    }
#endif
    DEBUG_SOCKET("'socket_link_rep::start' created socket with id "
      << socket_id);
    ::start (contact, socket_id);
    if (!is_active (contact)) {
      CLOSE(socket_id);
      socket_id= -1;
      return "contact has not started";
    }
    else {
      if (DEBUG_IO)
        debug_io << "contact started for socket " << socket_id << "\n";
    }
    call ("client-add", object (socket_id));
  }
  read_notifier_ptr=
    tm_new<QSocketNotifier> (socket_id, QSocketNotifier::Read);
  write_notifier_ptr=
    tm_new<QSocketNotifier> (socket_id, QSocketNotifier::Write);
  if (!read_notifier_ptr || !write_notifier_ptr)
    return "cannot create socket notifiers";

  write_notifier_ptr->setEnabled (false);
#if QT_VERSION < 0x060000
  QObject::connect (read_notifier_ptr, SIGNAL (activated(int)),
    this, SLOT (data_set_ready(int)));
#else
  QObject::connect (read_notifier_ptr, &QSocketNotifier::activated,
    this, &socket_link_rep::data_set_ready);
#endif
  write_notifier_ptr->setEnabled (false);
#if QT_VERSION < 0x060000
  QObject::connect (write_notifier_ptr, SIGNAL (activated(int)),
    this, SLOT (ready_to_send(int)));
#else
  QObject::connect (write_notifier_ptr,&QSocketNotifier::activated,
    this, &socket_link_rep::ready_to_send);
#endif
  alive= true;
  return "";
}

void
socket_link_rep::stop () {
  // cout << "socket_link_rep::stop, port= " << port << LF;
  if (used_by_server ())
    server_log_write (log_info, string ("closing socket ") *
      as_string (socket_id));
  DEBUG_SOCKET("'socket_link_rep::stop' is closing socket " << socket_id);
  if (!alive)
    return;
  if (read_notifier_ptr) {
    read_notifier_ptr->setEnabled (false);
    read_notifier_ptr->disconnect (SIGNAL(activated(int)));
    tm_delete (read_notifier_ptr);
    read_notifier_ptr= NULL;
  }
  if (write_notifier_ptr) {
    write_notifier_ptr->setEnabled (false);
    write_notifier_ptr->disconnect (SIGNAL(activated(int)));
    tm_delete (write_notifier_ptr);
    write_notifier_ptr= NULL;
  }
  if (used_by_server ()) { // socket created by the server
    call ("server-logout-client", object (socket_id));
    call ("server-remove", object (socket_id));
  }
  else { // socket created by a client
    call ("client-remove", object (socket_id));
  }
  alive= false;
  ::stop (contact);
  if (socket_id >= 0) {
    if (CLOSE(socket_id) && used_by_server ())
      server_log_write (log_error, string ("cannot close socket ")
        * as_string (socket_id));
  }
  if (used_by_server ()) // socket created by the server
    emit disconnection (this);
  else
    socket_id= -1;
}

string&
socket_link_rep::watch (int channel) {
  static string empty_string= "";
  if (channel == LINK_OUT)
    return input_buffer;
  return empty_string;
}

string
socket_link_rep::read (int channel) {
  DEBUG_SOCKET("'socket_link_rep::read' received size " << N(input_buffer));
  if (channel == LINK_OUT && N(input_buffer)) {
    string r= input_buffer;
    input_buffer= "";
    return r;
  }
  else return "";
}

void
socket_link_rep::write (string s, int channel) {
  DEBUG_SOCKET("'socket_link_rep::write' received size " << N(s));
  if ((!alive) || (channel != LINK_IN) || !N(s))
    return;
  output_buffer << s;
  write_notifier_ptr->setEnabled (true);
}

void
socket_link_rep::data_set_ready (int s) {
  (void) s;
  DEBUG_SOCKET("'socket_link_rep::data_set_ready', socket "
    << socket_id << ", s= " << s);
  char data[16384];
  if (!alive) {
    read_notifier_ptr->setEnabled (false);
    return;
  }
  //if (!read_notifier_ptr->isEnabled ())
  //  return;
  read_notifier_ptr->setEnabled (false);
  int n= receive (contact, (void*) data, 16384);
  DEBUG_SOCKET("'socket_link_rep::data_set_ready', socket "
    << socket_id << " 'receive' returned " << n);
  if (n == 0) {
    DEBUG_SOCKET("'socket_link_rep::data_set_ready', socket "
      << socket_id << " hung up");
    if (!used_by_server ())
      io_error << "connection to server '" << host << "' hung up" << LF;
    stop ();
  }
  else if (n < 0) {
    if (is_active (contact)) {
      //if (!used_by_server ())
      // io_warning << "retrying connection to server '"
      //	    << host << "': "<< last_error (contact) << LF;
      read_notifier_ptr->setEnabled (true);
    }
    else {
      DEBUG_SOCKET("'socket_link_rep::data_set_ready', 'receive' failed: "
        << last_error (contact));
      if (!used_by_server ())
        io_error << "connection to server '" << host << "' aborted" << LF;
      stop ();
    }
  }
  else {
    input_buffer << string (data, n);
    if (DEBUG_IO) {
      string s (data, n);
      bool ok= true;
      for (int i= 0; i < N(s); i++)
        if (((int) (unsigned char) s[i]) >= 128 ||
            (((int) (unsigned char) s[i]) < 32 &&
             s[i] != '\n' && s[i] != '\t'))
          ok= false;
      if (ok) {
        DEBUG_SOCKET("'socket_link_rep::data_set_ready', received data: "
          << s);
      }
      else {
        DEBUG_SOCKET("'socket_link_rep::data_set_ready', received size "
          << N(s));
      }
    }
    if (!is_nil (feed_cmd))
      feed_cmd->apply ();
    if (!used_by_server ())
      emit data_set_ready (s);
    read_notifier_ptr->setEnabled (true);
  }
}

void
socket_link_rep::ready_to_send (int s) {
  (void) s;
  //cout  << "socket_link_rep::ready_to_send, s= " << s << LF;
#ifdef OS_MINGW
  using namespace wsoc;
#endif
  if (!alive) // || !write_notifier_ptr->isEnabled ())
    return;
  write_notifier_ptr->setEnabled (false);
  int n= N(output_buffer);
  if (n > 0) {
    c_string buf (output_buffer);
    int ret= send (contact, buf, n);
    DEBUG_SOCKET("'socket_link_rep::ready_to_send', 'send' returned "
      << ret);
    if (ret > 0) {
      if (ret == n)
        output_buffer= "";
      else
        output_buffer= output_buffer (ret, n);
      n -= ret;
      if (n > 0) write_notifier_ptr->setEnabled (true);
      if (!used_by_server ())
        emit ready_to_send (s);
    }
    else if (ret < 0) {
      DEBUG_SOCKET("'socket_link_rep::ready_to_send', error: "
        << last_error (contact));
      if (is_active (contact)) {
        if (!used_by_server ())
          io_warning << "retrying connection to server '"
            << host << "': " << last_error (contact);
        write_notifier_ptr->setEnabled (true);
      }
      else {
        if (!used_by_server ())
          io_error << "connection to server '"
            << host << "' aborted: " << last_error (contact);
        stop ();
      }
    }
    else write_notifier_ptr->setEnabled (true);
  }
}

void
socket_link_rep::listen (int msecs) {
  //cout << "socket_link_rep::listen" << LF;
  ASSERT (!used_by_server (), "unexpected call of 'listen'");
#ifdef OS_MINGW
  using namespace wsoc;
#endif
  if (!alive) return;
  //ready_to_send (socket_id);
  fd_set rfds, wfds, efds;
  FD_ZERO(&rfds);
  FD_ZERO(&wfds);
  FD_ZERO(&efds);
  TM_FD_SET(socket_id, &rfds);
  TM_FD_SET(socket_id, &wfds);
  TM_FD_SET(socket_id, &efds);
  struct timeval tv;
  tv.tv_sec = msecs / 1000;
  tv.tv_usec= 1000 * (msecs % 1000);
  int nr= select (socket_id+1, &rfds, &wfds, &efds, &tv);
  DEBUG_SOCKET("'socket_link_rep::listen', 'select' returned " << nr);
  if (nr == -1) {
    io_error << "connection to server '" << host << "' aborted" << LF;
    stop ();
    return;
  }
  if (nr >= 1 && !FD_ISSET (socket_id, &efds)) {
    if (FD_ISSET (socket_id, &rfds))
      emit data_set_ready (socket_id);
    //if (FD_ISSET (socket_id, &wfds))
    //  emit ready_to_send (socket_id);
  }
}

/******************************************************************************
* Server
******************************************************************************/

socket_server_rep::socket_server_rep (string host2, unsigned short port2):
  host (host2), port (port2), socket_id (-1), notifier_ptr (NULL),
  socket_ptr_from_id ((pointer) NULL) {}

socket_server_rep::~socket_server_rep () {}

string
socket_server_rep::start () {
#if defined(OS_MACOS)
  mac_begin_server ();
#endif
  socket_id = -1;
  if (!socket_present ()) {
    server_log_write (log_error, "cannot use sockets");
    return "cannot use sockets";
  }
  c_string _port (as_string (port));
  c_string _host (host);
  struct ADDRINFO hints;
  struct ADDRINFO *result, *rp;
  memset (&hints, 0, sizeof(struct ADDRINFO));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE;
  hints.ai_protocol = 0;
  hints.ai_canonname = NULL;
  hints.ai_addr = NULL;
  hints.ai_next = NULL;
  int x = GETADDRINFO(host == "" ? (char*) NULL : (char*) _host,
    (char*) _port, &hints, &result);
  if (x != 0)  {
    server_log_write (log_error, "'getaddrinfo' failed for " * host *
      string (" via port ") * as_string (port) *
      string (": " ) * string (GAI_STRERROR(x)));
    return "'getaddrinfo' failed";
  }
  for (rp = result; rp != NULL; rp = rp->ai_next) {
    hostname= string_from_socket_address ((SOCKADDR_STORAGE*) rp->ai_addr);
    server_log_write (log_info, "trying to serve at " * hostname
      * " via port " * as_string (port));
    socket_id= SOCKET(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
    if (socket_id < 0) {
      server_log_write (log_info, string ("server socket creation failed: ")
        * strerror (errno));
      socket_id = -1;
      continue;
    }
#ifndef OS_MINGW
    if (fcntl (socket_id, F_SETFL, O_NONBLOCK) == -1)  {
      server_log_write (log_info,
        string ("'fcntl' failed: ") * strerror (errno));
      safe_server_close (socket_id);
      socket_id = -1;
      continue;
    }
#else 
    { using namespace wsoc;
      u_long flags = -1;
      if (ioctlsocket (socket_id, FIONBIO, &flags) == SOCKET_ERROR) {
        server_log_write (log_info,
          string ("'ioctlsocket' failed: ") *
          strerror (errno));
        safe_server_close (socket_id);
        socket_id = -1;
        continue;
      }
    }
#endif
    if (BIND(socket_id, rp->ai_addr, rp->ai_addrlen) == 0)
      break;
    server_log_write (log_info,
      string ("'bind' failed: ") * strerror (errno));
    safe_server_close (socket_id);
    socket_id = -1;
  }
  FREEADDRINFO(result); 
  if (socket_id < 0) {
    server_log_write (log_error,
      string ("cannot start server at ") *
      host * " via " * as_string (port));
    return "cannot create server socket";
  }
  if (LISTEN(socket_id, 10) != 0) {
    server_log_write (log_error,
      string ("'listen' failed at ") *
      host * " via " * as_string (port) * ": " *
      strerror (errno));
    return "'listen' failed";
  }
  notifier_ptr= tm_new<QSocketNotifier> (socket_id, QSocketNotifier::Read);
#if QT_VERSION < 0x060000
  QObject::connect (notifier_ptr, SIGNAL(activated(int)),
    this, SLOT(connection(int)));
#else
  QObject::connect (notifier_ptr, &QSocketNotifier::activated,
    this, &socket_server_rep::connection);
#endif
  server_log_write (log_info,
    string ("waiting for connections at ") *
    host * " via " * as_string (port));
  call ("server-create-default-admin-account");
  return "";
}

void
socket_server_rep::stop () {
  server_log_write (log_info, string ("stopping server at ") *
    host * " via port " * as_string (port));
  iterator<pointer> it= iterate (connections);
  while (it->busy ()) {
    socket_link_rep* c= (socket_link_rep*) it->next ();
    c->stop ();
    disconnection (c);
  }
  if (notifier_ptr) {
    notifier_ptr->disconnect (SIGNAL(activated(int)));
    tm_delete (notifier_ptr);
    notifier_ptr= NULL;
  }
  safe_server_close (socket_id);
  socket_id= -1;
  server_log_stop ();
#if defined(OS_MACOS)
  mac_end_server ();
#endif
  server_log_write (log_info, string ("server stopped at ") *
    host * " via port " * as_string (port));
}

void
socket_server_rep::connection (int s) {
  server_log_write (log_info,
    string ("new connection received from ") * as_string (s));
  int client; socket_link_rep* clt;
  SOCKADDR_STORAGE cltadd;
  socklen_t sz= sizeof (cltadd);
  if (!notifier_ptr->isEnabled ()) return;
  client= ACCEPT(s, (SOCKADDR*) &cltadd, &sz);
  if (client <= 0) {
    switch (ERRNO) {
    case ERRSOC(EWOULDBLOCK):
    case ERRSOC(ECONNABORTED): break;
    default: notifier_ptr->setEnabled (false);
    }
    server_log_write (log_error,
      "connection failed from "
      * string_from_socket_address (&cltadd));
    return;
  }
  string address= string_from_socket_address (&cltadd);
  address_from_id (client)= address;
  server_log_write (log_info,
    string ("connection accepted from " ) *
    address * string (" at socket ") * as_string (client));
  array<array<string> > authentications;
  array<string> _anonymous; _anonymous << string ("anonymous");
  if (get_preference ("tls-server") == string ("on")) {
    if (get_preference ("tls-server authentication anonymous")
      == string ("on"))
      authentications << _anonymous;
  }
  else
    authentications << _anonymous;
  tm_contact contact= (get_preference ("tls-server") == string ("on")) ?
    make_tls_server_contact (authentications) :
    make_socket_server_contact (authentications);
  ::start (contact, client);
  if (!is_active (contact)) {
    server_log_write (log_error,
      "contact failed from "
      * string_from_socket_address (&cltadd)
      * " at socket " * as_string (client));
    ::stop (contact);
    CLOSE(client);
    return;
  }
  server_log_write (log_info,
    "contact started from "
    * string_from_socket_address (&cltadd)
    * " at socket " * as_string (client));
  clt= tm_new<socket_link_rep> (client, &cltadd, contact);
  string st= clt->start ();
  if (st != "") {
    server_log_write (log_error,
      "'socket_link_rep' failed from "
      * string_from_socket_address (&cltadd)
      * " at socket " * as_string (client) * ": " * st);
    clt->stop ();
    tm_delete (clt);
  }
  connect (clt, SIGNAL (disconnection(socket_link_rep*)), this,
    SLOT (disconnection (socket_link_rep*)));
  connections->insert ((pointer) clt);
  socket_ptr_from_id (clt->get_socket_id ())= (pointer) clt;
  call ("server-add", object (clt->get_socket_id ()));
  server_log_write (log_info,
    "'socket_link_rep' started from "
    * string_from_socket_address (&cltadd)
    * " at socket " * as_string (client));
}

void 
socket_server_rep::disconnection (class socket_link_rep* clt) {
  //cout << "socket_server_rep::disconnection" << LF;
  int io= clt->get_socket_id ();
  server_log_write (log_info,
    string ("disconnection of ")
    * clt->get_host_name ()
    * " from socket " * as_string (io));
  connections->remove ((pointer) clt);
  socket_ptr_from_id->reset (io);
  address_from_id->reset (io);
  tm_delete (clt);
}

string
socket_server_rep::read (int id) {
  socket_link_rep* clt= find_connection_ptr (id);
  if (!clt)
    return "";
  if (!clt->complete_packet (LINK_OUT)) return "";
  bool success;
  string back= clt->read_packet (LINK_OUT, 0, success);
  return back;
}

void
socket_server_rep::write (int id, string s) {
  socket_link_rep* clt= find_connection_ptr (id);
  if (clt)
    clt->write_packet(s, LINK_IN);
}

socket_link_rep*
socket_server_rep::find_connection_ptr (int id) {
  pointer ptr= socket_ptr_from_id[id];
  if (ptr)
    return (socket_link_rep*) ptr;
  server_log_write (log_error,
    string ("'socket_server_rep::find_connection_ptr', ")
    * "cannot find socket " * as_string (id));
  return NULL;
}

void
socket_server_rep::listen_connections (int msecs) {
  //cout << "socket_server_rep::listen_connections" << LF;
#ifdef OS_MINGW
  using namespace wsoc;
#endif
  fd_set rfds, wfds, efds;
  FD_ZERO(&rfds);
  FD_ZERO(&wfds);
  FD_ZERO(&efds);
  int max_io= 0;
  iterator<pointer> it= iterate (connections);
  while (it->busy ()) {
    socket_link_rep* c= (socket_link_rep*) it->next ();
    int io= c->get_socket_id ();
    max_io= max (max_io, io);
    TM_FD_SET(io, &rfds);
    TM_FD_SET(io, &wfds);
    TM_FD_SET(io, &efds);
  }
  struct timeval tv;
  tv.tv_sec = msecs / 1000;
  tv.tv_usec= 1000 * (msecs % 1000);
  int nr= select (max_io+1, &rfds, &wfds, &efds, &tv);
  if (nr >= 1) {
    it= iterate (connections);
    while (it->busy ()) {
      socket_link_rep* c= (socket_link_rep*) it->next ();
      int io= c->get_socket_id ();
      if (FD_ISSET (io, &efds))
        continue;
      if (FD_ISSET (io, &rfds))
        emit c->data_set_ready (io);
      if (FD_ISSET (io, &wfds))
        emit c->ready_to_send (io);
    }
  }
}
