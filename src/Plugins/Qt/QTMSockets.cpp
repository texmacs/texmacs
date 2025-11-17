
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
#include "boot.hpp"
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
    SERRNO_LOGE ("server socket close");
    SLOGE ("cannot close server socket. Please exit TeXmacs in order to stop "
        "the server completely");
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
      SERRNO_LOGE ("WSAStartup failed, disabling sockets");
    }
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

static hashset<pointer> all_connections;

static inline bool
exists (socket_link_rep* s) {
  return all_connections->contains ((pointer) s);
}

static inline void
checkin (socket_link_rep* s) {
  all_connections->insert ((pointer) s);
}

static inline void
checkout (socket_link_rep* s) {
  all_connections->remove ((pointer) s);
}

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
    if (!is_alive (contact)) {
      string msg= string ("unexpected closed contact for socket ") *
        as_string (socket_id);
      checkin (this);
      SLOGE (msg);
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
      DEBUG_SOCKET("'socket_link_rep::start' trying socket for addr");
      socket_id= SOCKET(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
      if (socket_id < 0)
        continue;

      // TODO: this call is blocking, the socket is put non blocking further
      // down after the connection succeeded. This blocks the UI if we
      // connect to a valid domain name that isn't a texmacs server.
      // We should put the socket as non blocking before the connect, just
      // like we did for the gnutls handshake, and then resume the connection
      // in resume_start. We would need to try all the results from
      // getaddrinfo one after the other (keep addrinfo struct as class attr)
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
      return SERRNO_FMT("set socket non block");
#else
    {
      using namespace wsoc;
      u_long flags = -1;
      if (ioctlsocket (socket_id, FIONBIO, &flags) == SOCKET_ERROR)
        return SERRNO_FMT("ioctlsocket");
    }
#endif
    DEBUG_SOCKET("'socket_link_rep::start' created socket with id "
      << socket_id);
    ::start (contact, socket_id);
    if (!is_alive (contact)) {
      CLOSE(socket_id);
      socket_id= -1;
      return "contact has not started";
    }

    SLOG ("contact started for socket " * as_string (socket_id));
    call ("client-add", object (socket_id));
  }
  checkin (this);
  read_notifier_ptr= new QSocketNotifier (socket_id, QSocketNotifier::Read);
  write_notifier_ptr= new QSocketNotifier (socket_id, QSocketNotifier::Write);
  if (!read_notifier_ptr || !write_notifier_ptr)
    return "cannot create socket notifiers";

  if (is_active (contact)) {
    connect_data_notifiers ();
  } else {
    connect_handshake_notifiers ();
  }
  read_notifier_ptr->setEnabled (true);
  write_notifier_ptr->setEnabled (true);
  alive= true;
  return "";
}

void socket_link_rep::connect_data_notifiers () {
  ASSERT (read_notifier_ptr && write_notifier_ptr,
	  "socket notifiers are NULL");
  ASSERT (is_active(contact), "inactive contact");
  SLOG ("connecting data notifiers for socket " * as_string (socket_id));
  read_notifier_ptr->disconnect();
  write_notifier_ptr->disconnect();

#if QT_VERSION < 0x060000
  QObject::connect (read_notifier_ptr, SIGNAL (activated(int)),
    this, SLOT (data_set_ready(int)));
  QObject::connect (write_notifier_ptr, SIGNAL (activated(int)),
    this, SLOT (ready_to_send(int)));
#else
  QObject::connect (read_notifier_ptr, &QSocketNotifier::activated,
    this, &socket_link_rep::data_set_ready);
  QObject::connect (write_notifier_ptr, &QSocketNotifier::activated,
    this, &socket_link_rep::ready_to_send);
#endif
}

void socket_link_rep::connect_handshake_notifiers () {
  ASSERT (read_notifier_ptr && write_notifier_ptr,
	  "socket notifiers are NULL");
  SLOG ("connecting handshake resume notifier for socket "
	* as_string (socket_id));
#if QT_VERSION < 0x060000
  QObject::connect (read_notifier_ptr, SIGNAL (activated(int)),
    this, SLOT (resume_start(int)));
  QObject::connect (write_notifier_ptr, SIGNAL (activated(int)),
    this, SLOT (resume_start(int)));
#else
  QObject::connect (read_notifier_ptr, &QSocketNotifier::activated,
    this, &socket_link_rep::resume_start);
  QObject::connect (write_notifier_ptr, &QSocketNotifier::activated,
    this, &socket_link_rep::resume_start);
#endif
}

void
socket_link_rep::stop () {
  DEBUG_SOCKET("'socket_link_rep::stop' is closing socket " << socket_id);
  if (!alive) {
    checkout (this);
    return;
  }
  if (read_notifier_ptr) {
    read_notifier_ptr->setEnabled (false);
    read_notifier_ptr->disconnect ();
    read_notifier_ptr->deleteLater ();
  }
  if (write_notifier_ptr) {
    write_notifier_ptr->setEnabled (false);
    write_notifier_ptr->disconnect ();
    write_notifier_ptr->deleteLater ();
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
      SLOGE ("cannot close socket " * as_string (socket_id));
  }
  if (used_by_server ()) // socket created by the server
    emit disconnection (this);
  else
    socket_id= -1;
  checkout (this);
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

inline bool
socket_link_rep::retry (int err) {
#ifdef USE_GNUTLS
  return err < 0 && err != EAGAIN && err != GNUTLS_E_AGAIN &&
    err != GNUTLS_E_INTERRUPTED;
#else
  return err < 0 && err != EAGAIN;
#endif
}

void
socket_link_rep::resume_start (int s) {
  if (!exists (this)) return;
  if (!is_alive (contact)) {
    DEBUG_SOCKET ("contact is dead for socket " * as_string (s));
    stop ();
    // on cert error an interactive trust-certificate widget is spawned
    if (!is_headless ()
        && contact->last_error() != ""
        && contact->last_error() != "certificate verify interactive")
      call ("client-open-error", contact->last_error());
    return;
  }
  // Get the notifier that sent the signal
  QSocketNotifier* notifier = qobject_cast<QSocketNotifier*> (sender ());
  if (!is_active (contact)) {
    notifier->setEnabled (false);
    ::start (contact, s);
    if (!is_alive (contact)) {
      stop ();
      if (!is_headless ()
          && contact->last_error() != ""
          && contact->last_error() != "certificate verify interactive")
        call ("client-open-error", contact->last_error());
      return;
    }
  }
  if (is_active (contact))
    connect_data_notifiers();
  notifier->setEnabled (true);
}

void
socket_link_rep::data_set_ready (int s) {
  if (!exists (this)) return;
  read_notifier_ptr->setEnabled (false);
  if (!alive)
    return;
  if (!is_active (contact)) {
    stop ();
    return;
  }
  DEBUG_SOCKET("'socket_link_rep::data_set_ready', socket "
    << socket_id << ", s= " << s);
  char data[16384];
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
    if (is_alive (contact))
      read_notifier_ptr->setEnabled (true);
    else {
      DEBUG_SOCKET("'socket_link_rep::data_set_ready', 'receive' failed: "
        << last_error (contact));
      if (used_by_server ())
	io_error << "connection to client " << s << " aborted" << LF;
      else
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
        DEBUG_SOCKET_DATA(
            "'socket_link_rep::data_set_ready', received data: ", s);
      }
      else {
        DEBUG_SOCKET("'socket_link_rep::data_set_ready', received size "
          << N(s));
      }
    }
    if (!is_nil (feed_cmd))
      feed_cmd->apply ();
    read_notifier_ptr->setEnabled (true);
  }
}

void
socket_link_rep::ready_to_send (int s) {
  if (!exists (this)) return;
#ifdef OS_MINGW
  using namespace wsoc;
#endif
  write_notifier_ptr->setEnabled (false);
  if (!alive)
    return;
  if (!is_active (contact)) {
    stop ();
    return;
  }
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
    }
    else if (retry (ret)) {
      DEBUG_SOCKET("'socket_link_rep::ready_to_send', error: "
        << last_error (contact));
      if (is_active (contact)) {
        if (used_by_server ())
          io_warning << "retrying connection to client "
            << as_string (s) << ": " << last_error (contact);
        else
          io_warning << "retrying connection to server '"
            << host << "': " << last_error (contact);
        write_notifier_ptr->setEnabled (true);
      }
      else {
        if (used_by_server ())
          io_error << "connection to client "
            << as_string (s) << " aborted: " << last_error (contact);
        else
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
  (void) msecs;
  FAILED ("not implemented");
}

/******************************************************************************
* Server
******************************************************************************/

static hashset<pointer> all_servers;

static inline bool
exists (socket_server_rep* s) {
  return all_servers->contains ((pointer) s);
}

static inline void
checkin (socket_server_rep* s) {
  all_servers->insert ((pointer) s);
}

static inline void
checkout (socket_server_rep* s) {
  all_servers->remove ((pointer) s);
}

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
    SLOGE ("cannot use sockets");
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
    SLOGE ("'getaddrinfo' failed for " * host * " via port " * as_string (port)
        * ": "  * as_string (GAI_STRERROR(x)));
    return "'getaddrinfo' failed";
  }
  for (rp = result; rp != NULL; rp = rp->ai_next) {
    hostname= string_from_socket_address ((SOCKADDR_STORAGE*) rp->ai_addr);
    SLOGI ("trying to serve at " * hostname * ":" * as_string (port));
    socket_id= SOCKET(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
    if (socket_id < 0) {
      SERRNO_LOGE ("server socket creation");
      socket_id = -1;
      continue;
    }
#ifndef OS_MINGW
    if (fcntl (socket_id, F_SETFL, O_NONBLOCK) == -1)  {
      SERRNO_LOGE ("cannot set socket as non blocking");
      safe_server_close (socket_id);
      socket_id = -1;
      continue;
    }
#else
    { using namespace wsoc;
      u_long flags = -1;
      if (ioctlsocket (socket_id, FIONBIO, &flags) == SOCKET_ERROR) {
        SERRNO_LOGI ("cannot set socket as non blocking");
        safe_server_close (socket_id);
        socket_id = -1;
        continue;
      }
    }
#endif
    if (BIND(socket_id, rp->ai_addr, rp->ai_addrlen) == 0)
      break;
    else if (errno == EADDRINUSE) {
      SERRNO_LOGE ("bind");
      safe_server_close (socket_id);
      socket_id = -1;
      break;
    }
    SERRNO_LOGE ("bind");
    safe_server_close (socket_id);
    socket_id = -1;
  }
  FREEADDRINFO(result); 
  if (socket_id < 0) {
    SLOGE ("cannot start server at " * host * " via " * as_string (port));
    return "cannot create server socket";
  }
  if (LISTEN(socket_id, 10) != 0) {
    SERRNO_LOGE ("listen on " * host * ":" * as_string (port));
    return "'listen' failed";
  }
  checkin (this);
  notifier_ptr= tm_new<QSocketNotifier> (socket_id, QSocketNotifier::Read);
#if QT_VERSION < 0x060000
  QObject::connect (notifier_ptr, SIGNAL(activated(int)),
    this, SLOT(connection(int)));
#else
  QObject::connect (notifier_ptr, &QSocketNotifier::activated,
    this, &socket_server_rep::connection);
#endif
  SLOGI ("waiting for connections at " * host * ":" * as_string (port));
  call ("server-create-default-admin-account");
  return "";
}

void
socket_server_rep::stop () {
  SLOGI ("stopping server at " * host * ":" * as_string (port));
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
  checkout (this);
  SLOGI ("server stopped at " * host * ":" * as_string (port));
}

void
socket_server_rep::connection (int s) {
  if (!exists (this)) return;
  SLOGI ("new connection received from " * as_string (s));
  int client; socket_link_rep* clt;
  SOCKADDR_STORAGE cltadd;
  socklen_t sz= sizeof (cltadd);
  if (!notifier_ptr->isEnabled ()) return;
  client= ACCEPT(s, (SOCKADDR*) &cltadd, &sz);
  if (client <= 0) {
    switch (ERRNO) {
    case ERRSOC(EWOULDBLOCK):
    case ERRSOC(ECONNABORTED): break;
    default: {
      notifier_ptr->setEnabled (false);
      SLOGE ("server socket aborted");
      stop (); }
    }
    SLOGE ("connection failed from " * string_from_socket_address (&cltadd));
    return;
  }
  string address= string_from_socket_address (&cltadd);
  address_from_id (client)= address;
  SLOGI ("connection accepted from " * address
      * " at socket " * as_string (client));

#ifndef OS_MINGW
  if (fcntl (client, F_SETFL, O_NONBLOCK) == -1) {
    SERRNO_LOGE ("cannot set socket as non blocking");
    return;
  }
#else
  {
    using namespace wsoc;
    u_long flags = -1;
    if (ioctlsocket (client, FIONBIO, &flags) == SOCKET_ERROR)
      SERRNO_LOGE ("cannot set socket as non blocking");
      return;
  }
#endif

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

  if (!is_alive (contact)) {
    SLOGE ("contact failed from " * string_from_socket_address (&cltadd)
      * " at socket " * as_string (client));
    ::stop (contact);
    CLOSE(client);
    return;
  } 

  SLOGI ("contact started from " * string_from_socket_address (&cltadd)
    * " at socket " * as_string (client));

  clt= new socket_link_rep (client, &cltadd, contact);
  string st= clt->start ();
  if (st != "") {
    SLOGE ("'socket_link_rep' failed from "
      * string_from_socket_address (&cltadd)
      * " at socket " * as_string (client) * ": " * st);
    clt->stop ();
    clt->deleteLater ();
  }
  socket_ptr_from_id (clt->get_socket_id ())= (pointer) clt;
  connections->insert ((pointer) clt);
  call ("server-add", object (clt->get_socket_id ()));
  connect (clt, SIGNAL (disconnection(socket_link_rep*)), this,
	   SLOT (disconnection (socket_link_rep*)));
  SLOGI ("'socket_link_rep' started from "
    * string_from_socket_address (&cltadd)
    * " at socket " * as_string (client));
}

void
socket_server_rep::disconnection (class socket_link_rep* clt) {
  if (!exists (this)) return;
  int io= clt->get_socket_id ();
  SLOGI ("disconnection of " * clt->get_host_name ()
      * " from socket " * as_string (io));
  connections->remove ((pointer) clt);
  socket_ptr_from_id->reset (io);
  address_from_id->reset (io);
  clt->stop ();
  clt->deleteLater();
}

string
socket_server_rep::read (int id) {
  socket_link_rep* clt= find_connection_ptr (id);
  if (!clt)
    return "";
  if (!clt->complete_packet (LINK_OUT)) return "";
  bool success;
  string ret= clt->read_packet (LINK_OUT, 0, success);
  return ret;
}

void
socket_server_rep::write (int id, string s) {
  socket_link_rep* clt= find_connection_ptr (id);
  if (clt)
    clt->write_packet (s, LINK_IN);
}

socket_link_rep*
socket_server_rep::find_connection_ptr (int id) {
  pointer ptr= socket_ptr_from_id[id];
  if (ptr)
    return (socket_link_rep*) ptr;
  SLOGE ("'socket_server_rep::find_connection_ptr', cannot find socket "
      * as_string (id));
  return NULL;
}

void
socket_server_rep::listen_connections (int msecs) {
  (void) msecs;
  FAILED ("not implemented");
}
