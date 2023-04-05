
/******************************************************************************
* MODULE     : QTMsockets.cpp
* DESCRIPTION: QT TeXmacs sockets manager
* COPYRIGHT  : (C) 2015 Denis RAUX
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMSockets.hpp"
#include "scheme.hpp"
#include "iterator.hpp"
#include "analyze.hpp"

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
#define RECV recv
#define GAI_STRERROR gai_strerror
#define BIND bind
#define LISTEN listen
#define ACCEPT accept
#define INET_NTOP inet_ntop

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
#define RECV wsoc::recv
#define GAI_STRERROR wsoc::gai_strerror
#define BIND wsoc::bind
#define LISTEN wsoc::listen
#define ACCEPT wsoc::accept
#define INET_NTOP wsoc::inet_ntop

#endif

unsigned qtmsocket_debug_count;

int socket_basic::count= 0;

#ifdef OS_MINGW
wsoc::WSADATA socket_basic::wsadata;
#endif

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
    return tmp;
  }
  return "";
}

socket_basic::socket_basic (): st (ST_VOID) {
#ifdef OS_MINGW
  if (!count) {
    using namespace wsoc;
    err= WSAStartup (MAKEWORD (2,0), &wsadata);
    if (err) {st= ST_WSA; return;}
  }
#endif
  count++;
};

socket_basic::~socket_basic () {
  if (count > 0) --count;
#ifdef OS_MINGW
  if (!count) wsoc::WSACleanup ();
#endif
};

socket_link::socket_link (int s, SOCKADDR_STORAGE* addr) {
  id++; sock= s; qsnr= NULL; qsnw= NULL;
  if (st != ST_VOID) return;
  memcpy (&add, addr, sizeof(add));
  qsnr= tm_new<QSocketNotifier> (s, QSocketNotifier::Read);
  qsnw= tm_new<QSocketNotifier> (s, QSocketNotifier::Write);
  if (!qsnr || !qsnw) { err= ERRNO; st= ST_NOTIF; return; }
  QObject::connect (qsnr, SIGNAL(activated(int)),
		    this, SLOT(data_set_ready(int)));
  QObject::connect (qsnw, SIGNAL(activated(int)),
		    this, SLOT(ready_to_send(int)));
  DBG_IO ("Socket created with fd= " << sock);
  st= ST_OK;
}

socket_link::socket_link (string host, unsigned short port) {
  ++id; qsnr= NULL; qsnw= NULL;
  if (st != ST_VOID) return;
  string host_tmp= host;
  if (starts (host, "[") && ends (host, "]"))
    host_tmp = host(1, N(host)-1);
  c_string _host (host_tmp);
  c_string _port (as_string (port));
  struct ADDRINFO hints;
  struct ADDRINFO *result, *rp;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE;
  hints.ai_protocol = 0;
  hints.ai_canonname = NULL;
  hints.ai_addr = NULL;
  hints.ai_next = NULL;
  int x = GETADDRINFO (_host, _port, &hints, &result);
  if (x != 0) { err= ERRNO; st= ST_GETHOST; return; };
  for (rp = result; rp != NULL; rp = rp->ai_next) {
    sock= SOCKET (rp->ai_family, rp->ai_socktype,
		  rp->ai_protocol);
    if (sock < 0)
      continue;
    if (CONNECT (sock, rp->ai_addr, rp->ai_addrlen) != -1)
      break;
    CLOSE (sock);
  }
  if (rp == NULL) { err= ERRNO; st= ST_CONNECTION; return; }
  FREEADDRINFO (result);
#ifndef OS_MINGW
  if (fcntl (sock, F_SETFL, O_NONBLOCK) == -1) {
    err= errno; st= ST_FCNTL; return; }
#else
  {
    using namespace wsoc;
    u_long flags = -1;
    if (ioctlsocket (sock, FIONBIO, &flags) == SOCKET_ERROR) {
      err= ERRNO; st= ST_FCNTL; return; }
  }
#endif
  qsnr= tm_new<QSocketNotifier> (sock, QSocketNotifier::Read);
  qsnw= tm_new<QSocketNotifier> (sock, QSocketNotifier::Write);
  if (!qsnr || !qsnw) { err= ERRNO; st= ST_NOTIF; return; }
  QObject::connect (qsnr, SIGNAL (activated(int)),
		    this, SLOT (data_set_ready(int)));
  qsnw->setEnabled (false);
  QObject::connect (qsnw, SIGNAL (activated(int)),
		    this, SLOT (ready_to_send(int)));
  DBG_IO ("Socket created with fd= " << sock);
  st= ST_OK;
}

socket_link::~socket_link() {
  DBG_IO ("Closing socket for fd= " << sock);
  if (qsnr) { qsnr->disconnect (SIGNAL(activated(int))); tm_delete (qsnr); }
  if (qsnw) { qsnw->disconnect (SIGNAL(activated(int))); tm_delete (qsnw); }
  if (sock != -1) { CLOSE (sock); sock=-1;}
  st= ST_CLOSED;
}

string
socket_link::start () {
  string ret;
  switch(st) {
  case ST_OK:         return ""; break;
  case ST_VOID:       ret= "Socket not initialised"; break;
  case ST_SOCKET:     ret= "Error in opening socket"; break;
  case ST_FCNTL:      ret= "Error in setting blocking mode"; break;
  case ST_BIND:       ret= "Error during bind"; break;
  case ST_LISTEN:     ret= "Error during listen"; break;
  case ST_CONNECTION: ret= "Error during connect"; break;
  case ST_GETHOST:    ret= "Error in getting host"; break;
  case ST_NOTIF:      ret= "Error in setting notifier"; break;
  default:            ret= "No error message";
  }
  return ret * " errno: " * strerror(err);
}

string&
socket_link::watch (int channel) {
  static string empty_string= "";
  if (channel == LINK_OUT) return inbuf;
  else return empty_string;
}

string
socket_link::read (int channel) {
  DBG_IO ("Socket read size= " << N(inbuf));
  if (channel == LINK_OUT && N(inbuf)) {
    string r= inbuf;
    inbuf= "";
    return r;
  }
  else return "";
}

void
socket_link::stop () {
  st= ST_HALTED;
  emit disconnection (this);
}

void
socket_link::data_set_ready (int s) {
  char data[2048];
  qsnr->setEnabled (false);
  int lgdata= RECV(s, data, sizeof(data), 0);
  DBG_IO ("Socket incoming code= " << lgdata);
  if (lgdata == 0) {
    DBG_IO ("Client disconnected");   
    stop ();
  }
  else if (lgdata == -1) {
    switch (ERRNO) {
    case ERRSOC(EWOULDBLOCK):
    case ERRSOC(ECONNRESET):
    case ERRSOC(ECONNABORTED): DBG_IO("Client disconnected"); break;
    default: DBG_IO("Receiving error: " << ERRNO);
    }
    stop ();
  }
  else {
    inbuf << string (data, lgdata);
    if (DEBUG_IO) {
      string s (data, lgdata);
      bool ok= true;
      for (int i= 0; i < N(s); i++)
        if (((int) (unsigned char) s[i]) >= 128 ||
            (((int) (unsigned char) s[i]) < 32 &&
             s[i] != '\n' && s[i] != '\t')) ok= false;
      if (ok) { DBG_IO("Data received: " << s); }
      else { DBG_IO("Binary data received size= " << N(s)); }
    }
    qsnr->setEnabled (true);
  }
}

void
socket_link::ready_to_send (int s) {
#ifdef OS_MINGW
  using namespace wsoc;
#endif
  qsnw->setEnabled (false);
  int sz= N(outbuf);
  if (sz) {
    char* buf= as_charp (outbuf);
    int ret= WRITE(s, buf, sz);
    DBG_IO ("Socket outcoming code= " << ret);
    if (ret >0) {
      if (ret == sz) outbuf= ""; else outbuf= outbuf (ret, sz);
      sz -= ret;
      if (sz) qsnw->setEnabled (true);
    }
    else if (ret <0) {
      DBG_IO ("Sending error: " << strerror (ERRNO));
      stop ();
    }
    else qsnw->setEnabled (true);
  }
}

void
socket_link::listen (int msecs) {
#ifdef OS_MINGW
  using namespace wsoc;
#endif
  if (!alive ()) return;
  ready_to_send (sock);
  fd_set rfds;
  FD_ZERO (&rfds);
  FD_SET (sock, &rfds);
  struct timeval tv;
  tv.tv_sec = msecs / 1000;
  tv.tv_usec= 1000 * (msecs % 1000);
  int nr= select (sock+1, &rfds, NULL, NULL, &tv);
  if (nr == 1) data_set_ready (sock);
  DBG_IO ("Listenning result: " << nr);
  if (nr == -1) stop();
}

void
socket_link::write (string s, int channel) {
  DBG_IO ("Socket write size= " << N(s));
  if ((!alive ()) || (channel != LINK_IN) || !N(s)) return;
  outbuf << s;
  qsnw->setEnabled(true);
}

socket_server::socket_server (string host, unsigned short port) {
  c_string _port (as_string (port));
  c_string _host (host);
  struct ADDRINFO hints;
  struct ADDRINFO *result, *rp;
  memset(&hints, 0, sizeof(struct ADDRINFO));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE;
  hints.ai_protocol = 0;
  hints.ai_canonname = NULL;
  hints.ai_addr = NULL;
  hints.ai_next = NULL;
  int x = GETADDRINFO (host == "" ? (char*) NULL : (char*)_host,
		       _port, &hints, &result);
  if (x != 0)  {
    DBG_IO(GAI_STRERROR(x));
    err= ERRNO;
    st= ST_GETHOST;
    return;
  }
  sock = -1;
  for (rp = result; rp != NULL; rp = rp->ai_next) {
    DBG_IO ("Serving at " *
	    string_from_socket_address ((SOCKADDR_STORAGE*) rp->ai_addr));
    sock= SOCKET (rp->ai_family, rp->ai_socktype, rp->ai_protocol);
    if (sock < 0)
      continue;
#ifndef OS_MINGW
    if (fcntl (sock, F_SETFL, O_NONBLOCK) == -1) 
      continue;
#else 
    { using namespace wsoc;
      u_long flags = -1;
      if (ioctlsocket (sock, FIONBIO, &flags) == SOCKET_ERROR)
	continue;
    }
#endif
    if (BIND (sock, rp->ai_addr, rp->ai_addrlen) == 0)
      break;
    DBG_IO (strerror (errno));
    DBG_IO ("Socket bind failed");
    CLOSE (sock);
    sock = -1;
  }
  FREEADDRINFO (result); 
  if (sock == -1) {
    DBG_IO ("Cannot find socket binding for server");
    err= ERRNO;
    st= ST_BIND;
    return; 
  } 
  if (LISTEN (sock, 10) != 0) {
    DBG_IO ("Cannot listen on server socket");
    err= ERRNO;
    st= ST_LISTEN;
    return;
  }
  qsnc= tm_new<QSocketNotifier> (sock, QSocketNotifier::Read);
  QObject::connect (qsnc, SIGNAL (activated(int)), this, SLOT (connection(int)));
  DBG_IO ("Wait for connection");
}

void
socket_server::connection (int s) {
  int sclt; socket_link* clt;
  SOCKADDR_STORAGE cltadd;
  socklen_t sz= sizeof (cltadd);
  if (!qsnc->isEnabled ()) return;
  sclt= ACCEPT (s, (SOCKADDR*) &cltadd, &sz);
  if (sclt > 0) {
    clt= tm_new<socket_link> (sclt, &cltadd);
    if (clt) {
      if (clt->alive ()) {
        connect (clt, SIGNAL (disconnection(socket_link*)), this,
		 SLOT (disconnection (socket_link*)));
        clts->insert ((pointer) clt);
        call ("server-add", object (clt->getid ()));
        DBG_IO ("Client Connected from "
	        << string_from_socket_address (&cltadd)
	        << ", with id: " << clt->getid ());
      }
      else tm_delete (clt);
    }
  }
  else switch (ERRNO) {
    case ERRSOC (EWOULDBLOCK):
    case ERRSOC (ECONNABORTED): break;
    default: err= ERRNO; qsnc->setEnabled (false); st= ST_CONNECTION;
    }
}

void 
socket_server::disconnection (class socket_link* clt) {
  call ("server-remove", object (clt->getid()));
  clts->remove ((pointer) clt);
  tm_delete (clt);
}

string
socket_server::read (int id) {
  socket_link *clt= find_client (id);
  if (!clt) return "";
  bool success;
  string back= clt->read_packet (LINK_OUT, 0, success);
  return back;
}

void
socket_server::write (int id, string s) {
  socket_link *clt= find_client (id);
  if (clt) clt->write_packet(s, LINK_IN);
}

socket_link *
socket_server::find_client (int id) {
  iterator<pointer> it= iterate (clts);
  while (it->busy ()) {
    socket_link* clt= (socket_link*) it->next ();
    if (clt->getid() == id) return clt;
  }
  array<int> ids;
  it= iterate (clts);
  while (it->busy ()) {
    socket_link* clt= (socket_link*) it->next ();
    ids << clt->getid();
  }
  DBG_IO ("Client not found, id= " << id << ", among= " << ids);
  return NULL;
}

socket_server::~socket_server () {
  iterator<pointer> it= iterate (clts);
  while (it->busy ()) {
    socket_link* clt= (socket_link*) it->next ();
    disconnection (clt);
  }
}

string
debug_io_string (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++) {
    unsigned char c= (unsigned char) s[i];
    if (c == DATA_BEGIN) r << "[BEGIN]";
    else if (c == DATA_END) r << "[END]";
    else if (c == DATA_COMMAND) r << "[COMMAND]";
    else if (c == DATA_ESCAPE) r << "[ESCAPE]";
    else r << s[i];
  }
  return r;
}
