
/******************************************************************************
* MODULE     : socket_link.cpp
* DESCRIPTION: TeXmacs links by sockets
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
* THANKS     : Beej's Guide to Network Programming has been helpful
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/
#ifndef QTTEXMACS

#include "socket_link.hpp"
#include "sys_utils.hpp"
#include "hashset.hpp"
#include "iterator.hpp"
#include "tm_timer.hpp"
#include "scheme.hpp"
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#ifndef OS_MINGW
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#else
namespace wsoc {
#include <sys/types.h>
#include <winsock2.h>
}
#endif
#include <errno.h>

hashset<pointer> socket_link_set;
void socket_callback (void *obj, void* info);

/******************************************************************************
* Constructors and destructors for socket_links
******************************************************************************/

socket_link_rep::socket_link_rep (string host2, int port2, int type2, int fd):
  host (host2), port (port2), type (type2)
{
  socket_link_set->insert ((pointer) this);
  io     = fd;
  outbuf = "";
  alive  = (fd != -1);
  if (type == SOCKET_SERVER) {
    sn = socket_notifier (io, &socket_callback, this, NULL);  
    add_notifier (sn);
    call ("server-add", object (io));
  }
}

socket_link_rep::~socket_link_rep () {
  stop ();
  socket_link_set->remove ((pointer) this);
}

socket_link_rep*
make_weak_socket_link (string host, int port, int type, int fd) {
  return tm_new<socket_link_rep> (host, port, type, fd);
}

tm_link
make_socket_link (string host, int port, int type, int fd) {
  return make_weak_socket_link (host, port, type, fd);
}

tm_link
find_socket_link (int fd) {
  iterator<pointer> it= iterate (socket_link_set);
  while (it->busy()) {
    socket_link_rep* con= (socket_link_rep*) it->next();
    if (con->io == fd && con->alive) return tm_link (con);
  }
  return tm_link ();
}

void
close_all_sockets () {
#ifndef OS_MINGW
  iterator<pointer> it= iterate (socket_link_set);
  while (it->busy()) {
    socket_link_rep* con= (socket_link_rep*) it->next();
    if (con->alive) {
      // FIXME: cleanly close the connection to the socket here
      con->alive= false;
    }
  }
#endif
}

/******************************************************************************
* Routines for socket_links
******************************************************************************/

string
socket_link_rep::start () {
#ifdef OS_MINGW
  using namespace wsoc;
  {
    WSAData data;
    int ret;
    ret=wsoc::WSAStartup(MAKEWORD(2,0), &data);
    if(ret) {
      char buf[32];
      string str="Error: WSAStartup failed code:";
      str << itoa(ret,buf,10);
      return(str);
    }
  }
#endif  
    if (alive) return "busy";
  if (DEBUG_AUTO)
    debug_io << "Connecting to '" << host << ":" << port << "'\n";
  
  // getting host
/*  
  c_string _host (host);
  struct hostent *hp = gethostbyname (_host);
  if (hp == NULL) return "Error: no connection for '" * host * "'";

  // creating socket
  io= socket (AF_INET, SOCK_STREAM, 0);
  if (io < 0) return "Error: socket could not be created";

   // connecting to socket
  struct sockaddr_in insock;
  string where= host * ":" * as_string (port);
  memset ((char*) &insock, 0, sizeof (insock));
  insock.sin_family = AF_INET;
  insock.sin_port = htons ((unsigned short) port);
  memcpy ((char*) &insock.sin_addr, hp->h_addr, hp->h_length);
  if (connect (io, (struct sockaddr*) &insock, sizeof (insock)) < 0)
    return "Error: refused connection to '" * where * "'";

*/
// the above original code used deprecated function gethostbyname.
// this was signaled in the tracker (#34182, #46726)
// Below the functionality is implemented using getaddrinfo,
// (closely following the client example given in man pages)
// same thing done also in QTMSockets.cpp
  c_string _host (host);
  c_string _port (as_string (port));
  string where= host * ":" * as_string (port);
  struct addrinfo hints;
  struct addrinfo *result, *rp;
  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_UNSPEC;    /* Allow IPv4 or IPv6, AF_INET for v4 only*/
  hints.ai_socktype = SOCK_STREAM; /* stream socket */
  hints.ai_flags = AI_PASSIVE;    /* For wildcard IP address */
  hints.ai_protocol = 0;          /* Any protocol */
  hints.ai_canonname = NULL;
  hints.ai_addr = NULL;
  hints.ai_next = NULL;

  int x =  getaddrinfo(_host, _port, &hints, &result);
  if (x != 0) return "Error: no address found for '" * host * "'";
  // getaddrinfo may return several addresses
  // trying them until we can connect
  for (rp = result; rp != NULL; rp = rp->ai_next) {
    io = socket (rp->ai_family, rp->ai_socktype, rp->ai_protocol);
    if (io == -1)
      continue;

    if (connect (io, rp->ai_addr, rp->ai_addrlen) != -1)
      break;                  /* Success */

    close(io);
  }

  if (rp == NULL) {               /* No address succeeded */
               return "Error: could not connect to '" * where * "'";
  }

  freeaddrinfo(result);           /* No longer needed */

  // testing whether it works
#ifdef OS_MINGW
  unsigned long flags = -1;
  if (ioctlsocket (io, FIONBIO, &flags) == SOCKET_ERROR)
    return "non working connection to '" * where * "'";
#else
  int flags = O_NONBLOCK;
  if (fcntl (io, F_SETFL, flags) < 0)
    return "Error: non working connection to '" * where * "'";
#endif
  alive = true;
  sn = socket_notifier (io, &socket_callback, this, NULL);  
  add_notifier (sn);
  return "ok";
}

static string
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

static int
send_all (int s, char *buf, int *len) {
#ifdef OS_MINGW
  using namespace wsoc;
#endif
  int total= 0;          // how many bytes we've sent
  int bytes_left= *len;  // how many we have left to send
  int n= 0;

  while (total < *len) {
    n= send (s, buf + total, bytes_left, 0);
    if (n == -1) break;
    total += n;
    bytes_left -= n;
  }

  *len= total;
  return n==-1? -1: 0;
} 


void
socket_link_rep::write (string s, int channel) {
  if ((!alive) || (channel != LINK_IN)) return;
  if (DEBUG_IO) debug_io << "---> " << debug_io_string (s) << "\n";
  int len= N(s);
  if (send_all (io, &(s[0]), &len) == -1) {
    io_error << "Write to '" << host << ":" << port << "' failed\n";
    stop ();
  }
}

void
socket_link_rep::feed (int channel) {
#ifdef OS_MINGW
  using namespace wsoc;
#endif
  if ((!alive) || (channel != LINK_OUT)) return;
  char tempout[1024];
  int r= recv (io, tempout, 1024, 0);
  if (r <= 0) {
    if (r == 0) debug_io << host << ":" << port << "' hung up\n";
    else io_warning << "TeXmacs] read failed from '" << host
                    << ":" << port << "'\n";
    stop ();
  }
  else if (r != 0) {
    if (DEBUG_IO) debug_io << debug_io_string (string (tempout, r));
    outbuf << string (tempout, r);
#ifdef QT_CPU_FIX
    tm_wake_up ();
#endif
  }
}

string&
socket_link_rep::watch (int channel) {
  static string empty_string= "";
  if (channel == LINK_OUT) return outbuf;
  else return empty_string;
}

string
socket_link_rep::read (int channel) {
  if (channel == LINK_OUT) {
    string r= outbuf;
    outbuf= "";
    return r;
  }
  else return "";
}

void
socket_link_rep::listen (int msecs) {
#ifdef OS_MINGW
  using namespace wsoc;
#endif
  if (!alive) return;
  fd_set rfds;
  FD_ZERO (&rfds);
  FD_SET (io, &rfds);
  struct timeval tv;
  tv.tv_sec  = msecs / 1000;
  tv.tv_usec = 1000 * (msecs % 1000);
  int nr= select (io+1, &rfds, NULL, NULL, &tv);
  if (nr != 0 && FD_ISSET (io, &rfds)) feed (LINK_OUT);
}

void
socket_link_rep::interrupt () {
}

void
socket_link_rep::stop () {
#ifdef OS_MINGW
  using namespace wsoc;
#endif
  if (!alive) return;
  if (type == SOCKET_SERVER) call ("server-remove", object (io));
  else if (type == SOCKET_CLIENT) call ("client-remove", object (io));
  close (io);
  io= -1;
  alive= false;
  remove_notifier (sn);
  sn = socket_notifier ();
#ifdef OS_MINGW
  closesocket (io);
  WSACleanup();
#else
  //  close (io);  //fix it?
  wait (NULL);
#endif
  
}

/******************************************************************************
* Call back for new information on socket
******************************************************************************/

void
socket_callback (void *obj, void* info) {
#ifdef OS_MINGW
  using namespace wsoc;
#endif
  (void) info;
  socket_link_rep* con= (socket_link_rep*) obj;
  if (!con->alive) {
    //cout << "con= " << con << ", " << con->alive << ", " << con->io << "\n";
    io_warning << "invalid callback invocation of deleted socket link\n";
    return;
  }
  bool busy= true;
  bool news= false;
  while (busy) {
    fd_set rfds;
    FD_ZERO (&rfds);
    int max_fd= con->io + 1;
    FD_SET (con->io, &rfds);
  
    struct timeval tv;
    tv.tv_sec  = 0;
    tv.tv_usec = 0;
    select (max_fd, &rfds, NULL, NULL, &tv);

    busy= false;
    if (con->alive && FD_ISSET (con->io, &rfds)) {
      //cout << "socket_callback OUT" << LF;
      con->feed (LINK_OUT);
      busy= news= true;
      if (!con->alive) break;
    }
  }
  if (!is_nil (con->feed_cmd) && news)
    con->feed_cmd->apply ();
}
#endif
