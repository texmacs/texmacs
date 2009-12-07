
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

#include "socket_link.hpp"
#include "sys_utils.hpp"
#include "hashset.hpp"
#include "iterator.hpp"
#include "timer.hpp"
#include "Scheme/object.hpp"
#include <stdio.h>
#include <string.h>
#ifndef __MINGW32__
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
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
  if (type == SOCKET_SERVER) call ("server-add", object (io));
  else if (type == SOCKET_CLIENT) call ("client-add");
}

socket_link_rep::~socket_link_rep () {
  stop ();
  socket_link_set->remove ((pointer) this);
}

tm_link
make_socket_link (string host, int port, int type, int fd) {
  return tm_new<socket_link_rep> (host, port, type, fd);
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

/******************************************************************************
* Routines for socket_links
******************************************************************************/

string
socket_link_rep::start () {
#ifndef __MINGW32__
  if (alive) return "busy";
  if (DEBUG_AUTO)
    cout << "TeXmacs] Connecting to '" << host << ":" << port << "'\n";
  
  // getting host
  char* _host= as_charp (host);
  struct hostent *hp = gethostbyname (_host);
  tm_delete_array (_host);
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

  // testing whether it works
  int flags = O_NONBLOCK;
  if (fcntl (io, F_SETFL, flags) < 0)
    return "Error: non working connection to '" * where * "'";
  alive = true;
  sn = socket_notifier (io, &socket_callback, this, NULL);  
  return "ok";
#else
  return "Error: sockets not implemented";
#endif
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
#ifndef __MINGW32__
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
#else
  return 0;
#endif
} 


void
socket_link_rep::write (string s, int channel) {
  if ((!alive) || (channel != LINK_IN)) return;
  if (DEBUG_IO) cout << "---> " << debug_io_string (s) << "\n";
  int len= N(s);
  if (send_all (io, &(s[0]), &len) == -1) {
    cerr << "TeXmacs] write to '" << host << ":" << port << "' failed\n";
    stop ();
  }
}

void
socket_link_rep::feed (int channel) {
#ifndef __MINGW32__
  if ((!alive) || (channel != LINK_OUT)) return;
  char tempout[1024];
  int r= recv (io, tempout, 1024, 0);
  if (r <= 0) {
    if (r == 0) cout << "TeXmacs] '" << host << ":" << port << "' hung up\n";
    else cerr << "TeXmacs] read failed from '" << host << ":" << port << "'\n";
    stop ();
  }
  else if (r != 0) {
    if (DEBUG_IO) cout << debug_io_string (string (tempout, r));
    outbuf << string (tempout, r);
  }

  if (!is_nil(feed_cmd)) (feed_cmd)->apply();
#endif
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
#ifndef __MINGW32__
  if (!alive) return;
  fd_set rfds;
  FD_ZERO (&rfds);
  FD_SET (io, &rfds);
  struct timeval tv;
  tv.tv_sec  = msecs / 1000;
  tv.tv_usec = 1000 * (msecs % 1000);
  int nr= select (io+1, &rfds, NULL, NULL, &tv);
  if (nr != 0 && FD_ISSET (io, &rfds)) feed (LINK_OUT);
#endif
}

void
socket_link_rep::interrupt () {
}

void
socket_link_rep::stop () {
#ifndef __MINGW32__
  if (!alive) return;
  if (type == SOCKET_SERVER) call ("server-remove", object (io));
  else if (type == SOCKET_CLIENT) call ("client-remove");
  close (io);
  io= -1;
  alive= false;
  sn = socket_notifier ();
  wait (NULL);
#endif
}

/******************************************************************************
* Call back for new information on socket
******************************************************************************/

void
socket_callback (void *obj, void* info) {
#ifndef __MINGW32__
  socket_link_rep* con= (socket_link_rep*) obj;  
  bool busy= true;
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
      busy= true;
    }
  }
#endif
}

/******************************************************************************
* Listen to all active sockets (may be optimized for speed)
******************************************************************************/

void
listen_to_sockets () {
#ifndef __MINGW32__
  while (true) {
    fd_set rfds;
    FD_ZERO (&rfds);
    int max_fd= 0;
    iterator<pointer> it= iterate (socket_link_set);
    while (it->busy()) {
      socket_link_rep* con= (socket_link_rep*) it->next();
      if (con->alive) {
	FD_SET (con->io, &rfds);
	if (con->io >= max_fd) max_fd= con->io+1;
      }
    }
    if (max_fd == 0) break;

    struct timeval tv;
    tv.tv_sec  = 0;
    tv.tv_usec = 0;
    int nr= select (max_fd, &rfds, NULL, NULL, &tv);
    if (nr==0) break;

    it= iterate (socket_link_set);
    while (it->busy()) {
      socket_link_rep* con= (socket_link_rep*) it->next();
      if (con->alive && FD_ISSET (con->io, &rfds)) con->feed (LINK_OUT);
    }
  }
#endif
}
