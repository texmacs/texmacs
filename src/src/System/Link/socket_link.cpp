
/******************************************************************************
* MODULE     : socket_link.cpp
* DESCRIPTION: TeXmacs links by sockets
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "socket_link.hpp"
#include "sys_utils.hpp"
#include "hashset.hpp"
#include "iterator.hpp"
#include "timer.hpp"
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>

hashset<pointer> socket_link_set;

/******************************************************************************
* Constructors and destructors for socket_links
******************************************************************************/

socket_link_rep::socket_link_rep (string host2, int port2):
  host (host2), port (port2)
{
  socket_link_set->insert ((pointer) this);
  io     = -1;
  outbuf = "";
  alive  = false;
}

socket_link_rep::~socket_link_rep () {
  stop ();
  socket_link_set->remove ((pointer) this);
}

tm_link
make_socket_link (string host, int port) {
  return new socket_link_rep (host, port);
}

/******************************************************************************
* Routines for socket_links
******************************************************************************/
#define TERMCHAR '\1'

string
socket_link_rep::start () {
  if (alive) return "busy";
  if (DEBUG_AUTO)
    cout << "TeXmacs] Connecting to '" << host << ":" << port << "'\n";
  
  // getting host
  char* _host= as_charp (host);
  struct hostent *hp = gethostbyname (_host);
  delete[] _host;
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
  alive= true;
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

void
socket_link_rep::write (string s, int channel) {
  if ((!alive) || (channel != LINK_IN)) return;
  if (DEBUG_IO) cout << "---> " << debug_io_string (s) << "\n";
  char* _s= as_charp (s);
  ::write (io, _s, N(s));
  delete[] _s;
}

void
socket_link_rep::feed (int channel) {
  if ((!alive) || (channel != LINK_OUT)) return;

  int r;
  char tempout[1024];
  r = ::read (io, tempout, 1024);
  if (r == ERROR) {
    cerr << "TeXmacs] read failed from#'" << host << ":" << port << "'\n";
    wait (NULL);
  }
  else if (r != 0) {
    if (DEBUG_IO) cout << debug_io_string (string (tempout, r));
    outbuf << string (tempout, r);
  }
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
  int wait_until= texmacs_time () + msecs;
  while (outbuf == "") {
    listen_to_sockets (); // FIXME: should listen more specifically
    if (texmacs_time () > wait_until) break;
  }
}

void
socket_link_rep::interrupt () {
}

void
socket_link_rep::stop () {
  if (!alive) return;
  alive= false;
  close (io);
  wait (NULL);
}

/******************************************************************************
* Listen to all active sockets (may be optimized for speed)
******************************************************************************/

void
listen_to_sockets () {
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
}

/******************************************************************************
* Emergency exit for all sockets
******************************************************************************/

void
close_all_sockets () {
  iterator<pointer> it= iterate (socket_link_set);
  while (it->busy()) {
    socket_link_rep* con= (socket_link_rep*) it->next();
    if (con->alive) {
      close (con->io);
      con->alive= false;
    }
  }
}
