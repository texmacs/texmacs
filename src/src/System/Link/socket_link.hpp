
/******************************************************************************
* MODULE     : socket_link.cpp
* DESCRIPTION: TeXmacs links by sockets
* COPYRIGHT  : (C) 2000  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef SOCKET_LINK_H
#define SOCKET_LINK_H
#include "tm_link.hpp"

/******************************************************************************
* The socket_link class
******************************************************************************/

struct socket_link_rep: tm_link_rep {
  string host;          // host for the socket
  int    port;          // port for the socket
  int    io;            // file descriptor for data going to the child
  string outbuf;        // pending output from plugin

public:
  socket_link_rep (string host, int port);
  ~socket_link_rep ();

  string start ();
  void   write (string s, int channel);
  string read (int channel);
  void   listen (int msecs);
  void   interrupt ();
  void   stop ();

  void   feed (int channel);
};

void listen_to_sockets ();
void close_all_sockets ();

#endif // SOCKET_LINK_H
