
/******************************************************************************
* MODULE     : socket_link.cpp
* DESCRIPTION: TeXmacs links by sockets
* COPYRIGHT  : (C) 2000  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SOCKET_LINK_H
#define SOCKET_LINK_H
#include "tm_link.hpp"
#include "socket_notifier.hpp"

/******************************************************************************
* The socket_link class
******************************************************************************/

struct socket_link_rep: tm_link_rep {
  string host;          // host for the socket
  int    port;          // port for the socket
  int    type;          // socket type
  int    io;            // file descriptor for data going to the child
  string outbuf;        // pending output from plugin

  socket_notifier sn;
  
public:
  socket_link_rep (string host, int port, int type, int fd);
  ~socket_link_rep ();

  string  start ();
  void    write (string s, int channel);
  string& watch (int channel);
  string  read (int channel);
  void    listen (int msecs);
  void    interrupt ();
  void    stop ();

  void    feed (int channel);
};

#endif // SOCKET_LINK_H
