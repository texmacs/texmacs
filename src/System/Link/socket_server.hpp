
/******************************************************************************
* MODULE     : socket_server.cpp
* DESCRIPTION: TeXmacs servers over sockets
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SOCKET_SERVER_H
#define SOCKET_SERVER_H
#include "tm_link.hpp"
#include "socket_notifier.hpp"

/******************************************************************************
* The socket_server class
******************************************************************************/

struct socket_server_rep: tm_link_rep {
  int port;                 // port for the socket
  int server;               // listening socket descriptor
  array<tm_link> incoming;  // list of clients

  socket_notifier sn;
  
public:
  socket_server_rep (int port);
  ~socket_server_rep ();

  string  start ();
  void    write (string s, int channel);
  string& watch (int channel);
  string  read (int channel);
  void    listen (int msecs);
  void    interrupt ();
  void    stop ();

  void    start_client ();
};

int  number_of_servers ();
void close_all_servers ();

#endif // SOCKET_SERVER_H
