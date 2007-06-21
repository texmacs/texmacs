
/******************************************************************************
* MODULE     : socket_server.cpp
* DESCRIPTION: TeXmacs servers over sockets
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef SOCKET_SERVER_H
#define SOCKET_SERVER_H
#include "tm_link.hpp"

/******************************************************************************
* The socket_server class
******************************************************************************/

struct socket_server_rep: tm_link_rep {
  int port;                 // port for the socket
  int server;               // listening socket descriptor
  array<tm_link> incoming;  // list of clients

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
void listen_to_servers ();
void close_all_servers ();

#endif // SOCKET_SERVER_H
