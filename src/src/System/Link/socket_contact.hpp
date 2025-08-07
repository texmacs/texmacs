
/******************************************************************************
* MODULE     : socket_contact.hpp
* DESCRIPTION: TeXmacs raw contacts for sockets
* COPYRIGHT  : (C) 2022  Gregoire Lecerf
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SOCKET_CONTACT_H
#define SOCKET_CONTACT_H
#include "tm_contact.hpp"

/******************************************************************************
* The socket contact class for TeXmacs clients
******************************************************************************/

struct socket_client_contact_rep: tm_contact_rep {
  int io, error_number;
public:
  socket_client_contact_rep (array<array<string> > credentials) :
    tm_contact_rep (credentials), io (-1), error_number (0) {
    type= SOCKET_CLIENT; }
  ~socket_client_contact_rep () {}
  void start (int io2) { io= io2; error_number= 0; }
  void stop () { io= -1; }
  int send (const void* buffer, size_t length);
  int receive (void* buffer, size_t length);
  bool active () { return io > 0; };
  string last_error ();
};

inline tm_contact 
make_socket_client_contact (array<array<string> > credentials
			    = array<array<string> > ()) {
  return tm_contact ((tm_contact_rep*)
		     tm_new<socket_client_contact_rep> (credentials)); }

/******************************************************************************
* The socket contact class for TeXmacs servers
******************************************************************************/

struct socket_server_contact_rep: tm_contact_rep {
  int io, error_number;
public:
  socket_server_contact_rep (array<array<string> > authentications) :
    tm_contact_rep (authentications), io (-1), error_number (0) {
    type= SOCKET_SERVER; }
  ~socket_server_contact_rep () {}
  void start (int io2) { io= io2; error_number= 0; }
  void stop () { io= -1; }
  int send (const void* buffer, size_t length);
  int receive (void* buffer, size_t length);
  bool active () { return io > 0; }
  string last_error ();
};

inline tm_contact 
make_socket_server_contact (array<array<string> > authentications=
			    array<array<string> > ()) {
  return tm_contact ((tm_contact_rep*)
		     tm_new<socket_server_contact_rep> (authentications)); }

#endif // SOCKET_CONTACT_H
