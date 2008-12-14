
/******************************************************************************
* MODULE     : client_server.hpp
* DESCRIPTION: TeXmacs clients and servers
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef CLIENT_SERVER_H
#define CLIENT_SERVER_H
#include "string.hpp"

void   server_start ();
void   server_stop ();
string server_read (int fd);
void   server_write (int fd, string s);

void   client_start (string host);
void   client_stop ();
string client_read ();
void   client_write (string s);

void   enter_secure_mode ();

#endif // defined CLIENT_SERVER_H
