
/******************************************************************************
* MODULE     : client_server.hpp
* DESCRIPTION: TeXmacs clients and servers
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
