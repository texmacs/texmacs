
/******************************************************************************
* MODULE     : connect.hpp
* DESCRIPTION: Connection of extern packages to TeXmacs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef CONNECT_H
#define CONNECT_H
#include "pipe_link.hpp"

bool   connection_declared (string name);
tree   connection_info (string name, string session);
tree   connection_handlers (string name);
string connection_start (string name, string session, bool again= false);
void   connection_write (string name, string session, string s);
void   connection_write (string name, string session, tree t);
tree   connection_read (string name, string session, string channel= "output");
void   connection_interrupt (string name, string session);
void   connection_stop (string name, string session);
void   connection_stop_all ();
int    connection_status (string name, string session);
tree   connection_eval (string name, string session, string s);
tree   connection_eval (string name, string session, tree t);
tree   connection_cmd (string name, string session, string s);
void   listen_to_connections ();

#endif // defined CONNECT_H
