
/******************************************************************************
* MODULE     : sqlite3.hpp
* DESCRIPTION: interface with Sqlite3
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TM_SQLITE3_H
#define TM_SQLITE3_H
#include "url.hpp"

bool sqlite3_present ();
tree sql_exec (url db_name, string cmd);
string sql_quote (string s);

#endif // TM_SQLITE3_H
