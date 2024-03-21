
/******************************************************************************
* MODULE     : unix_sys_utils.hpp
* DESCRIPTION: external command handling
* COPYRIGHT  : (C) 2009  David MICHEL
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef UNIX_SYS_UTILS_H
#define UNIX_SYS_UTILS_H

#include "string.hpp"
#include "array.hpp"

int unix_system (string);
int unix_system (string, string&);
int unix_system (string, string&, string&);

int unix_system (array<string> arg,
		 array<int> fd_in, array<string> str_in,
		 array<int> fd_out, array<string*> str_out);

string unix_get_login ();
string unix_get_username ();

#endif // defined UNIX_SYS_UTILS_H

