
/******************************************************************************
* MODULE     : persistent.hpp
* DESCRIPTION: persistent caching of string key-value pairs
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PERSISTENT_H
#define PERSISTENT_H
#include "file.hpp"

void persistent_set (url dir, string key, string val);
void persistent_reset (url dir, string key);
bool persistent_contains (url dir, string key);
string persistent_get (url dir, string key);
url persistent_file_name (url dir, string suffix);

#endif // defined PERSISTENT_H
