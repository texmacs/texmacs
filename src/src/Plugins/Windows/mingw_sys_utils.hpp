
/******************************************************************************
* MODULE     : mingw_sys_utils.hpp
* DESCRIPTION: external command handling
* COPYRIGHT  : (C) 2015 Denis RAUX
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef MINGW_SYS_UTILS_H
#define MINGW_SYS_UTILS_H

#include "string.hpp"
#include "array.hpp"

int mingw_system (array< ::string> arg,
                  array<int> fd_in, array< ::string> str_in,
                  array<int> fd_out, array< ::string*> str_out);

namespace sys_utils {
  ::string mingw_get_username ();
}

#endif // defined MINGW_SYS_UTILS_H
