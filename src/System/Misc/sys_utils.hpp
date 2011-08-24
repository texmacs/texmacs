
/******************************************************************************
* MODULE     : sys_utils.hpp
* DESCRIPTION: system utilities
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SYS_UTILS_H
#define SYS_UTILS_H
#include "string.hpp"

extern int script_status; // 0: never accept, 1: prompt, 2: always accept

int    system (string s);
string eval_system (string s);
string var_eval_system (string s);
string get_env (string var);
void   set_env (string var, string with);
string get_stacktrace (unsigned int max_frames= 127);

#endif // defined SYS_UTILS_H
