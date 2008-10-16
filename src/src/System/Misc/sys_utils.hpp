
/******************************************************************************
* MODULE     : sys_utils.hpp
* DESCRIPTION: system utilities
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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

#endif // defined SYS_UTILS_H
