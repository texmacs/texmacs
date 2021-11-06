
/******************************************************************************
* MODULE     : sys_utils.hpp
* DESCRIPTION: system utilities
* COPYRIGHT  : (C) 1999-2016  Joris van der Hoeven, Denis Raux
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SYS_UTILS_H
#define SYS_UTILS_H
#include "string.hpp"
#include "url.hpp"
#include "array.hpp"

extern int script_status; // 0: never accept, 1: prompt, 2: always accept

int    system (string s);
int    system (string s, string &r);
int    system (string s, string &r, string& e);
string eval_system (string s);
string var_eval_system (string s);
string get_env (string var);
void   set_env (string var, string with);
int    os_version ();
string get_stacktrace (unsigned int max_frames= 127);

url get_texmacs_path ();
url get_texmacs_home_path ();

array<string> evaluate_system (array<string> arg,
			       array<int> fd_in, array<string> in,
			       array<int> fd_out);

string get_printing_default ();
bool has_printing_cmd (void);
string get_printing_cmd (void);
void set_printing_cmd (string cmd);

string get_user_login ();
string get_user_name ();

#endif // defined SYS_UTILS_H
