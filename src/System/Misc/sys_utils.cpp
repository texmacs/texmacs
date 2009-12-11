
/******************************************************************************
* MODULE     : sys_utils.cpp
* DESCRIPTION: file handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "sys_utils.hpp"
#include "file.hpp"
#include "tree.hpp"
#ifdef OS_WIN32
#include <sys/misc.h>
#endif

#if defined (QTTEXMACS) && (defined (__MINGW__) || defined (__MINGW32__))
#include "../../Plugins/Qt/qt_sys_utils.hpp"
#else
#include "../../Plugins/Unix/unix_sys_utils.hpp"
#endif

int script_status = 1;

/******************************************************************************
* System functions
******************************************************************************/

int
system (string s) {
  if (DEBUG_STD) cerr << "TeXmacs] System: " << s << "\n";
#if defined (QTTEXMACS) && (defined (__MINGW__) || defined (__MINGW32__))
//  if (starts (s, "convert ")) return 1;
  string result;
  return qt_system (s, result);
#else
  return unix_system (s);
#endif
}

string
eval_system (string s) {
#if defined (QTTEXMACS) && (defined (__MINGW__) || defined (__MINGW32__))
  string result;
  qt_system (s, result);
  return result;
#else
  return unix_eval_system (s);
#endif

}

string
var_eval_system (string s) {
  string r= eval_system (s);
  while ((N(r)>0) && (r[N(r)-1]=='\n')) r= r (0,N(r)-1);
  return r;
}

string
get_env (string var) {
  char* _var= as_charp (var);
  char* _ret= getenv (_var);
  tm_delete_array (_var);
  if (_ret==NULL) {
    if (var == "PWD") return get_env ("HOME");
    return "";
  }
  string ret (_ret);
  return ret;
  // do not delete _ret !
}

void
set_env (string var, string with) {
#if defined(STD_SETENV) && !defined(__MINGW32__)
  char* _var = as_charp (var);
  char* _with= as_charp (with);
  setenv (_var, _with, 1);
  tm_delete_array(_var);
  tm_delete_array(_with);
#else
  char* _varw= as_charp (var * "=" * with);
  (void) putenv (_varw);
  // do not delete _varw !!!
  // -> known memory leak, but solution more complex than it is worth
#endif
}
