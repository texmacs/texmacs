
/******************************************************************************
* MODULE     : sys_utils.cpp
* DESCRIPTION: file handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "sys_utils.hpp"
#include "file.hpp"
#include "tree.hpp"
#ifdef OS_WIN32
#include <sys/misc.h>
#endif

int script_status = 1;

/******************************************************************************
* System functions
******************************************************************************/

int
system (string s) {
  // cout << "System: " << s << "\n";
  char* _s= as_charp (s);
#ifdef OS_WIN32
  int r= _system (_s);
#else
  int r= system (_s);
#endif
  delete[] _s;
  return r;
}

string
eval_system (string s) {
  url temp= url_temp ();
  string temp_s= escape_sh (concretize (temp));
#ifdef OS_WIN32
  system (s * " > \"" * temp_s * "\"");
#else
  system (s * " > " * temp_s);
#endif
  string result;
  bool flag= load_string (temp, result, false);
#ifdef OS_WIN32
  system ("rm \"" * temp_s * "\"");
#else
  system ("rm " * temp_s);
#endif
  if (flag) {
#ifdef OS_WIN32
    cerr << "TeXmacs] failed: " << s * " > " * temp_s << "\n";
#endif
    return "";
  }
  return result;
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
  delete[] _var;
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
#ifdef STD_SETENV
  char* _var = as_charp (var);
  char* _with= as_charp (with);
  setenv (_var, _with, 1);
#else
  char* _varw= as_charp (var * "=" * with);
  (void) putenv (_varw);
#endif
  // do not delete _var and _with !!!
}
