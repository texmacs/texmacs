
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
  string temp_s= concretize (temp);
  system (s * " > " * temp_s);
  string result;
  bool flag= load_string (temp, result, false);
  system ("rm \"" * temp_s * "\"");
  if (flag) return "";
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

static tree
analyze (string s, char c) {
  tree t (TUPLE);
  int i=0, last= 0, n= N(s);
  while ((i<n) && (s[i]==c)) i++;
  for (; i<n; i++)
    if (s[i] == c) {
      t << s (last, i);
      while ((i<n) && (s[i]==c)) i++;
      last= i; i--;
    }
  if ((n>0) && (s[n-1]!=c))
    t << s (last, n);
  return t;
}

static int
search_pos (tree t, string what) {
  int i, n= N(t);
  for (i=0; i<n; i++)
    if (t[i] == what)
      return i;
  return -1;
}

static bool
check_pos (tree t, int pos, string what) {
  int i, n= N(t);
  for (i=0; i<n; i++)
    if ((N(t[i])>pos) && (t[i][pos] == what))
      return true;
  return false;
}

void
recursive_kill (int pid) {
  string s= eval_system ("ps -l");
  int i, n= N(s);
  for (i=0; i<n; i++)
    if (s[i] == '\t')
      s[i]= ' ';
  tree t= analyze (s, '\n');
  n= N(t);
  for (i=0; i<n; i++)
    t[i]= analyze (t[i]->label, ' ');

  if (n>1) {
    int pid_pos = search_pos (t[0], "PID");
    int ppid_pos= search_pos (t[0], "PPID");
    if (pid_pos  == -1) pid_pos = search_pos (t[0], "pid");
    if (ppid_pos == -1) ppid_pos= search_pos (t[0], "ppid");
    if (pid_pos  == -1) pid_pos = search_pos (t[0], "Pid");
    if (ppid_pos == -1) ppid_pos= search_pos (t[0], "Ppid");
    if (ppid_pos == -1) ppid_pos= search_pos (t[0], "PPid");
    if (pid_pos  == -1) pid_pos = 3;
    if (ppid_pos == -1) ppid_pos= 4;
    if (check_pos (t, pid_pos, as_string (pid)) &&
	check_pos (t, ppid_pos, as_string (pid)))
      {
	for (i=0; i<n; i++)
	  if (t[i][ppid_pos] == as_string (pid))
	    recursive_kill (as_int (t[i][pid_pos]));
      }
  }
  // cout << "Killing " << pid << "\n";
  system ("kill -9 " * as_string (pid) * " 2> /dev/null");
}
