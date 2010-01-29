
/******************************************************************************
* MODULE     : unix_sys_utils.cpp
* DESCRIPTION: external command handling
* COPYRIGHT  : (C) 2009  David MICHEL
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "unix_sys_utils.hpp"
#include "file.hpp"
#include <stdlib.h>

int
unix_system (string s) {
  char* _s = as_charp (s * " > /dev/null 2>&1");
  int ret = system (_s);
  tm_delete_array (_s);  
  return ret;
}

int
unix_system (string cmd, string& result) {
  url temp= url_temp ();
  string temp_s= escape_sh (concretize (temp));
  char* _cmd = as_charp (cmd * " > " * temp_s * " 2>&1");
  int ret = system (_cmd);
  tm_delete_array (_cmd);
  bool flag= load_string (temp, result, false);
  remove (temp);
  if (flag) result= "";
  return ret;
}

