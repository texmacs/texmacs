
/******************************************************************************
* MODULE     : inittex.cpp
* DESCRIPTION: initialize conversion from and to TeX
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Tex/convert_tex.hpp"
#include "rel_hashmap.hpp"
#include "scheme.hpp"

static string
latex_type_func (string s) {
  return as_string (call ("latex-type", s));
}

static int
latex_arity_func (string s) {
  return as_int (call ("latex-arity", s));
}

hashfunc<string,string>    latex_std_type (latex_type_func, "undefined");
hashfunc<string,int>       latex_std_arity (latex_arity_func, 0);

rel_hashmap<string,string> command_type ("undefined");
rel_hashmap<string,int>    command_arity (0);
rel_hashmap<string,string> command_def ("undefined");

string
latex_type (string s) {
  if (command_type->contains (s)) return command_type[s];
  else return latex_std_type [s];
}

int
latex_arity (string s) {
  if (command_arity->contains (s)) return command_arity[s];
  else return latex_std_arity [s];
}
