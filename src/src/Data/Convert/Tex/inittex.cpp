
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

static int
latex_arity_func (string s) {
  return as_int (call ("latex-arity", s));
}

static string
latex_type_func (string s) {
  return as_string (call ("latex-type", s));
}

rel_hashmap<string,string> command_type ("undefined");
rel_hashmap<string,int>    command_arity (0);
rel_hashmap<string,string> command_def ("undefined");
hashfunc<string,int>       latex_arity (latex_arity_func, 0);
hashfunc<string,string>    latex_type (latex_type_func, "undefined");
