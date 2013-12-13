
/******************************************************************************
* MODULE     : inittex.cpp
* DESCRIPTION: initialize conversion from and to TeX
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Tex/convert_tex.hpp"
#include "rel_hashmap.hpp"
#include "scheme.hpp"

static string
paper_opts_func (string s) {
  return as_string (call ("latex-paper-opts", s));
}

static string
paper_type_func (string s) {
  return as_string (call ("latex-paper-type", s));
}

static string
latex_type_func (string s) {
  return as_string (call ("latex-type", s));
}

static int
latex_arity_func (string s) {
  return as_int (call ("latex-arity", s));
}

hashfunc<string,string>    paper_std_opts (paper_opts_func, "undefined");
hashfunc<string,string>    paper_std_type (paper_type_func, "undefined");
hashfunc<string,string>    latex_std_type (latex_type_func, "undefined");
hashfunc<string,int>       latex_std_arity (latex_arity_func, 0);

static array<string> empty_array_string;

rel_hashmap<string,string> command_type ("undefined");
rel_hashmap<string,int>    command_arity (0);
rel_hashmap<string,array<string> > command_def (empty_array_string);

string
paper_opts (string s) {
  return paper_std_opts [s];
}

string
paper_type (string s) {
  return paper_std_type [s];
}

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
