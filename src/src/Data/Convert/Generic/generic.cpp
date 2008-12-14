
/******************************************************************************
* MODULE     : generic.cpp
* DESCRIPTION: routines for generic formats
* COPYRIGHT  : (C) 2000  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "file.hpp"
#include "scheme.hpp"

bool
is_snippet (tree doc) {
  if (!is_document (doc)) return true;
  int i, n= N(doc);
  for (i=0; i<n; i++)
    if (is_compound (doc[i], "TeXmacs", 1))
      return false;
  return true;
}

string
get_texmacs_path () {
  string tmpath= get_env ("TEXMACS_PATH");
  while ((N(tmpath)>0) && (tmpath [N(tmpath) - 1] == '/'))
    tmpath= tmpath (0, N(tmpath)-1);
  return tmpath;
}

string
suffix_to_format (string suffix) {
  return as_string (call ("format-from-suffix", suffix));
}

string
format_to_suffix (string fm) {
  return as_string (call ("format-default-suffix", fm));
}

string
get_format (string s, string suffix) {
  return as_string (call ("format-determine", s, suffix));
}

tree
generic_to_tree (string s, string fm) {
  return as_tree (call ("generic->texmacs", s, fm));
}

string
tree_to_generic (tree doc, string fm) {
  return as_string (call ("texmacs->generic", doc, fm));
}
