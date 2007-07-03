
/******************************************************************************
* MODULE     : scheme_language.cpp
* DESCRIPTION: the "scheme" language
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "analyze.hpp"
#include "language.hpp"
#include "Languages/impl_language.hpp"
#include "Scheme/object.hpp"

scheme_language_rep::scheme_language_rep (string name):
  language_rep (name), colored ("")
{
  eval ("(use-modules (utils misc tm-keywords))");
  list<string> l= as_list_string (eval ("(map symbol->string highlight-any)"));
  while (!nil (l)) {
    colored (l->item)= "blue";
    l= l->next;
  }
}

text_property
scheme_language_rep::advance (string s, int& pos) {
  if (pos==N(s)) return &tp_normal_rep;
  switch (s[pos]) {
  case ' ':
    pos++;
    return &tp_space_rep;
  case '(':
  case ')':
    pos++;
    return &tp_normal_rep;
  }
  while ((pos<N(s)) && (s[pos]!=' ') && (s[pos]!='(') && (s[pos]!=')')) pos++;
  return &tp_normal_rep;
}

array<int>
scheme_language_rep::get_hyphens (string s) {
  int i;
  array<int> penalty (N(s)+1);
  for (i=0; i<N(penalty); i++) penalty[i]= HYPH_INVALID;
  return penalty;
}

void
scheme_language_rep::hyphenate (
  string s, int after, string& left, string& right)
{ 
  left = s(0, after);
  right= s(after, N(s));
}

string
scheme_language_rep::get_color (tree t, int start, int end) {
  static string none= "";
  if (start >= end) return none;
  string s= t->label;
  for (int i=0; i<=start; i++)
    switch (s[i]) {
    case ';': return "brown";
    case '\042':
      i++;
      while (i <= start && s[i] != '\042')
	if (s[i] == '\\' && i < start) i += 2;
	else i++;
      if (i >= start) return "dark grey";
      break;
    }
  if (is_numeric (s[start]) || s[start] == '\042' || s[start] == '#')
    return "dark grey";
  if (s[start] == ':') return "dark magenta";
  string r= s (start, end);
  if (!colored->contains (r)) {
    colored (r)= "";
    if (as_bool (call ("defined?", symbol_object (tm_decode (r)))))
      colored (r)= "#303080";
  }
  return colored[r];
}
