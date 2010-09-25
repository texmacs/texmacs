
/******************************************************************************
* MODULE     : scheme_language.cpp
* DESCRIPTION: the "scheme" language
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "analyze.hpp"
#include "impl_language.hpp"
#include "Scheme/object.hpp"

scheme_language_rep::scheme_language_rep (string name):
  language_rep (name), colored ("")
{
  eval ("(use-modules (utils misc tm-keywords))");
  list<string> l= as_list_string (eval ("(map symbol->string highlight-any)"));
  while (!is_nil (l)) {
    colored (l->item)= "#309090";
    l= l->next;
  }
}

text_property
scheme_language_rep::advance (tree t, int& pos) {
  string s= t->label;
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
  if (pos < N(s) && pos >= 2 && (s[pos] == '(' || s[pos] == ')'))
    if (s[pos-2] == '#' && s[pos-1] == '\\') pos++;
  return &tp_normal_rep;
}

array<int>
scheme_language_rep::get_hyphens (string s) {
  int i;
  array<int> penalty (N(s)+1);
  penalty[0]= HYPH_INVALID;
  for (i=1; i<N(s); i++)
    if (s[i-1] == '-' && is_alpha (s[i]))
      penalty[i]= HYPH_STD;
    else penalty[i]= HYPH_INVALID;
  penalty[i]= HYPH_INVALID;
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
  for (int i= max (0, start-1000); i <= start; i++)
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
      colored (r)= "#204080";
  }
  return colored[r];
}
