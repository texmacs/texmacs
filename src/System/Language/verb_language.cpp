
/******************************************************************************
* MODULE     : verb_language.cpp
* DESCRIPTION: the "verbatim" language
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "analyze.hpp"
#include "impl_language.hpp"
#include "scheme.hpp"
#include "packrat.hpp"

verb_language_rep::verb_language_rep (string name):
  language_rep (name)
{
  hl_lan= packrat_abbreviation (res_name, "Main");
}

inline static bool
is_sep_char (char c) {
  return c == '-' || c == '/' || c == ',' || c == '?';
}

text_property
verb_language_rep::advance (tree t, int& pos) {
  string s= t->label;
  if (pos==N(s)) return &tp_normal_rep;
  if (s[pos]==' ') {
    pos++;
    return &tp_space_rep;
  }
  if (is_sep_char (s[pos])) {
    pos++;
    while (pos<N(s) && is_sep_char (s[pos]) && s[pos] != '-') pos++;
    return &tp_hyph_rep;
  }

  array<int> cols= obtain_highlight (t, hl_lan);
  if (N(cols) == 0)
    while ((pos<N(s)) && (s[pos]!=' ') && !is_sep_char (s[pos])) pos++;
  else if ((pos<N(s)) && (s[pos]!=' ') && !is_sep_char (s[pos])) {
    pos++;
    while ((pos<N(s)) && (s[pos]!=' ') && !is_sep_char (s[pos]) &&
	   cols[pos] == cols[pos-1]) pos++;
  }
  return &tp_normal_rep;
}

array<int>
verb_language_rep::get_hyphens (string s) {
  int i;
  array<int> penalty (N(s)+1);
  for (i=0; i<N(penalty); i++) penalty[i]= HYPH_INVALID;
  return penalty;
}

void
verb_language_rep::hyphenate (
  string s, int after, string& left, string& right)
{ 
  left = s(0, after);
  right= s(after, N(s));
}


string
verb_language_rep::get_color (tree t, int start, int end) {
  if (start >= end) return "";
  array<int> cols= obtain_highlight (t, hl_lan);
  if (start < N(cols) && cols[start] != 0)
    return decode_color (res_name, cols[start]);
  return "";
}
