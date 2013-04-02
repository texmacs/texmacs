
/******************************************************************************
* MODULE     : post_convert.cpp
* DESCRIPTION: hacks with post-corrections of conversions
* COPYRIGHT  : (C) 2013  Francois Poulain, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "vars.hpp"

/******************************************************************************
* Hacks for HTML import from Open Office
******************************************************************************/

bool
seems_buggy_html_paste (string s) {
  return occurs ("Version:", s)       &&
         occurs ("StartHTML:", s)     &&
         occurs ("EndHTML:", s)       &&
         occurs ("StartFragment:", s) &&
         occurs ("EndFragment:", s);
}

string
correct_buggy_html_paste (string s) {
  int begin_html, end_html, start= 0, stop=0;

  start= search_forwards ("StartHTML:", start, s);
  start= search_forwards (":", start, s) + 1;
  stop= start;
  while (is_digit (s[stop])) stop++;
  begin_html= as_int (s (start, stop + 1));

  start= 0;
  start= search_forwards ("EndHTML:", start, s);
  start= search_forwards (":", start, s) + 1;
  stop= start;
  while (is_digit (s[stop])) stop++;
  end_html= as_int (s (start, stop + 1)) + 1;

  if (begin_html > N(s)) begin_html= 0;
  if (end_html > N(s)) end_html= N(s);
  if (end_html <= begin_html) {
    begin_html= 0;
    end_html= N(s);
  }
  return s (begin_html, end_html);
}

bool
seems_buggy_paste (string s) {
  return N(s) > 0 && s[N(s)-1] == '\0';
}

string
correct_buggy_paste (string s) {
  while (N(s) > 0 && s[N(s)-1] == '\0') s= s(0, N(s)-1);
  return s;
}

tree
default_with_simplify (tree t) {
  if (is_atomic (t)) return t;
  else if (is_func (t, WITH, 3) && t[0] == COLOR && t[1] == "black")
    return default_with_simplify (t[2]);
  else if (is_func (t, DOCUMENT) ||
           is_func (t, CONCAT) ||
           is_compound (t, "itemize") ||
           is_compound (t, "enumerate") ||
           is_compound (t, "description") ||
           (is_func (t, WITH, 3) && t[0] == FONT_SERIES)) {
    int n= N(t);
    tree r (t, n);
    for (int i=0; i<n; i++)
      r[i]= default_with_simplify (t[i]);
    return r;
  }
  else return t;
}
