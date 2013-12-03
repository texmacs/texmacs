
/******************************************************************************
* MODULE     : tree_search.cpp
* DESCRIPTION: Searching inside trees
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tree_search.hpp"
#include "analyze.hpp"

void search (range_set& sel, tree t, tree what, path p);

void
search_string (range_set& sel, string s, tree what, path p) {
  if (is_atomic (what)) {
    string w= what->label;
    int pos= 0;
    while (pos < N(s)) {
      int next= search_forwards (w, pos, s);
      if (next < 0 || next >= N(s)) break;
      sel << (p * next) << (p * (next + N(w)));
      pos= next + N(w);
    }
  }
}

bool
search_match (tree t, tree what) {
  if (what == "") return true;
  if (is_atomic (t)) return t == what;
  if (L(t) != L(what) || N(t) != N(what)) return false;
  for (int i=0; i<N(t); i++)
    if (!search_match (t[i], what[i])) return false;
  return true;
}

void
search_exact (range_set& sel, tree t, tree what, path p) {
  if (L(t) != L(what) || N(t) != N(what)) return;
  for (int i=0; i<N(t); i++)
    if (!search_match (t[i], what[i]))
      return;
  sel << (p * start (t)) << (p * end (t));
}

void
search_compound (range_set& sel, tree t, tree what, path p) {
  search_exact (sel, t, what, p);
  for (int i=0; i<N(t); i++)
    if (is_accessible_child (t, i))
      search (sel, t[i], what, p * i);
}

void
search_concat (range_set& sel, tree t, tree what, path p) {
  return search_compound (sel, t, what, p);
}

void
search_document (range_set& sel, tree t, tree what, path p) {
  return search_compound (sel, t, what, p);
}

void
search (range_set& sel, tree t, tree what, path p) {
  if (is_atomic (t)) search_string (sel, t->label, what, p);
  else if (is_func (t, CONCAT)) search_concat (sel, t, what, p);
  else if (is_func (t, DOCUMENT)) search_document (sel, t, what, p);
  else search_compound (sel, t, what, p);
}

range_set
search (tree t, tree what, path p) {
  range_set sel;
  search (sel, t, what, p);
  return sel;
}
