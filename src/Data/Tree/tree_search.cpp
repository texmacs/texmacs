
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
#include "boot.hpp"

bool blank_match_flag= false;
bool initial_match_flag= false;
bool partial_match_flag= false;
bool injective_match_flag= false;
bool cascaded_match_flag= false;

void search (range_set& sel, tree t, tree what, path p);
bool match (tree t, tree what);
void select (range_set& sel, tree t, tree what, path p);

tree_label WILDCARD= UNKNOWN;
tree_label SELECT_REGION= UNKNOWN;

/******************************************************************************
* Initialization
******************************************************************************/

static void
initialize_search () {
  WILDCARD= make_tree_label ("wildcard");
  SELECT_REGION= make_tree_label ("select-region");
  blank_match_flag=
    (get_user_preference ("allow-blank-match", "on") == "on");
  initial_match_flag=
    (get_user_preference ("allow-initial-match", "on") == "on");
  partial_match_flag=
    (get_user_preference ("allow-partial-match", "on") == "on");
  injective_match_flag=
    (get_user_preference ("allow-injective-match", "on") == "on");
  cascaded_match_flag=
    (get_user_preference ("allow-cascaded-match", "on") == "on");
}

/******************************************************************************
* Matching
******************************************************************************/

bool
match_cascaded (tree t, tree what) {
  if (match (t, what)) return true;
  if (cascaded_match_flag && is_compound (t))
    for (int i=0; i<N(t); i++)
      if (is_accessible_child (t, i))
        if (match_cascaded (t[i], what))
          return true;
  return false;
}

bool
match (tree t, tree what) {
  if (blank_match_flag && what == "") return true;
  if (is_atomic (t)) {
    if (!is_atomic (what)) return false;
    if (partial_match_flag) {
      int pos= 0;
      return tm_search_forwards (what->label, pos, t->label) != -1;
    }
    else if (initial_match_flag)
      return starts (t->label, what->label);
    else
      return t->label == what->label;
  }
  else if (injective_match_flag) {
    if (L(t) != L(what)) return false;
    int cur=0;
    for (int i=0; i<N(t); i++) {
      if (match_cascaded (t[i], what[cur])) cur++;
      if (cur >= N(what)) return true;
    }
    return false;
  }
  else {
    if (L(t) != L(what) || N(t) != N(what)) return false;
    for (int i=0; i<N(t); i++)
      if (!match_cascaded (t[i], what[i])) return false;
    return true;
  }
}

/******************************************************************************
* Searching
******************************************************************************/

void
search_string (range_set& sel, string s, tree what, path p) {
  if (is_atomic (what)) {
    string w= what->label;
    int pos= 0;
    while (pos < N(s)) {
      int next= tm_search_forwards (w, pos, s);
      if (next < 0 || next >= N(s)) break;
      sel << (p * next) << (p * (next + N(w)));
      pos= next + N(w);
    }
  }
}

void
search_compound (range_set& sel, tree t, tree what, path p) {
  if (match (t, what)) {
    sel << (p * start (t)) << (p * end (t));
    return;
  }
  for (int i=0; i<N(t); i++)
    if (is_accessible_child (t, i))
      search (sel, t[i], what, p * i);
}

void
search_format (range_set& sel, tree t, tree what, path p) {
  if (N(what) == 0) return;
  for (int i=0; i<N(t); i++) {
    bool found= false;
    range_set fsel;
    search (fsel, t[i], what[0], p * i);
    if (N(fsel) != 0 && fsel[N(fsel)-1] == (p * i) * end (t[i]))
      if (N(sel) == 0 || path_less_eq (sel[N(sel)-1], fsel[N(fsel)-2])) {
        int cur= 1;
        for (int j=i+1; j<N(t); j++) {
          if (cur == N(what) - 1) {
            range_set lsel;
            search (lsel, t[j], what[cur], p * j);
            if (N(lsel) != 0) {
              if (lsel[0] != (p * j) * start (t[j])) break;
              sel << fsel[N(fsel) - 2] << lsel[1];
              found= true;
              i= j-1;
            }
            break;
          }
          else {
            range_set msel;
            search (msel, t[j], what[cur], p * j);
            if (N(msel) == 0) break;
            if (msel[0] != (p * j) * start (t[j])) break;
            if (msel[N(msel)-1] != (p * j) * end (t[j])) break;
            cur++;
            if (cur >= N(what)) break;
          }
        }
      }
    if (!found) search (sel, t[i], what, p * i);
  }
}

void
search (range_set& sel, tree t, tree what, path p) {
  if (is_atomic (t))
    search_string (sel, t->label, what, p);
  else if (is_func (t, CONCAT) && is_func (what, CONCAT))
    search_format (sel, t, what, p);
  else if (is_func (t, DOCUMENT) && is_func (what, DOCUMENT))
    search_format (sel, t, what, p);
  else
    search_compound (sel, t, what, p);
}

/******************************************************************************
* Selections inside specified contexts
******************************************************************************/

bool
contains_select_region (tree t) {
  if (is_atomic (t)) return false;
  else if (is_func (t, SELECT_REGION, 1)) return true;
  else {
    for (int i=0; i<N(t); i++)
      if (contains_select_region (t[i]))
        return true;
    return false;
  }
}

void
select_direct (range_set& sel, tree t, tree what, path p) {
  if (L(t) != L(what)) return;
  int cur=0;
  for (int i=0; i<N(t); i++)
    if (contains_select_region (what[cur])) {
      range_set ssel;
      select (ssel, t[i], what[cur], p * i);
      if (N(ssel) == 0) continue;
      int cur2= cur+1;
      for (int i2=i+1; i2<N(t); i2++)
        if (cur2 >= N(what)) break;
        else if (contains_select_region (what[cur2])) return;
        else if (match_cascaded (t[i2], what[cur2])) cur2++;
      if (cur2 >= N(what)) sel << ssel;
      continue;
    }
    else {
      if (match_cascaded (t[i], what[cur])) cur++;
      if (cur >= N(what)) return;
    }
}

void
select_cascaded (range_set& sel, tree t, tree what, path p) {
  for (int i=0; i<N(t); i++)
    if (is_accessible_child (t, i))
      select (sel, t[i], what, p*i);  
}

void
select (range_set& sel, tree t, tree what, path p) {
  if (is_func (what, SELECT_REGION, 1)) {
    if (is_empty (what[0]))
      sel << (p * start (t)) << (p * end (t));
    else
      search (sel, t, what[0], p);
  }
  else {
    range_set ssel;
    select_direct (ssel, t, what, p);
    if (N(ssel) > 0) sel << ssel;
    else select_cascaded (sel, t, what, p);
  }
}

/******************************************************************************
* Front end
******************************************************************************/

range_set
search (tree t, tree what, path p) {
  initialize_search ();
  range_set sel;
  //cout << "Search " << what << ", " << contains_select_region (what) << "\n";
  if (contains_select_region (what)) select (sel, t, what, p);
  else search (sel, t, what, p);
  //cout << "Selected " << sel << "\n";
  return sel;
}
