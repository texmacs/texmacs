
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
#include "drd_mode.hpp"

int  search_max_hits= 1000000;
bool blank_match_flag= false;
bool initial_match_flag= false;
bool partial_match_flag= false;
bool injective_match_flag= false;
bool cascaded_match_flag= false;
bool case_insensitive_match_flag= false;

void search (range_set& sel, tree t, tree what, path p);
bool match (tree t, tree what);
void select (range_set& sel, tree t, tree what, path p);

tree_label WILDCARD= UNKNOWN;
tree_label SELECT_REGION= UNKNOWN;

/******************************************************************************
* Initialization and useful subroutines
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
  case_insensitive_match_flag=
    (get_user_preference ("case-insensitive-match", "off") == "on");
  
  if (get_user_preference ("search-and-replace", "on") == "on") {
    blank_match_flag=
      (get_user_preference ("allow-blank-replace", "off") == "on");
    initial_match_flag=
      (get_user_preference ("allow-initial-replace", "off") == "on");
    partial_match_flag=
      (get_user_preference ("allow-partial-replace", "off") == "on");
    injective_match_flag=
      (get_user_preference ("allow-injective-replace", "off") == "on");
  }
}

static void
merge (range_set& sel, range_set ssel) {
  for (int i=0; i+1<N(ssel); i+=2)
    if (N(sel) == 0 || path_less_eq (sel[N(sel)-1], ssel[i]))
      sel << ssel[i] << ssel[i+1];
}

static bool
is_accessible_for_search (tree t, int i) {
  if (is_accessible_child (t, i)) return true;
  if (is_func (t, HIDDEN)) return true;
  if (get_access_mode () != DRD_ACCESS_SOURCE) return false;
  if (is_func (t, RAW_DATA)) return false;
  return i >= 0 && i < N(t);
}

/******************************************************************************
* Matching complex patterns inside strings
******************************************************************************/

bool
match_atomic (string s, tree what, int pos, int i, int& start, int& end) {
  if (i == 0) start= pos;
  if (i >= N(what)) {
    end= pos;
    if (initial_match_flag) return true;
    return pos == N(s);
  }
  if (is_atomic (what[i])) {
    if (test (s, pos, what[i]->label) &&
        match_atomic (s, what, pos + N(what[i]->label), i+1, start, end))
      return true;
    if (i == 0 &&
        partial_match_flag &&
        pos < N(s) &&
        match_atomic (s, what, tm_char_next (s, pos), i, start, end))
      return true;
    return false;
  }
  else if (is_func (what[i], WILDCARD, 1)) {
    if (i+1 >= N(what)) {
      end= N(s);
      return true;
    }
    if (is_func (what[i+1], WILDCARD, 1))
      return match_atomic (s, what, pos, i+1, start, end);
    if (!is_atomic (what[i+1])) return false;
    while (pos < N(s)) {
      pos= tm_search_forwards (what[i+1]->label, pos, s);
      if (pos < 0) return false;
      if (match_atomic (s, what, pos, i+1, start, end)) return true;
      if (pos < N(s)) tm_char_forwards (s, pos);
    }
    return false;
  }
  else return false;
}

/******************************************************************************
* Matching
******************************************************************************/

bool
match_cascaded (tree t, tree what) {
  if (match (t, what)) return true;
  if (cascaded_match_flag && is_compound (t))
    for (int i=0; i<N(t); i++)
      if (is_accessible_for_search (t, i))
        if (match_cascaded (t[i], what))
          return true;
  return false;
}

bool
match (tree t, tree what) {
  if (blank_match_flag && what == "") return true;
  if (is_func (what, WILDCARD, 1)) return true;
  if (is_atomic (t)) {
    if (is_concat (what)) {
      int start, end;
      return match_atomic (t->label, what, 0, 0, start, end);
    }
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
      if (cur >= N(what)) return true;
      if (match_cascaded (t[i], what[cur])) cur++;
    }
    return cur >= N(what);
  }
  else {
    if (L(t) != L(what) || N(t) != N(what)) return false;
    for (int i=0; i<N(t); i++)
      if (!match_cascaded (t[i], what[i])) return false;
    return true;
  }
}

/******************************************************************************
* Matching complex patterns inside concat tags
******************************************************************************/

bool
search_concat (tree t, tree what, int pos, int i,
               path p, path cur, path& p1, path& p2) {
  //cout << "Search " << what << ", " << i
  //     << " in " << t << ", " << pos << "\n";
  if (pos >= N(t)) return false;
  if (is_atomic (t[pos])) {
    int j, k= i;
    while (k<N(what) &&
           (is_atomic (what[k]) || is_func (what[k], WILDCARD, 1))) k++;
    for (j=k; j>i; j--) {
      if (is_func (what[j-1], WILDCARD, 1)) continue;
      tree swhat= what (i, j);
      if (j == i+1) swhat= swhat[0];
      range_set sel;
      search (sel, t[pos], swhat, p * pos);
      if (N(sel) != 0) {
        bool c1= (i == 0 || sel[0] == (p * pos) * start (t[pos]));
        bool c2= (j == N(what) || sel[N(sel)-1] == (p * pos) * end (t[pos]));
        if (j<N(what) && is_func (what[j], WILDCARD, 1)) c2= true;
        if (i == 0) p1= sel[N(sel)-2];
        if (j == N(what)) p2= sel[1];
        if (i == 0 && is_func (what[0], WILDCARD, 1))
          p1= (p * 0) * start (t[0]);
        if (c1 && c2 && (i > 0 || path_less_eq (cur, p1))) {
          if (j >= N(what)) return true;
          if (search_concat (t, what, pos+1, j, p, cur, p1, p2)) return true;
        }
      }
    }
    if (i == 0 || is_func (what[i], WILDCARD, 1))
      return search_concat (t, what, pos+1, i, p, cur, p1, p2);
    return false;
  }
  else if (is_func (what[i], WILDCARD, 1)) {
    if (i == 0) {
      p1= (p * 0) * start (t[0]);
      if (!path_less_eq (cur, p1)) return false;
    }
    if (i+1 < N(what) && is_func (what[i+1], WILDCARD, 1))
      return search_concat (t, what, pos, i+1, p, cur, p1, p2);
    if (i+1 >= N(what)) {
      p2= (p * (N(t)-1)) * end (t[N(t)-1]);
      return true;
    }
    if (search_concat (t, what, pos, i+1, p, cur, p1, p2)) return true;
    path dummy;
    return search_concat (t, what, pos+1, i, p, cur, dummy, p2);
  }
  else {
    range_set sel;
    search (sel, t[pos], what[i], p * pos);
    if (N(sel) != 0) {
      bool c1= (i == 0 || sel[0] == (p * pos) * start (t[pos]));
      bool c2= (i == N(what)-1 || sel[N(sel)-1] == (p * pos) * end (t[pos]));
      if (i == 0) p1= sel[0];
      if (i == N(what)-1) p2= sel[1];
      if (c1 && c2 && (i > 0 || path_less_eq (cur, p1))) {
        if (i+1 >= N(what)) return true;
        if (search_concat (t, what, pos+1, i+1, p, cur, p1, p2)) return true;
      }
    }
    if (i == 0) {
      range_set test;
      search (test, t[pos], what, p * pos);
      if (N(test) != 0) return false;
      return search_concat (t, what, pos+1, i, p, cur, p1, p2);
    }
    return false;
  }
}

/******************************************************************************
* Searching
******************************************************************************/

void
search_string (range_set& sel, string s, tree what, path p) {
  string source= (case_insensitive_match_flag)? locase_all (s): s;

  if (is_atomic (what)) {
    string w= what->label;
    int pos= 0;
    while (pos < N(s)) {
      int next= tm_search_forwards (w, pos, source);
      if (next < 0 || next >= N(s)) break;
      merge (sel, simple_range (p * next, p * (next + N(w))));
      pos= next + N(w);
    }
  }
  else if (is_concat (what)) {
    for (int pos=0; pos<N(s); ) {
      int start, end;
      if (match_atomic (s, what, pos, 0, start, end)) {
        merge (sel, simple_range (p * start, p * end));
        pos= end;
      }
      else break;
    }
  }
}

void
search_compound (range_set& sel, tree t, tree what, path p) {
  if (match (t, what)) {
    merge (sel, simple_range (p * start (t), p * end (t)));
    return;
  }
  for (int i=0; i<N(t); i++)
    if (is_accessible_for_search (t, i))
      search (sel, t[i], what, p * i);
}

void
search_concat (range_set& sel, tree t, tree what, path p) {
  for (int pos=0; pos<N(t); ) {
    range_set ssel;
    search (ssel, t[pos], what, p * pos);
    if (N(ssel) != 0) {
      if (is_func (what[0], WILDCARD, 1))
        for (int i=0; i+1<N(ssel); i++)
          ssel[i]= (p * 0) * start (t[0]);
      if (is_func (what[N(what)-1], WILDCARD, 1))
        for (int i=0; i+1<N(ssel); i++)
          ssel[i+1]= (p * (N(t)-1)) * end (t[N(t)-1]);
      merge (sel, ssel);
      pos++;
      continue;
    }
    path p1, p2, cur= (p * 0) * start (t[0]);
    if (N(sel) != 0) cur= sel[N(sel)-1];
    if (search_concat (t, what, pos, 0, p, cur, p1, p2)) {
      merge (sel, simple_range (p1, p2));
      int next= (p2 / p)->item;
      pos= max (pos+1, next);
    }
    else pos++;
  }
}

void
search_document (range_set& sel, tree t, tree what, path p) {
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
              merge (sel, simple_range (fsel[N(fsel) - 2], lsel[1]));
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
  if (N(sel) > search_max_hits) return;
  if (is_atomic (t))
    search_string (sel, t->label, what, p);
  else if (is_func (t, CONCAT) && is_func (what, CONCAT))
    search_concat (sel, t, what, p);
  else if (is_func (t, DOCUMENT) && is_func (what, DOCUMENT))
    search_document (sel, t, what, p);
  else
    search_compound (sel, t, what, p);
}

void
search (range_set& sel, tree t, tree what, path p, path pos) {
  if (is_document (what) || is_atomic (t))
    search (sel, t, what, p);
  else if (is_concat (what) && is_concat (t))
    search (sel, t, what, p);
  else {
    if (is_nil (pos)) search (sel, t, what, p);
    else {
      int hits= 0;
      array<range_set> sub (N(t));
      if (pos->item >= 0 && pos->item < N(t))
        if (is_accessible_for_search (t, pos->item)) {
          search (sub[pos->item], t[pos->item], what, p * pos->item, pos->next);
          hits += N(sub[pos->item]);
        }
      for (int d=1; d<N(t); d++)
        for (int e=0; e<=1; e++) {
          if (hits > search_max_hits) break;
          int i= (e==0? pos->item + d: pos->item - d);
          if (i >= 0 && i < N(t))
            if (is_accessible_for_search (t, i)) {
              search (sub[i], t[i], what, p * i);
              hits += N(sub[i]);
            }
        }
      for (int i=0; i<N(t); i++) sel << sub[i];
    }
  }
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
      if (cur2 >= N(what)) merge (sel, ssel);
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
    if (is_accessible_for_search (t, i))
      select (sel, t[i], what, p*i);  
}

void
select (range_set& sel, tree t, tree what, path p) {
  if (is_func (what, SELECT_REGION, 1)) {
    if (is_empty (what[0]))
      merge (sel, simple_range (p * start (t), p * end (t)));
    else
      search (sel, t, what[0], p);
  }
  else {
    range_set ssel;
    select_direct (ssel, t, what, p);
    if (N(ssel) > 0) merge (sel, ssel);
    else select_cascaded (sel, t, what, p);
  }
}

/******************************************************************************
* Front end
******************************************************************************/

range_set
search (tree t, tree what, path p, int limit) {
  search_max_hits= limit;
  initialize_search ();
  range_set sel;
  //cout << "Search " << what << ", " << contains_select_region (what) << "\n";
  if (contains_select_region (what)) select (sel, t, what, p);
  else search (sel, t, what, p);
  //cout << "Selected " << sel << "\n";
  search_max_hits= 1000000;
  return sel;
}

range_set
search (tree t, tree what, path p, path pos, int limit) {
  search_max_hits= limit;
  initialize_search ();
  range_set sel;
  //cout << "Search " << what << ", " << contains_select_region (what) << "\n";
  if (contains_select_region (what)) select (sel, t, what, p);
  else search (sel, t, what, p, pos);
  //cout << "Selected " << sel << "\n";
  search_max_hits= 1000000;
  return sel;
}

range_set
previous_search_hit (range_set sels, path cur, bool strict) {
  int i= (N(sels) >> 1) << 1;
  while (i >= 2 && !path_less_eq (sels[i-2], cur)) i -= 2;
  if (strict && i >= 2 && !path_less (sels[i-1], cur)) i -= 2;
  if (i >= 2) return range (sels, i-2, i);
  return range_set ();
}

range_set
next_search_hit (range_set sels, path cur, bool strict) {
  int i=0, n=N(sels);
  while (i+2 <= n && !path_less_eq (cur, sels[i+1])) i += 2;
  while (i+4 <= n && sels[i+1] == sels[i+2] && sels[i+1] == cur) i += 2;
  if (strict && i+2 <= n) i += 2;
  if (i+2 <= n) return range (sels, i, i+2);
  return range_set ();
}

range_set current_alt_selection (string name);

range_set
navigate_search_hit (path cur, bool forward, bool extreme, bool strict) {
  range_set sels= current_alt_selection ("alternate");
  if ((N(sels) & 1) == 1) sels= range (sels, 0, N(sels) - 1);
  if (N(sels) < 2) return range_set ();
  if (extreme && !forward) return range (sels, 0, 2);
  if (extreme && forward) return range (sels, N(sels)-2, N(sels));
  if (!forward) return previous_search_hit (sels, cur, strict);
  else return next_search_hit (sels, cur, strict);
}
