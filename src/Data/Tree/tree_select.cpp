
/******************************************************************************
* MODULE     : tree_select.cpp
* DESCRIPTION: abstract cursor handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tree_select.hpp"
#include "drd_std.hpp"
#include "analyze.hpp"

path closest_up (tree t, path p);

/******************************************************************************
* Useful subroutines
******************************************************************************/

bool
right_most_inside (path p, tree t) {
  if (is_atomic (t)) return p == path (N(t->label));
  else if (p == path (1)) return true;
  if (is_nil (p) || is_nil (p->next)) return false;
  if (N(t) == 1 && p->item == 0)
    return right_most_inside (p->next, t[0]);
  if (is_func (t, WIDE, 2))
    return p->item == 0 && right_most_inside (p->next, t[0]);
  if (is_func (t, CONCAT) || is_func (t, DOCUMENT))
    return p->item == N(t) - 1 && right_most_inside (p->next, t[N(t) - 1]);
  return false;
}

path
correct_right_most_inside (path p, tree t) {
  if (is_nil (p) || is_nil (p->next) || is_atomic (t)) return p;
  if (0 > p->item || p->item >= N(t)) return p;
  p= path (p->item, correct_right_most_inside (p->next, t[p->item]));
  if (right_most_inside (p, t)) return path (1);
  else return p;
}

/******************************************************************************
* Structural correction of the selection
******************************************************************************/

static void
selection_adjust_border (tree t, path i1, path i2, path& o1, path& o2) {
  o1= i1; o2= i2;
  if (is_compound (t) && !is_atom (i1) && !is_atom (i2) &&
      i1->item == i2->item) {
    path O1, O2;
    selection_adjust_border (t[i1->item], i1->next, i2->next, O1, O2);
    if (the_drd->var_without_border (L(t[i1->item])) &&
        (O1->item != O2->item)) {
      o1= path (0);
      o2= path (1);
    }
    else {
      o1= path (i1->item, O1);
      o2= path (i1->item, O2);
    }
  }
}

static void
adjust_right_script (tree t, path& o1) {
  while (is_concat (t) && o1->item > 0 && o1->next == path (0)) {
    tree st= t[o1->item];
    if (is_right_script_prime (st)) {
      tree pt= t[o1->item-1];
      if (!is_atomic (pt))
        o1= path (o1->item-1, start (pt));
      else {
        string s= pt->label;
        int pos= N(s);
        while (pos > 0) {
          int prev= pos;
          tm_char_backwards (s, prev);
          if (pos == N(s));
          else if (is_numeric (s (prev, N(s))));
          else if (is_iso_alpha (s (prev, N(s))));
          else break;
          pos= prev;
        }
        o1= path (o1->item-1, pos);
      }
    }
    else break;
  }
}

static void
adjust_left_script (tree t, path& o2) {
  while (is_concat (t) && o2->item + 1 < N(t) && o2->next == path (1)) {
    tree st= t[o2->item];
    if (is_left_script_prime (st)) {
      tree nt= t[o2->item+1];
      if (!is_atomic (nt)) o2= path (o2->item+1, end (nt));
      else {
        string s= nt->label;
        int pos= 0;
        while (pos < N(s)) {
          int next= pos;
          tm_char_forwards (s, next);
          if (pos == 0);
          else if (is_numeric (s (0, next)));
          else if (is_iso_alpha (s (0, next)));
          else break;
          pos= next;
        }
        o2= path (o2->item+1, pos);
      }
    }
    else break;
  }
}

static void
adjust_right_script (tree t, path& o1, path& o2) {
  if (!is_concat (t)) return;
  if (o1->item == o2->item) return;
  if (is_right_script_prime (t[o2->item])) {
    int i= o2->item;
    while (i+1<N(t) && is_right_script_prime (t[i+1])) i++;
    o2= path (i, 1);
  }
}

static void
adjust_left_script (tree t, path& o1, path& o2) {
  if (!is_concat (t)) return;
  if (o1->item == o2->item && o2->next != path (1)) return;
  if (is_left_script_prime (t[o1->item])) {
    int i= o1->item;
    while (i>=1 && is_left_script_prime (t[i-1])) i--;
    o1= path (i, 0);
  }
}

static void
selection_adjust (tree t, path i1, path i2, path& o1, path& o2) {
  //cout << "Adjust " << i1 << " -- " << i2 << " in " << t << "\n";
  if (i1 == i2) {
    o1= i1;
    o2= i2;
  }
  else if (is_atom (i1) || is_atom (i2)) {
    if (is_atomic (t)) {
      o1= i1;
      o2= i2;
    }
    else {
      o1= start (t);
      o2= end (t);
    }
  }
  else if (i1->item == i2->item) {
    selection_adjust (t[i1->item], i1->next, i2->next, o1, o2);
    o1= path (i1->item, o1);
    o2= path (i2->item, o2);
    adjust_right_script (t, o1);
    adjust_left_script (t, o2);
    adjust_right_script (t, o1, o2);
    adjust_left_script (t, o1, o2);
  }
  else {
    tree_label l= L(t);
    if ((l==DOCUMENT) || (l==PARA) || (l==CONCAT)) {
      if (is_compound (t[i1->item], "slide"))
        o1= path (i1->item, start (t[i1->item]));
      else if (is_compound (t[i1->item])) {
        path mid;
        selection_adjust (t[i1->item], i1->next, end (t[i1->item]), o1, mid);
        o1= path (i1->item, o1);
      }
      else o1= i1;
      if (is_compound (t[i2->item], "slide"))
        o2= path (i2->item, end (t[i2->item]));
      else if (is_compound (t[i2->item])) {
        path mid;
        selection_adjust (t[i2->item], start(t[i2->item]), i2->next, mid, o2);
        o2= path (i2->item, o2);
      }
      else o2= i2;
      if (l == CONCAT) {
        adjust_right_script (t, o1);
        adjust_left_script (t, o2);
        adjust_right_script (t, o1, o2);
        adjust_left_script (t, o1, o2);
      }
    }
    else {
      o1= start (t);
      o2= end (t);
    }
  }
  //cout << "Adjusted " << o1 << " -- " << o2 << " in " << t << "\n";
}

static void
selection_make_accessible (tree t, path i1, path i2, path& o1, path& o2) {
  o1= i1; o2= i2;
  if (!is_accessible_cursor (t, o1))
    o1= previous_accessible (t, o1);
  if (!is_accessible_cursor (t, o1))
    o1= next_accessible (t, o1);
  if (!is_accessible_cursor (t, o2))
    o2= next_accessible (t, o2);
  if (!is_accessible_cursor (t, o2))
    o2= previous_accessible (t, o2);
  if (path_inf (o1, o2))
    o1= shift (t, o1, 1);
}

void
selection_correct (tree t, path i1, path i2, path& o1, path& o2) {
  o1= i1; o2= i2;
#ifdef SANITY_CHECKS
  ASSERT (is_inside (t, o1), "invalid selection [selection_correct]");
  ASSERT (is_inside (t, o2), "invalid selection [selection_correct]");
#else
  if (!is_inside (t, o1)) o1= closest_up (t, o1);
  if (!is_inside (t, o2)) o2= closest_up (t, o2);
#endif
  while (true) {
    path p1= o1, p2= o2;
    i1= o1; i2= o2;
    selection_adjust_border (t, i1, i2, o1, o2);
    i1= o1; i2= o2;
    selection_adjust (t, i1, i2, o1, o2);
    i1= o1; i2= o2;
    selection_make_accessible (t, i1, i2, o1, o2);
    if (o1 == i1 && o2 == i2) break;
    if (o1 == p1 && o2 == p2) break;
  }
}

/******************************************************************************
* Computation of the selected tree
******************************************************************************/

tree
selection_compute (tree t, path start, path end) {
  int  i1= start->item;
  int  i2= end->item;
  path p1= start->next;
  path p2= end->next;

  if (is_nil (p1) || is_nil (p2)) {
    if (start == path (right_index (t))) return "";
    if (end == path (0)) return "";
    if (start == end) return "";
    if (is_nil (p1) && is_nil (p2)) {
      if (is_compound (t)) return copy (t);
      if (i1>=i2) return "";
      return t->label (i1, i2);
    }
    if (is_compound (t) && (!is_format (t))) return copy (t);
    if (is_nil (p1)) {
      i1= 0;
      p1= (start->item==0? 0: right_index (t[i1]));
    }
    if (is_nil (p2)) {
      i2= N(t)-1;
      p2= (end->item==0? 0: right_index (t[i2]));
    }
  }

  if (i1==i2) return selection_compute (t[i1], p1, p2);
  if (is_compound (t) && (!is_format (t))) return copy (t);

  int i;
  tree r (t, i2-i1+1);
  r[0]     = selection_compute (t[i1], p1, path (right_index (t[i1])));
  r[N(r)-1]= selection_compute (t[i2], path (0), p2);
  for (i=1; i<N(r)-1; i++) r[i]= copy (t[i+i1]);
  return r;
}

/******************************************************************************
* Selections in tables
******************************************************************************/

path
table_search_format (tree t, path p) {
  tree st= subtree (t, p);
  if (is_func (st, TFORMAT) && is_func (st[N(st)-1], TABLE)) return p;
  while ((!is_nil (p)) && (!is_func (subtree (t, p), TABLE))) p= path_up (p);
  if ((!is_nil (p)) && (is_func (subtree (t, path_up (p)), TFORMAT)))
    p= path_up (p);
  return p;
}

void
table_search_coordinates (tree t, path p, int& row, int& col) {
  row= col= 0;
  while (true) {
    if (is_nil (p)) p= path (1);
    if (p == path (0)) p= path (0, 0);
    if (p == path (1)) p= path (N(t)-1, 1);
    if (is_func (t, TFORMAT));
    else if (is_func (t, TABLE)) row= p->item;
    else if (is_func (t, ROW)) col= p->item;
    else return;
    t= t [p->item];
    p= p->next;
  }
}

path
table_search_cell (tree t, int row, int col) {
  path p;
  while (is_func (t, TFORMAT)) {
    p= p * (N(t)-1);
    t= t [N(t)-1];
  }
  p= p * row;
  t= t [row];
  while (is_func (t, TFORMAT)) {
    p= p * (N(t)-1);
    t= t [N(t)-1];
  }
  p= p * col;
  t= t [col];
  while (is_func (t, TFORMAT)) {
    p= p * (N(t)-1);
    t= t [N(t)-1];
  }
  return p;
}


bool
is_table_selection (tree et, path p1, path p2, bool strict) {
  if (p1 == p2) return false;
  path p= common (p1, p2);
  if ((p == p1 || p == p2) && !is_nil (p)) p= path_up (p);
  tree t= subtree (et, p);
  return
    is_func (t, TFORMAT) || is_func (t, TABLE) ||
    is_func (t, ROW) || is_func (t, CELL) ||
    (!strict && is_compound (t) && N(t) == 1 && is_func (t[0], TFORMAT));
}

path
find_subtable_selection (tree et, path p1, path p2,
                         int& row1, int& col1, int& row2, int& col2) {
  if (is_table_selection (et, p1, p2, true)) {
    path fp= table_search_format (et, common (p1, p2));
    if (is_nil (fp)) return fp;
    tree st= subtree (et, fp);
    table_search_coordinates (st, tail (p1, N(fp)), row1, col1);
    table_search_coordinates (st, tail (p2, N(fp)), row2, col2);
    if (row1>row2) { int tmp= row1; row1= row2; row2= tmp; }
    if (col1>col2) { int tmp= col1; col1= col2; col2= tmp; }
    // table_bound (fp, row1, col1, row2, col2);
    // FIXME: table editing routines should be moved to src/Data/Tree
    return fp;
  }
  else if (is_table_selection (et, p1, p2, false)) {
    path fp= table_search_format (et, common (p1, p2) * 0);
    if (is_nil (fp)) return fp;
    path p= fp;
    tree st;
    while (true) {
      st= subtree (et, p);
      if (is_func (st, TABLE) && N(st) > 0 && is_func (st[0], ROW)) break;
      if (!is_func (st, TFORMAT)) return path ();
      p= p * (N(st) - 1);
    }
    row1= 0; col1= 0;
    row2= N(st)-1; col2= N(st[0])-1;
    return fp;
  }
  else return path ();
}


/******************************************************************************
* Range sets
******************************************************************************/

range_set
no_ranges () {
  return range_set (path (), path ());
}

range_set
simple_range (path p1, path p2) {
  return range_set (copy (p1), copy (p2));
}

bool
is_empty (range_set sel) {
  return N(sel) == 0 || sel[0] == sel[N(sel)-1];
}

path
start (range_set sel) {
  if (N(sel) == 0) return path ();
  else return copy (sel[0]);
}

path
end (range_set sel) {
  if (N(sel) == 0) return path ();
  else return copy (sel[N(sel)-1]);
}

path
common (range_set sel) {
  if (N(sel) == 0) return path ();
  else return common (sel[0], sel[N(sel)-1]);
}
