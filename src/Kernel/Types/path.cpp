
/******************************************************************************
* MODULE     : path.cpp
* DESCRIPTION: paths are integer lists,
*              which are for instance useful to select subtrees in trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "path.hpp"
#include "analyze.hpp"

/******************************************************************************
* General routines on paths
******************************************************************************/

bool
zero_path (path p) {
  if (is_nil (p)) return true;
  return (p->item == 0) && zero_path (p->next);
}

int
hash (path p) {
  if (is_nil (p)) return 0;
  else {
    int h= hash (p->next);
    return p->item ^ ((h<<7) + (h>>25));
  }
}

string
as_string (path p) {
  if (is_nil (p)) return "";
  if (is_atom (p)) return as_string (p->item);
  return as_string (p->item) * "." * as_string (p->next);
}

path
as_path (string s) {
  int i, j, n= N(s);
  for (i=0; i<n; i++)
    if (is_digit (s[i])) break;
  if (i==n) return path ();
  for (j=i; j<n; j++)
    if (!is_digit (s[j])) break;
  return path (as_int (s (i, j)), as_path (s (j, n)));
}

bool
version_inf_eq (string v1, string v2) {
  if (starts (v2, v1)) return true;
  return path_inf_eq (as_path (v1), as_path (v2));
}

bool
version_inf (string v1, string v2) {
  if (v1 == v2) return false;
  return version_inf_eq (v1, v2);
}

/******************************************************************************
* Operations on paths
******************************************************************************/

path
path_add (path p, int plus) {
  if (is_atom (p)) return path (p->item+plus);
  return path (p->item, path_add (p->next, plus));
}

path
path_add (path p, int plus, int pos) {
  p= copy (p);
  p[pos]+=plus;
  return p;
}

path
path_up (path p) {
  ASSERT (!is_nil (p), "path is too short");
  if (is_nil (p->next)) return path ();
  return path (p->item, path_up (p->next));
}

path
path_up (path p, int times) {
  return head (p, N(p)-times);
}

bool
path_inf (path p1, path p2) {
  if (is_nil (p1) || is_nil (p2)) return false;
  if (p1->item<p2->item) return true;
  if (p1->item>p2->item) return false;
  return path_inf (p1->next, p2->next);
}

bool
path_inf_eq (path p1, path p2) {
  if (is_nil (p1) || is_nil (p2)) return (p1 == p2);
  if (p1->item<p2->item) return true;
  if (p1->item>p2->item) return false;
  return path_inf_eq (p1->next, p2->next);
}

bool
path_less (path p1, path p2) {
  return path_less_eq (p1, p2) && (p1 != p2);
}

bool
path_less_eq (path p1, path p2) {
  if (is_nil (p1) || is_nil (p2)) return p1 == p2;
  if (is_atom (p1) || is_atom (p2)) {
    if (is_atom (p1) && is_atom (p2)) return p1->item <= p2->item;
    if ((p1->item == 0) && is_nil (p1->next)) return true;
    if ((p2->item == 1) && is_nil (p2->next)) return true;
    return false;
  }
  if (p1->item<p2->item) return true;
  if (p1->item>p2->item) return false;
  return path_less_eq (p1->next, p2->next);
}

path
operator - (path p, path q) {
  if (is_nil (q)) return p;
  else if (is_nil (p) || (p->item != q->item)) {
    FAILED ("path did not start with required path"); }
  else return p->next - q-> next;
  return path (); // NOT REACHED
}

path
common (path start, path end) {
  if (is_nil (start) || is_nil (end)) return path ();
  if (start->item != end->item) return path ();
  return path (start->item, common (start->next, end->next));
}

/******************************************************************************
* Main modification routines
******************************************************************************/

tree&
subtree (tree& t, path p) {
  if (is_nil (p)) return t;
  else return subtree (t[p->item], p->next);
}

tree&
parent_subtree (tree& t, path p) {
  ASSERT (!is_nil (p), "path too short");
  if (is_nil (p->next)) return t;
  else return parent_subtree (t[p->item], p->next);
}
