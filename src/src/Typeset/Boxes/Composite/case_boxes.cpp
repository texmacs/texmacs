
/******************************************************************************
* MODULE     : case_boxes.cpp
* DESCRIPTION: Boxes that can be rendered in different ways
*              as a function of messages that we send to them
* COPYRIGHT  : (C) 2022  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Boxes/composite.hpp"
#include "Boxes/construct.hpp"
#include "analyze.hpp"

/******************************************************************************
* Case boxes
******************************************************************************/

struct case_box_rep: public box_rep {
public:
  array<tree> conds;
  array<box>  bs;
  int         current;
  bool        entered;

  case_box_rep (path ip, array<tree> conds, array<box> bs);
  ~case_box_rep ();
  int  subnr () { return 1; }
  box  subbox (int i) { (void) i; return bs[current]; }
  void display (renderer ren) { (void) ren; }
  operator tree ();
  
  bool satisfies (tree t, tree cond);
  void switch_to (int i, rectangles& rs);
  tree message (tree t, SI x, SI y, rectangles& rs);
  void loci (SI x, SI y, SI delta, list<string>& ids, rectangles& rs);
  void collect_page_numbers (hashmap<string,tree>& h, tree page);
  path find_tag (string name);
  box  adjust_kerning (int mode, double factor);
  box  expand_glyphs (int mode, double factor);

  path          find_lip ();
  path          find_rip ();
  path          find_left_box_path ();
  path          find_right_box_path ();
  path          find_box_path (SI x, SI y, SI delta, bool force, bool& found);
  path          find_box_path (path p, bool& found);
  path          find_tree_path (path bp);
  cursor        find_cursor (path bp);
  selection     find_selection (path lbp, path rbp);
  gr_selections graphical_select (SI x, SI y, SI dist);
};

/******************************************************************************
* Basic routines for case boxes
******************************************************************************/

case_box_rep::case_box_rep (path ip, array<tree> c2, array<box> b2):
  box_rep (ip), conds (c2), bs (b2), current (N(b2) - 1), entered (false)
{
  ASSERT (N(bs) != 0, "no cases");
  int i, n= N(bs);
  x1= y1= x3= y3= MAX_SI;
  x2= y2= x4= y4= -MAX_SI;
  for (i=0; i<n; i++) {
    x1= min (x1, bs[i]->x1);
    y1= min (y1, bs[i]->y1);
    x2= max (x2, bs[i]->x2);
    y2= max (y2, bs[i]->y2);
    x3= min (x3, bs[i]->x3);
    y3= min (y3, bs[i]->y3);
    x4= max (x4, bs[i]->x4);
    y4= max (y4, bs[i]->y4);
  }
}

case_box_rep::~case_box_rep () {}

case_box_rep::operator tree () {
  int i, n= N(bs);
  tree t (TUPLE, 1);
  t[0]= "case";
  for (i=0; i<n; i++) {
    if (i<N(conds)) t << conds[i];
    t << (tree) bs[i];
  }
  return t;
}

void
case_box_rep::switch_to (int i, rectangles& rs) {
  if (current == i) return;
  current= i;
  rs << rectangle (x3, y3, x4, y4);
}

bool
case_box_rep::satisfies (tree t, tree cond) {
  if (is_atomic (cond) && occurs (",", cond->label)) {
    array<string> a= tokenize (cond->label, ",");
    for (int i=0; i<N(a); i++)
      if (satisfies (t, a[i])) return true;
    return false;
  }
  if (t == cond) return true;
  if (cond == "mouse-over") return entered;
  return false;
}

tree
case_box_rep::message (tree t, SI x, SI y, rectangles& rs) {
  if (t == "enter" || t == "move") entered= true;
  if (t == "leave") entered= false;
  for (int i=0; i < min (N(conds), N(bs)); i++)
    if (satisfies (t, conds[i])) {
      switch_to (i, rs);
      return bs[current]->message (t, x, y, rs);
    }
  switch_to (N(bs) - 1, rs);
  return bs[current]->message (t, x, y, rs);
}

void
case_box_rep::loci (SI x, SI y, SI delta, list<string>& ids, rectangles& rs) {
  bs[current]->loci (x, y, delta, ids, rs);
}

void
case_box_rep::collect_page_numbers (hashmap<string,tree>& h, tree page) {
  bs[current]->collect_page_numbers (h, page);
}

path
case_box_rep::find_tag (string name) {
  return bs[current]->find_tag (name);
}

box
case_box_rep::adjust_kerning (int mode, double factor) {
  int i, n= N(bs);
  array<box> adj (n);
  for (i=0; i<n; i++)
    adj[i]= bs[i]->adjust_kerning (mode, factor);
  return case_box (ip, conds, adj);
}

box
case_box_rep::expand_glyphs (int mode, double factor) {
  int i, n= N(bs);
  array<box> adj (n);
  for (i=0; i<n; i++)
    adj[i]= bs[i]->expand_glyphs (mode, factor);
  return case_box (ip, conds, adj);
}

/******************************************************************************
* Cursor routines for case boxes
******************************************************************************/

path
case_box_rep::find_lip () {
  return bs[current]->find_lip ();
}

path
case_box_rep::find_rip () {
  return bs[current]->find_rip ();
}

path
case_box_rep::find_left_box_path () {
  return path (0, bs[current]->find_left_box_path ());
}

path
case_box_rep::find_right_box_path () {
  return path (0, bs[current]->find_right_box_path ());
}

path
case_box_rep::find_box_path (SI x, SI y, SI delta,
                             bool force, bool& found) {
  if (outside (x, delta, x1, x2) && (is_accessible (ip) || force))
    return box_rep::find_box_path (x, y, delta, force, found);
  return path (0, bs[current]->find_box_path (x, y, delta, force, found));
}

path
case_box_rep::find_box_path (path p, bool& found) {
  path r= path (0, bs[current]->find_box_path (p, found));
  if (found) return r;
  return box_rep::find_box_path (p, found);
}

path
case_box_rep::find_tree_path (path bp) {
  if (is_atom (bp)) return box_rep::find_tree_path (bp);
  return bs[current]->find_tree_path (bp->next);
}

cursor
case_box_rep::find_cursor (path bp) {
  if (is_atom (bp)) return box_rep::find_cursor (bp);
  else return bs[current]->find_cursor (bp->next);
}

selection
case_box_rep::find_selection (path lbp, path rbp) {
  if (!is_atom (lbp) && !is_atom (rbp))
    return bs[current]->find_selection (lbp->next, rbp->next);
  else return box_rep::find_selection (lbp, rbp);
}

gr_selections
case_box_rep::graphical_select (SI x, SI y, SI dist) {
  return bs[current]->graphical_select (x- sx(0), y- sy(0), dist);
}

/******************************************************************************
* User interface
******************************************************************************/

box
case_box (path ip, array<tree> conds, array<box> bs) {
  return tm_new<case_box_rep> (ip, conds, bs);
}
