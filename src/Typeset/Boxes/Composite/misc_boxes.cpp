
/******************************************************************************
* MODULE     : misc.cpp
* DESCRIPTION: Miscellaneous composite boxes
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Boxes/composite.hpp"
#include "Boxes/construct.hpp"

/******************************************************************************
* A page box contains a main box and decorations
******************************************************************************/

struct scatter_box_rep: composite_box_rep {
  tree page;
  box  decoration;

  scatter_box_rep (path ip, array<box> bs, array<SI> x, array<SI> y);
  operator tree ();
  int       find_child (SI x, SI y, SI delta, bool force);
  path      find_left_box_path ();
  path      find_right_box_path ();
  selection find_selection (path lbp, path rbp);
};

scatter_box_rep::scatter_box_rep (
  path ip2, array<box> bs, array<SI> x, array<SI> y):
    composite_box_rep (ip2, bs, x, y)
{
  finalize ();
}

scatter_box_rep::operator tree () {
  int i, n= N(bs);
  tree t (TUPLE, n+1);
  t[0]= "scattered";
  for (i=0; i<n; i++) t[i+1]= (tree) bs[i];
  return t;
}

int
scatter_box_rep::find_child (SI x, SI y, SI delta, bool force) {
  int i, n= subnr(), d= MAX_SI, m= -1;
  for (i=0; i<n; i++)
    if (distance (i, x, y, delta)< d)
      if (bs[i]->accessible () || force) {
	d= distance (i, x, y, delta);
	m= i;
      }
  return m;
}

path
scatter_box_rep::find_left_box_path () {
  return path (0, bs[0]->find_left_box_path ());
}

path
scatter_box_rep::find_right_box_path () {
  return path (N(bs)-1, bs[N(bs)-1]->find_right_box_path ());
}

selection
scatter_box_rep::find_selection (path lbp, path rbp) {
  if (is_nil (lbp) || is_nil (rbp) || is_atom (lbp) || is_atom (rbp))
    return composite_box_rep::find_selection (lbp, rbp);
  else {
    int i;
    selection sel; sel->valid= true;
    for (i=lbp->item; i<=rbp->item; i++) {
      path lsp= (i==lbp->item? lbp->next: bs[i]->find_left_box_path  ());
      path rsp= (i==rbp->item? rbp->next: bs[i]->find_right_box_path ());
      selection ssel= bs[i]->find_selection (lsp, rsp);
      sel->valid= sel->valid && ssel->valid;
      sel->rs << translate (ssel->rs, sx(i), sy(i));
      if (i==lbp->item) sel->start= ssel->start;
      if (i==rbp->item) sel->end  = ssel->end  ;
    }
    return sel;
  }
}

/******************************************************************************
* A page box contains a main box and decorations
******************************************************************************/

struct page_box_rep: composite_box_rep {
  tree page;
  box  decoration;

  page_box_rep (path ip, tree page, SI w, SI h,
		array<box> bs, array<SI> x, array<SI> y, box dec);
  operator tree ();
  int find_child (SI x, SI y, SI delta, bool force);
  void display (renderer ren);
  void clear_incomplete (rectangles& rs, SI pixel, int i, int i1, int i2);
  void collect_page_numbers (hashmap<string,tree>& h, tree page);
  path find_left_box_path ();
  path find_right_box_path ();
};

page_box_rep::page_box_rep (path ip2, tree page2, SI w, SI h,
			    array<box> bs, array<SI> x, array<SI> y, box dec):
  composite_box_rep (ip2, bs, x, y), page (page2), decoration (dec)
{
  x1= min (x1, 0);
  x2= max (x2, w);
  y1= -h;
  y2= 0;
  if (!is_nil (decoration)) {
    x3= min (x3, decoration->x0+ decoration->x3);
    x4= max (x4, decoration->x0+ decoration->x4);
    y3= min (y3, decoration->y0+ decoration->y3);
    y4= max (y4, decoration->y0+ decoration->y4);
  }
  finalize ();
}

page_box_rep::operator tree () {
  int i, n= N(bs);
  tree t (TUPLE, n+1);
  if (is_atomic (page)) t[0]= "page-" * page->label;
  else t[0]= "page";
  for (i=0; i<n; i++) t[i+1]= (tree) bs[i];
  return t;
}

int
page_box_rep::find_child (SI x, SI y, SI delta, bool force) {
  int i, n= subnr(), d= MAX_SI, m= -1;
  for (i=0; i<n; i++)
    if (distance (i, x, y, delta)< d)
      if (bs[i]->accessible () || force) {
	d= distance (i, x, y, delta);
	m= i;
      }
  return m;
}

void
page_box_rep::display (renderer ren) {
  if (!is_nil (decoration)) {
    rectangles rs;
    decoration->redraw (ren, path (), rs);
  }
}

void
page_box_rep::clear_incomplete (
  rectangles& rs, SI pixel, int which, int i1, int i2)
{
  (void) which; (void) i1; (void) i2;
  rectangle r1 (sx3 (0)- 2*pixel, sy3 (0)- 2*pixel,
		sx4 (0)+ 2*pixel, sy4 (0)+ 2*pixel);
  rectangles extra (rectangle (x1, y1, x2, y2));
  // cout << "context= " << extra << "\n";
  // cout << "main   = " << r1 << "\n";
  extra = extra - r1;

  if (!is_nil (decoration)) {
    int i, n= N (decoration);
    for (i=0; i<n; i++) {
      box b= decoration [i];
      rectangle r (b->x0+ b->x3, b->y0+ b->y3, b->x0+ b->x4, b->y0+ b->y4);
      if ((r->x2 > r->x1) && (r->y2 > r->y1)) extra = extra - r;
    }
    // cout << "extra  = " << extra << "\n";
  }
  rs= extra * rs;
}

void
page_box_rep::collect_page_numbers (hashmap<string,tree>& h, tree dummy) {
  (void) dummy;
  bs[0]->collect_page_numbers (h, page);
}

path
page_box_rep::find_left_box_path () {
  return path (0, bs[0]->find_left_box_path ());
}

path
page_box_rep::find_right_box_path () {
  return path (N(bs)-1, bs[N(bs)-1]->find_right_box_path ());
}

/******************************************************************************
* box construction routines
******************************************************************************/

box
scatter_box (path ip, array<box> bs, array<SI> x, array<SI> y) {
  return tm_new<scatter_box_rep> (ip, bs, x, y);
}

box
page_box (path ip, tree page, SI w, SI h,
	  array<box> bs, array<SI> bs_x, array<SI> bs_y,
	  array<box> decs, array<SI> decs_x, array<SI> decs_y) {
  box dec;
  if (N (decs) > 0) dec= composite_box (ip, decs, decs_x, decs_y, false);
  return tm_new<page_box_rep> (ip, page, w, h, bs, bs_x, bs_y, dec);
}
