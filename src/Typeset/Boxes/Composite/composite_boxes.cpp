
/******************************************************************************
* MODULE     : composite.cpp
* DESCRIPTION: Boxes with a list of child boxes
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Boxes/composite.hpp"
#include "Boxes/construct.hpp"

/******************************************************************************
* Setting up composite boxes
******************************************************************************/

composite_box_rep::composite_box_rep (path ip): box_rep (ip) { }

composite_box_rep::composite_box_rep (path ip, array<box> B): box_rep (ip) {
  bs= B;
  position ();
}

composite_box_rep::composite_box_rep (
  path ip, array<box> B, bool init_sx_sy):
    box_rep (ip)
{
  bs= B;
  if (init_sx_sy) {
    int i, n= N(bs);
    for (i=0; i<n; i++) {
      sx(i)= 0;
      sy(i)= 0;
    }
  }
  position ();
}

composite_box_rep::composite_box_rep (
  path ip, array<box> B, array<SI> x, array<SI> y):
    box_rep (ip)
{
  bs= B;
  int i, n= subnr();
  for (i=0; i<n; i++) {
    sx(i)= x[i];
    sy(i)= y[i];
  }
  position ();
}

composite_box_rep::~composite_box_rep () {}

void
composite_box_rep::insert (box b, SI x, SI y) {
  int n= N(bs);
  bs << b;
  sx(n)= x;
  sy(n)= y;
}

void
composite_box_rep::position () {
  int i, n= subnr();
  if (n == 0) {
    x1= y1= x3= y3= 0;
    x2= y2= x4= y4= 0;
    FAILED ("empty composite box");
  }
  else {
    x1= y1= x3= y3= MAX_SI;
    x2= y2= x4= y4= -MAX_SI;
    for (i=0; i<n; i++) {
      x1= min (x1, sx1(i));
      y1= min (y1, sy1(i));
      x2= max (x2, sx2(i));
      y2= max (y2, sy2(i));
      x3= min (x3, sx3(i));
      y3= min (y3, sy3(i));
      x4= max (x4, sx4(i));
      y4= max (y4, sy4(i));
    }
  }
}

void
composite_box_rep::left_justify () {
  int i, n= subnr();
  SI d= x1;
  x1-=d; x2-=d; x3-=d; x4-=d;
  for (i=0; i<n; i++) sx(i) -= d;
}

/******************************************************************************
* Routines for composite boxes
******************************************************************************/

void
composite_box_rep::display (renderer ren) {
  (void) ren;
}

int
composite_box_rep::subnr () {
  return N(bs);
}

box
composite_box_rep::subbox (int i) {
  return bs[i];
}

tree
composite_box_rep::action (tree t, SI x, SI y, SI delta) {
  int m= find_child (x, y, delta, true);
  if (m == -1) return "";
  else return bs[m]->action (t, x- sx(m), y- sy(m),
			     delta + get_delta (x, x1, x2));
}

void
composite_box_rep::loci (SI x, SI y, SI delta,
			 list<string>& ids, rectangles& rs)
{
  int m= find_child (x, y, delta, true);
  if (m == -1) box_rep::loci (x, y, delta, ids, rs);
  else {
    bs[m]->loci (x- sx(m), y- sy(m), delta + get_delta (x, x1, x2), ids, rs);
    rs= translate (rs, sx(m), sy(m));
  }
}

void
composite_box_rep::collect_page_numbers (hashmap<string,tree>& h, tree page) {
  int i, n= N(bs);
  for (i=0; i<n; i++)
    bs[i]->collect_page_numbers (h, page);
}

path
composite_box_rep::find_tag (string name) {
  int i, n= N(bs);
  for (i=0; i<n; i++) {
    path p= bs[i]->find_tag (name);
    if (!is_nil (p)) return p;
  }
  return path ();
}

bool
composite_box_rep::access_allowed () {
  return true;
}

box
composite_box_rep::transform (frame fr) {
  int i;
  array<box> bs;
  for (i= 0; i<subnr(); i++) {
    if (!is_nil (subbox (i))) {
      box sb= subbox (i)->transform (fr);
      if (!is_nil (sb)) bs << sb;
    }
  }
  return N (bs)==0?box ():composite_box (ip, bs);
}

/******************************************************************************
* Cursor routines
******************************************************************************/

void
composite_box_rep::finalize () {
  int i, n= subnr ();
  lip= descend (ip, 0);
  rip= descend (ip, 1);
  for (i=0; i<n; i++) {
    path l= bs[i]->find_lip ();
    path r= bs[i]->find_rip ();
    /*
    cout << "  i  = " << i << "\n";
    cout << "  l  = " << l << "\n";
    cout << "  r  = " << r << "\n";
    */
    if (is_accessible (l) && is_accessible (r)) {
      if (is_decoration (lip) || path_less (reverse (l), reverse (lip)))
	lip= l;
      if (is_decoration (rip) || path_less (reverse (rip), reverse (r)))
	rip= r;
    }
  }
  /*
  cout << ((tree) (*((box_rep*) this))) << " " << ip << "\n";
  cout << "  lip= " << lip << "\n";
  cout << "  rip= " << rip << "\n";
  */
}

int
composite_box_rep::find_child (SI x, SI y, SI delta, bool force) {
  if (outside (x, delta, x1, x2) && (is_accessible (ip) || force)) return -1;
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
composite_box_rep::find_box_path (SI x, SI y, SI delta,
                                  bool force, bool& found) {
  int i, n= subnr(), m= find_child (x, y, delta, force);
  if (m != -1)
    for (i=0; i<=n; i++) {
      int c= (m+i) % n;
      SI xx= x- sx(c), yy= y- sy(c);
      SI dd= delta + get_delta (xx, bs[c]->x1, bs[c]->x2);
      path r= path (c, bs[c]->find_box_path (xx, yy, dd, force, found));
      if (found || i == n) return r;
    }
  return box_rep::find_box_path (x, y, delta, force, found);
}

path
composite_box_rep::find_lip () {
  return lip;
}

path
composite_box_rep::find_rip () {
  return rip;
}

path
composite_box_rep::find_box_path (path p, bool& found) {
  // cout << "Find box path " << box (this) << ", " << p
  //      << "; " << reverse (ip)
  //      << ", " << reverse (find_lip ())
  //      << " -- " << reverse (find_rip ()) << "\n";
  int n= subnr();
  // cout << "Search cursor " << p << " among " << n
  //      << " at " << box (this) << " " << reverse (ip) << "\n";
  if (n == 0) return box_rep::find_box_path (p, found);

  int start= n>>1, acc= start, step= (start+1)>>1;
  bool last= false;
  while (step > 0) {
    path sr= bs[acc]->find_rip ();
    while (!is_accessible (sr)) {
      acc--;
      if (acc<0) break;
      sr= bs[acc]->find_rip ();
    }
    if (acc<0) {
      start= 0;
      break;
    }
    if (path_less (reverse (sr), p)) {
      int old_start= start, old_acc= acc;
      start= min (n-1, start+ step);
      acc  = start;
      while ((acc > old_start) &&
	     (!is_accessible (bs[acc]->find_rip ()))) acc--;
      if (acc == old_start) acc= old_acc;
    }
    else {
      start= max (0, start- step);
      acc  = min (acc, start);
    }
    if (last) break;
    if (step <= 1) last= true;
    step= (step+1)>>1;
  }

  path bp;
  bool flag= false;
  int i= start;
  found= false;
  while (true) {
    path sl= bs[i]->find_lip ();
    path sr= bs[i]->find_rip ();
    // cout << "  " << i << ":\t" << reverse(sl) <<", "<< reverse(sr) << "\n";
    if (is_accessible (sl) && is_accessible (sr) &&
	path_less_eq (reverse (sl), p) && path_less_eq (p, reverse (sr)))
      {
	flag= true;
	bp= path (i, bs[i]->find_box_path (p, found));
	if (found) return bp;
      }
    i++;
    if (i==n) i=0;
    if (i==start) break;
  }

  if (is_accessible (ip) && (path_up (p) == reverse (ip)) && access_allowed ())
    return box_rep::find_box_path (p, found);
  if (flag) return bp;
  if (start > 0) {
    path sl= bs[start-1]->find_rip ();
    path sr= bs[start  ]->find_lip ();
    if (is_accessible (sl) && is_accessible (sr) &&
	path_less_eq (reverse (sl), p) && path_less_eq (p, reverse (sr)))
      {
	int c1= N (common (reverse (sl), p));
	int c2= N (common (reverse (sr), p));
	int i = (c1 >= c2? start-1: start);
	return path (i, bs[i]->find_box_path (p, found));
      }
  }
  return box_rep::find_box_path (p, flag);
}

path
composite_box_rep::find_tree_path (path bp) {
  if (is_atom (bp)) return box_rep::find_tree_path (bp);
  return bs[bp->item]->find_tree_path (bp->next);
}

cursor
composite_box_rep::find_cursor (path bp) {
  if (is_atom (bp)) return box_rep::find_cursor (bp);
  else {
    int i= bp->item;
    cursor cu= bs[i]->find_cursor (bp->next);
    cu->delta -= get_delta (cu->ox, bs[i]->x1, bs[i]->x2);
    cu->ox    += sx(i);
    cu->oy    += sy(i);
    return cu;
  }
}

selection
composite_box_rep::find_selection (path lbp, path rbp) {
  if ((!is_atom (lbp)) && (!is_atom (rbp)) && (lbp->item == rbp->item)) {
    int i= lbp->item;
    selection sel= bs[i]->find_selection (lbp->next, rbp->next);
    return selection (translate (sel->rs, sx(i), sy(i)), sel->start, sel->end);
  }
  else return box_rep::find_selection (lbp, rbp);
}

gr_selections
composite_box_rep::graphical_select (SI x, SI y, SI dist) {
  gr_selections res;
  if (graphical_distance (x, y) <= dist) {
    int i, n= subnr();
    for (i=n-1; i>=0; i--)
      res << bs[i]->graphical_select (x- sx(i), y- sy(i), dist);
  }
  return res;
}

gr_selections
composite_box_rep::graphical_select (SI x1, SI y1, SI x2, SI y2) {
  gr_selections res;
  if (contains_rectangle (x1, y1, x2, y2)) {
    int i, n= subnr();
    for (i=n-1; i>=0; i--)
      res << bs[i]->graphical_select (x1- sx(i), y1- sy(i),
				      x2- sx(i), y2- sy(i));
  }
  return res;
}

/******************************************************************************
* Concrete composite box
******************************************************************************/

int
concrete_composite_box_rep::find_child (SI x, SI y, SI delta, bool force) {
  if (border_flag &&
      outside (x, delta, x1, x2) &&
      (is_accessible (ip) || force)) return -1;
  int i, n= subnr(), d= MAX_SI, m= -1;
  for (i=0; i<n; i++)
    if (distance (i, x, y, delta)< d)
      if (bs[i]->accessible () || force) {
	d= distance (i, x, y, delta);
	m= i;
      }
  return m;
}

/******************************************************************************
* Table box
******************************************************************************/

struct table_box_rep: public concrete_composite_box_rep {
  int rows, cols;
  array<SI> x, y;
  array<string> halign;
  table_box_rep (
    path ip, array<box> bs, array<SI> x2, array<SI> y2,
    array<string> halign2, int cols2):
      concrete_composite_box_rep (ip, bs, x2, y2, false),
      rows (N(bs) / cols2), cols (cols2),
      x (x2), y (y2), halign (halign2) { finalize (); }
  operator tree () { return tree ("table"); }
  box adjust_kerning (int mode, double factor);
  box expand_glyphs (int mode, double factor);
};

box
table_box_rep::adjust_kerning (int mode, double factor) {
  (void) mode;
  int n= N(bs), i, j;
  array<box> adj (n);
  array<SI>  cdw (n);
  for (i=0; i<n; i++) {
    SI l, r;
    adj[i]= bs[i]->adjust_kerning (TABLE_CELL, factor);
    bs[i]->get_cell_extents (l, r);
    SI w1= r - l;
    adj[i]->get_cell_extents (l, r);
    SI w2= r - l;
    cdw[i]= w2 - w1;
  }
  SI dx= 0;
  array<SI> nx (n);
  for (j=0; j<cols; j++) {
    for (i=0; i<rows; i++)
      nx[i*cols+j]= x[i*cols+j] + dx;
    SI dw= MINUS_INFINITY;
    for (i=0; i<rows; i++)
      dw= max (dw, cdw[i*cols+j]);
    dx += dw;
    for (i=0; i<rows; i++) {
      int k= i*cols + j;
      string ha= halign[k];
      SI d= dw - cdw[k];
      SI cdx= 0;
      if (ha[0] == 'l') cdx= 0;
      if (ha[0] == 'r') cdx= d;
      if (ha[0] == 'c') cdx= d >> 1;
      adj[k]= adj[k]->adjust_cell_geometry (cdx, 0, dw);
    }
  }
  return table_box (ip, adj, nx, y, halign, cols);
}

box
table_box_rep::expand_glyphs (int mode, double factor) {
  (void) mode;
  int n= N(bs), i, j;
  array<box> adj (n);
  array<SI>  cdw (n);
  for (i=0; i<n; i++) {
    SI l, r;
    adj[i]= bs[i]->expand_glyphs (TABLE_CELL, factor);
    bs[i]->get_cell_extents (l, r);
    SI w1= r - l;
    adj[i]->get_cell_extents (l, r);
    SI w2= r - l;
    cdw[i]= w2 - w1;
  }
  SI dx= 0;
  array<SI> nx (n);
  for (j=0; j<cols; j++) {
    for (i=0; i<rows; i++)
      nx[i*cols+j]= x[i*cols+j] + dx;
    SI dw= MINUS_INFINITY;
    for (i=0; i<rows; i++)
      dw= max (dw, cdw[i*cols+j]);
    dx += dw;
    for (i=0; i<rows; i++) {
      int k= i*cols + j;
      string ha= halign[k];
      SI d= dw - cdw[k];
      SI cdx= 0;
      if (ha[0] == 'l') cdx= 0;
      if (ha[0] == 'r') cdx= d;
      if (ha[0] == 'c') cdx= d >> 1;
      adj[k]= adj[k]->adjust_cell_geometry (cdx, 0, dw);
    }
  }
  return table_box (ip, adj, nx, y, halign, cols);
}

/******************************************************************************
* User interface
******************************************************************************/

box
composite_box (path ip, array<box> bs, bool bfl) {
  return tm_new<concrete_composite_box_rep> (ip, bs, bfl);
}

box
composite_box (path ip, array<box> bs, array<SI> x, array<SI> y, bool bfl) {
  return tm_new<concrete_composite_box_rep> (ip, bs, x, y, bfl);
}

box
table_box (path ip, array<box> bs, array<SI> x, array<SI> y,
           array<string> halign, int cols) {
  return tm_new<table_box_rep> (ip, bs, x, y, halign, cols);
}
