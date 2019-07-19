
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
#include "Boxes/change.hpp"
#include "colors.hpp"

/******************************************************************************
* A page box contains a main box and decorations
******************************************************************************/

struct scatter_box_rep: composite_box_rep {
  tree       page;
  box        decoration;
  rectangles rs;

  scatter_box_rep (path ip, array<box> bs, array<SI> x, array<SI> y, bool f);
  operator tree ();
  int       find_child (SI x, SI y, SI delta, bool force);
  path      find_left_box_path ();
  path      find_right_box_path ();
  selection find_selection (path lbp, path rbp);
  void      pre_display (renderer& ren);
  void      display_background (renderer ren);
};

scatter_box_rep::scatter_box_rep (
  path ip2, array<box> bs, array<SI> x, array<SI> y, bool f):
    composite_box_rep (ip2, bs, x, y)
{
  finalize ();
  if (f) {
    rectangles all (rectangle (x1, y1, x2, y2));
    for (int i=0; i<N(bs); i++)
      rs= rectangles (rectangle (sx1(i), sy1(i), sx2(i), sy2(i)), rs);
    rs= all - rs;
  }
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

void
scatter_box_rep::pre_display (renderer& ren) {
  display_background (ren);
}

void
scatter_box_rep::display_background (renderer ren) {
  if (is_nil (rs)) return;
  brush bgc= ren->get_background ();
  ren->set_background (tm_background);
  rectangles rects= rs;
  while (!is_nil (rects)) {
    rectangle r= rects->item;
    ren->clear_pattern (r->x1, r->y1, r->x2, r->y2);
    rects= rects->next;
  }
  ren->set_background (bgc);
}

/******************************************************************************
* A page box contains a main box and decorations
******************************************************************************/

struct page_box_rep: composite_box_rep {
  tree  page;
  int   page_nr;
  brush page_bgc;
  box   decoration;
  int   old_page;

  page_box_rep (path ip, tree page, int page_nr, brush bgc, SI w, SI h,
		array<box> bs, array<SI> x, array<SI> y, box dec);
  operator tree ();
  int find_child (SI x, SI y, SI delta, bool force);
  void display_background (renderer ren);
  void redraw_background (renderer ren);
  void pre_display (renderer& ren);
  void post_display (renderer& ren);
  void display (renderer ren);
  void clear_incomplete (rectangles& rs, SI pixel, int i, int i1, int i2);
  void collect_page_numbers (hashmap<string,tree>& h, tree page);
  void collect_page_colors (array<brush>& bs, array<rectangle>& rs);
  path find_left_box_path ();
  path find_right_box_path ();
};

page_box_rep::page_box_rep (path ip2, tree p2, int nr2, brush bgc, SI w, SI h,
			    array<box> bs, array<SI> x, array<SI> y, box dec):
  composite_box_rep (ip2, bs, x, y),
  page (p2), page_nr (nr2), page_bgc (bgc), decoration (dec), old_page (0)
{
  x1= min (x1, 0);
  x2= max (x2, w);
  y1= -h;
  y2=  0;
  x3= min (x3,  0);
  x4= max (x4,  w);
  y3= min (y3, -h);
  y4= max (y4,  0);
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
page_box_rep::display_background (renderer ren) {
  if (page_bgc->get_type () != brush_none) {
    brush bgc= ren->get_background ();
    ren->set_background (page_bgc);
    ren->clear_pattern (x1, y1, x2, y2);
    ren->set_background (bgc);
  }
  else ren->clear_pattern (x1, y1, x2, y2);
}

void
page_box_rep::redraw_background (renderer ren) {
  ren->move_origin (x0, y0);
  display_background (ren);
  ren->move_origin (-x0, -y0);
}

void
page_box_rep::pre_display (renderer& ren) {
  display_background (ren);
  old_page= ren->cur_page;
  ren->set_page_nr (page_nr);
}

void
page_box_rep::post_display (renderer& ren) {
  ren->set_page_nr (old_page);
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

void
page_box_rep::collect_page_colors (array<brush>& bs, array<rectangle>& rs) {
  bs << page_bgc;
  rs << rectangle (x1, y1, x2, y2);
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
* Page border boxes
******************************************************************************/

struct page_border_box_rep: change_box_rep {
  color tmb;
  SI l, r, b, t, pixel;
  page_border_box_rep (path ip, box pb, color tmb,
                       SI l, SI r, SI b, SI t, SI pixel);
  operator tree ();
  void pre_display (renderer& ren);
  void display_background (renderer ren);
};

page_border_box_rep::page_border_box_rep (path ip2, box pb, color tmb2,
                                          SI l2, SI r2, SI b2, SI t2,
                                          SI pixel2):
  change_box_rep (ip2, false), tmb (tmb2),
  l (l2), r (r2), b (b2), t (t2), pixel (pixel2)
{
  insert (pb, l, -t);
  position ();
  x1 -= l; x2 += r;
  y1 -= b; y2 += t;
  x3 -= l; x4 += r;
  y3 -= b; y4 += t;
  finalize ();
}

page_border_box_rep::operator tree () {
  return tree (TUPLE, "bordered", (tree) bs[0]);
}

void
page_border_box_rep::pre_display (renderer& ren) {
  display_background (ren);
}

static void
set_shadow (renderer ren, color bg, SI alpha) {
  color c= rgb_color (0, 0, 0, alpha);
  ren->set_background (blend_colors (c, bg));
}

void
page_border_box_rep::display_background (renderer ren) {
  if (!ren->is_screen) return;
  brush bgc= ren->get_background ();
  ren->set_background (tmb);
  SI X1= sx1(0), Y1= sy1(0), X2= sx2(0), Y2= sy2(0);
  if (X1 > x1) ren->clear_pattern (x1, y1, X1, y2);
  if (x2 > X2) ren->clear_pattern (X2, y1, x2, y2);
  if (Y1 > y1) ren->clear_pattern (X1, y1, X2, Y1);
  if (y2 > Y2) ren->clear_pattern (X1, Y2, X2, y2);

  SI p= ren->pixel;
  if (X1 > x1 + 2 * p) {
    set_shadow (ren, tmb, 128);
    ren->clear_pattern (X1-p, Y1-p, X1, Y2-p);
    set_shadow (ren, tmb, 16);
    ren->clear_pattern (X1-2*p, Y1-p, X1-p, Y2-2*p);
  }
  if (x2 - 2 * p > X2) {
    set_shadow (ren, tmb, 16);
    ren->clear_pattern (X2+p, Y1, X2+2*p, Y2-2*p);
    set_shadow (ren, tmb, 128);
    ren->clear_pattern (X2, Y1, X2+p, Y2-p);
  }
  if (Y1 > y1 - 4 * p) {
    set_shadow (ren, tmb, 160);
    ren->clear_pattern (X1, Y1-p, X2, Y1);
    set_shadow (ren, tmb, 128);
    ren->clear_pattern (X1-p, Y1-2*p, X2+p, Y1-p);
    set_shadow (ren, tmb, 64);
    ren->clear_pattern (X1-p, Y1-3*p, X2+p, Y1-2*p);
    set_shadow (ren, tmb, 16);
    ren->clear_pattern (X1-p, Y1-4*p, X2+p, Y1-3*p);
  }

  ren->set_background (bgc);
}

/******************************************************************************
* Crop marks boxes
******************************************************************************/

struct crop_marks_box_rep: change_box_rep {
  SI w, h, lw, ll;
  crop_marks_box_rep (path ip, box pb, SI w, SI h, SI lw, SI ll);
  operator tree ();
  void pre_display (renderer& ren);
  void display_background (renderer ren);
};

crop_marks_box_rep::crop_marks_box_rep (path ip2, box pb,
                                        SI w2, SI h2, SI lw2, SI ll2):
  change_box_rep (ip2, false), w (w2), h (h2), lw (lw2), ll (ll2)
{
  SI dw= w - pb->w();
  SI dh= h - pb->h();
  insert (pb, dw/2 - pb->x1, -dh/2);
  position ();
  x1 = 0; y1 = -h;
  x2 = w; y2 = 0;
  x3 = min (0, x3); y3 = min (-h, y3);
  x4 = max (0, x4); y4 = max (0, y4);
  finalize ();
}

crop_marks_box_rep::operator tree () {
  return tree (TUPLE, "crop-marks", (tree) bs[0]);
}

void
crop_marks_box_rep::pre_display (renderer& ren) {
  display_background (ren);
}

void
crop_marks_box_rep::display_background (renderer ren) {
  SI X1= sx1(0), Y1= sy1(0), X2= sx2(0), Y2= sy2(0);
  
  brush old_bgc= ren->get_background ();
  ren->set_background (white);
  if (X1 > x1) ren->clear_pattern (x1, y1, X1, y2);
  if (x2 > X2) ren->clear_pattern (X2, y1, x2, y2);
  if (Y1 > y1) ren->clear_pattern (x1, y1, x2, Y1);
  if (y2 > Y2) ren->clear_pattern (x1, Y2, x2, y2);
  ren->set_background (old_bgc);

  pencil old_pen= ren->get_pencil ();
  pencil pen= pencil (black, lw);
  ren->set_pencil (pen);
  ren->line (X1-ll, Y1   , X1-lw, Y1   );
  ren->line (X1-ll, Y2   , X1-lw, Y2   );
  ren->line (X2+lw, Y1   , X2+ll, Y1   );
  ren->line (X2+lw, Y2   , X2+ll, Y2   );
  ren->line (X1   , Y1-ll, X1   , Y1-lw);
  ren->line (X2   , Y1-ll, X2   , Y1-lw);
  ren->line (X1   , Y2+lw, X1   , Y2+ll);
  ren->line (X2   , Y2+lw, X2   , Y2+ll);
  ren->set_pencil (old_pen);
}

/******************************************************************************
* box construction routines
******************************************************************************/

box
scatter_box (path ip, array<box> bs, array<SI> x, array<SI> y, bool f) {
  return tm_new<scatter_box_rep> (ip, bs, x, y, f);
}

box
page_box (path ip, tree page, int page_nr, brush bgc, SI w, SI h,
	  array<box> bs, array<SI> bs_x, array<SI> bs_y,
	  array<box> decs, array<SI> decs_x, array<SI> decs_y) {
  box dec;
  if (N (decs) > 0) dec= composite_box (ip, decs, decs_x, decs_y, false);
  return tm_new<page_box_rep> (ip, page, page_nr, bgc,
                               w, h, bs, bs_x, bs_y, dec);
}

box
page_border_box (path ip, box pb, color tmb, SI l, SI r, SI b, SI t, SI pixel) {
  box rb= tm_new<page_border_box_rep> (ip, pb, tmb, l, r, b, t, pixel);
  return rb;
}

box
crop_marks_box (path ip, box pb, SI w, SI h, SI lw, SI ll) {
  box rb= tm_new<crop_marks_box_rep> (ip, pb, w, h, lw, ll);
  return rb;
}
