
/******************************************************************************
* MODULE     : boxes.cpp
* DESCRIPTION: Important routines for all boxes
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "boxes.hpp"
#include "formatter.hpp"
#include "point.hpp"
#include "printer.hpp"
#include "file.hpp"
#include "merge_sort.hpp"
#include "player.hpp"

extern tree the_et;

/******************************************************************************
* Default settings for virtual routines
******************************************************************************/

int box_rep::subnr () { return 0; }
box box_rep::subbox (int i) { (void) i; return box (); }
box box::operator [] (path p) {
  if (is_nil (p)) return *this; else return rep->subbox(p->item)[p->next]; }
double box_rep::left_slope () { return 0.0; }
double box_rep::right_slope () { return 0.0; }
SI box_rep::left_correction () { return (SI) (-min (0, y1) * left_slope ()); }
SI box_rep::right_correction () { return (SI) (max (0, y2) * right_slope ()); }
SI box_rep::lsub_correction () { return 0; }
SI box_rep::lsup_correction () { return 0; }
SI box_rep::rsub_correction () { return 0; }
SI box_rep::rsup_correction () { return 0; }
SI box_rep::sub_lo_base (int level) { (void) level; return y1; }
SI box_rep::sub_hi_lim  (int level) { (void) level; return y1 + ((y2-y1)/3); }
SI box_rep::sup_lo_lim  (int level) { (void) level; return (y1 + y2) >> 1; }
SI box_rep::sup_lo_base (int level) { (void) level; return y2 - ((y2-y1)/3); }
SI box_rep::sup_hi_lim  (int level) { (void) level; return y2; }
SI box_rep::wide_correction (int mode) { (void) mode; return 0; }
void box_rep::get_bracket_extents (SI& lo, SI& hi) { lo= y1; hi= y2; }

/******************************************************************************
* Positioning routines
******************************************************************************/

bool
outside (SI x, SI delta, SI x1, SI x2) {
  return
    (x<x1) || ((x==x1) && (delta<0)) ||
    (x>x2) || ((x==x2) && (delta>=0));
}

SI
get_delta (SI x, SI x1, SI x2) {
  if (x1==x2) return 0;
  if (x==x1) return -1;
  if (x==x2) return 1;
  return 0;
}

SI
box_rep::distance (int i, SI x, SI y, SI delta) {
  box b= subbox (i);
  x -= sx(i);
  y -= sy(i);
  int dx, dy;
  if (x <=  b->x1) dx = b->x1- x- (delta<0? 1:0);
  else if (x >=  b->x2) dx = x- b->x2+ (delta<0? 0:1);
  else dx = 0;
  if (y <  b->y1) dy = b->y1- y;
  else if (y >= b->y2) dy = y- b->y2;
  else dy = 0;
  return dx+dy;
}

bool
box_rep::in_rectangle (SI X1, SI Y1, SI X2, SI Y2) {
  return x1>=X1 && y1>=Y1 && x2<=X2 && y2<=Y2;
}

bool
box_rep::contains_rectangle (SI X1, SI Y1, SI X2, SI Y2) {
  return x1<=X1 && y1<=Y1 && x2>=X2 && y2>=Y2;
}

box
box_rep::adjust_kerning (int mode, double factor) {
  (void) mode; (void) factor;
  return this;
}

box
box_rep::expand_glyphs (int mode, double factor) {
  (void) mode; (void) factor;
  return this;
}

void
box_rep::get_cell_extents (SI& l, SI& r) {
  (void) l; (void) r;
  FAILED ("cell box expected");
}

box
box_rep::adjust_cell_geometry (SI dx, SI dl, SI dr) {
  (void) dx; (void) dl; (void) dr;
  FAILED ("cell box expected");
  return this;
}

/******************************************************************************
* Cursor routines
******************************************************************************/

path
box_rep::find_box_path (SI x, SI y, SI delta, bool force, bool& found) {
  (void) y;
  (void) force;
  found= true;
  SI m= (x1+x2)>>1;
  return path (((x<m) || ((x==m) && (delta<0)))? 0: 1);
}

path
box_rep::find_lip () {
  return descend (ip, 0);
}

path
box_rep::find_rip () {
  return descend (ip, 1);
}

path
box_rep::find_left_box_path () {
  return path (0);
}

path
box_rep::find_right_box_path () {
  return path (1);
}

path
box_rep::find_box_path (path p, bool& found) {
  // cout << "Find box path " << box (this) << ", " << p
  //      << "; " << reverse (ip)
  //      << ", " << reverse (find_lip ())
  //      << " -- " << reverse (find_rip ()) << "\n";
  found= (!is_nil(p)) && is_accessible (ip);
  if (last_item (p) == 0) return path (0);
  else return path (1);
}

path
box_rep::find_tree_path (path bp) {
  if (bp == path (0)) return reverse (descend_decode (ip, 0));
  else return reverse (descend_decode (ip, 1));
}

cursor
box_rep::find_cursor (path bp) {
  bool flag= bp == path (0);
  double slope= flag? left_slope (): right_slope ();
  cursor cu (flag? x1: x2, 0);
  cu->y1= y1; cu->y2= y2;
  cu->slope= slope;
  return cu;
}

selection
box_rep::find_selection (path lbp, path rbp) {
  if (lbp == rbp)
    return selection (rectangles (),
		      find_tree_path (lbp), find_tree_path (rbp));
  else
    return selection (rectangle (x1, y1, x2, y2),
		      find_tree_path (path (0)), find_tree_path (path (1)));
}

path
box_rep::find_tree_path (SI x, SI y, SI delta) {
  bool found;
  path bp= find_box_path (x, y, delta, false, found);
  //cout << "Find " << x << ", " << y << "; " << delta;
  //cout << " -> " << bp << "\n";
  return find_tree_path (bp);
}

cursor
box_rep::find_check_cursor (path p) {
  bool found;
  path bp= find_box_path (p, found);
  cursor cu= find_cursor (bp);
  cu->valid= found;
  return cu;
}

selection
box_rep::find_check_selection (path lp, path rp) {
  bool lfound= false, rfound= false;
  path lbp= find_box_path (lp, lfound);
  path rbp= find_box_path (rp, rfound);
  selection sel= find_selection (lbp, rbp);
  sel->valid= lfound && rfound;
  return sel;
}

void
box_rep::relocate (path new_ip, bool force) {
  if (!force)
    if (is_nil (ip) || (ip->item >= 0) || (ip == new_ip)) return;
  ip= new_ip;
  int i, n= subnr ();
  for (i=0; i<n; i++) subbox (i)->relocate (ip, force);
}

box
box_rep::transform (frame fr) {
  (void) fr;
  return box ();
}

/******************************************************************************
* Modified cursor routines in presence of scrolled boxes
******************************************************************************/

path
find_innermost_scroll (box b, path p) {
  // Given a box b and a logical path p, this routine returns 
  // the longest box path sp such that b[sp] is a scroll node
  path bp;
  while (true) {
    bool found= false;
    bp= b->find_box_path (p, found);
    if (found) break;
    p= path_up (p);
    if (is_nil (p)) return path ();
  }
  bp= path_up (bp);
  path cp, sp;
  while (!is_nil (bp)) {
    if (b->get_type () == SCROLL_BOX) sp= reverse (cp);
    b = b[bp->item];
    cp= path (bp->item, cp);
    bp= bp->next;
  }
  if (is_nil (sp)) return sp;
  else return sp * 0;
}

path
find_scrolled_box_path (box b, path sp, SI x, SI y, SI delta) {
  if (is_nil (sp)) {
    bool found;
    return b->find_box_path (x, y, delta, false, found);
  }
  else {
    int m= sp->item;
    SI xx= x - b->sx (m), yy= y - b->sy (m);
    SI dd= delta + get_delta (xx, b[m]->x1, b[m]->x2);
    return path (m, find_scrolled_box_path (b[m], sp->next, xx, yy, dd));
  }
}

/*
void
debug (box b, path bp) {
  tree t= (tree) b;
  if (is_tuple (t) && N(t) > 0) cout << t[0];
  else cout << t;
  cout << ", " << bp << "\n";
  if (!is_nil (bp))
    debug (b[bp->item], bp->next);
}
*/

path
find_scrolled_tree_path (box b, path sp, SI x, SI y, SI delta) {
  path bp= find_scrolled_box_path (b, sp, x, y, delta);
  //cout << "Find " << x << ", " << y << "; " << delta;
  //cout << " -> " << bp << "\n";
  return b->find_tree_path (bp);
}

void
find_canvas_info (box b, path sp, SI& x, SI& y, SI& sx, SI& sy,
		  rectangle& outer, rectangle& inner)
{
  if (is_nil (sp)) {
    x= y= sx= sy= 0;
    outer= inner= rectangle (0, 0, 0, 0);
  }
  else if (is_atom (sp)) {
    x    = 0;
    y    = 0;
    sx   = b->sx (0);
    sy   = b->sy (0);
    outer= rectangle (b->x1, b->y1, b->x2, b->y2);
    inner= rectangle (b[0]->x1, b[0]->y1, b[0]->x2, b[0]->y2);
  }
  else {
    find_canvas_info (b[sp->item], sp->next, x, y, sx, sy, outer, inner);
    x += b->sx (sp->item);
    y += b->sy (sp->item);
  }
}

/******************************************************************************
* For graphical boxes
******************************************************************************/

frame
box_rep::get_frame () {
  return frame ();
}

grid
box_rep::get_grid () {
  return grid ();
}

void
box_rep::get_limits (point& lim1, point& lim2) {
  lim1= point (); lim2= point ();
}

frame
box_rep::find_frame (path bp, bool last) {
  SI    x= 0;
  SI    y= 0;
  box   b= this;
  frame f= get_frame ();
  while (!is_nil (bp)) {
    x += b->sx (bp->item);
    y += b->sy (bp->item);
    b  = b->subbox (bp->item);
    bp = bp->next;
    frame g= b->get_frame ();
    if (!is_nil (g)) {
      if (last)
	f= g;
      else
	f= scaling (1.0, point (x, y)) * g;
    }
  }
  return f;
}

grid
box_rep::find_grid (path bp) {
  box  b= this;
  grid g= get_grid ();
  while (!is_nil (bp)) {
    b  = b->subbox (bp->item);
    bp = bp->next;
    grid g2= b->get_grid ();
    if (!is_nil (g2)) g= g2;
  }
  return g;
}

void
box_rep::find_limits (path bp, point& lim1, point& lim2) {
  box b= this;
  get_limits (lim1, lim2);
  while (!is_nil (bp)) {
    point slim1, slim2;
    b  = b->subbox (bp->item);
    bp = bp->next;
    b->get_limits (slim1, slim2);
    if (slim1 != point ()) {
      lim1= slim1;
      lim2= slim2;
    }
  }
}

SI
box_rep::graphical_distance (SI x, SI y) {
  SI dx, dy;
  if (x <=  x1) dx= x1 - x;
  else if (x >=  x2) dx= x - x2;
  else dx= 0;
  if (y <  y1) dy= y1 - y;
  else if (y >= y2) dy= y - y2;
  else dy= 0;
  return (SI) norm (point (dx, dy));
}

gr_selections
box_rep::graphical_select (SI x, SI y, SI dist) {
  gr_selections res;
  if (graphical_distance (x, y) <= dist) {
    gr_selection gs;
    gs->type= "box";
    gs->dist= graphical_distance (x, y);
    gs->cp << find_tree_path (x, y, dist);
    // FIXME: check whether this is correct: I do not remember whether
    // find_tree_path returns an absolute or a relative path
    gs->c= curve ();
    res << gs;
  }
  return res;
}

gr_selections
box_rep::graphical_select (SI x1, SI y1, SI x2, SI y2) {
  gr_selections res;
  if (in_rectangle (x1, y1, x2, y2)) {
    gr_selection gs;
    gs->type= "box";
    gs->dist= graphical_distance (x1, y1);
    SI dist= (SI)norm (point (x2-x1, y2-y1));
    gs->cp << find_tree_path (x1, y1, dist);
    // FIXME: as above, check whether this is correct or not
    gs->pts= array<point> (0);
    gs->c= curve ();
    res << gs;
  }
  return res;
}

/******************************************************************************
* Getting information from boxes
******************************************************************************/

int
box_rep::get_type () {
  return STD_BOX;
}

tree
box_rep::get_info (tree in) {
  (void) in;
  return "";
}

int
box_rep::get_leaf_left_pos () {
  failed_error << "The box is " << box (this) << "\n";
  FAILED ("this box is not textual");
  return 0;
}

int
box_rep::get_leaf_right_pos () {
  failed_error << "The box is " << box (this) << "\n";
  FAILED ("this box is not textual");
  return 0;
}

string
box_rep::get_leaf_string () {
  failed_error << "The box is " << box (this) << "\n";
  FAILED ("this box is not textual");
  return "";
}

font
box_rep::get_leaf_font () {
  failed_error << "The box is " << box (this) << "\n";
  FAILED ("this box is not textual");
  return font ();
}

pencil
box_rep::get_leaf_pencil () {
  failed_error << "The box is " << box (this) << "\n";
  FAILED ("this box is not textual");
  return pencil (false);
}

language
box_rep::get_leaf_language () {
  failed_error << "The box is " << box (this) << "\n";
  FAILED ("this box is not textual");
  return language ();
}

tree
box_rep::get_leaf_tree () {
  failed_error << "The box is " << box (this) << "\n";
  FAILED ("no tree attached to this box");
  return "";
}

box
box_rep::get_leaf_box () {
  failed_error << "The box is " << box (this) << "\n";
  FAILED ("no box attached to this box");
  return box ();
}

lazy
box_rep::get_leaf_lazy () {
  failed_error << "The box is " << box (this) << "\n";
  FAILED ("no lazy attached to this box");
  return lazy ();
}

SI
box_rep::get_leaf_offset (string search) {
  (void) search;
  return w();
}

/******************************************************************************
* Redrawing boxes
******************************************************************************/

int nr_painted= 0;

void
clear_pattern_rectangles (renderer ren, rectangle m, rectangles l) {
  while (!is_nil (l)) {
    rectangle r (l->item);
    ren->clear_pattern (m->x1- ren->ox, m->y1- ren->oy,
			m->x2- ren->ox, m->y2- ren->oy,
                        r->x1- ren->ox, r->y1- ren->oy,
			r->x2- ren->ox, r->y2- ren->oy);
    l= l->next;
  }
}

int
box_rep::reindex (int i, int item, int n) {
  if (item<0) item=0;
  if (item>n) item=n;
  if (i==0) return item;
  if ((i <= (item<<1)) && (i <= ((n-item)<<1))) {
    int d=(i+1)>>1;
    if (((i+1)&1)==0) return item-d;
    else return item+d;
  }
  if (i > (item<<1)) return i;
  return n-i;
}

void
box_rep::redraw (renderer ren, path p, rectangles& l) {
  if ((nr_painted&15) == 15 && ren->is_screen && gui_interrupted (true)) return;
  ren->move_origin (x0, y0);
  SI delta= ren->retina_pixel; // adjust visibility to compensate truncation
  if (ren->is_visible (x3- delta, y3- delta, x4+ delta, y4+ delta)) {
    rectangles ll;
    l= rectangles();
    pre_display (ren);
    
    int i, item=-1, n=subnr (), i1= n, i2= -1;
    if (!is_nil(p)) i1= i2= item= p->item;
    for (i=0; i<n; i++) {
      int k= reindex (i, item, n-1);
      if (is_nil(p)) subbox (k)->redraw (ren, path (), ll);
      else if (i!=0) {
        if (k > item) subbox(k)->redraw (ren, path (0), ll);
        else subbox(k)->redraw (ren, path (subbox(k)->subnr()-1), ll);
      }
      else subbox(k)->redraw (ren, p->next, ll);
      if (!is_nil(ll)) {
        i1= min (i1, k);
        i2= max (i2, k);
        l = ll * l;
        ll= rectangles ();
      }
    }
    
    if ((nr_painted&15) == 15 && ren->is_screen && gui_interrupted ()) {
      l= translate (l, -ren->ox, -ren->oy);
      clear_incomplete (l, ren->retina_pixel, item, i1, i2);
      l= translate (l, ren->ox, ren->oy);
    }
    else {
      l= rectangle (x3+ ren->ox, y3+ ren->oy, x4+ ren->ox, y4+ ren->oy);
      display (ren);
      if (!ren->is_screen) display_links (ren);
      if (nr_painted < 15) ren->apply_shadow (x1, y1, x2, y2);
      nr_painted++;
    }

    post_display (ren);
  }
  ren->move_origin (-x0, -y0);
}

void
box_rep::redraw (renderer ren, path p, rectangles& l, SI x, SI y) {
  ren->move_origin (x, y);
  redraw (ren, p, l);
  ren->move_origin (-x, -y);
}

void
box_rep::clear_incomplete (rectangles& rs, SI pixel, int i, int i1, int i2) {
  (void) rs; (void) pixel; (void) i; (void) i1; (void) i2;
}

void
box_rep::pre_display (renderer &ren) {
  (void) ren;
}

void
box_rep::post_display (renderer &ren) {
  (void) ren;
}

void
box_rep::display_background (renderer ren) {
  (void) ren;
}

void
box_rep::redraw_background (renderer ren) {
  ren->move_origin (x0, y0);
  display_background (ren);
  int i, n=subnr ();
  for (i=0; i<n; i++)
    subbox (i)->redraw_background (ren);
  ren->move_origin (-x0, -y0);
}

void
box_rep::clear (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  SI old_x1, old_y1, old_x2, old_y2;
  ren->get_clipping (old_x1, old_y1, old_x2, old_y2);
  if (max (old_x1, x1) < min (old_x2, x2) &&
      max (old_y1, y1) < min (old_y2, y2)) {
    ren->extra_clipping (x1, y1, x2, y2);
    redraw_background (ren);
    ren->set_clipping (old_x1, old_y1, old_x2, old_y2, true);
  }
}

/******************************************************************************
* The cursor class
******************************************************************************/

cursor::cursor (SI x, SI y, SI delta, SI y1, SI y2, double slope, bool valid):
  rep (tm_new<cursor_rep> ())
{
  rep->ox= x ; rep->oy= y ; rep->delta= delta;
  rep->y1= y1; rep->y2= y2; rep->slope= slope;
  rep->valid= valid;
}

cursor
copy (cursor cu) {
  return cursor (cu->ox, cu->oy, cu->delta, cu->y1, cu->y2,
		 cu->slope, cu->valid);
}

bool
operator == (cursor cu1, cursor cu2) {
  return
    (cu1->ox == cu2->ox) && (cu1->oy == cu2->oy) &&
    // (cu1->delta == cu2->delta) &&
    (cu1->y1 == cu2->y1) && (cu1->y2 == cu2->y2) &&
    (cu1->slope == cu2->slope);
}

bool
operator != (cursor cu1, cursor cu2) {
  return ! (cu1 == cu2);
}

tm_ostream&
operator << (tm_ostream& out, cursor cu) {
  out << "cursor (" << (cu->ox>>8) << ", " << (cu->oy>>8) << ": "
      << cu->delta << ": "
      << (cu->y1>>8) << ", " << (cu->y2>>8) << ": "
      << cu->slope << ")";
  return out;
}

/******************************************************************************
* Selections
******************************************************************************/

selection::selection (rectangles rs, path start, path end, bool valid):
  rep (tm_new<selection_rep> ())
{
  rep->rs   = rs;
  rep->start= start;
  rep->end  = end;
  rep->valid= valid;
}

bool
operator == (selection sel1, selection sel2) {
  return
    (sel1->start == sel2->start) &&
    (sel1->end == sel2->end);
}

bool
operator != (selection sel1, selection sel2) {
  return !(sel1 == sel2);
}

tm_ostream&
operator << (tm_ostream& out, selection sel) {
  return out << "selection (" << sel->start << ", " << sel->end << ")";
}

/******************************************************************************
* Graphical selections
******************************************************************************/

gr_selection::gr_selection (array<path> cp, SI dist):
  rep (tm_new<gr_selection_rep> ())
{
  rep->cp  = cp;
  rep->dist= dist;
}

tm_ostream&
operator << (tm_ostream& out, gr_selection sel) {
  return out << "gr_selection (" << sel->type << ", "
	     << sel->dist << ", " << sel->cp << ")";
}

struct less_eq_gr_selection {
  static inline bool leq (gr_selection& a, gr_selection& b) {
    return a->dist <= b->dist; }
};

void
sort (gr_selections& sels) {
  merge_sort_leq <gr_selection, less_eq_gr_selection> (sels);
}

tree
as_tree (gr_selections sels) {
  sort (sels);
  int i, n= N(sels);
  array<array<path> > res (n);
  for (i=0; i<n; i++)
    res[i]= sels[i]->cp;
  return (tree) res;
}

/******************************************************************************
* Animations
******************************************************************************/

player box_rep::anim_player () { return player (); }

double
box_rep::anim_delay () {
  int i, n= subnr ();
  double r= 0.0;
  for (i=0; i<n; i++) {
    double sr= subbox (i)->anim_delay ();
    r= max (r, sr);
  }
  return r;
}

double
box_rep::anim_duration () {
  int i, n= subnr ();
  double r= 0.0;
  for (i=0; i<n; i++) {
    double sr= subbox (i)->anim_duration ();
    r= max (r, sr);
  }
  return r;
}

void
box_rep::anim_position (double t) {
  int i, n= subnr ();
  for (i=0; i<n; i++)
    subbox (i)->anim_position (t);
}

double
box_rep::anim_next () {
  double r= 1.0e12;
  int i, n= subnr ();
  for (i=0; i<n; i++) {
    double sr= subbox (i)->anim_next ();
    r= min (r, sr);
  }
  return r;
}

rectangles
box_rep::anim_invalid () {
  rectangles rs;
  int i, n= subnr ();
  for (i=0; i<n; i++) {
    rectangles rs2= subbox (i)->anim_invalid ();
    rs2= translate (rs2, sx (i), sy (i));
    rs << rs2;
  }
  return rs;
}

/******************************************************************************
* Miscellaneous routines
******************************************************************************/

tree
box_rep::action (tree t, SI x, SI y, SI delta) {
  (void) x; (void) y; (void) delta; (void) t;
  return "";
}

void
box_rep::loci (SI x, SI y, SI delta, list<string>& ids, rectangles& rs) {
  (void) x; (void) y; (void) delta;  
  ids= list<string> ();
  rs = rectangles ();
}

void
box_rep::display_links (renderer ren) {
  if (!is_nil (ip) && ip->item >= 0 && x2 > x1 && y2 > y1) {
    path p= reverse (ip);
    while (N(p) > 1) {
      // FIXME: we might want to sort out overlapping and adjacent links
      if (has_subtree (the_et, p)) {
        tree t= subtree (the_et, p);
        list<string> ids= get_ids (t);
        for (int i=0; i<N(ids); i++) {
          list<tree> lns= get_links (compound ("id", ids[i]));
          for (int j=0; j<N(lns); j++)
            if (is_compound (lns[j], "link", 4) &&
                lns[j][0] == "hyperlink" &&
                is_compound (lns[j][3], "url", 1) &&
                is_atomic (lns[j][3][0])) {
              string dest= lns[j][3][0]->label;
              ren->href (dest, x1, y1, x2, y2);
            }
        }
      }
      p= path_up (p);
    }
  }
}

void
box_rep::position_at (SI x, SI y, rectangles& change_log) {
  int i, n= subnr ();
  x += x0; y += y0;
  for (i=0; i<n; i++) subbox (i)->position_at (x, y, change_log);
}

void
box_rep::collect_page_numbers (hashmap<string,tree>& h, tree page) {
  (void) h; (void) page;
}

void
box_rep::collect_page_colors (array<brush>& bs, array<rectangle>& rs) {
  int i, n= subnr ();
  for (i=0; i<n; i++) {
    array<rectangle> rs2;
    subbox (i)->collect_page_colors (bs, rs2);
    for (int j=0; j<N(rs2); j++)
      rs << translate (rs2[j], sx (i), sy (i));
  }
}

path
box_rep::find_tag (string name) {
  (void) name;
  return path ();
}

bool box::operator == (box b2) { return rep==b2.rep; }
bool box::operator != (box b2) { return rep!=b2.rep; }

box::operator tree () { return tree (*rep); }
tm_ostream& operator << (tm_ostream& out, box b) { return out << ((tree) b); }

path
descend_decode (path ip, int side) {
  if (is_nil (ip)) return descend (ip, side);
  else switch (ip->item) {
  case DECORATION       : return ip->next;
  case DECORATION_LEFT  : return descend (ip->next, 0);
  case DECORATION_MIDDLE: return descend (ip->next, side);
  case DECORATION_RIGHT : return descend (ip->next, 1);
  default               : return descend (ip, side);
  }
}

tree
attach_dip (tree ref, path dip) {
  path old_ip= obtain_ip (ref);
  if (old_ip != path (DETACHED)) return ref;
  if (is_atomic (ref)) {
    tree r (ref->label);
    r->obs= list_observer (ip_observer (dip), r->obs);
    return r;
  }
  else {
    int i, n= N(ref);
    tree r (ref, n);
    for (i=0; i<n; i++)
      r[i]= attach_dip (ref[i], descend (dip, i));
    r->obs= list_observer (ip_observer (dip), r->obs);
    return r;
  }
}

/******************************************************************************
* Convert to postscript or pdf 
******************************************************************************/

void
make_eps (url name, box b, int dpi) {
  //cout << "Make eps " << name << ", " << dpi << ", " << ((tree) b) << "\n";
  //cout << "  Extents " << b->w() / PIXEL << ", " << b->h() / PIXEL << "\n";
  double inch= ((double) dpi * PIXEL);
  double cm  = inch / 2.54;
  SI w= b->x4 - b->x3;
  SI h= b->y4 - b->y3;
  b->x0= -b->x3;
  b->y0= -b->y4;
  renderer ren= printer (name, dpi, 1, "user", false, w/cm, h/cm);
  ren->set_background (white);
  ren->set_pencil (black);
  rectangles rs;
  b->redraw (ren, path (0), rs);
  tm_delete (ren);
}
