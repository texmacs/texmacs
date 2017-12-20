
/******************************************************************************
* MODULE     : change.cpp
* DESCRIPTION: Boxes whose behaviour is changed
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Boxes/change.hpp"
#include "Boxes/construct.hpp"
#include "scheme.hpp"
#include "gui.hpp"
#include "effect.hpp"

/******************************************************************************
* changing the behaviour of a box
******************************************************************************/

change_box_rep::change_box_rep (path ip, bool fl1, bool fl2):
  composite_box_rep (ip), child_flag (fl1), big_flag (fl2) {}

change_box_rep::operator tree () {
  return tree (TUPLE, "change", (tree) bs[0]); }

int
change_box_rep::find_child (SI x, SI y, SI delta, bool force) {
  if (child_flag) return composite_box_rep::find_child (x, y, delta, force);
  return 0;
}

path
change_box_rep::find_left_box_path () {
  return child_flag?
    composite_box_rep::find_left_box_path ():
    path (0, bs[0]->find_left_box_path ());
}

path
change_box_rep::find_right_box_path () {
  return child_flag?
    composite_box_rep::find_right_box_path ():
    path (0, bs[0]->find_right_box_path ());
}

double change_box_rep::left_slope () {
  return big_flag? bs[0]->left_slope(): box_rep::left_slope(); }
double change_box_rep::right_slope () { 
  return big_flag? bs[0]->right_slope(): box_rep::right_slope(); }
SI change_box_rep::left_correction () {
  return big_flag? bs[0]->left_correction():box_rep::left_correction(); }
SI change_box_rep::right_correction () {
  return big_flag? bs[0]->right_correction():box_rep::right_correction(); }
SI change_box_rep::lsub_correction () {
  return big_flag? bs[0]->lsub_correction(): box_rep::lsub_correction(); }
SI change_box_rep::lsup_correction () {
  return big_flag? bs[0]->lsup_correction(): box_rep::lsup_correction(); }
SI change_box_rep::rsub_correction () {
  return big_flag? bs[0]->rsub_correction(): box_rep::rsub_correction(); }
SI change_box_rep::rsup_correction () {
  return big_flag? bs[0]->rsup_correction(): box_rep::rsup_correction(); }
SI change_box_rep::sub_lo_base (int l) {
  return big_flag? bs[0]->sub_lo_base (l): box_rep::sub_lo_base (l); }
SI change_box_rep::sub_hi_lim  (int l) {
  return big_flag? bs[0]->sub_hi_lim (l): box_rep::sub_hi_lim (l); }
SI change_box_rep::sup_lo_lim  (int l) {
  return big_flag? bs[0]->sup_lo_lim (l): box_rep::sup_lo_lim (l); }
SI change_box_rep::sup_lo_base (int l) {
  return big_flag? bs[0]->sup_lo_base (l): box_rep::sup_lo_base (l); }
SI change_box_rep::sup_hi_lim  (int l) {
  return big_flag? bs[0]->sup_hi_lim (l): box_rep::sup_hi_lim (l); }
SI change_box_rep::wide_correction (int mode) {
  return bs[0]->wide_correction (mode); }

void change_box_rep::get_bracket_extents (SI& lo, SI& hi) {
  if (sx(0) == 0 && sy(0) == 0) bs[0]->get_bracket_extents (lo, hi);
  else box_rep::get_bracket_extents (lo, hi); }

SI change_box_rep::get_leaf_offset (string search) {
  return sx1(0) + bs[0]->get_leaf_offset (search); }

gr_selections
change_box_rep::graphical_select (SI x, SI y, SI dist) { 
//TODO : Check if it is correct
  if (child_flag)
    return composite_box_rep::graphical_select (x, y, dist);
  else
    return bs[0]->graphical_select (x- sx(0), y- sy(0), dist);
}

gr_selections
change_box_rep::graphical_select (SI x1, SI y1, SI x2, SI y2) {
//TODO : Check if it is correct
  if (child_flag)
    return composite_box_rep::graphical_select (x1, y1, x2, y2);
  else
    return bs[0]->graphical_select (x1- sx(0), y1- sy(0),
				    x2- sx(0), y2- sy(0));
}

/******************************************************************************
* Moving boxes
******************************************************************************/

struct move_box_rep: public change_box_rep {
  SI dx, dy;
  move_box_rep (path ip, box b, SI x, SI y, bool fl1, bool fl2);
  int get_type () { return MOVE_BOX; }
  box adjust_kerning (int mode, double factor);
  operator tree () { return tree (TUPLE, "move", (tree) bs[0]); }
};

move_box_rep::move_box_rep (path ip, box b, SI x, SI y, bool fl1, bool fl2):
  change_box_rep (ip, fl1, fl2), dx (x), dy (y)
{
  insert (b, x, y);
  position ();
  finalize ();
}

box
move_box_rep::adjust_kerning (int mode, double factor) {
  box body= bs[0]->adjust_kerning (mode, factor);
  return move_box (ip, body, dx, dy, child_flag, big_flag);
}

struct shift_box_rep: public change_box_rep {
  SI dx, dy;
  shift_box_rep (path ip, box b, SI x, SI y, bool fl1, bool fl2);
  int get_type () { return MOVE_BOX; }
  box adjust_kerning (int mode, double factor);
  operator tree () { return tree (TUPLE, "shift", (tree) bs[0]); }
  void get_bracket_extents (SI& lo, SI& hi);
};

shift_box_rep::shift_box_rep (path ip, box b, SI x, SI y, bool fl1, bool fl2):
  change_box_rep (ip, fl1, fl2), dx (x), dy (y)
{
  insert (b, x, y);
  position ();
  x1 -= x; y1 -= y;
  x2 -= x; y2 -= y;
  finalize ();
}

box
shift_box_rep::adjust_kerning (int mode, double factor) {
  box body= bs[0]->adjust_kerning (mode, factor);
  return move_box (ip, body, dx, dy, child_flag, big_flag);
}

void
shift_box_rep::get_bracket_extents (SI& lo, SI& hi) {
  if (sx(0) == 0 && sy(0) == 0) {
    bs[0]->get_bracket_extents (lo, hi);
    lo += dy;
    hi += dy;
  }
  else box_rep::get_bracket_extents (lo, hi);
}

/******************************************************************************
* Resizing boxes
******************************************************************************/

struct resize_box_rep: public change_box_rep {
  resize_box_rep (path ip, box b, SI x1, SI y1, SI x2, SI y2,
		  bool child_flag, bool adjust);
  operator tree () { return tree (TUPLE, "resize", (tree) bs[0]); }
  void get_bracket_extents (SI& lo, SI& hi) { lo= y1; hi= y2; }
  gr_selections graphical_select (SI x, SI y, SI dist);
};

resize_box_rep::resize_box_rep (
  path ip, box b, SI X1, SI Y1, SI X2, SI Y2, bool child_flag, bool adjust):
    change_box_rep (ip, child_flag)
{
  insert (b, 0, 0);
  position ();
  x1= X1; y1= Y1;
  x2= X2; y2= Y2;
  if (adjust) left_justify ();
  finalize ();
}

gr_selections
resize_box_rep::graphical_select (SI x, SI y, SI dist) {
  gr_selections res;
  int i, n= subnr();
  for (i=n-1; i>=0; i--)
    res << bs[i]->graphical_select (x- sx(i), y- sy(i), dist);
  return res;
}

struct vresize_box_rep: public change_box_rep {
  vresize_box_rep (path ip, box b, SI y1, SI y2);
  operator tree () { return tree (TUPLE, "vresize", (tree) bs[0]); }
  box adjust_kerning (int mode, double factor);
  void get_bracket_extents (SI& lo, SI& hi) { lo= y1; hi= y2; }
};

vresize_box_rep::vresize_box_rep (path ip, box b, SI Y1, SI Y2):
  change_box_rep (ip, false)
{
  insert (b, 0, 0);
  position ();
  x1= b->x1; y1= Y1;
  x2= b->x2; y2= Y2;
  finalize ();
}

box
vresize_box_rep::adjust_kerning (int mode, double factor) {
  box body= bs[0]->adjust_kerning (mode, factor);
  return vresize_box (ip, body, y1, y2);
}

struct vcorrect_box_rep: public change_box_rep {
  SI top_cor, bot_cor;
  vcorrect_box_rep (path ip, box b, SI top_cor, SI bot_cor);
  operator tree () { return tree (TUPLE, "vcorrect", (tree) bs[0]); }
  box adjust_kerning (int mode, double factor);
  void get_bracket_extents (SI& lo, SI& hi) { lo= y1; hi= y2; }
};

vcorrect_box_rep::vcorrect_box_rep (path ip, box b, SI top_cor2, SI bot_cor2):
  change_box_rep (ip, false, false), top_cor (top_cor2), bot_cor (bot_cor2)
{
  insert (b, 0, -top_cor);
  position ();
  y1 -= bot_cor;
  y2 += top_cor;
  finalize ();
}

box
vcorrect_box_rep::adjust_kerning (int mode, double factor) {
  box body= bs[0]->adjust_kerning (mode, factor);
  return vcorrect_box (ip, body, top_cor, bot_cor);
}

/******************************************************************************
* Transformed boxes
******************************************************************************/

// FIXME: several things are not yet OK
// 1) The clip region is not tight anymore under transformations,
// so it should only be considered as a hint for rejecting redraw operations
// for which we know for sure that no work has to be done.
// The actual clipping for the painting itself should be ensured
// using a different mechanism.
// 2) Cursor movement and selections inside transformed text are
// not implemented.  We might determine the "underlying transformation"
// of a cursor position or selection and draw the cursor/selection
// using that transformation.

struct transformed_box_rep: public change_box_rep {
  frame fr;
public:
  transformed_box_rep (path ip, box b, frame fr);
  operator tree () { return tree (TUPLE, "transform", (tree) bs[0]); }
  void pre_display (renderer &ren);
  void post_display (renderer &ren);
  //cursor find_cursor (path bp);
  //selection find_selection (path lbp, path rbp);
  void get_bracket_extents (SI& lo, SI& hi) { lo= y1; hi= y2; }
};

transformed_box_rep::transformed_box_rep (path ip, box b, frame fr2):
  change_box_rep (ip, true), fr (fr2)
{
  insert (b, 0, 0);
  position ();
  finalize ();
  rectangle r1= fr (rectangle (x1, y1, x2, y2));
  rectangle r2= fr (rectangle (x3, y3, x4, y4));
  x1= r1->x1; y1= r1->y1;
  x2= r1->x2; y2= r1->y2;
  x3= r2->x1; y3= r2->y1;
  x4= r2->x2; y4= r2->y2;
}

void
transformed_box_rep::pre_display (renderer &ren) {
  ren->set_transformation (fr);
}

void
transformed_box_rep::post_display (renderer &ren) {
  ren->reset_transformation ();
}

/******************************************************************************
* Effects applied on boxes
******************************************************************************/

struct effect_box_rep: public change_box_rep {
  tree   eff_t;
  effect eff;
public:
  effect_box_rep (path ip, array<box> bs, tree eff);
  operator tree () { return tree (TUPLE, "effect", eff_t); }
  box adjust_kerning (int mode, double factor);
  void redraw (renderer ren, path p, rectangles& l);
  void get_bracket_extents (SI& lo, SI& hi) { lo= y1; hi= y2; }
};

effect_box_rep::effect_box_rep (path ip, array<box> bs, tree eff2):
  change_box_rep (ip, true), eff_t (eff2), eff (build_effect (eff2))
{
  for (int i=0; i<N(bs); i++)
    insert (bs[i], 0, 0);
  array<rectangle> rs (N(bs));
  for (int i=0; i<N(bs); i++)
    rs[i]= rectangle (bs[i]->x1, bs[i]->y1, bs[i]->x2, bs[i]->y2);
  rectangle r= eff->get_logical_extents (rs);
  x1= r->x1;
  y1= r->y1;
  x2= r->x2;
  y2= r->y2;
  rs= array<rectangle> (N(bs));
  for (int i=0; i<N(bs); i++)
    rs[i]= rectangle (bs[i]->x3, bs[i]->y3, bs[i]->x4, bs[i]->y4);
  r = eff->get_extents (rs);
  x3= r->x1;
  y3= r->y1;
  x4= r->x2;
  y4= r->y2;
  finalize ();
}

box
effect_box_rep::adjust_kerning (int mode, double factor) {
  int n= N(bs);
  array<box> adj (n);
  for (int i=0; i<n; i++)
    adj[i]= bs[i]->adjust_kerning (mode, factor);
  return effect_box (ip, adj, eff_t);
}

extern int nr_painted;

void
effect_box_rep::redraw (renderer ren, path p, rectangles& l) {
  (void) p; (void) l;
  if (((nr_painted&15) == 15) && gui_interrupted (true)) return;
  ren->move_origin (x0, y0);
  array<picture> pics (subnr ());
  SI shad_pixel= ren->pixel;
  for (int i=0; i<subnr(); i++) {
    renderer shad= ren->shadow (pics[i], sx3(i), sy3(i), sx4(i), sy4(i));
    shad_pixel= shad->pixel;
    rectangles rs;
    subbox (i)->redraw (shad, path (), rs);
    delete_renderer (shad);
  }
  if (((nr_painted&15) == 15) && gui_interrupted (true));
  else {
    picture result_pic= eff->apply (pics, shad_pixel);
    ren->draw_picture (result_pic, 0, 0);
  }
  ren->move_origin (-x0, -y0);
}

/******************************************************************************
* Clipped boxes
******************************************************************************/

struct clip_box_rep: public change_box_rep {
  tree xt, yt;
  SI old_clip_x1, old_clip_x2, old_clip_y1, old_clip_y2;
public:
  clip_box_rep (path ip, box b, SI x1, SI y1, SI x2, SI y2,
		tree xt, tree yt, SI scx, SI scy);
  operator tree () { return tree (TUPLE, "clip", (tree) bs[0]); }
  int get_type () {
    return xt!=UNINIT || yt!=UNINIT? SCROLL_BOX: change_box_rep::get_type(); }
  tree get_info (tree in) {
    if (in == "scroll-x") return xt;
    else if (in == "scroll-y") return yt;
    else return box_rep::get_info (in); }
  void pre_display (renderer &ren);
  void post_display (renderer &ren);
  selection find_selection (path lbp, path rbp);
  void get_bracket_extents (SI& lo, SI& hi) { lo= y1; hi= y2; }
};

clip_box_rep::clip_box_rep (
  path ip, box b, SI X1, SI Y1, SI X2, SI Y2,
  tree xt2, tree yt2, SI scx, SI scy):
  change_box_rep (ip, true), xt (xt2), yt (yt2)
{
  insert (b, scx, scy);
  position ();
  x1= X1; y1= Y1;
  x2= X2; y2= Y2;
  // left_justify ();
  finalize ();
}

void
clip_box_rep::pre_display (renderer &ren) {
  ren->get_clipping (old_clip_x1, old_clip_y1, old_clip_x2, old_clip_y2);
  ren->extra_clipping (x1, y1, x2, y2);
}

void
clip_box_rep::post_display (renderer &ren) {
  ren->set_clipping (
    old_clip_x1, old_clip_y1, old_clip_x2, old_clip_y2, true);
}

selection
clip_box_rep::find_selection (path lbp, path rbp) {
  selection sel= change_box_rep::find_selection (lbp, rbp);
  return selection (sel->rs & rectangle (x1, y1, x2, y2),
		    sel->start, sel->end, sel->valid);
}

/******************************************************************************
* horizontally repetition of one box inside another one
******************************************************************************/

struct repeat_box_rep: public change_box_rep {
  box repeat;
  SI xoff;
  repeat_box_rep (path ip, box b, box repeat, SI xoff);
  operator tree () { return tree (TUPLE, "repeat", (tree) bs[0]); }
  box adjust_kerning (int mode, double factor);
};

repeat_box_rep::repeat_box_rep (path ip, box b, box repeat2, SI xoff2):
  change_box_rep (ip, false), repeat (repeat2), xoff (xoff2)
{
  insert (b, 0, 0);
  position ();

  SI width= repeat->w ();
  if (width >= PIXEL) {
    int i;
    int i1= ((xoff+b->x1)/width)-1;
    int i2= ((xoff+b->x2)/width)+1;
    while (i1*width < (xoff+b->x1)) i1++;
    while (i2*width > (xoff+b->x2)) i2--;
    for (i=i1; i<i2; i++) {
      box bb= move_box (decorate_right (ip), repeat, 0, 0);
      insert (bb, i*width-xoff, 0);
    }
  }

  finalize ();
  x1= b->x1; y1= b->y1;
  x2= b->x2; y2= b->y2;
}

box
repeat_box_rep::adjust_kerning (int mode, double factor) {
  box body= bs[0]->adjust_kerning (mode, factor);
  return repeat_box (ip, body, repeat, xoff);
}

/******************************************************************************
* cell boxes for tables
******************************************************************************/

struct cell_box_rep: public change_box_rep {
  SI    X0, Y0;
  SI    bl, br, bb, bt;
  brush fg, bg, old_bg;
  cell_box_rep (path ip, box b, SI x0, SI y0, SI x1, SI y1, SI x2, SI y2,
		SI bl, SI br, SI bb, SI bt, brush fg, brush bg);
  operator tree () { return tree (TUPLE, "cell", (tree) bs[0]); }
  box  adjust_kerning (int mode, double factor);
  void get_cell_extents (SI& l, SI& r);
  box  adjust_cell_geometry (SI dx, SI dl, SI dr);
  void pre_display (renderer &ren);
  void post_display (renderer &ren);
};

cell_box_rep::cell_box_rep (
  path ip, box b, SI X0b, SI Y0b, SI X1, SI Y1, SI X2, SI Y2,
  SI Bl, SI Br, SI Bb, SI Bt, brush Fg, brush Bg):
  change_box_rep (ip, false),
  X0 (X0b), Y0 (Y0b),
  bl (Bl<<1), br (Br<<1), bb (Bb<<1), bt (Bt<<1),
  fg (Fg), bg (Bg)
{
  insert (b, X0, Y0);
  position ();
  x1= X1; y1= Y1;
  x2= X2; y2= Y2;
  if (bg->get_type () != brush_none || (bl>0) || (br>0) || (bb>0) || (bt>0)) {
    // the 4*PIXEL extra space is sufficient for (shrinking factor) <= 8
    x3= min (x3, x1 - (bl>>1) - (bl>0? PIXEL<<2: 0));
    y3= min (y3, y1 - (bb>>1) - (bb>0? PIXEL<<2: 0));
    x4= max (x4, x2 + (br>>1) + (br>0? PIXEL<<2: 0));
    y4= max (y4, y2 + (bt>>1) + (bt>0? PIXEL<<2: 0));
  }
  finalize ();
}

box
cell_box_rep::adjust_kerning (int mode, double factor) {
  box nb= bs[0]->adjust_kerning (mode & (~TABLE_CELL), factor);
  SI  d = nb->w() - bs[0]->w();
  if ((mode & TABLE_CELL) != 0) d= 0;
  return cell_box (ip, nb, X0, Y0, x1, y1, x2 + d, y2,
                   bl>>1, br>>1, bb>>1, bt>>1, fg, bg);
}

void
cell_box_rep::get_cell_extents (SI& l, SI& r) {
  l= sx1 (0);
  r= sx2 (0);
}

box
cell_box_rep::adjust_cell_geometry (SI dx, SI dl, SI dr) {
  return cell_box (ip, bs[0], X0+dx, Y0, x1+dl, y1, x2+dr, y2,
                   bl>>1, br>>1, bb>>1, bt>>1, fg, bg);
}

void
cell_box_rep::pre_display (renderer& ren) {
  SI l= bl, r= br, b= bb, t= bt;
  SI lx1, rx1, by1, ty1;
  SI lx2, rx2, by2, ty2;
  if (ren->is_screen) { // correction for screen display only
    SI  pixel= ren->pixel;
    l= ((l + (pixel - 1)) / pixel) * pixel;
    r= ((r + (pixel - 1)) / pixel) * pixel;
    b= ((b + (pixel - 1)) / pixel) * pixel;
    t= ((t + (pixel - 1)) / pixel) * pixel;
  }

  lx1= x1 - (l>>1); lx2= lx1 + l;
  by1= y1 - (b>>1); by2= by1 + b;
  rx2= x2 + (r>>1); rx1= rx2 - r;
  ty2= y2 + (t>>1); ty1= ty2 - t;

  if (bg->get_type () != brush_none) {
    old_bg= ren->get_background ();
    ren->set_background (bg);
    ren->clear_pattern (lx2, by2, rx1, ty1);
  }

  if ((l>0) || (r>0) || (b>0) || (t>0)) {
    ren->set_pencil (fg);
    ren->fill (lx1, by1, lx2, ty2);
    ren->fill (rx1, by1, rx2, ty2);
    ren->fill (lx1, by1, rx2, by2);
    ren->fill (lx1, ty1, rx2, ty2);
  }
}

void
cell_box_rep::post_display (renderer& ren) {
  if (bg->get_type () != brush_none)
    ren->set_background (old_bg);
}

/******************************************************************************
* Remember boxes
******************************************************************************/

class remember_box_rep: public change_box_rep {
public:
  rectangles* logs_ptr;
  SI          ox, oy;
public:
  inline remember_box_rep (path ip, box b):
    change_box_rep (ip, true), logs_ptr (NULL)
  {
    insert (b, 0, 0);
    position ();
    finalize ();
  }
  inline ~remember_box_rep () {
    if (logs_ptr != NULL) {
      rectangles& logs= *logs_ptr;
      logs= rectangles (rectangle (ox+x3, oy+y3, ox+x4, oy+y4), logs);
      logs= rectangles (rectangle (0, 0, 0, 0), logs);
      // cout << "  8=X " << rectangle (ox+x3, oy+y3, ox+x4, oy+y4) << "\n";
    }
  }
  inline void position_at (SI x, SI y, rectangles& logs) {
    x += x0; y += y0;
    if (logs_ptr == NULL) logs= rectangles (rectangle (0, 0, 0, 0), logs);
    else logs= rectangles (rectangle (ox+x3, oy+y3, ox+x4, oy+y4), logs);
    ox= x; oy= y;
    logs= rectangles (rectangle (ox+x3, oy+y3, ox+x4, oy+y4), logs);
    logs_ptr= &logs;
  }
  inline void display (renderer ren) {
    ren->apply_shadow (x1, y1, x2, y2);
  }
};

/******************************************************************************
* locus boxes
******************************************************************************/

struct locus_box_rep: public change_box_rep {
  list<string> ids;
  SI pixel;
  string ref;
  string anchor;
  locus_box_rep (path ip, box b, list<string> ids, SI pixel);
  locus_box_rep (path ip, box b, list<string> ids, SI pixel, string _rep, string _anchor);
  operator tree () { return tree (TUPLE, "locus"); }
  box adjust_kerning (int mode, double factor);
  void loci (SI x, SI y, SI delta, list<string>& ids2, rectangles& rs);
  void post_display (renderer &ren);
};

locus_box_rep::locus_box_rep (path ip, box b, list<string> ids2, SI pixel2):
  change_box_rep (ip, true), ids (ids2), pixel (pixel2)
{
  ref = "";
  anchor = "";
  insert (b, 0, 0);
  position ();
  left_justify ();
  finalize ();
}

locus_box_rep::locus_box_rep (path ip, box b, list<string> ids2, SI pixel2, string _ref, string _anchor):
  change_box_rep (ip, true), ids (ids2), pixel (pixel2)
{
  ref = _ref;
  anchor = _anchor;
  insert (b, 0, 0);
  position ();
  left_justify ();
  finalize ();
}

box
locus_box_rep::adjust_kerning (int mode, double factor) {
  box body= bs[0]->adjust_kerning (mode, factor);
  return locus_box (ip, body, ids, pixel, ref, anchor);
}

void
locus_box_rep::loci (SI x, SI y, SI delta, list<string>& l, rectangles& rs) {
  bs[0]->loci (x, y, delta, l, rs);
  l = l * ids;
  rs= rs * outline (rectangles (rectangle (x1, y1, x2, y2)), pixel);
}

void
locus_box_rep::post_display (renderer &ren) {
  if (ref != "") ren->href (ref, x1, y1, x2, y2);
  if (anchor != "") ren->anchor (anchor, x1, y1, x2, y2);
}

/******************************************************************************
* tag boxes
******************************************************************************/

struct tag_box_rep: public change_box_rep {
  tree keys;
  path tip;
  tag_box_rep (path ip, path tip, box b, tree keys);
  operator tree () { return tree (TUPLE, "tag", bs[0]); }
  box adjust_kerning (int mode, double factor);
  tree tag (tree t, SI x, SI y, SI delta);
  void collect_page_numbers (hashmap<string,tree>& h, tree page);
  path find_tag (string name);
};

tag_box_rep::tag_box_rep (path ip, path tip2, box b, tree keys2):
  change_box_rep (ip, false), keys (keys2), tip (tip2)
{
  insert (b, 0, 0);
  position ();
  left_justify ();
  finalize ();
}

box
tag_box_rep::adjust_kerning (int mode, double factor) {
  box body= bs[0]->adjust_kerning (mode, factor);
  return tag_box (ip, tip, body, keys);
}

void
tag_box_rep::collect_page_numbers (hashmap<string,tree>& h, tree page) {
  for (int i=0; i<N(keys); i++)
    h (keys[i]->label)= page;
  bs[0]->collect_page_numbers (h, page);
}

path
tag_box_rep::find_tag (string search) {
  for (int i=0; i<N(keys); i++)
    if (keys[i]->label == search)
      return reverse (descend_decode (tip, 1));
  return path ();
}

/******************************************************************************
* note boxes
******************************************************************************/

struct note_box_rep: public change_box_rep {
  note_box_rep (path ip, box b, box note, SI nx, SI ny);
  operator tree () { return tree (TUPLE, "note", bs[0], bs[1]); }
};

note_box_rep::note_box_rep (path ip, box b, box note, SI nx, SI ny):
  change_box_rep (ip, false)
{
  insert (b, 0, 0);
  insert (note, nx, ny);
  position ();
  finalize ();
  x1= b->x1;
  y1= b->y1;
  x2= b->x2;
  y2= b->y2;
}

/******************************************************************************
* text_at boxes
******************************************************************************/

struct text_at_box_rep: public move_box_rep {
  SI axis;
  SI pad;
  text_at_box_rep (path ip, box b, SI x, SI y, SI axis, SI pad);
  gr_selections graphical_select (SI x, SI y, SI dist);
  operator tree () { return tree (TUPLE, "text-at", (tree) bs[0]); }
  /*
  void pre_display (renderer &ren) {
    array<SI> xs, ys;
    xs << x1 - pad << x2 + pad << x2 + pad << x1 - pad << x1 - pad;
    ys << y1 - pad << y1 - pad << y2 + pad << y2 + pad << y1 - pad;
    ren->set_pencil (pencil (rgb_color (255, 255, 224), ren->pixel));
    ren->polygon (xs, ys);
  }
  */
};

text_at_box_rep::text_at_box_rep (path ip, box b, SI x, SI y, SI a2, SI p2):
  move_box_rep (ip, b, x, y, false, false), axis (a2), pad (p2) {}

gr_selections
text_at_box_rep::graphical_select (SI x, SI y, SI dist) {
  array<point> special;
  special << point (x1 - pad, y1 - pad)
	  << point (x1 - pad, y2 + pad)
	  << point (x2 + pad, y2 + pad)
	  << point (x2 + pad, y1 - pad)
	  << point ((x1 + x2) >> 1, y1 - pad)
	  << point ((x1 + x2) >> 1, y2 + pad)
	  << point (x1 - pad, y1 + axis)
	  << point (x2 + pad, y1 + axis);
  array<point> ps;
  ps << point (x1 - pad, y1 - pad) << point (x2 + pad, y1 - pad)
     << point (x2 + pad, y2 + pad) << point (x1 - pad, y2 + pad)
     << point (x1 - pad, y1 - pad);
  array<curve> cs;
  for (int i=0; i<N(ps)-1; i++)
    cs << segment (ps[i], ps[i+1]);

  gr_selections res;
  point p= point (x, y);
  if (norm (p - point (sx(0), sy(0))) <= dist) {
    gr_selection gs;
    gs->type= "text-handle";
    gs->dist= (SI) norm (p - point (sx(0), sy(0)));
    gs->p= point (sx(0), sy(0));
    gs->cp << reverse (path (0, path (1, ip)));
    gs->pts << gs->p;
    gs->c= curve ();
    res << gs;
  }
  else if (graphical_distance (x, y) == 0) {
    gr_selection gs;
    gs->type= "text";
    gs->dist= graphical_distance (x, y);
    gs->p= p;
    gs->cp << box_rep::find_tree_path (x, y, dist);
    gs->pts << gs->p;
    gs->c= curve ();
    res << gs;
  }

  for (int i=0; i<N(special); i++)
    if (norm (special[i] - p) <= dist) {
      gr_selection gs;
      gs->type= "text-border-point";
      gs->p= special[i];
      gs->dist= (SI) norm (gs->p - p);
      gs->cp << box_rep::find_tree_path (x, y, dist);
      gs->pts << gs->p;
      gs->c= curve ();
      res << gs;
    }
  for (int i=0; i<N(cs); i++) {
    if (N(res) == 0 && norm (closest (cs[i], p) - p) <= dist) {
      gr_selection gs;
      gs->type= "text-border";
      gs->p= closest (cs[i], p);
      gs->dist= (SI) norm (gs->p - p);
      gs->cp << box_rep::find_tree_path (x, y, dist);
      gs->pts << gs->p;
      gs->c= cs[i];
      res << gs;    
    }
  }
  return res;
}

/******************************************************************************
* box construction routines
******************************************************************************/

box
move_box (path ip, box b, SI x, SI y, bool child_flag, bool big_flag) {
  return tm_new<move_box_rep> (ip, b, x, y, child_flag, big_flag);
}

box
shift_box (path ip, box b, SI x, SI y, bool child_flag, bool big_flag) {
  return tm_new<shift_box_rep> (ip, b, x, y, child_flag, big_flag);
}

box
resize_box (path ip, box b, SI x1, SI y1, SI x2, SI y2,
	    bool child_flag, bool adjust) {
  return tm_new<resize_box_rep> (ip, b, x1, y1, x2, y2, child_flag, adjust);
}

box
vresize_box (path ip, box b, SI y1, SI y2) {
  return tm_new<vresize_box_rep> (ip, b, y1, y2);
}

box
transformed_box (path ip, box b, frame fr) {
  return tm_new<transformed_box_rep> (ip, b, fr);
}

box
effect_box (path ip, array<box> bs, tree eff) {
  return tm_new<effect_box_rep> (ip, bs, eff);
}

box
clip_box (path ip, box b, SI x1, SI y1, SI x2, SI y2) {
  return tm_new<clip_box_rep> (ip, b, x1, y1, x2, y2, UNINIT, UNINIT, 0, 0);
}

box
clip_box (path ip, box b, SI x1, SI y1, SI x2, SI y2,
	  tree xt, tree yt, SI scx, SI scy) {
  return tm_new<clip_box_rep> (ip, b, x1, y1, x2, y2, xt, yt, scx, scy);
}

box
vcorrect_box (path ip, box b, SI top_cor, SI bot_cor) {
  return tm_new<vcorrect_box_rep> (ip, b, top_cor, bot_cor);
}

box
repeat_box (path ip, box ref, box repeat, SI xoff) {
  return tm_new<repeat_box_rep> (ip, ref, repeat, xoff);
}

box
cell_box (path ip, box b, SI x0, SI y0, SI x1, SI y1, SI x2, SI y2,
	  SI bl, SI br, SI bb, SI bt, brush fg, brush bg)
{
  box cb= tm_new<cell_box_rep> (ip, b, x0, y0, x1, y1, x2, y2,
                                bl, br, bb, bt, fg, bg);
  return cb;
}

box
remember_box (path ip, box b) {
  return tm_new<remember_box_rep> (ip, b);
}

box
locus_box (path ip, box b, list<string> ids, SI pixel) {
  return tm_new<locus_box_rep> (ip, b, ids, pixel);
}

box
locus_box (path ip, box b, list<string> ids, SI pixel, string ref, string anchor) {
  return tm_new<locus_box_rep> (ip, b, ids, pixel, ref, anchor);
}

box
tag_box (path ip, path tip, box b, tree keys) {
  return tm_new<tag_box_rep> (ip, tip, b, keys);
}

box
note_box (path ip, box b, box note, SI nx, SI ny) {
  return tm_new<note_box_rep> (ip, b, note, nx, ny);
}

box
text_at_box (path ip, box b, SI x, SI y, SI axis, SI pad) {
  return tm_new<text_at_box_rep> (ip, b, x, y, axis, pad);
}
