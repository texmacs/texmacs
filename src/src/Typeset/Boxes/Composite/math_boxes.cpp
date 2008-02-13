
/******************************************************************************
* MODULE     : math.cpp
* DESCRIPTION: Boxes for mathematical typesetting:
*              fractions, roots, negations, limits and scripts
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Boxes/composite.hpp"
#include "Boxes/construct.hpp"
#include "Boxes/Composite/italic_correct.hpp"

/******************************************************************************
* Miscellaneous routines
******************************************************************************/

inline void left_italic_correct (box b) { b->x1 -= b->left_correction (); }
inline void left_italic_restore (box b) { b->x1 += b->left_correction (); }
inline void right_italic_correct (box b) { b->x2 += b->right_correction (); }
inline void right_italic_restore (box b) { b->x2 -= b->right_correction (); }

SI
italic_correction (box L, box R) {
  double slope_L= L->right_slope ();
  double slope_R= R->left_slope ();
  if (slope_L == slope_R) return 0;
  if (slope_L * slope_R == 0.0)
    return L->right_correction () + R->left_correction ();
  double M= (slope_L <= slope_R? slope_R: slope_L);
  if (M<0) M= (slope_L >= slope_R? slope_R: slope_L);
  double factor= (slope_R- slope_L) / M;
  if (factor < 0) factor= -factor;
  return (SI) (factor * (L->right_correction () + R->left_correction ()));
}

/******************************************************************************
* Fractions
******************************************************************************/

struct frac_box_rep: public composite_box_rep {
  frac_box_rep (path ip, box b1, box b2, font fn, font sfn, color c);
  operator tree () { return tree (TUPLE, "frac", bs[0], bs[1]); }
  int find_child (SI x, SI y, SI delta, bool force);
};

frac_box_rep::frac_box_rep (
  path ip, box b1, box b2, font fn, font sfn, color c):
    composite_box_rep (ip)
{
  // Italic correction does not lead to nicer results,
  // because right correction is not equilibrated w.r.t. left correction

  SI bar_y = fn->yfrac;
  SI bar_w = fn->wline;
  SI sep   = fn->sep;
  SI b1_y  = min (b1->y1, sfn->y1);
  SI b2_y  = max (b2->y2, sfn->y2);
  SI w     = max (b1->w (), b2->w()) + 2*sep;
  SI d     = sep >> 1;

  insert (b1, (w>>1) - (b1->x2>>1), bar_y+ sep+ (bar_w>>1)- b1_y);
  insert (b2, (w>>1) - (b2->x2>>1), bar_y- sep- (bar_w>>1)- b2_y);
  insert (line_box (decorate_middle (ip), d, 0, w-d, 0, bar_w, c), 0, bar_y);

  italic_correct (b1);
  italic_correct (b2);
  position ();
  italic_restore (b1);
  italic_restore (b2);
  x1= min (0, x1);
  x2= max (w, x2);
  left_justify ();
  finalize ();
}

int
frac_box_rep::find_child (SI x, SI y, SI delta, bool force) {
  if (outside (x, delta, x1, x2) && (is_accessible (ip) || force)) return -1;
  int i= (y >= sy(2))? 0: 1;
  if (bs[i]->decoration () && (!force)) {
    i= 1-i;
    if (bs[i]->decoration () && (!force)) return -1;
  }
  return i;
}

/******************************************************************************
* Square roots
******************************************************************************/

struct sqrt_box_rep: public composite_box_rep {
  sqrt_box_rep (path ip, box b1, box b2, box sqrtb, font fn, color c);
  operator tree () { return tree (TUPLE, "sqrt", bs[0]); }
  int find_child (SI x, SI y, SI delta, bool force);
};

sqrt_box_rep::sqrt_box_rep (
  path ip, box b1, box b2, box sqrtb, font fn, color c):
    composite_box_rep (ip)
{
  right_italic_correct (b1);

  SI sep  = fn->sep;
  SI wline= fn->wline;
  SI dx   = -fn->wfn/36, dy= -fn->wfn/36; // correction
  SI by   = sqrtb->y2+ dy;

  insert (b1, 0, 0);
  if (!is_nil (b2)) {
    SI X = - sqrtb->w();
    SI M = X / 3;
    SI Y = sqrtb->y1;
    SI bw= sqrtb->w();
    SI bh= sqrtb->h();
    if (bh < 3*bw) Y += bh >> 1;
    else Y += (bw*3) >> 1;
    insert (b2, min (X, M- b2->x2), Y- b2->y1+ sep);
  }
  insert (sqrtb, -sqrtb->x2, 0);
  insert (line_box (decorate_middle (ip), dx, by, b1->x2, by, wline, c), 0, 0);
  
  position ();
  left_justify ();
  y1 -= wline;
  y2 += wline;
  x2 += sep >> 1;

  right_italic_restore (b1);
  finalize ();
}

int
sqrt_box_rep::find_child (SI x, SI y, SI delta, bool force) {
  (void) y;
  if (outside (x, delta, x1, x2) && (is_accessible (ip) || force)) return -1;
  int i= ((N(bs)==3) || (x>sx(0)) || ((x>=sx(0)) && (delta>=0)))? 0: 1;
  if (bs[i]->decoration () && (!force)) {
    i= 1-i;
    if (bs[i]->decoration () && (!force)) return -1;
  }
  return i;
}

/******************************************************************************
* Negation boxes
******************************************************************************/

struct neg_box_rep: public composite_box_rep {
  neg_box_rep (path ip, box b1, font fn, color c);
  operator tree () { return tree (TUPLE, "neg", bs[0]); }
  int find_child (SI x, SI y, SI delta, bool force);
};

neg_box_rep::neg_box_rep (path ip, box b, font fn, color c):
  composite_box_rep (ip)
{
  SI wline= fn->wline;
  SI delta= fn->wfn/6;
  SI X    = (b->x1 + b->x2) >> 1;
  SI Y    = (b->y1 + b->y2) >> 1;
  SI DX, DY;

  insert (b, 0, 0);
  if ((3*(b->x2-b->x1)) > (2*(b->y2-b->y1))) {
    DY= delta + ((b->y2 - b->y1)>>1);
    DX= DY>>1;
  }
  else {
    DX= delta + ((b->x2 - b->x1)>>1);
    DY= DX;
  }
  insert (line_box (decorate_middle (ip),
		    X+DX, Y+DY, X-DX, Y-DY, wline, c), 0, 0);
  
  italic_correct (b);
  position ();
  italic_restore (b);
  finalize ();
}

int
neg_box_rep::find_child (SI x, SI y, SI delta, bool force) {
  (void) y;
  if (outside (x, delta, x1, x2) && (is_accessible (ip) || force)) return -1;
  if (bs[0]->decoration () && (!force)) return -1;
  return 0;
}

/******************************************************************************
* Tree boxes
******************************************************************************/

struct tree_box_rep: public composite_box_rep {
  SI  border;
  tree_box_rep (path ip, array<box> bs, font fn, color line_c);
  operator tree () { return "tree box"; }
  int find_child (SI x, SI y, SI delta, bool force);
};

tree_box_rep::tree_box_rep (path ip, array<box> bs, font fn, color line_c):
  composite_box_rep (ip)
{
  SI sep   = fn->sep;
  SI hsep  = 2*fn->spc->def;
  SI vsep  = 4*fn->spc->def;
  SI line_w= fn->wline;

  int i, n= N(bs), cw, w= 0, h= MIN_SI, x, x_0, up;
  for (i=1; i<n; i++) w += bs[i]->w();
  for (i=1; i<n; i++) h  = max (h, max (bs[i]->y2, fn->y2) + sep);
  w += (n-2)*hsep;
  cw = w;
  x_0= 0; if (bs[0]->w()>w) { x_0= (bs[0]->w()-w)>>1; w= bs[0]->w(); }
  up= min (bs[0]->y1, fn->y1) - sep - vsep;

  insert (bs[0], (w>>1)- ((bs[0]->x1+bs[0]->x2)>>1), 0);
  for (x=x_0, i=1; i<n; i++) {
    SI x_i= x- bs[i]->x1;
    SI y_i= up- h;
    insert (bs[i], x_i, y_i);
    x += bs[i]->w()+ hsep;
  }

  for (x=x_0, i=1; i<n; i++) {
    SI x_i= x + (bs[i]->w()>>1);
    SI y_i= up + max (bs[i]->y2, fn->y2) + sep - h;
    SI bm = w>>1;
    SI bw = min (bs[0]->w(), cw>>1);
    SI bx = bm + ((2*i-n) * bw) / (2*n-2);
    SI by = min (bs[0]->y1, fn->y1) - sep;
    insert (line_box (decorate_middle (ip),
		      bx, by, x_i, y_i, line_w, line_c), 0, 0);
    x += bs[i]->w()+ hsep;
  }

  position ();
  border= up+ (vsep>>1);
  finalize ();
}

int
tree_box_rep::find_child (SI x, SI y, SI delta, bool force) {
  if (outside (x, delta, x1, x2) && (is_accessible (ip) || force)) return -1;
  int i=0;
  if (y < border) {
    int j, d=MAX_SI;
    for (j=1; j<((N(bs)+1)>>1); j++)
      if (distance (j, x, y, delta)< d)
	if (bs[j]->accessible () || force) {
	  d= distance (j, x, y, delta);
	  i= j;
	}
  }
  if (bs[i]->decoration () && (!force)) return -1;
  return i;
}

/******************************************************************************
* wide hats, tildas, etc...
******************************************************************************/

struct wide_box_rep: public composite_box_rep {
  box ref;
  SI  dw, dh, dd;
  bool above;
  wide_box_rep (path ip, box ref, box hi, font fn, SI sep, bool above);
  operator tree () { return tree (TUPLE, "wide", bs[0]); }
  int find_child (SI x, SI y, SI delta, bool force);
  double left_slope ();
  double right_slope ();

  SI left_correction () {
    return ref->left_correction (); }
  SI right_correction () {
    if (above) {
      SI rc= ref->right_correction () + dw;
      if (sx4 (1) >= (sx2 (1) - (dd>>1))) // corrects buggy extents wide chars
	rc= max (rc, sx2(1)- x2+ dd);
      return rc; }
    return ref->right_correction (); }
  SI lsub_correction () {
    return ref->lsub_correction (); }
  SI lsup_correction () {
    return ref->lsup_correction (); }
  SI rsub_correction () {
    return ref->rsub_correction (); }
  SI rsup_correction () {
    if (above) {
      SI rc= ref->rsup_correction () + dw;
      if (sx4 (1) >= (sx2 (1) - (dd>>1))) // corrects buggy extents wide chars
	rc= max (rc, sx2(1)- x2+ dd);
      return rc; }
    return ref->rsub_correction (); }
  SI sub_lo_base (int level) {
    return ref->sub_lo_base (level); }
  SI sub_hi_lim  (int level) {
    return ref->sub_hi_lim (level); }
  SI sup_lo_lim  (int level) {
    if (!above)
      return max (ref->sup_lo_lim (level) - dh, box_rep::sup_lo_lim (level));
    return ref->sup_lo_lim (level); }
  SI sup_lo_base (int level) {
    return ref->sup_lo_base (level); }
  SI sup_hi_lim  (int level) {
    if (above)
      return min (ref->sup_hi_lim (level) + dh, box_rep::sup_hi_lim (level));
    return ref->sup_hi_lim (level); }
};

wide_box_rep::wide_box_rep (
  path ip, box ref2, box hi, font fn, SI sep, bool above2):
    composite_box_rep (ip), ref (ref2), above (above2)
{
  SI X, Y, dx;
  SI hw= max (ref->w(), hi->w()) >> 1;
  SI m = (ref->x1 + ref->x2) >> 1;
  insert (ref, 0, 0);
  if (above) {
    Y= ref->y2;
    X= ((SI) (ref->right_slope () * Y)) + m;
    insert (hi, X- ((hi->x1 + hi->x2)>>1), Y+ sep);
  }
  else {
    Y= ref->y1 - hi->y2;
    X= ((SI) (ref->right_slope () * (Y - sep))) + m;
    insert (hi, X- ((hi->x1 + hi->x2)>>1), Y- sep);
  }
  position ();
  dx= x1;
  left_justify ();

  dh= hi->y2+ sep;
  dw= (SI) (dh * ref->right_slope ());
  dd= fn->sep;
  x1= m- hw- dx;
  x2= m+ hw- dx;
  if (!above) y1 += fn->sep - sep;
  finalize ();
}

int
wide_box_rep::find_child (SI x, SI y, SI delta, bool force) {
  (void) y;
  if (outside (x, delta, x1, x2) && (is_accessible (ip) || force)) return -1;
  if (bs[0]->decoration () && (!force)) return -1;
  return 0;
}

double
wide_box_rep::left_slope () {
  return ref->left_slope ();
}

double
wide_box_rep::right_slope () {
  return ref->right_slope ();
}

/******************************************************************************
* box construction routines
******************************************************************************/

box
frac_box (path ip, box b1, box b2, font fn, font sfn, color c) {
  return new frac_box_rep (ip, b1, b2, fn, sfn, c);
}

box
sqrt_box (path ip, box b1, box b2, box sqrtb, font fn, color c) {
  return new sqrt_box_rep (ip, b1, b2, sqrtb, fn, c);
}

box
neg_box (path ip, box b, font fn, color c) {
  return new neg_box_rep (ip, b, fn, c);
}

box
tree_box (path ip, array<box> bs, font fn, color line_c) {
  return new tree_box_rep (ip, bs, fn, line_c);
}

box
wide_box (path ip, box ref, box hi, font fn, SI sep, bool above) {
  return new wide_box_rep (ip, ref, hi, fn, sep, above);
}
