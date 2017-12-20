
/******************************************************************************
* MODULE     : math.cpp
* DESCRIPTION: Boxes for mathematical typesetting:
*              fractions, roots, negations, limits and scripts
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Boxes/composite.hpp"
#include "Boxes/construct.hpp"
#include "Boxes/Composite/italic_correct.hpp"
#include "analyze.hpp"

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
  //cout << L << ", " << slope_L << " | " << R << ", " << slope_R << LF;
  if (L->get_type () == BIG_OP_BOX)
    if (1.5 * slope_L >= slope_R && slope_L > 0)
      if (R->get_type () != BIG_OP_BOX)
        return L->right_correction () + R->left_correction ();
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
  font fn, sfn;
  pencil pen;
  frac_box_rep (path ip, box b1, box b2, font fn, font sfn, pencil pen);
  operator tree () { return tree (TUPLE, "frac", bs[0], bs[1]); }
  box adjust_kerning (int mode, double factor);
  int find_child (SI x, SI y, SI delta, bool force);
};

frac_box_rep::frac_box_rep (
  path ip, box b1, box b2, font fn2, font sfn2, pencil pen2):
    composite_box_rep (ip), fn (fn2), sfn (sfn2), pen (pen2)
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

  pencil bar_pen= pen->set_width (bar_w);
  insert (b1, (w>>1) - (b1->x2>>1), bar_y+ sep+ (bar_w>>1)- b1_y);
  insert (b2, (w>>1) - (b2->x2>>1), bar_y- sep- (bar_w>>1)- b2_y);
  insert (line_box (decorate_middle (ip), d, 0, w-d, 0, bar_pen), 0, bar_y);

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

box
frac_box_rep::adjust_kerning (int mode, double factor) {
  (void) mode;
  box num= bs[0]->adjust_kerning (0, factor);
  box den= bs[1]->adjust_kerning (0, factor);
  return frac_box (ip, num, den, fn, sfn, pen);
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
  font fn;
  pencil pen;
  sqrt_box_rep (path ip, box b1, box b2, box sqrtb, font fn, pencil pen);
  operator tree () { return tree (TUPLE, "sqrt", bs[0]); }
  box adjust_kerning (int mode, double factor);
  int find_child (SI x, SI y, SI delta, bool force);
};

sqrt_box_rep::sqrt_box_rep (
  path ip, box b1, box b2, box sqrtb, font fn2, pencil pen2):
    composite_box_rep (ip), fn (fn2), pen (pen2)
{
  right_italic_correct (b1);

  SI sep  = fn->sep;
  SI wline= fn->wline;
  SI dx   = -fn->wfn/36, dy= -fn->wfn/36; // correction
  SI by   = sqrtb->y2+ dy;
  if (sqrtb->x2 - sqrtb->x4 > wline) dx -= (sqrtb->x2 - sqrtb->x4);
  
  pencil rpen= pen->set_width (wline);
  insert (b1, 0, 0);
  if (!is_nil (b2)) {
    SI X = - sqrtb->w();
    SI M = X / 3;
    SI Y = sqrtb->y1;
    SI bw= sqrtb->w();
    SI bh= sqrtb->h();
    if (fn->math_type == MATH_TYPE_TEX_GYRE) {
      if (2*bh < 9*bw) Y += bh >> 1;
      else if (occurs ("ermes", fn->res_name)) Y += (19*bw) >> 3;
      else if (occurs ("agella", fn->res_name)) Y += (16*bw) >> 3;
      else Y += (15*bw) >> 3;
    }
    else {
      if (bh < 3*bw) Y += bh >> 1;
      else Y += (bw*3) >> 1;
    }
    insert (b2, min (X, M- b2->x2), Y- b2->y1+ sep);
  }
  insert (sqrtb, -sqrtb->x2, 0);
  insert (line_box (decorate_middle (ip), dx, by, b1->x2, by, rpen), 0, 0);
  
  position ();
  left_justify ();
  y1 -= wline;
  y2 += wline;
  x2 += sep >> 1;

  right_italic_restore (b1);
  finalize ();
}

box
sqrt_box_rep::adjust_kerning (int mode, double factor) {
  (void) mode;
  box body = bs[0]->adjust_kerning (0, factor);
  box ramif= (N(bs) == 3? box (): bs[1]->adjust_kerning (0, factor/2));
  box sqrtb= (N(bs) == 3? bs[1]: bs[2]);
  return sqrt_box (ip, body, ramif, sqrtb, fn, pen);
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
  font fn;
  pencil pen;
  neg_box_rep (path ip, box b1, font fn, pencil pen);
  operator tree () { return tree (TUPLE, "neg", bs[0]); }
  box adjust_kerning (int mode, double factor);
  int find_child (SI x, SI y, SI delta, bool force);
};

neg_box_rep::neg_box_rep (path ip, box b, font fn2, pencil pen2):
  composite_box_rep (ip), fn (fn2), pen (pen2)
{
  SI wline= fn->wline;
  SI delta= fn->wfn/6;
  SI X    = (b->x1 + b->x2) >> 1;
  SI Y    = (b->y1 + b->y2) >> 1;
  SI DX, DY;

  pencil npen= pen->set_width (wline);
  insert (b, 0, 0);
  if ((3*(b->x2-b->x1)) > (2*(b->y2-b->y1))) {
    DY= delta + ((b->y2 - b->y1)>>1);
    DX= DY>>1;
  }
  else {
    DX= delta + ((b->x2 - b->x1)>>1);
    DY= DX;
  }
  insert (line_box (decorate_middle (ip), X+DX, Y+DY, X-DX, Y-DY, npen), 0, 0);
  
  italic_correct (b);
  position ();
  italic_restore (b);
  finalize ();
}

box
neg_box_rep::adjust_kerning (int mode, double factor) {
  box body= bs[0]->adjust_kerning (mode, factor);
  return neg_box (ip, body, fn, pen);
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
  font fn;
  pencil pen;
  SI  border;
  tree_box_rep (path ip, array<box> bs, font fn, pencil pen);
  operator tree () { return "tree box"; }
  box adjust_kerning (int mode, double factor);
  int find_child (SI x, SI y, SI delta, bool force);
};

tree_box_rep::tree_box_rep (path ip, array<box> bs, font fn2, pencil pen2):
  composite_box_rep (ip), fn (fn2), pen (pen2)
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

  pencil tpen= pen->set_width (line_w);
  for (x=x_0, i=1; i<n; i++) {
    SI x_i= x + (bs[i]->w()>>1);
    SI y_i= up + max (bs[i]->y2, fn->y2) + sep - h;
    SI bm = w>>1;
    SI bw = min (bs[0]->w(), cw>>1);
    SI bx = bm + ((2*i-n) * bw) / (2*n-2);
    SI by = min (bs[0]->y1, fn->y1) - sep;
    insert (line_box (decorate_middle (ip), bx, by, x_i, y_i, tpen), 0, 0);
    x += bs[i]->w()+ hsep;
  }

  position ();
  border= up+ (vsep>>1);
  finalize ();
}

box
tree_box_rep::adjust_kerning (int mode, double factor) {
  (void) mode;
  int n= (N(bs)+1)>>1;
  array<box> adj (n);
  for (int i=0; i<n; i++)
    adj[i]= bs[i]->adjust_kerning (0, factor);
  return tree_box (ip, adj, fn, pen);
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
* Computation of wide accent
******************************************************************************/

bool
compute_wide_accent (path ip, box b, string s,
                     font fn, pencil pen, bool request_wide, bool above,
                     box& wideb, SI& sep) {
  bool unicode= (fn->type == FONT_TYPE_UNICODE);
  bool stix= (fn->math_type == MATH_TYPE_STIX);
  bool tex_gyre= (fn->math_type == MATH_TYPE_TEX_GYRE);
  bool wide= (b->w() > (fn->wquad)) || request_wide;
  if (ends (s, "dot>") || (s == "<acute>") ||
      (s == "<grave>") || (s == "<abovering>")) wide= false;
  if (wide && !request_wide && b->wide_correction (0) != 0) wide= false;
  bool very_wide= false;
  SI   accw= fn->wfn;
  if (wide) {
    if (tex_gyre) {
      if (s == "^" || s == "<hat>" ||
          s == "~" || s == "<tilde>" ||
          s == "<check>")
        very_wide= (b->w() >= ((8*fn->wfn) >> 2));
      else if (ends (s, "brace>") || ends (s, "brace*>")) {
        if (starts (s, "<sq"))
          very_wide= (b->w() >= ((11*fn->wfn) >> 2));
        else very_wide= (b->w() >= ((15*fn->wfn) >> 2));
      }
      else very_wide= true;
    }
    else if (!unicode) {
      if (s == "^" || s == "<hat>" || s == "~" || s == "<tilde>")
        very_wide= (b->w() >= ((9*fn->wfn) >> 2));
      else very_wide= true;
    }
    else if (stix) very_wide= true;
    /*
    else if (s == "^" || s == "<hat>" || s == "~" || s == "<tilde>" ||
             s == "<bar>" || s == "<vect>" || s == "<check>" ||
             s == "<breve>" || s == "<invbreve>") {
      box wb= text_box (decorate_middle (ip), 0, s, fn, pen);
      accw= wb->x4 - wb->x3;
      if (b->w() >= 16*accw) very_wide= true;
    }
    */
    else very_wide= true;
  }
  if (wide && stix) {
    if (s == "^") s= "<hat>";
    if (s == "~") s= "<tilde>";
    if (s == "<hat>" || s == "<tilde>" || s == "<check>" ||
        ends (s, "brace>") || ends (s, "brace*>")) {
      font rfn= rubber_font (fn);
      SI width= b->x2- b->x1 - fn->wfn/4;
      wideb= wide_stix_box (decorate_middle (ip),
                            "<rubber-" * s (1, N(s)-1) * ">",
                            rfn, pen, width);
      if (wideb->w() >= width) {
        if (b->right_slope () != 0)
          wideb= shift_box (decorate_middle (ip), wideb,
                            (SI) (-0.5 * b->right_slope () * fn->yx), 0);
        sep= above? -fn->yx: fn->sep;
        if (above) {
          if (s == "<overbrace>" || s == "<squnderbrace*>") sep= 2 * fn->sep;
          if (s == "<poverbrace>") sep= 3 * fn->sep;
        }
        return wide;
      }
    }
  }
  if (very_wide) {
    SI w= fn->wline;
    if (stix) w= (SI) (1.189 * w);
    pencil wpen= pen->set_width (w);
    if ((s == "^") || (s == "<hat>"))
      wideb= wide_hat_box   (decorate_middle (ip), b->x1, b->x2, wpen);
    else if ((s == "~") || (s == "<tilde>"))
      wideb= wide_tilda_box (decorate_middle (ip), b->x1, b->x2, wpen);
    else if (s == "<bar>")
      wideb= wide_bar_box   (decorate_middle (ip), b->x1, b->x2, wpen);
    else if (s == "<vect>")
      wideb= wide_vect_box  (decorate_middle (ip), b->x1, b->x2, wpen);
    else if (s == "<check>")
      wideb= wide_check_box (decorate_middle (ip), b->x1, b->x2, wpen);
    else if (s == "<breve>" || s == "<punderbrace>" || s == "<punderbrace*>")
      wideb= wide_breve_box (decorate_middle (ip), b->x1, b->x2, wpen);
    else if (s == "<invbreve>" || s == "<poverbrace>" || s == "<poverbrace*>")
      wideb= wide_invbreve_box(decorate_middle (ip), b->x1, b->x2, wpen);
    else if (s == "<squnderbrace>" || s == "<squnderbrace*>")
      wideb= wide_squbr_box (decorate_middle (ip), b->x1, b->x2, wpen);
    else if (s == "<sqoverbrace>" || s == "<sqoverbrace*>")
      wideb= wide_sqobr_box (decorate_middle (ip), b->x1, b->x2, wpen);
    else wideb= wide_box (decorate_middle (ip),
                          "<rubber-" * s (1, N(s)-1) * ">",
                          fn, pen, b->x2- b->x1);
    sep= fn->sep;
    if (stix || !unicode) sep= (SI) (1.5 * sep);
  }
  else if (wide && tex_gyre) {
    string ws= "<wide-" * s (1, N(s)-1) * ">";
    SI width= b->x2- b->x1 - fn->wfn/4;
    wideb= wide_box (decorate_middle (ip), ws, fn, pen, width);
    if (b->right_slope () != 0) {
      bool times= stix || (tex_gyre && occurs ("ermes", fn->res_name));
      double factor= ((times || !above)? 0.2: 0.5);
      wideb= shift_box (decorate_middle (ip), wideb,
                        (SI) (-factor * b->right_slope () * fn->yx), 0);
    }
    sep= above? -fn->yx: fn->sep;
  }
  else if (wide && !unicode) {
    string ss= s (1, N(s)-1);
    if (ss == "^") ss= "hat";
    if (ss == "~") ss= "tilde";
    string ws= "<wide-" * ss * ">";
    SI width= b->x2- b->x1 - fn->wfn/4;
    wideb= wide_box (decorate_middle (ip), ws, fn, pen, width);
    if (b->right_slope () != 0) {
      double factor= (above? 0.5: 0.2);
      wideb= shift_box (decorate_middle (ip), wideb,
                        (SI) (-factor * b->right_slope () * fn->yx), 0);
    }
    sep= above? -fn->yx: fn->sep;
  }
  else if (wide) {
    SI pad= fn->wfn - accw;
    pad= (SI) ((0.75 * accw * pad) / (b->w() - pad));
    double sx= ((double) (b->w() - pad)) / ((double) accw);
    sx= floor (4.0*sx) / 4.0;
    double sy= sqrt (sqrt (sx));
    font sfn= fn->magnify (sx, sy);
    wideb= text_box (decorate_middle (ip), 0, s, sfn, pen);
    wideb= resize_box (decorate_middle (ip), wideb,
                       max (wideb->x1, wideb->x3), wideb->y1,
                       min (wideb->x2, wideb->x4), wideb->y2);
    if (unicode && b->right_slope () != 0)
      wideb= shift_box (decorate_middle (ip), wideb,
                        (SI) (-0.5 * b->right_slope () * fn->yx), 0);
    sep= above? -fn->yx: fn->sep;
    if (above) sep -= 3 * (sy - 1.0) * fn->sep;
  }
  else {
    wideb= text_box (decorate_middle (ip), 0, s, fn, pen);
    if (unicode && b->right_slope () != 0) {
      bool times= stix || (tex_gyre && occurs ("ermes", fn->res_name));
      double factor= ((times || !above)? 0.2: 0.5);
      wideb= shift_box (decorate_middle (ip), wideb,
                        (SI) (-factor * b->right_slope () * fn->yx), 0);
    }
    sep= above? -fn->yx: fn->sep;
  }
  if (above && unicode) {
    SI min_d= fn->yx / 8;
    SI max_d= fn->yx / 3;
    if (wideb->y1 + sep <  min_d) sep= min_d - wideb->y1;
    if (wideb->y1 + sep >= max_d) sep= max_d - wideb->y1;
  }
  if (!unicode && !wide && !above)
    wideb= vresize_box (wideb->ip, wideb, wideb->y1 + fn->yx, wideb->y2);
  else if (unicode && s == "<vect>") {
    if (wide);
    else if (above) sep -= fn->yx + (fn->sep >> 1);
    else wideb= vresize_box (wideb->ip, wideb, wideb->y1 + fn->yx, wideb->y2);
  }
  else if (stix || tex_gyre) sep += fn->sep >> 1;
  return wide;
}

/******************************************************************************
* wide hats, tildas, etc...
******************************************************************************/

struct wide_box_rep: public composite_box_rep {
  box    ref;
  string s;
  font   fn;
  pencil pen;
  bool   request_wide;
  bool   wide;
  bool   above;
  SI     sep;
  SI     dw, dh, dd;
  wide_box_rep (path ip, box b, string s, font fn, pencil p, bool wf, bool af);
  operator tree () { return tree (TUPLE, "wide", bs[0]); }
  box adjust_kerning (int mode, double factor);
  int find_child (SI x, SI y, SI delta, bool force);
  double left_slope ();
  double right_slope ();

  SI left_correction () {
    return ref->left_correction (); }
  SI right_correction () {
    /*
    if (above) {
      SI rc= ref->right_correction () + dw;
      if (sx4 (1) >= (sx2 (1) - (dd>>1))) // corrects buggy extents wide chars
	rc= max (rc, sx2(1)- x2+ dd);
      return rc; }
    */
    return ref->right_correction (); }
  SI lsub_correction () {
    return ref->lsub_correction (); }
  SI lsup_correction () {
    return ref->lsup_correction (); }
  SI rsub_correction () {
    return ref->rsub_correction (); }
  SI rsup_correction () {
    /*
    if (above) {
      SI rc= ref->rsup_correction () + dw;
      if (sx4 (1) >= (sx2 (1) - (dd>>1))) // corrects buggy extents wide chars
	rc= max (rc, sx2(1)- x2+ dd);
      return rc; }
    */
    return ref->rsup_correction (); }
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
  SI wide_correction (int mode) {
    return ref->wide_correction (mode); }
  void get_bracket_extents (SI& lo, SI& hi);
};

wide_box_rep::wide_box_rep (
  path ip, box ref2, string s2, font fn2, pencil pen2,
  bool request_wide2, bool above2):
    composite_box_rep (ip), ref (ref2), s (s2), fn (fn2), pen (pen2),
    request_wide (request_wide2), above (above2)
{
  box hi;
  wide= compute_wide_accent (ip, ref, s, fn, pen, request_wide, above, hi, sep);
  SI X, Y, dx;
  SI hw= max (ref->w(), hi->w()) >> 1;
  SI m = (ref->x1 + ref->x2) >> 1;
  insert (ref, 0, 0);
  if (above) {
    Y= ref->y2;
    X= m;
    if (ref->right_slope () != 0)
      X += ref->rsup_correction() + ((SI) (ref->right_slope() * fn->yx * 0.5));
    X += ref->wide_correction (1);
    //X= ((SI) (ref->right_slope () * (Y - fn->yx))) + m;
    insert (hi, X- ((hi->x1 + hi->x2)>>1), Y+ sep);
  }
  else {
    Y= ref->y1 - hi->y2;
    X= m - ((SI) (ref->right_slope () * sep));
    X += ref->wide_correction (-1);
    //X= ((SI) (ref->right_slope () * (Y - sep))) + m;
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

box
wide_box_rep::adjust_kerning (int mode, double factor) {
  (void) mode;
  box body= ref->adjust_kerning (START_OF_LINE + END_OF_LINE, factor);
  return wide_box (ip, body, s, fn, pen, request_wide, above);
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

void
wide_box_rep::get_bracket_extents (SI& lo, SI& hi) {
  if (wide) box_rep::get_bracket_extents (lo, hi);
  else {
    lo= ref->y1;
    hi= ref->y2;
  }
}

/******************************************************************************
* box construction routines
******************************************************************************/

box
frac_box (path ip, box b1, box b2, font fn, font sfn, pencil pen) {
  return tm_new<frac_box_rep> (ip, b1, b2, fn, sfn, pen);
}

box
sqrt_box (path ip, box b1, box b2, box sqrtb, font fn, pencil pen) {
  return tm_new<sqrt_box_rep> (ip, b1, b2, sqrtb, fn, pen);
}

box
neg_box (path ip, box b, font fn, pencil pen) {
  return tm_new<neg_box_rep> (ip, b, fn, pen);
}

box
tree_box (path ip, array<box> bs, font fn, pencil pen) {
  return tm_new<tree_box_rep> (ip, bs, fn, pen);
}

box
wide_box (path ip, box ref, string s, font fn, pencil pen, bool wf, bool af) {
  return tm_new<wide_box_rep> (ip, ref, s, fn, pen, wf, af);
}
