
/******************************************************************************
* MODULE     : effect.hpp
* DESCRIPTION: Graphical effects
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "effect.hpp"
#include "true_color.hpp"
#include "gui.hpp"
#include "matrix.hpp"

/******************************************************************************
* Default implementations of virtual routines
******************************************************************************/

static rectangle
join (array<rectangle> rs, composition_mode mode) {
  if (N(rs) == 0) return rectangle (0, 0, 0, 0);
  rectangle ret= rs[0];
  for (int i=1; i<N(rs); i++) {
    rectangle aux= rs[i];
    switch (composition_type (mode)) {
    case 0:
      ret= rectangle (max (ret->x1, aux->x1), max (ret->y1, aux->y1),
                      min (ret->x2, aux->x2), min (ret->y2, aux->y2));
      break;
    case 1:
      ret= aux;
      break;
    case 3:
      ret= rectangle (min (ret->x1, aux->x1), min (ret->y1, aux->y1),
                      max (ret->x2, aux->x2), max (ret->y2, aux->y2));
      break;
    }
  }
  ret->x1= min (ret->x1, ret->x2);
  ret->y1= min (ret->y1, ret->y2);
  return ret;
}

rectangle
effect_rep::get_logical_extents (array<rectangle> rs) {
  return join (rs, compose_source_over);
}

/******************************************************************************
* Unmodified argument
******************************************************************************/

class argument_effect_rep: public effect_rep {
  int nr;
public:
  argument_effect_rep (int nr2): nr (nr2) {}
  rectangle get_logical_extents (array<rectangle> rs) { return rs[nr]; }
  rectangle get_extents (array<rectangle> rs) { return rs[nr]; }
  picture apply (array<picture> pics, SI pixel) {
    (void) pixel; return pics[nr]; }
};

effect argument_effect (int nr) {
  return tm_new<argument_effect_rep> (nr); }

/******************************************************************************
* Turbulence effect
******************************************************************************/

class turbulence_effect_rep: public effect_rep {
  effect eff;
  long seed;
  double w, h;
  int octaves;
  bool frac_sum;
public:
  turbulence_effect_rep (effect e, long s, double w2, double h2, int o, bool f):
    eff (e), seed (s), w (w2), h (h2), octaves (o), frac_sum (f) {}
  rectangle get_logical_extents (array<rectangle> rs) {
    return eff->get_logical_extents (rs); }
  rectangle get_extents (array<rectangle> rs) {
    return eff->get_extents (rs); }
  picture apply (array<picture> pics, SI pixel) {
    if (frac_sum)
      return fractal_noise (eff->apply (pics, pixel), seed,
                            w / pixel, h / pixel, octaves);
    else
      return turbulence (eff->apply (pics, pixel), seed,
                         w / pixel, h / pixel, octaves); }
};

effect turbulence (effect e, long s, double w, double h, int oct) {
  return tm_new<turbulence_effect_rep> (e, s, w, h, oct, false); }
effect fractal_noise (effect e, long s, double w, double h, int oct) {
  return tm_new<turbulence_effect_rep> (e, s, w, h, oct, true); }

/******************************************************************************
* Coordinate transformations
******************************************************************************/

class move_effect_rep: public effect_rep {
  effect eff;
  double dx, dy;
public:
  move_effect_rep (effect eff2, double dx2, double dy2):
    eff (eff2), dx (dx2), dy (dy2) {}
  rectangle get_logical_extents (array<rectangle> rs) {
    rectangle r= eff->get_logical_extents (rs);
    return rectangle ((SI) round (r->x1 + dx), (SI) round (r->y1 + dy),
                      (SI) round (r->x2 + dx), (SI) round (r->y2 + dy)); }
  rectangle get_extents (array<rectangle> rs) {
    rectangle r= eff->get_extents (rs);
    return rectangle ((SI) floor (r->x1 + dx), (SI) floor (r->y1 + dy),
                      (SI) ceil  (r->x2 + dx), (SI) ceil  (r->y2 + dy)); }
  picture apply (array<picture> pics, SI pixel) {
    return shift (eff->apply (pics, pixel), dx / pixel, dy / pixel); }
};

class magnify_effect_rep: public effect_rep {
  effect eff;
  double sx, sy;
public:
  magnify_effect_rep (effect eff2, double sx2, double sy2):
    eff (eff2), sx (sx2), sy (sy2) {}
  rectangle get_logical_extents (array<rectangle> rs) {
    rectangle r= eff->get_logical_extents (rs);
    return rectangle ((SI) round (sx * r->x1), (SI) round (sy * r->y1),
                      (SI) round (sx * r->x2), (SI) round (sy * r->y2)); }
  rectangle get_extents (array<rectangle> rs) {
    rectangle r= eff->get_extents (rs);
    return rectangle ((SI) floor (sx * r->x1), (SI) floor (sy * r->y1),
                      (SI) ceil  (sx * r->x2), (SI) ceil  (sy * r->y2)); }
  picture apply (array<picture> pics, SI pixel) {
    return magnify (eff->apply (pics, pixel), sx, sy); }
};

class bubble_effect_rep: public effect_rep {
  effect eff;
  double r, a;
public:
  bubble_effect_rep (effect eff2, double r2, double a2):
    eff (eff2), r (r2), a (a2) {}
  rectangle get_extents (array<rectangle> rs) {
    rectangle rect= eff->get_extents (rs);
    double delta= r * (1 - a);
    return rectangle ((SI) floor (rect->x1 - delta),
                      (SI) floor (rect->y1 - delta),
                      (SI) ceil  (rect->x2 + delta),
                      (SI) ceil  (rect->y2 + delta)); }
  picture apply (array<picture> pics, SI pixel) {
    return bubble (eff->apply (pics, pixel), r / pixel, a); }
};

class crop_effect_rep: public effect_rep {
  effect eff;
  double cx1, cy1, cx2, cy2;
public:
  crop_effect_rep (effect eff2, double x1, double y1, double x2, double y2):
    eff (eff2), cx1 (x1), cy1 (y1), cx2 (x2), cy2 (y2) {}
  rectangle get_logical_extents (array<rectangle> rs) {
    rectangle r= eff->get_logical_extents (rs);
    SI x1= (SI) round (r->x1 + cx1 * (r->x2 - r->x1));
    SI y1= (SI) round (r->y1 + cy1 * (r->y2 - r->y1));
    SI x2= (SI) round (r->x1 + cx2 * (r->x2 - r->x1));
    SI y2= (SI) round (r->y1 + cy2 * (r->y2 - r->y1));
    return rectangle (0, 0, x2 - x1, y2 - y1); }
  rectangle get_extents (array<rectangle> rs) {
    rectangle r= eff->get_extents (rs);
    SI x1= (SI) floor (r->x1 + cx1 * (r->x2 - r->x1));
    SI y1= (SI) floor (r->y1 + cy1 * (r->y2 - r->y1));
    SI x2= (SI) ceil  (r->x1 + cx2 * (r->x2 - r->x1));
    SI y2= (SI) ceil  (r->y1 + cy2 * (r->y2 - r->y1));
    return rectangle (0, 0, x2 - x1, y2 - y1); }
  picture apply (array<picture> pics, SI pixel) {
    return crop (eff->apply (pics, pixel), cx1, cy1, cx2, cy2); }
};

effect move (effect eff, double dx, double dy) {
  return tm_new<move_effect_rep> (eff, dx, dy); }
effect magnify (effect eff, double sx, double sy) {
  return tm_new<move_effect_rep> (eff, sx, sy); }
effect bubble (effect eff, double r, double a) {
  return tm_new<bubble_effect_rep> (eff, r, a); }
effect crop (effect eff, double cx1, double cy1, double cx2, double cy2) {
  cx1= max (cx1, 0.0); cy1= max (cy1, 0.0);
  cx2= min (cx2, 1.0); cy2= min (cy2, 1.0);
  return tm_new<crop_effect_rep> (eff, cx1, cy1, cx2, cy2); }

/******************************************************************************
* Pens
******************************************************************************/

class gaussian_pen_effect_rep: public effect_rep {
  double rx, ry, phi;
public:
  gaussian_pen_effect_rep (double rx2, double ry2, double phi2):
    rx (rx2), ry (ry2), phi (phi2) {}
  rectangle get_extents (array<rectangle> rs) { (void) rs;
    SI R= (SI) ceil (2.5 * max (rx, ry));
    return rectangle (-R, -R, R, R); }
  picture apply (array<picture> pics, SI pixel) { (void) pics;
    return gaussian_pen_picture (rx / pixel, ry / pixel, phi); }
};

class oval_pen_effect_rep: public effect_rep {
  double rx, ry, phi;
public:
  oval_pen_effect_rep (double rx2, double ry2, double phi2):
    rx (rx2), ry (ry2), phi (phi2) {}
  rectangle get_extents (array<rectangle> rs) { (void) rs;
    SI R= (SI) max (rx, ry);
    return rectangle (-R, -R, R, R); }
  picture apply (array<picture> pics, SI pixel) { (void) pics;
    return oval_pen_picture (rx / pixel, ry / pixel, phi); }
};

class rectangular_pen_effect_rep: public effect_rep {
  double rx, ry, phi;
public:
  rectangular_pen_effect_rep (double rx2, double ry2, double p2):
    rx (rx2), ry (ry2), phi (p2) {}
  rectangle get_extents (array<rectangle> rs) { (void) rs;
    SI R= (SI) sqrt (rx * rx + ry * ry);
    return rectangle (-R, -R, R, R); }
  picture apply (array<picture> pics, SI pixel) { (void) pics;
    return rectangular_pen_picture (rx / pixel, ry / pixel, phi); }
};

class motion_pen_effect_rep: public effect_rep {
  double dx, dy;
public:
  motion_pen_effect_rep (double dx2, double dy2):
    dx (dx2), dy (dy2) {}
  rectangle get_extents (array<rectangle> rs) { (void) rs;
    return rectangle ((SI) min (dx, 0.0), (SI) min (dy, 0.0),
                      (SI) max (dx, 0.0), (SI) max (dy, 0.0)); }
  picture apply (array<picture> pics, SI pixel) { (void) pics;
    return motion_pen_picture (dx / pixel, dy / pixel); }
};

effect gaussian_pen_effect (double r) {
  return tm_new<gaussian_pen_effect_rep> (r, r, 0.0); }
effect oval_pen_effect (double r) {
  return tm_new<oval_pen_effect_rep> (r, r, 0.0); }
effect rectangular_pen_effect (double r) {
  return tm_new<rectangular_pen_effect_rep> (r, r, 0.0); }

effect gaussian_pen_effect (double rx, double ry, double phi) {
  return tm_new<gaussian_pen_effect_rep> (rx, ry, phi); }
effect oval_pen_effect (double rx, double ry, double phi) {
  return tm_new<oval_pen_effect_rep> (rx, ry, phi); }
effect rectangular_pen_effect (double rx, double ry, double phi) {
  return tm_new<rectangular_pen_effect_rep> (rx, ry, phi); }

effect motion_pen_effect (double dx, double dy) {
  return tm_new<motion_pen_effect_rep> (dx, dy); }

/******************************************************************************
* Special effects, taking a pen as parameter
******************************************************************************/

class blur_effect_rep: public effect_rep {
  effect eff, pen;
public:
  blur_effect_rep (effect eff2, effect pen2):
    eff (eff2), pen (pen2) {}
  rectangle get_extents (array<rectangle> rs) {
    rectangle r1= eff->get_extents (rs);
    rectangle r2= pen->get_extents (rs);
    return rectangle (r1->x1 + r2->x1, r1->y1 + r2->y1,
                      r1->x2 + r2->x2, r1->y2 + r2->y2); }
  picture apply (array<picture> pics, SI pixel) {
    picture p1= eff->apply (pics, pixel);
    picture p2= pen->apply (pics, pixel);
    return blur (p1, p2); }
};

class outline_effect_rep: public effect_rep {
  effect eff, pen;
public:
  outline_effect_rep (effect eff2, effect pen2):
    eff (eff2), pen (pen2) {}
  rectangle get_extents (array<rectangle> rs) {
    rectangle r1= eff->get_extents (rs);
    rectangle r2= pen->get_extents (rs);
    return rectangle (r1->x1 + r2->x1, r1->y1 + r2->y1,
                      r1->x2 + r2->x2, r1->y2 + r2->y2); }
  picture apply (array<picture> pics, SI pixel) {
    picture p1= eff->apply (pics, pixel);
    picture p2= pen->apply (pics, pixel);
    return outline (p1, p2); }
};

class thicken_effect_rep: public effect_rep {
  effect eff, pen;
public:
  thicken_effect_rep (effect eff2, effect pen2):
    eff (eff2), pen (pen2) {}
  rectangle get_extents (array<rectangle> rs) {
    rectangle r1= eff->get_extents (rs);
    rectangle r2= pen->get_extents (rs);
    return rectangle (r1->x1 + r2->x1, r1->y1 + r2->y1,
                      r1->x2 + r2->x2, r1->y2 + r2->y2); }
  picture apply (array<picture> pics, SI pixel) {
    picture p1= eff->apply (pics, pixel);
    picture p2= pen->apply (pics, pixel);
    return thicken (p1, p2); }
};

class erode_effect_rep: public effect_rep {
  effect eff, pen;
public:
  erode_effect_rep (effect eff2, effect pen2):
    eff (eff2), pen (pen2) {}
  rectangle get_extents (array<rectangle> rs) {
    rectangle r1= eff->get_extents (rs);
    rectangle r2= pen->get_extents (rs);
    return rectangle (r1->x1 + r2->x1, r1->y1 + r2->y1,
                      r1->x2 + r2->x2, r1->y2 + r2->y2); }
  picture apply (array<picture> pics, SI pixel) {
    picture p1= eff->apply (pics, pixel);
    picture p2= pen->apply (pics, pixel);
    return erode (p1, p2); }
};

effect blur (effect eff, effect pen) {
  return tm_new<blur_effect_rep> (eff, pen); }
effect outline (effect eff, effect pen) {
  return tm_new<outline_effect_rep> (eff, pen); }
effect thicken (effect eff, effect pen) {
  return tm_new<thicken_effect_rep> (eff, pen); }
effect erode (effect eff, effect pen) {
  return tm_new<erode_effect_rep> (eff, pen); }

/******************************************************************************
* Effects that depend on randomization
******************************************************************************/

class degrade_effect_rep: public effect_rep {
  effect eff;
  double wx, wy, th, sh;
public:
  degrade_effect_rep (effect e2, double Wx, double Wy, double Th, double Sh):
    eff (e2), wx (Wx), wy (Wy), th (Th), sh (Sh) {}
  rectangle get_extents (array<rectangle> rs) {
    return eff->get_extents (rs); }
  picture apply (array<picture> pics, SI pixel) {
    return degrade (eff->apply (pics, pixel),
                    wx / pixel, wy / pixel, th, sh); }
};

effect degrade (effect eff, double wx, double wy, double th, double sh) {
  return tm_new<degrade_effect_rep> (eff, wx, wy, th, sh); }

class distort_effect_rep: public effect_rep {
  effect eff;
  double wx, wy, rx, ry;
public:
  distort_effect_rep (effect e2, double Wx, double Wy, double Rx, double Ry):
    eff (e2), wx (Wx), wy (Wy), rx (Rx), ry (Ry) {}
  rectangle get_extents (array<rectangle> rs) {
    return eff->get_extents (rs); }
  picture apply (array<picture> pics, SI pixel) {
    return distort (eff->apply (pics, pixel),
                    wx / pixel, wy / pixel, rx / pixel, ry / pixel); }
};

effect distort (effect eff, double wx, double wy, double rx, double ry) {
  return tm_new<distort_effect_rep> (eff, wx, wy, rx, ry); }

class gnaw_effect_rep: public effect_rep {
  effect eff;
  double wx, wy, rx, ry;
public:
  gnaw_effect_rep (effect e2, double Wx, double Wy, double Rx, double Ry):
    eff (e2), wx (Wx), wy (Wy), rx (Rx), ry (Ry) {}
  rectangle get_extents (array<rectangle> rs) {
    return eff->get_extents (rs); }
  picture apply (array<picture> pics, SI pixel) {
    return gnaw (eff->apply (pics, pixel),
                 wx / pixel, wy / pixel, rx / pixel, ry / pixel); }
};

effect gnaw (effect eff, double wx, double wy, double rx, double ry) {
  return tm_new<gnaw_effect_rep> (eff, wx, wy, rx, ry); }

/******************************************************************************
* Composing various effects
******************************************************************************/

class compose_effect_rep: public effect_rep {
  array<effect> effs;
  composition_mode mode;
public:
  compose_effect_rep (array<effect> e, composition_mode m):
    effs (e), mode (m) {
      ASSERT (N (effs) > 0, "at least one effect expected"); }
  rectangle get_logical_extents (array<rectangle> rs) {
    array<rectangle> xrs (N(effs));
    for (int i=0; i<N(effs); i++)
      xrs[i]= effs[i]->get_logical_extents (rs);
    return join (xrs, mode); }
  rectangle get_extents (array<rectangle> rs) {
    array<rectangle> xrs (N(effs));
    for (int i=0; i<N(effs); i++)
      xrs[i]= effs[i]->get_extents (rs);
    return join (xrs, mode); }
  picture apply (array<picture> pics, SI pixel) {
    array<picture> args (N(effs));
    for (int i=0; i<N(effs); i++)
      args[i]= effs[i]->apply (pics, pixel);
    return compose (args, mode); }
};

effect compose (array<effect> effs, composition_mode mode) {
  return tm_new<compose_effect_rep> (effs, mode); }
effect superpose (array<effect> effs) {
  return compose (effs, compose_source_over); }
effect add (array<effect> effs) {
  return compose (effs, compose_add); }
effect sub (array<effect> effs) {
  return compose (effs, compose_sub); }
effect mul (array<effect> effs) {
  return compose (effs, compose_mul); }
effect min (array<effect> effs) {
  return compose (effs, compose_min); }
effect max (array<effect> effs) {
  return compose (effs, compose_max); }

class mix_effect_rep: public effect_rep {
  effect eff1, eff2;
  double a1, a2;
public:
  mix_effect_rep (effect eff1b, double a1b, effect eff2b, double a2b):
    eff1 (eff1b), eff2 (eff2b), a1 (a1b), a2 (a2b) {}
  rectangle get_extents (array<rectangle> rs) {
    array<rectangle> xrs (2);
    xrs[0]= eff1->get_extents (rs);
    xrs[1]= eff2->get_extents (rs);
    return join (xrs, compose_add); }
  picture apply (array<picture> pics, SI pixel) {
    picture pic1= eff1->apply (pics, pixel);
    picture pic2= eff2->apply (pics, pixel);
    return mix (pic1, a1, pic2, a2); }
};

effect mix (effect eff1, double a1, effect eff2, double a2) {
  return tm_new<mix_effect_rep> (eff1, a1, eff2, a2); }

/******************************************************************************
* Color effects
******************************************************************************/

class normalize_effect_rep: public effect_rep {
  effect eff;
public:
  normalize_effect_rep (effect eff2):
    eff (eff2) {}
  rectangle get_extents (array<rectangle> rs) {
    return eff->get_extents (rs); }
  picture apply (array<picture> pics, SI pixel) {
    return normalize (eff->apply (pics, pixel)); }
};

class color_matrix_effect_rep: public effect_rep {
  effect eff;
  array<double> m;
public:
  color_matrix_effect_rep (effect eff2, array<double> m2):
    eff (eff2), m (m2) {}
  rectangle get_extents (array<rectangle> rs) {
    return eff->get_extents (rs); }
  picture apply (array<picture> pics, SI pixel) {
    return color_matrix (eff->apply (pics, pixel), m); }
};

class make_transparent_effect_rep: public effect_rep {
  effect eff;
  color bgc;
  double t;
public:
  make_transparent_effect_rep (effect eff2, color bgc2, double t2):
    eff (eff2), bgc (bgc2), t (t2) {}
  rectangle get_extents (array<rectangle> rs) {
    return eff->get_extents (rs); }
  picture apply (array<picture> pics, SI pixel) {
    return make_transparent (eff->apply (pics, pixel), bgc, t); }
};

class make_opaque_effect_rep: public effect_rep {
  effect eff;
  color bgc;
public:
  make_opaque_effect_rep (effect eff2, color bgc2):
    eff (eff2), bgc (bgc2) {}
  rectangle get_extents (array<rectangle> rs) {
    return eff->get_extents (rs); }
  picture apply (array<picture> pics, SI pixel) {
    return make_opaque (eff->apply (pics, pixel), bgc); }
};

class recolor_effect_rep: public effect_rep {
  effect eff;
  color col;
public:
  recolor_effect_rep (effect eff2, color col2):
    eff (eff2), col (col2) {}
  rectangle get_extents (array<rectangle> rs) {
    return eff->get_extents (rs); }
  picture apply (array<picture> pics, SI pixel) {
    return recolor (eff->apply (pics, pixel), col); }
};

class skin_effect_rep: public effect_rep {
  effect eff;
  color col;
public:
  skin_effect_rep (effect eff2, color col2):
    eff (eff2), col (col2) {}
  rectangle get_extents (array<rectangle> rs) {
    return eff->get_extents (rs); }
  picture apply (array<picture> pics, SI pixel) {
    return apply_skin (eff->apply (pics, pixel), col); }
};

effect normalize (effect eff) {
  return tm_new<normalize_effect_rep> (eff); }
effect color_matrix (effect eff, array<double> m) {
  return tm_new<color_matrix_effect_rep> (eff, m); }
effect make_transparent (effect eff, color bgc, double t) {
  return tm_new<make_transparent_effect_rep> (eff, bgc, t); }
effect make_opaque (effect eff, color bgc) {
  return tm_new<make_opaque_effect_rep> (eff, bgc); }
effect recolor (effect eff, color col) {
  return tm_new<recolor_effect_rep> (eff, col); }
effect apply_skin (effect eff, color col) {
  return tm_new<skin_effect_rep> (eff, col); }

/******************************************************************************
* Hatching
******************************************************************************/

class hatch_effect_rep: public effect_rep {
  effect eff;
  int sx, sy;
  double fp;
  double de;
public:
  hatch_effect_rep (effect e, int sx2, int sy2, double fp2, double de2):
    eff (e), sx (sx2), sy (sy2), fp (fp2), de (de2) {}
  rectangle get_logical_extents (array<rectangle> rs) {
    return eff->get_logical_extents (rs); }
  rectangle get_extents (array<rectangle> rs) {
    return eff->get_extents (rs); }
  picture apply (array<picture> pics, SI pixel) {
    return hatch (eff->apply (pics, pixel), sx, sy, fp, de); }
};

effect hatch (effect e, int sx, int sy, double fp, double de) {
  return tm_new<hatch_effect_rep> (e, sx, sy, fp, de); }

class dots_effect_rep: public effect_rep {
  effect eff;
  int a, b, c, d;
  double fp;
  double de;
public:
  dots_effect_rep (effect e, int a2, int b2, int c2, int d2,
                   double fp2, double de2):
    eff (e), a (a2), b (b2), c (c2), d (d2), fp (fp2), de (de2) {}
  rectangle get_logical_extents (array<rectangle> rs) {
    return eff->get_logical_extents (rs); }
  rectangle get_extents (array<rectangle> rs) {
    return eff->get_extents (rs); }
  picture apply (array<picture> pics, SI pixel) {
    return dots (eff->apply (pics, pixel), a, b, c, d, fp, de); }
};

effect dots (effect e, int a, int b, int c, int d, double fp, double de) {
  return tm_new<dots_effect_rep> (e, a, b, c, d, fp, de); }

/******************************************************************************
* Building effects from tree description
******************************************************************************/

array<effect>
build_effects (array<tree> a) {
  array<effect> effs (N(a));
  for (int i=0; i<N(a); i++)
    effs[i]= build_effect (a[i]);
  return effs;
}

effect
build_effect (tree t) {
  if (t == "")
    return argument_effect (0);
  else if (is_int (t))
    return argument_effect (as_int (t));
  else if (is_func (t, EFF_MOVE, 3)) {
    effect eff= build_effect (t[0]);
    double dx = as_double (t[1]);
    double dy = as_double (t[2]);
    return move (eff, dx, dy);
  }
  else if (is_func (t, EFF_MAGNIFY, 3)) {
    effect eff= build_effect (t[0]);
    double sx = as_double (t[1]);
    double sy = as_double (t[2]);
    return magnify (eff, sx, sy);
  }
  else if (is_func (t, EFF_BUBBLE, 3)) {
    effect eff= build_effect (t[0]);
    double r  = as_double (t[1]);
    double a  = as_double (t[2]);
    return bubble (eff, r, a);
  }
  else if (is_func (t, EFF_GAUSSIAN, 1)) {
    double r= as_double (t[0]);
    return gaussian_pen_effect (r);
  }
  else if (is_func (t, EFF_TURBULENCE, 5)) {
    effect e= build_effect (t[0]);
    int    s= as_int (t[1]);
    double w= as_double (t[2]);
    double h= as_double (t[3]);
    int    o= as_int (t[4]);
    return turbulence (e, s, w, h, o);
  }
  else if (is_func (t, EFF_FRACTAL_NOISE, 5)) {
    effect e= build_effect (t[0]);
    int    s= as_int (t[1]);
    double w= as_double (t[2]);
    double h= as_double (t[3]);
    int    o= as_int (t[4]);
    return fractal_noise (e, s, w, h, o);
  }
  else if (is_compound (t, EFF_HATCH, 5)) {
    effect e = build_effect (t[0]);
    int    sx= as_int (t[1]);
    int    sy= as_int (t[2]);
    double fp= as_double (t[3]);
    double de= as_double (t[4]);
    return hatch (e, sx, sy, fp, de);
  }
  else if (is_compound (t, EFF_DOTS, 7)) {
    effect e = build_effect (t[0]);
    int    a = as_int (t[1]);
    int    b = as_int (t[2]);
    int    c = as_int (t[3]);
    int    d = as_int (t[4]);
    double fp= as_double (t[5]);
    double de= as_double (t[6]);
    return dots (e, a, b, c, d, fp, de);
  }
  else if (is_func (t, EFF_GAUSSIAN, 3)) {
    double rx= as_double (t[0]);
    double ry= as_double (t[1]);
    double a = as_double (t[2]);
    return gaussian_pen_effect (rx, ry, a);
  }
  else if (is_func (t, EFF_OVAL, 1)) {
    double r= as_double (t[0]);
    return oval_pen_effect (r);
  }
  else if (is_func (t, EFF_OVAL, 3)) {
    double rx= as_double (t[0]);
    double ry= as_double (t[1]);
    double a = as_double (t[2]);
    return oval_pen_effect (rx, ry, a);
  }
  else if (is_func (t, EFF_RECTANGULAR, 1)) {
    double r= as_double (t[0]);
    return rectangular_pen_effect (r);
  }
  else if (is_func (t, EFF_RECTANGULAR, 3)) {
    double rx= as_double (t[0]);
    double ry= as_double (t[1]);
    double a = as_double (t[2]);
    return rectangular_pen_effect (rx, ry, a);
  }
  else if (is_func (t, EFF_MOTION, 2)) {
    double dx= as_double (t[0]);
    double dy= as_double (t[1]);
    return motion_pen_effect (dx, dy);
  }
  else if (is_func (t, EFF_BLUR, 2)) {
    effect eff= build_effect (t[0]);
    effect pen= build_effect (t[1]);
    return blur (eff, pen);
  }
  else if (is_func (t, EFF_OUTLINE, 2)) {
    effect eff  = build_effect (t[0]);
    effect pen= build_effect (t[1]);
    return outline (eff, pen);
  }
  else if (is_func (t, EFF_THICKEN, 2)) {
    effect eff  = build_effect (t[0]);
    effect pen= build_effect (t[1]);
    return thicken (eff, pen);
  }
  else if (is_func (t, EFF_ERODE, 2)) {
    effect eff  = build_effect (t[0]);
    effect pen= build_effect (t[1]);
    return erode (eff, pen);
  }
  else if (is_func (t, EFF_DEGRADE, 5)) {
    effect eff= build_effect (t[0]);
    double wx = as_double (t[1]);
    double wy = as_double (t[2]);
    double th = as_double (t[3]);
    double sh = as_double (t[4]);
    return degrade (eff, wx, wy, th, sh);
  }
  else if (is_func (t, EFF_DISTORT, 5)) {
    effect eff= build_effect (t[0]);
    double wx = as_double (t[1]);
    double wy = as_double (t[2]);
    double rx = as_double (t[3]);
    double ry = as_double (t[4]);
    return distort (eff, wx, wy, rx, ry);
  }
  else if (is_func (t, EFF_GNAW, 5)) {
    effect eff= build_effect (t[0]);
    double wx = as_double (t[1]);
    double wy = as_double (t[2]);
    double rx = as_double (t[3]);
    double ry = as_double (t[4]);
    return gnaw (eff, wx, wy, rx, ry);
  }
  else if (is_func (t, EFF_SUPERPOSE)) {
    array<effect> effs= build_effects (A(t));
    return superpose (effs);
  }
  else if (is_func (t, EFF_ADD)) {
    array<effect> effs= build_effects (A(t));
    return add (effs);
  }
  else if (is_func (t, EFF_SUB)) {
    array<effect> effs= build_effects (A(t));
    return sub (effs);
  }
  else if (is_func (t, EFF_MUL)) {
    array<effect> effs= build_effects (A(t));
    return mul (effs);
  }
  else if (is_func (t, EFF_MIN)) {
    array<effect> effs= build_effects (A(t));
    return min (effs);
  }
  else if (is_func (t, EFF_MAX)) {
    array<effect> effs= build_effects (A(t));
    return max (effs);
  }
  else if (is_func (t, EFF_MIX, 4)) {
    effect eff1= build_effect (t[0]);
    double a1  = as_double (t[1]);
    effect eff2= build_effect (t[2]);
    double a2  = as_double (t[3]);
    return mix (eff1, a1, eff2, a2);
  }
  else if (is_func (t, EFF_NORMALIZE)) {
    effect eff= build_effect (t[0]);
    return normalize (eff);
  }
  else if (is_func (t, EFF_MONOCHROME, 3)) {
    effect eff= build_effect (t[0]);
    color  col= named_color (as_string (t[1]));
    double a  = as_double (t[2]);
    true_color c (col);
    array<double> m (20);
    for (int i=0; i<20; i++) m[i]= 0.0;
    m[0]= m[6]= m[12]= 1-a;
    m[4]= c.r; m[9]= c.g; m[14]= c.b;
    m[18]= 1-a + c.a * a;
    return color_matrix (eff, m);
  }
  else if (is_func (t, EFF_COLOR_MATRIX, 2)) {
    effect eff = build_effect (t[0]);
    matrix<double> m= as_matrix<double> (t[1]);
    if (NR (m) != 4 && NC (m) != 5) return argument_effect (0);
    array<double> v;
    for (int i=0; i<4; i++)
      for (int j=0; j<5; j++)
        v << m (i, j);
    return color_matrix (eff, v);
  }
  else if (is_func (t, EFF_GRADIENT, 3)) {
    effect eff= build_effect (t[0]);
    color  fgc= named_color (as_string (t[1]));
    color  bgc= named_color (as_string (t[2]));
    true_color fg (fgc);
    true_color bg (bgc);
    array<double> v;
    v << bg.r - fg.r << 0.0 << 0.0 << 0.0 << fg.r
      << 0.0 << bg.g - fg.g << 0.0 << 0.0 << fg.g
      << 0.0 << 0.0 << bg.b - fg.b << 0.0 << fg.b
      << 0.0 << 0.0 << 0.0 << 1.0 << 0.0;
    return color_matrix (eff, v);
  }
  else if (is_func (t, EFF_MAKE_TRANSPARENT)) {
    effect eff= build_effect (t[0]);
    color  bgc= named_color (as_string (t[1]));
    double thr= (N(t)<=2)? 1.0: as_double (t[2]);
    return make_transparent (eff, bgc, thr);
  }
  else if (is_func (t, EFF_MAKE_OPAQUE)) {
    effect eff= build_effect (t[0]);
    color  bgc= named_color (as_string (t[1]));
    return make_opaque (eff, bgc);
  }
  else if (is_func (t, EFF_RECOLOR)) {
    effect eff= build_effect (t[0]);
    color  col= named_color (as_string (t[1]));
    return recolor (eff, col);
  }
  else if (is_func (t, EFF_SKIN)) {
    effect eff= build_effect (t[0]);
    color  col= named_color (as_string (t[1]));
    return apply_skin (eff, col);
  }
  else if (is_compound (t, "eff-crop", 5)) {
    effect eff= build_effect (t[0]);
    double cx1= as_double (t[1]);
    double cy1= as_double (t[2]);
    double cx2= as_double (t[3]);
    double cy2= as_double (t[4]);
    return crop (eff, cx1, cy1, cx2, cy2);
  }
  else
    return argument_effect (0);
}
