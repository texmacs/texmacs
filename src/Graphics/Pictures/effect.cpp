
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

/******************************************************************************
* Unmodified argument
******************************************************************************/

class argument_effect_rep: public effect_rep {
  int nr;
public:
  argument_effect_rep (int nr2): nr (nr2) {}
  rectangle get_extents (array<rectangle> rs) { return rs[nr]; }
  picture apply (array<picture> pics, SI pixel) { return pics[nr]; }
};

effect argument_effect (int nr) {
  return tm_new<argument_effect_rep> (nr); }

/******************************************************************************
* Coordinate transformations
******************************************************************************/

class move_effect_rep: public effect_rep {
  effect eff;
  double dx, dy;
public:
  move_effect_rep (effect eff2, double dx2, double dy2):
    eff (eff2), dx (dx2), dy (dy2) {}
  rectangle get_extents (array<rectangle> rs) {
    rectangle r= eff->get_extents (rs);
    return rectangle (floor (r->x1 + dx), floor (r->y1 + dy),
                      ceil  (r->x2 + dx), ceil  (r->y2 + dy)); }
  picture apply (array<picture> pics, SI pixel) {
    return shift (eff->apply (pics, pixel), dx / pixel, dy / pixel); }
};

class magnify_effect_rep: public effect_rep {
  effect eff;
  double sx, sy;
public:
  magnify_effect_rep (effect eff2, double sx2, double sy2):
    eff (eff2), sx (sx2), sy (sy2) {}
  rectangle get_extents (array<rectangle> rs) {
    rectangle r= eff->get_extents (rs);
    return rectangle (floor (sx * r->x1), floor (sy * r->y1),
                      ceil  (sx * r->x2), ceil  (sy * r->y2)); }
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
    return rectangle (floor (rect->x1 - delta), floor (rect->y1 - delta),
                      ceil  (rect->x2 + delta), ceil  (rect->y2 + delta)); }
  picture apply (array<picture> pics, SI pixel) {
    return bubble (eff->apply (pics, pixel), r / pixel, a); }
};

effect move (effect eff, double dx, double dy) {
  return tm_new<move_effect_rep> (eff, dx, dy); }
effect magnify (effect eff, double sx, double sy) {
  return tm_new<move_effect_rep> (eff, sx, sy); }
effect bubble (effect eff, double r, double a) {
  return tm_new<bubble_effect_rep> (eff, r, a); }

/******************************************************************************
* Color transformations
******************************************************************************/

effect set_color (effect eff, color c, color mask);
effect make_transparent (effect eff, color bgc);
effect make_opaque (effect eff, color bgc);
effect change_color (effect eff, color what, color by);

/******************************************************************************
* Brushes
******************************************************************************/

class gaussian_brush_effect_rep: public effect_rep {
  double rx, ry, phi;
public:
  gaussian_brush_effect_rep (double rx2, double ry2, double phi2):
    rx (rx2), ry (ry2), phi (phi2) {}
  rectangle get_extents (array<rectangle> rs) { (void) rs;
    SI R= ceil (2.5 * max (rx, ry));
    return rectangle (-R, -R, R, R); }
  picture apply (array<picture> pics, SI pixel) { (void) pics;
    return gaussian_brush_picture (rx / pixel, ry / pixel, phi); }
};

class oval_brush_effect_rep: public effect_rep {
  double rx, ry, phi;
public:
  oval_brush_effect_rep (double rx2, double ry2, double phi2):
    rx (rx2), ry (ry2), phi (phi2) {}
  rectangle get_extents (array<rectangle> rs) { (void) rs;
    SI R= max (rx, ry);
    return rectangle (-R, -R, R, R); }
  picture apply (array<picture> pics, SI pixel) { (void) pics;
    return oval_brush_picture (rx / pixel, ry / pixel, phi); }
};

class rectangular_brush_effect_rep: public effect_rep {
  double rx, ry, phi;
public:
  rectangular_brush_effect_rep (double rx2, double ry2, double p2):
    rx (rx2), ry (ry2), phi (p2) {}
  rectangle get_extents (array<rectangle> rs) { (void) rs;
    SI R= sqrt (rx * rx + ry * ry);
    return rectangle (-R, -R, R, R); }
  picture apply (array<picture> pics, SI pixel) { (void) pics;
    return rectangular_brush_picture (rx / pixel, ry / pixel, phi); }
};

effect gaussian_brush_effect (double r) {
  return tm_new<gaussian_brush_effect_rep> (r, r, 0.0); }
effect oval_brush_effect (double r) {
  return tm_new<oval_brush_effect_rep> (r, r, 0.0); }
effect rectangular_brush_effect (double r) {
  return tm_new<rectangular_brush_effect_rep> (r, r, 0.0); }

effect gaussian_brush_effect (double rx, double ry, double phi) {
  return tm_new<gaussian_brush_effect_rep> (rx, ry, phi); }
effect oval_brush_effect (double rx, double ry, double phi) {
  return tm_new<oval_brush_effect_rep> (rx, ry, phi); }
effect rectangular_brush_effect (double rx, double ry, double phi) {
  return tm_new<rectangular_brush_effect_rep> (rx, ry, phi); }

/******************************************************************************
* Special effects, taking a brush as parameter
******************************************************************************/

class blur_effect_rep: public effect_rep {
  effect eff, brush;
public:
  blur_effect_rep (effect eff2, effect brush2):
    eff (eff2), brush (brush2) {}
  rectangle get_extents (array<rectangle> rs) {
    rectangle r1= eff->get_extents (rs);
    rectangle r2= brush->get_extents (rs);
    return rectangle (r1->x1 + r2->x1, r1->y1 + r2->y1,
                      r1->x2 + r2->x2, r1->y2 + r2->y2); }
  picture apply (array<picture> pics, SI pixel) {
    picture p1= eff->apply (pics, pixel);
    picture p2= brush->apply (pics, pixel);
    return blur (p1, p2); }
};

class outline_effect_rep: public effect_rep {
  effect eff, brush;
public:
  outline_effect_rep (effect eff2, effect brush2):
    eff (eff2), brush (brush2) {}
  rectangle get_extents (array<rectangle> rs) {
    rectangle r1= eff->get_extents (rs);
    rectangle r2= brush->get_extents (rs);
    return rectangle (r1->x1 + r2->x1, r1->y1 + r2->y1,
                      r1->x2 + r2->x2, r1->y2 + r2->y2); }
  picture apply (array<picture> pics, SI pixel) {
    picture p1= eff->apply (pics, pixel);
    picture p2= brush->apply (pics, pixel);
    return outline (p1, p2); }
};

class thicken_effect_rep: public effect_rep {
  effect eff, brush;
public:
  thicken_effect_rep (effect eff2, effect brush2):
    eff (eff2), brush (brush2) {}
  rectangle get_extents (array<rectangle> rs) {
    rectangle r1= eff->get_extents (rs);
    rectangle r2= brush->get_extents (rs);
    return rectangle (r1->x1 + r2->x1, r1->y1 + r2->y1,
                      r1->x2 + r2->x2, r1->y2 + r2->y2); }
  picture apply (array<picture> pics, SI pixel) {
    picture p1= eff->apply (pics, pixel);
    picture p2= brush->apply (pics, pixel);
    return thicken (p1, p2); }
};

effect blur (effect eff, effect brush) {
  return tm_new<blur_effect_rep> (eff, brush); }
effect outline (effect eff, effect brush) {
  return tm_new<outline_effect_rep> (eff, brush); }
effect thicken (effect eff, effect brush) {
  return tm_new<thicken_effect_rep> (eff, brush); }

/******************************************************************************
* Building effects from tree description
******************************************************************************/

effect
build_effect (tree t) {
  if (is_int (t)) {
    return argument_effect (as_int (t));
  }
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
    return gaussian_brush_effect (r);
  }
  else if (is_func (t, EFF_GAUSSIAN, 3)) {
    double rx= as_double (t[0]);
    double ry= as_double (t[1]);
    double a = as_double (t[2]);
    return gaussian_brush_effect (rx, ry, a);
  }
  else if (is_func (t, EFF_OVAL, 1)) {
    double r= as_double (t[0]);
    return oval_brush_effect (r);
  }
  else if (is_func (t, EFF_OVAL, 3)) {
    double rx= as_double (t[0]);
    double ry= as_double (t[1]);
    double a = as_double (t[2]);
    return oval_brush_effect (rx, ry, a);
  }
  else if (is_func (t, EFF_RECTANGULAR, 1)) {
    double r= as_double (t[0]);
    return rectangular_brush_effect (r);
  }
  else if (is_func (t, EFF_RECTANGULAR, 3)) {
    double rx= as_double (t[0]);
    double ry= as_double (t[1]);
    double a = as_double (t[2]);
    return rectangular_brush_effect (rx, ry, a);
  }
  else if (is_func (t, EFF_BLUR, 2)) {
    effect eff  = build_effect (t[0]);
    effect brush= build_effect (t[1]);
    return blur (eff, brush);
  }
  else if (is_func (t, EFF_OUTLINE, 2)) {
    effect eff  = build_effect (t[0]);
    effect brush= build_effect (t[1]);
    return outline (eff, brush);
  }
  else if (is_func (t, EFF_THICKEN, 2)) {
    effect eff  = build_effect (t[0]);
    effect brush= build_effect (t[1]);
    return thicken (eff, brush);
  }
  else {
    return argument_effect (0);
  }
}
