
/******************************************************************************
* MODULE     : raster_picture.cpp
* DESCRIPTION: Raster pictures
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "raster_picture.hpp"
#include "gui.hpp"

/******************************************************************************
* Constructor
******************************************************************************/

picture
raster_picture (int w, int h, int ox, int oy) {
  return raster_picture (raster<true_color> (w, h, ox, oy));
}

/******************************************************************************
* General composition
******************************************************************************/

void
draw_on (picture& pic, color c, composition_mode mode) {
  raster<true_color> ras= as_raster<true_color> (pic);
  draw_on (ras, true_color (c), mode);
}

picture
compose (picture pic, color c, composition_mode mode) {
  raster<true_color> ras= as_raster<true_color> (pic);
  return raster_picture (compose (ras, true_color (c), mode));
}

void
draw_on (picture& dest, picture src, int x, int y, composition_mode mode) {
  raster<true_color> dest_ras= as_raster<true_color> (dest);
  raster<true_color> src_ras = as_raster<true_color> (src);
  draw_on (dest_ras, src_ras, x, y, mode);
}

picture
compose (picture p1, picture p2, composition_mode mode) {
  raster<true_color> r1= as_raster<true_color> (p1);
  raster<true_color> r2= as_raster<true_color> (p2);
  raster<true_color> r = compose (r1, r2, mode);
  return raster_picture (r);
}

picture
compose (array<picture> ps, composition_mode mode) {
  array<raster<true_color> > rs (N(ps));
  for (int i=0; i<N(ps); i++)
    rs[i]= as_raster<true_color> (ps[i]);
  raster<true_color> r= compose (rs, mode);
  return raster_picture (r);
}

/******************************************************************************
* Coordinate transformations
******************************************************************************/

picture
shift (picture pic, double dx, double dy) {
  raster<true_color> ras= as_raster<true_color> (pic);
  return raster_picture (shift (ras, dx, dy));
}

picture
magnify (picture pic, double sx, double sy) {
  raster<true_color> ras= as_raster<true_color> (pic);
  return raster_picture (magnify (ras, sx, sy));
}

picture
bubble (picture pic, double r, double a) {
  raster<true_color> ras= as_raster<true_color> (pic);
  return raster_picture (bubble (ras, r, a));
}

/******************************************************************************
* Special effects
******************************************************************************/

picture
gaussian_brush_picture (double rx, double ry, double phi) {
  raster<double> ras= gaussian_brush<double> (rx, ry, phi);
  return raster_picture (apply_alpha (true_color (0, 0, 0, 1), ras));
}

picture
gaussian_brush_picture (double r) {
  return gaussian_brush_picture (r, r, 0.0);
}

picture
oval_brush_picture (double rx, double ry, double phi) {
  raster<double> ras= oval_brush<double> (rx, ry, phi);
  return raster_picture (apply_alpha (true_color (0, 0, 0, 1), ras));
}

picture
oval_brush_picture (double r) {
  return oval_brush_picture (r, r, 0.0);
}

picture
rectangular_brush_picture (double rx, double ry, double phi) {
  raster<double> ras= rectangular_brush<double> (rx, ry, phi);
  return raster_picture (apply_alpha (true_color (0, 0, 0, 1), ras));
}

picture
rectangular_brush_picture (double r) {
  return rectangular_brush_picture (r, r, 0.0);
}

/******************************************************************************
* Special effects, with brush as a parameter
******************************************************************************/

picture
blur (picture pic, picture brush) {
  raster<true_color> ras= as_raster<true_color> (pic);
  raster<double> alpha= get_alpha (as_raster<true_color> (brush));
  return raster_picture (blur (ras, alpha));
}

picture
outline (picture pic, picture brush) {
  raster<true_color> ras= as_raster<true_color> (pic);
  raster<double> alpha= get_alpha (as_raster<true_color> (brush));
  return raster_picture (variation (ras, alpha));
}

picture
thicken (picture pic, picture brush) {
  raster<true_color> ras= as_raster<true_color> (pic);
  raster<double> alpha= get_alpha (as_raster<true_color> (brush));
  return raster_picture (thicken (ras, alpha));
}

/******************************************************************************
* Special effects
******************************************************************************/

picture
inner_shadow (picture pic, color c, double dx, double dy) {
  true_color col (c);
  raster<true_color> r= as_raster<true_color> (pic);
  raster<double> alpha = get_alpha (r);
  raster<double> ashift= shift (alpha, dx, dy);
  raster<double> compat= change_extents (ashift, r->w, r->h, r->ox, r->oy);
  raster<double> cshift= ((double) 1) - compat;
  raster<double> shad  = min (alpha, cshift);
  raster<true_color> appl= apply_alpha (col, shad);
  return raster_picture (appl);
}

picture
engrave (picture pic, color c, double dx, double dy) {
  picture shad= inner_shadow (pic, c, dx, dy);
  return compose (pic, shad, compose_source_over);
}

picture
gravitational_outline (picture pic, int R, double expon) {
  raster<true_color> ras= as_raster<true_color> (pic);
  return raster_picture (gravitational_outline (ras, R, expon));
}

picture
gravitational_shadow (picture pic, color col, double phi) {
  raster<true_color> ras= as_raster<true_color> (pic);
  return raster_picture (gravitational_shadow (ras, 30, 10.0, col, phi));
}

picture
engrave (picture pic, double a0, color tlc, color brc, double tlw, double brw) {
  raster<true_color> ras= as_raster<true_color> (pic);
  int w= ras->w, h= ras->h, ox= ras->ox, oy= ras->oy;
  raster<double> tl= tl_distances (ras);
  raster<double> br= br_distances (ras);
  raster<true_color> ret (w, h, ox, oy);
  true_color c1 (tlc);
  true_color c2 (brc);
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++) {
      double d1= tl->a[y*w+x];
      double d2= br->a[y*w+x];
      double a1= 1.0 / (1.0 + d1*d1/(tlw*tlw));
      double a2= 1.0 / (1.0 + d2*d2/(brw*brw));
      true_color c0= ras->a[y*w+x];
      true_color cc (c0.r, c0.g, c0.b, a0);
      true_color mc= (a0 * cc + a1 * c1 + a2 * c2) / (a0 + a1 + a2);
      ret->a[y*w+x]= true_color (mc.r, mc.g, mc.b, c0.a * mc.a);
    }
  return raster_picture (ret);
}
