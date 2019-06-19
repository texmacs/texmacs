
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

picture
mix (picture pic1, double a1, picture pic2, double a2) {
  raster<true_color> r1= as_raster<true_color> (pic1);
  raster<true_color> r2= as_raster<true_color> (pic2);
  return raster_picture (mix<true_color,double> (r1, a1, r2, a2));
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

picture
turbulence (picture pic, long seed, double w, double h, int oct) {
  raster<true_color> ras= as_raster<true_color> (pic);
  return raster_picture (turbulence (ras, seed, w, h, oct, false));  
}

picture
fractal_noise (picture pic, long seed, double w, double h, int oct) {
  raster<true_color> ras= as_raster<true_color> (pic);
  return raster_picture (turbulence (ras, seed, w, h, oct, true));
}

/******************************************************************************
* Special effects
******************************************************************************/

picture
gaussian_pen_picture (double rx, double ry, double phi) {
  raster<double> ras= gaussian_pen<double> (rx, ry, phi);
  return raster_picture (apply_alpha (true_color (0, 0, 0, 1), ras));
}

picture
gaussian_pen_picture (double r) {
  return gaussian_pen_picture (r, r, 0.0);
}

picture
oval_pen_picture (double rx, double ry, double phi) {
  raster<double> ras= oval_pen<double> (rx, ry, phi);
  return raster_picture (apply_alpha (true_color (0, 0, 0, 1), ras));
}

picture
oval_pen_picture (double r) {
  return oval_pen_picture (r, r, 0.0);
}

picture
rectangular_pen_picture (double rx, double ry, double phi) {
  raster<double> ras= rectangular_pen<double> (rx, ry, phi);
  return raster_picture (apply_alpha (true_color (0, 0, 0, 1), ras));
}

picture
rectangular_pen_picture (double r) {
  return rectangular_pen_picture (r, r, 0.0);
}

picture
motion_pen_picture (double dx, double dy) {
  raster<double> ras= motion_pen<double> (dx, dy);
  return raster_picture (apply_alpha (true_color (0, 0, 0, 1), ras));
}

/******************************************************************************
* Special effects, with pen as a parameter
******************************************************************************/

picture
blur (picture pic, picture pen) {
  raster<true_color> ras= as_raster<true_color> (pic);
  raster<double> alpha= get_alpha (as_raster<true_color> (pen));
  return raster_picture (blur (ras, alpha));
}

picture
outline (picture pic, picture pen) {
  raster<true_color> ras= as_raster<true_color> (pic);
  raster<double> alpha= get_alpha (as_raster<true_color> (pen));
  return raster_picture (variation (ras, alpha));
}

picture
thicken (picture pic, picture pen) {
  raster<true_color> ras= as_raster<true_color> (pic);
  raster<double> alpha= get_alpha (as_raster<true_color> (pen));
  return raster_picture (thicken (ras, alpha));
}

picture
erode (picture pic, picture pen) {
  raster<true_color> ras= as_raster<true_color> (pic);
  raster<double> alpha= get_alpha (as_raster<true_color> (pen));
  return raster_picture (erode (ras, alpha));
}

/******************************************************************************
* Effects that involve randomization
******************************************************************************/

picture
degrade (picture pic, double wx, double wy, double th, double sh) {
  raster<true_color> ras= as_raster<true_color> (pic);
  return raster_picture (degrade (ras, wx, wy, th, sh));
}

picture
distort (picture pic, double wx, double wy, double rx, double ry) {
  raster<true_color> ras= as_raster<true_color> (pic);
  return raster_picture (distort (ras, wx, wy, rx, ry));
}

picture
gnaw (picture pic, double wx, double wy, double rx, double ry) {
  raster<true_color> ras= as_raster<true_color> (pic);
  return raster_picture (gnaw (ras, wx, wy, rx, ry));
}

/******************************************************************************
* Color effects
******************************************************************************/

picture
normalize (picture pic) {
  raster<true_color> ras= as_raster<true_color> (pic);
  return raster_picture (normalize (ras));
}

picture
color_matrix (picture pic, array<double> m) {
  raster<true_color> ras= as_raster<true_color> (pic);
  return raster_picture (map (color_matrix_function (m), ras));
}

picture
make_transparent (picture pic, color bgc) {
  raster<true_color> ras= as_raster<true_color> (pic);
  true_color tbgc (bgc);
  return raster_picture (map (make_transparent_function (tbgc), ras));
}

picture
make_opaque (picture pic, color bgc) {
  raster<true_color> ras= as_raster<true_color> (pic);
  true_color tbgc (bgc);
  return raster_picture (map (make_opaque_function (tbgc), ras));
}

color
average_color (picture pic) {
  raster<true_color> tra= as_raster<true_color> (pic);
  return average (tra);
}

picture
copy_alpha (picture pic, picture alf) {
  raster<true_color> rpic= as_raster<true_color> (pic);
  raster<true_color> ralf= as_raster<true_color> (alf);
  return raster_picture (copy_alpha<true_color> (rpic, ralf));
}

picture
recolor (picture pic, color col) {
  color bg= average_color (pic);
  picture opa= make_opaque (pic, bg);
  picture tra= make_transparent (opa, bg);
  picture res= make_opaque (tra, col);
  return copy_alpha (res, pic);
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

/******************************************************************************
* Hatches
******************************************************************************/

raster<true_color>
hatch (int w, int h, int sx, int sy, double fill_prop, double deform) {
  raster<true_color> ret (w, h, 0, 0);
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++) {
      double t= ((double) (sx * y)) / ((double) h) -
                ((double) (sy * x)) / ((double) w);
      t += 0.5;
      double d= t - floor (t);
      double v= (min (d, 1.0-d) < fill_prop / 2.0 ? 1.0: 0.0);
      ret->a[y*w+x]= true_color (0.0, 0.0, 0.0, v);
    }
  return ret;
}

picture
hatch (picture pic, int sx, int sy, double fp, double deform) {
  double f= (deform > 0.0? 4.0: 8.0);
  raster<true_color> ras= as_raster<true_color> (pic);
  raster<true_color> hat= hatch (f*ras->w, f*ras->h, sx, sy, fp, deform);
  if (deform > 0.0) {
    double rx= hat->w * deform;
    double ry= hat->h * deform;
    hat= tiled_distort (hat, rx, ry);
  }
  return raster_picture (magnify (hat, 1.0/f, 1.0/f));
}

raster<true_color>
dots (int w, int h, int a, int b, int c, int d, double fp, double deform) {
  raster<true_color> ret (w, h, 0, 0);
  double det= a*d - b*c;
  double A= d/det, B= -b/det, C= -c/det, D= a/det;
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++) {
      double X = ((double) x) / w, Y = ((double) y) / h;
      double px= a*X + b*Y, py= c*X + d*Y;
      double fx= floor (px), fy= floor (py);
      double cx= ceil  (px), cy= ceil  (py);
      double x1= A*fx + B*fy, y1= C*fx + D*fy;
      double x2= A*fx + B*cy, y2= C*fx + D*cy;
      double x3= A*cx + B*fy, y3= C*cx + D*fy;
      double x4= A*cx + B*cy, y4= C*cx + D*cy;
      double d1= sqrt ((x1-X)*(x1-X) + (y1-Y)*(y1-Y));
      double d2= sqrt ((x2-X)*(x2-X) + (y2-Y)*(y2-Y));
      double d3= sqrt ((x3-X)*(x3-X) + (y3-Y)*(y3-Y));
      double d4= sqrt ((x4-X)*(x4-X) + (y4-Y)*(y4-Y));
      double d = min (min (d1, d2), min (d3, d4));
      double nd= d * sqrt (fabs (det));
      double v= (nd < fp ? 1.0: 0.0);
      ret->a[y*w+x]= true_color (0.0, 0.0, 0.0, v);
    }
  return ret;
}

picture
dots (picture pic, int a, int b, int c, int d, double fp, double deform) {
  double f= 4.0;
  raster<true_color> ras= as_raster<true_color> (pic);
  raster<true_color> hat= dots (f*ras->w, f*ras->h, a, b, c, d, fp, deform);
  if (deform > 0.0) {
    double rx= hat->w * deform;
    double ry= hat->h * deform;
    hat= tiled_distort (hat, rx, ry);
  }
  return raster_picture (magnify (hat, 1.0/f, 1.0/f));
}
