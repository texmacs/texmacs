
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
draw_on (picture pic, color c, composition_mode mode) {
  raster<true_color> ras= as_raster<true_color> (pic);
  draw_on (ras, c, mode);
}

picture
compose (picture pic, color c, composition_mode mode) {
  raster<true_color> ras= compose (as_raster<true_color> (pic), c, mode);
  return raster_picture (ras);
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

/******************************************************************************
* Special effects
******************************************************************************/

picture
blur (picture pic, double r) {
  if (r <= 0.001) return pic;
  int R= max (3, ((int) (3.0 * r)));
  raster<true_color> ras= as_raster<true_color> (pic);
  return raster_picture (blur (ras, R, r));
}

picture
gravitational_outline (picture pic, int R, double expon) {
  raster<true_color> ras= as_raster<true_color> (pic);
  return raster_picture (gravitational_outline (ras, R, expon));
}

picture
add_shadow (picture pic, int x, int y, color c, double r) {
  picture shad= blur (compose (pic, c, compose_towards_source), r);
  shad->translate_origin (-x, -y);
  return compose (shad, pic, compose_source_over);
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
