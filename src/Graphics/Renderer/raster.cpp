
/******************************************************************************
* MODULE     : raster.cpp
* DESCRIPTION: Raster pictures
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "alpha_color.hpp"
#include "raster.hpp"

picture
raster_picture (int w, int h, int ox, int oy) {
  return tm_new<raster_rep<true_color> > (picture_raster, w, h, ox, oy);
}

picture
alpha_picture (int w, int h, int ox, int oy) {
  return tm_new<raster_rep<alpha_color> > (picture_alpha, w, h, ox, oy);
}

picture
as_raster_picture (picture pic) {
  if (pic->get_type () == picture_raster) return pic;
  picture ret= raster_picture (pic->get_width (), pic->get_height (),
                               pic->get_origin_x (), pic->get_origin_y ());
  pic->copy_to (ret);
  return ret;
}

picture
test_effect (picture pic) {
  int w= pic->get_width (), h= pic->get_height ();
  int ox= pic->get_origin_x (), oy= pic->get_origin_y ();
  picture ret= raster_picture (w, h, ox, oy);
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++) {
      true_color c= pic->get_pixel (x, y);
      true_color n (c.r, c.g, c.g, (c.a * x) / (w - 1));
      ret->set_pixel (x, y, n);
    }
  return ret;
}

true_color*
get_raster (picture pic) {
  typedef raster_rep<true_color> R;
  R* handle= (R*) pic->get_handle ();
  return handle->a;
}

picture
blur (picture orig, float r) {
  picture pic= as_raster_picture (orig);
  int R= ((int) (3.0 * r));
  int w= pic->get_width (), h= pic->get_height ();
  int ox= pic->get_origin_x (), oy= pic->get_origin_y ();
  picture ret= raster_picture (w + 2*R, h + 2*R, ox + R, oy + R);
  blur<true_color, alpha_color> (get_raster (ret), get_raster (pic), w, h, R, r);
  return ret;
}
