
/******************************************************************************
* MODULE     : mupdf_picture.hpp
* DESCRIPTION: Picture objects for MuPDF
* COPYRIGHT  : (C) 2022 Massimiliano Gubinelli, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef MUPDF_PICTURE_HPP
#define MUPDF_PICTURE_HPP

#include "mupdf_renderer.hpp"

#include <mupdf/fitz.h>

// in fitz pixmaps are stored in rgba premultiplied format while our color type
// is argb not premultiplied...
// we code for a little endian system

inline color argb_to_rgbap (color c) {
  int r, g, b, a;
  get_rgb_color(c, r, g, b, a);
  r= ((r*a)/255) & 0xFF;
  g= ((g*a)/255) & 0xFF;
  b= ((b*a)/255) & 0xFF;
  return (a << 24) + (b << 16) + (g << 8) + r;
}

inline color rgbap_to_argb (color c) {
  int r, g, b, a;
  a= (c >> 24) & 0xFF;
  b= (c >> 16) & 0xFF;
  g= (c >>  8) & 0xFF;
  r= (c >>  0) & 0xFF;
  if (a) {
    r= ((r*255)/a) & 0xFF;
    g= ((g*255)/a) & 0xFF;
    b= ((b*255)/a) & 0xFF;
  } else {
    r = g = b = 0;
  }
  return rgb_color(r, g, b, a);
}

class mupdf_picture_rep: public picture_rep {
public:
  fz_pixmap *pix;
  fz_image *im;
  int w, h;
  int ox, oy;

protected:
  color internal_get_pixel (int x, int y);
  void internal_set_pixel (int x, int y, color c);

public:
  mupdf_picture_rep (fz_pixmap *_pix, int ox2, int oy2);
  ~mupdf_picture_rep ();

  picture_kind get_type ();
  void* get_handle ();
  int get_width ();
  int get_height ();
  int get_origin_x ();
  int get_origin_y ();
  void set_origin (int ox2, int oy2);
};

picture mupdf_picture (fz_pixmap *im, int ox, int oy);
picture as_mupdf_picture (picture pic);

fz_image  *mupdf_load_image (url u);
fz_pixmap *mupdf_load_pixmap (url u, int w, int h, tree eff, SI pixel);

#endif // defined MUPDF_PICTURE_HPP
