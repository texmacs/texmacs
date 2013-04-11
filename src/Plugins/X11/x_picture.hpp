
/******************************************************************************
* MODULE     : x_picture.hpp
* DESCRIPTION: X pictures
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef X_PICTURE_HPP
#define X_PICTURE_HPP

#include "x_drawable.hpp"

class x_picture_rep: public picture_rep {
public:
  Pixmap pm;
  XImage *im;
  Pixmap bm;
  char   *data;
  int    w, h;
  int    ox, oy;
  bool   ok;

protected:
  color internal_get_pixel (int x, int y);
  void internal_set_pixel (int x, int y, color c);

public:
  x_picture_rep (Pixmap pm2, int w2, int h2, int ox2, int oy2);
  ~x_picture_rep ();
  void force_mask ();
  picture_kind get_type ();
  void* get_handle ();
  int get_width ();
  int get_height ();
  int get_origin_x ();
  int get_origin_y ();
  void set_origin (int ox2, int oy2);
};

picture x_picture (Pixmap pm, int w, int h, int ox, int oy);
picture as_x_picture (picture pic);
Pixmap retrieve_pixmap (picture pic);
Pixmap retrieve_bitmap (picture pic);

class x_image_renderer_rep: public x_drawable_rep {
public:
  picture pict;
  int x1, y1, x2, y2;
  
public:
  x_image_renderer_rep (picture pict, double zoom);
  ~x_image_renderer_rep ();
  void* get_data_handle ();
};

#endif // defined X_PICTURE_HPP
