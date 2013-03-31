
/******************************************************************************
* MODULE     : raster_picture.hpp
* DESCRIPTION: Raster pictures
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef RASTER_PICTURE_H
#define RASTER_PICTURE_H
#include "raster.hpp"
#include "picture.hpp"

/******************************************************************************
* Raster picture class
******************************************************************************/

template<class C>
class raster_picture_rep: public picture_rep {
public:
  picture_kind kind;
  raster<C> r;

public:
  raster_picture_rep (picture_kind k2, raster<C> r2): kind (k2), r (r2) {}
  ~raster_picture_rep () {}

  picture_kind get_type () { return kind; }
  void* get_handle () { return (void*) this; }

  int get_width () { return r->w; }
  int get_height () { return r->h; }
  int get_origin_x () { return r->ox; }
  int get_origin_y () { return r->oy; }
  void set_origin (int ox2, int oy2) { r->ox= ox2; r->oy= oy2; }
  color get_pixel (int x, int y) { return r->get_pixel (x, y); }
  void set_pixel (int x, int y, color c) { r->set_pixel (x, y, c); }
  //color get_pixel (int x, int y) { return (color) r->get_pixel (x, y); }
  //void set_pixel (int x, int y, color c) { r->set_pixel (x, y, C (c)); }
};

template<class C> raster<C>
as_raster (picture pict) {
  return raster<C> ((raster_rep<C>*) pict->get_handle ());
}

#endif // defined RASTER_PICTURE_H
