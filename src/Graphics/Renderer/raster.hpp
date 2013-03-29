
/******************************************************************************
* MODULE     : raster.hpp
* DESCRIPTION: Raster pictures
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef RASTER_H
#define RASTER_H
#include "picture.hpp"

template<class C> class raster;

template<class C>
class raster_rep: public picture_rep {
public:
  picture_kind kind;
  int w, h;
  int ox, oy;
  C* a;

public:
  raster_rep (picture_kind kind2, int w2, int h2, int ox2, int oy2):
    kind (kind2), w (w2), h (h2), ox (ox2), oy (oy2), a (NULL) {
      if (w * h != 0) a= tm_new_array<C> (w * h); }
  ~raster_rep () { if (w * h != 0) tm_delete_array (a); }

  picture_kind get_type () { return picture_raster; }
  void* get_handle () { return (void*) this; }

  int get_width () { return w; }
  int get_height () { return h; }
  int get_origin_x () { return ox; }
  int get_origin_y () { return oy; }

  color get_pixel (int x, int y) {
    x -= ox; y -= oy;
    if (0 > x || 0 > y || x >= w || y >= h) return 0;
    else return (color) (a [y*w + x]);
  }

  void set_pixel (int x, int y, color c) {
    x -= ox; y -= oy;
    if (0 > x || 0 > y || x >= w || y >= h);
    else a [y*w + x]= C (c);
  }

  friend class raster<C>;
};

#endif // defined RASTER_H
