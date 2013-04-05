
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
#include "true_color.hpp"

/******************************************************************************
* Picture kind associated to color type
******************************************************************************/

template<typename C>
struct picture_kind_helper {
  static picture_kind kind;
};

template<typename C> picture_kind
picture_kind_helper<C>::kind= picture_raster;

/******************************************************************************
* Raster picture class
******************************************************************************/

template<class C>
class raster_picture_rep: public picture_rep {
public:
  raster<C> r;

protected:
  color internal_get_pixel (int x, int y) {
    if (0 > x || 0 > y || x >= r->w || y >= r->h) return 0;
    else return (color) r->a [y*r->w + x]; }
  void internal_set_pixel (int x, int y, color c) {
    if (0 > x || 0 > y || x >= r->w || y >= r->h);
    else r->a [y*r->w + x]= C (c); }
  color internal_smooth_pixel (double x, double y) {
    return (color) r->internal_smooth_pixel (x, y); }

public:
  raster_picture_rep (raster<C> r2): r (r2) {}
  ~raster_picture_rep () {}

  picture_kind get_type () { return picture_kind_helper<C>::kind; }
  void* get_handle () { return (void*) this; }

  int get_width () { return r->w; }
  int get_height () { return r->h; }
  int get_origin_x () { return r->ox; }
  int get_origin_y () { return r->oy; }
  void set_origin (int ox2, int oy2) { r->ox= ox2; r->oy= oy2; }
};

template<class C> picture
raster_picture (raster<C> r) {
  return tm_new<raster_picture_rep<C> > (r);
}

template<class C> raster<C>
as_raster (picture pic) {
  if (pic->get_type () != picture_kind_helper<C>::kind) {
    picture rew= raster_picture (pic->get_width (), pic->get_height (),
                                 pic->get_origin_x (), pic->get_origin_y ());
    pic->copy_to (rew);
    pic= rew;
  }
  raster_picture_rep<C>* rep= (raster_picture_rep<C>*) pic->get_handle ();
  return rep->r;
}

#endif // defined RASTER_PICTURE_H
