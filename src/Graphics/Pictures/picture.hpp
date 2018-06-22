
/******************************************************************************
* MODULE     : picture.hpp
* DESCRIPTION: Abstract graphical pictures
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PICTURE_H
#define PICTURE_H
#include "url.hpp"

/******************************************************************************
* The abstract picture class
******************************************************************************/

enum picture_kind {
  picture_native,
  picture_raster,
  picture_lazy };

class picture_rep;
class picture {
ABSTRACT_NULL(picture);
};

class picture_rep: public abstract_struct {
protected:
  virtual color internal_smooth_pixel (double x, double y);
  virtual color internal_get_pixel (int x, int y) = 0;
  virtual void internal_set_pixel (int x, int y, color c) = 0;
  virtual void internal_copy_from (int x, int y, picture src,
                                   int x1, int y1, int x2, int y2);
  virtual void internal_copy_to   (int x, int y, picture dest,
                                   int x1, int y1, int x2, int y2);

public:
  inline picture_rep () {}
  inline virtual ~picture_rep () {}

  virtual picture_kind get_type () = 0;
  virtual void* get_handle () = 0;
  virtual url get_name ();

  virtual int get_width () = 0;
  virtual int get_height () = 0;
  virtual int get_origin_x () = 0;
  virtual int get_origin_y () = 0;
  virtual void set_origin (int ox, int oy) = 0;
  virtual void translate_origin (int dx, int dy);

  inline color smooth_pixel (double x, double y) {
    return internal_smooth_pixel (x + get_origin_x (), y + get_origin_y ()); }
  inline color get_pixel (int x, int y) {
    return internal_get_pixel (x + get_origin_x (), y + get_origin_y ()); }
  inline void  set_pixel (int x, int y, color c) {
    internal_set_pixel (x + get_origin_x (), y + get_origin_y (), c); }
  inline void copy_from (picture s) {
    internal_copy_from (0, 0, s, 0, 0, s->get_width (), s->get_height ()); }
  inline void copy_to (picture d) {
    internal_copy_to (0, 0, d, 0, 0, get_width (), get_height ()); }

  friend class picture;
};

ABSTRACT_NULL_CODE(picture);

/******************************************************************************
* Pictures on disk
******************************************************************************/

picture load_picture (url u, int w, int h);
picture load_xpm (url file_name);
void picture_cache_reserve (url u, int w, int h);
void picture_cache_release (url u, int w, int h);
void picture_cache_clean ();
picture cached_load_picture (url u, int w, int h, bool permanent= true);
string picture_as_eps (picture pic, int dpi);

/******************************************************************************
* Drawing on pictures and combining pictures
******************************************************************************/

enum composition_mode {
  compose_destination,
  compose_source,
  compose_source_over,
  compose_towards_source,
  compose_alpha_distance,
  compose_add,
  compose_sub,
  compose_mul,
  compose_min,
  compose_max
};

picture native_picture (int w, int h, int ox, int oy);
picture raster_picture (int w, int h, int ox= 0, int oy= 0);
picture as_raster_picture (picture pict);
picture error_picture (int w, int h);
picture as_native_picture (picture pict);

int     composition_type (composition_mode mode);
void    draw_on (picture& pic, color c, composition_mode mode);
picture compose (picture pic, color c, composition_mode mode);
void    draw_on (picture& d, picture s, int x, int y, composition_mode m);
picture compose (picture p1, picture p2, composition_mode mode);
picture compose (array<picture> ps, composition_mode mode);
picture mix (picture pic1, double a1, picture pic2, double a2);

/******************************************************************************
* Operations on pictures
******************************************************************************/

picture shift (picture pic, double dx, double dy);
picture magnify (picture pic, double sx, double sy);
picture bubble (picture pic, double r, double a);
picture turbulence (picture pic, long seed, double w, double h, int oct);
picture fractal_noise (picture pic, long seed, double w, double h, int oct);
picture hatch (picture pic, int sx, int sy, double fill_prop);

picture gaussian_pen_picture (double r);
picture oval_pen_picture (double r);
picture rectangular_pen_picture (double r);
picture gaussian_pen_picture (double rx, double ry, double phi= 0.0);
picture oval_pen_picture (double rx, double ry, double phi= 0.0);
picture rectangular_pen_picture (double rx, double ry, double phi= 0.0);
picture motion_pen_picture (double dx, double dy);

picture blur (picture pic, picture pen);
picture outline (picture pic, picture pen);
picture thicken (picture pic, picture pen);
picture erode (picture pic, picture pen);

picture degrade (picture pic, double wx, double wy, double th, double sh);
picture distort (picture pic, double wx, double wy, double rx, double ry);
picture gnaw (picture pic, double wx, double wy, double rx, double ry);

picture normalize (picture eff);
picture color_matrix (picture eff, array<double> m);
picture make_transparent (picture eff, color bgc);
picture make_opaque (picture eff, color bgc);

#endif // defined PICTURE_H
