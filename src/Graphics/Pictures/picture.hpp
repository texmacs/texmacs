
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

typedef unsigned int color;

/******************************************************************************
* The abstract picture class
******************************************************************************/

enum picture_kind { picture_native, picture_raster, picture_alpha };

class picture_rep;
class picture {
ABSTRACT_NULL(picture);
};

class picture_rep: public abstract_struct {
protected:
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

  virtual int get_width () = 0;
  virtual int get_height () = 0;
  virtual int get_origin_x () = 0;
  virtual int get_origin_y () = 0;
  virtual void set_origin (int ox, int oy) = 0;
  virtual void translate_origin (int dx, int dy);

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

picture load_xpm (url file_name);
string picture_as_eps (picture pic, int dpi);

/******************************************************************************
* Operations on pictures
******************************************************************************/

enum composition_mode {
  compose_destination,
  compose_source,
  compose_source_over,
  compose_towards_source
};

picture raster_picture (int w, int h, int ox= 0, int oy= 0);
picture as_raster_picture (picture pict);
picture error_picture (int w, int h);
picture as_native_picture (picture pict);

picture blur (picture pic, double r);
picture gravitational_outline (picture pic, int R, double expon= 2.0);
picture gravitational_shadow (picture pic, color col, double alpha);
void    draw_on (picture& pic, color c, composition_mode mode);
picture compose (picture pic, color c, composition_mode mode);
void    draw_on (picture& d, picture s, int x, int y, composition_mode m);
picture combine (picture p1, picture p2, composition_mode mode);
picture add_shadow (picture pic, int x, int y, color c, double r);
picture engrave (picture src, double alpha, color tlc, color brc,
                 double tlw=1.0, double brw= 1.0);

#endif // defined PICTURE_H
