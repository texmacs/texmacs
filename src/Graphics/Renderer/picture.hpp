
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

class picture;
class picture_rep: public abstract_struct {
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

  virtual color get_pixel (int x, int y) = 0;
  virtual void set_pixel (int x, int y, color c) = 0;

  void copy_from (picture src);
  void copy_to   (picture dest);
  virtual void copy_from (int x, int y, picture src,
                          int x1, int y1, int x2, int y2);
  virtual void copy_to   (int x, int y, picture dest,
                          int x1, int y1, int x2, int y2);

  friend class picture;
};

class picture {
ABSTRACT_NULL(picture);
};
ABSTRACT_NULL_CODE(picture);

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
picture alpha_picture (int w, int h, int ox= 0, int oy= 0);
picture as_raster_picture (picture pict);

picture test_effect (picture pic);
picture blur (picture pic, float r);
picture compose (picture pic, color c, composition_mode mode);
picture combine (picture p1, picture p2, composition_mode mode);
picture shadow (picture pic, int x, int y, color c, float r);
picture engrave (picture src, color c);

#endif // defined PICTURE_H
