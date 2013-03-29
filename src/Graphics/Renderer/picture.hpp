
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

enum picture_kind { picture_native, picture_raster };

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

  virtual color get_pixel (int x, int y) = 0;
  virtual void set_pixel (int x, int y, color c) = 0;

  void copy_from (int x, int y, picture p,
                  int x1, int y1, int x2, int y2);
  void copy_to   (int x, int y, picture p,
                  int x1, int y1, int x2, int y2);

  friend class picture;
};

class picture {
  ABSTRACT_NULL(picture);
  picture (url u);
  picture (int w, int h, int ox= 0, int oy= 0);
};
ABSTRACT_NULL_CODE(picture);

#endif // defined PICTURE_H
