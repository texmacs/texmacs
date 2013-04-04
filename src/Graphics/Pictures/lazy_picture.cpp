
/******************************************************************************
* MODULE     : lazy_picture.cpp
* DESCRIPTION: Lazy loading of pictures
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "renderer.hpp"

class lazy_picture_rep: public picture_rep {
public:
  url name;
  int w, h;
  int ox, oy;
  picture p;

protected:
  inline void initialize () {
    if (is_nil (p)) {
      p= load_picture (name, w, h);
      p->set_origin (ox, oy);
    }
  }

  color internal_get_pixel (int x, int y) {
    initialize ();
    return p->internal_get_pixel (x, y);
  }

  void internal_set_pixel (int x, int y, color c) {
    initialize ();
    p->internal_set_pixel (x, y, c);
  }

  void internal_copy_from (int x, int y, picture src,
                           int x1, int y1, int x2, int y2) {
    initialize ();
    p->internal_copy_from (x, y, src, x1, y1, x2, y2);
  }

  void internal_copy_to (int x, int y, picture dest,
                         int x1, int y1, int x2, int y2) {
    initialize ();
    p->internal_copy_to (x, y, dest, x1, y1, x2, y2);
  }

public:
  lazy_picture_rep (url name2, int w2, int h2):
    name (name2), w (w2), h (h2), ox (0), oy (0) {}
  ~lazy_picture_rep () {}

  picture_kind get_type () { return picture_lazy; }
  void* get_handle () { return (void*) this; }
  url get_name () { return name; }
  picture non_lazy () { initialize (); return p; }

  int get_width () { return w; }
  int get_height () { return h; }
  int get_origin_x () { return 0; }
  int get_origin_y () { return 0; }

  void set_origin (int ox2, int oy2) {
    ox= ox2; oy= oy2;
    if (!is_nil (p)) p->set_origin (ox2, oy2);
  }
};

picture
lazy_picture (url name, int w, int h) {
  return tm_new<lazy_picture_rep> (name, w, h);
}
