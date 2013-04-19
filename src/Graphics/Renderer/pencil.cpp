
/******************************************************************************
* MODULE     : pencil.cpp
* DESCRIPTION: pencils for painting
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "pencil.hpp"

/******************************************************************************
* Simple pencils
******************************************************************************/

class simple_pencil_rep: public pencil_rep {
  color c;
  SI w;

public:
  simple_pencil_rep (color c2, SI w2): c (c2), w (w2) {}

  pencil_kind get_type () { return pencil_simple; }
  void* get_handle () { return (void*) this; }

  pencil set_width (SI nw) { return pencil (c, nw); }
  color get_color () { return c; }
  brush get_brush () { return brush (c); }
  SI get_width () { return w; }
  pencil_cap get_cap () { return cap_round; }
  pencil_join get_join () { return join_round; }
  double get_miter_lim () { return 2.0; }
};

pencil::pencil (color c, SI w):
  rep (tm_new<simple_pencil_rep> (c, w)) { INC_COUNT(rep); }

/******************************************************************************
* Standard and brushed pencils
******************************************************************************/

class complex_pencil_rep: public pencil_rep {
  pencil_kind k;
  brush br;
  SI w;
  pencil_cap cap;
  pencil_join join;
  double miter_lim;

public:
  complex_pencil_rep (pencil_kind k2, brush b, SI w2,
                      pencil_cap c, pencil_join j, double l):
    k (k2), br (b), w (w2), cap (c), join (j), miter_lim (l) {}

  pencil_kind get_type () { return k; }
  void* get_handle () { return (void*) this; }

  pencil set_width (SI nw) {
    if (k == pencil_brush) return pencil (br, nw, cap, join, miter_lim);
    else return pencil (br->get_color (), nw, cap, join, miter_lim); }
  color get_color () { return br->get_color (); }
  brush get_brush () { return br; }
  SI get_width () { return w; }
  pencil_cap get_cap () { return cap; }
  pencil_join get_join () { return join; }
  double get_miter_lim () { return miter_lim; }
};

pencil::pencil (brush br, SI w):
  rep (tm_new<complex_pencil_rep>
       (pencil_brush, br, w, cap_round, join_round, 2.0)) { INC_COUNT(rep); }
pencil::pencil (color col, SI w, pencil_cap c, pencil_join j, double l):
  rep (tm_new<complex_pencil_rep>
       (pencil_standard, col, w, c, j, l)) { INC_COUNT(rep); }
pencil::pencil (brush br, SI w, pencil_cap c, pencil_join j, double l):
  rep (tm_new<complex_pencil_rep>
       (pencil_brush, br, w, c, j, l)) { INC_COUNT(rep); }
