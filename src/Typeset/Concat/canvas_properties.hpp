
/******************************************************************************
* MODULE     : canvas_properties.hpp
* DESCRIPTION: Container for canvas properties
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef CANVAS_PROPERTIES_H
#define CANVAS_PROPERTIES_H
#include "env.hpp"

class canvas_properties_rep: public concrete_struct {
public:
  edit_env env;
  string   type;
  tree     x1, y1, x2, y2;
  tree     scx, scy;
  tree     xt, yt;
  SI       hpadding;
  SI       vpadding;
  SI       border;
  tree     bg;
  int      alpha;
  color    sunny;
  color    shadow;
  SI       bar_width;
  SI       bar_padding;
  tree     bar_bg;
  tree     bar_button;

public:
  inline canvas_properties_rep () {}
};

class canvas_properties {
CONCRETE(canvas_properties);
  inline canvas_properties (): rep (tm_new<canvas_properties_rep> ()) {}
};
CONCRETE_CODE(canvas_properties);

canvas_properties get_canvas_properties (edit_env env, tree t);
void get_canvas_horizontal (canvas_properties props,
			    SI bx1, SI bx2, SI& x1, SI& x2, SI& scx);
void get_canvas_vertical (canvas_properties props,
			  SI by1, SI by2, SI& y1, SI& y2, SI& scy);
box put_scroll_bars (canvas_properties props, box b, path ip,
		     box inner, SI scx, SI scy);

#endif // defined CANVAS_PROPERTIES_H
