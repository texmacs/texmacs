
/******************************************************************************
* MODULE     : canvas_properties.hpp
* DESCRIPTION: Container for canvas properties
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
  color    bg;
  color    sunny;
  color    shadow;
  SI       bar_width;
  SI       bar_padding;
  color    bar_bg;
  color    bar_button;

public:
  inline canvas_properties_rep () {}
};

class canvas_properties {
CONCRETE(canvas_properties);
  inline canvas_properties (): rep (new canvas_properties_rep ()) {}
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
