
/******************************************************************************
* MODULE     : tm_button.cpp
* DESCRIPTION: Text widgets for output only
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Widget/basic_widget.hpp"
#include "boxes.hpp"
#include "Boxes/construct.hpp"
#include "font.hpp"
#include "window.hpp"
#include "tm_layout.hpp"

void layout_default (window win, SI x1, SI y1, SI x2, SI y2);

/******************************************************************************
* Text widgets
******************************************************************************/

class box_widget_rep: public basic_widget_rep {
  box  b;
  bool transparent;
  SI   X1, Y1, X2, Y2;
  int  dw, dh;

public:
  box_widget_rep (display dis, box b, bool trans, int dw, int dh);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_repaint (repaint_event ev);
};

box_widget_rep::box_widget_rep (
  display dis, box b2, bool trans2, int dw2, int dh2):
    basic_widget_rep (dis, south_west), b (b2),
    transparent (trans2), dw (dw2+2*PIXEL), dh (dh2+2*PIXEL) {}

box_widget_rep::operator tree () {
  return tree (TUPLE, "box", (tree) b);
}

#define SHRINK 6

void
box_widget_rep::handle_get_size (get_size_event ev) {
  X1= b->x1; Y1= b->y1;
  X2= b->x2; Y2= b->y2;
  ev->w = ((X2- X1+ SHRINK- 1)/SHRINK)+ 2*dw;
  ev->h = ((Y2- Y1+ SHRINK- 1)/SHRINK)+ 2*dh;
  abs_round (ev->w, ev->h);
}

void
box_widget_rep::handle_repaint (repaint_event ev) { (void) ev;
  if (!transparent) layout_default (win, 0, 0, w, h);
  win->set_shrinking_factor (SHRINK);
  rectangles l (rectangle (0, 0, w, h));
  SI x= ((SHRINK*w-b->w())>>1) - b->x1;
  SI y= ((SHRINK*h-b->h())>>1) - b->y1;
  b->redraw (win, path(), l, x, y);
  win->set_shrinking_factor (1);
}

/******************************************************************************
* Interface
******************************************************************************/

widget
box_widget (box b, bool trans) {
  return new box_widget_rep (current_display (), b, trans, 3*PIXEL, 3*PIXEL);
}

widget
box_widget (scheme_tree p, string s, color col, bool trans, bool ink) {
  string family  = "roman";
  string fn_class= "mr";
  string series  = "medium";
  string shape   = "normal";
  int    sz      = 10;
  int    dpi     = 600;
  int    n       = arity (p);
  if ((n >= 1) && is_atomic (p[0])) family  = as_string (p[0]);
  if ((n >= 2) && is_atomic (p[1])) fn_class= as_string (p[1]);
  if ((n >= 3) && is_atomic (p[2])) series  = as_string (p[2]);
  if ((n >= 4) && is_atomic (p[3])) shape   = as_string (p[3]);
  if ((n >= 5) && is_atomic (p[4])) sz      = as_int (p[4]);
  if ((n >= 6) && is_atomic (p[5])) dpi     = as_int (p[5]);
  display dis= current_display ();
  font fn= find_font (dis, family, fn_class, series, shape, sz, dpi);
  box  b = text_box (decorate (), 0, s, fn, col);
  if (ink) b= resize_box (decorate (), b, b->x3, b->y3, b->x4, b->y4, true);
  return box_widget (b, trans);
}
