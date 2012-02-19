
/******************************************************************************
* MODULE     : ink_widget.cpp
* DESCRIPTION: Widget for inking
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Widkit/attribute_widget.hpp"
#include "Widkit/layout.hpp"

/******************************************************************************
* Ink widget
******************************************************************************/

class ink_widget_rep: public attribute_widget_rep {

public:
  ink_widget_rep ();
  operator tree ();
  void handle_get_size (get_size_event ev);
  void handle_repaint (repaint_event ev);
  void handle_mouse (mouse_event ev);
};

/******************************************************************************
* Routines for ink_widgets
******************************************************************************/

ink_widget_rep::ink_widget_rep ():
  attribute_widget_rep () {}

ink_widget_rep::operator tree () {
  return tree (TUPLE, "ink");
}

void
ink_widget_rep::handle_get_size (get_size_event ev) {
  ev->w= 400 * PIXEL;
  ev->h= 300 * PIXEL;
  abs_round (ev->w, ev->h);
}

void
ink_widget_rep::handle_repaint (repaint_event ev) { (void) ev;
  renderer ren= win->get_renderer ();
  layout_pastel_lower (ren, ev->x1, ev->y1, ev->x2, ev->y2);
}

void
ink_widget_rep::handle_mouse (mouse_event ev) {
  (void) ev;
}

/******************************************************************************
* Interface
******************************************************************************/

wk_widget
ink_wk_widget () {
  return tm_new<ink_widget_rep> ();
}
