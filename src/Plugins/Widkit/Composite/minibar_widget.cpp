
/******************************************************************************
* MODULE     : minibar_widget.cpp
* DESCRIPTION: Simple transparent buttons
* COPYRIGHT  : (C) 2010  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "window.hpp"
#include "Widkit/attribute_widget.hpp"
#include "Widkit/layout.hpp"

/******************************************************************************
* Balloon widgets
******************************************************************************/

class minibar_widget_rep: public attribute_widget_rep {
public:
  minibar_widget_rep (wk_widget w);
  operator tree ();
  void handle_get_size (get_size_event ev);
  void handle_position (position_event ev);
  void handle_repaint (repaint_event ev);
  bool handle (event ev);
};

/******************************************************************************
* Implementation of balloon widgets
******************************************************************************/

minibar_widget_rep::minibar_widget_rep (wk_widget w):
  attribute_widget_rep (1, south_west)
{
  a[0]= w;
}

minibar_widget_rep::operator tree () {
  return tree (TUPLE, "transparent button", (tree) a[0]);
}

void
minibar_widget_rep::handle_get_size (get_size_event ev) {
  attribute_widget_rep::handle_get_size (ev);
  ev->w += 2*PIXEL;
  ev->h += 2*PIXEL;
}

void
minibar_widget_rep::handle_position (position_event ev) {
  (event) ev;
  SI ww= w-2*PIXEL;
  SI hh= h-2*PIXEL;
  a[0] << emit_position (PIXEL, PIXEL, ww, hh, south_west);
}

void
minibar_widget_rep::handle_repaint (repaint_event ev) {
  (void) ev;
  renderer ren= win->get_renderer ();
  layout_higher (ren, 0, 0, w, h);
}

bool
minibar_widget_rep::handle (event ev) {
  switch (ev->type) {
  case MOUSE_EVENT:
  case GET_SIZE_EVENT:
  case SET_WIDGET_EVENT:
  case ATTACH_WINDOW_EVENT:
  case POSITION_EVENT:
  case UPDATE_EVENT:
  case INVALIDATE_EVENT:
  case REPAINT_EVENT:
  case FIND_CHILD_EVENT:
    return attribute_widget_rep::handle (ev);
  default:
    a[0] << ev;
    return true;
  }
}

/******************************************************************************
* exported routines
******************************************************************************/

wk_widget
minibar_widget (wk_widget w) {
  return tm_new<minibar_widget_rep> (w);
}
