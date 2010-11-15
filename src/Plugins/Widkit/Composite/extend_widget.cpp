
/******************************************************************************
* MODULE     : extend_widget.cpp
* DESCRIPTION: Size extension to the maximum size of a list of widgets
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

class extend_widget_rep: public basic_widget_rep {
  array<wk_widget> ws;
public:
  extend_widget_rep (wk_widget w, array<wk_widget> ws);
  operator tree ();
  void handle_get_size (get_size_event ev);
  void handle_position (position_event ev);
  bool handle (event ev);
};

/******************************************************************************
* Implementation of balloon widgets
******************************************************************************/

extend_widget_rep::extend_widget_rep (wk_widget w, array<wk_widget> ws2):
  basic_widget_rep (1, south_west), ws (ws2)
{
  a[0]= w;
}

extend_widget_rep::operator tree () {
  int i;
  tree t (TUPLE, N(ws)+2);
  t[0]= "extend";
  t[1]= (tree) a[0];
  for (i=0; i<N(ws); i++) t[i+2]= (tree) ws[i];
  return t;
}

void
extend_widget_rep::handle_get_size (get_size_event ev) {
  SI w=0, h=0;
  for (int i=0; i<N(ws); i++) {
    SI ww= ev->w, hh= ev->h;
    a[i] << get_size (ww, hh, ev->mode);
    w= max (w, ww);
    h= max (h, hh);
  }
  ev->w= w;
  ev->h= h;
}

void
extend_widget_rep::handle_position (position_event ev) {
  (event) ev;
  a[0] << emit_position (0, 0, w, h, south_west);
}

bool
extend_widget_rep::handle (event ev) {
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
    return basic_widget_rep::handle (ev);
  default:
    a[0] << ev;
    return true;
  }
}

/******************************************************************************
* exported routines
******************************************************************************/

wk_widget
extend (wk_widget w, array<wk_widget> a) {
  return tm_new<extend_widget_rep> (w, a);
}
