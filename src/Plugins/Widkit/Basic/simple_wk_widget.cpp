
/******************************************************************************
* MODULE     : simple_wk_widget.cpp
* DESCRIPTION: Simple wk_widgets for customization later on
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Widkit/simple_wk_widget.hpp"
#include "gui.hpp"

/******************************************************************************
* Constructor
******************************************************************************/

simple_widget_rep::simple_widget_rep ():
  attribute_widget_rep () {}

/******************************************************************************
* Empty handlers for redefinition later on
******************************************************************************/

void
simple_widget_rep::handle_get_size_hint (SI& w, SI& h) {
  gui_root_extents (w, h);  
}

void
simple_widget_rep::handle_notify_resize (SI w, SI h) {
  (void) w; (void) h;
}

void
simple_widget_rep::handle_keypress (string key, time_t t) {
  (void) key; (void) t;
}

void
simple_widget_rep::handle_keyboard_focus (bool has_focus, time_t t) {
  (void) has_focus; (void) t;
}

void
simple_widget_rep::handle_mouse (string kind, SI x, SI y, int mods, time_t t) {
  (void) kind; (void) x; (void) y; (void) mods; (void) t;
}

void
simple_widget_rep::handle_set_shrinking_factor (int sf) {
  (void) sf;
}

void
simple_widget_rep::handle_clear (SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2;
}

void
simple_widget_rep::handle_repaint (SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2;
}

/******************************************************************************
* Calling the handlers from the usual widkit handlers
******************************************************************************/

void
simple_widget_rep::handle_get_size (get_size_event ev) {
  handle_get_size_hint (ev->w, ev->h);
}

void
simple_widget_rep::handle_attach_window (attach_window_event ev) {
  basic_widget_rep::handle_attach_window (ev);
}

void
simple_widget_rep::handle_resize (resize_event ev) { (void) ev;
  handle_notify_resize (0, 0); // FIXME
}

void
simple_widget_rep::handle_keypress (keypress_event ev) {
  handle_keypress (ev->key, ev->t);
}

void
simple_widget_rep::handle_keyboard_focus (keyboard_focus_event ev) {
  handle_keyboard_focus (ev->flag, ev->t);
}

void
simple_widget_rep::handle_mouse (mouse_event ev) {
  handle_mouse (ev->type, ev->x, ev->y, ev->mods, ev->t);
}

void
simple_widget_rep::handle_set_integer (set_integer_event ev) {
  if (ev->which == "shrinking factor")
    handle_set_shrinking_factor (ev->i);
}

void
simple_widget_rep::handle_clear (clear_event ev) {
  handle_clear (ev->x1, ev->y1, ev->x2, ev->y2);
}

void
simple_widget_rep::handle_repaint (repaint_event ev) {
  handle_repaint (ev->x1, ev->y1, ev->x2, ev->y2);
}

void
simple_widget_rep::handle_set_coord2 (set_coord2_event ev) {
  if (ev->which == "extra width" && ev->c1 == 0 && ev->c2 == 0) return;
  else WK_FAILED ("could not set coord2 attribute " * ev->which);
}

void
simple_widget_rep::handle_get_coord2 (get_coord2_event ev) {
  if (ev->which == "extra width") { ev->c1= ev->c2= 0; return; }
  else WK_FAILED ("could not get coord2 attribute " * ev->which);
}
