
/******************************************************************************
* MODULE     : window_widget.cpp
* DESCRIPTION: Window widgets
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "window.hpp"
#include "message.hpp"
#include "Widkit/wk_widget.hpp"
#include "Widkit/Event/basic_event.hpp"

/******************************************************************************
* Window widgets
******************************************************************************/

class window_widget_rep: public wk_widget_rep {
  command quit;
public:
  window_widget_rep (array<wk_widget> a, array<string> name, command quit2):
    wk_widget_rep (a, name, north_west), quit (quit2) {}
  operator tree () { return tree (TUPLE, "window", (tree) a[0]); }
  bool is_window_widget () { return true; }
  bool handle (event ev);
};

bool
window_widget_rep::handle (event ev) {
  if (ev->type == ATTACH_WINDOW_EVENT)
    win= ((attach_window_event) ev)->win;
  if (ev->type == DESTROY_EVENT && !is_nil (quit))
    quit ();
  return a[0] -> handle (ev);
}

wk_widget
window_widget (wk_widget w, command quit) {
  array<wk_widget> a (1);
  a[0]= w;
  array<string> name (1);
  name[0]= "window";
  return tm_new<window_widget_rep> (a, name, quit);
}

/******************************************************************************
* Interface
******************************************************************************/

wk_widget
plain_window_widget (wk_widget wid, string s, command quit) {
  SI W, H;
  gui_root_extents (W, H);
  SI min_w= 0, min_h= 0, def_w= W, def_h= H, max_w= W, max_h= H;
  wid << get_size (min_w, min_h, -1);
  wid << get_size (def_w, def_h, 0);
  wid << get_size (max_w, max_h, 1);
  wid= window_widget (wid, quit);
  (void) plain_window (abstract (wid), s,
		       min_w, min_h, def_w, def_h, max_w, max_h);
  return wid;
}

wk_widget
popup_window_widget (wk_widget wid, string s) {
  SI W, H;
  gui_root_extents (W, H);
  SI min_w= 0, min_h= 0, def_w= W, def_h= H, max_w= W, max_h= H;
  wid << get_size (min_w, min_h, -1);
  wid << get_size (def_w, def_h, 0);
  wid << get_size (max_w, max_h, 1);
  wid= window_widget (wid, command ());
  (void) popup_window (abstract (wid), s,
		       min_w, min_h, def_w, def_h, max_w, max_h);
  return wid;
}

void
destroy_window_widget (wk_widget w) {
  ASSERT (w->is_window_widget (), "not a window widget");
  tm_delete (w->win);
}

void
refresh_size (widget wid, bool exact) {
  window win= concrete (wid) -> win;
  SI old_w, old_h;
  win->get_size (old_w, old_h);
  SI def_w= old_w, def_h= old_h;
  SI min_w= old_w, min_h= old_h;
  SI max_w= old_w, max_h= old_h;
  concrete (wid) << get_size (def_w, def_h, 0);
  concrete (wid) << get_size (min_w, min_h, -1);
  concrete (wid) << get_size (max_w, max_h, 1);
  win->set_size_limits (min_w, min_h, max_w, max_h);
  SI new_w= min (max (old_w, min_w), max_w);
  SI new_h= min (max (old_h, min_h), max_h);
  if (exact) { new_w= def_w; new_h= def_h; }
  if (new_w != old_w || new_h != old_h) {
    concrete (wid) << emit_position (0, 0, new_w, new_h);
    win->set_size (new_w, new_h);
  }
}
