
/******************************************************************************
* MODULE     : window_widget.cpp
* DESCRIPTION: Window widgets
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "window.hpp"
#include "Widkit/wk_widget.hpp"
#include "Widkit/Event/basic_event.hpp"

/******************************************************************************
* Window widgets
******************************************************************************/

class window_widget_rep: public wk_widget_rep {
public:
  window_widget_rep (array<wk_widget> a, array<string> name):
    wk_widget_rep (a, name, north_west) {}
  operator tree () { return tree (TUPLE, "window", (tree) a[0]); }
  bool is_window_widget () { return true; }
  bool handle (event ev);
};

bool
window_widget_rep::handle (event ev) {
  if (ev->type == ATTACH_WINDOW_EVENT)
    win= ((attach_window_event) ev)->win;
  return a[0] -> handle (ev);
}

wk_widget
window_widget (wk_widget w) {
  array<wk_widget> a (1);
  a[0]= w;
  array<string> name (1);
  name[0]= "window";
  return new window_widget_rep (a, name);
}

/******************************************************************************
* Interface
******************************************************************************/

wk_widget
plain_window_widget (wk_widget wid, string s) {
  SI W, H;
  gui_root_extents (W, H);
  SI min_w= 0, min_h= 0, def_w= H, def_h= H, max_w= H, max_h= H;
  wid << get_size (min_w, min_h, -1);
  wid << get_size (def_w, def_h, 0);
  wid << get_size (max_w, max_h, 1);
  wid= window_widget (wid);
  (void) plain_window (abstract (wid), s,
		       min_w, min_h, def_w, def_h, max_w, max_h);
  return wid;
}

wk_widget
popup_window_widget (wk_widget wid, string s) {
  SI W, H;
  gui_root_extents (W, H);
  SI min_w= 0, min_h= 0, def_w= H, def_h= H, max_w= H, max_h= H;
  wid << get_size (min_w, min_h, -1);
  wid << get_size (def_w, def_h, 0);
  wid << get_size (max_w, max_h, 1);
  wid= window_widget (wid);
  (void) popup_window (abstract (wid), s,
		       min_w, min_h, def_w, def_h, max_w, max_h);
  return wid;
}

void
destroy_window_widget (wk_widget w) {
  if (!w->is_window_widget ())
    fatal_error ("not a window widget", "destroy_window_widget");
  delete w->win;
}
