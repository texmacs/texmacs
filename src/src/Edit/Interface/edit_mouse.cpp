
/******************************************************************************
* MODULE     : edit_mouse.cpp
* DESCRIPTION: Mouse handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "edit_interface.hpp"
#include "tm_buffer.hpp"
#include "timer.hpp"

/******************************************************************************
* dispatching
******************************************************************************/

void
edit_interface_rep::mouse_any (string type, SI x, SI y, time_t t) {
  last_x= x; last_y= y;
  buf->mark_undo_block ();

  if ((type != "move") && (type != "enter") && (type != "leave"))
    set_input_normal ();
  if ((popup_win != NULL) && (type != "leave")) {
    popup_win->map ();
    delete popup_win;
    popup_win= NULL;
    this << emit_mouse_grab (false);
  }

  if (inside_graphics ()) {
    string type2= type;
    if (type == "enter")
      dragging= start_drag= false;
    if (type == "press-left")
      start_drag= true;

    if (start_drag && type == "move") {
      type2= "start-drag";
      start_drag= false;
      dragging= true;
    }
    else if (dragging && (type == "move"))
      type2= "dragging"; 
    if (dragging && (type == "release-left"))
      type2= "end-drag";

    if (type == "release-left")
      dragging= start_drag= false;
    if (mouse_graphics (type2, x, y, t)) return;
    if (!over_graphics (x, y))
      eval ("(graphics-reset-context 'text-cursor)");
  }

  if (type == "press-left") mouse_click (x, y);
  if (dragging && (type == "move")) {
    if (attached () && dis->check_event (DRAG_EVENT)) return;
    mouse_drag (x, y);
  }
  if (type == "release-left") {
    dragging= false;
    this << emit_mouse_grab (false);
    if ((t >= last_click) && ((t - last_click) <= 250)) {
      last_click= t;
      if (mouse_extra_click (x, y))
	last_click= t- 1000;
    }
    else {
      last_click= t;
      mouse_select (x, y);
    }
  }
  if (type == "press-middle") mouse_paste (x, y);
  if (type == "press-right") mouse_adjust (x, y);
  if (type == "press-up") mouse_scroll (x, y, true);
  if (type == "press-down") mouse_scroll (x, y, false);

  if ((type == "press-left") ||
      (type == "release-left") ||
      (type == "press-middle") ||
      (type == "press-right"))
    notify_change (THE_DECORATIONS);
}

/******************************************************************************
* Routines for the mouse
******************************************************************************/

void
edit_interface_rep::mouse_click (SI x, SI y) {
  if (eb->action ("click" , x, y, 0) != "") return;
  start_x   = x;
  start_y   = y;
  start_drag= true;
  dragging  = true;
  this << emit_mouse_grab (true);
}

bool
edit_interface_rep::mouse_extra_click (SI x, SI y) {
  go_to (x, y);

  // temporary hack for clickable footnotes
  path p= path_up (tp);
  if (rp < p) {
    if (is_compound (subtree (et, p), "footnote", 1)) {
      go_to (start (et, p * 0));
      return true;
    }
    tree st= subtree (et, path_up (p));
    if (is_concat (st) && ((last_item (p) + 1) < N(st)))
      if (last_item (tp) == right_index (st [last_item (p)]))
	if (is_compound (subtree (et, path_inc (p)), "footnote", 1)) {
	  go_to (start (et, path_inc (p) * 0));
	  return true;
	}
    path q= search_upwards ("footnote");
    if ((!nil (q)) && (tp == start (et, q * 0))) {
      go_to (end (et, q));
      return true;
    }
  }
  // end temporary hack

  if (eb->action ("double-click" , x, y, 0) != "") return true;
  go_to (x, y);
  path p1, p2;
  get_selection (p1, p2);
  if ((p1==p2) || path_less (tp, p1) || path_less (p2, tp)) select (tp, tp);
  select_enlarge ();
  return false;
}

void
edit_interface_rep::mouse_drag (SI x, SI y) {
  if (eb->action ("drag" , x, y, 0) != "") return;
  end_x  = x;
  end_y  = y;
  selection_visible ();
  path p1= tree_path (start_x, start_y, 0);
  path p2= tree_path (end_x  , end_y  , 0);
  if (path_inf (p2, p1)) {
    path temp= p1;
    p1= p2;
    p2= temp;
  }
  set_selection (p1, p2);
  if ((p1 == p2) && start_drag) return;
  start_drag= false;
  notify_change (THE_SELECTION);
}

void
edit_interface_rep::mouse_select (SI x, SI y) {
  if (eb->action ("select" , x, y, 0) != "") return;
  tree g;
  bool b= inside_graphics ();
  if (b) g= get_graphics ();
  go_to (x, y);
  if ((!b && inside_graphics ()) || (b && !inside_graphics ()))
    dragging= start_drag= false;
  if (!b && inside_graphics ())
    eval ("(graphics-reset-context 'begin)");
  if (b && (!inside_graphics () || g != get_graphics ())) {
    invalidate_graphical_object ();
    eval ("(graphics-reset-context 'exit)");
  }
  if (selection_active_any ())
    selection_set ("primary", selection_get (), true);
}

void
edit_interface_rep::mouse_paste (SI x, SI y) { (void) x; (void) y;
  if (eb->action ("paste" , x, y, 0) != "") return;
  selection_copy ();
  selection_paste ();
}

void
edit_interface_rep::mouse_adjust (SI x, SI y) {
  if (eb->action ("adjust" , x, y, 0) != "") return;
  x /= sfactor; y /= sfactor;
  abs_round (x, y);
  if (popup_win == NULL) {
    SI wx, wy;
    win->get_position (wx, wy);
    widget wid;
    SERVER (menu_widget ("(vertical (link texmacs-popup-menu))", wid));
    widget popup_wid= popup_widget (wid, center);
    popup_win= popup_window (popup_wid, wx+ ox+ x, wy+ oy+ y);
    popup_win->map ();
    this << emit_mouse_grab (true);
    popup_wid << set_integer ("grabbed", 1);
    // popup_wid << set_integer ("freeze", 1);
    popup_wid << emit_mouse_grab (true);
  }
}

void
edit_interface_rep::mouse_scroll (SI x, SI y, bool up) {
  string action= up? string ("scroll up"): string ("scroll down");
  if (eb->action (action , x, y, 0) != "") return;
  SI dy= 100*PIXEL;
  if (!up) dy= -dy;
  SERVER (scroll_where (x, y));
  y += dy;
  SERVER (scroll_to (x, y));
}

/******************************************************************************
* getting the cursor (both for text and graphics)
******************************************************************************/

cursor
edit_interface_rep::get_cursor () {
  if (inside_graphics ()) {
    frame f= find_frame ();
    if (!nil (f)) {
      point p= f [point (last_x, last_y)];
      p= f (adjust (p));
      SI x= (SI) p[0];
      SI y= (SI) p[1];
      return cursor (x, y, 0, -5*pixel, 5*pixel, 1.0);
    }
  }
  return copy (the_cursor ());
}

void
edit_interface_rep::set_pointer (string name) {
  sv->get_display()->set_pointer(name);
}

void
edit_interface_rep::set_pointer (
  string curs_name, string mask_name)
{
  sv->get_display()->set_pointer(curs_name, mask_name);
}

/******************************************************************************
* event handlers
******************************************************************************/

void
edit_interface_rep::handle_mouse (mouse_event ev) {
  string type= ev->type;
  SI     x   = ev->x*sfactor;
  SI     y   = ev->y*sfactor;
  mouse_any (type, x, y, ev->t);
}
