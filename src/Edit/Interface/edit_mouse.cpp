
/******************************************************************************
* MODULE     : edit_mouse.cpp
* DESCRIPTION: Mouse handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "edit_interface.hpp"
#include "tm_buffer.hpp"
#include "timer.hpp"
#include "link.hpp"
#include "analyze.hpp"
#include "drd_mode.hpp"
#include "message.hpp"
#include "window.hpp"

/******************************************************************************
* dispatching
******************************************************************************/

void
edit_interface_rep::mouse_any (string type, SI x, SI y, int mods, time_t t) {
  last_x= x; last_y= y;
  mark_undo_blocks ();
  if (type != "move" || (is_attached (this) && !check_event (MOTION_EVENT)))
    update_active_loci ();

  if (type == "leave")
    set_pointer ("XC_top_left_arrow");
  if ((type != "move") && (type != "enter") && (type != "leave"))
    set_input_normal ();
  if (!is_nil (popup_win) && (type != "leave")) {
    set_visibility (popup_win, false);
    destroy_window_widget (popup_win);
    popup_win= widget ();
  }

  if (inside_graphics (false)) {
    string type2= type;
    if (type == "enter") {
      dragging= start_drag= false;
      right_dragging= start_right_drag= false;
    }
    if (type == "press-left")
      start_drag= true;
    if (type == "press-right")
      start_right_drag= true;

    if (start_drag && type == "move") {
      type2= "start-drag";
      start_drag= false;
      dragging= true;
    }
    else if (dragging && (type == "move"))
      type2= "dragging";
    if (dragging && (type == "release-left"))
      type2= "end-drag";

    if (start_right_drag && type == "move") {
      type2= "start-right-drag";
      start_right_drag= false;
      right_dragging= true;
    }
    else if (right_dragging && (type == "move"))
      type2= "right-dragging";
    if (right_dragging && (type == "release-right"))
      type2= "end-right-drag";

    if (type == "release-left")
      dragging= start_drag= false;
    if (type == "release-right")
      right_dragging= start_right_drag= false;
    if (mouse_graphics (type2, x, y, mods, t)) return;
    if (!over_graphics (x, y))
      eval ("(graphics-reset-context 'text-cursor)");
  }

  if (type == "press-left") mouse_click (x, y);
  if (dragging && (type == "move")) {
    if (is_attached (this) && check_event (DRAG_EVENT)) return;
    mouse_drag (x, y);
  }
  if (type == "release-left" || type == "release-right") {
    dragging= right_dragging= false;
    send_mouse_grab (this, false);
    if ((t >= last_click) && ((t - last_click) <= 250)) {
      last_click= t;
      if (mouse_extra_click (x, y))
	last_click= t- 1000;
    }
    else {
      last_click= t;
      mouse_select (x, y, mods);
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
  if (eb->action ("click", x, y, 0) != "") return;
  start_x   = x;
  start_y   = y;
  start_drag= dragging= true;
  start_right_drag= right_dragging= false;
  send_mouse_grab (this, true);
}

bool
edit_interface_rep::mouse_extra_click (SI x, SI y) {
  go_to (x, y);
  if (eb->action ("double-click", x, y, 0) != "") return true;
  go_to (x, y);
  path p1, p2;
  get_selection (p1, p2);
  if ((p1==p2) || path_less (tp, p1) || path_less (p2, tp)) select (tp, tp);
  select_enlarge ();
  return false;
}

void
edit_interface_rep::mouse_drag (SI x, SI y) {
  if (inside_graphics ()) return;
  if (eb->action ("drag", x, y, 0) != "") return;
  end_x  = x;
  end_y  = y;
  selection_visible ();
  path sp= find_innermost_scroll (eb, tp);
  path p1= tree_path (sp, start_x, start_y, 0);
  path p2= tree_path (sp, end_x  , end_y  , 0);
  if (path_inf (p2, p1)) {
    path temp= p1;
    p1= p2;
    p2= temp;
  }
  set_selection (p1, p2);
  if ((p1 == p2) && start_drag) return;
  start_drag= start_right_drag= false;
  notify_change (THE_SELECTION);
}

void
edit_interface_rep::mouse_select (SI x, SI y, int mods) {
  if (eb->action ("select" , x, y, 0) != "") return;
  if (!is_nil (active_ids) && (mods & 256) == 0) {
    call ("link-follow-ids", object (active_ids));
    return;
  }
  tree g;
  bool b0= inside_graphics (false);
  bool b= inside_graphics ();
  if (b) g= get_graphics ();
  go_to (x, y);
  if ((!b0 && inside_graphics (false)) || (b0 && !inside_graphics (false))) {
    dragging= start_drag= false;
    right_dragging= start_right_drag= false;
  }
  if (!b && inside_graphics ())
    eval ("(graphics-reset-context 'begin)");
  tree g2= get_graphics ();
  if (b && (!inside_graphics () || obtain_ip (g) != obtain_ip (g2))) {
    invalidate_graphical_object ();
    eval ("(graphics-reset-context 'exit)");
  }
  if (selection_active_any ())
    selection_set ("mouse", selection_get (), true);
}

void
edit_interface_rep::mouse_paste (SI x, SI y) { (void) x; (void) y;
  if (eb->action ("paste", x, y, 0) != "") return;
  go_to (x, y);
  selection_paste ("mouse");
}

void
edit_interface_rep::mouse_adjust (SI x, SI y) {
  if (eb->action ("adjust", x, y, 0) != "") return;
  x /= sfactor; y /= sfactor;
  abs_round (x, y);
  if (is_nil (popup_win)) {
    SI wx, wy;
    ::get_position (get_window (this), wx, wy);
    widget wid;
    SERVER (menu_widget ("(vertical (link texmacs-popup-menu))", wid));
    widget popup_wid= popup_widget (wid);
    popup_win= ::popup_window_widget (popup_wid, "Popup menu");
#if defined (QTTEXMACS) || defined(AQUATEXMACS)
    SI ox, oy;
    get_position (this, ox, oy);
    set_position (popup_win, wx+     x, wy+     y);
#else
    set_position (popup_win, wx+ ox+ x, wy+ oy+ y);
#endif
    set_visibility (popup_win, true);
    send_keyboard_focus (this);
    send_mouse_grab (popup_wid, true);
  }
}

void
edit_interface_rep::mouse_scroll (SI x, SI y, bool up) {
  string action= up? string ("scroll up"): string ("scroll down");
  if (eb->action (action , x, y, 0) != "") return;
  SI dy= 100*PIXEL;
  if (!up) dy= -dy;
  path sp= find_innermost_scroll (eb, tp);
  if (is_nil (sp)) {
    SERVER (scroll_where (x, y));
    y += dy;
    SERVER (scroll_to (x, y));
  }
  else {
    SI x, y, sx, sy;
    rectangle outer, inner;
    find_canvas_info (eb, sp, x, y, sx, sy, outer, inner);
    SI ty= inner->y2 - inner->y1;
    SI cy= outer->y2 - outer->y1;
    if (ty > cy) {
      tree   old_yt= eb[path_up (sp)]->get_info ("scroll-y");
      string old_ys= as_string (old_yt);
      double old_p = 0.0;
      if (ends (old_ys, "%")) old_p= as_double (old_ys (0, N(old_ys)-1));
      double new_p= old_p + 100.0 * ((double) dy) / ((double) (ty - cy));
      new_p= max (min (new_p, 100.0), 0.0);
      tree new_yt= as_string (new_p) * "%";
      if (new_yt != old_yt && is_accessible (obtain_ip (old_yt))) {
	object fun= symbol_object ("tree-set");
	object cmd= list_object (fun, old_yt, new_yt);
	eval_delayed (cmd);
	temp_invalid_cursor= true;
      }
    }
  }
}

/******************************************************************************
* getting the cursor (both for text and graphics)
******************************************************************************/

cursor
edit_interface_rep::get_cursor () {
  if (inside_graphics ()) {
    frame f= find_frame ();
    if (!is_nil (f)) {
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
  send_mouse_pointer (this, name);
}

void
edit_interface_rep::set_pointer (
  string curs_name, string mask_name)
{
  send_mouse_pointer (this, curs_name, mask_name);
}

/******************************************************************************
* Active loci
******************************************************************************/

void
edit_interface_rep::update_active_loci () {
  int old_mode= set_access_mode (DRD_ACCESS_SOURCE);
  path cp= path_up (tree_path (path (), last_x, last_y, 0));
  set_access_mode (old_mode);
  tree mt= subtree (et, cp);
  path p = cp;
  list<string> ids1, ids2;
  rectangles rs1, rs2;
  eb->loci (last_x, last_y, 0, ids1, rs1);
  while (rp <= p) {
    ids2 << get_ids (subtree (et, p));
    p= path_up (p);
  }

  locus_new_rects= rectangles ();
  active_ids= list<string> ();
  if (!is_nil (ids1 * ids2) && !has_changed (THE_FOCUS)) {
    list<tree> l= as_list_tree (call ("link-active-upwards", object (mt)));
    while (!is_nil (l)) {
      tree lt= l->item;
      path lp= reverse (obtain_ip (lt));
      selection sel= eb->find_check_selection (lp * start(lt), lp * end(lt));
      rs2 << outline (sel->rs, pixel);
      l= l->next;
    }
    ids1= as_list_string (call ("link-active-ids", object (ids1)));
    ids2= as_list_string (call ("link-active-ids", object (ids2)));
    if (is_nil (ids1)) rs1= rectangles ();
    // FIXME: we should keep track which id corresponds to which rectangle
    locus_new_rects= rs1 * rs2;
    active_ids= ids1 * ids2;
  }
  if (locus_new_rects != locus_rects) notify_change (THE_LOCUS);
}

/******************************************************************************
* Event handlers
******************************************************************************/

void
edit_interface_rep::handle_mouse (string kind, SI x, SI y, int m, time_t t) {
  x *= sfactor;
  y *= sfactor;
  //cout << kind << " (" << x << ", " << y << "; " << m << ") at " << t << "\n";
  mouse_any (kind, x, y, m, t);
}
