
/******************************************************************************
* MODULE     : basic_event.cpp
* DESCRIPTION: The most common events
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "window.hpp"
#include "Widkit/Event/basic_event.hpp"

/******************************************************************************
* Attribute events
******************************************************************************/

get_size_event_rep::get_size_event_rep (SI& w2, SI& h2, int m):
  event_rep (GET_SIZE_EVENT), w (w2), h (h2), mode (m) {}
get_size_event_rep::operator tree () {
  if (mode==-1) return "get_min_size_event";
  if (mode== 0) return "get_size_event";
  else          return "get_max_size_event"; }
event get_size (SI& w, SI& h, int mode) {
  return tm_new<get_size_event_rep> (w, h, mode); }

get_widget_event_rep::get_widget_event_rep (string ww, wk_widget& w2):
  event_rep (GET_WIDGET_EVENT), which (ww), w (w2) {}
get_widget_event_rep::operator tree () {
  return tree (TUPLE, "get_widget_event", which); }
event get_widget (string which, wk_widget& w) {
  return tm_new<get_widget_event_rep> (which, w); }

set_widget_event_rep::set_widget_event_rep (string ww, wk_widget w2):
  event_rep (SET_WIDGET_EVENT), which (ww), w (w2) {}
set_widget_event_rep::operator tree () {
  return tree (TUPLE, "set_widget_event", which); }
event set_widget (string which, wk_widget w) {
  return tm_new<set_widget_event_rep> (which, w); }

/******************************************************************************
* Structure events
******************************************************************************/

attach_window_event_rep::attach_window_event_rep (window win2):
  event_rep (ATTACH_WINDOW_EVENT), win (win2) {}
attach_window_event_rep::operator tree () { return "attach_window_event"; }
event emit_attach_window (window win) {
  return tm_new<attach_window_event_rep> (win); }

position_event_rep::position_event_rep ():
  event_rep (POSITION_EVENT), flag (true),
  ox (0), oy (0), w (0), h (0), grav (north_west) {}
position_event_rep::position_event_rep (SI a, SI b, SI c, SI d, gravity grav2):
  event_rep (POSITION_EVENT), flag (false),
  ox (a), oy (b), w (c), h (d), grav (grav2) {}
position_event_rep::operator tree () {
  if (flag) return "reposition";
  return tree (TUPLE, "position_event", as_tree (grav),
	       tree (TUPLE, as_string (ox/PIXEL), as_string (oy/PIXEL)),
	       tree (TUPLE, as_string (w /PIXEL), as_string (h /PIXEL))); }
event emit_reposition () {
  return tm_new<position_event_rep> (); }
event emit_position (SI ox, SI oy, SI w, SI h, gravity grav) {
  return tm_new<position_event_rep> (ox, oy, w, h, grav); }

move_event_rep::move_event_rep ():
  event_rep (MOVE_EVENT) {}
move_event_rep::operator tree () { return "move_event"; }
event emit_move () {
  return tm_new<move_event_rep> (); }

resize_event_rep::resize_event_rep ():
  event_rep (RESIZE_EVENT) {}
resize_event_rep::operator tree () { return "resize_event"; }
event emit_resize () {
  return tm_new<resize_event_rep> (); }

destroy_event_rep::destroy_event_rep ():
  event_rep (DESTROY_EVENT) {}
destroy_event_rep::operator tree () { return "destroy_event"; }
event emit_destroy () {
  return tm_new<destroy_event_rep> (); }

/******************************************************************************
* Input events
******************************************************************************/

keypress_event_rep::keypress_event_rep (string key2, time_t t2):
  event_rep (KEYPRESS_EVENT), key (key2), t (t2) {}
keypress_event_rep::operator tree () {
  return tree (TUPLE, "keypress_event", key); }
event emit_keypress (string key, time_t t) {
  return tm_new<keypress_event_rep> (key, t); }
event emit_keypress (keypress_event ev, string key) {
  return tm_new<keypress_event_rep> (key, ev->t); }

keyboard_focus_event_rep::keyboard_focus_event_rep (bool io, time_t t2):
  event_rep (KEYBOARD_FOCUS_EVENT), flag (io), t (t2) {}
keyboard_focus_event_rep::operator tree () {
  return tree (TUPLE, "keyboard_focus_event", (char*) (flag? "in": "out")); }
event emit_keyboard_focus (bool in_out_flag, time_t t) {
  return tm_new<keyboard_focus_event_rep> (in_out_flag, t); }

mouse_event_rep::mouse_event_rep (string type2, SI x2, SI y2,
  int mods2, time_t t2): event_rep (MOUSE_EVENT),
    type (type2), x (x2), y (y2), mods (mods2), t (t2) {}
mouse_event_rep::operator tree () {
  return tree (TUPLE, "mouse_event", type,
	       tree (TUPLE, as_string (x/PIXEL), as_string (y/PIXEL))); }
bool
mouse_event_rep::pressed (string s) {
  if (s == "left") return (mods&1) != 0;
  if (s == "middle") return (mods&2) != 0;
  if (s == "right") return (mods&4) != 0;
  if (s == "extra1") return (mods&8) != 0;
  if (s == "extra2") return (mods&16) != 0;
  return false; }
event emit_mouse (string type, SI x, SI y, int mods, time_t t) {
  return tm_new<mouse_event_rep> (type, x, y, mods, t); }
event emit_mouse (mouse_event ev, string type, SI x, SI y) {
  return tm_new<mouse_event_rep> (type, x, y, ev->mods, ev->t); }

alarm_event_rep::alarm_event_rep (string message2, time_t t2):
  event_rep (ALARM_EVENT), message (message2), t (t2) {}
alarm_event_rep::operator tree () {
  return tree (TUPLE, "alarm_event", message); }
event emit_alarm (string message, time_t t) {
  return tm_new<alarm_event_rep> (message, t); }

/******************************************************************************
* Output events
******************************************************************************/

clear_event_rep::clear_event_rep (SI x1b, SI y1b, SI x2b, SI y2b):
  event_rep (CLEAR_EVENT), x1 (x1b), y1 (y1b), x2 (x2b), y2 (y2b) {}
clear_event_rep::operator tree () {
  return tree (TUPLE, "clear event",
	       tree (TUPLE, as_string (x1/PIXEL), as_string (y1/PIXEL)),
	       tree (TUPLE, as_string (x2/PIXEL), as_string (y2/PIXEL))); }
event emit_clear (SI x1, SI y1, SI x2, SI y2) {
  return tm_new<clear_event_rep> (x1, y1, x2, y2); }

repaint_event_rep::repaint_event_rep (SI x1b, SI y1b, SI x2b, SI y2b, bool& b):
  event_rep (REPAINT_EVENT), x1 (x1b), y1 (y1b), x2 (x2b), y2 (y2b), stop(b) {}
repaint_event_rep::operator tree () {
  return tree (TUPLE, "repaint event",
	       tree (TUPLE, as_string (x1/PIXEL), as_string (y1/PIXEL)),
	       tree (TUPLE, as_string (x2/PIXEL), as_string (y2/PIXEL))); }
event emit_repaint (SI x1, SI y1, SI x2, SI y2, bool& stop) {
  return tm_new<repaint_event_rep> (x1, y1, x2, y2, stop); }

/******************************************************************************
* Request some action
******************************************************************************/

update_event_rep::update_event_rep ():
  event_rep (UPDATE_EVENT) {}
update_event_rep::operator tree () { return "update_event"; }
event emit_update () {
  return tm_new<update_event_rep> (); }

invalidate_event_rep::invalidate_event_rep ():
  event_rep (INVALIDATE_EVENT), all_flag (true),
  x1 (0), y1 (0), x2 (0), y2 (0) {}
invalidate_event_rep::invalidate_event_rep (SI x1b, SI y1b, SI x2b, SI y2b):
  event_rep (INVALIDATE_EVENT), all_flag (false),
  x1 (x1b), y1 (y1b), x2 (x2b), y2 (y2b) {}
invalidate_event_rep::operator tree () {
  if (all_flag) return "invalidate_event";
  else return tree (TUPLE, "invalidate_event",
		    tree (TUPLE, as_string (x1/PIXEL),as_string (y1/PIXEL)),
		    tree (TUPLE, as_string (x2/PIXEL),as_string (y2/PIXEL))); }
event emit_invalidate_all () {
  return tm_new<invalidate_event_rep> (); }
event emit_invalidate (SI x1, SI y1, SI x2, SI y2) {
  return tm_new<invalidate_event_rep> (x1, y1, x2, y2); }

keyboard_grab_event_rep::keyboard_grab_event_rep (bool io, time_t t2):
  event_rep (KEYBOARD_GRAB_EVENT), flag (io), t (t2) {}
keyboard_grab_event_rep::operator tree () {
  return tree (TUPLE, "keyboard_grab_event", (char*) (flag? "in": "out")); }
event emit_keyboard_grab (bool in_out_flag, time_t t) {
  return tm_new<keyboard_grab_event_rep> (in_out_flag, t); }

mouse_grab_event_rep::mouse_grab_event_rep (bool io, time_t t2):
  event_rep (MOUSE_GRAB_EVENT), flag (io), t (t2) {}
mouse_grab_event_rep::operator tree () {
  return tree (TUPLE, "mouse_grab_event", (char*) (flag? "in": "out")); }
event emit_mouse_grab (bool in_out_flag, time_t t) {
  return tm_new<mouse_grab_event_rep> (in_out_flag, t); }

request_alarm_event_rep::request_alarm_event_rep (event ev2, time_t delay2):
  event_rep (REQUEST_ALARM_EVENT), ev (ev2), delay (delay2) {}
request_alarm_event_rep::operator tree () {
  return tree (TUPLE, "request_alarm_event",
	       (tree) ev, as_string ((int) delay)); }
event emit_request_alarm (event ev, time_t delay) {
  return tm_new<request_alarm_event_rep> (ev, delay); }

/******************************************************************************
* Miscellaneous events
******************************************************************************/

find_child_event_rep::find_child_event_rep (SI x2, SI y2, int& which2):
  event_rep (FIND_CHILD_EVENT), x (x2), y (y2), which (which2) {}
find_child_event_rep::operator tree () {
  return tree (TUPLE, "find_child_event",
	       tree (TUPLE, as_string (x/PIXEL), as_string (y/PIXEL))); }
event emit_find_child (SI x, SI y, int& which) {
  return tm_new<find_child_event_rep> (x, y, which); }

/******************************************************************************
* Output routines for the gravity class
******************************************************************************/

ostream&
operator << (ostream& out, gravity grav) {
  return out << as_tree (grav);
}

tree
as_tree (gravity grav) {
  switch (grav) {
  case north_west: return "north west";
  case north     : return "north";
  case north_east: return "north east";
  case west      : return "west";
  case center    : return "center";
  case east      : return "east";
  case south_west: return "south west";
  case south     : return "south";
  case south_east: return "south east";
  }
  FAILED ("unknown gravity");
  return "";
}
