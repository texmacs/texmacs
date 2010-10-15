
/******************************************************************************
* MODULE     : tm_frame.cpp
* DESCRIPTION: Routines for main TeXmacs frames
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_frame.hpp"
#include "tm_window.hpp"
#include "message.hpp"

/******************************************************************************
* Constructor and destructor
******************************************************************************/

tm_frame_rep::tm_frame_rep ():
  full_screen (false), full_screen_edit (false), dialogue_win () {}
tm_frame_rep::~tm_frame_rep () {}

/******************************************************************************
* Subroutines
******************************************************************************/

widget
make_menu_widget (object menu) {
  widget w= as_widget (call ("make-menu-widget", menu, true));
  //string s= "(make-menu-widget '" * menu * " #t)";
  //widget w= as_widget (eval (s));
  if (is_nil (w)) {
    array<widget> a (0);
    return horizontal_menu (a);
  }
  return w;
}

string
icon_bar_name (int which) {
  if (which == 0) return "main";
  else if (which == 1) return "context";
  else return "user";
}

/******************************************************************************
* Properties of the current window
******************************************************************************/

int
tm_frame_rep::get_window_id () {
  return get_window () -> serial;
}

void
tm_frame_rep::set_window_property (scheme_tree what, scheme_tree val) {
  get_window () -> set_property (what, val);
}

void
tm_frame_rep::set_bool_window_property (string what, bool val) {
  get_window () -> set_property (what, val? string ("true"): string ("false"));
}

void
tm_frame_rep::set_int_window_property (string what, int val) {
  get_window () -> set_property (what, as_tree (val));
}

void
tm_frame_rep::set_string_window_property (string what, string val) {
  get_window () -> set_property (what, val);
}

scheme_tree
tm_frame_rep::get_window_property (scheme_tree what) {
  return get_window () -> get_property (what);
}

bool
tm_frame_rep::get_bool_window_property (string what) {
  return as_bool (get_window () -> get_property (what));
}

int
tm_frame_rep::get_int_window_property (string what) {
  return as_int (get_window () -> get_property (what));
}

string
tm_frame_rep::get_string_window_property (string what) {
  return as_string (get_window () -> get_property (what));
}

/******************************************************************************
* Interface
******************************************************************************/

void
tm_frame_rep::menu_widget (string menu, widget& w) {
  object xmenu= eval ("'" * menu);
  w= make_menu_widget (xmenu);
}

void
tm_frame_rep::menu_main (string menu) {
  if (!has_view ()) return;
  get_window () -> menu_main (menu);
}

void
tm_frame_rep::menu_icons (int which, string menu) {
  if ((which<0) || (which>2) || (!has_view())) return;
  get_window () -> menu_icons (which, menu);
}

void
tm_frame_rep::show_header (bool flag) {
  if (!has_view ()) return;
  get_window () -> set_header_flag (flag);
}

void
tm_frame_rep::show_icon_bar (int which, bool flag) {
  if ((which<0) || (which>2) || (!has_view())) return;
  get_window () -> set_icon_bar_flag (which, flag);
}

void
tm_frame_rep::show_footer (bool flag) {
  if (!has_view ()) return;
  get_window () -> set_footer_flag (flag);
}

bool
tm_frame_rep::visible_header () {
  return get_window () -> get_header_flag ();
}

bool
tm_frame_rep::visible_icon_bar (int which) {
  if ((which<0) || (which>2)) return false;
  return get_window () -> get_icon_bar_flag (which);
}

bool
tm_frame_rep::visible_footer () {
  return get_window () -> get_footer_flag ();
}

void
tm_frame_rep::set_shrinking_factor (int sf) {
  if (!has_view ()) return;
  get_window () -> set_shrinking_factor (sf);
}

int
tm_frame_rep::get_shrinking_factor () {
  return get_window () -> get_shrinking_factor ();
}

/******************************************************************************
* Routines concerning the widget
******************************************************************************/

void
tm_frame_rep::get_visible (SI& x1, SI& y1, SI& x2, SI& y2) {
  get_window () -> get_visible (x1, y1, x2, y2);
}

void
tm_frame_rep::set_scrollbars (int sb) {
  get_window () -> set_scrollbars (sb);
}

void
tm_frame_rep::scroll_where (SI& x, SI& y) {
  get_window () -> get_scroll_pos (x, y);
}

void
tm_frame_rep::scroll_to (SI x, SI y) {
  get_window () -> set_scroll_pos (x, y);
}

void
tm_frame_rep::get_extents (SI& x1, SI& y1, SI& x2, SI& y2) {
  get_window () -> get_extents (x1, y1, x2, y2);
}

void
tm_frame_rep::set_extents (SI x1, SI y1, SI x2, SI y2) {
  get_window () -> set_extents (x1, y1, x2, y2);
}

void
tm_frame_rep::set_left_footer (string s) {
  if (!has_window ()) return;
  get_window () -> set_left_footer (s);
}

void
tm_frame_rep::set_right_footer (string s) {
  if (!has_window ()) return;
  get_window () -> set_right_footer (s);
}

void
tm_frame_rep::set_message (tree left, tree right, bool temp) {
  if (!has_window ()) return;
  get_editor() -> set_message (left, right, temp);
}

void
tm_frame_rep::recall_message () {
  if (!has_window ()) return;
  get_editor() -> recall_message ();
}

void
tm_frame_rep::full_screen_mode (bool on, bool edit) {
  if (on && !edit) {
    show_header (false);
    show_footer (false);
  }
  else {
    show_header (true);
    show_footer (true);
  }
  set_full_screen (get_window () -> win, on);
  get_editor () -> full_screen_mode (on && !edit);
  full_screen = on;
  full_screen_edit = on && edit;
}

bool
tm_frame_rep::in_full_screen_mode () {
  return full_screen && !full_screen_edit;
}

bool
tm_frame_rep::in_full_screen_edit_mode () {
  return full_screen && full_screen_edit;
}
