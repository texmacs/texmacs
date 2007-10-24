
/******************************************************************************
* MODULE     : tm_menus.cpp
* DESCRIPTION: Dynamic menus
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tm_layout.hpp"
#include "tm_window.hpp"

/******************************************************************************
* Constructor and destructor
******************************************************************************/

tm_layout_rep::tm_layout_rep () {}
tm_layout_rep::~tm_layout_rep () {}

/******************************************************************************
* Subroutines
******************************************************************************/

wk_widget
make_menu_widget (object menu) {
  wk_widget w= concrete (as_widget (call ("make-menu-widget", menu, true)));
  //string s= "(make-menu-widget '" * menu * " #t)";
  //widget w= as_widget (eval (s));
  if (nil (w)) {
    array<wk_widget> a (0);
    return horizontal_array (a);
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
tm_layout_rep::get_window_id () {
  return get_window () -> serial;
}

void
tm_layout_rep::set_window_property (scheme_tree what, scheme_tree val) {
  get_window () -> set_property (what, val);
}

void
tm_layout_rep::set_bool_window_property (string what, bool val) {
  get_window () -> set_property (what, val? string ("true"): string ("false"));
}

void
tm_layout_rep::set_int_window_property (string what, int val) {
  get_window () -> set_property (what, as_tree (val));
}

void
tm_layout_rep::set_string_window_property (string what, string val) {
  get_window () -> set_property (what, val);
}

scheme_tree
tm_layout_rep::get_window_property (scheme_tree what) {
  return get_window () -> get_property (what);
}

bool
tm_layout_rep::get_bool_window_property (string what) {
  return as_bool (get_window () -> get_property (what));
}

int
tm_layout_rep::get_int_window_property (string what) {
  return as_int (get_window () -> get_property (what));
}

string
tm_layout_rep::get_string_window_property (string what) {
  return as_string (get_window () -> get_property (what));
}

/******************************************************************************
* Interface
******************************************************************************/

void
tm_layout_rep::menu_widget (string menu, wk_widget& w) {
  object xmenu= eval ("'" * menu);
  w= make_menu_widget (xmenu);
}

void
tm_layout_rep::menu_main (string menu) {
  if (!has_view ()) return;
  get_window () -> menu_main (menu);
}

void
tm_layout_rep::menu_icons (int which, string menu) {
  if ((which<0) || (which>2) || (!has_view())) return;
  get_window () -> menu_icons (which, menu);
}

void
tm_layout_rep::show_header (bool flag) {
  if (!has_view ()) return;
  get_window () -> set_header_flag (flag);
}

void
tm_layout_rep::show_icon_bar (int which, bool flag) {
  if ((which<0) || (which>2) || (!has_view())) return;
  get_window () -> set_icon_bar_flag (which, flag);
}

void
tm_layout_rep::show_footer (bool flag) {
  if (!has_view ()) return;
  get_window () -> set_footer_flag (flag);
}

bool
tm_layout_rep::visible_header () {
  return get_window () -> get_header_flag ();
}

bool
tm_layout_rep::visible_icon_bar (int which) {
  if ((which<0) || (which>2)) return false;
  return get_window () -> get_icon_bar_flag (which);
}

bool
tm_layout_rep::visible_footer () {
  return get_window () -> get_footer_flag ();
}

void
tm_layout_rep::set_shrinking_factor (int sf) {
  if (!has_view ()) return;
  get_window () -> set_shrinking_factor (sf);
}

int
tm_layout_rep::get_shrinking_factor () {
  return get_window () -> get_shrinking_factor ();
}
