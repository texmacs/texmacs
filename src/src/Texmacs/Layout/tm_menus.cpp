
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
#include "Widget/make_widget.hpp"
#include "tm_buffer.hpp"

/******************************************************************************
* Constructor and destructor
******************************************************************************/

tm_layout_rep::tm_layout_rep () {}
tm_layout_rep::~tm_layout_rep () {}

/******************************************************************************
* Subroutines
******************************************************************************/

widget
make_menu_widget (object menu) {
  widget w= as_widget (call ("make-menu-widget", menu, true));
  //string s= "(make-menu-widget '" * menu * " #t)";
  //widget w= as_widget (eval (s));
  if (nil (w)) {
    array<widget> a (0);
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
  tm_widget meta= get_meta ();
  return meta->serial;
}

void
tm_layout_rep::set_window_property (scheme_tree what, scheme_tree val) {
  tm_widget meta= get_meta ();
  meta->props (what)= val;
}

void
tm_layout_rep::set_bool_window_property (string what, bool val) {
  tm_widget meta= get_meta ();
  meta->props (what)= (val? string ("true"): string ("false"));
}

void
tm_layout_rep::set_int_window_property (string what, int val) {
  tm_widget meta= get_meta ();
  meta->props (what)= as_tree (val);
}

void
tm_layout_rep::set_string_window_property (string what, string val) {
  tm_widget meta= get_meta ();
  meta->props (what)= val;
}

scheme_tree
tm_layout_rep::get_window_property (scheme_tree what) {
  tm_widget meta= get_meta ();
  return meta->props [what];
}

bool
tm_layout_rep::get_bool_window_property (string what) {
  tm_widget meta= get_meta ();
  return as_bool (meta->props [what]);
}

int
tm_layout_rep::get_int_window_property (string what) {
  tm_widget meta= get_meta ();
  return as_int (meta->props [what]);
}

string
tm_layout_rep::get_string_window_property (string what) {
  tm_widget meta= get_meta ();
  return as_string (meta->props [what]);
}

/******************************************************************************
* Interface
******************************************************************************/

void
tm_layout_rep::menu_widget (string menu, widget& w) {
  object xmenu= eval ("'" * menu);
  tm_widget meta= get_meta ();
  w= make_menu_widget (xmenu);
}

void
tm_layout_rep::menu_main (string menu) {
  if (!has_view ()) return;
  tm_widget meta= get_meta ();  
  meta->menu_main (menu);
}

void
tm_layout_rep::menu_icons (int which, string menu) {
  if ((which<0) || (which>2) || (!has_view())) return;
  tm_widget meta= get_meta ();
  meta->menu_icons (which, menu);
}

void
tm_layout_rep::show_header (bool flag) {
  if (!has_view ()) return;
  tm_widget meta= get_meta ();
  meta->set_subwidget_flag (((widget) meta) ["header"], flag);
}

void
tm_layout_rep::show_icon_bar (int which, bool flag) {
  if ((which<0) || (which>2) || (!has_view())) return;
  string name= icon_bar_name (which);
  tm_widget meta= get_meta ();
  meta->set_subwidget_flag (((widget) meta) ["header"] [name], flag);
}

void
tm_layout_rep::show_footer (bool flag) {
  if (!has_view ()) return;
  tm_widget meta= get_meta ();
  meta->set_footer_flag (flag);
}

bool
tm_layout_rep::visible_header () {
  tm_widget meta= get_meta ();
  return meta->get_subwidget_flag (((widget) meta) ["header"]);
}

bool
tm_layout_rep::visible_icon_bar (int which) {
  if ((which<0) || (which>2)) return false;
  string name= icon_bar_name (which);
  tm_widget meta= get_meta ();
  return meta->get_subwidget_flag (((widget) meta) ["header"] [name]);
}

bool
tm_layout_rep::visible_footer () {
  tm_widget meta= get_meta ();
  return meta->get_footer_flag ();
}

void
tm_layout_rep::set_shrinking_factor (int sf) {
  if (!has_view ()) return;
  tm_widget meta= get_meta ();
  meta->set_shrinking_factor (sf);
}

int
tm_layout_rep::get_shrinking_factor () {
  tm_widget meta= get_meta ();
  return meta->get_shrinking_factor ();
}
