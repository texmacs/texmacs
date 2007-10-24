
/******************************************************************************
* MODULE     : tm_window.cpp
* DESCRIPTION: Main TeXmacs windows
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "window.hpp"
#include "Widkit/wk_widget.hpp"
#include "tm_widget.hpp"
#include "tm_window.hpp"

wk_widget make_menu_widget (object menu);
string icon_bar_name (int which);

/******************************************************************************
* Meta editor constructor and destructor
******************************************************************************/

static int tm_window_serial= 0;

tm_window_rep::tm_window_rep (tm_widget wid2, tree geom):
  win (texmacs_window (abstract (wid2), geom)),
  wid (wid2), id (create_window_id ()),
  serial (tm_window_serial++),
  texmacs_menu (NULL), texmacs_icon_menu (NULL),
  text_ptr (NULL)
{
  sfactor= get_server () -> get_default_shrinking_factor ();
}

tm_window_rep::~tm_window_rep () {
  if (texmacs_menu != NULL) delete[] texmacs_menu;
  if (texmacs_icon_menu != NULL) delete[] texmacs_icon_menu;
  destroy_window_id (id);
}

void
tm_window_rep::set_window_name (string s) {
  get_main () << set_string ("window name", s);
}

/******************************************************************************
* Menus
******************************************************************************/

void
tm_window_rep::menu_main (string menu) {
  if (texmacs_menu == NULL) texmacs_menu= new object[1];
  object xmenu= call ("menu-expand", eval ("'" * menu));
  if (xmenu == texmacs_menu[0]) return;
  texmacs_menu[0]= xmenu;
  wk_widget w= make_menu_widget (xmenu);
  get_main () << set_widget ("menu bar", w);
}

void
tm_window_rep::menu_icons (int which, string menu) {
  if ((which<0) || (which>2)) return;
  if (texmacs_icon_menu == NULL) texmacs_icon_menu= new object[3];
  object xmenu= call ("menu-expand", eval ("'" * menu));
  if (xmenu == texmacs_icon_menu[which]) return;
  texmacs_icon_menu[which]= xmenu;
  string name= icon_bar_name (which);
  wk_widget w= make_menu_widget (xmenu);
  get_main () << set_widget (name * " icons bar", w);
}

void
tm_window_rep::set_header_flag (bool flag) {
  get_main () << set_string ("header", flag? string ("on"): string ("off"));
}

void
tm_window_rep::set_icon_bar_flag (int which, bool flag) {
  string name= icon_bar_name (which);
  get_main () <<
    set_string (name * " icons", flag? string ("on"): string ("off"));
}

bool
tm_window_rep::get_header_flag () {
  string s;
  get_main () << get_string ("header", s);
  return s == "on";
}

bool
tm_window_rep::get_icon_bar_flag (int which) {
  string name= icon_bar_name (which);
  string s;
  get_main () << get_string (name * " icons", s);
  return s == "on";
}

/******************************************************************************
* The canvas
******************************************************************************/

void
tm_window_rep::set_shrinking_factor (int sf) {
  sfactor= sf;
  get_main () << set_string ("shrinking factor", as_string (sf));
}

int
tm_window_rep::get_shrinking_factor () {
  return sfactor;
}

void
tm_window_rep::get_visible (SI& x1, SI& y1, SI& x2, SI& y2) {
  get_main () << ::get_visible (x1, y1, x2, y2);
}

void
tm_window_rep::get_extents (SI& x1, SI& y1, SI& x2, SI& y2) {
  get_main () << ::get_extents (x1, y1, x2, y2);
}

void
tm_window_rep::set_extents (SI x1, SI y1, SI x2, SI y2) {
  get_main () << ::set_extents (x1, y1, x2, y2);
}

void
tm_window_rep::set_scrollbars (int i) {
  get_main () << set_string ("scrollbars", as_string (i));
}

void
tm_window_rep::get_scroll_pos (SI& x, SI& y) {
  get_main () << ::get_scroll_pos (x, y);
}

void
tm_window_rep::set_scroll_pos (SI x, SI y) {
  get_main () << ::set_scroll_pos (x, y);
}

/******************************************************************************
* The footer and executing commands on the bottom line
******************************************************************************/

void
tm_window_rep::set_left_footer (string s) {
  get_main () << set_string ("left footer", s);
}

void
tm_window_rep::set_right_footer (string s) {
  get_main () << set_string ("right footer", s);
}

int
tm_window_rep::get_footer_mode () {
  string s;
  get_main () << get_string ("footer mode", s);
  return as_int (s);
}

void
tm_window_rep::set_footer_mode (int which) {
  get_main () << set_string ("footer mode", as_string (which));
}

bool
tm_window_rep::get_footer_flag () {
  string s;
  get_main () << get_string ("footer flag", s);
  return s == "on";
}

void
tm_window_rep::set_footer_flag (bool on) {
  get_main () <<
    set_string ("footer flag", on? string ("on"): string ("off"));
}

/******************************************************************************
* Interactive commands on the footer
******************************************************************************/

class ia_command_rep: public command_rep {
  tm_window_rep* win;
public:
  ia_command_rep (tm_window_rep* win2): win (win2) {}
  void apply () { win->interactive_return (); }
  ostream& print (ostream& out) { return out << "tm_window command"; }
};

void
tm_window_rep::interactive (string name, string type, array<string> def,
			    string& s, command cmd)
{
  if (get_footer_mode () == 1) { s= "cancel"; return; }
  text_ptr = &s;
  call_back= cmd;
  wk_widget tw = text_wk_widget (name, false, "english");
  wk_widget inp= input_text_wk_widget (new ia_command_rep (this), type, def);
  get_main () << set_widget ("interactive prompt", tw);
  get_main () << set_widget ("interactive input", inp);
  set_footer_mode (1);
}

void
tm_window_rep::interactive_return () {
  get_main () << get_string ("interactive input", *text_ptr);
  text_ptr= NULL;
  set_footer_mode (get_footer_flag ()? 0: 2);
  call_back ();
}
