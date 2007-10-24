
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
#include "tm_window.hpp"

widget make_menu_widget (object menu);
string icon_bar_name (int which);

/******************************************************************************
* Meta editor constructor and destructor
******************************************************************************/

static int tm_window_serial= 0;

tm_window_rep::tm_window_rep (widget wid2, tree geom):
  win (texmacs_window (wid2, geom)),
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

/******************************************************************************
* Meta mathods
******************************************************************************/

void
tm_window_rep::set_window_name (string s) {
  wid -> set<string> ("window name", s);
}

void
tm_window_rep::map () {
  win->map ();
}

void
tm_window_rep::unmap () {
  win->unmap ();
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
  widget w= make_menu_widget (xmenu);
  wid -> set<widget> ("menu bar", w);
}

void
tm_window_rep::menu_icons (int which, string menu) {
  if ((which<0) || (which>2)) return;
  if (texmacs_icon_menu == NULL) texmacs_icon_menu= new object[3];
  object xmenu= call ("menu-expand", eval ("'" * menu));
  if (xmenu == texmacs_icon_menu[which]) return;
  texmacs_icon_menu[which]= xmenu;
  string name= icon_bar_name (which);
  widget w= make_menu_widget (xmenu);
  wid -> set<widget> (name * " icons bar", w);
}

void
tm_window_rep::set_header_flag (bool flag) {
  wid -> set<string> ("header", flag? string ("on"): string ("off"));
}

void
tm_window_rep::set_icon_bar_flag (int which, bool flag) {
  string name= icon_bar_name (which);
  wid -> set<string> (name * " icons", flag? string ("on"): string ("off"));
}

bool
tm_window_rep::get_header_flag () {
  return wid -> get<string> ("header") == "on";
}

bool
tm_window_rep::get_icon_bar_flag (int which) {
  string name= icon_bar_name (which);
  return wid -> get<string> (name * " icons") == "on";
}

/******************************************************************************
* The canvas
******************************************************************************/

void
tm_window_rep::set_shrinking_factor (int sf) {
  sfactor= sf;
  wid -> set<string> ("shrinking factor", as_string (sf));
}

int
tm_window_rep::get_shrinking_factor () {
  return sfactor;
}

void
tm_window_rep::get_visible (SI& x1, SI& y1, SI& x2, SI& y2) {
  wid -> get<SI,SI,SI,SI> ("visible", x1, y1, x2, y2);
}

void
tm_window_rep::get_extents (SI& x1, SI& y1, SI& x2, SI& y2) {
  wid -> get<SI,SI,SI,SI> ("extents", x1, y1, x2, y2);
}

void
tm_window_rep::set_extents (SI x1, SI y1, SI x2, SI y2) {
  wid -> set<SI,SI,SI,SI> ("extents", x1, y1, x2, y2);
}

void
tm_window_rep::set_scrollbars (int i) {
  wid -> set<string> ("scrollbars", as_string (i));
}

void
tm_window_rep::get_scroll_pos (SI& x, SI& y) {
  wid -> get<SI,SI> ("scroll position", x, y);
}

void
tm_window_rep::set_scroll_pos (SI x, SI y) {
  wid -> set<SI,SI> ("scroll position", x, y);
}

/******************************************************************************
* The footer and executing commands on the bottom line
******************************************************************************/

void
tm_window_rep::set_left_footer (string s) {
  wid -> set<string> ("left footer", s);
}

void
tm_window_rep::set_right_footer (string s) {
  wid -> set<string> ("right footer", s);
}

int
tm_window_rep::get_footer_mode () {
  return as_int (wid -> get<string> ("footer mode"));
}

void
tm_window_rep::set_footer_mode (int which) {
  wid -> set<string> ("footer mode", as_string (which));
}

bool
tm_window_rep::get_footer_flag () {
  return wid -> get<string> ("footer flag") == "on";
}

void
tm_window_rep::set_footer_flag (bool on) {
  wid -> set<string> ("footer flag", on? string ("on"): string ("off"));
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
  widget tw = text_widget (name, false, "english");
  widget inp= input_text_widget (new ia_command_rep (this), type, def);
  wid -> set<widget> ("interactive prompt", tw);
  wid -> set<widget> ("interactive input", inp);
  set_footer_mode (1);
}

void
tm_window_rep::interactive_return () {
  *text_ptr= wid -> get<string> ("interactive input");
  text_ptr= NULL;
  set_footer_mode (get_footer_flag ()? 0: 2);
  call_back ();
}
