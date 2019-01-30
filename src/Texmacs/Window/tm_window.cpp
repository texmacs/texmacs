
/******************************************************************************
* MODULE     : tm_window.cpp
* DESCRIPTION: Main TeXmacs windows
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_window.hpp"
#include "tm_data.hpp"
#include "message.hpp"
#include "dictionary.hpp"
#include "merge_sort.hpp"
#include "iterator.hpp"
#include "boot.hpp"

int geometry_w= 800, geometry_h= 600;
int geometry_x= 0  , geometry_y= 0;

widget texmacs_window_widget (widget wid, tree geom);
widget make_menu_widget (object menu);
void refresh_size (widget wid, bool exact);

static int last_window_handle= 0;
static hashmap<int,widget> window_table (NULL);
static hashmap<tree,path> window_by_name;
static time_t refresh_time= 0;

/******************************************************************************
* User preference management concerning the geometry of windows
******************************************************************************/

hashmap<string,string> window_names ("");

string
unique_window_name (string name) {
  for (int i=1; true; i++) {
    string wname= name;
    if (i > 1) wname= wname * ":" * as_string (i);
    if (!window_names->contains (wname)) {
      window_names (wname)= name;
      return wname;
    }
  }
}

static bool
move_accept (int old_x, int old_y, int x, int y) {
  if (old_x == x && old_y == y) return false;
  return true;
}

void
notify_window_move (string name, SI xx, SI yy) {
  int x=  xx / PIXEL;
  int y= -yy / PIXEL;
  if (name != "popup") {
    //cout << "Move " << name << " to " << x << ", " << y << "\n";
    string old_x= get_user_preference ("abscissa " * name, "");
    string old_y= get_user_preference ("ordinate " * name, "");
    //cout << "Move " << name << ": " << old_x << ", " << old_y
    //<< " --> " << x << ", " << y << "\n";
    if (old_x == "" || old_y == "" ||
        move_accept (as_int (old_x), as_int (old_y), x, y)) {
      set_user_preference ("abscissa " * name, as_string (x));
      set_user_preference ("ordinate " * name, as_string (y));
    }
  }
}

static bool
resize_accept (int old_w, int old_h, int w, int h) {
  if (old_w == w && old_h == h) return false;
#ifdef QTTEXMACS
  if (old_w == w && old_h == h + 41) return false;
  if (old_w == w && old_h == h + 24) return false;
  if (old_w == w && old_h == h + 22) return false;
  if (old_w == w && old_h == h + 16) return false;
  if (old_w == w && old_h == h - 40) return false;
#endif
  return true;
}

void
notify_window_resize (string name, SI ww, SI hh) {
  int w= ww / PIXEL;
  int h= hh / PIXEL;
  if (name != "popup") {
    //cout << "Resize " << name << " to " << ww << ", " << hh << "\n";
    string old_w= get_user_preference ("width " * name, "");
    string old_h= get_user_preference ("height " * name, "");
    //cout << "Resize " << name << ": " << old_w << ", " << old_h
    //<< " --> " << w << ", " << h << "\n";
    if (old_w == "" || old_h == "" ||
        resize_accept (as_int (old_w), as_int (old_h), w, h)) {
      set_user_preference ("width " * name, as_string (w));
      set_user_preference ("height " * name, as_string (h));
    }
  }
}

void
notify_window_destroy (string name) {
  window_names->reset (name);
}

void
get_preferred_position (string name, SI& xx, SI& yy) {
  if (has_user_preference ("abscissa " * name)) {
    int x= as_int (get_user_preference ("abscissa " * name));
    int y= as_int (get_user_preference ("ordinate " * name));
    
    xx=  x * PIXEL;
    yy= -y * PIXEL;
  }
}

void
get_preferred_size (string name, SI& ww, SI& hh) {
  if (has_user_preference ("width " * name)) {
    int w= as_int (get_user_preference ("width " * name));
    int h= as_int (get_user_preference ("height " * name));
    ww= w * PIXEL;
    hh= h * PIXEL;
    //cout << "Size " << name << ": " << w << ", " << h << "\n";
  }
}

/******************************************************************************
* Meta editor constructor and destructor
******************************************************************************/

static int tm_window_serial= 0;

tm_window_rep::tm_window_rep (widget wid2, tree geom):
  win (texmacs_window_widget (wid2, geom)),
  wid (wid2), id (create_window_id ()),
  serial (tm_window_serial++),
  menu_current (object ()), menu_cache (widget ()),
  text_ptr (NULL)
{
  zoomf= get_server () -> get_default_zoom_factor ();
}

tm_window_rep::tm_window_rep (tree doc, command quit):
  win (texmacs_widget (0, quit)),
  wid (win), id (url_none ()),
  serial (tm_window_serial++),
  menu_current (object ()), menu_cache (widget ()),
  text_ptr (NULL)
{
  (void) doc;
  zoomf= get_server () -> get_default_zoom_factor ();
}

tm_window_rep::~tm_window_rep () {
  if (!is_none (id)) destroy_window_id (id);
}

/******************************************************************************
* Creation of TeXmacs window
******************************************************************************/

widget
texmacs_window_widget (widget wid, tree geom) {
  int W, H;
  int w= geometry_w, h= geometry_h;
  int x= geometry_x, y= geometry_y;
  bool custom= is_tuple (geom) && N (geom) >= 2;
#ifndef QTTEXMACS
  if (use_side_tools) { w += 200; h += 100; }
#endif
  if (custom) {
    w= as_int (geom[0]);
    h= as_int (geom[1]);
  }
  gui_root_extents (W, H); W /= PIXEL; H /= PIXEL;
  if (x < 0) x= W + x + 1 - w;
  if (y < 0) y= H + y + 1 - h;
  string name= "TeXmacs";
  name= unique_window_name (name);
  widget win= plain_window_widget (wid, name);
  SI xx= x * PIXEL, yy= -y * PIXEL;
  SI ww= w * PIXEL, hh=  h * PIXEL;
  if (!custom) {
    get_preferred_position (name, xx, yy);
    get_preferred_size (name, ww, hh);
  }
  set_size (win, ww, hh);
  set_position (win, xx, yy);
  return win;
}

/******************************************************************************
* Closing embedded TeXmacs widgets
******************************************************************************/

class close_embedded_command_rep: public command_rep {
  tm_view vw;
  url name;
  int win_id;
public:
  close_embedded_command_rep (tm_view vw2, url n2, int win2):
    vw (vw2), name (n2), win_id (win2) {
      window_by_name(name->t)= path (win_id, window_by_name[name->t]); }
  void apply ();
  tm_ostream& print (tm_ostream& out) {
    return out << "Close_Embedded widget command"; }
};

void
close_embedded_command_rep::apply () {
  //cout << "Destroy " << vw->buf->buf->name << "\n";
  ASSERT (!is_nil(vw->ed), "embedded command acting on deleted editor");
  url foc= abstract_window (vw->ed->mvw->win);
  if (is_none (foc)) {
    array<url> a= windows_list ();
    ASSERT (N(a) != 0, "no remaining windows");
    foc= a[0];
  }
  window_focus (foc);
  //cout << "Changed focus\n";
  tm_window win= vw->win;
  ASSERT (N (buffer_to_views (vw->buf->buf->name)) == 1,
          "invalid cloned embedded TeXmacs widget");
  window_by_name(name->t)= remove<int> (window_by_name[name->t], win_id);
  remove_buffer (vw->buf->buf->name);
  //cout << "Deleted buffer\n";
  tm_delete (win);
  //cout << "Deleted window\n";
}

command
close_embedded_command (tm_view vw, url name, int win) {
  return tm_new<close_embedded_command_rep> (vw, name, win);
}

static path
filter_existing (path wins) {
  if (is_nil (wins)) return wins;
  if (window_table->contains (wins->item))
    return path (wins->item, filter_existing (wins->next));
  return filter_existing (wins->next);
}

path
window_search (url name) {
  return filter_existing (window_by_name[name->t]);
}

bool
is_embedded_buffer (url name) {
  return !is_nil (window_search (name));
}

/******************************************************************************
* Embedded TeXmacs widgets
******************************************************************************/

url
embedded_name (url name) {
  static int nr= 0;
  if (!is_none (name)) return name;
  nr++;
  return url (string ("tmfs://aux/TeXmacs-input-" * as_string (nr)));
}

tree
enrich_embedded_document (tree body, tree style) {
  tree orig= body;
  if (is_func (body, WITH)) body= body[N(body)-1];
  if (!is_func (body, DOCUMENT)) body= tree (DOCUMENT, body);
  hashmap<string,tree> initial (UNINIT);
  initial (PAGE_MEDIUM)= "automatic";
  initial (PAGE_SCREEN_LEFT)= "4px";
  initial (PAGE_SCREEN_RIGHT)= "4px";
  initial (PAGE_SCREEN_TOP)= "2px";
  initial (PAGE_SCREEN_BOT)= "2px";
  if (is_func (orig, WITH))
    for (int i=0; i+2<N(orig); i+=2)
      if (is_atomic (orig[i]))
        initial (orig[i]->label)= orig[i+1];
  initial (DPI)= "720";
  initial (ZOOM_FACTOR)= "1.2";
  initial ("no-zoom")= "true";
  tree doc (DOCUMENT);
  doc << compound ("TeXmacs", TEXMACS_VERSION);
  doc << style; //compound ("style", style);
  doc << compound ("body", body);
  doc << compound ("initial", make_collection (initial));
  return doc;
}

widget
texmacs_input_widget (tree doc, tree style, url wname) {
  doc= enrich_embedded_document (doc, style);
  url       base = get_master_buffer (get_current_buffer ());
  tm_view   curvw= concrete_view (get_current_view ());
  url       name = embedded_name (wname);
  if (contains (name, get_all_buffers ())) set_buffer_tree (name, doc);
  else create_buffer (name, doc);
  tm_view   vw   = concrete_view (get_passive_view (name));
  tm_window win  = tm_new<tm_window_rep> (doc, command ());
  set_master_buffer (name, base);
  vw->win= win;
  set_scrollable (win->wid, vw->ed);
  vw->ed->cvw= win->wid.rep;
  vw->ed->mvw= curvw;
  command close_cmd= close_embedded_command (vw, name, last_window_handle);
  return wrapped_widget (win->wid, close_cmd);
}

/******************************************************************************
* Meta mathods
******************************************************************************/

void
tm_window_rep::set_window_name (string s) {
  if (cur_title != s) {
    cur_title= s;
    set_name (wid, s);
  }
}

void
tm_window_rep::set_modified (bool flag) {
  ::set_modified (wid, flag);
}

void
tm_window_rep::set_window_url (url u) {
  if (!is_none (u)) set_file (wid, as_string (u));
}

void
tm_window_rep::map () {
  set_visibility (win, true);
}

void
tm_window_rep::unmap () {
  set_visibility (win, false);
}

void
tm_window_rep::refresh () {
  menu_cache= hashmap<object,widget> (widget ());
}

/******************************************************************************
* Menus
******************************************************************************/

bool menu_caching= true;

bool
tm_window_rep::get_menu_widget (int which, string menu, widget& w) {
  object xmenu= call ("menu-expand", eval ("'" * menu));
  //cout << "xmenu= " << xmenu << "\n";
  if (menu_cache->contains (xmenu)) {
    //if (menu_current[which] == xmenu) cout << "Same " << menu << "\n";
    if (menu_current[which] == xmenu) return false;
    menu_current (which)= xmenu;
    //cout << "Cached " << menu << "\n";
    w= menu_cache [xmenu];
    return true;
  }
  menu_current (which)= xmenu;
  //cout << "Compute " << menu << "\n";
  object umenu= eval ("'" * menu);
  w= make_menu_widget (umenu);
  if (menu_caching)
    if (as_bool (call ("cache-menu?", xmenu)))
      menu_cache (xmenu)= w;
  return true;
}

void
tm_window_rep::menu_main (string menu) {
  eval ("(lazy-initialize-force)");
  widget w;
  if (get_menu_widget (-1, menu, w))
    ::set_main_menu (wid, w);
}

void
tm_window_rep::menu_icons (int which, string menu) {
  eval ("(lazy-initialize-force)");
  widget w;
  if (get_menu_widget (which, menu, w)) {
    if      (which == 0) set_main_icons (wid, w);
    else if (which == 1) set_mode_icons (wid, w);
    else if (which == 2) set_focus_icons (wid, w);
    else if (which == 3) set_user_icons (wid, w);
  }
}

void
tm_window_rep::side_tools (int which, string tools) {
  eval ("(lazy-initialize-force)");
  widget w;
  if (get_menu_widget (10 + which, tools, w)) {
    if (which == 0) set_side_tools (wid, w);
  }
}

void
tm_window_rep::bottom_tools (int which, string tools) {
  eval ("(lazy-initialize-force)");
  widget w;
  if (get_menu_widget (20 + which, tools, w)) {
    if (which == 0) set_bottom_tools (wid, w);
  }
}

void
tm_window_rep::set_header_flag (bool flag) {
  set_header_visibility (wid, flag);
}

void
tm_window_rep::set_icon_bar_flag (int which, bool flag) {
  if      (which == 0) set_main_icons_visibility (wid, flag);
  else if (which == 1) set_mode_icons_visibility (wid, flag);
  else if (which == 2) set_focus_icons_visibility (wid, flag);
  else if (which == 3) set_user_icons_visibility (wid, flag);
}

void
tm_window_rep::set_side_tools_flag (int which, bool flag) {
  if (which == 0) set_side_tools_visibility (wid, flag);
}

void
tm_window_rep::set_bottom_tools_flag (int which, bool flag) {
  if (which == 0) set_bottom_tools_visibility (wid, flag);
}

bool
tm_window_rep::get_header_flag () {
  return get_header_visibility (wid);
}

bool
tm_window_rep::get_icon_bar_flag (int which) {
  if      (which == 0) return get_main_icons_visibility (wid);
  else if (which == 1) return get_mode_icons_visibility (wid);
  else if (which == 2) return get_focus_icons_visibility (wid);
  else if (which == 3) return get_user_icons_visibility (wid);
  else return false;
}

bool
tm_window_rep::get_side_tools_flag (int which) {
  if (which == 0) return get_side_tools_visibility (wid);
  else return false;
}

bool
tm_window_rep::get_bottom_tools_flag (int which) {
  if (which == 0) return get_bottom_tools_visibility (wid);
  else return false;
}

/******************************************************************************
* The canvas
******************************************************************************/

void
tm_window_rep::set_window_zoom_factor (double zoom) {
  zoomf= zoom;
  ::set_zoom_factor (wid, zoom);
}

double
tm_window_rep::get_window_zoom_factor () {
  return zoomf;
}

void
tm_window_rep::get_visible (SI& x1, SI& y1, SI& x2, SI& y2) {
  get_visible_part (wid, x1, y1, x2, y2);
}

void
tm_window_rep::get_extents (SI& x1, SI& y1, SI& x2, SI& y2) {
  ::get_extents (wid, x1, y1, x2, y2);
}

void
tm_window_rep::set_extents (SI x1, SI y1, SI x2, SI y2) {
  ::set_extents (wid, x1, y1, x2, y2);
}

void
tm_window_rep::set_scrollbars (int i) {
  ::set_scrollbars_visibility (wid, i);
}

void
tm_window_rep::get_scroll_pos (SI& x, SI& y) {
  get_scroll_position (wid, x, y);
}

void
tm_window_rep::set_scroll_pos (SI x, SI y) {
  set_scroll_position (wid, x, y);
}

/******************************************************************************
* The footer as a status bar
******************************************************************************/

bool
tm_window_rep::get_footer_flag () {
  return get_footer_visibility (wid);
}

void
tm_window_rep::set_footer_flag (bool flag) {
  set_footer_visibility (wid, flag);
}

void
tm_window_rep::set_left_footer (string s) {
  ::set_left_footer (wid, s);
}

void
tm_window_rep::set_right_footer (string s) {
  ::set_right_footer (wid, s);
}

/******************************************************************************
* Interactive commands on the footer
******************************************************************************/

class ia_command_rep: public command_rep {
  tm_window_rep* win;
public:
  ia_command_rep (tm_window_rep* win2): win (win2) {}
  void apply () { win->interactive_return (); }
  tm_ostream& print (tm_ostream& out) { return out << "tm_window command"; }
};

bool
tm_window_rep::get_interactive_mode () {
  return ::get_interactive_mode (wid);
}

void
tm_window_rep::set_interactive_mode (bool flag) {
  ::set_interactive_mode (wid, flag);
}

void
tm_window_rep::interactive (string name, string type, array<string> def,
			    string& s, command cmd)
{
  if (get_interactive_mode ()) { s= "cancel"; return; }
  text_ptr = &s;
  call_back= cmd;
  widget tw = text_widget (translate (name), 0, black, false);
  widget inp= input_text_widget (tm_new<ia_command_rep> (this), type, def,
                                 WIDGET_STYLE_MINI);
  set_interactive_prompt (wid, tw);
  set_interactive_input (wid, inp);
  set_interactive_mode (true);
}

void
tm_window_rep::interactive_return () {
  *text_ptr= get_interactive_input (wid);
  text_ptr= NULL;
  set_interactive_mode (false);
  call_back ();
}

/******************************************************************************
* Other top level windows
******************************************************************************/

int
window_handle () {
  static int window_next= 1;
  last_window_handle= window_next;
  return window_next++;
}

void
window_create (int win, widget wid, string name, bool plain) {
  widget pww;
  if (plain)
    pww= plain_window_widget (wid, name);
  else
    pww= popup_window_widget (wid, name);
  window_table (win)= pww;
}

void
window_create (int win, widget wid, string name, command quit) {
  widget pww;
  pww= plain_window_widget (wid, name, quit);
  window_table (win)= pww;
}

/*
FIXME: this old implementation does not work in the presence
of texmacs_input widgets.  The current hack remedies this situation
by explicitly signalling the widget destruction slot before
the actual destruction of the widget.  This is still not sufficient
in the case of Qt though and also might cause the desruction slot
to be signalled twice.

void
window_delete (int win) {
  ASSERT (window_table->contains (win), "window does not exist");
  widget pww= window_table [win];
  window_table->reset (win);
  destroy_window_widget (pww);
}
*/

void
window_delete (int win) {
  static hashmap<int,bool> busy (false);
  if (busy->contains (win)) return;
  busy (win)= true;
  ASSERT (window_table->contains (win), "window does not exist");
  widget pww= window_table [win];
  window_table->reset (win);
  send_destroy (pww);
  destroy_window_widget (pww);
  busy (win)= false;
}

void
window_show (int win) {
  ASSERT (window_table->contains (win), "window does not exist");
  widget pww= window_table [win];
  set_visibility (pww, true);
}

void
window_hide (int win) {
  ASSERT (window_table->contains (win), "window does not exist");
  widget pww= window_table [win];
  set_visibility (pww, false);
}

scheme_tree
window_get_size (int win) {
  ASSERT (window_table->contains (win), "window does not exist");
  widget pww= window_table [win];
  int w, h;
  get_size(pww, w, h);
  return tuple (as_string (w/PIXEL), as_string (h/PIXEL));
}

void
window_set_size (int win, int w, int h) {
  ASSERT (window_table->contains (win), "window does not exist");
  widget pww= window_table [win];
  set_size (pww, w*PIXEL, h*PIXEL);
}

scheme_tree
window_get_position (int win) {
  ASSERT (window_table->contains (win), "window does not exist");
  widget pww= window_table [win];
  int x, y;
  get_position(pww, x, y);
  return tuple (as_string (x/PIXEL), as_string (y/PIXEL));
}

void
window_set_position (int win, int x, int y) {
  ASSERT (window_table->contains (win), "window does not exist");
  widget pww= window_table [win];
  set_position (pww, x*PIXEL, y*PIXEL);
}

void
windows_delayed_refresh (int ms) {
  refresh_time= texmacs_time () + ms;
}

void
windows_refresh (string kind) {
  if (kind == "auto" && texmacs_time () < refresh_time) return;
  iterator<int> it= iterate (window_table);
  while (it->busy ()) {
    int id= it->next ();
    send_refresh (window_table[id], kind);
#ifdef X11TEXMACS
    if (kind == "auto") refresh_size (window_table[id], false);
#endif
  }
  if (kind == "auto") windows_delayed_refresh (1000000000);
}
