
/******************************************************************************
* MODULE     : widkit_wrapper.hpp
* DESCRIPTION: Conversions between widget and wk_widget
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "promise.hpp"
#include "url.hpp"
#include "Widkit/wk_widget.hpp"
#include "message.hpp"
#include "window.hpp"

#define THIS wk_widget (this)

void send_geometry (wk_widget w, blackbox val);
blackbox query_geometry (wk_widget w, int type_id);

/******************************************************************************
* Type conversions
******************************************************************************/

array<widget>
abstract (array<wk_widget> a) {
  int i, n= N(a);
  array<widget> b (n);
  for (i=0; i<n; i++) b[i]= abstract (a[i]);
  return b;
}

array<wk_widget>
concrete (array<widget> a) {
  int i, n= N(a);
  array<wk_widget> b (n);
  for (i=0; i<n; i++) b[i]= concrete (a[i]);
  return b;
}

class abstract_promise_rep: public promise_rep<widget> {
  promise<wk_widget> p;
public:
  abstract_promise_rep (promise<wk_widget> p2): p (p2) {}
  ostream& print (ostream& out) { return out << p; }
  widget eval () { return abstract (p ()); }
};

promise<widget>
abstract (promise<wk_widget> pw) {
  return new abstract_promise_rep (pw);
}

class concrete_promise_rep: public promise_rep<wk_widget> {
  promise<widget> p;
public:
  concrete_promise_rep (promise<widget> p2): p (p2) {}
  ostream& print (ostream& out) { return out << p; }
  wk_widget eval () { return concrete (p ()); }
};

promise<wk_widget>
concrete (promise<widget> pw) {
  return new concrete_promise_rep (pw);
}

/******************************************************************************
* Exported special widgets
******************************************************************************/

widget
horizontal_list (array<widget> a) {
  return abstract (horizontal_list (concrete (a)));
}

widget
horizontal_list (array<widget> a, array<string> name) {
  return abstract (horizontal_list (concrete (a), name));
}

widget
vertical_list (array<widget> a) {
  return abstract (vertical_list (concrete (a)));
}

widget
vertical_list (array<widget> a, array<string> name) {
  return abstract (vertical_list (concrete (a), name));
}

widget
vertical_menu (array<widget> a) {
  return abstract (vertical_menu (concrete (a)));
}

widget
tile (array<widget> a, int cols) {
  return abstract (tile (concrete (a), cols));
}

widget
tile (array<widget> a, int cols, array<string> name) {
  return abstract (tile (concrete (a), cols, name));
}

widget
horizontal_array (array<widget> a, int stretch_me) {
  return abstract (horizontal_array (concrete (a), stretch_me));
}

widget
horizontal_array (array<widget> a, array<string> s, int stretch_me) {
  return abstract (horizontal_array (concrete (a), s, stretch_me));
}

widget
switch_widget (array<widget> a, array<string> name, int init) {
  return abstract (switch_widget (concrete (a), name, init));
}

widget
optional_widget (widget w, bool on) {
  return abstract (optional_widget (concrete (w), on));
}

widget
glue_widget (bool hx, bool vx, SI w, SI h) {
  return abstract (glue_wk_widget (hx, vx, w, h));
}

widget
separator_widget (SI pre, SI post, bool vert) {
  return abstract (separator_wk_widget (pre, post, vert));
}

widget
text_widget (string s, bool tsp, string lan) {
  return abstract (text_wk_widget (s, tsp, lan));
}

widget
menu_text_widget (string s, color col, string lan, bool tt) {
  return abstract (menu_text_wk_widget (s, col, lan, tt));
}

widget
xpm_widget (url file_name, bool transp) {
  return abstract (xpm_wk_widget (file_name, transp));
}

widget
command_button (widget w, command cmd, bool button_flag) {
  return abstract (command_button (concrete (w), cmd, button_flag));
}

widget
command_button (widget lw, widget rw, command cmd) {
  return abstract (command_button (concrete (lw), concrete (rw), cmd));
}

widget
command_button (widget lw, widget cw, widget rw, command cmd, bool e, bool c) {
  return abstract (command_button (concrete (lw), concrete (cw),
				   concrete (rw), cmd, e, c));
}

widget
pulldown_button (widget w, widget m, bool button_flag) {
  return abstract (pulldown_button (concrete (w), concrete (m), button_flag));
}

widget
pullright_button (widget w, widget m, bool button_flag) {
  return abstract (pullright_button (concrete (w), concrete (m), button_flag));
}

widget
pulldown_button (widget w, promise<widget> pw) {
  return abstract (pulldown_button (concrete (w), concrete (pw)));
}

widget
pullright_button (widget w, promise<widget> pw) {
  return abstract (pullright_button (concrete (w), concrete (pw)));
}

widget
popup_widget (widget w, gravity quit) {
  return abstract (popup_widget (concrete (w), quit));
}

widget
canvas_widget (widget w, gravity grav) {
  return abstract (canvas_widget (concrete (w), grav));
}

widget
input_text_widget (command call_back) {
  return abstract (input_text_wk_widget (call_back));
}

widget
input_text_widget (command call_back, string type, array<string> def) {
  return abstract (input_text_wk_widget (call_back, type, def));
}

widget
inputs_list_widget (command call_back, array<string> prompts) {
  return abstract (inputs_list_wk_widget (call_back, prompts));
}

widget
file_chooser_widget (command cmd, string type, string mgn) {
  return abstract (file_chooser_wk_widget (cmd, type, mgn));
}

widget
balloon_widget (widget w, widget help) {
  return abstract (balloon_widget (concrete (w), concrete (help)));
}

widget
wait_widget (SI w, SI h, string message) {
  return abstract (wait_wk_widget (w, h, message));
}

widget
texmacs_widget (int mask, command quit) {
  return abstract (texmacs_wk_widget (mask, quit));
}

widget
plain_window_widget (widget wid, char* s) {
  return abstract (plain_window_widget (concrete (wid), s));
}

widget
popup_window_widget (widget wid) {
  return abstract (popup_window_widget (concrete (wid)));
}

void
destroy_window_widget (widget w) {
  destroy_window_widget (concrete (w));
}

/******************************************************************************
* Helper routines for message passing
******************************************************************************/

void
send_bool (wk_widget w, string key, blackbox val) {
  if (type_box (val) != type_helper<bool>::id)
    fatal_error ("type mismatch", "send_bool");
  w << set_string (key, open_box<bool> (val)? string ("on"): string ("off"));
}

void
send_int (wk_widget w, string key, blackbox val) {
  if (type_box (val) != type_helper<int>::id)
    fatal_error ("type mismatch", "send_int");
  w << set_integer (key, open_box<int> (val));
}

void
send_string (wk_widget w, string key, blackbox val) {
  if (type_box (val) != type_helper<string>::id)
    fatal_error ("type mismatch", "send_string");
  w << set_string (key, open_box<string> (val));
}

void
send_coord2 (wk_widget w, string key, blackbox val) {
  typedef pair<SI,SI> coord2;
  if (type_box (val) != type_helper<coord2>::id)
    fatal_error ("type mismatch", "send_coord2");
  coord2 p= open_box<coord2> (val);
  w << set_coord2 (key, p.x1, p.x2);
}

void
send_coord4 (wk_widget w, string key, blackbox val) {
  typedef quadruple<SI,SI,SI,SI> coord4;
  if (type_box (val) != type_helper<coord4>::id)
    fatal_error ("type mismatch", "send_coord4");
  coord4 p= open_box<coord4> (val);
  w << set_coord4 (key, p.x1, p.x2, p.x3, p.x4);
}

void
send_position (wk_widget w, blackbox val) {
  typedef pair<SI,SI> coord2;
  typedef quintuple<SI,SI,SI,SI,gravity> geometry;
  if (type_box (val) != type_helper<coord2>::id)
    fatal_error ("type mismatch", "send_position");
  coord2 p= open_box<coord2> (val);
  if (w->is_window_widget ()) w->win->move (p.x1, p.x2);
  else {
    // FIXME: we should use coordinates relative to parent widget
    geometry g=
      open_box<geometry> (query_geometry (w, type_helper<geometry>::id));
    g.x1= p.x1; g.x2= p.x2;
    send_geometry (w, close_box<geometry> (g));
  }
}

void
send_size (wk_widget w, blackbox val) {
  typedef pair<SI,SI> coord2;
  typedef quintuple<SI,SI,SI,SI,gravity> geometry;
  if (type_box (val) != type_helper<coord2>::id)
    fatal_error ("type mismatch", "send_size");
  coord2 p= open_box<coord2> (val);
  if (w->is_window_widget ()) w->win->resize (p.x1, p.x2);
  else {
    geometry g=
      open_box<geometry> (query_geometry (w, type_helper<geometry>::id));
    g.x3= p.x1; g.x4= p.x2;
    send_geometry (w, close_box<geometry> (g));
  }
}

void
send_gravity (wk_widget w, blackbox val) {
  typedef quintuple<SI,SI,SI,SI,gravity> geometry;
  if (type_box (val) != type_helper<gravity>::id)
    fatal_error ("type mismatch", "send_gravity");
  geometry g=
    open_box<geometry> (query_geometry (w, type_helper<geometry>::id));
  g.x5= open_box<gravity> (val);
  send_geometry (w, close_box<geometry> (g));
}

void
send_geometry (wk_widget w, blackbox val) {
  typedef quintuple<SI,SI,SI,SI,gravity> geometry;
  if (type_box (val) != type_helper<geometry>::id)
    fatal_error ("type mismatch", "send_geometry");
  geometry g= open_box<geometry> (val);
  if (w->is_window_widget ()) {
    w->win->move (g.x1, g.x2);
    w->win->resize (g.x3, g.x4);
  }
  else {
    // FIXME: we should use coordinates relative to parent widget
    w << emit_position (g.x1, g.x2, g.x3, g.x4, g.x5);
  }
}

void
send_keyboard (wk_widget w, blackbox val) {
  typedef pair<string,time_t> keypress;
  if (type_box (val) != type_helper<keypress>::id)
    fatal_error ("type mismatch", "send_keyboard");
  keypress k= open_box<keypress> (val);
  w << emit_keypress (k.x1, k.x2);
}

void
send_keyboard_focus (wk_widget w, blackbox val) {
  typedef pair<bool,time_t> focus;
  if (type_box (val) != type_helper<focus>::id)
    fatal_error ("type mismatch", "send_keyboard_focus");
  focus f= open_box<focus> (val);
  w << emit_keyboard_focus (f.x1, f.x2);
}

void
send_mouse (wk_widget w, blackbox val) {
  typedef quintuple<string,int,int,time_t,int> mouse;
  if (type_box (val) != type_helper<mouse>::id)
    fatal_error ("type mismatch", "send_mouse");
  mouse m= open_box<mouse> (val);
  w << emit_mouse (m.x1, m.x2, m.x3, m.x4, m.x5);
}

void
send_invalidate_all (wk_widget w, blackbox val) {
  if (!nil (val))
    fatal_error ("type mismatch", "send_invalidate_all");
  w << emit_invalidate_all ();
}

void
send_invalidate (wk_widget w, blackbox val) {
  typedef quadruple<SI,SI,SI,SI> coord4;
  if (type_box (val) != type_helper<coord4>::id)
    fatal_error ("type mismatch", "send_invalidate");
  coord4 p= open_box<coord4> (val);
  w << emit_invalidate (w->ox + p.x1, w->oy + p.x2,
			w->ox + p.x3, w->oy + p.x4);
}

void
send_destroy (wk_widget w, blackbox val) {
  if (!nil (val))
    fatal_error ("type mismatch", "send_destroy");
  w << emit_destroy ();
}

blackbox
query_bool (wk_widget w, string key, int type_id) {
  if (type_id != type_helper<bool>::id)
    fatal_error ("type mismatch", "query_bool");
  string s;
  w << get_string (key, s);
  return close_box<bool> (s == "on");
}

blackbox
query_int (wk_widget w, string key, int type_id) {
  if (type_id != type_helper<int>::id)
    fatal_error ("type mismatch", "query_int");
  int i;
  w << get_integer (key, i);
  return close_box<int> (i);
}

blackbox
query_string (wk_widget w, string key, int type_id) {
  if (type_id != type_helper<string>::id)
    fatal_error ("type mismatch", "query_string");
  string s;
  w << get_string (key, s);
  return close_box<string> (s);
}

blackbox
query_coord2 (wk_widget w, string key, int type_id) {
  typedef pair<SI,SI> coord2;
  if (type_id != type_helper<coord2>::id)
    fatal_error ("type mismatch", "query_coord2");
  SI c1, c2;
  w << get_coord2 (key, c1, c2);
  return close_box<coord2> (coord2 (c1, c2));
}

blackbox
query_coord4 (wk_widget w, string key, int type_id) {
  typedef quadruple<SI,SI,SI,SI> coord4;
  if (type_id != type_helper<coord4>::id)
    fatal_error ("type mismatch", "query_coord4");
  SI c1, c2, c3, c4;
  w << get_coord4 (key, c1, c2, c3, c4);
  return close_box<coord4> (coord4 (c1, c2, c3, c4));
}

blackbox
query_size (wk_widget w, int type_id) {
  typedef pair<SI,SI> coord2;
  if (type_id != type_helper<coord2>::id)
    fatal_error ("type mismatch", "query_size");
  if (w->is_window_widget ()) {
    SI W, H;
    w->win->get_size (W, H);
    return close_box<coord2> (coord2 (W, H));
  }
  else return close_box<coord2> (coord2 (w->w, w->h));
}

blackbox
query_position (wk_widget w, int type_id) {
  typedef pair<SI,SI> coord2;
  if (type_id != type_helper<coord2>::id)
    fatal_error ("type mismatch", "query_position");
  if (w->is_window_widget ()) {
    SI x, y;
    w->win->get_position (x, y);
    return close_box<coord2> (coord2 (x, y));
  }
  else {
    // FIXME: we should use coordinates relative to parent widget
    return close_box<coord2> (coord2 (w->ox, w->oy));
  }
}

blackbox
query_gravity (wk_widget w, int type_id) {
  if (type_id != type_helper<gravity>::id)
    fatal_error ("type mismatch", "query_gravity");
  return close_box<gravity> (w->grav);
}

blackbox
query_geometry (wk_widget w, int type_id) {
  typedef quintuple<SI,SI,SI,SI,gravity> geometry;
  if (type_id != type_helper<geometry>::id)
    fatal_error ("type mismatch", "query_geometry");
  if (w->is_window_widget ()) {
    SI x, y, W, H;
    w->win->get_position (x, y);
    w->win->get_size (W, H);
    return close_box<geometry> (geometry (x, y, W, H, w->grav));
  }
  else {
    // FIXME: we should use coordinates relative to parent widget
    return close_box<geometry> (geometry (w->ox, w->oy, w->w, w->h, w->grav));
  }
}

template<class T> void
check_type (blackbox bb, string s) {
  if (type_box (bb) != type_helper<T>::id) {
    cerr << "\nslot type= " << s << "\n";
    fatal_error ("type mismatch", "check_type");
  }
}

void
check_type_void (blackbox bb, string s) {
  if (!nil (bb)) {
    cerr << "\nslot type= " << s << "\n";
    fatal_error ("type mismatch", "check_type");
  }
}

/******************************************************************************
* Message passing
******************************************************************************/

void
wk_widget_rep::send (slot s, blackbox val) {
  switch (s) {
  case SLOT_VISIBILITY:
    check_type<bool> (val, "SLOT_VISIBILITY");
    if (open_box<bool> (val)) win->map ();
    else win->unmap ();
    break;
  case SLOT_FULL_SCREEN:
    check_type<bool> (val, "SLOT_FULL_SCREEN");
    win->full_screen (open_box<bool> (val));
    break;
  case SLOT_NAME:
    send_string (THIS, "window name", val);
    break;
  case SLOT_SIZE:
    send_size (THIS, val);
    break;
  case SLOT_POSITION:
    send_position (THIS, val);
    break;
  case SLOT_GRAVITY:
    send_gravity (THIS, val);
    break;
  case SLOT_GEOMETRY:
    send_geometry (THIS, val);
    break;
  case SLOT_KEYBOARD:
    send_keyboard (THIS, val);
    break;
  case SLOT_KEYBOARD_FOCUS:
    send_keyboard_focus (THIS, val);
    break;
  case SLOT_MOUSE:
    send_mouse (THIS, val);
    break;
  case SLOT_INVALIDATE_ALL:
    send_invalidate_all (THIS, val);
    break;
  case SLOT_INVALIDATE:
    send_invalidate (THIS, val);
    break;
  case SLOT_DESTROY:
    send_destroy (THIS, val);
    break;

  case SLOT_SHRINKING_FACTOR:
    send_int (THIS, "shrinking factor", val);
    break;
  case SLOT_EXTENTS:
    send_coord4 (THIS, "extents", val);
    break;
  case SLOT_SCROLLBARS_VISIBILITY:
    send_int (THIS, "scrollbars", val);
    break;
  case SLOT_SCROLL_POSITION:
    send_coord2 (THIS, "scroll position", val);
    break;

  case SLOT_HEADER_VISIBILITY:
    send_bool (THIS, "header", val);
    break;
  case SLOT_MAIN_ICONS_VISIBILITY:
    send_bool (THIS, "main icons", val);
    break;
  case SLOT_CONTEXT_ICONS_VISIBILITY:
    send_bool (THIS, "context icons", val);
    break;
  case SLOT_USER_ICONS_VISIBILITY:
    send_bool (THIS, "user icons", val);
    break;
  case SLOT_FOOTER_VISIBILITY:
    send_bool (THIS, "footer flag", val);
    break;
  case SLOT_LEFT_FOOTER:
    send_string (THIS, "left footer", val);
    break;
  case SLOT_RIGHT_FOOTER:
    send_string (THIS, "right footer", val);
    break;
  case SLOT_INTERACTIVE_MODE:
    send_bool (THIS, "interactive mode", val);
    break;

  case SLOT_STRING_INPUT:
    send_string (THIS, "input", val);
    break;
  case SLOT_INPUT_TYPE:
    send_string (THIS, "type", val);
    break;
  case SLOT_INPUT_PROPOSAL:
    send_string (THIS, "default", val);
    break;
  case SLOT_FILE:
    send_string (THIS, "file", val);
    break;
  case SLOT_DIRECTORY:
    send_string (THIS, "directory", val);
    break;
  default:
    fatal_error ("cannot handle slot type", "wk_widget_rep::send");
  }
}

blackbox
wk_widget_rep::query (slot s, int type_id) {
  switch (s) {
  case SLOT_PS_DEVICE:
    if (type_id != type_helper<ps_device>::id)
      fatal_error ("ps_device expected", "wk_widget_rep::query");
    return close_box<ps_device> ((ps_device) win);
  case SLOT_SIZE:
    return query_size (THIS, type_id);
  case SLOT_POSITION:
    return query_position (THIS, type_id);
  case SLOT_GRAVITY:
    return query_gravity (THIS, type_id);
  case SLOT_GEOMETRY:
    return query_geometry (THIS, type_id);
  case SLOT_INVALID:
    fatal_error ("not yet implemented", "wk_widget_rep::query");

  case SLOT_EXTENTS:
    return query_coord4 (THIS, "extents", type_id);
  case SLOT_VISIBLE_PART:
    return query_coord4 (THIS, "visible", type_id);
  case SLOT_SCROLLBARS_VISIBILITY:
    return query_int (THIS, "scrollbars", type_id);
  case SLOT_SCROLL_POSITION:
    return query_coord2 (THIS, "scroll position", type_id);

  case SLOT_HEADER_VISIBILITY:
    return query_bool (THIS, "header", type_id);
  case SLOT_MAIN_ICONS_VISIBILITY:
    return query_bool (THIS, "main icons", type_id);
  case SLOT_CONTEXT_ICONS_VISIBILITY:
    return query_bool (THIS, "context icons", type_id);
  case SLOT_USER_ICONS_VISIBILITY:
    return query_bool (THIS, "user icons", type_id);
  case SLOT_FOOTER_VISIBILITY:
    return query_bool (THIS, "footer flag", type_id);
  case SLOT_INTERACTIVE_MODE:
    return query_bool (THIS, "interactive mode", type_id);
  case SLOT_INTERACTIVE_INPUT:
    return query_string (THIS, "interactive input", type_id);

  case SLOT_STRING_INPUT:
    return query_string (THIS, "input", type_id);
  default:
    fatal_error ("cannot handle slot type", "wk_widget_rep::query");
    return blackbox ();
  }
}

widget
wk_widget_rep::read (slot s, blackbox index) {
  switch (s) {
  case SLOT_WINDOW:
    check_type_void (index, "SLOT_WINDOW");
    return win -> get_widget ();
  case SLOT_FORM_FIELD:
    check_type<int> (index, "SLOT_FORM_FIELD");
    return abstract (THIS [0] ["inputs"] [open_box<int> (index)] ["input"]);
  case SLOT_FILE:
    check_type_void (index, "SLOT_FILE");
    return abstract (THIS [0] ["file"] ["input"]);
  case SLOT_DIRECTORY:
    check_type_void (index, "SLOT_DIRECTORY");
    return abstract (THIS [0] ["directory"] ["input"]);
  default:
    fatal_error ("cannot handle slot type", "wk_widget_rep::read");
  }
}

void
wk_widget_rep::write (slot s, blackbox index, widget w) {
  switch (s) {
  case SLOT_MAIN_MENU:
    check_type_void (index, "SLOT_MAIN_MENU");
    THIS << set_widget ("menu bar", concrete (w));
    break;
  case SLOT_MAIN_ICONS:
    check_type_void (index, "SLOT_MAIN_ICONS");
    THIS << set_widget ("main icons bar", concrete (w));
    break;
  case SLOT_CONTEXT_ICONS:
    check_type_void (index, "SLOT_CONTEXT_ICONS");
    THIS << set_widget ("context icons bar", concrete (w));
    break;
  case SLOT_USER_ICONS:
    check_type_void (index, "SLOT_USER_ICONS");
    THIS << set_widget ("user icons bar", concrete (w));
    break;
  case SLOT_CANVAS:
    check_type_void (index, "SLOT_CANVAS");
    THIS << set_widget ("scrollable", concrete (w));
    break;
  case SLOT_INTERACTIVE_PROMPT:
    check_type_void (index, "SLOT_INTERACTIVE_PROMPT");
    THIS << set_widget ("interactive prompt", concrete (w));
    break;
  case SLOT_INTERACTIVE_INPUT:
    check_type_void (index, "SLOT_INTERACTIVE_INPUT");
    THIS << set_widget ("interactive input", concrete (w));
    break;
  default:
    fatal_error ("cannot handle slot type", "wk_widget_rep::write");
  }
}
