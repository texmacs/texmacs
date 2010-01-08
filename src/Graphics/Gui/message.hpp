
/******************************************************************************
* MODULE     : message.hpp
* DESCRIPTION: Messages to and between widgets
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef MESSAGE_H
#define MESSAGE_H
#include "renderer.hpp"
#include "widget.hpp"
#include "ntuple.hpp"
#include "rectangles.hpp"

/******************************************************************************
* C++ stupidity does not allow forward declaration of enums.
* Slots should really be defined in message.hpp
******************************************************************************/

enum slot_id {
  SLOT_IDENTIFIER,
  SLOT_WINDOW,
  SLOT_RENDERER,
  SLOT_VISIBILITY,
  SLOT_FULL_SCREEN,
  SLOT_NAME,
  SLOT_SIZE,
  SLOT_POSITION,
  SLOT_UPDATE,
  SLOT_KEYBOARD,
  SLOT_KEYBOARD_FOCUS,
  SLOT_MOUSE,
  SLOT_MOUSE_GRAB,
  SLOT_MOUSE_POINTER,
  SLOT_INVALIDATE,
  SLOT_INVALIDATE_ALL,
  SLOT_REPAINT,
  SLOT_DELAYED_MESSAGE,
  SLOT_DESTROY,

  SLOT_SHRINKING_FACTOR,
  SLOT_EXTENTS,
  SLOT_VISIBLE_PART,
  SLOT_SCROLLBARS_VISIBILITY,
  SLOT_SCROLL_POSITION,
  SLOT_CANVAS,

  SLOT_HEADER_VISIBILITY,
  SLOT_MAIN_MENU,
  SLOT_MAIN_ICONS_VISIBILITY,
  SLOT_MAIN_ICONS,
  SLOT_CONTEXT_ICONS_VISIBILITY,
  SLOT_CONTEXT_ICONS,
  SLOT_USER_ICONS_VISIBILITY,
  SLOT_USER_ICONS,
  SLOT_FOOTER_VISIBILITY,
  SLOT_LEFT_FOOTER,
  SLOT_RIGHT_FOOTER,
  SLOT_INTERACTIVE_MODE,
  SLOT_INTERACTIVE_PROMPT,
  SLOT_INTERACTIVE_INPUT,

  SLOT_FORM_FIELD,
  SLOT_STRING_INPUT,
  SLOT_INPUT_TYPE,
  SLOT_INPUT_PROPOSAL,
  SLOT_FILE,
  SLOT_DIRECTORY
};

class slot {
public:
  slot_id sid;
  inline slot (slot_id sid2): sid (sid2) {}
  inline slot (const slot& s): sid (s.sid) {}
  inline slot& operator = (slot s) { sid= s.sid; return *this; }
  inline operator slot_id () { return sid; }
  inline bool operator == (slot_id sid2) { return sid == sid2; }
  inline bool operator != (slot_id sid2) { return sid != sid2; }
  inline bool operator == (slot s) { return sid == s.sid; }
  inline bool operator != (slot s) { return sid != s.sid; }
  inline friend tm_ostream& operator << (tm_ostream& out, slot s) {
    return out << s.sid; }
};

const char * slot_name (const slot s);

//extern bool* slot_state_table;
//inline bool is_state_slot (slot s) { return slots_state_table[s]; }

/******************************************************************************
* Helper templates for sending messages
******************************************************************************/

inline void
send (widget w, slot s) {
  w->send (s, blackbox ());
}

template<class T1> inline void
send (widget w, slot s, T1 val) {
  w->send (s, close_box (val));
}

template<class T1, class T2> void
send (widget w, slot s, T1 val1, T2 val2) {
  typedef pair<T1,T2> T;
  w->send (s, close_box<T> (T (val1, val2)));
}

template<class T1, class T2, class T3> void
send (widget w, slot s, T1 val1, T2 val2, T3 val3) {
  typedef triple<T1,T2,T3> T;
  w->send (s, close_box<T> (T (val1, val2, val3)));
}

template<class T1, class T2, class T3, class T4> void
send (widget w, slot s, T1 val1, T2 val2, T3 val3, T4 val4) {
  typedef quartet<T1,T2,T3,T4> T;
  w->send (s, close_box<T> (T (val1, val2, val3, val4)));
}

template<class T1, class T2, class T3, class T4, class T5> void
send (widget w, slot s, T1 val1, T2 val2, T3 val3, T4 val4, T5 val5) {
  typedef quintuple<T1,T2,T3,T4,T5> T;
  w->send (s, close_box<T> (T (val1, val2, val3, val4, val5)));
}

template<class T1> inline T1
query (widget w, slot s) {
  return open_box<T1> (w->query (s, type_helper<T1>::id));
}

template<class T1, class T2> void
query (widget w, slot s, T1& val1, T2& val2) {
  typedef pair<T1,T2> T;
  T p= open_box<T> (w->query (s, type_helper<T>::id));
  val1= p.x1; val2= p.x2;
}

template<class T1, class T2, class T3> void
query (widget w, slot s, T1& val1, T2& val2, T3& val3) {
  typedef triple<T1,T2,T3> T;
  T t= open_box<T> (w->query (s, type_helper<T>::id));
  val1= t.x1; val2= t.x2; val3= t.x3;
}

template<class T1, class T2, class T3, class T4> void
query (widget w, slot s, T1& val1, T2& val2, T3& val3, T4& val4) {
  typedef quartet<T1,T2,T3,T4> T;
  T q= open_box<T> (w->query (s, type_helper<T>::id));
  val1= q.x1; val2= q.x2; val3= q.x3; val4= q.x4;
}

template<class T1, class T2, class T3, class T4, class T5> void
query (widget w, slot s, T1& val1, T2& val2, T3& val3, T4& val4, T5& val5) {
  typedef quintuple<T1,T2,T3,T4,T5> T;
  T q= open_box<T> (w->query (s, type_helper<T>::id));
  val1= q.x1; val2= q.x2; val3= q.x3; val4= q.x4; val5= q.x5;
}

inline void
notify (widget w, slot s) {
  w->notify (s, blackbox ());
}

template<class T1> inline void
notify (widget w, slot s, T1 val) {
  w->notify (s, close_box (val));
}

template<class T1, class T2> void
notify (widget w, slot s, T1 val1, T2 val2) {
  typedef pair<T1,T2> T;
  w->notify (s, close_box<T> (T (val1, val2)));
}

inline widget
read (widget w, slot s) {
  return w->read (s, blackbox ());
}

template<class T1> inline widget
read (widget w, slot s, T1 i1) {
  return w->read (s, close_box (i1));
}

inline void
write (widget w, slot s, widget val) {
  w->write (s, blackbox (), val);
}

template<class T1> inline void
write (widget w, slot s, T1 i1, widget val) {
  w->write (s, close_box (i1), val);
}

inline void
connect (widget w1, slot s1, widget w2, slot s2) {
  w1->connect (s1, w2, s2);
}

inline void
deconnect (widget w1, slot s1, widget w2, slot s2) {
  w1->deconnect (s1, w2, s2);
}

/******************************************************************************
* Standard messages
******************************************************************************/

inline int
get_identifier (widget w) {
  // get low-level handle for the widget's window, as used by the OS
  // widgets which have not been attached to a window have a zero identifier
  return query<int> (w, SLOT_IDENTIFIER);
}

inline bool
is_attached (widget w) {
  // is the widget attached to some window?
  return get_identifier (w) != 0;
}

inline void
set_identifier (widget w, int id) {
  // attach a widget w to the window given by its low-level identifier id
  // if id=0, then the widget is detached from its underlying window
  return send<int> (w, SLOT_IDENTIFIER, id);
}

inline widget
get_window (widget w) {
  // get the top-level window widget in which the widget is embedded
  return read (w, SLOT_WINDOW);
}

inline renderer
get_renderer (widget w) {
  // get renderer associated to widget (or NULL if the widget is not attached)
  return query<renderer> (w, SLOT_RENDERER);
}

inline void
set_visibility (widget w, bool flag) {
  // map or unmap a window widget
  send<bool> (w, SLOT_VISIBILITY, flag);
}

inline void
set_full_screen (widget w, bool flag) {
  // set or reset full screen mode for a window widget
  send<bool> (w, SLOT_FULL_SCREEN, flag);
}

inline void
set_name (widget w, string s) {
  // set the name of a widget (usually a window)
  send<string> (w, SLOT_NAME, s);
}

inline void
set_size (widget w, SI width, SI height) {
  // set the current size of the widget
  send<SI,SI> (w, SLOT_SIZE, width, height);
}

inline void
get_size (widget w, SI& width, SI& height) {
  // get the current size of the widget
  query<SI,SI> (w, SLOT_SIZE, width, height);
}

inline void
notify_size (widget w, SI new_width, SI new_height) {
  // notify a size change for the widget
  notify<SI,SI> (w, SLOT_SIZE, new_width, new_height);
}

inline void
set_position (widget w, SI x, SI y) {
  // set the position of the top left corner of the widget w
  // w.r.t. the top left of its parent widget
  send<SI,SI> (w, SLOT_POSITION, x, y);
}

inline void
get_position (widget w, SI& x, SI& y) {
  // get the position of the top left corner of the widget w
  // w.r.t. the top left of its parent widget
  query<SI,SI> (w, SLOT_POSITION, x, y);
}

inline void
notify_position (widget w, SI new_x, SI new_y) {
  // notify a change in the position of the widget
  notify<SI,SI> (w, SLOT_POSITION, new_x, new_y);
}

inline void
send_update (widget w) {
  // this message is issued if the contents of w or a subwidget of w
  // has changed in such a way that the geometries of w and its subwidgets
  // may need to be adjusted. Example: a change of the current output language
  send (w, SLOT_UPDATE);
}

inline void
send_keyboard (widget w, string key, time_t t= 0) {
  // send a key press event
  send<string,time_t> (w, SLOT_KEYBOARD, key, t);
}

inline void
send_keyboard_focus (widget w, bool get_focus= true) {
  // request the keyboard focus for a widget
  send<bool> (w, SLOT_KEYBOARD_FOCUS, get_focus);
}

inline void
notify_keyboard_focus (widget w, bool has_focus) {
  // notify that the widget got or lost keyboard focus
  notify<bool> (w, SLOT_KEYBOARD_FOCUS, has_focus);
}

inline bool
query_keyboard_focus (widget w) {
  // does this widget have the keyboard focus?
  return query<bool> (w, SLOT_KEYBOARD_FOCUS);
}

inline void
send_mouse (widget w, string kind, SI x, SI y, int mods, time_t t) {
  // send a mouse event of a given kind at position (x, y) and time t
  // mods corresponds to active buttons and keyboard modifiers at time t
  // the position is specified w.r.t. the top left corner of w
  send<string,SI,SI,int,time_t> (w, SLOT_MOUSE, kind, x, y, mods, t);
}

inline void
send_mouse_grab (widget w, bool get_grab) {
  // request a mouse grab for the widget
  send<bool> (w, SLOT_MOUSE_GRAB, get_grab);
}

inline void
notify_mouse_grab (widget w, bool has_grab) {
  // notify that the widget got or lost the mouse grab
  notify<bool> (w, SLOT_MOUSE_GRAB, has_grab);
}

inline bool
query_mouse_grab (widget w) {
  // does this widget have the mouse grab?
  return query<bool> (w, SLOT_MOUSE_GRAB);
}

inline void
send_mouse_pointer (widget w, string name, string mask_name= "") {
  // request a permanent change for the mouse pointer
  send<string,string> (w, SLOT_MOUSE_POINTER, name, mask_name);
}

inline void
send_invalidate_all (widget w) {
  // invalidate the widget so that it will be repaint at a next iteration
  send (w, SLOT_INVALIDATE_ALL);
}

inline void
send_invalidate (widget w, SI x1, SI y1, SI x2, SI y2) {
  // invalidate a region so that it will be repaint at a next iteration;
  // the region is specified w.r.t. the top left corner of w
  send<SI,SI,SI,SI> (w, SLOT_INVALIDATE, x1, y1, x2, y2);
}

inline void
send_repaint (widget w, SI x1, SI y1, SI x2, SI y2) {
  // request widget to repaint a region;
  // the region is specified w.r.t. the top left corner of w
  send<SI,SI,SI,SI> (w, SLOT_REPAINT, x1, y1, x2, y2);
}

inline void
send_delayed_message (widget w, string message, time_t delay) {
  // send a message to w which will only be received after a given delay
  send<string,time_t> (w, SLOT_DELAYED_MESSAGE, message, delay);
}

inline void
send_destroy (widget w) {
  // request a widget to be destroyed
  send (w, SLOT_DESTROY);
}

/******************************************************************************
* Canvas related messages
******************************************************************************/

inline void
set_shrinking_factor (widget w, int sf) {
  // set shrinking factor for canvas
  send<int> (w, SLOT_SHRINKING_FACTOR, sf);
}

inline void
set_extents (widget w, SI x1, SI y1, SI x2, SI y2) {
  // set extents of a canvas
  send<SI,SI,SI,SI> (w, SLOT_EXTENTS, x1, y1, x2, y2);
}

inline void
get_extents (widget w, SI& x1, SI& y1, SI& x2, SI& y2) {
  // get extents of a canvas
  query<SI,SI,SI,SI> (w, SLOT_EXTENTS, x1, y1, x2, y2);
}

inline void
get_visible_part (widget w, SI& x1, SI& y1, SI& x2, SI& y2) {
  // get visible part of a canvas
  query<SI,SI,SI,SI> (w, SLOT_VISIBLE_PART, x1, y1, x2, y2);
}

inline void
set_scrollbars_visibility (widget w, int sb) {
  // set visibility of scrollbars
  send<int> (w, SLOT_SCROLLBARS_VISIBILITY, sb);
}

inline void
set_scroll_position (widget w, SI x, SI y) {
  // set scroll position in a canvas
  send<SI,SI> (w, SLOT_SCROLL_POSITION, x, y);
}

inline void
get_scroll_position (widget w, SI& x, SI& y) {
  // get scroll position in a canvas
  query<SI,SI> (w, SLOT_SCROLL_POSITION, x, y);
}

inline void
set_canvas (widget w, widget cv) {
  // set the scrollable canvas itself
  write (w, SLOT_CANVAS, cv);
}

/******************************************************************************
* Top-level window related messages (also behaves as a canvas)
******************************************************************************/

inline void
set_header_visibility (widget w, bool visible) {
  // set visibility of header (menu and icon bars)
  send<bool> (w, SLOT_HEADER_VISIBILITY, visible);
}

inline bool
get_header_visibility (widget w) {
  // get visibility of header (menu and icon bars)
  return query<bool> (w, SLOT_HEADER_VISIBILITY);
}

inline void
set_main_menu (widget w, widget bar) {
  // set main menu bar
  write (w, SLOT_MAIN_MENU, bar);
}

inline void
set_main_icons_visibility (widget w, bool visible) {
  // set visibility of main icons bar
  send<bool> (w, SLOT_MAIN_ICONS_VISIBILITY, visible);
}

inline bool
get_main_icons_visibility (widget w) {
  // get visibility of main icons bar
  return query<bool> (w, SLOT_MAIN_ICONS_VISIBILITY);
}

inline void
set_main_icons (widget w, widget bar) {
  // set main icons bar
  write (w, SLOT_MAIN_ICONS, bar);
}

inline void
set_context_icons_visibility (widget w, bool visible) {
  // set visibility of context icons bar
  send<bool> (w, SLOT_CONTEXT_ICONS_VISIBILITY, visible);
}

inline bool
get_context_icons_visibility (widget w) {
  // get visibility of context icons bar
  return query<bool> (w, SLOT_CONTEXT_ICONS_VISIBILITY);
}

inline void
set_context_icons (widget w, widget bar) {
  // set context icons bar
  write (w, SLOT_CONTEXT_ICONS, bar);
}

inline void
set_user_icons_visibility (widget w, bool visible) {
  // set visibility of user icons bar
  send<bool> (w, SLOT_USER_ICONS_VISIBILITY, visible);
}

inline bool
get_user_icons_visibility (widget w) {
  // get visibility of user icons bar
  return query<bool> (w, SLOT_USER_ICONS_VISIBILITY);
}

inline void
set_user_icons (widget w, widget bar) {
  // set user icons bar
  write (w, SLOT_USER_ICONS, bar);
}

inline void
set_footer_visibility (widget w, bool visible) {
  // set visibility of footer
  send<bool> (w, SLOT_FOOTER_VISIBILITY, visible);
}

inline bool
get_footer_visibility (widget w) {
  // get visibility of footer
  return query<bool> (w, SLOT_FOOTER_VISIBILITY);
}

inline void
set_left_footer (widget w, string s) {
  // set left footer
  send<string> (w, SLOT_LEFT_FOOTER, s);
}

inline void
set_right_footer (widget w, string s) {
  // set right footer
  send<string> (w, SLOT_RIGHT_FOOTER, s);
}

inline void
set_interactive_mode (widget w, bool on) {
  // set interactive mode, allowing users to input text on footer
  send<bool> (w, SLOT_INTERACTIVE_MODE, on);
}

inline bool
get_interactive_mode (widget w) {
  // check whether footer is in interactive mode
  return query<bool> (w, SLOT_INTERACTIVE_MODE);
}

inline void
set_interactive_prompt (widget w, widget prompt) {
  // set prompt for interactive input
  write (w, SLOT_INTERACTIVE_PROMPT, prompt);
}

inline void
set_interactive_input (widget w, widget input) {
  // set interactive input widget
  write (w, SLOT_INTERACTIVE_INPUT, input);
}

inline string
get_interactive_input (widget w) {
  // set interactive input widget
  return query<string> (w, SLOT_INTERACTIVE_INPUT);
}

/******************************************************************************
* Dialogue windows
******************************************************************************/

inline widget
get_form_field (widget w, int i) {
  // get the i-th input widget from a dialogue widget
  return read<int> (w, SLOT_FORM_FIELD, i);
}

inline string
get_string_input (widget w) {
  // get input string from input widget
  return query<string> (w, SLOT_STRING_INPUT);
}

inline void
set_string_input (widget w, string s) {
  // set default input string of input widget
  send<string> (w, SLOT_STRING_INPUT, s);
}

inline void
set_input_type (widget w, string s) {
  // set the type of an input field
  send<string> (w, SLOT_INPUT_TYPE, s);
}

inline void
add_input_proposal (widget w, string s) {
  // add an extra proposal for input widget (e.g. from history)
  send<string> (w, SLOT_INPUT_PROPOSAL, s);
}

inline void
set_file (widget w, string s) {
  // set current file of file chooser widget
  send<string> (w, SLOT_FILE, s);
}

inline widget
get_file (widget w) {
  // get file input widget
  return read (w, SLOT_FILE);
}

inline void
set_directory (widget w, string s) {
  // set current directory of directory chooser widget
  send<string> (w, SLOT_DIRECTORY, s);
}

inline widget
get_directory (widget w) {
  // get directory input widget
  return read (w, SLOT_DIRECTORY);
}

#endif // defined MESSAGE_H
