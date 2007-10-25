
/******************************************************************************
* MODULE     : message.hpp
* DESCRIPTION: Messages to and between widgets
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef MESSAGE_H
#define MESSAGE_H
#include "widget.hpp"
#include "ntuple.hpp"

/******************************************************************************
* Helper templates for sending messages
******************************************************************************/

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
  typedef quadruple<T1,T2,T3,T4> T;
  w->send (s, close_box<T> (T (val1, val2, val3, val4)));
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
  typedef quadruple<T1,T2,T3,T4> T;
  T q= open_box<T> (w->query (s, type_helper<T>::id));
  val1= q.x1; val2= q.x2; val3= q.x3; val4= q.x4;
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
get_window_id (widget w) {
  // get window ID of a widget or 0 if the widget is not attached
  return query<int> (w, SLOT_WINDOW_ID);
}

inline void
set_name (widget w, string s) {
  send<string> (w, SLOT_NAME, s);
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

/******************************************************************************
* Top-level window related messages
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
  send<widget> (w, SLOT_MAIN_MENU, bar);
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
  send<widget> (w, SLOT_MAIN_ICONS, bar);
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
  send<widget> (w, SLOT_CONTEXT_ICONS, bar);
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
  send<widget> (w, SLOT_USER_ICONS, bar);
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
  send<widget> (w, SLOT_INTERACTIVE_PROMPT, prompt);
}

inline void
set_interactive_input (widget w, widget input) {
  // set interactive input widget
  send<widget> (w, SLOT_INTERACTIVE_INPUT, input);
}

inline string
get_interactive_input (widget w) {
  // set interactive input widget
  return query<string> (w, SLOT_INTERACTIVE_INPUT);
}

#endif // defined MESSAGE_H
