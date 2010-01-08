
/******************************************************************************
* MODULE     : basic_event.hpp
* DESCRIPTION: The most common events
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef BASIC_EVENT_H
#define BASIC_EVENT_H
#include "Widkit/event.hpp"
#include "Widkit/wk_widget.hpp"
#include "Widkit/Event/event_codes.hpp"

/******************************************************************************
* Attribute events
******************************************************************************/

struct get_size_event_rep: public event_rep {
  SI& w; SI& h; int mode;
  get_size_event_rep (SI& w, SI& h, int mode);
  operator tree ();
};
EVENT(get_size_event);

struct get_widget_event_rep: public event_rep {
  string which; wk_widget& w;
  get_widget_event_rep (string which, wk_widget& w);
  operator tree ();
};
EVENT(get_widget_event);

struct set_widget_event_rep: public event_rep {
  string which; wk_widget w;
  set_widget_event_rep (string which, wk_widget w);
  operator tree ();
};
EVENT(set_widget_event);

/******************************************************************************
* Structure events
******************************************************************************/

struct attach_window_event_rep: public event_rep {
  window win;
  attach_window_event_rep (window win);
  operator tree ();
};
EVENT(attach_window_event);

struct position_event_rep: public event_rep {
  bool flag; SI ox, oy, w, h; gravity grav;
  position_event_rep ();
  position_event_rep (SI ox, SI oy, SI w, SI h, gravity grav);
  operator tree ();
};
tree as_tree (gravity grav);
tm_ostream& operator << (tm_ostream& out, gravity grav);
EVENT(position_event);

struct move_event_rep: public event_rep {
  move_event_rep ();
  operator tree ();
};
EVENT(move_event);

struct resize_event_rep: public event_rep {
  resize_event_rep ();
  operator tree ();
};
EVENT(resize_event);

struct destroy_event_rep: public event_rep {
  destroy_event_rep ();
  operator tree ();
};
EVENT(destroy_event);

/******************************************************************************
* Input events
******************************************************************************/

struct keypress_event_rep: public event_rep {
  string key; time_t t;
  keypress_event_rep (string key, time_t t);
  operator tree ();
};
EVENT(keypress_event);

struct keyboard_focus_event_rep: public event_rep {
  bool flag; time_t t;
  keyboard_focus_event_rep (bool in_out_flag, time_t t);
  operator tree ();
};
EVENT(keyboard_focus_event);

struct mouse_event_rep: public event_rep {
  string type; SI x, y; int mods; time_t t;
  mouse_event_rep (string type, SI x, SI y, int mods, time_t t);
  bool pressed (string which);
  operator tree ();
};
EVENT(mouse_event);

struct alarm_event_rep: public event_rep {
  string message; time_t t;
  alarm_event_rep (string message, time_t t);
  operator tree ();
};
EVENT(alarm_event);

/******************************************************************************
* Output events
******************************************************************************/

struct clear_event_rep: public event_rep {
  SI x1, y1, x2, y2;
  clear_event_rep (SI x1, SI y1, SI x2, SI y2);
  operator tree ();
};
EVENT(clear_event);

struct repaint_event_rep: public event_rep {
  SI x1, y1, x2, y2; bool& stop;
  repaint_event_rep (SI x1, SI y1, SI x2, SI y2, bool& stop);
  operator tree ();
};
EVENT(repaint_event);

/******************************************************************************
* Request some action
******************************************************************************/

struct update_event_rep: public event_rep {
  update_event_rep ();
  operator tree ();
};
EVENT(update_event);

struct invalidate_event_rep: public event_rep {
  bool all_flag;
  SI x1, y1, x2, y2;
  invalidate_event_rep ();
  invalidate_event_rep (SI x1, SI y1, SI x2, SI y2);
  operator tree ();
};
EVENT(invalidate_event);

struct keyboard_grab_event_rep: public event_rep {
  bool flag; time_t t;
  keyboard_grab_event_rep (bool in_out_flag, time_t t);
  operator tree ();
};
EVENT(keyboard_grab_event);

struct mouse_grab_event_rep: public event_rep {
  bool flag; time_t t;
  mouse_grab_event_rep (bool in_out_flag, time_t t);
  operator tree ();
};
EVENT(mouse_grab_event);

struct request_alarm_event_rep: public event_rep {
  event ev; time_t delay;
  request_alarm_event_rep (event ev, time_t delay);
  operator tree ();
};
EVENT(request_alarm_event);

/******************************************************************************
* Miscellaneous events
******************************************************************************/

struct find_child_event_rep: public event_rep {
  SI x, y; int& which;
  find_child_event_rep (SI x, SI y, int& which);
  operator tree ();
};
EVENT(find_child_event);

/******************************************************************************
* Modification of events
******************************************************************************/

event emit_keypress (keypress_event ev, string key);
event emit_mouse (mouse_event ev, string type, SI x, SI y);

#endif // defined BASIC_EVENT_H
