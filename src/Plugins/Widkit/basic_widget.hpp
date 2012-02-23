
/******************************************************************************
* MODULE     : basic_widget.hpp
* DESCRIPTION: Basic widgets can handle the most common events
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef BASIC_WIDGET_H
#define BASIC_WIDGET_H
#include "Widkit/wk_widget.hpp"
#include "Widkit/Event/basic_event.hpp"

class basic_widget_rep: public wk_widget_rep {
protected:
  int ptr_focus; // subwidget where the pointer is (-1 if none)

public:
  basic_widget_rep (gravity grav= north_west);
  basic_widget_rep (array<wk_widget> a, gravity grav= north_west);
  basic_widget_rep (array<wk_widget> a, array<string> name,
		    gravity grav= north_west);

  event emit_position   (SI ox, SI oy, SI w, SI h, gravity grav= north_west);
  event emit_invalidate (SI x1, SI y1, SI x2, SI y2);
  event emit_mouse      (mouse_event ev);
  event emit_mouse      (mouse_event ev, string type);
  event emit_mouse      (mouse_event ev, string type, SI x, SI y);
  event emit_clear      (SI x1, SI y1, SI x2, SI y2);
  event emit_repaint    (SI x1, SI y1, SI x2, SI y2, bool& stop);
  event emit_find_child (SI x, SI y, int& which);

  virtual void handle_get_size       (get_size_event ev);
  virtual void handle_get_widget     (get_widget_event ev);
  virtual void handle_set_widget     (set_widget_event ev);
  virtual void handle_attach_window  (attach_window_event ev);
  virtual void handle_position       (position_event ev);
  virtual void handle_move           (move_event ev);
  virtual void handle_resize         (resize_event ev);
  virtual void handle_destroy        (destroy_event ev);
  virtual void handle_keypress       (keypress_event ev);
  virtual void handle_keyboard_focus (keyboard_focus_event ev);
  virtual void handle_mouse          (mouse_event ev);
  virtual void handle_alarm          (alarm_event ev);
  virtual void handle_clear          (clear_event ev);
  virtual void handle_repaint        (repaint_event ev);
  virtual void handle_update         (update_event ev);
  virtual void handle_refresh        (refresh_event ev);
  virtual void handle_invalidate     (invalidate_event ev);
  virtual void handle_keyboard_grab  (keyboard_grab_event ev);
  virtual void handle_mouse_grab     (mouse_grab_event ev);
  virtual void handle_request_alarm  (request_alarm_event ev);
  virtual void handle_find_child     (find_child_event ev);
  virtual bool handle                (event ev);
};

#endif // defined BASIC_WIDGET_H
