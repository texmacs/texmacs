
/******************************************************************************
* MODULE     : simple_wk_widget.hpp
* DESCRIPTION: Simple wk_widgets for customization later on
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SIMPLE_WK_WIDGET_H
#define SIMPLE_WK_WIDGET_H
#include "Widkit/attribute_widget.hpp"

class attribute_widget_rep;
class simple_widget_rep: public attribute_widget_rep {
public:
  simple_widget_rep ();

  virtual bool is_editor_widget ();
  virtual bool is_embedded_widget ();
  virtual void handle_get_size_hint (SI& w, SI& h);
  virtual void handle_notify_resize (SI w, SI h);
  virtual void handle_keypress (string key, time_t t);
  virtual void handle_keyboard_focus (bool has_focus, time_t t);
  virtual void handle_mouse (string kind, SI x, SI y, int mods, time_t t,
                             array<double> data= array<double> ());
  virtual void handle_set_zoom_factor (double zoom);
  virtual void handle_clear (renderer ren, SI x1, SI y1, SI x2, SI y2);
  virtual void handle_repaint (renderer ren, SI x1, SI y1, SI x2, SI y2);

  void handle_get_size (get_size_event ev);
  void handle_attach_window (attach_window_event ev);
  void handle_resize (resize_event ev);
  void handle_keypress (keypress_event ev);
  void handle_keyboard_focus (keyboard_focus_event ev);
  void handle_mouse (mouse_event ev);
  void handle_set_integer (set_integer_event ev);
  void handle_set_double (set_double_event ev);
  void handle_clear (clear_event ev);
  void handle_repaint (repaint_event ev);
  void handle_set_coord2 (set_coord2_event ev);
  void handle_get_coord2 (get_coord2_event ev);
};

#endif // defined WK_WIDGET_H
