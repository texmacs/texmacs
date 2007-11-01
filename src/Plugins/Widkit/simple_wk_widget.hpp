
/******************************************************************************
* MODULE     : simple_wk_widget.hpp
* DESCRIPTION: Simple wk_widgets for customization later on
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef SIMPLE_WK_WIDGET_H
#define SIMPLE_WK_WIDGET_H
#include "Widkit/attribute_widget.hpp"

class attribute_widget_rep;
class simple_widget_rep: public attribute_widget_rep {
public:
  simple_widget_rep ();

  virtual void handle_get_size_hint (SI& w, SI& h);
  virtual void handle_notify_resize (SI w, SI h);
  virtual void handle_keypress (string key, time_t t);
  virtual void handle_keyboard_focus (bool has_focus, time_t t);
  virtual void handle_mouse (string kind, SI x, SI y, int mods, time_t t);
  virtual void handle_set_shrinking_factor (int sf);
  virtual void handle_clear (SI x1, SI y1, SI x2, SI y2);
  virtual void handle_repaint (SI x1, SI y1, SI x2, SI y2);

  void handle_get_size (get_size_event ev);
  void handle_attach_window (attach_window_event ev);
  void handle_resize (resize_event ev);
  void handle_keypress (keypress_event ev);
  void handle_keyboard_focus (keyboard_focus_event ev);
  void handle_mouse (mouse_event ev);
  void handle_set_integer (set_integer_event ev);
  void handle_clear (clear_event ev);
  void handle_repaint (repaint_event ev);
};

#endif // defined WK_WIDGET_H
