
/******************************************************************************
* MODULE     : tm_widget.hpp
* DESCRIPTION: Main current graphical interface for user applications
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TM_WIDGET_H
#define TM_WIDGET_H
#include "server.hpp"
#include "window.hpp"
#include "scheme.hpp"
#include "Widkit/Event/attribute_event.hpp"

class tm_widget_rep;
class tm_widget;

class tm_widget_rep: public basic_widget_rep {
protected:
  bool     footer_flag;        // footer visible ?

protected:
  void set_left_footer (string s);
  void set_right_footer (string s);
  int  get_footer_mode ();
  void set_footer_mode (int which);
  bool get_footer_flag ();
  void set_footer_flag (bool on);

  void set_subwidget (wk_widget w, string which, wk_widget sw);
  bool get_subwidget_flag (wk_widget w);
  void set_subwidget_flag (wk_widget w, bool on);

public:
  tm_widget_rep (int mask);
  ~tm_widget_rep ();
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_set_widget (set_widget_event ev);
  void handle_get_widget (get_widget_event ev);
  void handle_set_string (set_string_event ev);
  void handle_get_string (get_string_event ev);
  void handle_set_coord2 (set_coord2_event ev);
  void handle_get_coord2 (get_coord2_event ev);
  void handle_set_coord4 (set_coord4_event ev);
  void handle_get_coord4 (get_coord4_event ev);
  void handle_keypress (keypress_event ev);
  void handle_mouse (mouse_event ev);
  void handle_keyboard_focus (keyboard_focus_event ev);
  void handle_resize (resize_event ev);
  void handle_destroy (destroy_event ev);

  bool handle (event ev);

  friend class tm_editor_rep;
  friend class tm_widget;
};

class tm_widget {
EXTEND_NULL(wk_widget,tm_widget);
};
EXTEND_NULL_CODE(wk_widget,tm_widget);

#endif // defined TM_WIDGET_H
