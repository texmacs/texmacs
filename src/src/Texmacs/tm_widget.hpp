
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
#include "Event/attribute_event.hpp"
#include "scheme.hpp"

class tm_widget_rep;
class tm_widget;
class server_rep;

class tm_widget_rep: public basic_widget_rep {
public:
  server_rep* sv;
  hashmap<tree,tree> props;
  bool serial;

protected:
  bool     footer_flag;        // footer visible ?
  string*  text_ptr;           // where the interactive string is returned
  command  call_back;          // called when typing finished
  int      sfactor;            // the shrinking factor
  object*  texmacs_menu;       // accelerate menu rendering
  object*  texmacs_icon_menu;  // accelerate icon bar rendering

public:
  tm_widget_rep (server_rep* sv, display dis);
  ~tm_widget_rep ();
  widget_rep* get_this ();

  operator tree ();
  void set_window_name (string s);
  void set_popup_menu (widget w, SI x, SI y);
  void set_subwidget (widget w, string which, widget sw);
  bool get_subwidget_flag (widget w);
  void set_subwidget_flag (widget w, bool on);

  void interactive (string name, string type, array<string> def,
		    string& s, command cmd);
  void interactive_return ();
  void set_left_footer (string s);
  void set_right_footer (string s);
  int  get_footer_mode ();
  void set_footer_mode (int which);
  bool get_footer_flag ();
  void set_footer_flag (bool on);
  void set_shrinking_factor (int sf);
  int  get_shrinking_factor ();

  void menu_widget (string menu, widget& w);
  void menu_main (string menu);
  void menu_icons (int which, string menu);

  void handle_get_size (get_size_event ev);
  void handle_get_widget (get_widget_event ev);
  void handle_set_widget (set_widget_event ev);
  void handle_set_string (set_string_event ev);
  void handle_keypress (keypress_event ev);
  void handle_mouse (mouse_event ev);
  void handle_keyboard_focus (keyboard_focus_event ev);
  void handle_resize (resize_event ev);
  void handle_destroy (destroy_event ev);

  bool handle (event ev);

  friend class server_rep;
  friend class tm_editor_rep;
  friend class tm_widget;
};

class tm_widget {
  EXTEND_NULL(widget,tm_widget);
};
EXTEND_NULL_CODE(widget,tm_widget);

#endif // defined TM_WIDGET_H
