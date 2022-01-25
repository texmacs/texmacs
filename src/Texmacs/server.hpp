
/******************************************************************************
* MODULE     : server.hpp
* DESCRIPTION: Main current graphical interface for user applications
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SERVER_H
#define SERVER_H
#include "editor.hpp"
#include "url.hpp"
#include "scheme.hpp"
#include "new_data.hpp"
#include "Data/new_buffer.hpp"
#include "Data/new_view.hpp"
#include "Data/new_window.hpp"
#include "Data/new_project.hpp"

class editor;

class server_rep: public abstract_struct {
public:
  server_rep ();
  virtual ~server_rep ();
  virtual server_rep* get_server () = 0;

  inline virtual void* derived_this () { return (server_rep*)this; }

  /* Control global server parameters */
  virtual void   set_font_rules (scheme_tree rules) = 0;
  virtual bool   kbd_get_command (string s, string& help, command& cmd) = 0;
  virtual void   insert_kbd_wildcard (string key, string im,
				      bool post, bool l, bool r) = 0;
  virtual string kbd_pre_rewrite (string l) = 0;
  virtual string kbd_post_rewrite (string l, bool var_flag= true) = 0;
  virtual tree   kbd_system_rewrite (string l) = 0;
  virtual void   set_variant_keys (string var, string unvar) = 0;
  virtual void   get_keycomb (string& s, int& status,
			      command& cmd, string& sh, string& help) = 0;

  /* TeXmacs frames */
  virtual int  get_window_serial () = 0;
  virtual void set_window_property (scheme_tree what, scheme_tree val) = 0;
  virtual void set_bool_window_property (string what, bool val) = 0;
  virtual void set_int_window_property (string what, int val) = 0;
  virtual void set_string_window_property (string what, string val) = 0;
  virtual scheme_tree get_window_property (scheme_tree what) = 0;
  virtual bool get_bool_window_property (string what) = 0;
  virtual int get_int_window_property (string what) = 0;
  virtual string get_string_window_property (string what) = 0;

  virtual void show_header (bool flag) = 0;
  virtual void show_icon_bar (int which, bool flag) = 0;
  virtual void show_side_tools (int which, bool flag) = 0;
  virtual void show_bottom_tools (int which, bool flag) = 0;
  virtual bool visible_header () = 0;
  virtual bool visible_icon_bar (int which) = 0;
  virtual bool visible_side_tools (int which) = 0;
  virtual bool visible_bottom_tools (int which) = 0;
  virtual void menu_widget (string menu, widget& w) = 0;
  virtual void menu_main (string menu) = 0;
  virtual void menu_icons (int which, string menu) = 0;
  virtual void side_tools (int which, string menu) = 0;
  virtual void bottom_tools (int which, string menu) = 0;

  virtual void set_window_zoom_factor (double zoom) = 0;
  virtual double get_window_zoom_factor () = 0;
  virtual void set_scrollbars (int sb) = 0;
  virtual void get_visible (SI& x1, SI& y1, SI& x2, SI& y2) = 0;
  virtual void scroll_where (SI& x, SI& y) = 0;
  virtual void scroll_to (SI x, SI y) = 0;
  virtual void set_extents (SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void get_extents (SI& x1, SI& y1, SI& x2, SI& y2) = 0;
  virtual void full_screen_mode (bool on, bool edit) = 0;
  virtual bool in_full_screen_mode () = 0;
  virtual bool in_full_screen_edit_mode () = 0;

  virtual void show_footer   (bool flag) = 0;
  virtual bool visible_footer () = 0;
  virtual void set_left_footer (string s) = 0;
  virtual void set_right_footer (string s) = 0;
  virtual void set_message (tree left, tree right, bool temp= false) = 0;
  virtual void recall_message () = 0;
  virtual void dialogue_start (string name, widget wid) = 0;
  virtual void dialogue_inquire (int i, string& arg) = 0;
  virtual void dialogue_end () = 0;
  virtual void choose_file (object fun, string title, string type,
			    string prompt, url name) = 0;
  virtual void interactive (object fun, scheme_tree p) = 0;

  /* Miscellaneous routines */
  virtual void   style_clear_cache () = 0;
  virtual void   refresh () = 0;
  virtual void   interpose_handler () = 0;
  virtual void   wait_handler (string message, string arg) = 0;
  virtual void   set_script_status (int i) = 0;
  virtual void   set_printing_command (string s) = 0;
  virtual void   set_printer_page_type (string s) = 0;
  virtual string get_printer_page_type () = 0;
  virtual void   set_printer_dpi (string dpi) = 0;
  virtual void   set_default_zoom_factor (double zoom) = 0;
  virtual double get_default_zoom_factor () = 0;
  virtual void   inclusions_gc (string which= "*") = 0;
  virtual void   typeset_update (path p) = 0;
  virtual void   typeset_update_all () = 0;
  virtual bool   is_yes (string s) = 0;
  virtual void   quit () = 0;
  virtual void   shell (string s) = 0;
};

template<> void tm_delete<server_rep> (server_rep* ptr);

class server {
  ABSTRACT(server);
  server ();
};
ABSTRACT_CODE(server);

extern bool rescue_mode;
scheme_tree menu_merge (scheme_tree m1, scheme_tree m2);
server get_server ();
void gui_set_output_language (string lan);
inline bool in_rescue_mode () { return rescue_mode; }

/* low level */
void create_buffer (url name, tree doc);
void new_buffer_in_this_window (url name, tree t);
#endif // defined SERVER_H
