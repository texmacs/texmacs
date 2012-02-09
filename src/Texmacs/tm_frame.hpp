
/******************************************************************************
* MODULE     : tm_frame.hpp
* DESCRIPTION: Routines for main TeXmacs frames
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TM_FRAME_H
#define TM_FRAME_H
#include "server.hpp"

class tm_frame_rep: virtual public server_rep {
protected:
  bool full_screen;        // full screen mode
  bool full_screen_edit;   // full screen edit mode
  widget dialogue_win;     // dialogue window
  widget dialogue_wid;     // dialogue widget

public:
  tm_frame_rep ();
  ~tm_frame_rep ();

  /* properties */
  int get_window_id ();
  void set_window_property (scheme_tree what, scheme_tree val);
  void set_bool_window_property (string what, bool val);
  void set_int_window_property (string what, int val);
  void set_string_window_property (string what, string val);
  scheme_tree get_window_property (scheme_tree what);
  bool get_bool_window_property (string what);
  int get_int_window_property (string what);
  string get_string_window_property (string what);

  /* menus */
  void show_header (bool flag);
  void show_icon_bar (int which, bool flag);
  void show_side_tools (int which, bool flag);
  bool visible_header ();
  bool visible_icon_bar (int which);
  bool visible_side_tools (int which);
  void menu_widget (string menu, widget& w);
  void menu_main (string menu);
  void menu_icons (int which, string menu);
  void side_tools (int which, string menu);

  /* canvas */
  void set_shrinking_factor (int sf);
  int  get_shrinking_factor ();
  void set_scrollbars (int sb);
  void get_visible (SI& x1, SI& y1, SI& x2, SI& y2);
  void scroll_where (SI& x, SI& y);
  void scroll_to (SI x, SI y);
  void set_extents (SI x1, SI y1, SI x2, SI y2);
  void get_extents (SI& x1, SI& y1, SI& x2, SI& y2);
  void full_screen_mode (bool on, bool edit);
  bool in_full_screen_mode ();
  bool in_full_screen_edit_mode ();

  /* footer */
  void show_footer (bool flag);
  bool visible_footer ();
  void set_left_footer (string s);
  void set_right_footer (string s);
  void set_message (tree left, tree right, bool temp= false);
  void recall_message ();
  void dialogue_start (string name, widget wid);
  void dialogue_inquire (int i, string& arg);
  void dialogue_end ();
  void choose_file (object fun, string title, string type);
  void interactive (object fun, scheme_tree p);
};

widget box_widget (box b, bool trans);
widget box_widget (scheme_tree p, string s, color col,
		   bool trans= true, bool ink= false);

#endif // defined TM_FRAME_H
