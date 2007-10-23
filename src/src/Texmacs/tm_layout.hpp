
/******************************************************************************
* MODULE     : tm_layout.hpp
* DESCRIPTION: Layout of TeXmacs windows, menus, icons, etc.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TM_LAYOUT_H
#define TM_LAYOUT_H
#include "server.hpp"

class tm_layout_rep: virtual public server_rep {
public:
  tm_layout_rep ();
  ~tm_layout_rep ();

  /* menus */
  void menu_widget (string menu, wk_widget& w);
  void menu_main (string menu);
  void menu_icons (int which, string menu);

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

  /* visibility properties */
  void show_header (bool flag);
  void show_icon_bar (int which, bool flag);
  void show_footer   (bool flag);
  bool visible_header ();
  bool visible_icon_bar (int which);
  bool visible_footer ();
  void set_shrinking_factor (int sf);
  int  get_shrinking_factor ();
};

wk_widget box_wk_widget (box b, bool trans);
wk_widget box_wk_widget (scheme_tree p, string s, color col,
			 bool trans= true, bool ink= false);
widget box_widget (box b, bool trans);
widget box_widget (scheme_tree p, string s, color col,
		   bool trans= true, bool ink= false);

#endif // defined TM_LAYOUT_H
