
/******************************************************************************
* MODULE     : tm_window.hpp
* DESCRIPTION: TeXmacs main data structures (buffers, views and windows)
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TM_WINDOW_H
#define TM_WINDOW_H
#include "tm_buffer.hpp"

class tm_window_rep {
public:
  widget win;
  widget wid;
  int    id;

public:
  hashmap<tree,tree> props;
  int                serial;
  int                sfactor;       // the shrinking factor

protected:
  object*  texmacs_menu;       // accelerate menu rendering
  object*  texmacs_icon_menu;  // accelerate icon bar rendering
  string*  text_ptr;           // where the interactive string is returned
  command  call_back;          // called when typing finished

public:
  tm_window_rep (widget wid2, tree geom);
  ~tm_window_rep ();
  void set_window_name (string s);
  void map ();
  void unmap ();
  inline void set_property (scheme_tree what, scheme_tree val) {
    props (what)= val; }
  inline scheme_tree get_property (scheme_tree what) {
    return props [what]; }

  void menu_main (string menu);
  void menu_icons (int which, string menu);
  void set_header_flag (bool flag);
  void set_icon_bar_flag (int which, bool flag);
  bool get_header_flag ();
  bool get_icon_bar_flag (int which);

  int  get_shrinking_factor ();
  void set_shrinking_factor (int sf);
  void get_visible (SI& x1, SI& y1, SI& x2, SI& y2);
  void get_extents (SI& x1, SI& y1, SI& x2, SI& y2);
  void set_extents (SI x1, SI y1, SI x2, SI y2);
  void set_scrollbars (int i);
  void get_scroll_pos (SI& x, SI& y);
  void set_scroll_pos (SI x, SI y);

  bool get_footer_flag ();
  void set_footer_flag (bool on);
  void set_left_footer (string s);
  void set_right_footer (string s);
  bool get_interactive_mode ();
  void set_interactive_mode (bool on);
  void interactive (string name, string type, array<string> def,
		    string& s, command cmd);
  void interactive_return ();
};

class tm_view_rep {
public:
  tm_buffer buf;
  editor    ed;
  tm_window win;
  inline tm_view_rep (tm_buffer buf2, editor ed2):
    buf (buf2), ed (ed2), win (NULL) {}
};

typedef tm_buffer_rep* tm_buffer;
typedef tm_view_rep*   tm_view;
typedef tm_window_rep* tm_window;

#endif // defined TM_WINDOW_H
