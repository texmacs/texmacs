
/******************************************************************************
* MODULE     : cursor.hpp
* DESCRIPTION: for cursor handling in TeXmacs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef EDIT_CURSOR_H
#define EDIT_CURSOR_H
#include "editor.hpp"

#define DIRECT 0
#define CENTER 1
#define HORIZONTAL 2
#define VERTICAL 3

class edit_cursor_rep: virtual public editor_rep {
protected:
  cursor cu;         // the cursor
  cursor mv;         // "ghost cursor" position when moving cursor
  int    mv_status;  // cursor status during movements

protected:
  cursor& the_cursor ();
  cursor& the_ghost_cursor ();

  bool cursor_is_accessible ();
  path make_cursor_accessible (path p, bool forwards);
  path tree_path (path sp, SI x, SI y, SI delta);
  bool cursor_move_sub (SI& x0, SI& y0, SI& delta, SI dx, SI dy);
  void cursor_move (SI dx, SI dy);
  void adjust_ghost_cursor (int status);
  void notify_cursor_moved (int status);
  void show_cursor_if_hidden ();

public:
  edit_cursor_rep ();
  ~edit_cursor_rep ();

  /* visual cursor movement */
  void go_to (SI x, SI y, bool absolute= true);
  void go_left_physical ();
  void go_right_physical ();
  void go_left ();
  void go_right ();
  void go_up ();
  void go_down ();
  void go_start_line ();
  void go_end_line ();
  void go_page_up ();
  void go_page_down ();

  /* logical cursor movement */
  void adjust_cursor ();
  void go_to_here ();
  void go_to (path p);
  void go_to_correct (path p);
  void go_to_start (path p);
  void go_to_end (path p);
  void go_to_border (path p, bool at_start);
  void go_start ();
  void go_end ();
  void go_start_paragraph ();
  void go_end_paragraph ();
  void go_start_of (tree_label what);
  void go_end_of (tree_label what);
  void go_start_with (string var, string val);
  void go_end_with (string var, string val);

  /* other cursor routines */
  path search_label (string s, bool local);
  void go_to_label (string s);
  tree get_labels ();
};

#endif // defined EDIT_CURSOR_H
