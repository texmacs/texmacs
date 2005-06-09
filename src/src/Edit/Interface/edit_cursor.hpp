
/******************************************************************************
* MODULE     : cursor.hpp
* DESCRIPTION: for cursor handling in TeXmacs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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

  path tree_path (SI x, SI y, SI delta);
  bool cursor_move_sub (SI& x0, SI& y0, SI& delta, SI dx, SI dy);
  void cursor_move (SI dx, SI dy);
  void adjust_ghost_cursor (int status);
  void notify_cursor_moved (int status);

public:
  edit_cursor_rep ();
  ~edit_cursor_rep ();

  /* visual cursor movement */
  void go_to (SI x, SI y);
  void go_left ();
  void go_right ();
  void go_up ();
  void go_down ();
  void go_page_up ();
  void go_page_down ();

  /* logical cursor movement */
  void go_to (path p);
  void go_to_correct (path p);
  void go_to_start (path p);
  void go_to_end (path p);
  void go_to_border (path p, bool at_start);
  void go_to_here ();
  void go_start ();
  void go_end ();
  void go_start_line ();
  void go_end_line ();
  void go_start_of (tree_label what);
  void go_end_of (tree_label what);
  void go_start_with (string var, string val);
  void go_end_with (string var, string val);

  /* other cursor routines */
  void go_to_label (string s);
  tree get_labels ();
};

#endif // defined EDIT_CURSOR_H
