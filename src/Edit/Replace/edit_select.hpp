
/******************************************************************************
* MODULE     : select.hpp
* DESCRIPTION: for selection handling in TeXmacs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef EDIT_SELECT_H
#define EDIT_SELECT_H
#include "editor.hpp"

#define DIRECT 0
#define CENTER 1
#define HORIZONTAL 2
#define VERTICAL 3

class edit_select_rep: virtual public editor_rep {
protected:
  path   start_p, end_p;
  bool   selecting, shift_selecting;
  path   mid_p;
  string selection_import;
  string selection_export;

protected:
  void get_selection (path& start, path& end);
  void set_selection (path start, path end);

public:
  edit_select_rep ();
  ~edit_select_rep ();

  void select (path p);
  void select (path start, path end);
  void select_all ();
  void select_line ();
  void select_from_cursor ();
  void select_from_cursor_if_active ();
  void select_from_keyboard (bool flag);
  void select_from_shift_keyboard ();
  void select_enlarge ();
  void select_enlarge_environmental ();

  bool selection_active_any ();
  bool selection_active_normal ();
  bool selection_active_table ();
  bool selection_active_small ();
  bool selection_active_enlarging ();

  path selection_get_subtable (int& row1, int& col1, int& row2, int& col2);
  void selection_get (selection& sel);
  void selection_get (path& start, path& end);
  void selection_set (string key, tree t, bool persistant= false);
  void selection_set (tree t);
  void selection_set_start ();
  void selection_set_end ();
  void selection_copy (string key= "primary");
  void selection_paste (string key= "primary");
  void selection_clear (string key= "primary");
  void selection_cancel ();
  void selection_set_import (string fm);
  void selection_set_export (string fm);
  string selection_get_import ();
  string selection_get_export ();

  tree selection_get ();
  void selection_cut (string key= "primary");
  tree selection_get_cut ();
  void selection_move ();
  void cut (path p);
  void cut (path start, path end);
};

#endif // defined EDIT_SELECT_H
