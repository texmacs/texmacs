
/******************************************************************************
* MODULE     : select.hpp
* DESCRIPTION: for selection handling in TeXmacs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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
  path   focus_p;
  bool   focus_hold;

protected:
  void get_selection (path& start, path& end);
  void set_selection (path start, path end);
  void raw_cut (path start, path end);

public:
  edit_select_rep ();
  ~edit_select_rep ();

  path semantic_root (path p);
  bool semantic_active (path p);
  bool semantic_select (path p, path& q1, path& q2, int mode);

  void select (path p);
  void select (path start, path end);
  void select_all ();
  void select_line ();
  void select_from_cursor ();
  void select_from_cursor_if_active ();
  void select_from_keyboard (bool flag);
  void select_from_shift_keyboard ();
  void select_enlarge_text ();
  void select_enlarge ();
  void select_enlarge_environmental ();

  bool selection_active_any ();
  bool selection_active_normal ();
  bool selection_active_table ();
  bool selection_active_small ();
  bool selection_active_enlarging ();

  void selection_raw_set (string key, tree t);
  tree selection_raw_get (string key);
  path selection_get_subtable (int& row1, int& col1, int& row2, int& col2);
  void selection_get (selection& sel);
  void selection_get (path& start, path& end);
  path selection_get_start ();
  path selection_get_end ();
  path selection_get_path ();
  void selection_set (string key, tree t, bool persistant= false);
  void selection_set (tree t);
  void selection_set_start (path p= path());
  void selection_set_end (path p= path());
  void selection_set_paths (path start, path end);
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
  path manual_focus_get ();
  void manual_focus_set (path p, bool force);
  void manual_focus_release ();
  path focus_search (path p, bool skip_flag, bool up_flag);
  path focus_get (bool skip_flag);
};

#endif // defined EDIT_SELECT_H
