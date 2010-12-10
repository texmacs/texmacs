
/******************************************************************************
* MODULE     : edit_modify.hpp
* DESCRIPTION: Main routines for the modification of the edit tree
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef EDIT_MODIFY_H
#define EDIT_MODIFY_H
#include "editor.hpp"
#include "archiver.hpp"

path inner_paragraph (tree t, path p);

class edit_modify_rep: virtual public editor_rep {
protected:
  observer cur_pos;  // tree_position corresponding to tp
  double   author;   // the author identifier associated to this view
  archiver arch;     // archiver attached to the editor

public:
  edit_modify_rep ();
  ~edit_modify_rep ();
  double this_author ();

  void notify_assign      (path p, tree u);
  void notify_insert      (path p, tree u);
  void notify_remove      (path p, int nr);
  void notify_split       (path p);
  void notify_join        (path p);
  void notify_assign_node (path p, tree_label op);
  void notify_insert_node (path p, tree t);
  void notify_remove_node (path p);
  void notify_set_cursor  (path p, tree data);
  void post_notify        (path p);

  void clear_undo_history ();
  void start_editing ();
  void end_editing ();
  void start_slave (double a);
  void mark_start (double a);
  bool mark_cancel (double a);
  void mark_end (double a);
  void add_undo_mark ();
  void remove_undo_mark ();
  int  undo_possibilities ();
  void undo (bool redoable);
  void unredoable_undo ();
  void undo (int i);
  int  redo_possibilities ();
  void redo (int i);
  void require_save ();
  void notify_save (bool real_save= true);
  bool need_save (bool real_save= true);
  void show_history ();

  observer position_new (path p);
  void position_delete (observer o);
  void position_set (observer o, path p);
  path position_get (observer o);
};

#endif // defined EDIT_MODIFY_H
