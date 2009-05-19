
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

path inner_paragraph (tree t, path p);

class edit_modify_rep: virtual public editor_rep {
protected:
  observer cur_pos;  // tree_position corresponding to tp

public:
  edit_modify_rep ();
  ~edit_modify_rep ();

  void notify_assign      (path p, tree u);
  void notify_insert      (path p, tree u);
  void notify_remove      (path p, int nr);
  void notify_split       (path p);
  void notify_join        (path p);
  void notify_assign_node (path p, tree_label op);
  void notify_insert_node (path p, tree t);
  void notify_remove_node (path p);
  void post_notify        (path p);

  void mark_undo_blocks ();
  void remove_undo_mark ();
  void add_undo_mark ();
  void undo (bool redoable);
  void unredoable_undo ();
  void undo ();
  void redo ();
  void perform_undo_redo (tree x);

  observer position_new (path p);
  void position_delete (observer o);
  void position_set (observer o, path p);
  path position_get (observer o);
};

#endif // defined EDIT_MODIFY_H
