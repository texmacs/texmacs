
/******************************************************************************
* MODULE     : edit_modify.hpp
* DESCRIPTION: Main routines for the modification of the edit tree
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef EDIT_MODIFY_H
#define EDIT_MODIFY_H
#include "editor.hpp"

path inner_paragraph (tree t, path p);

class edit_modify_rep: virtual public editor_rep {
protected:
  bool     undo_flag;                // when undoing some text
  bool     redo_flag;                // when redoing some text
  observer cur_pos;                  // tree_position corresponding to tp

public:
  edit_modify_rep ();
  ~edit_modify_rep ();

  void assign             (path p, tree u);
  void insert             (path p, tree u);
  void remove             (path p, int nr);
  void split              (path p);
  void join               (path p);
  void insert_node        (path p, tree t);
  void remove_node        (path p);
  void assign_node        (path p, tree_label op);
  void finished           (path p);
  void notify_assign      (path p, tree u);
  void notify_insert      (path p, tree u);
  void notify_remove      (path p, int nr);
  void notify_split       (path p);
  void notify_join        (path p);
  void notify_insert_node (path p, tree t);
  void notify_remove_node (path p);
  void notify_assign_node (path p, tree_label op);
  void post_notify        (path p);

  void notify_undo (string op, path p, tree t); 
  void mark_undo_blocks ();
  void remove_undo_mark ();
  void add_undo_mark ();
  void undo (bool redoable);
  void unredoable_undo ();
  void forget_undo ();
  void undo ();
  void redo ();
  void perform_undo_redo (tree x);

  observer position_new (path p);
  void position_delete (observer o);
  void position_set (observer o, path p);
  path position_get (observer o);
};

#endif // defined EDIT_MODIFY_H
