
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
  bool undo_flag;                // when undoing some text
  bool redo_flag;                // when redoing some text

  array<path>      pps;          // all cursor positions
  array<int>       nr_to_code;   // code name for i-th cursor position
  hashmap<int,int> code_to_nr;   // inverse to nr_to_code

public:
  edit_modify_rep ();
  ~edit_modify_rep ();

  void assign           (path p, tree u);
  void insert           (path p, tree u);
  void remove           (path p, int nr);
  void split            (path p);
  void join             (path p);
  void ins_unary        (path p, tree_label op);
  void rem_unary        (path p);
  void finished         ();
  void notify_assign    (tree& t, path p, tree u);
  void notify_insert    (tree& t, path p, tree u);
  void notify_remove    (tree& t, path p, int nr);
  void notify_split     (tree& t, path p);
  void notify_join      (tree& t, path p);
  void notify_ins_unary (tree& t, path p, tree_label op);
  void notify_rem_unary (tree& t, path p);
  void post_notify      (tree& t);

  void notify_undo (string op, path p, tree t);
  void undo ();
  void redo ();
  void perform_undo_redo (tree x);

  int  position_new ();
  void position_delete (int i);
  void position_set (int i, path p);
  path position_get (int i);
};

#endif // defined EDIT_MODIFY_H
