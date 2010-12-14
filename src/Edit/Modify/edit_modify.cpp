
/******************************************************************************
* MODULE     : edit_modify.cpp
* DESCRIPTION: base routines for modifying the edit tree + notification
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "modification.hpp"
#include "edit_modify.hpp"
#include "tm_window.hpp"
#ifdef EXPERIMENTAL
#include "../../Style/Memorizer/clean_copy.hpp"
#endif

/******************************************************************************
* Constructors and destructors
******************************************************************************/

edit_modify_rep::edit_modify_rep ():
  author (new_author ()),
  arch (author, rp) {}
edit_modify_rep::~edit_modify_rep () {}

/******************************************************************************
* Notification of changes in document
******************************************************************************/

void
edit_modify_rep::notify_assign (path p, tree u) {
  (void) u;
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_assign (get_typesetter (), p / rp, u);
}

void
edit_modify_rep::notify_insert (path p, tree u) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_insert (get_typesetter (), p / rp, u);
}

void
edit_modify_rep::notify_remove (path p, int nr) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_remove (get_typesetter (), p / rp, nr);
}

void
edit_modify_rep::notify_split (path p) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_split (get_typesetter (), p / rp);
}

void
edit_modify_rep::notify_join (path p) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_join (get_typesetter (), p / rp);
}

void
edit_modify_rep::notify_assign_node (path p, tree_label op) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_assign_node (get_typesetter (), p / rp, op);
}

void
edit_modify_rep::notify_insert_node (path p, tree t) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_insert_node (get_typesetter (), p / rp, t);
}

void
edit_modify_rep::notify_remove_node (path p) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_remove_node (get_typesetter (), p / rp);
}

void
edit_modify_rep::notify_set_cursor (path p, tree data) {
  if (!(rp <= p)) return;
  if (data[0] == as_string (author)) {
    if (is_compound (data, "cursor", 1) ||
        is_compound (data, "cursor-clear", 1)) {
      if (tp != p) {
        tp= p;
        go_to_correct (tp);
      }
      if (is_compound (data, "cursor-clear", 1)) {
        //cout << "Clear selection\n";
        select (tp, tp);
      }
    }
    else if (is_compound (data, "start", 1)) {
      if (selection_get_start () != p) {
        //cout << "Set start selection: " << p << "\n";
        select (p, p);
      }
    }
    else if (is_compound (data, "end", 1)) {
      if (selection_get_end () != p) {
        //cout << "Set end selection: " << p << "\n";
        selection_set_end (p);
      }
    }
  }
}

void
edit_modify_rep::post_notify (path p) {
  // cout << "Post notify\n";
  if (!(rp <= p)) return;
  selection_cancel ();
  notify_change (THE_TREE);
  tp= position_get (cur_pos);
  position_delete (cur_pos);
  cur_pos= nil_observer;
  go_to_correct (tp);
  /*
  cout << "et= " << et << "\n";
  cout << "tp= " << tp << "\n\n";
  */
}

/******************************************************************************
* Hooks / notify changes to editor
******************************************************************************/

// FIXME: the notification might be slow when we have many
// open buffers. In the future, we might obtain the relevant editors
// from all possible prefixes of p using a hashtable

// FIXME: the undo system is not safe when a change is made inside
// a buffer which has no editor attached to it

void
edit_assign (editor_rep* ed, path pp, tree u) {
  path p= copy (pp);
  ASSERT (ed->the_buffer_path() <= p, "invalid modification");
  ed->notify_assign (p, u);
}

void
edit_insert (editor_rep* ed, path pp, tree u) {
  path p= copy (pp);
  ASSERT (ed->the_buffer_path() <= p, "invalid modification");
  ed->notify_insert (p, u);
}

void
edit_remove (editor_rep* ed, path pp, int nr) {
  path p= copy (pp);
  ASSERT (ed->the_buffer_path() <= p, "invalid modification");
  if (nr <= 0) return;
  ed->notify_remove (p, nr);
}

void
edit_split (editor_rep* ed, path pp) {
  path p= copy (pp);
  ASSERT (ed->the_buffer_path() <= p, "invalid modification");
  ed->notify_split (p);
}

void
edit_join (editor_rep* ed, path pp) {
  path p= copy (pp);
  ASSERT (ed->the_buffer_path() <= p, "invalid modification");
  if (N(p)<1) FAILED ("path too short in join");
  ed->notify_join (p);
}

void
edit_assign_node (editor_rep* ed, path pp, tree_label op) {
  path p= copy (pp);
  ASSERT (ed->the_buffer_path() <= p, "invalid modification");
  ed->notify_assign_node (p, op);
}

void
edit_insert_node (editor_rep* ed, path pp, tree t) {
  path p= copy (pp);
  ASSERT (ed->the_buffer_path() <= p, "invalid modification");
  ed->notify_insert_node (p, t);
}

void
edit_remove_node (editor_rep* ed, path pp) {
  path p= copy (pp);
  ASSERT (ed->the_buffer_path() <= p, "invalid modification");
  ed->notify_remove_node (p);
}

void
edit_set_cursor (editor_rep* ed, path pp, tree data) {
  path p= copy (pp);
  ASSERT (ed->the_buffer_path() <= p, "invalid modification");
  ed->notify_set_cursor (p, data);
}

void
edit_announce (editor_rep* ed, modification mod) {
  switch (mod->k) {
  case MOD_ASSIGN:
    edit_assign (ed, mod->p, mod->t);
    break;
  case MOD_INSERT:
    edit_insert (ed, mod->p, mod->t);
    break;
  case MOD_REMOVE:
    edit_remove (ed, path_up (mod->p), last_item (mod->p));
    break;
  case MOD_SPLIT:
    edit_split (ed, mod->p);
    break;
  case MOD_JOIN:
    edit_join (ed, mod->p);
    break;
  case MOD_ASSIGN_NODE:
    edit_assign_node (ed, mod->p, L(mod));
    break;
  case MOD_INSERT_NODE:
    edit_insert_node (ed, mod->p, mod->t);
    break;
  case MOD_REMOVE_NODE:
    edit_remove_node (ed, mod->p);
    break;
  case MOD_SET_CURSOR:
    edit_set_cursor (ed, mod->p, mod->t);
    break;
  default: FAILED ("invalid modification type");
  }
}

void
edit_done (editor_rep* ed, modification mod) {
  path p= copy (mod->p);
  ASSERT (ed->the_buffer_path() <= p, "invalid modification");
  if (mod->k != MOD_SET_CURSOR)
    ed->post_notify (p);
#ifdef EXPERIMENTAL
  copy_announce (subtree (ed->et, ed->rp), ed->cct, mod / ed->rp);
#endif
}

void
edit_touch (editor_rep* ed, path p) {
  //cout << "Touch " << p << "\n";
  ASSERT (ed->the_buffer_path() <= p, "invalid touch");
  ed -> typeset_invalidate (p);
}

/******************************************************************************
* undo and redo handling
******************************************************************************/

void
edit_modify_rep::clear_undo_history () {
  global_clear_history ();
}

double
edit_modify_rep::this_author () {
  return author;
}

void
edit_modify_rep::archive_state () {
  path sp1= selection_get_start ();
  path sp2= selection_get_end ();
  if (path_less (sp1, sp2)) {
    //cout << "Selection: " << sp1 << "--" << sp2 << "\n";
    set_cursor (sp2, compound ("end", as_string (author)));
    set_cursor (sp1, compound ("start", as_string (author)));
    set_cursor (tp, compound ("cursor", as_string (author)));
  }
  else set_cursor (tp, compound ("cursor-clear", as_string (author)));
}

void
edit_modify_rep::start_editing () {
  set_author (this_author ());
}

void
edit_modify_rep::end_editing () {
  global_confirm ();
}

void
edit_modify_rep::start_slave (double a) {
  arch->start_slave (a);
}

void
edit_modify_rep::mark_start (double a) {
  arch->mark_start (a);
}

bool
edit_modify_rep::mark_cancel (double a) {
  return arch->mark_cancel (a);
}

void
edit_modify_rep::mark_end (double a) {
  arch->mark_end (a);
}

void
edit_modify_rep::add_undo_mark () {
  arch->confirm ();
}

void
edit_modify_rep::remove_undo_mark () {
  arch->retract ();
}

int
edit_modify_rep::undo_possibilities () {
  return arch->undo_possibilities ();
}

void
edit_modify_rep::undo (bool redoable) {
  interrupt_shortcut ();
  arch->forget_cursor ();
  if (inside_graphics () && !as_bool (eval ("graphics-undo-enabled"))) {
    eval ("(graphics-reset-context 'undo)"); return; }
  if (arch->undo_possibilities () == 0) {
    set_message ("No more undo information available", "undo"); return; }
  if (redoable) {
    path p= arch->undo ();
    if (!is_nil (p)) go_to (p);
  }
  else arch->forget ();
  if (arch->conform_save ()) {
    set_message ("Your document is back in its original state", "undo");
    beep (); }
  if (inside_graphics ())
    eval ("(graphics-reset-context 'undo)");
}

void
edit_modify_rep::unredoable_undo () {
  undo (false);
}

void
edit_modify_rep::undo (int i) {
  ASSERT (i == 0, "invalid undo");
  undo (true);
}

int
edit_modify_rep::redo_possibilities () {
  return arch->redo_possibilities ();
}

void
edit_modify_rep::redo (int i) {
  interrupt_shortcut ();
  arch->forget_cursor ();
  if (arch->redo_possibilities () == 0) {
    set_message ("No more redo information available", "redo"); return; }
  path p= arch->redo (i);
  if (!is_nil (p)) go_to (p);
  if (arch->conform_save ()) {
    set_message ("Your document is back in its original state", "undo");
    beep (); }
}

void
edit_modify_rep::require_save () {
  arch->require_autosave ();
  arch->require_save ();
}

void
edit_modify_rep::notify_save (bool real_save) {
  arch->confirm ();
  arch->notify_autosave ();
  if (real_save) arch->notify_save ();
}

bool
edit_modify_rep::need_save (bool real_save) {
  if (arch->conform_save ()) return false;
  if (real_save) return true;
  return !arch->conform_autosave ();
}

void
edit_modify_rep::show_history () {
  arch->show_all ();
}

/******************************************************************************
* handling multiple cursor positions
******************************************************************************/

observer
edit_modify_rep::position_new (path p) {
  tree st= subtree (et, path_up (p));
  int index= last_item (p);
  observer o= tree_position (st, index);
  attach_observer (st, o);
  return o;
}

void
edit_modify_rep::position_delete (observer o) {
  tree st;
  int  index;
  if (o->get_position (st, index))
    detach_observer (st, o);
}

void
edit_modify_rep::position_set (observer o, path p) {
  tree st= subtree (et, path_up (p));
  int index= last_item (p);
  o->set_position (st, index);
}

path
edit_modify_rep::position_get (observer o) {
  //return super_correct (et, obtain_position (o));
  return correct_cursor (et, obtain_position (o));
}
