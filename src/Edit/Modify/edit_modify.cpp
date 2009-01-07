
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

extern int max_undo_depth;

/******************************************************************************
* Constructors and destructors
******************************************************************************/

edit_modify_rep::edit_modify_rep () {}
edit_modify_rep::~edit_modify_rep () {}

/******************************************************************************
* Notification of changes in document
******************************************************************************/

void
edit_modify_rep::notify_assign (path p, tree u) {
  (void) u;
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_assign (get_typesetter (), p - rp, u);
}

void
edit_modify_rep::notify_insert (path p, tree u) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_insert (get_typesetter (), p - rp, u);
}

void
edit_modify_rep::notify_remove (path p, int nr) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_remove (get_typesetter (), p - rp, nr);
}

void
edit_modify_rep::notify_split (path p) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_split (get_typesetter (), p - rp);
}

void
edit_modify_rep::notify_join (path p) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_join (get_typesetter (), p - rp);
}

void
edit_modify_rep::notify_assign_node (path p, tree_label op) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_assign_node (get_typesetter (), p - rp, op);
}

void
edit_modify_rep::notify_insert_node (path p, tree t) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_insert_node (get_typesetter (), p - rp, t);
}

void
edit_modify_rep::notify_remove_node (path p) {
  if (!(rp <= p)) return;
  cur_pos= position_new (tp);
  ::notify_remove_node (get_typesetter (), p - rp);
}

void
edit_modify_rep::post_notify (path p) {
  // cout << "Post notify\n";
  if (!(rp <= p)) return;
  selection_cancel ();
  invalidate_mutators ();
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
  if (N(p)<1) fatal_error ("path too short in join", "editor::join");
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
  default: FAILED ("invalid modification type");
  }
}

void
edit_done (editor_rep* ed, modification mod) {
  path p= copy (mod->p);
  ASSERT (ed->the_buffer_path() <= p, "invalid modification");
  ed->post_notify (p);
}

/******************************************************************************
* undo and redo handling
******************************************************************************/

static tree
encode (string op, path p, tree t) {
  string s= copy (op);
  while (!is_nil (p)) {
    s << ";" << as_string (p->item);
    p= p->next;
  }
  if (t == "") return s;
  else return tree (TUPLE, s, t);
}

static void
decode (tree x, string& op, path& p, tree& t) {
  string s;
  if (is_atomic (x)) {
    s= x->label;
    t= "";
  }
  else {
    s= x[0]->label;
    t= x[1];
  }

  p= path ();
  int i=N(s);
  while (i>0) {
    int end= i;
    for (; i>0; i--)
      if (s[i-1]==';') break;
    if (i==0) op= s (i, end);
    else {
      p= path (as_int (s (i, end)), p);
      i--;
    }
  }
}

void
archive (tm_buffer buf, string op, path p, tree t) {
  // cout << "Undone by " << op << " " << t << " at " << p << "\n";
  tree x= encode (op, p, t);
  if (buf->undo_flag) buf->redo= tree (BACKUP, x, buf->redo);
  else {
    if (!buf->redo_flag) {
      if (buf->redo != "nil") {
	buf->redo_to_undo ();
	buf->mark_undo_block ();
      }
      if ((max_undo_depth > 0) && (buf->undo_depth >= (2*max_undo_depth)))
	buf->truncate_undos (max_undo_depth);
    }
    if ((op == "remove") &&
	(buf->needs_to_be_autosaved ()) &&
	(buf->undo != "nil") && (buf->undo[0] == "") &&
	(buf->undo[1] != "nil") &&
	(buf->undo[1][1] != "nil") && (buf->undo[1][1][0] == ""))
      {
	string op2; path p2; tree t2;
	decode (buf->undo[1][0], op2, p2, t2);
	if ((op2 == "remove") && (p == path_add (p2, as_int (t2)))) {
	  if (is_atomic (subtree (the_et, path_up (p)))) {
	    buf->unmark_undo_block ();
	    buf->undo= buf->undo [1];
	    int nr= as_int (t2)+ as_int (t);
	    x= encode (op, p2, as_string (nr));
	  }
	}
      }
    buf->undo= tree (BACKUP, x, buf->undo);
  }
  /*
  cout << "undo tree : " << buf->undo << "\n";
  cout << "redo tree : " << buf->redo << "\n";
  cout << "exdo tree : " << buf->exdo << "\n";
  cout << "undo depth: " << buf->undo_depth << "\n";
  cout << "redo depth: " << buf->redo_depth << "\n";
  cout << "last save : " << buf->last_save << "\n";
  */
}

void
edit_modify_rep::mark_undo_blocks () {
  int i;
  for (i=0; i<sv->nr_bufs(); i++) {
    tm_buffer b= sv->get_buf (i);
    b->mark_undo_block ();
  }
}

void
edit_modify_rep::remove_undo_mark () {
  if (buf->undo != "nil") {
    tree s= buf->undo, t= buf->undo[1];
    while ((t != "nil") && (t[0] != "")) {
      s= t;
      t= t[1];
    }
    if (t != "nil") {
      s[1]= t[1];
      buf->undo_depth--;
    }
  }
}

void
edit_modify_rep::add_undo_mark () {
  buf->mark_undo_block ();
}

void
edit_modify_rep::undo (bool redoable) {
  if (inside_graphics () && !as_bool (eval ("graphics-undo-enabled"))) {
    eval ("(graphics-reset-context 'undo)");
    return;
  }
  buf->unmark_undo_block ();
  if (buf->undo == "nil") {
    set_message ("No more undo information available", "undo");
    return;
  }
  buf->mark_redo_block ();
  while ((buf->undo != "nil") && (buf->undo[0] != "")) {
    tree x= buf->undo[0];
    buf->undo= buf->undo[1];
    buf->undo_flag= true;
    buf->exdo= tree (BACKUP, copy (x), buf->exdo);
    perform_undo_redo (x);
    buf->undo_flag= false;
  }
  if (!redoable) {
    buf->unmark_redo_block ();
    while ((buf->redo != "nil") && (buf->redo[0] != ""))
      buf->redo= buf->redo[1];
    buf->unmark_redo_block ();
  }         
  buf->unmark_undo_block ();
  if (buf->undo_depth == buf->last_save) {
    beep ();
    set_message ("Your document is back in its original state", "undo");
  }
  if (inside_graphics ())
    eval ("(graphics-reset-context 'undo)");
}

void
edit_modify_rep::forget_undo () {
  buf->unmark_undo_block ();
  while ((buf->undo != "nil") && (buf->undo[0] != ""))
    buf->undo= buf->undo[1];
  buf->unmark_undo_block ();
}

void
edit_modify_rep::unredoable_undo () {
  undo (false);
}

void
edit_modify_rep::undo () {
  undo (true);
}

void
edit_modify_rep::redo () {
  buf->unmark_redo_block ();
  if (buf->redo == "nil") {
    set_message ("No more redo information available", "redo");
    return;
  }
  buf->mark_undo_block ();
  while ((buf->redo != "nil") && (buf->redo[0] != "")) {
    tree x= buf->redo[0];
    buf->redo= buf->redo[1];
    buf->exdo= buf->exdo[1];
    buf->redo_flag= true;
    perform_undo_redo (x);
    buf->redo_flag= false;
  }
  buf->unmark_redo_block ();
  if (buf->undo_depth == buf->last_save) {
    beep ();
    set_message ("Your document is back in its original state", "redo");
  }
}

void
edit_modify_rep::perform_undo_redo (tree x) {
  string op; path p; tree t;
  decode (x, op, p, t);

  if (op == "assign") {
    assign (p, t);
    go_to (end (et, p));
  }
  else if (op == "insert") {
    insert (p, t);
    if (is_atomic (t)) go_to (path_add (p, N(t->label)));
    else go_to (end (et, path_add (p, N(t)-1)));
  }
  else if (op == "remove") {
    if (is_atomic (subtree (et, path_up (p)))) {
      remove (p, as_int (t));
      go_to (p);
    }
    else {
      remove (p, as_int (t));
      if (last_item (p) == 0) go_to (start (et, p));
      else go_to (end (et, path_dec (p)));
    }
  }
  else if (op == "split") {
    split (p);
    path q= path_inc (path_up (p));
    if (is_atomic (subtree (et, q))) go_to (q * 0);
    else go_to (start (et, q));
    // else go_to (start (et, q * 0));
  }
  else if (op == "join") {
    tree& st1= subtree (et, p);
    tree& st2= subtree (et, path_inc (p));
    if (is_atomic (st1) && is_atomic (st2)) {
      int last= N (st1->label);
      join (p);
      go_to (p * last);
    }
    else {
      int  last= arity (st1) - 1;
      join (p);
      if (last == -1) go_to (start (et, p));
      else go_to (end (et, p * last));
    }
  }
  else if (op == "assign_node") {
    if (p <= tp) assign_node (p, as_tree_label (t->label));
    else {
      assign_node (p, as_tree_label (t->label));
      go_to (end (et, p));
    }
  }
  else if (op == "insert_node") {
    if (p < tp) insert_node (p, t);
    else {
      insert_node (p, t);
      go_to (end (et, p));
    }
  }
  else if (op == "remove_node") {
    if (p < tp) remove_node (p);
    else if (tp == path_up (p) * 0) {
      remove_node (p);
      go_to (start (et, path_up (p)));
    }
    else {
      remove_node (p);
      go_to (end (et, path_up (p)));
    }
  }
}

/******************************************************************************
* Hooks / notify changes to undoer
******************************************************************************/

void
archive_assign (tm_buffer buf, path pp, tree u) {
  path p= copy (pp);
  ASSERT (buf->rp <= p, "invalid modification");
  archive (buf, "assign", p, subtree (the_et, p));

#ifdef EXPERIMENTAL
  global_notify_assign (p, u);
#endif
}

void
archive_insert (tm_buffer buf, path pp, tree u) {
  path p= copy (pp);
  ASSERT (buf->rp <= p, "invalid modification");
  archive (buf, "remove", p, as_string (is_atomic (u)? N(u->label): N(u)));

#ifdef EXPERIMENTAL
  global_notify_insert (p, u);
#endif
}

void
archive_remove (tm_buffer buf, path pp, int nr) {
  path p= copy (pp);
  ASSERT (buf->rp <= p, "invalid modification");
  if (nr <= 0) return;
  tree& st= subtree (the_et, path_up (p));
  int l= last_item (p);
  if (is_atomic (st)) archive (buf, "insert", p, st->label (l, l+ nr));
  else archive (buf, "insert", p, st (l, l+ nr));

#ifdef EXPERIMENTAL
  global_notify_remove (p, nr);
#endif
}

void
archive_split (tm_buffer buf, path pp) {
  path p= copy (pp);
  ASSERT (buf->rp <= p, "invalid modification");
  if (N(p)<2) fatal_error ("path too short in split", "editor::split");
  archive (buf, "join", path_up (p), "");

#ifdef EXPERIMENTAL
  global_notify_split (p);
#endif
}

void
archive_join (tm_buffer buf, path pp) {
  path p= copy (pp);
  ASSERT (buf->rp <= p, "invalid modification");
  if (N(p)<1) fatal_error ("path too short in join", "editor::join");
  tree& st= subtree (the_et, path_up (p));
  int  l1 = last_item (p);
  // int  l2 = is_atomic (st[l1])? N (st[l1]->label): N (st[l1]);
  if (l1+1 >= arity (st)) fatal_error ("invalid join", "archive_join");
  bool string_mode= is_atomic (st[l1]) && is_atomic (st[l1+1]);
  int len= string_mode? N (st[l1]->label): arity (st[l1]);
  archive (buf, "split", p * len, "");

#ifdef EXPERIMENTAL
  global_notify_join (p);
#endif
}

void
archive_assign_node (tm_buffer buf, path pp, tree_label op) {
  path p= copy (pp);
  ASSERT (buf->rp <= p, "invalid modification");
  tree& st= subtree (the_et, p);
  archive (buf, "assign_node", p, get_label (st));

#ifdef EXPERIMENTAL
  global_notify_assign_node (p, op);
#endif
}

void
archive_insert_node (tm_buffer buf, path pp, tree t) {
  path p= copy (pp);
  ASSERT (buf->rp <= p, "invalid modification");
  archive (buf, "remove_node", p, "");

#ifdef EXPERIMENTAL
  global_notify_insert_node (p, t);
#endif
}

void
archive_remove_node (tm_buffer buf, path pp) {
  path p= copy (pp);
  ASSERT (buf->rp <= p, "invalid modification");
  int pos= last_item (pp);
  tree& st= subtree (the_et, path_up (p));
  archive (buf, "insert_node", p, st (0, pos) * st (pos+1, N(st)));

#ifdef EXPERIMENTAL
  global_notify_remove_node (p);
#endif
}

void
archive_announce (tm_buffer buf, modification mod) {
  switch (mod->k) {
  case MOD_ASSIGN:
    archive_assign (buf, mod->p, mod->t);
    break;
  case MOD_INSERT:
    archive_insert (buf, mod->p, mod->t);
    break;
  case MOD_REMOVE:
    archive_remove (buf, path_up (mod->p), last_item (mod->p));
    break;
  case MOD_SPLIT:
    archive_split (buf, mod->p);
    break;
  case MOD_JOIN:
    archive_join (buf, mod->p);
    break;
  case MOD_ASSIGN_NODE:
    archive_assign_node (buf, mod->p, L(mod));
    break;
  case MOD_INSERT_NODE:
    archive_insert_node (buf, mod->p, mod->t);
    break;
  case MOD_REMOVE_NODE:
    archive_remove_node (buf, mod->p);
    break;
  default: FAILED ("invalid modification type");
  }
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
