
/******************************************************************************
* MODULE     : edit_modify.cpp
* DESCRIPTION: base routines for modifying the edit tree + notification
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "edit_modify.hpp"
#include "tm_buffer.hpp"

extern int max_undo_depth;

/******************************************************************************
* Constructors and destructors
******************************************************************************/

edit_modify_rep::edit_modify_rep ():
  undo_flag (false), redo_flag (false),
  pps (1), nr_to_code (1), code_to_nr (-1) {}
edit_modify_rep::~edit_modify_rep () {}

/******************************************************************************
* modification routines
******************************************************************************/

// FIXME: the following notification loop is slow when we have many
// open buffers. In the future, we might obtain the relevant editors
// from all possible prefixes of p using a hashtable

#define FOR_ALL_EDITORS_BEGIN(p) \
  int i, j; \
  for (i=0; i<sv->nr_bufs(); i++) { \
    tm_buffer b= sv->get_buf (i); \
    if (b->rp <= p) \
      for (j=0; j<N(b->vws); j++) { \
	editor& ed= ((tm_view) (b->vws[j]))->ed;

#define FOR_ALL_EDITORS_END \
      } \
  }

void
edit_modify_rep::assign (path pp, tree u) {
  path p= copy (pp);
  // cout << "Assign " << u << " at " << p << "\n";
  notify_undo ("assign", p, subtree (et, p));

  FOR_ALL_EDITORS_BEGIN (p)
    ed->notify_assign (p, u);
  FOR_ALL_EDITORS_END

  subtree (et, p)= u;
  finished (pp);
}

void
edit_modify_rep::insert (path pp, tree u) {
  path p= copy (pp);
  // cout << "Insert " << u << " at " << p << "\n";
  notify_undo ("remove", p, as_string (is_atomic (u)? N(u->label): N(u)));

  FOR_ALL_EDITORS_BEGIN (p)
    ed->notify_insert (p, u);
  FOR_ALL_EDITORS_END

  insert_at (et, p, u);
  finished (pp);
}

void
edit_modify_rep::remove (path pp, int nr) {
  if (nr <= 0) return;
  path p= copy (pp);
  // cout << "Remove " << nr << " at " << p << "\n";
  tree& st= subtree (et, path_up (p));
  int l= last_item (p);
  if (is_atomic (st)) notify_undo ("insert", p, st->label (l, l+ nr));
  else notify_undo ("insert", p, st (l, l+ nr));

  FOR_ALL_EDITORS_BEGIN (p)
    ed->notify_remove (p, nr);
  FOR_ALL_EDITORS_END

  remove_at (et, p, nr);
  finished (pp);
}

void
edit_modify_rep::split (path pp) {
  path p= copy (pp);
  // cout << "Split at " << p << "\n";
  if (N(p)<2) fatal_error ("path too short in split", "editor::split");
  tree& st= subtree (et, path_up (path_up (p)));
  int  l1 = last_item (path_up (p));
  int  l2 = last_item (p);
  notify_undo ("join", path_up (p), "");

  FOR_ALL_EDITORS_BEGIN (p)
    ed->notify_split (p);
  FOR_ALL_EDITORS_END

  if (is_atomic (st[l1])) {
    string s1, s2;
    ::split (st[l1]->label, l2, s1, s2);
    st[l1]= s2;
    st= insert_one (st, l1, tree (s1));
  }
  else {
    tree st1, st2;
    ::split (st[l1], l2, st1, st2);
    st[l1]= st2;
    st= insert_one (st, l1, st1);
  }
  finished (pp);
}

void
edit_modify_rep::join (path pp) {
  path p= copy (pp);
  // cout << "Join at " << p << "\n";
  if (N(p)<1) fatal_error ("path too short in join", "editor::join");
  tree& st= subtree (et, path_up (p));
  int  l1 = last_item (p);
  // int  l2 = is_atomic (st[l1])? N (st[l1]->label): N (st[l1]);
  if (l1+1 >= arity (st)) fatal_error ("invalid join", "editor::join");
  bool string_mode= is_atomic (st[l1]) && is_atomic (st[l1+1]);
  int len= string_mode? N (st[l1]->label): arity (st[l1]);
  notify_undo ("split", p * len, "");

  FOR_ALL_EDITORS_BEGIN (p)
    ed->notify_join (p);
  FOR_ALL_EDITORS_END

  if (string_mode) st[l1]->label << st[l1+1]->label;
  else {
    if (is_atomic (st[l1  ])) st[l1  ]= tree (L(st[l1+1]), st[l1  ]);
    if (is_atomic (st[l1+1])) st[l1+1]= tree (L(st[l1  ]), st[l1+1]);
    st[l1] << A (st[l1+1]);
  }
  st= ::remove (st, l1+1, 1);
  finished (pp);
}

void
edit_modify_rep::ins_unary (path pp, tree_label op) {
  path p= copy (pp);
  // cout << "Insert unary " << get_label (tree (op)) << " at " << p << "\n";
  notify_undo ("rem_unary", p, "");

  FOR_ALL_EDITORS_BEGIN (p)
    ed->notify_ins_unary (p, op);
  FOR_ALL_EDITORS_END

  tree& st= subtree (et, p);
  st= tree (op, st);
  finished (pp);
}

void
edit_modify_rep::rem_unary (path pp) {
  path p= copy (pp);
  // cout << "Remove unary at " << p << "\n";
  tree& st= subtree (et, p);
  if (arity (st) != 1) fatal_error ("not a unary tree", "editor::rem_unary");
  notify_undo ("ins_unary", p, get_label (st));

  FOR_ALL_EDITORS_BEGIN (p)
    ed->notify_rem_unary (p);
  FOR_ALL_EDITORS_END

  st= st[0];
  finished (pp);
}

void
edit_modify_rep::finished (path pp) {
  FOR_ALL_EDITORS_BEGIN (pp)
    ed->post_notify (pp);
  FOR_ALL_EDITORS_END
}

/******************************************************************************
* Cursor handling after notification of changes in document
******************************************************************************/

#define FOR_ALL_POINTERS_BEGIN \
  pps[0]= tp; \
  int idx, n= N(pps); \
  for (idx=0; idx<n; idx++) { \
    path& pp= pps[idx];

#define FOR_ALL_POINTERS_END \
  } \
  tp= pps[0]

void
edit_modify_rep::notify_assign (path p, tree u) { (void) u;
  if (!(rp <= p)) return;
  FOR_ALL_POINTERS_BEGIN
    if (p<pp) pp= p * 0;
  FOR_ALL_POINTERS_END;
  ::notify_assign (get_typesetter (), p - rp, u);
}

void
edit_modify_rep::notify_insert (path p, tree u) {
  if (!(rp <= p)) return;
  FOR_ALL_POINTERS_BEGIN
    if ((N(p)>=2) && path_inf (path_up (p), pp));
    else if (path_inf (p, pp) || (p <= pp)) {
      int nr= is_atomic (u)? N(u->label): N(u);
      pp[N(p)-1] += nr;
    }
  FOR_ALL_POINTERS_END;
  ::notify_insert (get_typesetter (), p - rp, u);
}

void
edit_modify_rep::notify_remove (path p, int nr) {
  if (!(rp <= p)) return;
  FOR_ALL_POINTERS_BEGIN
    if ((N(p)>=2) && path_inf (path_up (p), pp));
    else {
      tree& st= subtree (et, path_up (p));
      path p2= path_add (p, nr, N(p)-1);
      if ((path_up (p) <= pp) && (arity (st) == nr))
	pp= path_up (p) * 0;
      else if (path_inf (p2, pp) || (p2 <= pp)) pp[N(p)-1] -= nr;
      else if (path_inf (p, pp)  || (p <= pp)) {
	if (is_atomic (st) || (last_item(p)==0)) pp= copy (p);
	else pp= path_dec (p) * right_index (st [last_item(p)-1]);
      }
    }
  FOR_ALL_POINTERS_END;
  ::notify_remove (get_typesetter (), p - rp, nr);
}

void
edit_modify_rep::notify_split (path p) {
  if (!(rp <= p)) return;
  FOR_ALL_POINTERS_BEGIN
    if (!(path_up (p, 2) <= path_up (pp)));
    else if (path_up (p, 2) == path_up (pp));
    else if (pp[N(p)-2]<p[N(p)-2]);
    else if (pp[N(p)-2]>p[N(p)-2]) pp[N(p)-2] ++;
    else if ((path_up (p) == path_up (pp)) &&
	     is_compound (subtree (et, path_up (p)))) {
      if (last_item (pp)==1) pp[N(p)-2] ++;
    }
    else if (pp[N(p)-1]<p[N(p)-1]);
    else {
      pp[N(p)-2] ++;
      pp[N(p)-1] -= last_item (p);
    }
  FOR_ALL_POINTERS_END;
  ::notify_split (get_typesetter (), p - rp);
}

void
edit_modify_rep::notify_join (path p) {
  if (!(rp <= p)) return;
  FOR_ALL_POINTERS_BEGIN
    tree& st= subtree (et, p);
    bool flag = is_atomic (st);
    int  extra= flag? N(st->label): N(st);
    if (!(path_up (p) <= path_up (pp)));
    else if (path_up (p) == path_up (pp));
    else if (pp[N(p)-1] <= p[N(p)-1]);
    else {
      if ((pp[N(p)-1] == p[N(p)-1]+1) &&
	  (flag || (path_inc (p) != path_up (pp)))) pp[N(p)] += extra;
      pp[N(p)-1] --;
    }
    FOR_ALL_POINTERS_END;
  ::notify_join (get_typesetter (), p - rp);
}

void
edit_modify_rep::notify_ins_unary (path p, tree_label op) { (void) op;
  if (!(rp <= p)) return;
  FOR_ALL_POINTERS_BEGIN
    if (p <= path_up (pp)) {
      path add= path (0, tail (pp, N(p)));
      pp= copy (p) * add;
    }
  FOR_ALL_POINTERS_END;
  ::notify_ins_unary (get_typesetter (), p - rp, op);
}

void
edit_modify_rep::notify_rem_unary (path p) {
  if (!(rp <= p)) return;
  FOR_ALL_POINTERS_BEGIN
    if (p == path_up (pp)) {
      if (last_item (pp)==1)
	pp[N(pp)-1]= right_index (subtree (et, p * 0));
    }
    else if (p <= path_up (pp)) {
      path add= tail (pp, N(p)+1);
      pp= p * add;
    }
  FOR_ALL_POINTERS_END;
  ::notify_rem_unary (get_typesetter (), p - rp);
}

void
edit_modify_rep::post_notify (path p) {
  if (!(rp <= p)) return;
  selection_cancel ();
  notify_change (THE_TREE);
  FOR_ALL_POINTERS_BEGIN
    pp= correct_cursor (et, pp);
  FOR_ALL_POINTERS_END;
  go_to (tp);
  /*
  cout << "et= " << et << "\n";
  cout << "tp= " << tp << "\n\n";
  */
}

/******************************************************************************
* undo and redo handling
******************************************************************************/

static tree
encode (string op, path p, tree t) {
  string s= copy (op);
  while (!nil (p)) {
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
edit_modify_rep::notify_undo (string op, path p, tree t) {
  // cout << "Undone by " << op << " " << t << " at " << p << "\n";
  tree x= encode (op, p, t);
  if (undo_flag) buf->redo= tree (BACKUP, x, buf->redo);
  else {
    if (!redo_flag) {
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
	  if (is_atomic (subtree (et, path_up (p)))) {
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
edit_modify_rep::undo () {
  buf->unmark_undo_block ();
  if (buf->undo == "nil") {
    set_message ("No more undo information available", "undo");
    return;
  }
  buf->mark_redo_block ();
  while ((buf->undo != "nil") && (buf->undo[0] != "")) {
    tree x= buf->undo[0];
    buf->undo= buf->undo[1];
    undo_flag= true;
    buf->exdo= tree (BACKUP, copy (x), buf->exdo);
    perform_undo_redo (x);
    undo_flag= false;
  }
  buf->unmark_undo_block ();
  if (buf->undo_depth == buf->last_save) {
    cerr << '\a';
    set_message ("Your document is back in its original state", "undo");
  }
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
    redo_flag= true;
    perform_undo_redo (x);
    redo_flag= false;
  }
  buf->unmark_redo_block ();
  if (buf->undo_depth == buf->last_save) {
    cerr << '\a';
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
  else if (op == "ins_unary") {
    if (p < tp) ins_unary (p, as_tree_label (t->label));
    else {
      ins_unary (p, as_tree_label (t->label));
      go_to (end (et, p * 0));
    }
  }
  else if (op == "rem_unary") {
    if (p * 0 < tp) rem_unary (p);
    else if (tp == p * 0) {
      rem_unary (p);
      go_to (start (et, p));
    }
    else {
      rem_unary (p);
      go_to (end (et, p));
    }
  }
}

/******************************************************************************
* Utility for only changing differences (very crude implementation though)
******************************************************************************/

void
edit_modify_rep::assign_diff (path p, tree t) {
  tree st= subtree (et, p);
  if (t == st) return;
  if (is_atomic (t) || (L(t) != L(st))) {
    assign (p, t);
    return;
  }
  int i, n= min (N(st), N(t));
  for (i=0; i<n; i++)
    assign_diff (p * i, t[i]);
  if (n < N(st)) remove (p * n, N(st)-n);
  else if (n < N(t)) insert (p * n, t (n, N(t)));
}

/******************************************************************************
* handling multiple cursor positions
******************************************************************************/

int
edit_modify_rep::position_new () {
  int i, n= N(pps);
  for (i=0; i<n; i++)
    if (!code_to_nr->contains (i))
      break;
  pps << copy (tp);
  nr_to_code << i;
  code_to_nr (i)= n;
  return i;
}

void
edit_modify_rep::position_delete (int i) {
  if (!code_to_nr->contains (i)) return;
  int j= code_to_nr (i), n= N(pps), k;
  for (k=j; k<n-1; k++) {
    int l= nr_to_code[k+1];
    pps[k]= pps[k+1];
    nr_to_code[k]= l;
    code_to_nr(l)= k;
  }
  pps->resize (n-1);
  nr_to_code->resize (n-1);
  code_to_nr->reset (i);
}

void
edit_modify_rep::position_set (int i, path p) {
  pps[code_to_nr(i)]= copy (p);
}

path
edit_modify_rep::position_get (int i) {
  return copy (pps[code_to_nr(i)]);
}
