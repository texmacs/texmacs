
/******************************************************************************
* MODULE     : bridge_locus.cpp
* DESCRIPTION: Bridge between logical and physical local enviroment changes
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bridge.hpp"

class bridge_locus_rep: public bridge_rep {
protected:
  int    last;
  bridge body;

public:
  bridge_locus_rep (typesetter ttt, tree st, path ip);
  void initialize ();

  void notify_assign (path p, tree u);
  void notify_insert (path p, tree u);
  void notify_remove (path p, int nr);
  bool notify_macro  (int type, string var, int level, path p, tree u);
  void notify_change ();

  void my_exec_until (path p);
  bool my_typeset_will_be_complete ();
  void my_typeset (int desired_status);
};

bridge_locus_rep::bridge_locus_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip)
{
  initialize ();
}

void
bridge_locus_rep::initialize () {
  last= N(st)-1;
  if (is_nil(body)) body= make_bridge (ttt, st[last], descend (ip, last));
  else replace_bridge (body, st[last], descend (ip, last));
}

bridge
bridge_locus (typesetter ttt, tree st, path ip) {
  return tm_new<bridge_locus_rep> (ttt, st, ip);
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_locus_rep::notify_assign (path p, tree u) {
  // cout << "Assign " << p << ", " << u << " in " << st << "\n";
  ASSERT (!is_nil (p) || is_func (u, LOCUS), "nil path");
  if (is_nil (p)) {
    st= u;
    initialize ();
  }
  else {
    bool mp_flag= is_multi_paragraph (st);
    if (p->item == last) {
      if (is_atom (p)) body= make_bridge (ttt, u, descend (ip, last));
      else body->notify_assign (p->next, u);
      st= substitute (st, p->item, body->st);
    }
    else {
      st= substitute (st, p, u);
      body->notify_change ();
    }
    if (mp_flag != is_multi_paragraph (st)) initialize ();
  }
  status= CORRUPTED;
}

void
bridge_locus_rep::notify_insert (path p, tree u) {
  // cout << "Insert " << p << ", " << u << " in " << st << "\n";
  ASSERT (!is_nil (p), "nil path");
  if (is_atom (p) || (p->item != last)) bridge_rep::notify_insert (p, u);
  else {
    bool mp_flag= is_multi_paragraph (st);
    body->notify_insert (p->next, u);
    st= substitute (st, last, body->st);
    if (mp_flag != is_multi_paragraph (st)) initialize ();
  }
  status= CORRUPTED;
}

void
bridge_locus_rep::notify_remove (path p, int nr) {
  // cout << "Remove " << p << ", " << nr << " in " << st << "\n";
  ASSERT (!is_nil (p), "nil path");
  if (is_atom (p) || (p->item != last)) bridge_rep::notify_remove (p, nr);
  else {
    bool mp_flag= is_multi_paragraph (st);
    body->notify_remove (p->next, nr);
    st= substitute (st, last, body->st);
    if (mp_flag != is_multi_paragraph (st)) initialize ();
  }
  status= CORRUPTED;
}

bool
bridge_locus_rep::notify_macro (int type, string var, int l, path p, tree u) {
  bool flag= body->notify_macro (type, var, l, p, u);
  if (flag) status= CORRUPTED;
  return flag;
}

void
bridge_locus_rep::notify_change () {
  status= CORRUPTED;
  body->notify_change ();
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_locus_rep::my_exec_until (path p) {
  if (p->item == last) body->exec_until (p->next);
  else env->exec_until (st, p);
}

bool
bridge_locus_rep::my_typeset_will_be_complete () {
  if (status != CORRUPTED) return false;
  return body->my_typeset_will_be_complete ();
}

void
bridge_locus_rep::my_typeset (int desired_status) {
  extern bool build_locus (edit_env env, tree t, list<string>& ids, string& c);
  list<string> ids;
  string col;
  bool ok= build_locus (env, st, ids, col);
  if (!ok) typeset_warning << "Ignored unaccessible loci\n";
  tree old_col= env->read (COLOR);
  env->write_update (COLOR, col);
  ttt->insert_marker (st, ip);
  body->typeset (desired_status);
  env->write_update (COLOR, old_col);
}
