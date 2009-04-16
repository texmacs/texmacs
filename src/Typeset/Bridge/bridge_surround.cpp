
/******************************************************************************
* MODULE     : bridge_surround.cpp
* DESCRIPTION: Bridge between logical and physical paragraph surroundings
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bridge.hpp"

class bridge_surround_rep: public bridge_rep {
protected:
  bridge               body;
  hashmap<string,tree> changes_before;

  bool                 corrupted;
  array<line_item>     a;
  array<line_item>     b;

public:
  bridge_surround_rep (typesetter ttt, tree st, path ip);
  void initialize ();

  void notify_assign (path p, tree u);
  void notify_insert (path p, tree u);
  void notify_remove (path p, int nr);
  bool notify_macro  (int type, string var, int level, path p, tree u);
  void notify_change ();

  void my_clean_links ();
  void my_exec_until (path p);
  bool my_typeset_will_be_complete ();
  void my_typeset (int desired_status);
};

bridge_surround_rep::bridge_surround_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip), changes_before (UNINIT)
{
  initialize ();
}

void
bridge_surround_rep::initialize () {
  while (N(st)<3) // hack for temporarily incorrect situations (A-backspace)
    st= tree (SURROUND, "") * st;
  if (is_nil (body)) body= make_bridge (ttt, st[2], descend (ip, 2));
  else replace_bridge (body, st[2], descend (ip, 2));
  changes_before= hashmap<string,tree> (UNINIT);
  corrupted= true;
}

bridge
bridge_surround (typesetter ttt, tree st, path ip) {
  return tm_new<bridge_surround_rep> (ttt, st, ip);
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_surround_rep::notify_assign (path p, tree u) {
  // cout << "Assign " << p << ", " << u << " in " << st << "\n";
  ASSERT (!is_nil (p) || is_func (u, SURROUND), "nil path");
  if (is_nil (p) || (p->item != 2)) {
    st= substitute (st, p, u);
    initialize ();
  }
  else {
    // bool mp_flag= is_multi_paragraph (st);
    if (is_atom (p)) {
      body= make_bridge (ttt, u, descend (ip, 2));
      st= substitute (st, 2, body->st);
    }
    else {
      body->notify_assign (p->next, u);
      st= substitute (st, p->item, body->st);
    }
    // if (mp_flag != is_multi_paragraph (st)) initialize ();
  }
  status= CORRUPTED;
}

void
bridge_surround_rep::notify_insert (path p, tree u) {
  // cout << "Insert " << p << ", " << u << " in " << st << "\n";
  ASSERT (!is_nil (p), "nil path");
  if (is_atom (p) || (p->item != 2)) bridge_rep::notify_insert (p, u);
  else {
    // bool mp_flag= is_multi_paragraph (st);
    body->notify_insert (p->next, u);
    st= substitute (st, 2, body->st);
    // if (mp_flag != is_multi_paragraph (st)) initialize ();
  }
  status= CORRUPTED;
}

void
bridge_surround_rep::notify_remove (path p, int nr) {
  // cout << "Remove " << p << ", " << nr << " in " << st << "\n";
  ASSERT (!is_nil (p), "nil path");
  if (is_atom (p) || (p->item != 2)) bridge_rep::notify_remove (p, nr);
  else {
    // bool mp_flag= is_multi_paragraph (st);
    body->notify_remove (p->next, nr);
    st= substitute (st, 2, body->st);
    // if (mp_flag != is_multi_paragraph (st)) initialize ();
  }
  status= CORRUPTED;
}

bool
bridge_surround_rep::notify_macro (int tp, string var, int l, path p, tree u) {
  /*
  cout << "Macro argument " << var << " [action=" << tp
       << ", level=" << l << "] " << p << ", " << u << " in " << st << "\n";
  */

  bool flag= env->depends (st[0], var, l) || env->depends (st[1], var, l);
  if (flag) initialize ();
  flag= body->notify_macro (tp, var, l, p, u) || flag;
  if (flag) status= CORRUPTED;
  return flag;
}

void
bridge_surround_rep::notify_change () {
  status= CORRUPTED;
  corrupted= true;
  body->notify_change ();
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_surround_rep::my_clean_links () {
  if (corrupted || (N(ttt->old_patch) != 0))
    link_env= link_repository (true);
}

void
bridge_surround_rep::my_exec_until (path p) {
  if (p->item == 2) {
    env->patch_env (changes_before);
    body->exec_until (p->next);
  }
  else env->exec_until (st, p);
}

bool
bridge_surround_rep::my_typeset_will_be_complete () {
  if (status != CORRUPTED) return false;
  return body->my_typeset_will_be_complete ();
}

void
bridge_surround_rep::my_typeset (int desired_status) {
  if (corrupted || (N(ttt->old_patch) != 0)) {
    hashmap<string,tree> prev_back (UNINIT);
    env->local_start (prev_back);
    /*
    cout << st[0] << "\n";
    cout << st[1] << "\n";
    cout << "-------------------------------------------------------------\n";
    */
    a= typeset_concat (env, st[0], descend (ip, 0));
    b= typeset_concat (env, st[1], descend (ip, 1));
    env->local_update (ttt->old_patch, changes_before);
    env->local_end (prev_back);
    corrupted= false;
  }
  else env->monitored_patch_env (changes_before);

  ttt->insert_marker (st, ip);
  ttt->insert_surround (a, b);
  body->typeset (desired_status);
}
