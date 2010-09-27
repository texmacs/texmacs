
/******************************************************************************
* MODULE     : bridge_highlight.cpp
* DESCRIPTION: Bridge for highlighted portions of text
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bridge.hpp"
#include "packrat.hpp"

class bridge_highlight_rep: public bridge_rep {
protected:
  bridge body;
  tree ht;

public:
  bridge_highlight_rep (typesetter ttt, tree st, path ip);
  ~bridge_highlight_rep ();
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

bridge_highlight_rep::bridge_highlight_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip)
{
  initialize ();
}

bridge_highlight_rep::~bridge_highlight_rep () {
  detach_highlight (ht);
}

void
bridge_highlight_rep::initialize () {
  if (is_nil (body)) body= make_bridge (ttt, st[0], descend (ip, 0));
  else replace_bridge (body, st[0], descend (ip, 0));
}

bridge
bridge_highlight (typesetter ttt, tree st, path ip) {
  return tm_new<bridge_highlight_rep> (ttt, st, ip);
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_highlight_rep::notify_assign (path p, tree u) {
  //cout << "Assign " << p << ", " << u << " in " << st << "\n";
  ASSERT (!is_nil (p) || is_func (u, HIGHLIGHT), "nil path");
  if (is_nil (p)) {
    st= u;
    initialize ();
  }
  else {
    bool mp_flag= is_multi_paragraph (st);
    if (p->item == 0) {
      if (is_atom (p)) body= make_bridge (ttt, u, descend (ip, 0));
      else body->notify_assign (p->next, u);
      st= substitute (st, p->item, body->st);
    }
    else {
      st= substitute (st, p, u);
      body->notify_change ();
    }
    if (mp_flag != is_multi_paragraph (st)) initialize ();
    initialize ();
  }
  status= CORRUPTED;
}

void
bridge_highlight_rep::notify_insert (path p, tree u) {
  //cout << "Insert " << p << ", " << u << " in " << st << "\n";
  ASSERT (!is_nil (p), "nil path");
  if (is_atom (p) || (p->item != 0)) bridge_rep::notify_insert (p, u);
  else {
    bool mp_flag= is_multi_paragraph (st);
    body->notify_insert (p->next, u);
    st= substitute (st, 0, body->st);
    if (mp_flag != is_multi_paragraph (st)) initialize ();
  }
  initialize ();
  status= CORRUPTED;
}

void
bridge_highlight_rep::notify_remove (path p, int nr) {
  //cout << "Remove " << p << ", " << nr << " in " << st << "\n";
  ASSERT (!is_nil (p), "nil path");
  if (is_atom (p) || (p->item != 0)) bridge_rep::notify_remove (p, nr);
  else {
    bool mp_flag= is_multi_paragraph (st);
    body->notify_remove (p->next, nr);
    st= substitute (st, 0, body->st);
    if (mp_flag != is_multi_paragraph (st)) initialize ();
  }
  initialize ();
  status= CORRUPTED;
}

bool
bridge_highlight_rep::notify_macro (int type, string var, int l, path p, tree u) {
  //cout << "Macro argument " << var << " [action=" << type
  //     << ", level=" << l << "] " << p << ", " << u << " in " << st << "\n";
  bool flag= body->notify_macro (type, var, l, p, u);
  flag= true;
  if (flag) status= CORRUPTED;
  initialize ();
  return flag;
}

void
bridge_highlight_rep::notify_change () {
  status= CORRUPTED;
  body->notify_change ();
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_highlight_rep::my_exec_until (path p) {
  if (p->item == 0) body->exec_until (p->next);
  else env->exec_until (st, p);
}

bool
bridge_highlight_rep::my_typeset_will_be_complete () {
  if (status != CORRUPTED) return false;
  return body->my_typeset_will_be_complete ();
}

void
bridge_highlight_rep::my_typeset (int desired_status) {
  detach_highlight (ht);
  ht= env->expand (st[0]);
  packrat_highlight (env->get_string (PROG_LANGUAGE), "Main", ht);
  ttt->insert_marker (st, ip);
  body->typeset (desired_status);
}
