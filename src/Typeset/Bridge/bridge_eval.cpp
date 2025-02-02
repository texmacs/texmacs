
/******************************************************************************
* MODULE     : bridge_eval.cpp
* DESCRIPTION: Bridge between logical and physical long macro expansions
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bridge.hpp"

/******************************************************************************
* The bridge_eval_rep class
******************************************************************************/

class bridge_eval_rep: public bridge_rep {
protected:
  tree   bt;
  bridge body;

public:
  bridge_eval_rep (typesetter ttt, tree st, path ip);
  void initialize (tree body_t);

  void notify_assign (path p, tree u);
  bool notify_macro  (int type, string var, int l, path p, tree u);
  void notify_change ();

  void my_typeset (int desired_status);
};

bridge_eval_rep::bridge_eval_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip) {}

void
bridge_eval_rep::initialize (tree body_t) {
  if (is_nil (body)) body= make_bridge (ttt, attach_right (body_t, ip));
  else replace_bridge (body, path (), bt, attach_right (body_t, ip));
  bt= copy (body_t);
}

bridge
bridge_eval (typesetter ttt, tree st, path ip) {
  return tm_new<bridge_eval_rep> (ttt, st, ip);
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_eval_rep::notify_assign (path p, tree u) {
  //cout << "Assign " << p << ", " << u << " in " << st << "\n";
  status= CORRUPTED;
  st= substitute (st, p, u);
}

bool
bridge_eval_rep::notify_macro (int tp, string v, int l, path p, tree u) {
  //cout << "Macro argument " << v << " [action=" << tp
  //<< ", level=" << l << "] " << p << ", " << u << " in " << st << "\n";
  //cout << "  Body= " << bt << "\n";
  (void) tp; (void) p; (void) u;
  bool flag= env->depends (st, v, l);
  //cout << "  Flag= " << flag << "\n";
  if (flag) {
    status= CORRUPTED;
    body->notify_macro (tp, v, l, p, u);
  }
  return flag;
}

void
bridge_eval_rep::notify_change () {
  status= CORRUPTED;
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_eval_rep::my_typeset (int desired_status) {
  if (is_func (st, EVAL, 1))
    initialize (env->exec (st[0]));
  else if (is_func (st, QUASI, 1))
    initialize (env->exec (tree (QUASIQUOTE, st[0])));
  else if (is_func (st, ANIM_STATIC) || is_func (st, ANIM_DYNAMIC))
    initialize (env->exec (st));
  else if (is_func (st, MAP_ARGS))
    initialize (env->rewrite (st));
  else initialize (tree (TMERROR, "bad eval bridge"));
  ttt->insert_marker (st, ip);
  body->typeset (desired_status);
}
