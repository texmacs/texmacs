
/******************************************************************************
* MODULE     : bridge_rewrite.cpp
* DESCRIPTION: Bridge between logical and physical long macro expansions
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bridge.hpp"

/******************************************************************************
* The bridge_rewrite_rep class
******************************************************************************/

class bridge_rewrite_rep: public bridge_rep {
protected:
  bridge body;

public:
  bridge_rewrite_rep (typesetter ttt, tree st, path ip);
  void initialize (tree body_t);

  void notify_assign (path p, tree u);
  bool notify_macro  (int type, string var, int l, path p, tree u);
  void notify_change ();

  void my_typeset (int desired_status);
};

bridge_rewrite_rep::bridge_rewrite_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip) {}

void
bridge_rewrite_rep::initialize (tree body_t) {
  if (is_nil (body)) body= make_bridge (ttt, attach_right (body_t, ip));
  else replace_bridge (body, attach_right (body_t, ip));
}

bridge
bridge_rewrite (typesetter ttt, tree st, path ip) {
  return new bridge_rewrite_rep (ttt, st, ip);
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_rewrite_rep::notify_assign (path p, tree u) {
  // cout << "Assign " << p << ", " << u << " in " << st << "\n";
  status= CORRUPTED;
  st= substitute (st, p, u);
}

bool
bridge_rewrite_rep::notify_macro (int tp, string v, int l, path p, tree u) {
  (void) tp; (void) p; (void) u;
  bool flag= env->depends (st, v, l);
  if (flag) status= CORRUPTED;
  return flag;
}

void
bridge_rewrite_rep::notify_change () {
  status= CORRUPTED;
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_rewrite_rep::my_typeset (int desired_status) {
  initialize (env->rewrite (st));
  ttt->insert_marker (st, ip);
  if (is_func (st, INCLUDE)) {
    url save_name= env->cur_file_name;
    env->cur_file_name= relative (env->base_file_name, as_string (st[0]));
    env->secure= is_secure (env->cur_file_name);
    body->typeset (desired_status);
    env->cur_file_name= save_name;
    env->secure= is_secure (env->cur_file_name);
  }
  else body->typeset (desired_status);
}
