
/******************************************************************************
* MODULE     : bridge_executable.cpp
* DESCRIPTION: Bridge between logical and physical long macro expansions
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "bridge.hpp"

/******************************************************************************
* The bridge_executable_rep class
******************************************************************************/

class bridge_executable_rep: public bridge_rep {
protected:
  bridge body;

public:
  bridge_executable_rep (typesetter ttt, tree st, path ip);
  void initialize (tree body_t);

  void notify_assign (path p, tree u);
  bool notify_macro  (int type, string var, int l, path p, tree u);
  void notify_change ();

  void my_typeset (int desired_status);
};

bridge_executable_rep::bridge_executable_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip) {}

void
bridge_executable_rep::initialize (tree body_t) {
  if (nil (body)) body= make_bridge (ttt, body_t, decorate_right (ip));
  else replace_bridge (body, body_t, decorate_right (ip));
}

bridge
bridge_executable (typesetter ttt, tree st, path ip) {
  return new bridge_executable_rep (ttt, st, ip);
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_executable_rep::notify_assign (path p, tree u) {
  // cout << "Assign " << p << ", " << u << " in " << st << "\n";
  status= CORRUPTED;
  st= substitute (st, p, u);
}

bool
bridge_executable_rep::notify_macro (int tp, string v, int l, path p, tree u) {
  (void) tp; (void) p; (void) u;
  bool flag= env->depends (st, v, l);
  if (flag) status= CORRUPTED;
  return flag;
}

void
bridge_executable_rep::notify_change () {
  status= CORRUPTED;
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_executable_rep::my_typeset (int desired_status) {
  if (env->preamble) {
    bridge_rep::my_typeset (desired_status);
    return;
  }

  tree r= env->exec (st);
  initialize (r);
  ttt->insert_marker (st, ip);
  body->typeset (desired_status);
}
