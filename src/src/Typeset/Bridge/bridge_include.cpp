
/******************************************************************************
* MODULE     : bridge_include.cpp
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
* The bridge_include_rep class
******************************************************************************/

class bridge_include_rep: public bridge_rep {
protected:
  bridge body;

public:
  bridge_include_rep (typesetter ttt, tree st, path ip);
  void initialize (tree body_t);

  void notify_assign (path p, tree u);
  bool notify_macro  (int type, string var, int l, path p, tree u);
  void notify_change ();

  void my_typeset (int desired_status);
};

bridge_include_rep::bridge_include_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip) {}

void
bridge_include_rep::initialize (tree body_t) {
  if (nil (body)) body= make_bridge (ttt, body_t, decorate_right (ip));
  else replace_bridge (body, body_t, decorate_right (ip));
}

bridge
bridge_include (typesetter ttt, tree st, path ip) {
  return new bridge_include_rep (ttt, st, ip);
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_include_rep::notify_assign (path p, tree u) {
  // cout << "Assign " << p << ", " << u << " in " << st << "\n";
  status= CORRUPTED;
  st= substitute (st, p, u);
}

bool
bridge_include_rep::notify_macro (int tp, string v, int l, path p, tree u) {
  (void) tp; (void) p; (void) u;
  bool flag= env->depends (st, v, l);
  if (flag) status= CORRUPTED;
  return flag;
}

void
bridge_include_rep::notify_change () {
  status= CORRUPTED;
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_include_rep::my_typeset (int desired_status) {
  if (env->preamble) {
    bridge_rep::my_typeset (desired_status);
    return;
  }

  url file_name= as_string (st[0]);
  tree incl= load_inclusion (relative (env->base_file_name, file_name));
  initialize (incl);
  ttt->insert_marker (st, ip);
  body->typeset (desired_status);
}
