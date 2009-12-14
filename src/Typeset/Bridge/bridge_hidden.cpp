
/******************************************************************************
* MODULE     : bridge_hidden.cpp
* DESCRIPTION: Bridge between logical and physical local enviroment changes
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bridge.hpp"

class bridge_hidden_rep: public bridge_rep {
protected:
  bridge body;

public:
  bridge_hidden_rep (typesetter ttt, tree st, path ip);

  void notify_assign (path p, tree u);
  bool notify_macro  (int type, string var, int level, path p, tree u);
  void notify_change ();

  void my_typeset (int desired_status);
};

bridge_hidden_rep::bridge_hidden_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip) {}

bridge
bridge_hidden (typesetter ttt, tree st, path ip) {
  //cout << "Construct " << st << "\n";
  return tm_new<bridge_hidden_rep> (ttt, st, ip);
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_hidden_rep::notify_assign (path p, tree u) {
  //cout << "Assign " << p << ", " << u << " in " << st << "\n";
  status= CORRUPTED;
  st= substitute (st, p, u);
}

bool
bridge_hidden_rep::notify_macro (int tp, string var, int l, path p, tree u) {
  //cout << "Notify macro in " << st << "\n";
  (void) tp; (void) p; (void) u;
  bool flag= env->depends (st, var, l);
  if (flag) status= CORRUPTED;
  return flag;
}

void
bridge_hidden_rep::notify_change () {
  //cout << "Notify change in " << st << "\n";
  status= CORRUPTED;
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_hidden_rep::my_typeset (int desired_status) {
  stack_border temp_sb;
  array<page_item> temp_l=
    typeset_stack (env, st, ip, ttt->a, ttt->b, temp_sb);
  int i=0, n= N(temp_l);
  for (i=0; i<n; i++)
    if (temp_l[i]->type != PAGE_CONTROL_ITEM) {
      box b= temp_l[i]->b;
      temp_l[i]->type= PAGE_HIDDEN_ITEM;
      temp_l[i]->b   = resize_box (ip, b, b->x1, 0, b->x2, 0);
      temp_l[i]->spc = space (0, 0, 0);
    }
  ttt->l = temp_l;
  ttt->sb= temp_sb; // stack_border ();
  //ttt->insert_stack (temp_l, temp_sb);
}
