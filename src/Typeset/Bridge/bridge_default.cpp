
/******************************************************************************
* MODULE     : bridge_default.cpp
* DESCRIPTION: Bridge between logical and physically typesetted fragments
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bridge.hpp"

class bridge_default_rep: public bridge_rep {
public:
  bridge_default_rep (typesetter ttt, tree st, path ip);

  void notify_assign (path p, tree u);
  bool notify_macro  (int type, string var, int l, path p, tree u);
  void notify_change ();
};

bridge_default_rep::bridge_default_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip) {}

bridge
bridge_default (typesetter ttt, tree st, path ip) {
  return tm_new<bridge_default_rep> (ttt, st, ip);
}

/******************************************************************************
* Subroutines for event notification
******************************************************************************/

tree
substitute (tree t, path p, tree u) {
  if (is_nil (p)) return u;
  int i, n= N(t);
  tree t2 (t, n);
  for (i=0; i<n; i++) {
    if (i == p->item) t2[i]= substitute (t[i], p->next, u);
    else t2[i]= t[i];
  }
  return t2;
}

tree
insert_at (tree t, path p, tree u) {
  if (is_atom (p)) {
    if (is_atomic (t)) {
      ASSERT (is_atomic (u), "two atoms expected");
      return t->label (0, p->item) *u->label* t->label (p->item, N(t->label));
    }
    else return (t (0, p->item) * u) * t (p->item, N(t));
  }
  else return substitute (t, p->item, insert_at (t[p->item], p->next, u));
}

tree
remove_at (tree t, path p, int nr) {
  if (is_atom (p)) {
    if (is_atomic (t))
      return t->label (0, p->item) * t->label (p->item+nr, N(t->label));
    else return t (0, p->item) * t (p->item+nr, N(t));
  }
  else return substitute (t, p->item, remove_at (t[p->item], p->next, nr));
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_default_rep::notify_assign (path p, tree u) {
  // cout << "Assign " << p << ", " << u << " in " << st << "\n";
  status= CORRUPTED;
  st= substitute (st, p, u);
}

bool
bridge_default_rep::notify_macro (int tp, string var, int l, path p, tree u) {
  (void) tp; (void) p; (void) u;
  bool flag= env->depends (st, var, l);
  if (flag) status= CORRUPTED;
  return flag;
}

void
bridge_default_rep::notify_change () {
  status= CORRUPTED;
}
