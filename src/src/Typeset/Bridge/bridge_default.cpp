
/******************************************************************************
* MODULE     : bridge_default.cpp
* DESCRIPTION: Bridge between logical and physically typesetted fragments
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
  return new bridge_default_rep (ttt, st, ip);
}

/******************************************************************************
* Subroutines for event notification
******************************************************************************/

tree
substitute (tree t, path p, tree u) {
  if (nil (p)) return u;
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
  if (atom (p)) {
    if (is_atomic (t)) {
      if (is_compound (u)) fatal_error ("two atoms expected", "insert");
      return insert_at (t->label, p->item, u->label);
    }
    else return insert_at (t, p->item, u);
  }
  else return substitute (t, p->item, insert_at (t[p->item], p->next, u));
}

tree
remove_at (tree t, path p, int nr) {
  if (atom (p)) {
    if (is_atomic (t)) return remove_at (t->label, p->item, nr);
    else return remove_at (t, p->item, nr);
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
