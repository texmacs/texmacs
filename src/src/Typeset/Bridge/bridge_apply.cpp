
/******************************************************************************
* MODULE     : bridge_apply.cpp
* DESCRIPTION: Bridge between logical and physical long macro expansions
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "bridge.hpp"

class bridge_apply_rep: public bridge_rep {
protected:
  bridge body;

public:
  bridge_apply_rep (typesetter ttt, tree st, path ip);
  void initialize (tree body_t);

  void notify_assign (path p, tree u);
  bool notify_macro  (int type, string var, int l, path p, tree u);
  void notify_change ();

  void my_typeset (int desired_status);
};

bridge_apply_rep::bridge_apply_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip) {}

void
bridge_apply_rep::initialize (tree body_t) {
  if (nil (body)) body= make_bridge (ttt, body_t, decorate_right (ip));
  else replace_bridge (body, body_t, decorate_right (ip));
}

bridge
bridge_apply (typesetter ttt, tree st, path ip) {
  return new bridge_apply_rep (ttt, st, ip);
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_apply_rep::notify_assign (path p, tree u) {
  // cout << "Assign " << p << ", " << u << " in " << st << "\n";
  status= CORRUPTED;
  st= substitute (st, p, u);
}

bool
bridge_apply_rep::notify_macro (int tp, string v, int l, path p, tree u) {
  (void) tp; (void) p; (void) u;
  bool flag= env->depends (st, v, l);
  if (flag) status= CORRUPTED;
  return flag;
}

void
bridge_apply_rep::notify_change () {
  status= CORRUPTED;
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_apply_rep::my_typeset (int desired_status) {
  if (env->preamble) {
    bridge_rep::my_typeset (desired_status);
    return;
  }

  tree f= st[0];
  if (is_compound (f)) f= env->exec (f);
  if (is_atomic (f)) {
    string var= f->label;
    if (env->provides (var)) f= env->read (var);
    else f= tree (ERROR, "apply " * var);
  }

  if (is_applicable (f)) {
    int i, k=N(f)-1, n=N(st)-1; // is k=0 allowed ?
    STACK_NEW_ARRAY(vars,string,k);
    STACK_NEW_ARRAY(oldv,tree,k);
    STACK_NEW_ARRAY(newv,tree,k);
    for (i=0; i<k; i++)
      if (is_atomic (f[i])) {
	vars[i]= f[i]->label;
	oldv[i]= env->read (vars[i]);
	newv[i]= (i<n? env->exec (st[i+1]): tree (""));
	if ((i==k-1) && (n>=k)) {
	  int nv= N(vars[i]);
	  if ((nv>0) && (vars[i][nv-1]=='*')) {
	    vars[i]= vars[i] (0, nv-1);
	    newv[i]= env->exec_extra_list (st, i+1);
	  }
	  else if (n>k) newv[i]= env->exec_extra_tuple (st, i+1);
	}
	env->monitored_write (vars[i], newv[i]);
      }
      /*
      else {
	STACK_DELETE_ARRAY(vars);
	STACK_DELETE_ARRAY(oldv);
	STACK_DELETE_ARRAY(newv);
	return;
      }
      */
    initialize (f[k]);
    ttt->insert_marker (st, ip);
    body->typeset (desired_status);
    for (i=k-1; i>=0; i--) env->write (vars[i], oldv[i]);
    STACK_DELETE_ARRAY(vars);
    STACK_DELETE_ARRAY(oldv);
    STACK_DELETE_ARRAY(newv);
  }
  else {
    initialize (f);
    ttt->insert_marker (st, ip);
    body->typeset (desired_status);
  }
}
