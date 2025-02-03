
/******************************************************************************
* MODULE     : bridge_mark.cpp
* DESCRIPTION: Bridge between logical and physical paragraph markings
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bridge.hpp"

class bridge_mark_rep: public bridge_rep {
protected:
  bridge body;

public:
  bridge_mark_rep (typesetter ttt, tree st, path ip);
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

bridge_mark_rep::bridge_mark_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip)
{
  initialize ();
}

void
bridge_mark_rep::initialize () {
  if (is_nil (body)) body= make_bridge (ttt, st[1], descend (ip, 1));
  else replace_bridge (body, st[1], descend (ip, 1));
}

bridge
bridge_mark (typesetter ttt, tree st, path ip) {
  return tm_new<bridge_mark_rep> (ttt, st, ip);
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_mark_rep::notify_assign (path p, tree u) {
  // cout << "Assign " << p << ", " << u << " in " << st << "\n";
  ASSERT (!is_nil (p) || is_func (u, MARK), "nil path");
  if (is_nil (p) || (p->item != 1)) {
    st= substitute (st, p, u);
    initialize ();
  }
  else {
    // bool mp_flag= is_multi_paragraph (st);
    if (is_atom (p)) {
      body= make_bridge (ttt, u, descend (ip, 1));
      st= substitute (st, 1, body->st);
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
bridge_mark_rep::notify_insert (path p, tree u) {
  // cout << "Insert " << p << ", " << u << " in " << st << "\n";
  ASSERT (!is_nil (p), "nil path");
  if (is_atom (p) || (p->item != 1)) bridge_rep::notify_insert (p, u);
  else {
    // bool mp_flag= is_multi_paragraph (st);
    body->notify_insert (p->next, u);
    st= substitute (st, 1, body->st);
    // if (mp_flag != is_multi_paragraph (st)) initialize ();
  }
  status= CORRUPTED;
}

void
bridge_mark_rep::notify_remove (path p, int nr) {
  // cout << "Remove " << p << ", " << nr << " in " << st << "\n";
  ASSERT (!is_nil (p), "nil path");
  if (is_atom (p) || (p->item != 1)) bridge_rep::notify_remove (p, nr);
  else {
    // bool mp_flag= is_multi_paragraph (st);
    body->notify_remove (p->next, nr);
    st= substitute (st, 1, body->st);
    // if (mp_flag != is_multi_paragraph (st)) initialize ();
  }
  status= CORRUPTED;
}

bool
bridge_mark_rep::notify_macro (int tp, string var, int l, path p, tree u) {
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
bridge_mark_rep::notify_change () {
  status= CORRUPTED;
  body->notify_change ();
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_mark_rep::my_exec_until (path p) {
  if (p->item == 1) body->exec_until (p->next);
  else env->exec_until (st, p);
}

bool
bridge_mark_rep::my_typeset_will_be_complete () {
  if (status != CORRUPTED) return false;
  return body->my_typeset_will_be_complete ();
}

void
bridge_mark_rep::my_typeset (int desired_status) {
  // cout << "Argument " << t << ", " << ip << "\n";
  if (is_func (st[0], ARG) &&
      is_atomic (st[0][0]) &&
      (!is_nil (env->macro_arg)) &&
      env->macro_arg->item->contains (st[0][0]->label))
    {
      string name = st[0][0]->label;
      tree   value= env->macro_arg->item [name];
      path   valip= decorate_right (ip);
      if (!is_func (value, BACKUP)) {
	path new_valip= env->macro_src->item [name];
	if (is_accessible (new_valip)) valip= new_valip;
      }

      if (N(st[0]) > 1) {
	int i, n= N(st[0]);
	for (i=1; i<n; i++) {
	  tree r= env->exec (st[0][i]);
	  if (!is_int (r)) break;
	  int nr= as_int (r);
	  if ((!is_compound (value)) || (nr<0) || (nr>=N(value))) break;
	  value= value[nr];
	  valip= descend (valip, nr);
	}
      }
      if (is_func (st, VAR_MARK)) {
        if (!is_nil (valip) && valip->item >= 0)
          ttt->insert_marker (st, valip->next);
      }
      else if (is_compound (value))
	ttt->insert_marker (st, valip);
    }

  body->typeset (desired_status);
}
