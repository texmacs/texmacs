
/******************************************************************************
* MODULE     : bridge_compound.cpp
* DESCRIPTION: Bridge between logical and physical long macro expansions
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bridge.hpp"
#include "drd_std.hpp"

tree insert_at (tree, path, tree);
tree remove_at (tree, path, int);

class bridge_compound_rep: public bridge_rep {
protected:
  bool   valid;
  bridge body;
  int    delta;
  tree   fun;

public:
  bridge_compound_rep (typesetter ttt, tree st, path ip);
  void initialize (tree body_t, int delta, tree fun);

  void notify_assign (path p, tree u);
  void notify_insert (path p, tree u);
  void notify_remove (path p, int nr);
  bool notify_macro  (int type, string var, int level, path p, tree u);
  void notify_change ();

  bool my_typeset_will_be_complete ();
  void my_typeset (int desired_status);
};

bridge_compound_rep::bridge_compound_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip)
{
  valid= false;
}

void
bridge_compound_rep::initialize (tree body_t, int delta2, tree fun2) {
  if ((!valid) || (body->st != body_t) || (delta != delta2) || (fun != fun2)) {
    valid= true;
    if (is_nil (body)) body= make_bridge (ttt, attach_right (body_t, ip));
    else replace_bridge (body, attach_right (body_t, ip));
    delta= delta2;
    fun  = fun2;
  }
}

bridge
bridge_compound (typesetter ttt, tree st, path ip) {
  return tm_new<bridge_compound_rep> (ttt, st, ip);
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_compound_rep::notify_assign (path p, tree u) {
  // cout << "Assign " << p << ", " << u << " in " << st << "\n";
  ASSERT (!is_nil (p) || L(u) >= START_EXTENSIONS, "nil path");
  if (is_nil (p) || (p->item == 0) || is_nil (body)) {
    st= substitute (st, p, u);
    valid= false;
  }
  else {
    // bool mp_flag= is_multi_paragraph (st);
    if (is_func (fun, XMACRO, 2))
      notify_macro (MACRO_ASSIGN, fun[0]->label, -1, p, u);
    else if (is_applicable (fun) && (p->item < N(fun)))
      notify_macro (MACRO_ASSIGN, fun[p->item-delta]->label, -1, p->next, u);
    st= substitute (st, p, u);
    // if (mp_flag != is_multi_paragraph (st)) valid= false;
  }
  status= CORRUPTED;
}

void
bridge_compound_rep::notify_insert (path p, tree u) {
  // cout << "Insert " << p << ", " << u << " in " << st << "\n";
  ASSERT (!is_nil (p), "nil path");
  if (is_atom (p) || is_nil (body)) bridge_rep::notify_insert (p, u);
  else {
    // bool mp_flag= is_multi_paragraph (st);
    if (is_func (fun, XMACRO, 2))
      notify_macro (MACRO_INSERT, fun[0]->label, -1, p, u);
    else if (is_applicable (fun) && (p->item < N(fun)))
      notify_macro (MACRO_INSERT, fun[p->item-delta]->label, -1, p->next, u);
    st= insert_at (st, p, u);
    // if (mp_flag != is_multi_paragraph (st)) valid= false;
  }
  status= CORRUPTED;
}

void
bridge_compound_rep::notify_remove (path p, int nr) {
  // cout << "Remove " << p << ", " << nr << " in " << st << "\n";
  ASSERT (!is_nil (p), "nil path");
  if (is_atom (p) || is_nil (body)) bridge_rep::notify_remove (p, nr);
  else {
    // bool mp_flag= is_multi_paragraph (st);
    if (is_func (fun, XMACRO, 2))
      notify_macro (MACRO_REMOVE, fun[0]->label, -1, p, tree (as_string (nr)));
    else if (is_applicable (fun) && (p->item < N(fun)))
      notify_macro (MACRO_REMOVE, fun[p->item-delta]->label, -1, p->next,
		    tree (as_string (nr)));
    st= remove_at (st, p, nr);
    // if (mp_flag != is_multi_paragraph (st)) valid= false;
  }
  status= CORRUPTED;
}

bool
bridge_compound_rep::notify_macro (
  int type, string var, int l, path p, tree u)
{
  /*
  cout << "Macro argument " << var << " [action=" << type
       << ", level=" << l << "] " << p << ", " << u << " in " << st << "\n";
  */

  bool flag;
  if (valid) {
    int i, n=N(fun)-1, m=N(st);
    env->macro_arg= list<hashmap<string,tree> > (
      hashmap<string,tree> (UNINIT), env->macro_arg);
    env->macro_src= list<hashmap<string,path> > (
      hashmap<string,path> (path (DECORATION)), env->macro_src);
    if (L(fun) == XMACRO) {
      if (is_atomic (fun[0])) {
	string var= fun[0]->label;
	env->macro_arg->item (var)= st;
	env->macro_src->item (var)= ip;
      }
    }
    else for (i=0; i<n; i++)
      if (is_atomic (fun[i])) {
	string var= fun[i]->label;
	env->macro_arg->item (var)=
	  i<m? st[i+delta]: attach_dip (tree (UNINIT), decorate_right (ip));
	env->macro_src->item (var)=
	  i<m? descend (ip,i+delta): decorate_right(ip);
      }
    flag= body->notify_macro (type, var, l+1, p, u);
    env->macro_arg= env->macro_arg->next;
    env->macro_src= env->macro_src->next;
  }
  else flag= env->depends (st, var, l);
  if (flag) status= CORRUPTED;
  return flag;
}

void
bridge_compound_rep::notify_change () {
  status= CORRUPTED;
  if (!is_nil (body)) body->notify_change ();
}

/******************************************************************************
* Typesetting
******************************************************************************/

bool
bridge_compound_rep::my_typeset_will_be_complete () {
  return !valid;
}

void
bridge_compound_rep::my_typeset (int desired_status) {
  int d; tree f;
  if (L(st) == COMPOUND) {
    d= 1;
    f= st[0];
    if (is_compound (f)) f= env->exec (f);
    if (is_atomic (f)) {
      string var= f->label;
      if (env->provides (var)) f= env->read (var);
      else f= tree (ERROR, st);
    }
  }
  else {
    string var= as_string (L(st));
    if (env->provides (var)) f= env->read (var);
    else f= tree (ERROR, st);
    d= 0;
  }

  if (is_applicable (f)) {
    int i, n=N(f)-1, m=N(st)-d;
    env->macro_arg= list<hashmap<string,tree> > (
      hashmap<string,tree> (UNINIT), env->macro_arg);
    env->macro_src= list<hashmap<string,path> > (
      hashmap<string,path> (path (DECORATION)), env->macro_src);
    if (L(f) == XMACRO) {
      if (is_atomic (f[0])) {
	string var= f[0]->label;
	env->macro_arg->item (var)= st;
	env->macro_src->item (var)= ip;
      }
    }
    else for (i=0; i<n; i++)
      if (is_atomic (f[i])) {
	string var= f[i]->label;
	env->macro_arg->item (var)=
	  i<m? st[i+d]: attach_dip (tree (UNINIT), decorate_right (ip));
	env->macro_src->item (var)=
	  i<m? descend (ip,i+d): decorate_right(ip);
      }
    initialize (f[n], d, f);
    // /*IF_NON_CHILD_ENFORCING(st)*/ ttt->insert_marker (st, ip);
    if (!the_drd->is_child_enforcing (st))
      ttt->insert_marker (st, ip);
    body->typeset (desired_status);
    env->macro_arg= env->macro_arg->next;
    env->macro_src= env->macro_src->next;
  }
  else {
    initialize (f, d, f);
    ///*IF_NON_CHILD_ENFORCING(st)*/ ttt->insert_marker (st, ip);
    if (!the_drd->is_child_enforcing (st))
      ttt->insert_marker (st, ip);
    body->typeset (desired_status);
  }
}
