
/******************************************************************************
* MODULE     : bridge_extension.cpp
* DESCRIPTION: Bridge between logical and physical long macro expansions
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "bridge.hpp"

tree insert (tree, path, tree);
tree remove (tree, path, int);

class bridge_extension_rep: public bridge_rep {
protected:
  bool   valid;
  bridge body;
  tree   fun;

public:
  bridge_extension_rep (typesetter ttt, tree st, path ip);
  void initialize (tree body_t, tree fun);

  void notify_assign (path p, tree u);
  void notify_insert (path p, tree u);
  void notify_remove (path p, int nr);
  bool notify_macro  (int type, string var, int level, path p, tree u);
  void notify_change ();

  bool my_typeset_will_be_complete ();
  void my_typeset (int desired_status);
};

bridge_extension_rep::bridge_extension_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip)
{
  valid= false;
}

void
bridge_extension_rep::initialize (tree body_t, tree fun2) {
  if ((!valid) || (body->st != body_t) || (fun != fun2)) {
    valid= true;
    if (nil (body)) body= make_bridge (ttt, body_t, decorate_right (ip));
    else replace_bridge (body, body_t, decorate_right (ip));
    fun= fun2;
  }
}

bridge
bridge_extension (typesetter ttt, tree st, path ip) {
  return new bridge_extension_rep (ttt, st, ip);
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_extension_rep::notify_assign (path p, tree u) {
  // cout << "Assign " << p << ", " << u << " in " << st << "\n";
  if (nil (p) && (L(u) < START_EXTENSIONS))
    fatal_error ("Nil path", "bridge_extension_rep::notify_assign");
  if (nil (p) || (p->item == 0) || nil (body)) {
    st= substitute (st, p, u);
    valid= false;
  }
  else {
    // bool mp_flag= is_multi_paragraph (st);
    if (is_applicable (fun) && (p->item < N(fun)))
      notify_macro (MACRO_ASSIGN, fun[p->item]->label, -1, p->next, u);
    st= substitute (st, p, u);
    // if (mp_flag != is_multi_paragraph (st)) valid= false;
  }
  status= CORRUPTED;
}

void
bridge_extension_rep::notify_insert (path p, tree u) {
  // cout << "Insert " << p << ", " << u << " in " << st << "\n";
  if (nil (p)) fatal_error ("Nil path", "bridge_extension_rep::notify_insert");
  if (atom (p) || nil (body)) bridge_rep::notify_insert (p, u);
  else {
    // bool mp_flag= is_multi_paragraph (st);
    if (is_applicable (fun) && (p->item < N(fun)))
      notify_macro (MACRO_INSERT, fun[p->item]->label, -1, p->next, u);
    st= insert (st, p, u);
    // if (mp_flag != is_multi_paragraph (st)) valid= false;
  }
  status= CORRUPTED;
}

void
bridge_extension_rep::notify_remove (path p, int nr) {
  // cout << "Remove " << p << ", " << nr << " in " << st << "\n";
  if (nil (p)) fatal_error ("Nil path", "bridge_extension_rep::notify_remove");
  if (atom (p) || nil (body)) bridge_rep::notify_remove (p, nr);
  else {
    // bool mp_flag= is_multi_paragraph (st);
    if (is_applicable (fun) && (p->item < N(fun)))
      notify_macro (MACRO_REMOVE, fun[p->item]->label, -1, p->next,
		    tree (as_string (nr)));
    st= remove (st, p, nr);
    // if (mp_flag != is_multi_paragraph (st)) valid= false;
  }
  status= CORRUPTED;
}

bool
bridge_extension_rep::notify_macro (
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
    for (i=0; i<n; i++)
      if (is_atomic (fun[i])) {
	string var= fun[i]->label;
	env->macro_arg->item (var)= i<m? st[i]: tree("");
	env->macro_src->item (var)= i<m? descend (ip,i): decorate_right(ip);
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
bridge_extension_rep::notify_change () {
  status= CORRUPTED;
  if (!nil (body)) body->notify_change ();
}

/******************************************************************************
* Typesetting
******************************************************************************/

bool
bridge_extension_rep::my_typeset_will_be_complete () {
  return !valid;
}

void
bridge_extension_rep::my_typeset (int desired_status) {
  string var= as_string (L(st));
  tree f;
  if (env->provides (var)) f= env->read (var);
  else f= tree (ERROR, "extension " * var);

  if (is_applicable (f)) {
    int i, n=N(f)-1, m=N(st);
    env->macro_arg= list<hashmap<string,tree> > (
      hashmap<string,tree> (UNINIT), env->macro_arg);
    env->macro_src= list<hashmap<string,path> > (
      hashmap<string,path> (path (DECORATION)), env->macro_src);
    for (i=0; i<n; i++)
      if (is_atomic (f[i])) {
	string var= f[i]->label;
	env->macro_arg->item (var)= i<m? st[i]: tree("");
	env->macro_src->item (var)= i<m? descend (ip,i): decorate_right(ip);
      }
    initialize (f[n], f);
    /*IF_NON_CHILD_ENFORCING(st)*/ ttt->insert_marker (st, ip);
    body->typeset (desired_status);
    env->macro_arg= env->macro_arg->next;
    env->macro_src= env->macro_src->next;
  }
  else {
    initialize (f, f);
    /*IF_NON_CHILD_ENFORCING(st)*/ ttt->insert_marker (st, ip);
    body->typeset (desired_status);
  }
}
