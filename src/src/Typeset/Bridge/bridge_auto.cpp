
/******************************************************************************
* MODULE     : bridge_auto.cpp
* DESCRIPTION: Bridge for automatically inserted macro expansions (inactive)
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

class bridge_auto_rep: public bridge_rep {
protected:
  tree   f;
  bool   border;
  bool   valid;
  bridge body;

public:
  bridge_auto_rep (typesetter ttt, tree st, path ip, tree f, bool border);
  void initialize ();

  void notify_assign (path p, tree u);
  void notify_insert (path p, tree u);
  void notify_remove (path p, int nr);
  bool notify_macro  (int type, string var, int level, path p, tree u);
  void notify_change ();

  void my_exec_until (path p);
  void exec_until (path p);
  bool my_typeset_will_be_complete ();
  void my_typeset (int desired_status);
};

bridge_auto_rep::bridge_auto_rep (
  typesetter ttt, tree st, path ip, tree f2, bool border2):
    bridge_rep (ttt, st, ip)
{
  f     = f2;
  border= border2;
  valid = false;
}

void
bridge_auto_rep::initialize () {
  if ((!valid) || (body->st != f[1])) {
    valid= true;
    if (nil (body)) body= make_bridge (ttt, f[1], decorate_right (ip));
    else replace_bridge (body, f[1], decorate_right (ip));
  }
}

bridge
bridge_auto (typesetter ttt, tree st, path ip, tree f, bool border) {
  return new bridge_auto_rep (ttt, st, ip, f, border);
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_auto_rep::notify_assign (path p, tree u) {
  //cout << "Assign " << p << ", " << u << " in " << st << "\n";
  if (nil (body)) {
    st= substitute (st, p, u);
    valid= false;
  }
  else {
    // bool mp_flag= is_multi_paragraph (st);
    notify_macro (MACRO_ASSIGN, f[0]->label, -1, p->next, u);
    st= substitute (st, p, u);
    // if (mp_flag != is_multi_paragraph (st)) valid= false;
  }
  status= CORRUPTED;
}

void
bridge_auto_rep::notify_insert (path p, tree u) {
  //cout << "Insert " << p << ", " << u << " in " << st << "\n";
  if (nil (body)) bridge_rep::notify_insert (p, u);
  else {
    // bool mp_flag= is_multi_paragraph (st);
    notify_macro (MACRO_INSERT, f[0]->label, -1, p->next, u);
    st= insert (st, p, u);
    // if (mp_flag != is_multi_paragraph (st)) valid= false;
  }
  status= CORRUPTED;
}

void
bridge_auto_rep::notify_remove (path p, int nr) {
  //cout << "Remove " << p << ", " << nr << " in " << st << "\n";
  if (nil (body)) bridge_rep::notify_remove (p, nr);
  else {
    // bool mp_flag= is_multi_paragraph (st);
    notify_macro (MACRO_REMOVE, f[0]->label, -1, p->next,
		  tree (as_string (nr)));
    st= remove (st, p, nr);
    // if (mp_flag != is_multi_paragraph (st)) valid= false;
  }
  status= CORRUPTED;
}

bool
bridge_auto_rep::notify_macro (
  int type, string var, int l, path p, tree u)
{
  /*
  cout << "Macro argument " << var << " [action=" << type
       << ", level=" << l << "] " << p << ", " << u << " in " << st << "\n";
  */

  bool flag;
  if (valid) {
    env->macro_arg= list<hashmap<string,tree> > (
      hashmap<string,tree> (UNINIT), env->macro_arg);
    env->macro_src= list<hashmap<string,path> > (
      hashmap<string,path> (path (DECORATION)), env->macro_src);
    string var= f[0]->label;
    env->macro_arg->item (var)= st;
    env->macro_src->item (var)= ip;
    flag= body->notify_macro (type, var, l+1, p, u);
    env->macro_arg= env->macro_arg->next;
    env->macro_src= env->macro_src->next;
  }
  else flag= env->depends (st, var, l);
  if (flag) status= CORRUPTED;
  return flag;
}

void
bridge_auto_rep::notify_change () {
  status= CORRUPTED;
  if (!nil (body)) body->notify_change ();
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_auto_rep::my_exec_until (path p) {
  env->macro_arg= list<hashmap<string,tree> >
    (hashmap<string,tree> (UNINIT), env->macro_arg);
  env->macro_src= list<hashmap<string,path> >
    (hashmap<string,path> (path (DECORATION)), env->macro_src);
  string var= f[0]->label;
  env->macro_arg->item (var)= st;
  (void) env->exec_until (f[1], p, var, 0);
  env->macro_arg= env->macro_arg->next;
  env->macro_src= env->macro_src->next;
}

void
bridge_auto_rep::exec_until (path p) {
  if ((status & VALID_MASK) != PROCESSED) env->exec_until (st, p);
  else my_exec_until (p);
}

bool
bridge_auto_rep::my_typeset_will_be_complete () {
  return !valid;
}

void
bridge_auto_rep::my_typeset (int desired_status) {
  env->macro_arg= list<hashmap<string,tree> > (
    hashmap<string,tree> (UNINIT), env->macro_arg);
  env->macro_src= list<hashmap<string,path> > (
    hashmap<string,path> (path (DECORATION)), env->macro_src);
  string var= f[0]->label;
  env->macro_arg->item (var)= st;
  env->macro_src->item (var)= ip;
  tree oldv= env->read (PREAMBLE);
  env->write_update (PREAMBLE, "false");
  initialize ();
  if (border) ttt->insert_marker (st, ip);
  body->typeset (desired_status);
  env->write_update (PREAMBLE, oldv);
  env->macro_arg= env->macro_arg->next;
  env->macro_src= env->macro_src->next;
}
