
/******************************************************************************
* MODULE     : bridge_argument.cpp
* DESCRIPTION: Bridge between logical and physical long macro expansions
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bridge.hpp"

class bridge_argument_rep: public bridge_rep {
protected:
  bool   valid;
  string name;
  path   prefix;
  bridge body;

public:
  bridge_argument_rep (typesetter ttt, tree st, path ip);
  void initialize ();
  void initialize (string name, path head, tree body_t, path body_ip);

  void notify_assign (path p, tree u);
  bool notify_macro  (int type, string var, int level, path p, tree u);
  void notify_change ();

  bool my_typeset_will_be_complete ();
  void my_typeset (int desired_status);
};

bridge_argument_rep::bridge_argument_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip)
{
  valid= false;
}

void
bridge_argument_rep::initialize (string name2, path pf, tree b_t, path b_ip) {
  // cout << "Initialize arg: " << name2 << ", " << pf << ": " << b_t << "\n";
  if ((!valid) || (name != name2) || (prefix != pf) ||
      (body->st != b_t) || (body->ip != b_ip))
    {
      valid = true;
      name  = name2;
      prefix= pf;
      if (is_nil (body)) body = make_bridge (ttt, b_t, b_ip);
      else replace_bridge (body, b_t, b_ip);
    }
}

bridge
bridge_argument (typesetter ttt, tree st, path ip) {
  return tm_new<bridge_argument_rep> (ttt, st, ip);
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_argument_rep::notify_assign (path p, tree u) {
  // cout << "Assign " << p << ", " << u << " in " << st << "\n";
  status= CORRUPTED;
  st    = substitute (st, p, u);
  valid = false;
}

bool
bridge_argument_rep::notify_macro (int tp, string var, int l, path p, tree u) {
  /*
  cout << "Macro argument " << var << " [action=" << tp
       << ", level=" << l << "] " << p << ", " << u << " in " << st << "\n";
  */

  bool flag;
  if (valid) {
    // cout << "  " << body->st << ", " << body->ip << "\n";
    flag= true;
    if (l==0) {
      int n= N(prefix);
      if ((var == name) && (N(p) >= n) && (head (p, n) == prefix)) {
	p= tail (p, n);
	switch (tp) {
	case MACRO_ASSIGN:
	  if (is_nil (p)) replace_bridge (body, u, body->ip);
	  else body->notify_assign (p, u);
	  break;
	case MACRO_INSERT:
	  body->notify_insert (p, u);
	  break;
	case MACRO_REMOVE:
	  body->notify_remove (p, as_int (u->label));
	  break;
	}
      }
      else flag= false;
    }
    else {
      list<hashmap<string,tree> > old_var= env->macro_arg;
      list<hashmap<string,path> > old_src= env->macro_src;
      if (!is_nil (env->macro_arg)) env->macro_arg= env->macro_arg->next;
      if (!is_nil (env->macro_src)) env->macro_src= env->macro_src->next;
      flag= body->notify_macro (tp, var, l-1, p, u);
      env->macro_arg= old_var;
      env->macro_src= old_src;
    }
  }
  else flag= env->depends (st, var, l);
  if (flag) status= CORRUPTED;
  return flag;
}

void
bridge_argument_rep::notify_change () {
  status= CORRUPTED;
  body->notify_change ();
}

/******************************************************************************
* Typesetting
******************************************************************************/

bool
bridge_argument_rep::my_typeset_will_be_complete () {
  return !valid;
}

void
bridge_argument_rep::my_typeset (int desired_status) {
  string name;
  tree   value;
  path   valip= decorate_right (ip);

  tree r= st[0];
  if (is_compound (r)) value= tree (_ERROR, "arg");
  else {
    name = r->label;
    if ((!is_nil (env->macro_arg)) && env->macro_arg->item->contains (r->label)) {
      value= env->macro_arg->item [name];
      if (!is_func (value, BACKUP)) {
	path new_valip= env->macro_src->item [name];
	if (is_accessible (new_valip)) valip= new_valip;
      }
    }
    else value= compound ("src-unknown", name);
  }

  path prefix;
  if (N(st) > 1) {
    int i, n= N(st);
    for (i=1; i<n; i++) {
      tree r= env->exec (st[i]);
      if (!is_int (r)) {
        prefix= path ();
        value= tree (_ERROR, "arg " * name);
        valip= decorate_right (ip);
        break;
      }
      int nr= as_int (r);
      if ((!is_compound (value)) || (nr<0) || (nr>=N(value))) {
        prefix= path ();
        value= tree (_ERROR, "arg " * name);
        valip= decorate_right (ip);
        break;
      }
      value = value[nr];
      valip = descend (valip, nr);
      prefix= prefix * nr;
    }
  }
  initialize (name, prefix, attach_here (value, valip));

  ttt->insert_marker (body->st, ip);
  list<hashmap<string,tree> > old_var= env->macro_arg;
  list<hashmap<string,path> > old_src= env->macro_src;
  if (!is_nil (env->macro_arg)) env->macro_arg= env->macro_arg->next;
  if (!is_nil (env->macro_src)) env->macro_src= env->macro_src->next;
  body->typeset (desired_status);
  env->macro_arg= old_var;
  env->macro_src= old_src;
}
