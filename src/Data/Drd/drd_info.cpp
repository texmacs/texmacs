
/******************************************************************************
* MODULE     : drd_info.cpp
* DESCRIPTION: data relation descriptions
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "vars.hpp"
#include "drd_info.hpp"
#include "drd_std.hpp"
#include "drd_mode.hpp"
#include "iterator.hpp"
#include "analyze.hpp"

/******************************************************************************
* Constructors and basic operations
******************************************************************************/

drd_info_rep::drd_info_rep (string name2):
  name (name2), info (tag_info ()) {}
drd_info_rep::drd_info_rep (string name2, drd_info base):
  name (name2), info (tag_info (), base->info) {}
drd_info::drd_info (string name):
  rep (tm_new<drd_info_rep> (name)) {}
drd_info::drd_info (string name, drd_info base):
  rep (tm_new<drd_info_rep> (name, base)) {}

tree
drd_info_rep::get_locals () {
  tree t (COLLECTION);
  iterator<tree_label> it= iterate (info->item);
  while (it->busy()) {
    tree_label l= it->next();
    tree v= (tree) info->item[l];
    t << tree (ASSOCIATE, as_string (l), v);
  }
  return t;
}

bool
drd_info_rep::set_locals (tree t) {
  if (!is_func (t, COLLECTION))
    return false;
  int i, n= N(t);
  for (i=0; i<n; i++)
    if (is_func (t[i], ASSOCIATE, 2) && is_atomic (t[i][0]))
      info (make_tree_label (t[i][0]->label))= tag_info (t[i][1]);
  return true;
}

bool
drd_info_rep::contains (string l) {
  return existing_tree_label (l) && info->contains (as_tree_label (l));
}

tm_ostream&
operator << (tm_ostream& out, drd_info drd) {
  return out << "drd [" << drd->name << "]";
}

/******************************************************************************
* Arity related methods
******************************************************************************/

void
drd_info_rep::set_arity (tree_label l, int arity, int extra, int am, int cm) {
  if (info[l]->pi.freeze_arity) return;
  if (!info->contains (l)) info(l)= copy (info[l]);
  tag_info& ti= info(l);
  ti->pi.arity_mode= am;
  ti->pi.child_mode= cm;
  if (am != ARITY_VAR_REPEAT) {
    ti->pi.arity_base = arity;
    ti->pi.arity_extra= extra;
  }
  else {
    ti->pi.arity_base = extra;
    ti->pi.arity_extra= arity;
  }
  int n;
  if (arity+extra == 0) n= 0;
  else if (cm == CHILD_UNIFORM) n= 1;
  else if (cm == CHILD_BIFORM) n= 2;
  else n= arity+extra;
  if (N(ti->ci) != n) ti->ci= array<child_info> (n);
}

int
drd_info_rep::get_arity_mode (tree_label l) {
  return info[l]->pi.arity_mode;
}

int
drd_info_rep::get_child_mode (tree_label l) {
  return info[l]->pi.child_mode;
}

int
drd_info_rep::get_arity_base (tree_label l) {
  return info[l]->pi.arity_base;
}

int
drd_info_rep::get_arity_extra (tree_label l) {
  return info[l]->pi.arity_extra;
}

int
drd_info_rep::get_nr_indices (tree_label l) {
  return N(info[l]->ci);
}

void
drd_info_rep::freeze_arity (tree_label l) {
  if (!info->contains (l)) info(l)= copy (info[l]);
  tag_info& ti= info(l);
  ti->pi.freeze_arity= true;
}

int
drd_info_rep::get_old_arity (tree_label l) {
  tag_info ti= info[l];
  if (ti->pi.arity_mode != ARITY_NORMAL) return -1;
  else return ((int) ti->pi.arity_base) + ((int) ti->pi.arity_extra);
}

bool
drd_info_rep::correct_arity (tree_label l, int i) {
  parent_info pi= info[l]->pi;
  switch (pi.arity_mode) {
  case ARITY_NORMAL:
    return i == ((int) pi.arity_base) + ((int) pi.arity_extra);
  case ARITY_OPTIONS:
    return (i >= ((int) pi.arity_base)) &&
           (i <= ((int) pi.arity_base) + ((int) pi.arity_extra));
  case ARITY_REPEAT:
  case ARITY_VAR_REPEAT:
    return (i >= ((int) pi.arity_base)) &&
           (((i-pi.arity_base) % pi.arity_extra) == 0);
  }
  return false; // NOT REACHED
}

bool
drd_info_rep::insert_point (tree_label l, int i, int n) {
  parent_info pi= info[l]->pi;
  switch (pi.arity_mode) {
  case ARITY_NORMAL:
    return false;
  case ARITY_OPTIONS:
    return (i >= ((int) pi.arity_base)) && (i <= n) &&
           (n < ((int) pi.arity_base) + ((int) pi.arity_extra));
  case ARITY_REPEAT:
    return (i >= 0) &&
           ((i < ((int) pi.arity_base)) ||
	    ((i - pi.arity_base) % pi.arity_extra) == 0);
  case ARITY_VAR_REPEAT:
    return (i >= 0) &&
           ((i > (n - ((int) pi.arity_base))) ||
	    (i % pi.arity_extra == 0));
  }
  return false; // NOT REACHED
}

bool
drd_info_rep::is_dynamic (tree t) {
  if (L(t) >= START_EXTENSIONS) return true; // FIXME: temporary fix
  if (is_atomic (t)) return false;
  if (is_func (t, DOCUMENT) || is_func (t, PARA) || is_func (t, CONCAT) ||
      is_func (t, TABLE) || is_func (t, ROW)) return false;
  return info[L(t)]->pi.arity_mode != ARITY_NORMAL;
}

/******************************************************************************
* Border accessability related methods
******************************************************************************/

void
drd_info_rep::set_no_border (tree_label l, bool has_no_border) {
  if (info[l]->pi.freeze_no_border) return;
  if (!info->contains (l)) info(l)= copy (info[l]);
  tag_info& ti= info(l);
  ti->pi.no_border= has_no_border;
}

bool
drd_info_rep::get_no_border (tree_label l) {
  return info[l]->pi.no_border;
}

void
drd_info_rep::freeze_no_border (tree_label l) {
  if (!info->contains (l)) info(l)= copy (info[l]);
  tag_info& ti= info(l);
  ti->pi.freeze_no_border= true;
}

bool
drd_info_rep::is_child_enforcing (tree t) {
  return info[L(t)]->pi.no_border && (N(t) != 0);
}

bool
drd_info_rep::var_without_border (tree_label l) {
  return info[l]->pi.no_border && (!std_contains (as_string (l)));
}

/******************************************************************************
* Other attributes
******************************************************************************/

void
drd_info_rep::set_attribute (tree_label l, string which, tree val) {
  if (!info->contains (l)) info(l)= copy (info[l]);
  tag_info& ti= info(l);
  ti->set_attribute (which, val);
}

tree
drd_info_rep::get_attribute (tree_label l, string which) {
  tree val= info[l]->get_attribute (which);
  if ((which == "name") && (val == ""))
    return as_string (l);
  return val;
}

void
drd_info_rep::set_name (tree_label l, string val) {
  set_attribute (l, "name", val);
}

string
drd_info_rep::get_name (tree_label l) {
  return as_string (get_attribute (l, "name"));
}

/******************************************************************************
* Children's accessability related methods
******************************************************************************/

void
drd_info_rep::set_accessible (tree_label l, int nr, int is_accessible) {
  if (!info->contains (l)) info(l)= copy (info[l]);
  tag_info  & ti= info(l);
  if (nr >= N(ti->ci)) return;
  child_info& ci= ti->ci[nr];
  if (ci.freeze_accessible) return;
  ci.accessible= is_accessible;
}

int
drd_info_rep::get_accessible (tree_label l, int nr) {
  return info[l]->ci[nr].accessible;
}

void
drd_info_rep::freeze_accessible (tree_label l, int nr) {
  if (!info->contains (l)) info(l)= copy (info[l]);
  tag_info  & ti= info(l);
  child_info& ci= ti->ci[nr];
  ci.freeze_accessible= true;
}

bool
drd_info_rep::all_accessible (tree_label l) {
  int i, n= N(info[l]->ci);
  for (i=0; i<n; i++)
    if (info[l]->ci[i].accessible != ACCESSIBLE_ALWAYS)
      return false;
  return n>0;
}

bool
drd_info_rep::is_accessible_child (tree t, int i) {
  tag_info ti= info[L(t)];
  int index= ti->get_index (i, N(t));
  if ((index<0) || (index>=N(ti->ci))) return false;
  switch (get_access_mode ()) {
  case DRD_ACCESS_NORMAL:
    return ti->ci[index].accessible == ACCESSIBLE_ALWAYS;
  case DRD_ACCESS_HIDDEN:
    return ti->ci[index].accessible == ACCESSIBLE_ALWAYS ||
           ti->ci[index].accessible == ACCESSIBLE_HIDDEN;
  case DRD_ACCESS_SOURCE:
    return true;
  }
  return true; // NOT REACHED
}

/******************************************************************************
* Children's writability
******************************************************************************/

void
drd_info_rep::set_writability (tree_label l, int nr, int writability) {
  if (!info->contains (l)) info(l)= copy (info[l]);
  tag_info  & ti= info(l);
  if (nr >= N(ti->ci)) return;
  child_info& ci= ti->ci[nr];
  if (ci.freeze_writability) return;
  ci.writability= writability;
}

int
drd_info_rep::get_writability (tree_label l, int nr) {
  return info[l]->ci[nr].writability;
}

void
drd_info_rep::freeze_writability (tree_label l, int nr) {
  if (!info->contains (l)) info(l)= copy (info[l]);
  tag_info  & ti= info(l);
  child_info& ci= ti->ci[nr];
  ci.freeze_writability= true;
}

int
drd_info_rep::get_writability_child (tree t, int i) {
  tag_info ti= info[L(t)];
  int index= ti->get_index (i, N(t));
  if ((index<0) || (index>=N(ti->ci))) return WRITABILITY_DISABLE;
  return ti->ci[index].writability;
}

/******************************************************************************
* Environment determination
******************************************************************************/

static tree
env_write (tree env, string var, tree val) {
  for (int i=0; i<=N(env); i+=2)
    if (i == N(env))
      return env * tree (WITH, var, val);
    else if (var <= env[i]->label) {
      if (var == env[i]->label)
	return env (0, i) * tree (WITH, var, val) * env (i+2, N(env));
      return env (0, i) * tree (WITH, var, val) * env (i, N(env));
    }
  return env;
}

static tree
env_merge (tree env, tree t) {
  int i, n= N(t);
  for (i=0; i<n; i+=2)
    if (is_atomic (t[i]))
      env= env_write (env, t[i]->label, t[i+1]);
  return env;
}

static tree
env_read (tree env, string var, tree val) {
  int i, n= N(env);
  for (i=0; i<n; i+=2)
    if (env[i] == var)
      return env[i+1];
  return val;
}

void
drd_info_rep::set_env (tree_label l, int nr, tree env) {
  //if (as_string (l) == "section")
  //cout << as_string (l) << ", " << nr << " -> " << env << "\n";
  //if (as_string (l) == "session")
  //cout << as_string (l) << ", " << nr << " -> " << env << "\n";
  if (!info->contains (l)) info(l)= copy (info[l]);
  tag_info  & ti= info(l);
  if (nr >= N(ti->ci)) return;
  child_info& ci= ti->ci[nr];
  if (ci.freeze_env) return;
  ci.env= drd_encode (env);
}

tree
drd_info_rep::get_env (tree_label l, int nr) {
  return drd_decode (info[l]->ci[nr].env);
}

void
drd_info_rep::freeze_env (tree_label l, int nr) {
  if (!info->contains (l)) info(l)= copy (info[l]);
  tag_info  & ti= info(l);
  child_info& ci= ti->ci[nr];
  ci.freeze_env= true;
}

tree
drd_info_rep::get_env_child (tree t, int i, tree env) {
  if (L(t) == WITH && i == N(t)-1)
    return env_merge (env, t (0, N(t)-1));
  else {
    if (L(t) == DOCUMENT && N(t) > 0 &&
	(is_compound (t[0], "hide-preamble", 1) ||
	 is_compound (t[0], "show-preamble", 1)))
      {
	tree u= t[0][0];
	if (!is_func (u, DOCUMENT)) u= tree (DOCUMENT, u);
	tree cenv (WITH);
	for (int i=0; i<N(u); i++)
	  if (is_func (u[i], ASSIGN, 2))
	    cenv << copy (u[i][0]) << copy (u[i][1]);
	env= env_merge (env, cenv);
      }

    tag_info ti= info[L(t)];
    int index= ti->get_index (i, N(t));
    if ((index<0) || (index>=N(ti->ci))) return "";
    tree cenv= drd_decode (ti->ci[index].env);
    for (int i=1; i<N(cenv); i+=2)
      if (is_func (cenv[i], ARG, 1) && is_int (cenv[i][0])) {
	cenv= copy (cenv);
	int j= as_int (cenv[i][0]);
	if (j>=0 && j<N(t)) cenv[i]= copy (t[j]);
      }
    return env_merge (env, cenv);
  }
}

tree
drd_info_rep::get_env_child (tree t, int i, string var, tree val) {
  tree env= get_env_child (t, i, tree (WITH));
  return env_read (env, var, val);
}

tree
drd_info_rep::get_env_descendant (tree t, path p, tree env) {
  if (is_nil (p) || env == "") return env;
  int  i= p->item;
  path q= p->next;
  if (is_compound (t) && i >= 0 && i < N(t))
    return get_env_descendant (t[i], q, get_env_child (t, i, env));
  return "";
}

/******************************************************************************
* Heuristic initialization of DRD
******************************************************************************/

static tree
arg_access_env (drd_info_rep* drd, tree t, tree arg, tree env) {
  // returns "" if unaccessible and the env if accessible
  //cout << "  arg_access_env " << t << ", " << arg << ", " << env << "\n";
  if (is_atomic (t)) return "";
  else if (t == arg) return env;
  else if (is_func (t, MAP_ARGS) && (t[2] == arg[0])) {
    if ((N(t) >= 4) && (N(arg) >= 2) && (as_int (t[3]) > as_int (arg[1])))
      return "";
    if ((N(t) == 5) && (N(arg) >= 2) && (as_int (t[3]) <= as_int (arg[1])))
      return "";
    tree_label inner= make_tree_label (as_string (t[0]));
    tree_label outer= make_tree_label (as_string (t[1]));
    if ((drd->get_nr_indices (inner) > 0) &&
	(drd->get_accessible (inner, 0) == ACCESSIBLE_ALWAYS) &&
	drd->all_accessible (outer))
      return env;
    return "";
  }
  else if (is_func (t, MACRO)) return "";
  else if (is_func (t, WITH)) {
    int n= N(t)-1;
    //cout << "env= " << env_merge (env, t (0, n)) << "\n";
    return arg_access_env (drd, t[n], arg, env_merge (env, t (0, n)));
  }
  else if (is_func (t, TFORMAT)) {
    int n= N(t)-1;
    tree oldf= env_read (env, CELL_FORMAT, tree (TFORMAT));
    tree newf= oldf * tree (TFORMAT, A (t (0, n)));
    tree w   = tree (WITH, CELL_FORMAT, newf);
    tree cenv= drd->get_env_child (t, n, env_merge (env, w));
    return arg_access_env (drd, t[n], arg, cenv);
  }
  else if (is_func (t, COMPOUND) && N(t) >= 1 && is_atomic (t[0]))
    return arg_access_env (drd, compound (t[0]->label, A (t (1, N(t)))),
			   arg , env);
  else if ((is_func (t, IF) || is_func (t, VAR_IF)) && N(t) >= 2)
    return arg_access_env (drd, t[1], arg, env);
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (drd->is_accessible_child (t, i)) {
	tree cenv= drd->get_env_child (t, i, env);
	tree aenv= arg_access_env (drd, t[i], arg, cenv);
	if (aenv != "") return aenv;
      }
    return "";
  }
}

static void
rewrite_symbolic_arguments (tree macro, tree& env) {
  if (!is_func (env, WITH)) return;
  for (int i=1; i<N(env); i+=2)
    if (is_func (env[i], ARG, 1)) {
      for (int j=0; j+1<N(macro); j++)
	if (macro[j] == env[i][0])
	  env[i]= tree (ARG, as_tree (j));
    }
}

bool
drd_info_rep::heuristic_init_macro (string var, tree macro) {
  //cout << "init_macro " << var << " -> " << macro << "\n";
  tree_label l = make_tree_label (var);
  tag_info old_ti= copy (info[l]);
  int i, n= N(macro)-1;
  set_arity (l, n, 0, ARITY_NORMAL, CHILD_DETAILED);
  for (i=0; i<n; i++) {
    tree arg (ARG, macro[i]);
    tree env= arg_access_env (this, macro[n], arg, tree (WITH));
    //if (var == "section" || var == "section-title")
    //cout << var << " -> " << env << ", " << macro << "\n";
    if (env != "") {
      //if (var == "eqnarray*")
      //cout << var << " -> " << env << "\n";
      //if (var == "session")
      //cout << var << " = " << macro << ", " << i << " -> " << env << "\n";
      rewrite_symbolic_arguments (macro, env);
      set_accessible (l, i, ACCESSIBLE_ALWAYS);
      set_env (l, i, env);
    }
  }
  // if (old_ti != info[l])
  //   cout << var << ": " << old_ti << " -> " << info[l] << "\n";
  return (old_ti != info[l]);
}

static int
minimal_arity (tree t, tree var) {
  if (is_atomic (t)) return 0;
  else if (is_func (t, ARG, 2) && (t[0] == var))
    return as_int (t[1]) + 1;
  else if (is_func (t, MAP_ARGS) && (N(t)>=4) && (t[2] == var))
    return as_int (t[3]);
  else {
    int i, n= N(t), m= 0;
    for (i=0; i<n; i++)
      m= max (m, minimal_arity (t[i], var));
    return m;
  }
}

bool
drd_info_rep::heuristic_init_xmacro (string var, tree xmacro) {
  tree_label l = make_tree_label (var);
  tag_info old_ti= copy (info[l]);
  int i, m= minimal_arity (xmacro[1], xmacro[0]);
  set_arity (l, m, 1, ARITY_REPEAT, CHILD_DETAILED);
  for (i=0; i<=m; i++) {
    tree arg (ARG, xmacro[0], as_string (i));
    tree env= arg_access_env (this, xmacro[1], arg, tree (WITH));
    if (env != "") {
      set_accessible (l, i, ACCESSIBLE_ALWAYS);
      set_env (l, i, env);
    }
  }
  // if (old_ti != info[l])
  //   cout << var << ": " << old_ti << " -> " << info[l] << "\n";
  return (old_ti != info[l]);
}

void
drd_info_rep::heuristic_init (hashmap<string,tree> env) {
  // time_t tt= texmacs_time ();
  bool flag= true;
  int round= 0;
  while (flag) {
    // cout << HRULE;
    flag= false;
    iterator<string> it= iterate (env);
    while (it->busy()) {
      string var= it->next();
      tree   val= env[var];
      if (is_func (val, MACRO))
	flag= heuristic_init_macro (var, val) | flag;
      if (is_func (val, XMACRO))
	flag= heuristic_init_xmacro (var, val) | flag;
    }
    if ((round++) == 10) {
      cout << "TeXmacs] Warning: bad heuristic drd convergence\n";
      flag= false;
    }
  }
  // cout << "--> " << (texmacs_time ()-tt) << "ms\n";
}
