
/******************************************************************************
* MODULE     : drd_info.cpp
* DESCRIPTION: data relation descriptions
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "drd_info.hpp"
#include "iterator.hpp"

/******************************************************************************
* Constructors and basic operations
******************************************************************************/

drd_info_rep::drd_info_rep (string name2):
  name (name2), info (tag_info ()) {}
drd_info_rep::drd_info_rep (string name2, drd_info base):
  name (name2), info (tag_info (), base->info) {}
drd_info::drd_info (string name):
  rep (new drd_info_rep (name)) {}
drd_info::drd_info (string name, drd_info base):
  rep (new drd_info_rep (name, base)) {}

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

void
drd_info_rep::set_locals (tree t) {
  if (!is_func (t, COLLECTION))
    fatal_error ("Bad set locals", "drd_info_rep::set_locals");
  int i, n= N(t);
  for (i=0; i<n; i++)
    if (is_func (t[i], ASSOCIATE, 2) && is_atomic (t[i][0]))
      info (make_tree_label (t[i][0]->label))= tag_info (t[i][1]);
}

ostream&
operator << (ostream& out, drd_info drd) {
  return out << "drd [" << drd->name << "]";
}

/******************************************************************************
* New access methods
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

void
drd_info_rep::set_accessible (tree_label l, int nr, bool is_accessible) {
  if (!info->contains (l)) info(l)= copy (info[l]);
  tag_info  & ti= info(l);
  child_info& ci= ti->ci[nr];
  if (ci.freeze_accessible) return;
  ci.accessible= is_accessible;
}

bool
drd_info_rep::get_accessible (tree_label l, int nr) {
  return info[l]->ci[nr].accessible;
}

bool
drd_info_rep::all_accessible (tree_label l) {
  int i, n= N(info[l]->ci);
  for (i=0; i<n; i++)
    if (!info[l]->ci[i].accessible)
      return false;
  return true;
}

void
drd_info_rep::freeze_accessible (tree_label l, int nr) {
  if (!info->contains (l)) info(l)= copy (info[l]);
  tag_info  & ti= info(l);
  child_info& ci= ti->ci[nr];
  ci.freeze_accessible= true;
}

/******************************************************************************
* Accessing the drd
******************************************************************************/

bool
drd_info_rep::contains (string l) {
  return existing_tree_label (l) && info->contains (as_tree_label (l));
}

int
drd_info_rep::get_arity (tree_label l) {
  tag_info ti= info[l];
  if (ti->pi.arity_mode != ARITY_NORMAL) return -1;
  else return ((int) ti->pi.arity_base) + ((int) ti->pi.arity_extra);
}

/******************************************************************************
* Heuristic initialization of DRD
******************************************************************************/

static bool
accessible_macro_arg (drd_info_rep* drd, tree t, tree var) {
  if (is_atomic (t)) return false;
  else if (is_func (t, ARGUMENT)) return t == tree (ARGUMENT, var);
  else if (is_func (t, MACRO)) return false;
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (drd->is_accessible_child (t, i))
	if (accessible_macro_arg (drd, t[i], var))
	  return true;
    return false;
  }
}

bool
drd_info_rep::heuristic_init (string var, tree macro) {
  tree_label l = make_tree_label (var);
  tag_info old_ti= copy (info[l]);
  int i, n= N(macro)-1;
  set_arity (l, n, 0, ARITY_NORMAL, CHILD_DETAILED);
  for (i=0; i<n; i++)
    set_accessible (l, i, accessible_macro_arg (this, macro[n], macro[i]));
  // if (old_ti != info[l])
  //   cout << var << ": " << old_ti << " -> " << info[l] << "\n";
  return (old_ti != info[l]);
}

void
drd_info_rep::heuristic_init (hashmap<string,tree> env) {
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
	flag= heuristic_init (var, val) | flag;
    }
    if ((round++) == 10) {
      cout << "TeXmacs] Warning: bad heuristic drd convergence\n";
      flag= false;
    }
  }
}

/******************************************************************************
* Drd-based predicates
******************************************************************************/

bool
drd_info_rep::is_dynamic (tree t) {
  if (L(t) >= START_EXTENSIONS) return true; // FIXME: temporary fix
  if (is_atomic (t)) return false;
  if (is_func (t, DOCUMENT) || is_func (t, PARAGRAPH) || is_func (t, CONCAT) ||
      is_func (t, TABLE) || is_func (t, ROW)) return false;
  return info[L(t)]->pi.arity_mode != ARITY_NORMAL;
}

bool
drd_info_rep::is_accessible_child (tree t, int i) {
  tag_info ti= info[L(t)];
  int index= ti->get_index (i, N(t));
  if ((index<0) || (index>=N(ti->ci))) return false;
  return ti->ci[index].accessible;
}

bool
drd_info_rep::is_child_enforcing (tree t) {
  return info[L(t)]->pi.no_border && (N(t) != 0);
}
