
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

drd_info::operator tree () {
  return tree (TUPLE, "drd", rep->name);
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
  tag_info& ti= info(l);
  if (ti->pi.freeze_arity) return;
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
  tag_info& ti= info(l);
  ti->pi.freeze_arity= true;
}

void
drd_info_rep::set_accessible (tree_label l, int nr, bool is_accessible) {
  tag_info  & ti= info(l);
  child_info& ci= ti->ci[nr];
  if (ci.freeze_accessible) return;
  ci.accessible= is_accessible;
}

void
drd_info_rep::freeze_accessible (tree_label l, int nr) {
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

void
drd_info_rep::set_arity (tree_label l, int arity) {
  if (!info->contains (l)) info(l)= tag_info (arity, 0);
  else info(l)->arity= arity;
}

void
drd_info_rep::set_props (tree_label l, int props) {
  if (!info->contains (l)) info(l)= tag_info (-1, props);
  else info(l)->props= props;
}

void
drd_info_rep::set_masked_props (tree_label l, int mask, int props) {
  if (!info->contains (l)) info(l)= tag_info (-1, mask & props);
  else info(l)->props= (info[l]->props & (~mask)) | props;
}

int
drd_info_rep::get_arity (tree_label l) {
  return info[l]->arity;
}

int
drd_info_rep::get_props (tree_label l) {
  return info[l]->props;
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
  int i, n= N(macro)-1;
  int old_arity= get_arity (l);
  int new_arity= old_arity;
  int old_props= get_props (l);
  int new_props= old_props;
  bool changed = false;

  // NEW
  tag_info old_ti= copy (info[l]);

  /* Getting accessibility flags */
  if ((new_props & FROZEN_ARITY) == 0) {
    new_arity= n;
    set_arity (l, new_arity);
    changed= changed || (new_arity != old_arity);
    // NEW
    set_arity (l, new_arity, 0, ARITY_NORMAL, CHILD_DETAILED);
  }

  /* Getting accessibility flags */
  if ((new_props & FROZEN_ACCESSIBLE) == 0) {
    int detailed = 0;
    bool all_on= true, all_off= true;
    int MASK= ACCESSIBLE_MASK + CUSTOM_ACCESSIBLE_MASK;
    int k= min (n, CUSTOM_ACCESSIBLE_MAX);
    new_props= new_props & (~MASK);
    for (i=0; i<k; i++) {
      if (accessible_macro_arg (this, macro[n], macro[i])) {
	detailed += (1 << (CUSTOM_ACCESSIBLE_SHIFT + i));
	all_off= false;
	// NEW
	info(l)->ci[i].accessible= true;
      }
      else {
	all_on= false;
	// NEW
	info(l)->ci[i].accessible= false;
      }
    }
    if (all_on) new_props += ACCESSIBLE;
    else if (all_off) new_props += NOT_ACCESSIBLE;
    else new_props += CUSTOM_ACCESSIBLE + detailed;
    set_props (l, new_props);
    changed= changed || (new_props != old_props);
  }

  // NEW
  bool new_changed= (old_ti != info[l]);
  if (new_changed != changed)
    cout << var << ": bad changed " << changed
	 << " -> " <<new_changed << "\n";

  return changed;
}

void
drd_info_rep::heuristic_init (hashmap<string,tree> env) {
  bool flag= true;
  while (flag) {
    //cout << HRULE;
    flag= false;
    iterator<string> it= iterate (env);
    while (it->busy()) {
      string var= it->next();
      tree   val= env[var];
      tree_label l= make_tree_label (var);
      if (is_func (val, MACRO) && ((get_props (l) && FROZEN_MASK) == 0))
	flag= heuristic_init (var, val) | flag;
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
  /*
  if (is_atomic (t)) return false;
  tree_label l= L (t);
  return (get_props (l) & DYNAMIC_MASK) == DYNAMIC;
  */
}

bool
drd_info_rep::old_is_accessible_child (tree t, int i) {
  switch (get_props (L(t)) & ACCESSIBLE_MASK) {
  case NOT_ACCESSIBLE:
    return false;
  case ACCESSIBLE:
    return true;
  case FIRST_ACCESSIBLE:
    return i==0;
  case LAST_ACCESSIBLE:
    return i==(N(t)-1);
  case TAIL_ACCESSIBLE:
    return i!=0;
  case TABLE_ACCESSIBLE:
    return i<(N(t)-2);
  case HIDE_EXPAND_ACCESSIBLE:
    return (i!=0) && (i!=(N(t)-1));
  case CUSTOM_ACCESSIBLE:
    if (i >= CUSTOM_ACCESSIBLE_MAX) return false;
    return ((get_props (L(t)) >> (CUSTOM_ACCESSIBLE_SHIFT + i)) & 1) != 0;
  default:
    return false;
  }
}

bool
drd_info_rep::new_is_accessible_child (tree t, int i) {
  if (!info->contains (L(t))) return false;
  if ((i<0) || (i>=N(t)))
    fatal_error ("Out of range", "drd_info_rep::new_is_accessible_child");
  tag_info   ti= info[L(t)];
  child_info ci= ti (i, N(t));
  return ci.accessible;
}

bool
drd_info_rep::is_accessible_child (tree t, int i) {
  bool b1= old_is_accessible_child (t, i);
  bool b2= new_is_accessible_child (t, i);
  if (b2 != b1)
    cout << as_string (L(t)) << ": child " << i
	 << " has bad accessability " << b1 << " -> " << b2 << "\n";
  return b1;
}

bool
drd_info_rep::is_child_enforcing (tree t) {
  if (L(t) >= START_EXTENSIONS) return false; // FIXME: temporary fix
  bool old_result=
    (N(t) != 0) &&
    ((get_props (L(t)) & BORDER_ACCESSIBLE_MASK) ==
     BORDER_NOT_ACCESSIBLE);
  bool new_result=
    info[L(t)]->pi.no_border && (N(t) != 0);
  if (new_result != old_result)
    cout << "Warning: " << as_string (L(t)) << ": bad no_border\n";
  return new_result;
}
