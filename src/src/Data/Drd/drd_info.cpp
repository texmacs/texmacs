
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
  name (name2), ti (tag_info ()) {}
drd_info_rep::drd_info_rep (string name2, drd_info base):
  name (name2), ti (tag_info (), base->ti) {}
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
* Accessing the drd
******************************************************************************/

void
drd_info_rep::set_arity (tree_label l, int arity) {
  if (!ti->contains (l)) ti(l)= tag_info (arity, 0);
  else ti(l)->arity= arity;
}

void
drd_info_rep::set_props (tree_label l, int props) {
  if (!ti->contains (l)) ti(l)= tag_info (-1, props);
  else ti(l)->props= props;
}

void
drd_info_rep::set_masked_props (tree_label l, int mask, int props) {
  if (!ti->contains (l)) ti(l)= tag_info (-1, mask & props);
  else ti(l)->props= (ti[l]->props & (~mask)) | props;
}

int
drd_info_rep::get_arity (tree_label l) {
  return ti[l]->arity;
}

int
drd_info_rep::get_props (tree_label l) {
  return ti[l]->props;
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
  tree_label l= make_tree_label (var);
  int i, n= N(macro)-1;
  int old_arity= get_arity (l);
  int new_arity= old_arity;
  int old_props= get_props (l);
  int new_props= old_props;
  bool changed= false;

  /* Getting accessibility flags */
  if ((new_props & FROZEN_ARITY) == 0) {
    new_arity= n;
    set_arity (l, new_arity);
    changed= changed || (new_arity != old_arity);
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
      }
      else all_on= false;
    }
    if (all_on) new_props += ACCESSIBLE;
    else if (all_off) new_props += NOT_ACCESSIBLE;
    else new_props += CUSTOM_ACCESSIBLE + detailed;
    set_props (l, new_props);
    changed= changed || (new_props != old_props);
  }

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
  tree_label l= L (t);
  return (get_props (l) & DYNAMIC_MASK) == DYNAMIC;
}

bool
drd_info_rep::is_accessible_child (tree t, int i) {
  // if (L(t) >= START_EXTENSIONS) return true; // FIXME: temporary fix
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
drd_info_rep::is_child_enforcing (tree t) {
  if (L(t) >= START_EXTENSIONS) return false; // FIXME: temporary fix
  return
    (N(t) != 0) &&
    ((get_props (L(t)) & BORDER_ACCESSIBLE_MASK) ==
     BORDER_NOT_ACCESSIBLE);
}
