
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

int
drd_info_rep::get_arity (tree_label l) {
  return ti[l]->arity;
}

int
drd_info_rep::get_props (tree_label l) {
  return ti[l]->props;
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
  if (L(t) >= START_EXTENSIONS) return true; // FIXME: temporary fix
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
