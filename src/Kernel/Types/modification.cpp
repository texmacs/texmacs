
/******************************************************************************
* MODULE     : modification.hpp
* DESCRIPTION: elementary tree modifications
* COPYRIGHT  : (C) 2008  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "modification.hpp"

/******************************************************************************
* Output
******************************************************************************/

ostream&
operator << (ostream& out, modification mod) {
  switch (mod->k) {
  case MOD_ASSIGN:
    return out << "assign (" << root (mod)
	       << ", " << mod->t << ")";
  case MOD_INSERT:
    return out << "insert (" << root (mod)
	       << ", " << index (mod) << ", " << mod->t << ")";
  case MOD_REMOVE:
    return out << "remove (" << root (mod)
	       << ", " << index (mod) << ", " << argument (mod) << ")";
  case MOD_SPLIT:
    return out << "split (" << root (mod)
	       << ", " << index (mod) << ", " << argument (mod) << ")";
  case MOD_JOIN:
    return out << "join (" << root (mod)
	       << ", " << index (mod) << ")";
  case MOD_ASSIGN_NODE:
    return out << "assign_node (" << root (mod)
	       << ", " << mod->t << ")";
  case MOD_INSERT_NODE:
    return out << "insert_node (" << root (mod)
	       << ", " << argument (mod) << ", " << mod->t << ")";
  case MOD_REMOVE_NODE:
    return out << "remove_node (" << root (mod)
	       << ", " << index (mod) << ")";
  default: FAILED ("invalid modification type");
    return out;
  }
}

/******************************************************************************
* Accessors
******************************************************************************/

path
root (modification mod) {
  switch (mod->k) {
  case MOD_ASSIGN: return mod->p;
  case MOD_INSERT: return path_up (mod->p);
  case MOD_REMOVE: return path_up (path_up (mod->p));
  case MOD_SPLIT: return path_up (path_up (mod->p));
  case MOD_JOIN: return path_up (mod->p);
  case MOD_ASSIGN_NODE: return mod->p;
  case MOD_INSERT_NODE: return path_up (mod->p);
  case MOD_REMOVE_NODE: return path_up (mod->p);
  default: FAILED ("invalid modification type");
  }
}

int
index (modification mod) {
  switch (mod->k) {
  case MOD_INSERT: return last_item (mod->p);
  case MOD_REMOVE: return last_item (path_up (mod->p));
  case MOD_SPLIT: return last_item (path_up (mod->p));
  case MOD_JOIN: return last_item (mod->p);
  case MOD_REMOVE_NODE: return last_item (mod->p);
  default: FAILED ("invalid modification type");
  }
}

int
argument (modification mod) {
  switch (mod->k) {
  case MOD_REMOVE: return last_item (mod->p);
  case MOD_SPLIT: return last_item (mod->p);
  case MOD_INSERT_NODE: return last_item (mod->p);
  default: FAILED ("invalid modification type");
  }
}

tree_label
L (modification mod) {
  ASSERT (mod->k == MOD_ASSIGN_NODE, "assign_node modification expected");
  return L (mod->t);
}
