
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

/******************************************************************************
* Applying modifications
******************************************************************************/

bool
has_subtree (tree t, path p) {
  if (is_nil (p)) return true;
  if (is_atomic (t)) return false;
  if ((0 > p -> item) || (p -> item >= N(t))) return false;
  return has_subtree (t[p->item], p->next);
}

bool
can_assign (tree t, path p, tree u) {
  (void) u;
  return has_subtree (t, p);
}

bool
can_insert (tree t, path p, int pos, tree u) {
  if (!has_subtree (t, p)) return false;
  if (is_atomic (t)) return pos >= 0 && pos <= N(t->label) && is_atomic (u);
  else return pos >= 0 && pos <= N(t) && is_compound (u);
}

bool
can_remove (tree t, path p, int pos, int nr) {
  if (!has_subtree (t, p)) return false;
  if (is_atomic (t)) return pos >= 0 && pos+nr <= N(t->label);
  else return pos >= 0 && pos+nr <= N(t);
}

bool
can_split (tree t, path p, int pos, int at) {
  if (!has_subtree (t, p * pos)) return false;
  tree st= subtree (t, p * pos);
  if (is_atomic (st)) return at >= 0 && at <= N(st->label);
  else return at >= 0 && at <= N(st);
}

bool
can_join (tree t, path p, int pos) {
  if (!has_subtree (t, p)) return false;
  if (pos < 0 || pos+1 >= N(t)) return false;
  if (is_atomic (t[pos]) && is_atomic (t[pos+1])) return true;
  if (is_compound (t[pos]) && is_compound (t[pos+1])) return true;
  return false;
}

bool
can_assign_node (tree t, path p, tree_label op) {
  return has_subtree (t, p) && is_compound (subtree (t, p));
}

bool
can_insert_node (tree t, path p, int pos, tree u) {
  return has_subtree (t, p) && is_compound (u) && pos >= 0 && pos <= N(u);
}

bool
can_remove_node (tree t, path p, int pos) {
  return has_subtree (t, p * pos);
}

bool
is_applicable (tree t, modification mod) {
  switch (mod->k) {
  case MOD_ASSIGN:
    return can_assign (t, root (mod), mod->t);
  case MOD_INSERT:
    return can_insert (t, root (mod), index (mod), mod->t);
  case MOD_REMOVE:
    return can_remove (t, root (mod), index (mod), argument (mod));
  case MOD_SPLIT:
    return can_split (t, root (mod), index (mod), argument (mod));
  case MOD_JOIN:
    return can_join (t, root (mod), index (mod));
  case MOD_ASSIGN_NODE:
    return can_assign_node (t, root (mod), L (mod));
  case MOD_INSERT_NODE:
    return can_insert_node (t, root (mod), argument (mod), mod->t);
  case MOD_REMOVE_NODE:
    return can_remove_node (t, root (mod), index (mod));
  default:
    return false;
  }
}

void
apply (tree& t, modification mod) {
  switch (mod->k) {
  case MOD_ASSIGN:
    assign (subtree (t, root (mod)), mod->t);
    break;
  case MOD_INSERT:
    insert (subtree (t, root (mod)), index (mod), mod->t);
    break;
  case MOD_REMOVE:
    remove (subtree (t, root (mod)), index (mod), argument (mod));
    break;
  case MOD_SPLIT:
    split (subtree (t, root (mod)), index (mod), argument (mod));
    break;
  case MOD_JOIN:
    join (subtree (t, root (mod)), index (mod));
    break;
  case MOD_ASSIGN_NODE:
    assign_node (subtree (t, root (mod)), L (mod));
    break;
  case MOD_INSERT_NODE:
    insert_node (subtree (t, root (mod)), argument (mod), mod->t);
    break;
  case MOD_REMOVE_NODE:
    remove_node (subtree (t, root (mod)), index (mod));
    break;
  }
}
