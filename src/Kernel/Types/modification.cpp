
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
* Equality and Output
******************************************************************************/

bool
operator == (modification m1, modification m2) {
  return m1->k == m2->k && m1->p == m2->p && m1->t == m2->t;
}

bool
operator != (modification m1, modification m2) {
  return m1->k != m2->k || m1->p != m2->p || m1->t != m2->t;
}

tm_ostream&
operator << (tm_ostream& out, modification mod) {
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
  case MOD_SET_CURSOR:
    return out << "set_cursor (" << root (mod)
	       << ", " << index (mod) << ", " << mod->t << ")";
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
  case MOD_SET_CURSOR: return path_up (mod->p);
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
  case MOD_SET_CURSOR: return last_item (mod->p);
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
* Test applicability of modifications
******************************************************************************/

bool
can_assign (tree t, path p, tree u) {
  (void) u;
  return has_subtree (t, p);
}

bool
can_insert (tree t, path p, int pos, tree u) {
  if (!has_subtree (t, p)) return false;
  tree st= subtree (t, p);
  if (is_atomic (st)) return pos >= 0 && pos <= N(st->label) && is_atomic (u);
  else return pos >= 0 && pos <= N(st) && is_compound (u);
}

bool
can_remove (tree t, path p, int pos, int nr) {
  if (!has_subtree (t, p)) return false;
  tree st= subtree (t, p);
  if (is_atomic (st)) return pos >= 0 && pos+nr <= N(st->label);
  else return pos >= 0 && pos+nr <= N(st);
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
  tree st= subtree (t, p);
  if (pos < 0 || pos+1 >= N(st)) return false;
  if (is_atomic (st[pos]) && is_atomic (st[pos+1])) return true;
  if (is_compound (st[pos]) && is_compound (st[pos+1])) return true;
  return false;
}

bool
can_assign_node (tree t, path p, tree_label op) {
  (void) op;
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
can_set_cursor (tree t, path p, int pos, tree data) {
  (void) data;
  if (!has_subtree (t, p)) return false;
  return pos >= 0 && pos <= right_index (subtree (t, p));
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
  case MOD_SET_CURSOR:
    return can_set_cursor (t, root (mod), index (mod), mod->t);
  default:
    return false;
  }
}

/******************************************************************************
* Functional application of modifications
******************************************************************************/

tree
clean_assign (tree t, path p, tree u) {
  if (is_nil (p)) return copy (u);
  else {
    int i, j= p->item, n= N(t);
    tree r (t, n);
    for (i=0; i<j; i++) r[i]= t[i];
    r[j]= clean_assign (t[j], p->next, u);
    for (i++; i<n; i++) r[i]= t[i];
    return r;
  }
}

tree
clean_insert (tree t, path p, tree u) {
  if (is_nil (p->next) && is_atomic (t)) {
    string s= t->label;
    return s (0, p->item) * u->label * s (p->item, N(s));
  }
  else if (is_nil (p->next)) {
    int i, j= p->item, n= N(t), nr= N(u);
    tree r (t, n+nr);
    for (i=0; i<j; i++) r[i]= t[i];
    for (; i<n; i++) r[i+nr]= t[i];
    for (i=0; i<nr; i++) r[j+i]= copy (u[i]);
    return r;
  }
  else {
    int i, j= p->item, n= N(t);
    tree r (t, n);
    for (i=0; i<j; i++) r[i]= t[i];
    r[j]= clean_insert (t[j], p->next, u);
    for (i++; i<n; i++) r[i]= t[i];
    return r;
  }  
}

tree
clean_remove (tree t, path p, int nr) {
  if (is_nil (p->next) && is_atomic (t)) {
    string s= t->label;
    return s (0, p->item) * s (p->item+nr, N(s));
  }
  else if (is_nil (p->next)) {
    int i, j= p->item, n= N(t);
    tree r (t, n-nr);
    for (i=0; i<j; i++) r[i]= t[i];
    for (i+=nr; i<n; i++) r[i-nr]= t[i];
    return r;
  }
  else {
    int i, j= p->item, n= N(t);
    tree r (t, n);
    for (i=0; i<j; i++) r[i]= t[i];
    r[j]= clean_remove (t[j], p->next, nr);
    for (i++; i<n; i++) r[i]= t[i];
    return r;
  }  
}

tree
clean_split (tree t, path p) {
  if (is_nil (p->next->next)) {
    tree u= t[p->item];
    int i, n1= p->next->item, n2= N(u)-n1;
    tree s1, s2;
    if (is_atomic (u)) {
      s1= u->label (0, n1);
      s2= u->label (n1, N(u->label));
    }
    else {
      s1= tree (u, n1);
      s2= tree (u, n2);
      for (i=0; i<n1; i++) s1[i]= u[i];
      for (i=0; i<n2; i++) s2[i]= u[n1+i];
    }

    int j= p->item, n= N(t);
    tree r (t, n+1);
    for (i=0; i<j; i++) r[i]= t[i];
    r[j]= s1; r[j+1]= s2;
    for (i++; i<n; i++) r[i+1]= t[i];
    return r;
  }
  else {
    int i, j= p->item, n= N(t);
    tree r (t, n);
    for (i=0; i<j; i++) r[i]= t[i];
    r[j]= clean_split (t[j], p->next);
    for (i++; i<n; i++) r[i]= t[i];
    return r;
  }  
}

tree
clean_join (tree t, path p) {
  if (is_nil (p->next)) {
    int i, j= p->item;
    tree s1= t[j], s2= t[j+1], u;
    if (is_atomic (s1))
      u= tree (s1->label * s2->label);
    else {
      int n1= N(s1), n2= N(s2);
      u= tree (s1, n1+n2);
      for (i=0; i<n1; i++) u[i]= s1[i];
      for (i=0; i<n2; i++) u[n1+i]= s2[i];
    }

    int n= N(t);
    tree r (t, n-1);
    for (i=0; i<j; i++) r[i]= t[i];
    r[j]= u;
    for (i+=2; i<n; i++) r[i-1]= t[i];
    return r;
  }
  else {
    int i, j= p->item, n= N(t);
    tree r (t, n);
    for (i=0; i<j; i++) r[i]= t[i];
    r[j]= clean_join (t[j], p->next);
    for (i++; i<n; i++) r[i]= t[i];
    return r;
  }  
}

tree
clean_assign_node (tree t, path p, tree_label op) {
  if (is_nil (p)) {
    int i, n= N(t);
    tree r (op, n);
    for (i=0; i<n; i++) r[i]= t[i];
    return r;
  }
  else {
    int i, j= p->item, n= N(t);
    tree r (t, n);
    for (i=0; i<j; i++) r[i]= t[i];
    r[j]= clean_assign_node (t[j], p->next, op);
    for (i++; i<n; i++) r[i]= t[i];
    return r;
  }  
}

tree
clean_insert_node (tree t, path p, tree u) {
  if (is_nil (p->next)) {
    int i, j= p->item, n= N(u);
    tree r (u, n+1);
    for (i=0; i<j; i++) r[i]= u[i];
    r[j]= t;
    for (; i<n; i++) r[i+1]= u[i];
    return r;
  }
  else {
    int i, j= p->item, n= N(t);
    tree r (t, n);
    for (i=0; i<j; i++) r[i]= t[i];
    r[j]= clean_insert_node (t[j], p->next, u);
    for (i++; i<n; i++) r[i]= t[i];
    return r;
  }  
}

tree
clean_remove_node (tree t, path p) {
  if (is_nil (p->next)) return t[p->item];
  else {
    int i, j= p->item, n= N(t);
    tree r (t, n);
    for (i=0; i<j; i++) r[i]= t[i];
    r[j]= clean_remove_node (t[j], p->next);
    for (i++; i<n; i++) r[i]= t[i];
    return r;
  }  
}

tree
clean_set_cursor (tree t, path p, tree data) {
  (void) p; (void) data;
  return t;
}

tree
clean_apply (tree t, modification mod) {
  switch (mod->k) {
  case MOD_ASSIGN:
    return clean_assign (t, mod->p, mod->t);
  case MOD_INSERT:
    return clean_insert (t, mod->p, mod->t);
  case MOD_REMOVE:
    return clean_remove (t, path_up (mod->p), last_item (mod->p));
  case MOD_SPLIT:
    return clean_split (t, mod->p);
  case MOD_JOIN:
    return clean_join (t, mod->p);
  case MOD_ASSIGN_NODE:
    return clean_assign_node (t, mod->p, L(mod));
  case MOD_INSERT_NODE:
    return clean_insert_node (t, mod->p, mod->t);
  case MOD_REMOVE_NODE:
    return clean_remove_node (t, mod->p);
  case MOD_SET_CURSOR:
    return clean_set_cursor (t, mod->p, mod->t);
  default:
    FAILED ("invalid modification type");
    return "";
  }
}
