
/******************************************************************************
* MODULE     : commute.cpp
* DESCRIPTION: Commutation of modifications
* COPYRIGHT  : (C) 2009  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "patch.hpp"

/******************************************************************************
* Commutation of modifications
******************************************************************************/

static modification
dup (modification m) {
  return modification (m->k, copy (m->p), m->t);
}

static int
insert_length (tree t) {
  if (is_atomic (t)) return N(t->label);
  else return N(t);
}

static bool
swap1 (modification& m1, modification& m2, int i, int d) {
  modification r1= dup (m2);
  modification r2= dup (m1);
  if (m2->p->item >= i) r1->p->item -= d;
  if (is_nil (r2))
    switch (m2->k) {
    case MOD_INSERT:
      {
	int b2= m2->p->item;
	int e2= b2 + insert_length (m2->t);
	if (b2 <= i) r2->p->item += (e2-b2);
	break;
      }
    case MOD_REMOVE:
      {
	int b2= m2->p->item;
	int e2= b2 + m2->p->next->item;
	if (b2 < i && e2 > i) return false;
	if (b2 < i) r2->p->item -= (e2-b2);
	break;
      }	      
    case MOD_SPLIT:
      if (m2->p->item < i) r2->p->item++;
      break;
    case MOD_JOIN:
      if (m2->p->item == i-1) return false;
      if (m2->p->item < i) r2->p->item--;
      break;
    case MOD_INSERT_NODE:
    case MOD_REMOVE_NODE:
      return false;
    }
  m1= r1;
  m2= r2;
  return true;
}

static bool
swap2 (modification& m1, modification& m2, int i, int d) {
  modification r1= dup (m2);
  modification r2= dup (m1);
  if (m1->p->item >= i) r2->p->item += d;
  m1= r1;
  m2= r2;
  return true;
}

bool
swap_basic (modification& m1, modification& m2) {
  modification aux= m1;
  m2= m1;
  m1= aux;
  return true;
}

bool
swap (modification& m1, modification& m2) {
  path r1= root (m1);
  path r2= root (m2);
  if (is_nil (r1))
    switch (m1->k) {
    case MOD_ASSIGN:
      return m1 == m2;
    case MOD_INSERT:
      {
	if (is_nil (m2->p)) return false;
	int b= m1->p->item;
	int e= b + insert_length (m1->t);
	if (m2->p->item >= b && m2->p->item < e) {
	  if (!is_nil (root (m2)) || m2->p->item != b || m2->k != MOD_INSERT)
	    return false;
	  modification r1= m2;
	  modification r2= dup (m1);
	  r2->p->item += insert_length (m2->t);
	  m1= r1;
	  m2= r2;
	  return true;
	}
	return swap1 (m1, m2, b, e-b);
      }
    case MOD_REMOVE:
      {
	if (is_nil (m2->p)) return false;
	int i= m1->p->item;
	int d= m1->p->next->item;
	return swap1 (m1, m2, i, -d);
      }
    case MOD_SPLIT:
      {
	if (is_nil (m2->p)) return false;
	int i= m1->p->item;
	if (m2->p->item == i || m2->p->item == i+1) return false;
	return swap1 (m1, m2, i, 1);
      }
    case MOD_JOIN:
      {
	if (is_nil (m2->p)) return false;
	int i= m1->p->item;
	return swap1 (m1, m2, i, -1);
      }
    case MOD_ASSIGN_NODE:
      {
	if (!is_nil (root (m2))) return swap_basic (m1, m2);
	return m1 == m2;
      }
    case MOD_INSERT_NODE:
      {
	if (is_nil (root (m2))) return false;
	if (m2->p->item != m1->p->item) return false;
	modification aux= m1;
	m1= modification (m2->k, m2->p->next, m2->t);
	m2= aux;
	return true;
      }
    case MOD_REMOVE_NODE:
      {
	if (is_nil (root (m2))) return false;
	modification aux= m1;
	m1= modification (m2->k, path (m1->p->item, m2->p), m2->t);
	m2= aux;
	return true;
      }
    }
  else if (is_nil (r2))
    switch (m2->k) {
    case MOD_ASSIGN:
      return false;
    case MOD_INSERT:
      {
	int b= m2->p->item;
	int e= b + insert_length (m2->t);
	return swap2 (m1, m2, b, e-b);
      }
    case MOD_REMOVE:
      {
	int b= m2->p->item;
	int e= b + m2->p->next->item;
	if (m1->p->item >= b && m1->p->item < e) return false;
	return swap2 (m1, m2, b, b-e);
      }
    case MOD_SPLIT:
      {
	int i= m2->p->item;
	if (m1->p->item == i) return false;
	return swap2 (m1, m2, i, 1);
      }
    case MOD_JOIN:
      {
	int i= m2->p->item;
	if (m1->p->item == i || m1->p->item == i+1) return false;
	return swap2 (m1, m2, i, -1);
      }
    case MOD_ASSIGN_NODE:
      return swap_basic (m1, m2);
    case MOD_INSERT_NODE:
      {
	modification aux= m2;
	m2= modification (m1->k, path (m2->p->item, m1->p), m1->t);
	m1= aux;
	return true;
      }
    case MOD_REMOVE_NODE:
      {
	modification aux= m2;
	m2= modification (m1->k, m1->p->next, m1->t);
	m1= aux;
	return true;
      }
    }
  else if (r1->item == r2->item) {
    path h (r1->item);
    modification s1= m1 / h;
    modification s2= m2 / h;
    bool r= swap (s1, s2);
    m1= h * s1;
    m2= h * s2;
    return r;
  }
  else return swap_basic (m1, m2);
}

bool
commute (modification m1, modification m2) {
  modification s1= m1;
  modification s2= m2;
  return swap (s1, s2);
}

/******************************************************************************
* Test routines
******************************************************************************/

modification
test_modification (int i) {
  switch (i) {
  case  0: return mod_assign (path (), "");
  case  1: return mod_insert (path (), 0, tree (TUPLE, "a", "b"));
  case  2: return mod_remove (path (), 0, 2);
  case  3: return mod_split (path (), 0, 1);
  case  4: return mod_join (path (), 0);
  case  5: return mod_assign_node (path (), TUPLE);
  case  6: return mod_insert_node (path (), 1, tree (TUPLE, "a", "b"));
  case  7: return mod_remove_node (path (), 0);

  case  8: return mod_insert (path (), 1, tree (TUPLE, "a", "b"));
  case  9: return mod_insert (path (), 2, tree (TUPLE, "a", "b"));
  case 10: return mod_remove (path (), 1, 2);
  case 11: return mod_remove (path (), 2, 2);
  case 12: return mod_split (path (), 1, 2);
  case 13: return mod_split (path (), 2, 1);
  case 14: return mod_join (path (), 1);
  case 15: return mod_join (path (), 2);
  case 16: return mod_remove_node (path (), 1);
  case 17: return mod_remove_node (path (), 2);

  case 18: case 19: case 20: case 21: case 22: case 23: case 24: case 25:
    return path (0) * test_modification (i-18);
  case 26: case 27: case 28: case 29: case 30: case 31: case 32: case 33:
    return path (1) * test_modification (i-26);
  case 34: case 35: case 36: case 37: case 38: case 39: case 40: case 41:
    return path (2) * test_modification (i-34);
  default:
    FAILED ("not implemented");
    return mod_assign (path (), "");
  }
}

void
test_commute () {
  for (int i=0; i<42; i++)
    for (int j=0; j<42; j++) {
      modification m1= test_modification (i);
      modification m2= test_modification (j);
      modification t1= m1;
      modification t2= m2;
      cout << "m1 = " << m1 << "\n";
      cout << "m2 = " << m2 << "\n";
      bool r= swap (m1, m2);
      if (!r) cout << "  Modifications do not commute\n";
      else {
	cout << "m1'= " << m1 << "\n";
	cout << "m2'= " << m2 << "\n";
	r= swap (m1, m2);
	if (r && m1 == t1 && m2 == t2) cout << "  Consistency check succeeded\n";
	else {
	  cout << "  Consistency check failed\n";
	  FAILED ("inconsistency");
	}
      }
      cout << "\n";
    }
}
