
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
	int e2= b2 + N (m2->t);
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
	int e= b + N (m1->t);
	if (m2->p->item >= b && m2->p->item < e)
	  if (m2->p->item != b || m2->k != MOD_INSERT)
	    return false;
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
	int e= b + N (m2->t);
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
