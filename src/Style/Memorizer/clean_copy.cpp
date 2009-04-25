
/******************************************************************************
* MODULE     : clean_copy.cpp
* DESCRIPTION: maintain a clean copy of the edit tree
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "clean_copy.hpp"

/******************************************************************************
* Functional tree modification routines
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

/******************************************************************************
* Attaching ips
******************************************************************************/

void
copy_ip (tree src, tree dest) {
  path src_ip= obtain_ip (src);
  path dest_ip= obtain_ip (dest);
  if (dest_ip != src_ip) {
    dest->obs= list_observer (ip_observer (src_ip), dest->obs);
    if (is_compound (src)) {
      int i, n= N(src);
      for (i=0; i<n; i++)
	copy_ip (src[i], dest[i]);
    }
  }
}

/******************************************************************************
* Clean copy callbacks
******************************************************************************/

void
copy_announce (tree src, tree& cct, modification mod) {
  //cout << "Announce copy " << mod << "\n";
  switch (mod->k) {
  case MOD_ASSIGN:
    cct= clean_assign (cct, mod->p, mod->t);
    break;
  case MOD_INSERT:
    cct= clean_insert (cct, mod->p, mod->t);
    break;
  case MOD_REMOVE:
    cct= clean_remove (cct, path_up (mod->p), last_item (mod->p));
    break;
  case MOD_SPLIT:
    cct= clean_split (cct, mod->p);
    break;
  case MOD_JOIN:
    cct= clean_join (cct, mod->p);
    break;
  case MOD_ASSIGN_NODE:
    cct= clean_assign_node (cct, mod->p, L(mod));
    break;
  case MOD_INSERT_NODE:
    cct= clean_insert_node (cct, mod->p, mod->t);
    break;
  case MOD_REMOVE_NODE:
    cct= clean_remove_node (cct, mod->p);
    break;
  default: FAILED ("invalid modification type");
  }
  copy_ip (src, cct);
}
