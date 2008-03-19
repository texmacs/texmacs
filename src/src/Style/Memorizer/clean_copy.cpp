
/******************************************************************************
* MODULE     : clean_copy.cpp
* DESCRIPTION: maintain a clean copy of the edit tree
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
* Functional tree modification routines
******************************************************************************/

static tree clean_et;

void
global_notify_assign (path p, tree u) {
  //cout << "Assign " << p << ", " << u << "\n";
  clean_et= clean_assign (clean_et, p, u);
}

void
global_notify_insert (path p, tree u) {
  //cout << "Insert " << p << ", " << u << "\n";
  clean_et= clean_insert (clean_et, p, u);
}

void
global_notify_remove (path p, int nr) {
  //cout << "Remove " << p << ", " << nr << "\n";
  clean_et= clean_remove (clean_et, p, nr);
}

void
global_notify_split (path p) {
  //cout << "Split " << p << "\n";
  clean_et= clean_split (clean_et, p);
}

void
global_notify_join (path p) {
  //cout << "Join " << p << "\n";
  clean_et= clean_join (clean_et, p);
}

void
global_notify_insert_node (path p, tree u) {
  //cout << "Insert node " << p << ", " << u << "\n";
  clean_et= clean_insert_node (clean_et, p, u);
}

void
global_notify_remove_node (path p) {
  //cout << "Remove node " << p << "\n";
  clean_et= clean_remove_node (clean_et, p);
}

void
global_notify_assign_node (path p, tree_label op) {
  //cout << "Assign node " << p << ", " << op << "\n";
  clean_et= clean_assign_node (clean_et, p, op);
}

tree
global_get_subtree (path p) {
  return subtree (clean_et, p);
}

void
global_trace_subtree (path p) {
  print_tree (subtree (clean_et, p));
}
