
/******************************************************************************
* MODULE     : assoc_environment.cpp
* DESCRIPTION: association lists as environments
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "assoc_environment.hpp"

tree assoc_node::uninit (UNINIT);
tree assoc_environment_rep::uninit (UNINIT);  

/******************************************************************************
* Standard methods for assoc environments
******************************************************************************/

bool
assoc_environment_rep::contains (int key) {
  for (int i=0; i<n; i++)
    if (a[i].key == key) return true;
  return false;
}

tree
assoc_environment_rep::read (int key) {
  for (int i=0; i<n; i++)
    if (a[i].key == key) return a[i].val;
  return uninit;
}

void
assoc_environment_rep::write (int key, const tree& val) {
  for (int i=0; i<n; i++)
    if (a[i].key == key) { a[i].val= val; return; }
  assoc_node* b= new assoc_node[n+1];
  for (int i=0; i<n; i++) b[i]= a[i];
  b[n].key= key;
  b[n].val= val;
  delete[] a;
  a= b;
  n++;
}

void
assoc_environment_rep::remove (int key) {
  for (int i=0; i<n; i++)
    if (a[i].key == key) {
      assoc_node* b= new assoc_node[n-1];
      for (int j=0; j<i; j++) b[j]= a[j];
      for (int j=i+1; j<n; j++) b[j-1]= a[j];
      delete[] a;
      a= b;
      n--;
      return;
    }
}

void
assoc_environment_rep::print (const string& prefix) {
  cout << prefix << "Assoc environment" << LF;
  for (int i=0; i<n; i++)
    cout << prefix << "| " << a[i].key
	 << ": " << as_string ((tree_label) a[i].key)
	 << " -> " << a[i].val << LF;
}

/******************************************************************************
* Copying
******************************************************************************/

assoc_environment
copy (assoc_environment env) {
  int i, n= env->n;
  assoc_environment r (n);
  for (i=0; i<n; i++)
    r->raw_write (i, env->a[i].key, env->a[i].val);
  return r;
}

/******************************************************************************
* Weak hashing
******************************************************************************/

int
weak_hash (assoc_environment env) {
  int i, n= env->n, h=0;
  assoc_node* a= env->a;
  for (i=0; i<n; i++)
    h= (h<<1) ^ (h>>31) ^ a[i].key ^ (weak_hash (a[i].val)<<2);
  return h;
}

bool
weak_equal (assoc_environment env1, assoc_environment env2) {
  int i, n= env1->n;
  if (env2->n != n) return false;
  assoc_node* a1= env1->a;
  assoc_node* a2= env2->a;
  for (i=0; i<n; i++)
    if (a1[i].key != a2[i].key || !weak_equal (a1[i].val, a2[i].val))
      return false;
  return true;
}
