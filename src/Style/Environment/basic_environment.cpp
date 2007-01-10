
/******************************************************************************
* MODULE     : basic_environment.cpp
* DESCRIPTION: hash tables as environments
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "basic_environment.hpp"

tree hash_node::uninit (UNINIT);
tree basic_environment_rep::uninit (UNINIT);

/******************************************************************************
* Raw access methods which assume an appropriate size for the hash table
******************************************************************************/

void
basic_environment_rep::raw_insert (int key, const tree& val) {
  //cout << "Raw insert " << key << ", " << val << "\n";
  int h= key & (n-1), f= free;
  free      = a[f].next;
  a[f].key  = key;
  a[f].val  = val;
  a[f].next = a[h].start;
  a[h].start= f;
  size++;
}

void
basic_environment_rep::raw_write (int key, const tree& val) {
  //cout << "Raw write " << key << ", " << val << "\n";
  int h= key & (n-1), i= a[h].start;
  while (i >= 0) {
    if (a[i].key == key) {
      a[i].val= val;
      return;
    }
    i= a[i].next;
  }
  int f= free;
  free      = a[f].next;
  a[f].key  = key;
  a[f].val  = val;
  a[f].next = a[h].start;
  a[h].start= f;
  size++;
}

tree*
basic_environment_rep::raw_read (int key) {
  //cout << "Raw read " << key << "\n";
  int h= key & (n-1), i= a[h].start;
  while (i >= 0) {
    if (a[i].key == key)
      return & (a[i].val);
    i= a[i].next;
  }
  return NULL;
}

void
basic_environment_rep::raw_remove (int key) {
  //cout << "Raw remove " << key << "\n";
  int h= key & (n-1), i= a[h].start, p= -1;
  while (i >= 0) {
    if (a[i].key == key) {
      a[i].val= uninit;
      if (p >= 0) a[p].next= a[i].next;
      else a[h].start= a[i].next;
      a[i].next= free;
      free= i;
      size--;
      return;
    }
    p= i;
    i= a[i].next;
  }
}

/******************************************************************************
* Methods for multiple accesses
******************************************************************************/

void
basic_environment_rep::multiple_insert (hash_node* b, int k) {
  for (int h=0; h<k; h++) {
    int i= b[h].start;
    while (i >= 0) {
      raw_insert (b[i].key, b[i].val);
      i= b[i].next;
    }
  }
}

void
basic_environment_rep::multiple_write (hash_node* b, int k) {
  for (int h=0; h<k; h++) {
    int i= b[h].start;
    while (i >= 0) {
      raw_write (b[i].key, b[i].val);
      i= b[i].next;
    }
  }
}

void
basic_environment_rep::multiple_remove (hash_node* b, int k) {
  for (int h=0; h<k; h++) {
    int i= b[h].start;
    while (i >= 0) {
      raw_remove (b[i].key);
      i= b[i].next;
    }
  }
}

void
basic_environment_rep::resize (int new_n) {
  //cout << "Resize " << n << " -> " << new_n << " (" << size << ")\n";
  if (new_n < size)
    fatal_error ("too small number of bags",
		 "basic_environment_rep::resize");
  int old_n= n;
  hash_node* old_a= a;
  n= new_n;
  a= new hash_node[n];
  size= 0;
  free= 0;
  for (int i=0; i<n; i++)
    a[i].next= i+1;
  multiple_insert (old_a, old_n);
}

/******************************************************************************
* Printing
******************************************************************************/

void
basic_environment_rep::print (const string& prefix) {
  cout << prefix << "Basic environment" << LF;
  for (int h=0; h<n; h++) {
    int i= a[h].start;
    while (i >= 0) {
      cout << prefix << "| " << a[i].key
	   << ": " << as_string ((tree_label) a[i].key)
	   << " -> " << a[i].val << LF;
      i= a[i].next;
    }
  }
}
