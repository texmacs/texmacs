
/******************************************************************************
* MODULE     : hashset.cpp
* DESCRIPTION: fixed size hashsets with reference counting
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef HASHSET_CC
#define HASHSET_CC
#include "hashset.hpp"

template<class T> void
hashset_rep<T>::resize (int n2) {
  int i;
  int oldn= n;
  list<T>* olda= a;
  n= n2;
  a= tm_new_array<list<T> > (n);
  for (i=0; i<oldn; i++) {
    list<T> l(olda[i]);
    while (!is_nil(l)) {
      list<T>& newl= a[hash(l->item)&(n-1)];
      newl= list<T> (l->item, newl);
      l=l->next;
    }
  }
  tm_delete_array (olda);
}

template<class T> static T*
search (list<T> l, T x) {
  while (!is_nil (l)) {
    if (l->item==x) return &(l->item);
    l= l->next;
  }
  return NULL;
}

template<class T> bool
hashset_rep<T>::contains (T x) {
  return (search (a[hash(x)&(n-1)], x)==NULL? false: true);
}

template<class T> void
hashset_rep<T>::insert (T x) {
  if (size==n*max) resize (n << 1);
  list<T>& l= a[hash(x)&(n-1)];
  if (search (l, x) != NULL) return;
  l= list<T> (x, l);
  size ++;
}

template<class T> void
hashset_rep<T>::remove (T x) {
  list<T>* lptr= &a[hash(x)&(n-1)];
  while (!is_nil (*lptr)) {
    if ((*lptr)->item==x) {
      *lptr=(*lptr)->next;
      size --;
      return;
    }
    lptr=&((*lptr)->next);
  }
}

template<class T> bool
operator <= (hashset<T> h1, hashset<T> h2) {
  int i=0, j=0, n=h1->n;
  if (N(h1)>N(h2)) return false;
  for (; i<n; i++) {
    list<T> l=h1->a[i];
    for (; !is_nil (l); l=l->next, j++)
      if (!h2->contains (l->item)) return false;
  }
  return true;
}

template<class T> bool
operator < (hashset<T> h1, hashset<T> h2) {
  return (N(h1)<N(h2)) && (h1<=h2);
}

template<class T> bool
operator == (hashset<T> h1, hashset<T> h2) {
  return (N(h1)==N(h2)) && (h1<=h2);
}

template<class T> tm_ostream&
operator << (tm_ostream& out, hashset<T> h) {
  int i=0, j=0, n=h->n, size=h->size;
  out << "{ ";
  for (; i<n; i++) {
    list<T> l=h->a[i];
    for (; !is_nil (l); l=l->next, j++) {
      out << l->item;
      if (j!=size-1) out << ", ";
    }
  }
  out << " }";
  return out;
}

template<class T>
hashset<T>::operator tree () {
  int i=0, j=0, n=this->rep->n, size=this->rep->size;
  tree t (COLLECTION, size);
  for (; i<n; i++) {
    list<T> l=this->rep->a[i];
    for (; !is_nil (l); l=l->next, j++)
      t[j]= as_tree(l->item);
  }
  return t;
}

#endif // defined HASHSET_CC
