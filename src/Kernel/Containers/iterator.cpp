
/******************************************************************************
* MODULE     : iterator.cpp
* DESCRIPTION: dynamic iterators
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef ITERATOR_CC
#define ITERATOR_CC
#include "hashmap.hpp"
#include "hashset.hpp"
#include "iterator.hpp"

template<class T> int
iterator_rep<T>::remains () {
  if (busy ()) return -1;
  else return 0;
}

template<class T>
iterator<T>::operator tree () {
  tree t (TUPLE);
  while (this->rep->busy ()) {
    t << as_tree (this->rep->next());
  }
  return t;
}

template<class T> tm_ostream&
operator << (tm_ostream& out, iterator<T> it) {
  bool flag=false;
  out << "[ ";
  while (it->busy ()) {
    if (flag) out << ", ";
    else flag= true;
    out << it->next ();
  }
  return out << " ]";
}

//hashset_iterator
template<class T>
class hashset_iterator_rep: public iterator_rep<T> {
  hashset<T> h;
  int i;
  list<T> l;
  void spool ();

public:
  hashset_iterator_rep<T> (hashset<T> h);
  bool busy ();
  T next ();
};

template<class T>
hashset_iterator_rep<T>::hashset_iterator_rep (hashset<T> h2):
  h (h2), i (0), l (h2->a[0]) {}

template<class T> void
hashset_iterator_rep<T>::spool () {
  if (i >= h->n) return;
  while (is_nil (l)) {
    if ((++i) >= h->n) return;
    l= h->a[i];
  }
}

template<class T> bool
hashset_iterator_rep<T>::busy () {
  spool ();
  return i < h->n;
}

template<class T> T
hashset_iterator_rep<T>::next () {
  ASSERT (busy (), "end of iterator");
  T x (l->item);
  l= l->next;
  return x;
}

template<class T> iterator<T>
iterate (hashset<T> h) {
  return tm_new<hashset_iterator_rep<T> > (h);
}
// end hashset_iterator

// hashmap_iterator
template<class T, class U>
class hashmap_iterator_rep: public iterator_rep<T> {
  hashmap<T,U> h;
  int i;
  list<hashentry<T,U> > l;
  void spool ();

public:
  hashmap_iterator_rep (hashmap<T,U> h);
  bool busy ();
  T next ();
};

template<class T, class U>
hashmap_iterator_rep<T,U>::hashmap_iterator_rep (hashmap<T,U> h2):
  h (h2), i (0), l (h2->a[0]) {}

template<class T, class U> void
hashmap_iterator_rep<T,U>::spool () {
  if (i >= h->n) return;
  while (is_nil (l)) {
    if ((++i) >= h->n) return;
    l= h->a[i];
  }
}

template<class T, class U> bool
hashmap_iterator_rep<T,U>::busy () {
  spool ();
  return i < h->n;
}

template<class T, class U> T
hashmap_iterator_rep<T,U>::next () {
  ASSERT (busy (), "end of iterator");
  T x (l->item.key);
  l= l->next;
  return x;
}

template<class T, class U> iterator<T>
iterate (hashmap<T,U> h) {
  return tm_new<hashmap_iterator_rep<T,U> > (h);
}
// hashmap_iterator

#endif // defined ITERATOR_CC
