
/******************************************************************************
* MODULE     : list.cpp
* DESCRIPTION: linked lists with reference counting
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef LIST_CC
#define LIST_CC
#include "list.hpp"

/******************************************************************************
* output and convertion
******************************************************************************/

template<class T> tm_ostream&
operator << (tm_ostream& out, list<T> l) {
  out << "[";
  if (!is_nil (l)) {
    out << " " << l->item;
    l=l->next;
  }
  while (!is_nil (l)) {
    out << ", " << l->item;
    l=l->next;
  }
  return out << " ]";
}

template<class T> T&
list<T>::operator [] (int i) {
  ASSERT (rep != NULL, "list too short");
  if (i==0) return rep->item;
  return rep->next[i-1];
}

template<class T> list<T>::operator tree () {
  list<T> l;
  int i, n=N(*this);
  tree t (TUPLE, n);
  for (i=0, l=*this; i<n; i++, l=l->next)
    t[i]= as_tree (l->item);
  return t;
}

/******************************************************************************
* insertion and suppression
******************************************************************************/

template<class T> list<T>&
operator << (list<T>& l, T item) {
  if (is_nil (l)) l= list<T> (item, list<T> ());
  else l->next << item;
  return l;
}

template<class T> list<T>&
operator << (list<T>& l1, list<T> l2) {
  if (is_nil (l1)) l1= l2;
  else l1->next << l2;
  return l1;
}

template<class T> list<T>&
operator >> (T item, list<T>& l) {
  return (l= list<T> (item, l));
}

template<class T> list<T>&
operator << (T& item, list<T>& l) {
  item= l->item;
  l   = l->next;
  return l;
}

template<class T> T
last_item (list<T> l) {
  ASSERT (!is_nil (l), "empty path");
  if (is_nil (l->next)) return l->item;
  return last_item (l->next);
}

template<class T> T&
access_last (list<T>& l) {
  ASSERT (!is_nil (l), "empty path");
  if (is_nil (l->next)) return l->item;
  return access_last (l->next);
}

template<class T> list<T>&
suppress_last (list<T>& l) {
  ASSERT (!is_nil (l), "empty path");
  if (is_nil (l->next)) l= list<T> ();
  else suppress_last (l->next);
  return l;
}

/******************************************************************************
* tests
******************************************************************************/

template<class T> bool
strong_equal (list<T> l1, list<T> l2) {
  return l1.rep == l2.rep;
}

template<class T> bool
operator == (list<T> l1, list<T> l2) {
  if (is_nil (l1) || is_nil (l2)) return (is_nil (l1) == is_nil (l2));
  return (l1->item==l2->item) && (l1->next==l2->next);
}

template<class T> bool
operator != (list<T> l1, list<T> l2) {
  if (is_nil (l1) || is_nil (l2)) return (is_nil (l1) != is_nil (l2));
  return (l1->item!=l2->item) || (l1->next!=l2->next);
}

template<class T> bool
operator < (list<T> l1, list<T> l2) {
  if (is_nil (l1) || is_nil (l2)) return !is_nil (l2);
  return (l1->item==l2->item) && (l1->next<l2->next);
}

template<class T> bool
operator <= (list<T> l1, list<T> l2) {
  if (is_nil (l1) || is_nil (l2)) return is_nil (l1);
  return (l1->item==l2->item) && (l1->next<=l2->next);
}

/******************************************************************************
* computations with list<T> structures
******************************************************************************/

template<class T> int
N (list<T> l) {
  if (is_nil (l)) return 0;
  else if (is_nil (l->next)) return 1;
  else if (is_nil (l->next->next)) return 2;
  else if (is_nil (l->next->next->next)) return 3;
  else if (is_nil (l->next->next->next->next)) return 4;
  else {
    register int i= 4;
    list<T> iter = l->next->next->next->next;
    while (!is_nil (iter)) {
      iter= iter->next;
      i= i+1;
    }
    return i;
  }
}

template<class T> list<T>
copy (list<T> l) {
  if (is_nil (l)) return list<T> ();
  else return list<T> (l->item, copy (l->next));
}

template<class T> list<T>
operator * (list<T> l1, T x) {
  if (is_nil (l1)) return x;
  else return list<T> (l1->item, l1->next * x);
}

template<class T> list<T>
operator * (list<T> l1, list<T> l2) {
  if (is_nil (l1)) return copy (l2);
  else return list<T> (l1->item, l1->next * l2);
}

template<class T> list<T>
head (list<T> l, int n) {
  if (n==0) return list<T> ();
  ASSERT (!is_nil (l), "list too short to get the head");
  return list<T> (l->item, head (l->next, n-1));
}

template<class T> list<T>
tail (list<T> l, int n) {
  for (; n>0; n--) {
    ASSERT (!is_nil (l), "list too short to get the tail");
    l=l->next;
  }
  return l;
}

template<class T> list<T>
reverse (list<T> l) {
  list<T> r;
  while (!is_nil(l)) {
    r= list<T> (l->item, r);
    l=l->next;
  }
  return r;
}

template<class T> list<T>
remove (list<T> l, T what) {
  if (is_nil (l)) return l;
  else if (l->item == what) return remove (l->next, what);
  else return list<T> (l->item, remove (l->next, what));
}

template<class T> bool
contains (list<T> l, T what) {
  return (!is_nil(l) && (l->item == what || contains(l->next, what)));
}

#endif // defined LIST_CC
