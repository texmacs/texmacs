
/******************************************************************************
* MODULE     : list.cpp
* DESCRIPTION: linked lists with reference counting
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef LIST_CC
#define LIST_CC
#include "list.hpp"

/******************************************************************************
* output and convertion
******************************************************************************/

template<class T> ostream&
operator << (ostream& out, list<T> l) {
  out << "[";
  if (!nil (l)) {
    out << " " << l->item;
    l=l->next;
  }
  while (!nil (l)) {
    out << ", " << l->item;
    l=l->next;
  }
  return out << " ]";
}

template<class T> T&
list<T>::operator [] (int i) {
  if (rep==NULL)
    fatal_error ("list too short", "list<T>::operator []", "list.cpp");
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
  if (nil (l)) l= list<T> (item, list<T> ());
  else l->next << item;
  return l;
}

template<class T> list<T>&
operator << (list<T>& l1, list<T> l2) {
  if (nil (l1)) l1= l2;
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

template<class T> list<T>&
suppress_last (list<T>& l) {
  if (nil (l)) fatal_error ("empty path", "last_item", "list.cpp");
  if (nil (l->next)) l= list<T> ();
  else suppress_last (l->next);
  return l;
}

template<class T> T&
last_item (list<T> l) {
  if (nil (l)) fatal_error ("empty path", "last_item", "list.cpp");
  if (nil (l->next)) return l->item;
  return last_item (l->next);
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
  if (nil (l1) || nil (l2)) return (nil (l1) == nil (l2));
  return (l1->item==l2->item) && (l1->next==l2->next);
}

template<class T> bool
operator != (list<T> l1, list<T> l2) {
  if (nil (l1) || nil (l2)) return (nil (l1) != nil (l2));
  return (l1->item!=l2->item) || (l1->next!=l2->next);
}

template<class T> bool
operator < (list<T> l1, list<T> l2) {
  if (nil (l1) || nil (l2)) return !nil (l2);
  return (l1->item==l2->item) && (l1->next<l2->next);
}

template<class T> bool
operator <= (list<T> l1, list<T> l2) {
  if (nil (l1) || nil (l2)) return nil (l1);
  return (l1->item==l2->item) && (l1->next<=l2->next);
}

/******************************************************************************
* computations with list<T> structures
******************************************************************************/

template<class T> int
N (list<T> l) {
  if (nil (l)) return 0;
  else return N (l->next) + 1;
}

template<class T> list<T>
copy (list<T> l) {
  if (nil (l)) return list<T> ();
  else return list<T> (l->item, copy (l->next));
}

template<class T> list<T>
operator * (list<T> l1, T x) {
  if (nil (l1)) return x;
  else return list<T> (l1->item, l1->next * x);
}

template<class T> list<T>
operator * (list<T> l1, list<T> l2) {
  if (nil (l1)) return copy (l2);
  else return list<T> (l1->item, l1->next * l2);
}

template<class T> list<T>
head (list<T> l, int n) {
  if (n==0) return list<T> ();
  if (nil (l)) fatal_error ("list too short", "head", "list.cpp");
  return list<T> (l->item, head (l->next, n-1));
}

template<class T> list<T>
tail (list<T> l, int n) {
  for (; n>0; n--) {
    if (nil (l)) fatal_error ("list too short", "tail", "list.cpp");
    l=l->next;
  }
  return l;
}

template<class T> list<T>
reverse (list<T> l) {
  list<T> r;
  while (!nil(l)) {
    r= list<T> (l->item, r);
    l=l->next;
  }
  return r;
}

#endif // defined LIST_CC
