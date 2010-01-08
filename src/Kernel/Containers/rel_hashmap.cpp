
/******************************************************************************
* MODULE     : rel_hashmap.cpp
* DESCRIPTION: Relative hashmaps are lists of hashmaps
*              the value of an entry is the value relative to
*              the last hashmap in the list for which the value is defined.
*              Relative hashmaps can be used to perform small local
*              changes on a hashmap, which may be discarded or
*              confirmed afterwards.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef REL_HASHMAP_CC
#define REL_HASHMAP_CC
#include "rel_hashmap.hpp"

template <class T, class U> U
rel_hashmap<T,U>::operator [] (T x) {
  ASSERT (rep != NULL, "invalid relative hashmap");
  if (rep->item->contains (x) || is_nil (rep->next)) return rep->item [x];
  return rep->next [x];
}

template <class T, class U> U&
rel_hashmap<T,U>::operator () (T x) {
  ASSERT (rep != NULL, "invalid relative hashmap");
  if (rep->item->contains (x)) return rep->item (x);
  if ((!is_nil (rep->next)) && rep->next->contains (x))
    rep->item(x)= copy (rep->next[x]);
  return rep->item (x);
}

template <class T, class U> bool
rel_hashmap_rep<T,U>::contains (T x) {
  if (item->contains (x)) return true;
  if (is_nil (next)) return false;
  return next->contains (x);
}

template <class T, class U> void
rel_hashmap_rep<T,U>::extend () {
  next= rel_hashmap<T,U> (item, next);
  item= hashmap<T,U> (item->init);
}

template <class T, class U> void
rel_hashmap_rep<T,U>::shorten () {
  ASSERT (!is_nil (next), "relative hashmap cannot be shortened");
  item= next->item;
  next= next->next;
}

template <class T, class U> void
rel_hashmap_rep<T,U>::merge () {
  ASSERT (!is_nil (next), "relative hashmap cannot be merged");
  next->change (item);
  shorten ();
}

template <class T, class U> void
rel_hashmap_rep<T,U>::find_changes (hashmap<T,U>& CH) {
  int i;
  rel_hashmap<T,U> h (item, next);
  list<hashentry<T,U> > remove;
  for (i=0; i<CH->n; i++) {
    list<hashentry<T,U> > l (CH->a[i]);
    while (!is_nil (l)) {
      if (h [l->item.key] == l->item.im)
	remove= list<hashentry<T,U> > (l->item, remove);
      l=l->next;
    }
  }
  while (!is_nil (remove)) {
    CH->reset (remove->item.key);
    remove= remove->next;
  }
}

template <class T, class U> void
rel_hashmap_rep<T,U>::find_differences (hashmap<T,U>& CH) {
  int i;
  list<hashentry<T,U> > add;
  for (i=0; i<item->n; i++) {
    list<hashentry<T,U> > l (item->a[i]);
    while (!is_nil (l)) {
      if (!CH->contains (l->item.key))
	add= list<hashentry<T,U> > (l->item, add);
      l=l->next;
    }
  }
  while (!is_nil (add)) {
    CH (add->item.key)= next [add->item.key];
    add= add->next;
  }
  find_changes (CH);
}

template <class T, class U> void
rel_hashmap_rep<T,U>::change (hashmap<T,U> CH) {
  int i;
  for (i=0; i<CH->n; i++) {
    list<hashentry<T,U> > l (CH->a[i]);
    while (!is_nil (l)) {
      item (l->item.key)= l->item.im;
      l=l->next;
    }
  }
}

template <class T, class U> tm_ostream&
operator << (tm_ostream& out, rel_hashmap<T,U> H) {
  if (is_nil (H)) out << "(null)";
  else {
    while (!is_nil (H->next)) {
      out << H->item << LF;
      out << HRULE << LF;
      H= H->next;
    }
    out << H->item << LF;
  }
  return out;
}

#endif // defined REL_HASHMAP_CC
