
/******************************************************************************
* MODULE     : hashset.hpp
* DESCRIPTION: hashsets with reference counting
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef HASHSET_H
#define HASHSET_H
#include "list.hpp"

template<class T> class hashset;
template<class T> class hashset_iterator_rep;
template<class T> int N (hashset<T> h);
template<class T> tm_ostream& operator << (tm_ostream& out, hashset<T> h);
template<class T> bool operator <= (hashset<T> h1, hashset<T> h2);

template<class T> class hashset_rep: concrete_struct {
  int size;    // size of hashset (nr of entries)
  int n;       // nr of keys (a power of two)
  int max;     // mean number of entries per key
  list<T>* a;  // the array of entries

public:
  inline hashset_rep ():
    size(0), n(1), max(1), a (tm_new_array<list<T> > (1)) {}
  inline hashset_rep(int n2, int max2=1):
    size(0), n(n2), max(max2), a (tm_new_array<list<T> > (n)) {}
  inline ~hashset_rep () { tm_delete_array (a); }

  bool contains (T x);
  void resize (int n);
  void insert (T x);
  void remove (T x);

  friend class hashset<T>;
  friend int N LESSGTR (hashset<T> h);
  friend tm_ostream& operator << LESSGTR (tm_ostream& out, hashset<T> h);
  friend bool operator <= LESSGTR (hashset<T> h1, hashset<T> h2);
  friend class hashset_iterator_rep<T>;
};

template<class T> class hashset {
CONCRETE_TEMPLATE(hashset,T);
  inline hashset (int n=1, int max=1):
    rep (tm_new<hashset_rep<T> > (n, max)) {}
  operator tree ();
};
CONCRETE_TEMPLATE_CODE(hashset,class,T);

template<class T> inline int N (hashset<T> h) { return h->size; }
template<class T> bool operator == (hashset<T> h1, hashset<T> h2);
template<class T> bool operator <= (hashset<T> h1, hashset<T> h2);
template<class T> bool operator <  (hashset<T> h1, hashset<T> h2);

#include "hashset.cpp"

#endif // defined HASHSET_H
