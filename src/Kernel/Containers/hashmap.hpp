
/******************************************************************************
* MODULE     : hashmap.hpp
* DESCRIPTION: fixed size hashmaps with reference counting
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef HASHMAP_H
#define HASHMAP_H
#include "list.hpp"

class tree;
template<class T> class list;
template<class T,class U> class hashmap;
template<class T,class U> class rel_hashmap;
template<class T,class U> class rel_hashmap_rep;
template<class T,class U> class hashmap_iterator_rep;

template<class T,class U> int N (hashmap<T,U> a);
template<class T,class U> tm_ostream& operator << (tm_ostream& out, hashmap<T,U> h);
template<class T,class U> hashmap<T,U> copy (hashmap<T,U> h);
template<class T,class U> hashmap<T,U> changes (hashmap<T,U> p,hashmap<T,U> b);
template<class T,class U> hashmap<T,U> invert (hashmap<T,U> p, hashmap<T,U> b);
template<class T,class U> bool operator == (hashmap<T,U> h1, hashmap<T,U> h2);
template<class T,class U> bool operator != (hashmap<T,U> h1, hashmap<T,U> h2);

template<class T, class U> struct hashentry {
  int code;
  T key;
  U im;
  hashentry<T,U> () { }
  hashentry<T,U> (int code, T key2, U im2);
  operator tree ();
};

template<class T, class U> class hashmap_rep: concrete_struct {
  int size;                  // size of hashmap (nr of entries)
  int n;                     // nr of keys (a power of two)
  int max;                   // mean number of entries per key
  U   init;                  // default entry
  list<hashentry<T,U> >* a;  // the array of entries

public:
  inline hashmap_rep<T,U>(U init2, int n2=1, int max2=1):
    size(0), n(n2), max(max2), init(init2), a(tm_new_array<list<hashentry<T,U> > > (n)) {}
  inline ~hashmap_rep<T,U> () { tm_delete_array (a); }
  void resize (int n);
  void reset (T x);
  void generate (void (*routine) (T));
  bool contains (T x);
  bool empty ();
  U    bracket_ro (T x);
  U&   bracket_rw (T x);
  void join (hashmap<T,U> H);

  friend class hashmap<T,U>;
  friend class rel_hashmap<T,U>;
  friend class rel_hashmap_rep<T,U>;
  friend class hashmap_iterator_rep<T,U>;
  friend int N LESSGTR (hashmap<T,U> h);
  friend tm_ostream& operator << LESSGTR (tm_ostream& out, hashmap<T,U> h);

  // only for hashmap<string,tree>
  void write_back (T x, hashmap<T,U> base);
  void pre_patch (hashmap<T,U> patch, hashmap<T,U> base);
  void post_patch (hashmap<T,U> patch, hashmap<T,U> base);
  friend hashmap<T,U> copy LESSGTR (hashmap<T,U> h);
  friend hashmap<T,U> changes LESSGTR (hashmap<T,U> patch, hashmap<T,U> base);
  friend hashmap<T,U> invert LESSGTR (hashmap<T,U> patch, hashmap<T,U> base);
  friend class edit_env_rep; // FIXME: broken encapsulation
  // end only for hashmap<string,tree>

  friend bool operator == LESSGTR (hashmap<T,U> h1, hashmap<T,U> h2);
  friend bool operator != LESSGTR (hashmap<T,U> h1, hashmap<T,U> h2);
};

template<class T, class U> class hashmap {
CONCRETE_TEMPLATE_2(hashmap,T,U);
  static hashmap<T,U> init;
  inline hashmap ():
    rep (tm_new<hashmap_rep<T,U> > (type_helper<U>::init, 1, 1)) {}
  inline hashmap (U init, int n=1, int max=1):
    rep (tm_new<hashmap_rep<T,U> > (init, n, max)) {}
  // only for hashmap<string,tree>
  hashmap (U init, tree t);
  // end only for hashmap<string,tree>
  inline U  operator [] (T x) { return rep->bracket_ro (x); }
  inline U& operator () (T x) { return rep->bracket_rw (x); }
  operator tree ();
};
CONCRETE_TEMPLATE_2_CODE(hashmap,class,T,class,U);

#define TMPL template<class T, class U>
TMPL inline int N (hashmap<T,U> h) { return h->size; }
TMPL hashmap<T,U> changes (hashmap<T,U> patch, hashmap<T,U> base);
TMPL hashmap<T,U> invert (hashmap<T,U> patch, hashmap<T,U> base);
#undef TMPL

#include "hashmap.cpp"
#include "hashmap_extra.cpp"

#endif // defined HASHMAP_H
