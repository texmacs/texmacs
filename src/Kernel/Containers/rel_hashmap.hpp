
/******************************************************************************
* MODULE     : rel_hashmap.hpp
* DESCRIPTION: see rel_hashmap.cpp
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef REL_HASMAP_H
#define REL_HASMAP_H
#include "hashmap.hpp"

template<class T, class U> class rel_hashmap;
template<class T, class U> class rel_hashmap_rep;
template<class T, class U> bool is_nil (rel_hashmap<T,U> h);

template<class T, class U> class rel_hashmap {
  CONCRETE_NULL_TEMPLATE_2(rel_hashmap,T,U);
  inline rel_hashmap (U init);
  inline rel_hashmap (hashmap<T,U> item);
  inline rel_hashmap (hashmap<T,U> item, rel_hashmap<T,U> next);
  U  operator [] (T x);
  U& operator () (T x);
};

template<class T, class U> class rel_hashmap_rep: concrete_struct {
public:
  hashmap<T,U>     item;
  rel_hashmap<T,U> next;

  inline rel_hashmap_rep<T,U> (hashmap<T,U> item2, rel_hashmap<T,U> next2):
    item(item2), next(next2) {}
  bool contains (T x);
  void extend ();
  void shorten ();
  void merge ();
  void find_changes (hashmap<T,U>& CH);
  void find_differences (hashmap<T,U>& CH);
  void change (hashmap<T,U> CH);

  friend class rel_hashmap<T,U>;
};

CONCRETE_NULL_TEMPLATE_2_CODE(rel_hashmap,class,T,class,U);
#define TMPL template<class T, class U>
TMPL inline rel_hashmap<T,U>::rel_hashmap (U init):
  rep (tm_new<rel_hashmap_rep<T,U> > (hashmap<T,U> (init), rel_hashmap<T,U> ())) {}
TMPL inline rel_hashmap<T,U>::rel_hashmap (hashmap<T,U> item):
  rep (tm_new<rel_hashmap_rep<T,U> > (item, rel_hashmap<T,U> ())) {}
TMPL inline rel_hashmap<T,U>::rel_hashmap
  (hashmap<T,U> item, rel_hashmap<T,U> next):
    rep (tm_new<rel_hashmap_rep<T,U> > (item, next)) {}

TMPL tm_ostream& operator << (tm_ostream& out, rel_hashmap<T,U> H);
#undef TMPL

#include "rel_hashmap.cpp"

#endif // defined REL_HASMAP_H
