
/******************************************************************************
* MODULE     : iterator.hpp
* DESCRIPTION: dynamic iterators
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef ITERATOR_H
#define ITERATOR_H
#include "hashset.hpp"
#include "hashmap.hpp"

extern int iterator_count;

template<class T> class iterator_rep: public abstract_struct {
public:
  inline iterator_rep<T> () { TM_DEBUG(iterator_count++); }
  inline virtual ~iterator_rep<T> () { TM_DEBUG(iterator_count--); }
  virtual bool busy () = 0;
  virtual T next () = 0;
  virtual int remains ();
};

template<class T> struct iterator {
  ABSTRACT_TEMPLATE(iterator,T);
  operator tree ();
};
ABSTRACT_TEMPLATE_CODE(iterator,class,T);

template<class T> ostream& operator << (ostream& out, iterator<T> it);

template<class T, class U> iterator<T> iterate (hashmap<T,U> h);
template<class T> iterator<T> iterate (hashset<T> h);

#include "iterator.cpp"

#endif // defined ITERATOR_H
