
/******************************************************************************
* MODULE     : iterator.hpp
* DESCRIPTION: dynamic iterators
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef ITERATOR_H
#define ITERATOR_H
#include "hashset.hpp"
#include "hashmap.hpp"

extern int iterator_count;

template<class T> class iterator_rep: public abstract_struct {
public:
  inline iterator_rep<T> () { DEBUG(iterator_count++); }
  inline virtual ~iterator_rep<T> () { DEBUG(iterator_count--); }
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
