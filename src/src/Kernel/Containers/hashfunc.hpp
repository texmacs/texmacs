
/******************************************************************************
* MODULE     : hashfunc.hpp
* DESCRIPTION: functions with a memorizing hash table
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef HASHFUNC_H
#define HASHFUNC_H
#include "hashmap.hpp"

template<class T, class U> class hashfunc_rep: public concrete_struct {
  U (*func) (T);          // the function
  hashmap<T,U> remember;  // remembered values
public:
  inline hashfunc_rep (U (*func2) (T), U init):
    func (func2), remember (init) {}
  U apply (T x);
};

template<class T, class U> class hashfunc {
  CONCRETE_TEMPLATE_2(hashfunc,T,U);
  inline hashfunc (U (*func) (T), U init):
    rep (new hashfunc_rep<T,U> (func, init)) {}
  inline U operator [] (T x) { return rep->apply (x); }
  operator tree ();
};
CONCRETE_TEMPLATE_2_CODE(hashfunc,class,T,class,U);

#include "hashfunc.cpp"

#endif // defined HASHFUNC_H
