
/******************************************************************************
* MODULE     : hashfunc.cpp
* DESCRIPTION: fixed size hashfuncs with reference counting
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef HASHFUNC_CC
#define HASHFUNC_CC
#include "hashfunc.hpp"

template<class T, class U> U
hashfunc_rep<T,U>::apply (T x) {
  if (remember->contains (x)) return remember[x];
  U y= func (x);
  remember(x)= y;
  return y;
}

#endif // defined HASHFUNC_CC
