
/******************************************************************************
* MODULE     : hashfunc.cpp
* DESCRIPTION: fixed size hashfuncs with reference counting
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
