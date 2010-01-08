
/******************************************************************************
* MODULE     : vpenalty.cpp
* DESCRIPTION: Vertical penalties
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "vpenalty.hpp"

tm_ostream&
operator << (tm_ostream& out, vpenalty pen) {
  return out << "[ " << pen->pen << ", " << pen->exc << " ]";
}

vpenalty
as_vpenalty (SI diff) {
  if (diff < 0) diff= -diff;
  if (diff < 0x1000) return vpenalty (0, (diff*diff) >> 16);
  else if (diff < 0x100000) return vpenalty (0, (diff >> 8) * (diff >> 8));
  else return vpenalty (0, 0x1000000);
}
