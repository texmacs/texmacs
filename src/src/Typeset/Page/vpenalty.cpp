
/******************************************************************************
* MODULE     : vpenalty.cpp
* DESCRIPTION: Vertical penalties
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "vpenalty.hpp"

ostream&
operator << (ostream& out, vpenalty pen) {
  return out << "[ " << pen->pen << ", " << pen->exc << " ]";
}

vpenalty
as_vpenalty (SI diff) {
  if (diff < 0) diff= -diff;
  if (diff < 0x1000) return vpenalty (0, (diff*diff) >> 16);
  else if (diff < 0x100000) return vpenalty (0, (diff >> 8) * (diff >> 8));
  else return vpenalty (0, 0x1000000);
}
