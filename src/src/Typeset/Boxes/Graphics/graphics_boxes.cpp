
/******************************************************************************
* MODULE     : graphics.cpp
* DESCRIPTION: Boxes for graphics
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Boxes/composite.hpp"

/******************************************************************************
* Graphics boxes
******************************************************************************/

struct graphics_box_rep: public composite_box_rep {
  graphics_box_rep (path ip, array<box> bs, SI X1, SI Y1, SI X2, SI Y2):
    composite_box_rep (ip, bs) { x1= X1; y1= Y1; x2= X2; y2= Y2; }
  operator tree () { return "graphics"; }
};

/******************************************************************************
* User interface
******************************************************************************/

box
graphics_box (path ip, array<box> bs, SI x1, SI y1, SI x2, SI y2) {
  return new graphics_box_rep (ip, bs, x1, y1, x2, y2);
}
