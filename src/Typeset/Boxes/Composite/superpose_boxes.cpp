
/******************************************************************************
* MODULE     : superpose.cpp
* DESCRIPTION: Superpositions of arrays of boxes
* COPYRIGHT  : (C) 2005  Henri Lesourd
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Boxes/composite.hpp"
#include "Boxes/construct.hpp"

/******************************************************************************
* The superpose_box representation
******************************************************************************/

struct superpose_box_rep: public concrete_composite_box_rep {
  superpose_box_rep (path ip, array<box> bs, bool bfl):
    concrete_composite_box_rep (ip, bs, bfl) {}
  operator tree ();
  int reindex (int i, int item, int n);
};

superpose_box_rep::operator tree () {
  int i, n= N(bs);
  tree t (TUPLE, n+1);
  t[0]= "superpose";
  for (i=0; i<n; i++) t[i+1]= (tree) bs[i];
  return t;
}

int
superpose_box_rep::reindex (int i, int item, int n) {
  return i;
}

/******************************************************************************
* User interface
******************************************************************************/

box
superpose_box (path ip, array<box> bs, bool bfl) {
  return new superpose_box_rep (ip, bs, bfl);
}
