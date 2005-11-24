
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
  int superpose_box_rep::find_child (SI x, SI y, SI delta, bool force);
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

int
superpose_box_rep::find_child (SI x, SI y, SI delta, bool force) {
  // FIXME: this is really a dirty hack. The problem is that the usual
  // composite_box::find_child thinks that frozen children are accessible.
  // Currently, there is no safe way to enforce unaccessability at
  // the physical box level.
  if (outside (x, delta, x1, x2) && (is_accessible (ip) || force)) return -1;
  int i, n= subnr(), d= MAX_SI, m= -1;
  for (i=0; i<n; i++) {
    if (distance (i, x, y, delta)< d)
      if (is_accessible (bs[i]->ip) || force) {
	d= distance (i, x, y, delta);
	m= i;
      }
  }
  return m;
}

/******************************************************************************
* User interface
******************************************************************************/

box
superpose_box (path ip, array<box> bs, bool bfl) {
  return new superpose_box_rep (ip, bs, bfl);
}
