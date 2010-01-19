
/******************************************************************************
* MODULE     : superpose.cpp
* DESCRIPTION: Superpositions of arrays of boxes
* COPYRIGHT  : (C) 2005  Henri Lesourd
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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
  (void) item; (void) n;
  return i;
}

/******************************************************************************
* User interface
******************************************************************************/

box
superpose_box (path ip, array<box> bs, bool bfl) {
  return tm_new<superpose_box_rep> (ip, bs, bfl);
}
