
/******************************************************************************
* MODULE     : transformation.hpp
* DESCRIPTION: linear or more general transformations on points
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TRANSFORMATION_H
#define TRANSFORMATION_H
#include "point.hpp"

class transformation_rep: public abstract_struct {
public:
  inline transformation_rep () {}
  inline virtual ~transformation_rep () {}

  virtual point direct_transform (point p) = 0;
  virtual point inverse_transform (point p) = 0;

  // Additional methods with bounds for the entries of
  // the Jacobian matrices at different points should be added later.
  // This should allow us to apply transformations on curves and such.
};

class transformation {
  ABSTRACT_NULL(transformation);
  transformation (array<double> magn); // dilatation (diagonal matrix)
  inline point operator () (point p) { return rep->direct_transform (p); }
  inline bool operator == (transformation c) { return rep == c.rep; }
  inline bool operator != (transformation c) { return rep != c.rep; }
};
ABSTRACT_NULL_CODE(transformation);

transformation operator * (transformation c1, transformation c2);
transformation invert (transformation c);

#endif // defined TRANSFORMATION_H
