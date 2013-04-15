
/******************************************************************************
* MODULE     : spacial.hpp
* DESCRIPTION: Abstract 3D objects
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SPACIAL_H
#define SPACIAL_H
#include "tree.hpp"
#include "rectangles.hpp"
#include "point.hpp"
#include "matrix.hpp"

class renderer_rep;
typedef renderer_rep* renderer;
typedef array<point> triangle;

/******************************************************************************
* The abstract spacial class
******************************************************************************/

enum spacial_kind {
  spacial_triangulated,
  spacial_transformed,
  spacial_enlightened
};

class spacial_rep;
class spacial {
ABSTRACT_NULL(spacial);
};

class spacial_rep: public abstract_struct {
public:
  inline spacial_rep () {}
  inline virtual ~spacial_rep () {}

  virtual spacial_kind get_type () = 0;
  virtual void* get_handle () = 0;

  virtual rectangle get_extents () = 0;
  virtual void draw (renderer ren) = 0;
  virtual spacial transform (matrix<double> m) = 0;
  virtual spacial enlighten (tree light) = 0;
};

ABSTRACT_NULL_CODE(spacial);

spacial triangulated (array<triangle> ts, array<color> cs);
spacial transformed (spacial obj, matrix<double> m);
spacial enlightened (spacial obj, tree light);

#endif // defined SPACIAL_H
