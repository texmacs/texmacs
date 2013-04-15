
/******************************************************************************
* MODULE     : transformed.cpp
* DESCRIPTION: Transformed spacial vector graphics
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "spacial.hpp"
#include "renderer.hpp"

/******************************************************************************
* The abstract spacial class
******************************************************************************/

class transformed_rep: public spacial_rep {
  spacial obj;
  matrix<double> m;
  spacial tobj;

public:
  transformed_rep (spacial obj2, matrix<double> m2):
    obj (obj2), m (m2) {}

  spacial_kind get_type () { return spacial_transformed; }
  void* get_handle () { return (void*) this; }

  rectangle get_extents ();
  void draw (renderer ren);
  spacial transform (matrix<double> m);
  spacial enlighten (tree light);
};

spacial
transformed (spacial obj, matrix<double> m) {
  return tm_new<transformed_rep> (obj, m);
}

/******************************************************************************
* Extents, drawing and transforming
******************************************************************************/

rectangle
transformed_rep::get_extents () {
  if (is_nil (tobj)) tobj= obj->transform (m);
  return tobj->get_extents ();
}

void
transformed_rep::draw (renderer ren) {
  if (is_nil (tobj)) tobj= obj->transform (m);
  tobj->draw (ren);
}

spacial
transformed_rep::transform (matrix<double> m2) {
  return transformed (obj, m2 * m);
}

spacial
transformed_rep::enlighten (tree light) {
  if (is_nil (tobj)) tobj= obj->transform (m);
  return tobj->enlighten (light);
}
