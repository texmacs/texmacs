
/******************************************************************************
* MODULE     : enlightened.cpp
* DESCRIPTION: Enlightened spacial vector graphics
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

class enlightened_rep: public spacial_rep {
  spacial obj;
  tree light;
  spacial tobj;

public:
  enlightened_rep (spacial obj2, tree light2):
    obj (obj2), light (light2) {}

  spacial_kind get_type () { return spacial_enlightened; }
  void* get_handle () { return (void*) this; }

  rectangle get_extents ();
  void draw (renderer ren);
  spacial transform (matrix<double> m);
  spacial enlighten (tree light);
};

spacial
enlightened (spacial obj, tree light) {
  return tm_new<enlightened_rep> (obj, light);
}

/******************************************************************************
* Extents, drawing and transforming
******************************************************************************/

rectangle
enlightened_rep::get_extents () {
  return obj->get_extents ();
}

void
enlightened_rep::draw (renderer ren) {
  if (is_nil (tobj)) tobj= obj->enlighten (light);
  tobj->draw (ren);
}

spacial
enlightened_rep::transform (matrix<double> m) {
  if (is_nil (tobj)) tobj= obj->enlighten (light);
  return tobj->transform (m);
}

spacial
enlightened_rep::enlighten (tree light2) {
  if (is_nil (tobj)) tobj= obj->enlighten (light);
  return tobj->enlighten (light2);
}
