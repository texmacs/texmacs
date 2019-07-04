
/******************************************************************************
* MODULE     : scalable.hpp
* DESCRIPTION: Abstract scalable vector graphics
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SCALABLE_H
#define SCALABLE_H
#include "url.hpp"
#include "rectangles.hpp"

class renderer_rep;
typedef renderer_rep* renderer;

/******************************************************************************
* The abstract scalable class
******************************************************************************/

enum scalable_kind {
  scalable_native,
  scalable_image
};

class scalable_rep;
class scalable {
ABSTRACT_NULL(scalable);
};

class scalable_rep: public abstract_struct {
public:
  inline scalable_rep () {}
  inline virtual ~scalable_rep () {}

  virtual scalable_kind get_type () = 0;
  virtual void* get_handle () = 0;
  virtual url get_name ();
  virtual tree get_effect ();

  virtual rectangle get_logical_extents () = 0;
  virtual rectangle get_physical_extents () = 0;
  virtual void draw (renderer ren, SI x, SI y, int alpha= 255) = 0;
};

ABSTRACT_NULL_CODE(scalable);

scalable load_scalable_image (url file_name, SI w, SI h, tree eff, SI pixel);

#endif // defined SCALABLE_H
