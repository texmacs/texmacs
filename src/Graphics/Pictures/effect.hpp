
/******************************************************************************
* MODULE     : effect.hpp
* DESCRIPTION: Graphical effects
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef EFFECT_H
#define EFFECT_H
#include "picture.hpp"
#include "rectangles.hpp"

/******************************************************************************
* The abstract effect class
******************************************************************************/

class effect_rep;
class effect {
ABSTRACT_NULL(effect);
};

class effect_rep: public abstract_struct {
public:
  inline effect_rep () {}
  inline virtual ~effect_rep () {}

  virtual rectangle get_extents (array<rectangle> rs) = 0;
  virtual picture apply (array<picture> pics, SI pixel) = 0;

  friend class effect;
};

ABSTRACT_NULL_CODE(effect);

/******************************************************************************
* Standard effects
******************************************************************************/

effect build_effect (tree description);
effect argument_effect (int arg);

effect move (effect eff, double dx, double dy);
effect magnify (effect eff, double sx, double sy);
effect bubble (effect eff, double r, double a);

effect set_color (effect eff, color c, color mask);
effect make_transparent (effect eff, color bgc);
effect make_opaque (effect eff, color bgc);
effect change_color (effect eff, color what, color by);

effect gaussian_brush (double r);
effect oval_brush (double r);
effect rectangular_brush (double r);
effect gaussian_brush (double rx, double ry, double phi= 0);
effect oval_brush (double rx, double ry, double phi= 0);
effect rectangular_brush (double rx, double ry, double phi= 0);

effect blur (effect eff, effect brush);
effect outline (effect eff, effect brush);
effect thicken (effect eff, effect brush);

effect superpose (array<effect> effs);
effect mix (array<effect> effs, array<double> alphas);
effect mul (array<effect> effs);
effect min (array<effect> effs);
effect max (array<effect> effs);

#endif // defined EFFECT_H
