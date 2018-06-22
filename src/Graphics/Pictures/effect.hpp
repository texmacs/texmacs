
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

  virtual rectangle get_logical_extents (array<rectangle> rs);
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
effect turbulence (effect e, long seed, double w, double h, int oct);
effect fractal_noise (effect e, long seed, double w, double h, int oct);
effect hatch (effect eff, int sx, int sy, double fill_prop, double deform);

effect gaussian_pen (double r);
effect oval_pen (double r);
effect rectangular_pen (double r);
effect gaussian_pen (double rx, double ry, double phi= 0);
effect oval_pen (double rx, double ry, double phi= 0);
effect rectangular_pen (double rx, double ry, double phi= 0);
effect motion_pen (double dx, double dy);

effect blur (effect eff, effect pen);
effect outline (effect eff, effect pen);
effect thicken (effect eff, effect pen);
effect erode (effect eff, effect pen);

effect degrade (effect eff, double wx, double wy, double th, double sh);
effect distort (effect eff, double wx, double wy, double rx, double ry);
effect gnaw (effect eff, double wx, double wy, double rx, double ry);

effect compose (array<effect> effs, composition_mode mode);
effect superpose (array<effect> effs);
effect add (array<effect> effs);
effect sub (array<effect> effs);
effect mul (array<effect> effs);
effect min (array<effect> effs);
effect max (array<effect> effs);
effect mix (effect eff1, double a1, effect eff2, double a2);

effect normalize (effect eff);
effect color_matrix (effect eff, array<double> m);
effect make_transparent (effect eff, color bgc);
effect make_opaque (effect eff, color bgc);

#endif // defined EFFECT_H
