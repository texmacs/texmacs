
/******************************************************************************
* MODULE     : space.cpp
* DESCRIPTION: spacing
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "space.hpp"

/******************************************************************************
* Constructors
******************************************************************************/

space_rep::space_rep (SI min2, SI def2, SI max2) {
  min= min2;
  def= def2;
  max= max2;
}

space_rep::space_rep (SI def2) {
  min= def= max= def2;
}

space::space (SI min, SI def, SI max) {
  rep= tm_new<space_rep> (min, def, max);
}

space::space (SI def) {
  rep= tm_new<space_rep> (def);
}

space::operator tree () {
  return tree (TUPLE,
	       as_string (rep->min),
	       as_string (rep->def),
	       as_string (rep->max));
}

/******************************************************************************
* The routines which are provided
******************************************************************************/

bool
operator == (space spc1, space spc2) {
  return
    (spc1->min == spc2->min) &&
    (spc1->def == spc2->def) &&
    (spc1->max == spc2->max);
}

bool
operator != (space spc1, space spc2) {
  return
    (spc1->min != spc2->min) ||
    (spc1->def != spc2->def) ||
    (spc1->max != spc2->max);
}

tm_ostream&
operator << (tm_ostream& out, space spc) {
  out << "[ " << spc->min << ", " << spc->def << ", " << spc->max << " ]";
  return out;
}

space
copy (space spc) {
  return space (spc->min, spc->def, spc->max);
}

space
operator + (space spc1, space spc2) {
  return space (spc1->min + spc2->min,
		spc1->def + spc2->def,
		spc1->max + spc2->max);
}

space
operator - (space spc1, space spc2) {
  return space (spc1->min - spc2->min,
		spc1->def - spc2->def,
		spc1->max - spc2->max);
}

space
operator * (int i, space spc) {
  return space (i*spc->min, i*spc->def, i*spc->max);
}

space
operator * (double x, space spc) {
  return space ((SI) (x*spc->min), (SI) (x*spc->def), (SI) (x*spc->max));
}

space
operator / (space spc, int i) {
  return space (spc->min/i, spc->def/i, spc->max/i);
}

space
operator / (space spc, double x) {
  return space ((SI) (spc->min/x), (SI) (spc->def/x), (SI) (spc->max/x));
}

space
max (space spc1, space spc2) {
  return space (max (spc1->min, spc2->min),
		max (spc1->def, spc2->def),
		max (spc1->max, spc2->max));
}
