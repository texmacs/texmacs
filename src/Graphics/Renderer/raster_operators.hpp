
/******************************************************************************
* MODULE     : raster_operators.hpp
* DESCRIPTION: Templates for several raster operations
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef RASTER_OPERATORS_HPP
#define RASTER_OPERATORS_HPP
#include "operators.hpp"

/******************************************************************************
* Unary operators
******************************************************************************/

struct mul_alpha_op {
  template<typename C> static inline C
  op (const C& x) { return mul_alpha (x); }
};

struct div_alpha_op {
  template<typename C> static inline C
  op (const C& x) { return div_alpha (x); }
};

/******************************************************************************
* Composition operators
******************************************************************************/

struct source_over_op {
  template<typename C> static inline C
  op (const C& x, const C& y) { return source_over (x, y); }
};

struct towards_source_op {
  template<typename C> static inline C
  op (const C& x, const C& y) { return towards_source (x, y); }
};

#endif // RASTER_OPERATORS_H
