
/******************************************************************************
* MODULE     : alpha_color.hpp
* DESCRIPTION: transparencies represented by floating point numbers
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef ALPHA_COLOR_H
#define ALPHA_COLOR_H
#include "true_color.hpp"

typedef unsigned int color;

class alpha_color {
public:
  double a;
  inline alpha_color () {}
  inline alpha_color (double a2): a (a2) {}
  inline alpha_color (color c): a (((double) ((c >> 24) & 0xff)) / 255.0) {}
  inline operator color () const { return (((int) (a * 255 + 0.5)) << 24); }
};

inline tm_ostream&
operator << (tm_ostream& out, const alpha_color& c) {
  return out << c.a;
}

inline alpha_color
normalize (const alpha_color& c) {
  return alpha_color ((double) max (min (c.a, 1.0), 0.0));
}

inline alpha_color
operator + (const alpha_color& c1, const alpha_color& c2) {
  return alpha_color (c1.a + c2.a);
}

inline alpha_color
operator - (const alpha_color& c1, const alpha_color& c2) {
  return alpha_color (c1.a - c2.a);
}

inline alpha_color
operator * (double x, const alpha_color& c) {
  return alpha_color (c.a * x);
}

inline alpha_color
operator * (const alpha_color& c, double x) {
  return alpha_color (c.a * x);
}

inline alpha_color
operator / (const alpha_color& c, double x) {
  return alpha_color (c.a / x);
}

inline true_color
operator * (const alpha_color& x, const true_color& c) {
  return true_color (c.r * x.a, c.g * x.a, c.b * x.a, c.a * x.a);
}

inline true_color
operator * (const true_color& c, const alpha_color& x) {
  return true_color (c.r * x.a, c.g * x.a, c.b * x.a, c.a * x.a);
}

inline true_color
operator / (const true_color& c, const alpha_color& x) {
  return true_color (c.r / x.a, c.g / x.a, c.b / x.a, c.a / x.a);
}

#endif // defined ALPHA_COLOR_H
