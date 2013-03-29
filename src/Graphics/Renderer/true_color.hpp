
/******************************************************************************
* MODULE     : true_color.hpp
* DESCRIPTION: RGBA colors represented by floating point numbers
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TRUE_COLOR_H
#define TRUE_COLOR_H
#include "tree.hpp"

typedef unsigned int color;

class true_color {
public:
  float b;
  float g;
  float r;
  float a;

  inline true_color () {}
  inline true_color (float r2, float g2, float b2, float a2):
    b (b2), g (g2), r (r2), a (a2) {}
  inline true_color (color c):
    b (((float) (c & 0xff)) / 255.0),
    g (((float) ((c >> 8) & 0xff)) / 255.0),
    r (((float) ((c >> 16) & 0xff)) / 255.0),
    a (((float) ((c >> 24) & 0xff)) / 255.0) {}
  inline operator color () {
    return
      ((int) (b * 255 + 0.5)) +
      (((int) (g * 255 + 0.5)) << 8) +
      (((int) (r * 255 + 0.5)) << 16) +
      (((int) (a * 255 + 0.5)) << 24); }
};

inline true_color
normalize (const true_color& c) {
  return true_color (max (min (c.r, 1.0), 0.0),
                     max (min (c.g, 1.0), 0.0),
                     max (min (c.b, 1.0), 0.0),
                     max (min (c.a, 1.0), 0.0));
}

inline true_color
operator + (const true_color& c1, const true_color& c2) {
  return true_color (c1.r + c2.r, c1.g + c2.g, c1.b + c2.b, c1.a + c2.a);
}

inline true_color
operator - (const true_color& c1, const true_color& c2) {
  return true_color (c1.r - c2.r, c1.g - c2.g, c1.b - c2.b, c1.a - c2.a);
}

inline true_color
operator * (float x, const true_color& c) {
  return true_color (c.r * x, c.g * x, c.b * x, c.a * x);
}

inline true_color
operator * (const true_color& c, float x) {
  return true_color (c.r * x, c.g * x, c.b * x, c.a * x);
}

inline true_color
operator / (const true_color& c, float x) {
  return true_color (c.r / x, c.g / x, c.b / x, c.a / x);
}

#endif // defined TRUE_COLOR_H
