
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
#include "unary_function.hpp"

/******************************************************************************
* Main true_color class
******************************************************************************/

class true_color {
public:
  typedef double scalar_type;

public:
  double b;
  double g;
  double r;
  double a;

public:
  inline true_color () {}
  inline true_color (const true_color& c):
    b (c.b), g (c.g), r (c.r), a (c.a) {}
  inline true_color& operator = (const true_color& c) {
    b= c.b; g= c.g; r= c.r; a= c.a; return *this; }
  inline true_color (double r2, double g2, double b2, double a2):
    b (b2), g (g2), r (r2), a (a2) {}
  inline true_color (color c):
    b (((double) (c & 0xff)) / 255.0),
    g (((double) ((c >> 8) & 0xff)) / 255.0),
    r (((double) ((c >> 16) & 0xff)) / 255.0),
    a (((double) ((c >> 24) & 0xff)) / 255.0) {}
  inline operator color () const {
    return
      ((int) (b * 255 + 0.5)) +
      (((int) (g * 255 + 0.5)) << 8) +
      (((int) (r * 255 + 0.5)) << 16) +
      (((int) (a * 255 + 0.5)) << 24); }
};

inline tm_ostream&
operator << (tm_ostream& out, const true_color& c) {
  return out << "[ " << c.r << ", " << c.g << ", " << c.b
             << "; " << c.a << "]";
}

/******************************************************************************
* Basic arithmetic
******************************************************************************/

inline true_color
operator + (const true_color& c1, const true_color& c2) {
  return true_color (c1.r + c2.r, c1.g + c2.g, c1.b + c2.b, c1.a + c2.a);
}

inline true_color
operator - (const true_color& c1, const true_color& c2) {
  return true_color (c1.r - c2.r, c1.g - c2.g, c1.b - c2.b, c1.a - c2.a);
}

inline true_color
operator * (const true_color& c1, const true_color& c2) {
  return true_color (c1.r * c2.r, c1.g * c2.g, c1.b * c2.b, c1.a * c2.a);
}

inline true_color
operator / (const true_color& c1, const true_color& c2) {
  return true_color (c1.r / c2.r, c1.g / c2.g, c1.b / c2.b, c1.a / c2.a);
}

inline true_color&
operator += (true_color& c1, const true_color& c2) {
  c1.r += c2.r; c1.g += c2.g; c1.b += c2.b; c1.a += c2.a;
  return c1;
}

inline true_color&
operator -= (true_color& c1, const true_color& c2) {
  c1.r -= c2.r; c1.g -= c2.g; c1.b -= c2.b; c1.a -= c2.a;
  return c1;
}

inline true_color&
operator *= (true_color& c1, const true_color& c2) {
  cout << c1 << ", " << c2 << "\n";
  c1.r *= c2.r; c1.g *= c2.g; c1.b *= c2.b; c1.a *= c2.a;
  return c1;
}

inline true_color&
operator /= (true_color& c1, const true_color& c2) {
  c1.r /= c2.r; c1.g /= c2.g; c1.b /= c2.b; c1.a /= c2.a;
  return c1;
}

inline true_color
operator * (double x, const true_color& c) {
  return true_color (c.r * x, c.g * x, c.b * x, c.a * x);
}

inline true_color
operator * (const true_color& c, double x) {
  return true_color (c.r * x, c.g * x, c.b * x, c.a * x);
}

inline true_color
operator / (const true_color& c, double x) {
  return true_color (c.r / x, c.g / x, c.b / x, c.a / x);
}

inline true_color
min (const true_color& c1, const true_color& c2) {
  return true_color (min (c1.r, c2.r), min (c1.g, c2.g),
                     min (c1.b, c2.b), min (c1.a, c2.a));
}

inline true_color
max (const true_color& c1, const true_color& c2) {
  return true_color (max (c1.r, c2.r), min (c1.g, c2.g),
                     max (c1.b, c2.b), min (c1.a, c2.a));
}

/******************************************************************************
* Composition operators
******************************************************************************/

inline true_color
source_over (const true_color& c1, const true_color& c2) {
  double a1= c1.a, a2= c2.a, a= a2 + a1 * (1 - a2);
  double u= 1.0 / (a + 1.0e-6);
  double f1= a1 * (1 - a2) * u, f2= a2 * u;
  return true_color (c1.r * f1 + c2.r * f2,
                     c1.g * f1 + c2.g * f2,
                     c1.b * f1 + c2.b * f2,
                     a);
}

inline true_color
towards_source (const true_color& c1, const true_color& c2) {
  double a2= c2.a, a1= 1.0 - a2;
  return true_color (c1.r * a1 + c2.r * a2,
                     c1.g * a1 + c2.g * a2,
                     c1.b * a1 + c2.b * a2,
                     c1.a);
}

inline true_color
alpha_distance (const true_color& c1, const true_color& c2) {
  double a1= c1.a, a2= c2.a, s= a1 + a2 + 1.0e-6, a= fabs (a1 - a2);
  double f1= a1 / s, f2= a2 / s;
  return true_color (c1.r * f1 + c2.r * f2,
                     c1.g * f1 + c2.g * f2,
                     c1.b * f1 + c2.b * f2,
                     a);
}

/******************************************************************************
* Transparency
******************************************************************************/

inline void
clear (true_color& c) {
  c.r= c.g= c.b= c.a= 0.0;
}

inline void
clear_alpha (true_color& c) {
  c.a= 0.0;
}

inline double
get_alpha (const true_color& c) {
  return c.a;
}

inline double&
get_alpha (true_color& c) {
  return c.a;
}

inline true_color
mul_alpha (const true_color& c) {
  return true_color (c.r * c.a, c.g * c.a, c.b * c.a, c.a);
}

inline true_color
div_alpha (const true_color& c) {
  if (c.a < 0.00390625 && c.a > -0.00390625) return c;
  else return true_color (c.r / c.a, c.g / c.a, c.b / c.a, c.a);
}

inline true_color
apply_alpha (const true_color& c, double a) {
  return true_color (c.r, c.g, c.b, c.a * a);
}

inline true_color
copy_alpha (const true_color& c, double a) {
  return true_color (c.r, c.g, c.b, a);
}

inline true_color
copy_alpha (const true_color& c, const true_color& a) {
  return true_color (c.r, c.g, c.b, a.a);
}

/******************************************************************************
* Other operators
******************************************************************************/

inline true_color
normalize (const true_color& c) {
  return true_color (max (min (c.r, 1.0), 0.0),
                     max (min (c.g, 1.0), 0.0),
                     max (min (c.b, 1.0), 0.0),
                     max (min (c.a, 1.0), 0.0));
}

inline true_color
hypot (const true_color& c1, const true_color& c2) {
  return true_color (sqrt (c1.r * c1.r + c2.r * c2.r),
                     sqrt (c1.g * c1.g + c2.g * c2.g),
                     sqrt (c1.b * c1.b + c2.b * c2.b),
                     sqrt (c1.a * c1.a + c2.a * c2.a));
}

inline double
max (const true_color& c) {
  return max (c.r, max (c.g, max (c.b, c.a)));
}

inline double
inner_max (const true_color& c1, const true_color& c2) {
  return max (max (c1.r * c2.r, c1.g * c2.g),
              max (c1.b * c2.b, c1.a * c2.a));
}

true_color mix (const true_color& c1, double a1,
		const true_color& c2, double a2);
true_color mix (const true_color& c1, double a1,
		const true_color& c2, double a2,
		const true_color& c3, double a3,
		const true_color& c4, double a4);

/******************************************************************************
* Color transformations
******************************************************************************/

unary_function<true_color,true_color>
color_matrix_function (const array<double>& a);
unary_function<true_color,true_color>
make_transparent_function (const true_color& bgc, const double& t= 1.0);
unary_function<true_color,true_color>
make_opaque_function (const true_color& bgc);

#endif // defined TRUE_COLOR_H
