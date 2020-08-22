
/******************************************************************************
* MODULE     : true_color.cpp
* DESCRIPTION: RGBA colors represented by floating point numbers
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "true_color.hpp"

/******************************************************************************
* Mixing
******************************************************************************/

true_color
mix (const true_color& c1, double a1, const true_color& c2, double a2) {
  double ta= a1 * c1.a + a2 * c2.a;
  if (ta <= 1.0e-10) return true_color (0.0, 0.0, 0.0, 0.0);
  double R1= c1.r * c1.a, G1= c1.g * c1.a, B1= c1.b * c1.a;
  double R2= c2.r * c2.a, G2= c2.g * c2.a, B2= c2.b * c2.a;
  double tR= a1 * R1 + a2 * R2;
  double tG= a1 * G1 + a2 * G2;
  double tB= a1 * B1 + a2 * B2;
  return true_color (tR / ta, tG / ta, tB / ta, ta);
}

true_color
mix (const true_color& c1, double a1, const true_color& c2, double a2,
     const true_color& c3, double a3, const true_color& c4, double a4)
{
  double ta= a1 * c1.a + a2 * c2.a + a3 * c3.a + a4 * c4.a;
  if (ta <= 1.0e-10) return true_color (0.0, 0.0, 0.0, 0.0);
  double R1= c1.r * c1.a, G1= c1.g * c1.a, B1= c1.b * c1.a;
  double R2= c2.r * c2.a, G2= c2.g * c2.a, B2= c2.b * c2.a;
  double R3= c3.r * c3.a, G3= c3.g * c3.a, B3= c3.b * c3.a;
  double R4= c4.r * c4.a, G4= c4.g * c4.a, B4= c4.b * c4.a;
  double tR= a1 * R1 + a2 * R2 + a3 * R3 + a4 * R4;
  double tG= a1 * G1 + a2 * G2 + a3 * G3 + a4 * G4;
  double tB= a1 * B1 + a2 * B2 + a3 * B3 + a4 * B4;
  return true_color (tR / ta, tG / ta, tB / ta, ta);
}

/******************************************************************************
* Color transformations
******************************************************************************/

class color_matrix_function_rep:
  public unary_function_rep<true_color,true_color>
{
  double rr, rg, rb, ra, r;
  double gr, gg, gb, ga, g;
  double br, bg, bb, ba, b;
  double ar, ag, ab, aa, a;
public:
  color_matrix_function_rep (array<double> m):
    rr (m[ 0]), rg(m[ 1]), rb (m[ 2]), ra(m[ 3]), r (m[ 4]),
    gr (m[ 5]), gg(m[ 6]), gb (m[ 7]), ga(m[ 8]), g (m[ 9]),
    br (m[10]), bg(m[11]), bb (m[12]), ba(m[13]), b (m[14]),
    ar (m[15]), ag(m[16]), ab (m[17]), aa(m[18]), a (m[19]) {}
  true_color eval (const true_color& c) {
    return true_color (rr * c.r + rg * c.g + rb * c.b + ra * c.a + r,
                       gr * c.r + gg * c.g + gb * c.b + ga * c.a + g,
                       br * c.r + bg * c.g + bb * c.b + ba * c.a + b,
                       ar * c.r + ag * c.g + ab * c.b + aa * c.a + a); }
};

unary_function<true_color,true_color>
color_matrix_function (const array<double>& m) {
  ASSERT (N(m) == 20, "5 x 4 matrix expected");
  return tm_new<color_matrix_function_rep> (m); }

class make_transparent_function_rep:
  public unary_function_rep<true_color,true_color>
{
  true_color b;
  double t;
public:
  make_transparent_function_rep (const true_color& b2, const double& t2):
    b (b2), t (max (min (t2, 1.0), 0.0)) {}
  true_color eval (const true_color& c) {
    double mr= 0.0, mg= 0.0, mb= 0.0;
    if (c.r > b.r) mr= (c.r - b.r) / (1 - b.r);
    else if (c.r < b.r) mr= (b.r - c.r) / b.r;
    if (c.g > b.g) mg= (c.g - b.g) / (1 - b.g);
    else if (c.g < b.g) mg= (b.g - c.g) / b.g;
    if (c.b > b.b) mb= (c.b - b.b) / (1 - b.b);
    else if (c.b < b.b) mb= (b.b - c.b) / b.b;
    double a= max (mr, max (mg, mb));
    if (a < t) a= a / t; else a= 1.0;
    if (a == 0) return true_color (c.r, c.g, c.b, 0);
    double nr= b.r + (c.r - b.r) / a;
    double ng= b.g + (c.g - b.g) / a;
    double nb= b.b + (c.b - b.b) / a;
    return true_color (nr, ng, nb, a * c.a);
  }
};

unary_function<true_color,true_color>
make_transparent_function (const true_color& b, const double& t) {
  return tm_new<make_transparent_function_rep> (b, t); }

class make_opaque_function_rep:
  public unary_function_rep<true_color,true_color>
{
  true_color b;
public:
  make_opaque_function_rep (const true_color& b2): b (b2) {}
  true_color eval (const true_color& c) {
    return source_over (b, c); }
};

unary_function<true_color,true_color>
make_opaque_function (const true_color& b) {
  return tm_new<make_opaque_function_rep> (b); }
