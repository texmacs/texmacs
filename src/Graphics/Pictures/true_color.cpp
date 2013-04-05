
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

true_color
mix (const true_color& c1, double a1, const true_color& c2, double a2) {
  double ta= a1 * c1.a + a2 * c2.a;
  if (ta <= 1.0e-10) return true_color (0.0, 0.0, 0.0, 0.0);
  double R1= c1.r * c1.a, G1= c1.g * c1.a, B1= c1.b * c1.b;
  double R2= c2.r * c2.a, G2= c2.g * c2.a, B2= c2.b * c2.b;
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
  double R1= c1.r * c1.a, G1= c1.g * c1.a, B1= c1.b * c1.b;
  double R2= c2.r * c2.a, G2= c2.g * c2.a, B2= c2.b * c2.b;
  double R3= c3.r * c3.a, G3= c3.g * c3.a, B3= c3.b * c3.b;
  double R4= c4.r * c4.a, G4= c4.g * c4.a, B4= c4.b * c4.b;
  double tR= a1 * R1 + a2 * R2 + a3 * R3 + a4 * R4;
  double tG= a1 * G1 + a2 * G2 + a3 * G3 + a4 * G4;
  double tB= a1 * B1 + a2 * B2 + a3 * B3 + a4 * B4;
  return true_color (tR / ta, tG / ta, tB / ta, ta);
}
