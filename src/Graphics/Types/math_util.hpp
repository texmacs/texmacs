
/******************************************************************************
* MODULE     : math_util.hpp
* DESCRIPTION: extra math functions
* COPYRIGHT  : (C) 2003  Henri Lesourd
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef MATH_UTIL_H
#define MATH_UTIL_H
#include "tree.hpp"

const  double tm_infinity=3.40282347e+38F;
const  double tm_PI=3.1415926535897932384626433832795029L;
const  double tm_E =2.7182818284590452353602874713526625L;

inline double square (double x) { return x*x; }
inline double norm (double x) { return x>0?x:-x; }
inline double nearest (double x) { // round missing in some math.h
  return floor (x+0.5); }
inline int    sign (double x) { return x>0?1:x<0?-1:0; } 
inline bool   fnull (double x, double approx) { return norm(x) <= approx; }
#ifndef __SUNPRO_CC
inline double pow (double x, int n) {
  if (n<=0) return 1; n--; while (n--) x*=x; return x; }
#endif


#endif // defined MATH_UTIL_H
