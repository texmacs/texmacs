
/******************************************************************************
* MODULE     : mathutil.hpp
* DESCRIPTION: extra math functions
* COPYRIGHT  : (C) 2003  Henri Lesourd
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef MATH_UTIL_H
#define MATH_UTIL_H
#include "tree.hpp"
#include <math.h>

const  double infinity=3.40282347e+38F;

inline double square (double x) { return x*x; }
inline double norm (double x) { return x>0?x:-x; }
inline double nearest (double x) { // round missing in some math.h
  return floor (x+0.5); }
inline int    sign (double x) { return x>0?1:x<0?-1:0; } 
inline int    fnull (double x, double approx) { return norm(x) <= approx; }
inline double pow (double x, int n) {
  if (n<=0) return 1; n--; while (n--) x*=x; return x; }

#endif // defined MATH_UTIL_H
