
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

const  double infinity=3.40282347e+38F;

inline double sq2 (double x) { return x*x; }
inline double sq3 (double x) { return x*x*x; }
inline double norm (double x) { return x>0?x:-x; }
inline int    sign (double x) { return x>0?1:x<0?-1:0; } 
inline int    fnull (double x, double approx) { return norm(x) <= approx; }
static double pow (double x, int n) {
  if (n<=0) return 1; n--; while (n--) x*=x; return x; }

#endif // defined MATH_UTIL_H
