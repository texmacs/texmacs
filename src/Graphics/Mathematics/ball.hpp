
/******************************************************************************
* MODULE     : ball.hpp
* DESCRIPTION: balls with center in C and radius in R (= norm_type (C))
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef BALL_H
#define BALL_H
#include "properties.hpp"
#include "operators.hpp"
#define TMPL template<typename C>
#define BINARY_TMPL template<typename C1,typename C2>
#define R typename properties<C>::norm_type
#define M typename binary_properties<C1,C2>::product_type

/******************************************************************************
* The ball class
******************************************************************************/

TMPL
class ball {
public:
  C cen;
  R rad;
public:
  inline ball (C c=0, R r=0): cen (c), rad (r) {}
};

TMPL inline C center (const ball<C>& b) { return b.cen; }
TMPL inline R radius (const ball<C>& b) { return b.rad; }
TMPL inline R upper (const ball<C>& b) { return norm (b.cen) + b.rad; }
TMPL inline R lower (const ball<C>& b) { return norm (b.cen) - b.rad; }

TMPL inline tree as_tree (const ball<C>& b) {
  return compound ("ball", as_tree (center (b)), as_tree (radius (b))); }
TMPL inline tm_ostream& operator << (tm_ostream& out, const ball<C>& b) {
  return out << as_math_string (as_tree (b)); }

TMPL
class properties<ball<C> > {
public:
  typedef ball<typename properties<C>::scalar_type> scalar_type;
  typedef typename properties<C>::norm_type norm_type;
  typedef typename properties<C>::index_type index_type;
  static inline tree index_name (index_type i) {
    return properties<C>::index_name (i); }
  static inline scalar_type access (ball<C> b, index_type var) {
    return scalar_type (properties<C>::access (center (b), var), radius (b)); }
};

BINARY_TMPL
class binary_properties<ball<C1>,ball<C2> > {
public:
  typedef ball<M > product_type;
};

/******************************************************************************
* Basic ball arithmetic
******************************************************************************/

TMPL ball<C>
operator - (const ball<C>& b) {
  return ball<C> (-center (b), radius (b));
}

TMPL ball<C>
operator + (const ball<C>& b1, const ball<C>& b2) {
  return ball<C> (center (b1) + center (b2), radius (b1) + radius (b2));
}

TMPL ball<C>
operator - (const ball<C>& b1, const ball<C>& b2) {
  return ball<C> (center (b1) - center (b2), radius (b1) + radius (b2));
}

BINARY_TMPL ball<M >
operator * (const ball<C1>& b1, const ball<C2>& b2) {
  return ball<M > (center (b1) * center (b2),
		   norm (center (b1)) * radius (b2) +
		   radius (b1) * norm (center (b2)) +
		   radius (b1) * radius (b2));
}

TMPL ball<C>
invert (const ball<C>& b) {
  return ball<C> (invert (center (b)), radius (b) / square (lower (b)));
}

BINARY_TMPL ball<M >
operator / (const ball<C1>& b1, const ball<C2>& b2) {
  return b1 * invert (b2);
}

/******************************************************************************
* Special functions
******************************************************************************/

TMPL ball<C>
sqrt (const ball<C>& b) {
  return ball<C> (sqrt (center (b)), radius (b) / (2 * square (loer (b))));
}

TMPL ball<C>
exp (const ball<C>& b) {
  return ball<C> (exp (center (b)), exp (upper (b)) * radius (b));
}

TMPL ball<C>
log (const ball<C>& b) {
  return ball<C> (log (center (b)), radius (b) / lower (b));
}

TMPL inline ball<C>
pow (const ball<C>& b1, const ball<C>& b2) {
  return exp (b2 * log (b1));
}

TMPL ball<C>
cos (const ball<C>& z) {
  if (radius (z) >= R (3.14159))
    return ball<C> (C(0), R(1));
  else {
    R u1= sin (lower (z));
    R u2= sin (upper (z));
    if (u1 * u2 <= 0)
      return ball<C> (cos (center (z)), radius (z));
    return ball<C> (cos (center (z)),
		    max (norm (u1), norm (u2)) * radius (z));
  }
}

TMPL ball<C>
sin (const ball<C>& z) {
  if (radius (z) >= R (3.14159))
    return ball<C> (sin (center (z)), radius (z));
  else {
    R u1= cos (lower (z));
    R u2= cos (upper (z));
    if (u1 * u2 <= 0)
      return ball<C> (sin (center (z)), radius (z));
    return ball<C> (sin (center (z)),
		    max (norm (u1), norm (u2)) * radius (z));
  }
}

TMPL inline ball<C>
tan (const ball<C>& b) {
  return sin (b) / cos (b);
}

/******************************************************************************
* Other routines
******************************************************************************/

TMPL ball<R>
norm (const ball<C>& b) {
  return ball<R> (norm (center (b)), radius (b));
}

#undef TMPL
#undef BINARY_TMPL
#undef R
#undef M
#endif // defined BALL_H
