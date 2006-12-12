
/******************************************************************************
* MODULE     : operators.hpp
* DESCRIPTION: Templates and default implementations of several operations
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef OPERATORS_HPP
#define OPERATORS_HPP
#include "math_tree.hpp"

/******************************************************************************
* Helper functions
******************************************************************************/

template<typename C> inline C
invert (C x) {
  return 1/x;
}

template<typename C> inline C
square (C x) {
  return x*x;
}

template<typename C> inline C
norm (C x) {
  return x >= 0? x: -x;
}

template<typename C> inline C
square_norm (C x) {
  return x*x;
}

/******************************************************************************
* Arithmetic operators
******************************************************************************/

struct copy_op {
  template<typename C> static inline C
  eval (C x) { return x; }
  static inline tree
  eval (tree x) { return x; }
  template<typename C, typename V> static inline C
  diff (C x, V v) { return derive (x, v); }
};

struct neg_op {
  template<typename C> static inline C
  eval (C x) { return -x; }
  static inline tree
  eval (tree x) { return neg (x); }
  template<typename C, typename V> static inline C
  diff (C x, V v) { return -derive (x, v); }
};

struct add_op {
  template<typename C> static inline C
  neutral () { return 0; }
  template<typename C> static inline C
  eval (C x, C y) { return x + y; }
  static inline tree
  eval (tree x, tree y) { return add (x, y); }
  template<typename C, typename V> static inline C
  diff (C x, C y, V v) { return derive (x, v) + derive (y, v); }
};

struct sub_op {
  template<typename C> static inline C
  eval (C x, C y) { return x - y; }
  static inline tree
  eval (tree x, tree y) { return sub (x, y); }
  template<typename C, typename V> static inline C
  diff (C x, C y, V v) { return derive (x, v) - derive (y, v); }
};

struct mul_op {
  template<typename C> static inline C
  neutral () { return 1; }
  template<typename C> static inline C
  eval (C x, C y) { return x * y; }
  static inline tree
  eval (tree x, tree y) { return mul (x, y); }
  template<typename C, typename V> static inline C
  diff (C x, C y, V v) { return derive (x, v) * y + x * derive (y, v); }
};

struct div_op {
  template<typename C> static inline C
  eval (C x, C y) { return x / y; }
  static inline tree
  eval (tree x, tree y) { return div (x, y); }
  template<typename C, typename V> static inline C
  diff (C x, C y, V v) {
    return (derive (x, v) * y - x * derive (y, v)) / square (y); }
};

/******************************************************************************
* Special functions
******************************************************************************/

struct sqrt_op {
  template<typename C> static inline C
  eval (C x) { return sqrt (x); }
  template<typename C, typename V> static inline C
  diff (C x, V v) { return derive (x, v) / (2 * sqrt (x)); }
};

struct exp_op {
  template<typename C> static inline C
  eval (C x) { return exp (x); }
  template<typename C, typename V> static inline C
  diff (C x, V v) { return derive (x, v) * exp (x); }
};

struct log_op {
  template<typename C> static inline C
  eval (C x) { return log (x); }
  template<typename C, typename V> static inline C
  diff (C x, V v) { return derive (x, v) / x; }
};

struct pow_op {
  template<typename C> static inline C
  eval (C x, C y) { return pow (x, y); }
  template<typename C, typename V> static inline C
  diff (C x, C y, V v) {
    return (derive (x, v) * y / x + log (x) * derive (y, v)) * pow (x, y); }
};

struct cst_pow_op {
  template<typename C, typename T> static inline C
  eval (C x, T c) { return pow (x, c); }
  template<typename C, typename T, typename V> static inline C
  diff (C x, T c, V v) { return derive (x, v) * pow (x, c - 1); }
};

struct cos_op {
  template<typename C> static inline C
  eval (C x) { return cos (x); }
  template<typename C, typename V> static inline C
  diff (C x, V v) { return -derive (x, v) * sin (x); }
};

struct sin_op {
  template<typename C> static inline C
  eval (C x) { return sin (x); }
  template<typename C, typename V> static inline C
  diff (C x, V v) { return derive (x, v) * cos (x); }
};

struct tan_op {
  template<typename C> static inline C
  eval (C x) { return tan (x); }
  template<typename C, typename V> static inline C
  diff (C x, V v) { return derive (x, v) / square (cos (x)); }
};

/******************************************************************************
* Other operators
******************************************************************************/

struct derive_op {
  template<typename C> static inline C
  eval (C x) { return derive (x); }
};

#endif // OPERATORS_H
