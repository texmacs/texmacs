
/******************************************************************************
* MODULE     : math_tree.hpp
* DESCRIPTION: trees as mathematical expressions
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef MATH_TREE_H
#define MATH_TREE_H
#include "tree.hpp"

tree neg (tree t1);
tree add (tree t1, tree t2);
tree sub (tree t1, tree t2);
tree mul (tree t1, tree t2);
tree div (tree t1, tree t2);

tree sqrt (tree t);
tree exp (tree t);
tree log (tree t);
tree pow (tree t1, tree t2);
tree sin (tree t);
tree cos (tree t);
tree tan (tree t);

string as_math_string (tree t);

template<typename T> void
parse (tree t, T& result) {
  FAILED ("unsupported type");
}

inline void
parse (tree t, double& result) {
  ASSERT (is_double (t), "not a double");
  result= as_double (t);
}

template<typename T> T
parse_as (tree t) {
  switch (L(t)) {
  case PLUS:
    return parse_as<T> (t[0]) + parse_as<T> (t[1]);
  case MINUS:
    if (N(t) == 1) return -parse_as<T> (t[0]);
    else return parse_as<T> (t[0]) - parse_as<T> (t[1]);
  case TIMES:
    return parse_as<T> (t[0]) * parse_as<T> (t[1]);
  case OVER:
    return parse_as<T> (t[0]) / parse_as<T> (t[1]);
  case MATH_SQRT:
    return sqrt (parse_as<T> (t[0]));
  case EXP:
    return exp (parse_as<T> (t[0]));
  case LOG:
    return log (parse_as<T> (t[0]));
  case POW:
    return pow (parse_as<T> (t[0]), parse_as<T> (t[1]));
  case COS:
    return cos (parse_as<T> (t[0]));
  case SIN:
    return sin (parse_as<T> (t[0]));
  case TAN:
    return tan (parse_as<T> (t[0]));
  default:
    {
      T result;
      parse (t, result);
      return result;
    }
  }
}

#endif // defined MATH_TREE_H
