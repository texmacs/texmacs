
/******************************************************************************
* MODULE     : math_tree.hpp
* DESCRIPTION: trees as mathematical expressions
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
