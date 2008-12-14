
/******************************************************************************
* MODULE     : math_tree.cpp
* DESCRIPTION: trees as mathematical expressions
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "math_tree.hpp"

/******************************************************************************
* Mathematical functions on trees
******************************************************************************/

tree
neg (tree t) {
  if (is_double (t)) return as_tree (-as_double (t));
  return tree (MINUS, t);
}

tree
add (tree t1, tree t2) {
  if (is_double (t1) && is_double (t2))
    return as_tree (as_double (t1) + as_double (t2));
  if (t1 == "0") return t2;
  if (t2 == "0") return t1;
  return tree (PLUS, t1, t2);
}

tree
sub (tree t1, tree t2) {
  if (is_double (t1) && is_double (t2))
    return as_tree (as_double (t1) - as_double (t2));
  if (t1 == "0") return tree (MINUS, t2);
  if (t2 == "0") return t1;
  return tree (MINUS, t1, t2);
}

tree
mul (tree t1, tree t2) {
  if (is_double (t1) && is_double (t2))
    return as_tree (as_double (t1) * as_double (t2));
  if (t1 == "0" || t2 == "0") return "0";
  if (t1 == "1") return t2;
  if (t2 == "1") return t1;
  return tree (TIMES, t1, t2);
}

tree
div (tree t1, tree t2) {
  if (is_double (t1) && is_double (t2))
    return as_tree (as_double (t1) / as_double (t2));
  if (t1 == "0") return "0";
  if (t2 == "1") return t1;
  return tree (OVER, t1, t2);
}

tree
sqrt (tree t) {
  if (is_double (t)) return as_tree (sqrt (as_double (t)));
  return tree (MATH_SQRT, t);
}

tree
exp (tree t) {
  if (is_double (t)) return as_tree (exp (as_double (t)));
  return tree (EXP, t);
}

tree
log (tree t) {
  if (is_double (t)) return as_tree (log (as_double (t)));
  return tree (LOG, t);
}

tree
pow (tree t1, tree t2) {
  if (is_double (t1) && is_double (t2))
    return as_tree (pow (as_double (t1), as_double (t2)));
  if (t1 == "0" || t1 == "1") return t1;
  if (t2 == "0") return "1";
  if (t2 == "1") return t1;
  return tree (POW, t1, t2);
}

tree
cos (tree t) {
  if (is_double (t)) return as_tree (cos (as_double (t)));
  return tree (COS, t);
}

tree
sin (tree t) {
  if (is_double (t)) return as_tree (sin (as_double (t)));
  return tree (SIN, t);
}

tree
tan (tree t) {
  if (is_double (t)) return as_tree (tan (as_double (t)));
  return tree (TAN, t);
}

/******************************************************************************
* Pretty printing
******************************************************************************/

static void
math_print (string& s, tree t, int level) {
  if (level <= 0 && is_func (t, PLUS, 2)) {
    math_print (s, t[0], 0);
    s << " + ";
    math_print (s, t[1], 0);
  }
  else if (level <= 0 && is_func (t, MINUS, 2)) {
    math_print (s, t[0], 0);
    s << " - ";
    math_print (s, t[1], 1);
  }
  else if (level <= 1 && is_func (t, TIMES, 2)) {
    math_print (s, t[0], 1);
    s << " * ";
    math_print (s, t[1], 1);
  }
  else if (level <= 1 && is_func (t, OVER, 2)) {
    math_print (s, t[0], 1);
    s << " / ";
    math_print (s, t[1], 2);
  }
  else if (level <= 2 && is_func (t, POW, 2)) {
    math_print (s, t[0], 3);
    s << "^";
    math_print (s, t[1], 2);
  }
  else if (level <= 3 && is_func (t, MINUS, 1)) {
    s << "-";
    math_print (s, t[0], 3);
  }
  else if (level <= 4 && is_func (t, RSUB, 2)) {
    math_print (s, t[0], 4);
    s << "[";
    math_print (s, t[1], 0);
    s << "]";
  }
  else if (is_func (t, PLUS, 2) || is_func (t, MINUS, 2) ||
	   is_func (t, TIMES, 2) || is_func (t, OVER, 2) ||
	   is_func (t, POW, 2) || is_func (t, MINUS, 1) ||
	   is_func (t, RSUB, 2))
    {
      s << "(";
      math_print (s, t, 0);
      s << ")";
    }
  else if (is_atomic (t)) s << t->label;
  else if (is_tuple (t)) {
    int i, n= N(t);
    s << "[ ";
    for (i=0; i<n; i++) {
      if (i != 0) s << ", ";
      math_print (s, t[i], 0);
    }
    if (n != 0) s << " ";
    s << "]";
  }
  else {
    int i, n= N(t);
    if (L(t) == MATH_SQRT) s << "sqrt (";
    else s << as_string (L(t)) << " (";
    for (i=0; i<n; i++) {
      if (i != 0) s << ", ";
      math_print (s, t[i], 0);
    }
    s << ")";
  }
}

string
as_math_string (tree t) {
  string r;
  math_print (r, t, 0);
  return r;
}
