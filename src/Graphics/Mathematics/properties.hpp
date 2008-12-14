
/******************************************************************************
* MODULE     : properties.hpp
* DESCRIPTION: Properties of mathematical data types
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PROPERTIES_HPP
#define PROPERTIES_HPP
#include "math_tree.hpp"

template<typename T>
class unary_properties {
public:
  typedef T scalar_type;
  typedef T norm_type;
  typedef int index_type;
  static inline tree index_name (index_type i) {
    (void) i; return "x"; }
  static inline scalar_type access (T x, index_type var) {
    return x; }
};

template<typename T, typename U>
class binary_properties {
public:
  typedef U product_type;
};

#endif // PROPERTIES_H
