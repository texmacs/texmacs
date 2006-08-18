
/******************************************************************************
* MODULE     : properties.hpp
* DESCRIPTION: Properties of mathematical data types
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef PROPERTIES_HPP
#define PROPERTIES_HPP
#include "math_tree.hpp"

template<typename T>
class properties {
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
