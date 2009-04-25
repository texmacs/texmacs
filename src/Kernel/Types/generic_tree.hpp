
/******************************************************************************
* MODULE     : generic_tree.hpp
* DESCRIPTION: generic objects as trees
* COPYRIGHT  : (C) 2009  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef GENERIC_TREE_H
#define GENERIC_TREE_H
#include "tree.hpp"
#include "blackbox.hpp"

class generic_rep: public tree_rep {
public:
  blackbox data;
  template<typename T> inline generic_rep (const T& x):
    tree_rep ((tree_label) type_helper<T>::id),
    data (close_box<T> (x)) {}
  friend class tree;
};

inline blackbox
as_blackbox (const tree& t) {
  return ((generic_rep*) t.rep) -> data;
}

template<typename T, typename F>
struct convert_helper {
  static inline T op (const F& x) { return (T) x; }
};

template<typename F>
struct convert_helper<tree,F> {
  static inline tree op (const F& data) {
    return tree ((tree_rep*) tm_new<generic_rep> (data)); }
};

template<typename T>
struct convert_helper<T,tree> {
  static inline T op (const tree& t) {
    return open_box<T> (as_blackbox (t)); }
};

template<typename T> inline bool is (const tree& t) {
  return ((int) L(t)) == type_helper<T>::id; }
template<typename T, typename F> inline T as (const F& t) {
  return convert_helper<F,T>::op (t); }

#endif // defined GENERIC_TREE_H
