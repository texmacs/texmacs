
/******************************************************************************
* MODULE     : edit_math.hpp
* DESCRIPTION: Editing mathematics
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef EDIT_MATH_H
#define EDIT_MATH_H
#include "editor.hpp"

class edit_math_rep: virtual public editor_rep {
public:
  edit_math_rep ();
  ~edit_math_rep ();

  void make_rigid ();
  void make_lprime (string s);
  void make_rprime (string s);
  void make_below ();
  void make_above ();
  void make_script (bool sup, bool right);
  void make_fraction ();
  void make_sqrt ();
  void make_var_sqrt ();
  void make_wide (string wide);
  void make_wide_under (string wide);
  void make_neg ();
  void make_tree ();
  void back_around (tree t, path p, bool forward);
  void back_prime (tree t, path p, bool forward);
  void back_in_around (tree t, path p, bool forward);
  void back_in_wide (tree t, path p, bool forward);
  void back_in_tree (tree t, path p, bool forward);
  void pre_remove_around (path p);
};

#endif // defined EDIT_MATH_H
