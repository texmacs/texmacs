
/******************************************************************************
* MODULE     : edit_math.hpp
* DESCRIPTION: Editing mathematics
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef EDIT_MATH_H
#define EDIT_MATH_H
#include "editor.hpp"

class edit_math_rep: virtual public editor_rep {
public:
  edit_math_rep ();
  ~edit_math_rep ();

  void make_group ();
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
  void back_prime (tree t, path p, bool forward);
  void back_in_wide (tree t, path p, bool forward);

  bool inside_tree ();
  path get_tree (int& i);
  void branch_insert (bool at_right);
  void branch_delete (bool forward);
  void back_in_tree (tree t, path p, bool forward);
};

#endif // defined EDIT_MATH_H
