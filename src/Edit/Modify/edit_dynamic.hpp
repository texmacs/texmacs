
/******************************************************************************
* MODULE     : edit_dynamic.hpp
* DESCRIPTION: Main routines for the modification of dynamic content
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef EDIT_DYNAMIC_H
#define EDIT_DYNAMIC_H
#include "editor.hpp"

class edit_dynamic_rep: virtual public editor_rep {
public:
  edit_dynamic_rep ();
  ~edit_dynamic_rep ();

  path find_dynamic (path p);
  path find_deactivated (path p);
  bool in_preamble_mode ();
  bool is_deactivated ();
  bool is_multi_paragraph_macro (tree t);
  void activate_macro (path p, string name, tree f);
  void activate ();
  void activate_compound ();

  void make_active (string op, int n);
  void make_deactivated (tree t, path p);
  void make_deactivated (string op, int n, string rfooter, string arg);
  bool make_return_before ();
  bool make_return_after ();
  void make_assign (tree var, tree by);
  void make_with (string var, string val);
  bool make_big_expand (string s);
  void make_expand (string s, int n=0);
  void temp_proof_fix ();
  void make_apply (string s);
  void insert_argument (bool forward);

  void back_dynamic (path p);
  void back_expand (path p);
  void back_hide_expand (path p);
  void back_compound (path p);
  void back_extension (path p);
  void back_in_dynamic (tree t, path p, int min_args=1, int with=1);
  void back_in_with (tree t, path p);
  void back_in_expand (tree t, path p);
  void back_in_compound (tree t, path p);
  void back_in_extension (tree t, path p);
};

#endif // defined EDIT_DYNAMIC_H
