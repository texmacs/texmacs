
/******************************************************************************
* MODULE     : edit_dynamic.hpp
* DESCRIPTION: Main routines for the modification of dynamic content
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef EDIT_DYNAMIC_H
#define EDIT_DYNAMIC_H
#include "editor.hpp"

class edit_dynamic_rep: virtual public editor_rep {
public:
  edit_dynamic_rep ();
  ~edit_dynamic_rep ();

  bool in_source ();
  path find_dynamic (path p);

  bool is_multi_paragraph_macro (tree t, bool pure);
  void make_compound (tree_label l, int n);
  void activate ();

  void go_to_argument (path p, bool start_flag);
  void insert_argument (path p, bool forward);
  void insert_argument (bool forward);
  void remove_empty_argument (path p, bool forward);
  void remove_argument (path p, bool forward);
  void remove_argument (bool forward);

  void back_monolithic (path p);
  void back_general (path p, bool forward);
  void back_in_general (tree t, path p, bool forward);

  void make_with (string var, string val);
  void insert_with (path p, string var, tree val);
  void remove_with (path p, string var);
  void back_in_with (tree t, path p, bool forward);

  void make_mod_active (tree_label l);
  void insert_style_with (path p, string var, string val);
  void make_style_with (string var, string val);

  void make_hybrid ();
  bool activate_latex ();
  void activate_hybrid (bool with_args_hint);
  void activate_symbol ();

  bool make_return_before ();
  bool make_return_after ();
  void temp_proof_fix ();
};

#endif // defined EDIT_DYNAMIC_H
