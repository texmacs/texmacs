
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

  bool in_preamble_mode ();
  bool is_deactivated ();
  path find_deactivated (path p);
  path find_dynamic (path p);

  bool is_multi_paragraph_macro (tree t);
  void make_compound (tree_label l, int n);
  void activate ();

  void go_to_argument (path p, bool start_flag);
  void insert_argument (path p, bool forward);
  void insert_argument (bool forward);
  void remove_argument (path p, bool forward);

  void back_monolithic (path p);
  void back_general (path p, bool forward);
  void back_in_general (tree t, path p, bool forward);

  void make_with (string var, string val);
  void insert_with (path p, string var, tree val);
  void remove_with (path p, string var);
  void back_in_with (tree t, path p, bool forward);

  void make_mod_active (tree_label l);

  void make_hybrid ();
  bool activate_latex ();
  void activate_hybrid ();
  void activate_symbol ();
  void activate_compound ();

  bool make_return_before ();
  bool make_return_after ();
  void temp_proof_fix ();
};

#endif // defined EDIT_DYNAMIC_H
