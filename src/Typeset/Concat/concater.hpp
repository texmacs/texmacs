
/******************************************************************************
* MODULE     : concater.hpp
* DESCRIPTION: Typesetting concatenations in two stages.
*                - produce an array of line items
*                - handle brackets and scripts
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef CONCATER_H
#define CONCATER_H
#include "typesetter.hpp"
#include "Format/line_item.hpp"
#include "Boxes/construct.hpp"

#define MODE_JUSTIFY   0
#define MODE_LEFT      1
#define MODE_CENTER    2
#define MODE_RIGHT     3

class concater_rep {
  edit_env              env;        // the environment
  array<line_item>      a;          // the line items
  bool                  rigid;      // when surely not wrappable

  // useful subroutines
  void print (box b);
  void print (int type, int op_type, box b);
  void control (tree t, path ip);
  void marker (path ip);
  void ghost (string s, path ip);
  void ghost (string s, path ip, color col);
  void flag_ok (string s, path ip, color col);
  void flag (string s, path ip, color col);
  void print (space spc);
  void penalty_min (int p);
  void penalty_max (int p);
  void with_limits (int status);

  // textual markup
  void typeset_substring (string s, path ip, int pos);
  void typeset_math_substring (string s, path ip, int pos, int op_type);
  void typeset_colored_substring (string s, path ip, int pos, string col);
  void typeset_text_string (tree t, path ip, int start, int end);
  void typeset_math_string (tree t, path ip, int start, int end);
  void typeset_prog_string (tree t, path ip, int start, int end);
  void typeset_document (tree t, path ip);
  void typeset_paragraph (tree t, path ip);
  void typeset_surround (tree t, path ip);
  void typeset_concat (tree t, path ip);
  void typeset_rigid (tree t, path ip);
  void typeset_hspace (tree t, path ip);
  void typeset_space (tree t, path ip);
  void typeset_move (tree t, path ip);
  void typeset_shift (tree t, path ip);
  void typeset_resize (tree t, path ip);
  void typeset_clipped (tree t, path ip);
  void typeset_float (tree t, path ip);
  void typeset_repeat (tree t, path ip);
  void typeset_formatting (tree t, path ip, string var);
  void typeset_decorated_box (tree t, path ip);
  void typeset_line_note (tree t, path ip);
  void typeset_page_note (tree t, path ip);

  // mathematical markup
  void typeset_around (tree t, path ip, bool colored);
  void typeset_large (tree t, path ip, int type, int op_type, string prefix);
  //void typeset_left (tree t, path ip);
  //void typeset_middle (tree t, path ip);
  //void typeset_right (tree t, path ip);
  void typeset_bigop (tree t, path ip);
  void typeset_long_arrow (tree t, path ip);
  void typeset_lprime (tree t, path ip);
  void typeset_rprime (tree t, path ip);
  void typeset_below (tree t, path ip);
  void typeset_above (tree t, path ip);
  void typeset_script (tree t, path ip, bool right);
  void typeset_wide_frac (tree t, path ip);
  void typeset_frac (tree t, path ip);
  void typeset_wide_sqrt (tree t, path ip);
  void typeset_sqrt (tree t, path ip);
  void typeset_wide (tree t, path ip, bool above);
  void typeset_neg (tree t, path ip);
  void typeset_tree (tree t, path ip);
  void typeset_wide_table (tree t, path ip);
  void typeset_table (tree t, path ip);
  void print_semantic (box b, tree sem);
  void typeset_syntax (tree t, path ip);

  // deactivated markup
  void typeset_blue (tree t, path ip);
  void typeset_src_open (tree t, path ip, string extra);
  void typeset_src_middle (tree t, path ip, int i);
  void typeset_src_close (tree t, path ip);
  void typeset_src_args (tree t, path ip);
  void typeset_src_tag (tree t, path ip);
  void typeset_inactive (tree t, path ip);
  void typeset_error (tree t, path ip);

  // active macro mechanisms
  void typeset_assign (tree t, path ip);
  void typeset_with (tree t, path ip);
  void typeset_compound (tree t, path ip);
  void typeset_auto (tree t, path ip, tree macro);
  void typeset_include (tree t, path ip);
  void typeset_drd_props (tree t, path ip);
  void typeset_eval (tree t, path ip);
  void typeset_value (tree t, path ip);
  void typeset_argument (tree t, path ip);
  void typeset_eval_args (tree t, path ip);
  void typeset_mark (tree t, path ip);
  void typeset_expand_as (tree t, path ip);
  void typeset_dynamic (tree t, path ip);
  void typeset_executable (tree t, path ip);
  void typeset_rewrite (tree t, path ip);

  // miscellaneous active markup
  void typeset_if (tree t, path ip);
  void typeset_var_if (tree t, path ip);
  void typeset_case (tree t, path ip);
  void typeset_range (tree t, path ip);
  void typeset_locus (tree t, path ip);
  void typeset_set_binding (tree t, path ip);
  void typeset_write (tree t, path ip);
  void typeset_toc_notify (tree t, path ip);
  void typeset_specific (tree t, path ip);
  void typeset_flag (tree t, path ip);
  void typeset_hyphenate_as (tree t, path ip);

  // animations
  void typeset_anim_compose (tree t, path ip);
  void typeset_anim_repeat (tree t, path ip);
  void typeset_anim_constant (tree t, path ip);
  void typeset_anim_accelerate (tree t, path ip);
  void typeset_anim_translate (tree t, path ip);
  void typeset_anim_progressive (tree t, path ip);
  void typeset_video (tree t, path ip);
  void typeset_sound (tree t, path ip);

  // graphical markup
  void typeset_graphical (array<box>& bs, tree t, path ip);
  void typeset_graphics (tree t, path ip);
  void typeset_superpose (tree t, path ip);
  void typeset_gr_group (tree t, path ip);
  void typeset_gr_transform (tree t, path ip);
  void typeset_gr_effect (tree t, path ip);
  void typeset_text_at (tree t, path ip);
  void typeset_math_at (tree t, path ip);
  void typeset_document_at (tree t, path ip);
  void typeset_point (tree t, path ip);
  array<box> typeset_line_arrows (path ip);
  void typeset_line (tree t, path ip, bool close);
  void typeset_arc (tree t, path ip, bool close);
  void typeset_spline (tree t, path ip, bool close);
  void typeset_var_spline (tree t, path ip);
  void typeset_cspline (tree t, path ip);
  void typeset_bezier (tree t, path ip);
  void typeset_fill (tree t, path ip);
  void typeset_image (tree t, path ip);
  void typeset_graphics_3d (tree t, path ip);

  // markup for graphical user interface
  void typeset_canvas (tree t, path ip);
  void typeset_ornament (tree t, path ip);

  // postprocessing of brackets and scripts
  int  prec (int i);
  int  succ (int i);
  void pre_glue ();
  box  glue_left_markers (box b, int ref, int arg);
  box  glue_right_markers (box b, int ref, int arg, bool flag);
  void glue (box b, int ref, int arg);
  void glue (box b, int ref, int arg1, int arg2);
  void clean_and_correct ();
  void handle_scripts (int start, int end);
  void handle_matching (int start, int end);
  void handle_brackets ();
  void kill_spaces ();

public:
  concater_rep (edit_env env, bool rigid= false);
  void typeset (tree t, path ip);
  void finish ();

  friend class liner_rep;
  friend class par_line_rep;
  friend class typesetter_rep;
  friend class document_rep;

  friend box              typeset_as_concat (edit_env env, tree t, path ip);
  friend box              typeset_as_concat (edit_env env, tree t, path ip,
                                             array<line_item>& a);
  friend box              typeset_as_box (edit_env env, tree t, path ip);
  friend box              typeset_as_atomic (edit_env env, tree t, path ip);
  friend array<line_item> typeset_concat (edit_env env, tree t, path ip);
  friend array<line_item> typeset_concat_range (edit_env env, tree t, path ip,
						int i1, int i2);
  friend array<line_item> typeset_marker (edit_env env, path ip);
};

typedef concater_rep* concater;

box typeset_as_concat (edit_env env, tree t, path ip);
array<line_item> typeset_concat (edit_env env, tree t, path ip);
array<line_item> typeset_concat_range (edit_env env, tree t, path ip,
				       int i1, int i2);
array<line_item> typeset_marker (edit_env env, path ip);

#endif // defined CONCATER_H
