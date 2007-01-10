
/******************************************************************************
* MODULE     : evaluate_main.cpp
* DESCRIPTION: standard style evaluation
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef EVALUATE_MAIN_H
#define EVALUATE_MAIN_H
#include "../Environment/environment.hpp"
#include "../Memorizer/memorizer.hpp"

extern environment std_env;

tree evaluate (tree t);
tree evaluate_error (string message);
tree evaluate_error (string message, tree a1);
tree evaluate_error (string message, array<tree> args);
memorizer evaluate (environment env, tree t);

tree expand (tree t, bool flag);

/* Length arithmetic */
bool is_length (string s);
bool is_length (tree t);
SI as_length (string s);
SI as_length (tree t);
bool is_anylen (tree t);
tree as_tmlen (tree t);
tree tmlen_plus (tree t1, tree t2);
tree tmlen_times (double sc, tree t);
tree tmlen_over (tree t1, tree t2);

/* Macro expansion */
tree evaluate_assign (tree t);
tree evaluate_with (tree t);
tree evaluate_provides (tree t);
tree evaluate_value (tree t);
tree evaluate_quote_value (tree t);
tree evaluate_drd_props (tree t);
tree evaluate_compound (tree t);
tree evaluate_arg (tree t);
tree evaluate_quote_arg (tree t);
tree evaluate_get_label (tree t);
tree evaluate_get_arity (tree t);

/* Quoting and evaluation */
tree evaluate_rewrite (tree t);
tree evaluate_eval_args (tree t);
tree evaluate_quasiquote (tree t);

/* Control structures */
tree evaluate_if (tree t);
tree evaluate_case (tree t);
tree evaluate_while (tree t);
tree evaluate_for_each (tree t);
tree evaluate_use_package (tree t);
tree evaluate_use_module (tree t);

/* Computational markup */
tree evaluate_or (tree t);
tree evaluate_xor (tree t);
tree evaluate_and (tree t);
tree evaluate_not (tree t);
tree evaluate_plus_minus (tree t);
tree evaluate_times_over (tree t);
tree evaluate_divide (tree t);
tree evaluate_modulo (tree t);
tree evaluate_math_sqrt (tree t);
tree evaluate_exp (tree t);
tree evaluate_log (tree t);
tree evaluate_pow (tree t);
tree evaluate_cos (tree t);
tree evaluate_sin (tree t);
tree evaluate_tan (tree t);
tree evaluate_merge (tree t);
tree evaluate_length (tree t);
tree evaluate_range (tree t);
tree evaluate_number (tree t);
tree evaluate_date (tree t);
tree evaluate_translate (tree t);
tree evaluate_change_case (tree t);
tree evaluate_find_file (tree t);
tree evaluate_is_tuple (tree t);
tree evaluate_lookup (tree t);
tree evaluate_equal (tree t);
tree evaluate_unequal (tree t);
tree evaluate_less (tree t);
tree evaluate_lesseq (tree t);
tree evaluate_greater (tree t);
tree evaluate_greatereq (tree t);

/* Length units */
tree evaluate_cm_length ();
tree evaluate_mm_length ();
tree evaluate_in_length ();
tree evaluate_pt_length ();
tree evaluate_bp_length ();
tree evaluate_dd_length ();
tree evaluate_pc_length ();
tree evaluate_cc_length ();
tree evaluate_fs_length ();
tree evaluate_fbs_length ();
tree evaluate_em_length ();
tree evaluate_ln_length ();
tree evaluate_sep_length ();
tree evaluate_yfrac_length ();
tree evaluate_ex_length ();
tree evaluate_fn_length ();
tree evaluate_fns_length ();
tree evaluate_bls_length ();
tree evaluate_spc_length ();
tree evaluate_xspc_length ();
tree evaluate_par_length ();
tree evaluate_pag_length ();
tree evaluate_gw_length ();
tree evaluate_gh_length ();
tree evaluate_tmpt_length ();
tree evaluate_px_length ();
tree evaluate_msec_length ();
tree evaluate_sec_length ();
tree evaluate_min_length ();
tree evaluate_h_length ();

/* Miscellaneous primitives */
tree evaluate_formatting (tree t, string v);
tree evaluate_table (tree t);
tree evaluate_hard_id (tree t);
tree evaluate_script (tree t);
tree evaluate_set_binding (tree t);
tree evaluate_get_binding (tree t);
tree evaluate_point (tree t);
//tree evaluate_box_info (tree t);
//tree evaluate_frame_direct (tree t);
//tree evaluate_frame_inverse (tree t);

#endif // defined EVLUATE_MAIN_H
