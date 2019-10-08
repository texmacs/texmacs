
/******************************************************************************
* MODULE     : env_exec.cpp
* DESCRIPTION: evaluation of trees w.r.t. the environment
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "env.hpp"
#include "convert.hpp"
#include "file.hpp"
#include "image_files.hpp"
#include "scheme.hpp"
#include "page_type.hpp"
#include "typesetter.hpp"
#include "drd_mode.hpp"
#include "dictionary.hpp"

extern int script_status;
extern tree with_package_definitions (string package, tree body);

/******************************************************************************
* Subroutines
******************************************************************************/

string
edit_env_rep::exec_string (tree t) {
  tree r= exec (t);
  if (is_atomic (r)) return r->label;
  else return "";
}

/******************************************************************************
* Rewriting (scheme-like macro expansion)
******************************************************************************/

// Hack to transmit the current environment back to C++
// across the Scheme level, and to maintain reentrancy.
static edit_env current_rewrite_env= edit_env ();

tree
edit_env_rep::rewrite (tree t) {
  switch (L(t)) {
  case EXTERN:
    {
      int i, n= N(t);
      if (n < 1) return tree (ERROR, "invalid extern");
      string fun= tm_decode(exec_string (t[0]));
      tree r (TUPLE, n);
      for (i=1; i<n; i++)
	r[i]= exec (t[i]);
      object expr= null_object ();
      for (i=n-1; i>0; i--)
	expr= cons (object (r[i]), expr);
      expr= cons (string_to_object (fun), expr);
      if (!secure && script_status < 2) {
	if (!as_bool (call ("secure?", expr)))
	  return tree (ERROR, "insecure script");
      }
      edit_env old_env= current_rewrite_env;
      current_rewrite_env= edit_env (this);
      object o= eval (expr);
      current_rewrite_env= old_env;
      return content_to_tree (o);
    }
  case MAP_ARGS:
    {
      if (N(t) < 3 ||
	  !(is_atomic (t[0]) && is_atomic (t[1]) && is_atomic (t[2])))
	return tree (ERROR, "invalid map arguments");
      if (is_nil (macro_arg) || (!macro_arg->item->contains (t[2]->label)))
	return tree (ERROR, "map arguments " * t[2]->label);
      tree v= macro_arg->item [t[2]->label];
      if (is_atomic (v))
	return tree (ERROR, "map arguments " * t[2]->label);
      list<hashmap<string,tree> > old_var= macro_arg;
      list<hashmap<string,path> > old_src= macro_src;
      if (!is_nil (macro_arg)) macro_arg= macro_arg->next;
      if (!is_nil (macro_src)) macro_src= macro_src->next;

      int start= 0, end= N(v);
      if (N(t)>=4) start= as_int (exec (t[3]));
      if (N(t)>=5) end  = as_int (exec (t[4]));
      int i, n= max (0, end-start);
      tree r (make_tree_label (t[1]->label), n);
      if (t[0]->label == "identity")
	for (i=0; i<n; i++)
	  r[i]= tree (ARG, copy (t[2]), as_string (start+i));
      else
	for (i=0; i<n; i++)
	  r[i]= tree (make_tree_label (t[0]->label),
		      tree (ARG, copy (t[2]), as_string (start+i)),
		      as_string (start+i));
      macro_arg= old_var;
      macro_src= old_src;
      return r;
    }
  case VAR_INCLUDE:
    {
      if (N(t) == 0) return tree (ERROR, "invalid include");
      url file_name= url_unix (exec_string (t[0]));
      //cout << "file_name= " << as_tree (file_name) << LF;
      return load_inclusion (relative (base_file_name, file_name));
    }
  case WITH_PACKAGE:
    {
      if (N(t) != 2) return tree (ERROR, "invalid with-package");
      string file_name= exec_string (t[0]);
      return with_package_definitions (file_name, t[1]);
    }
  case REWRITE_INACTIVE:
    {
      if (N(t) == 0 || N(t[0]) == 0)
	return tree (ERROR, "invalid rewrite-inactive");
      if ((!is_func (t[0], ARG)) ||
	  is_compound (t[0][0]) ||
	  is_nil (macro_arg) ||
	  (!macro_arg->item->contains (t[0][0]->label)))
	return tree (ERROR, "invalid rewrite-inactive");
      tree val= macro_arg->item [t[0][0]->label];
      int i, n= N(t[0]);
      for (i=1; i<n; i++) {
	int j= as_int (t[0][i]);
	if ((j>=0) && (j<N(val))) val= val[j];
	else return tree (ERROR, "invalid rewrite-inactive");
      }
      if (N(t) < 2)
	return tree (ERROR, "invalid rewrite-inactive");
      if (t[1] == "recurse") inactive_mode= INACTIVE_INLINE_RECURSE;
      else if (t[1] == "recurse*") inactive_mode= INACTIVE_BLOCK_RECURSE;
      else if (t[1] == "once") inactive_mode= INACTIVE_INLINE_ONCE;
      else if (t[1] == "once*") inactive_mode= INACTIVE_BLOCK_ONCE;
      else if (t[1] == "error") inactive_mode= INACTIVE_INLINE_ERROR;
      else if (t[1] == "error*") inactive_mode= INACTIVE_BLOCK_ERROR;
      else inactive_mode= INACTIVE_INLINE_RECURSE;
      return rewrite_inactive (val, t[0]);
    }
  default:
    return t;
  }
}

tree
edit_env_rep::exec_rewrite (tree t) {
  /*
  cout << "t= " << t << "\n";
  tree r= rewrite (t);
  r= exec (r);
  cout << "r= " << r << "\n";
  return r;
  */
  return exec (rewrite (t));
}

bool
edit_env_rep::exec_until_rewrite (tree t, path p, string var, int level) {
  /*
  cout << "Execute " << t << " (" << var << ", "
       << level << ") until " << p << "\n"
       << "  -> " << rewrite (t) << "\n";
  */
  return exec_until (rewrite (t), p, var, level);
}

tree
texmacs_exec (edit_env env, tree cmd) {
  // re-entrancy
  if (!is_nil (current_rewrite_env)) env= current_rewrite_env;
  return env->exec (cmd);
}

/******************************************************************************
* Evaluation of trees
******************************************************************************/

tree
edit_env_rep::exec (tree t) {
  // cout << "Execute: " << t << "\n";
  if (is_atomic (t)) return t;
  switch (L(t)) {
  case DATOMS:
    return exec_formatting (t, ATOM_DECORATIONS);
  case DLINES:
    return exec_formatting (t, LINE_DECORATIONS);
  case DPAGES:
    return exec_formatting (t, PAGE_DECORATIONS);
  case TFORMAT:
    return exec_formatting (t, CELL_FORMAT);
  case TABLE:
    return exec_table (t);
  case ASSIGN:
    return exec_assign (t);
  case PROVIDE:
    return exec_provide (t);
  case WITH:
    return exec_with (t);
  case PROVIDES:
    return exec_provides (t);
  case VALUE:
    return exec_value (t);
  case QUOTE_VALUE:
    return exec_quote_value (t);
  case OR_VALUE:
    return exec_or_value (t);
  case MACRO:
    return copy (t);
  case DRD_PROPS:
    return exec_drd_props (t);
  case ARG:
    return exec_arg (t);
  case QUOTE_ARG:
    return exec_quote_arg (t);
  case COMPOUND:
    return exec_compound (t);
  case XMACRO:
    return copy (t);
  case GET_LABEL:
    return exec_get_label (t);
  case GET_ARITY:
    return exec_get_arity (t);
  case MAP_ARGS:
    return exec_rewrite (t);
  case EVAL_ARGS:
    return exec_eval_args (t);
  case NEW_THEME:
    return exec_new_theme (t);
  case COPY_THEME:
    return exec_copy_theme (t);
  case APPLY_THEME:
    return exec_apply_theme (t);
  case SELECT_THEME:
    return exec_select_theme (t);
  case MARK:
    if (N(t) < 2)
      return tree (ERROR, "invalid mark");
    return tree (MARK, copy (t[0]), exec (t[1]));
  case EXPAND_AS:
    if (N(t) < 2)
      return tree (ERROR, "invalid expand-as");
    return exec (t[1]);
  case EVAL:
    if (N(t) < 1)
      return tree (ERROR, "invalid eval");
    return exec (exec (t[0]));
  case QUOTE:
    if (N(t) < 1)
      return tree (ERROR, "invalid quote");
    return t[0];
  case QUASI:
    if (N(t) < 1)
      return tree (ERROR, "invalid quasi");
    return exec (exec_quasiquoted (t[0]));
  case QUASIQUOTE:
    if (N(t) < 1)
      return tree (ERROR, "invalid quasiquote");
    return exec_quasiquoted (t[0]);
  case UNQUOTE:
    if (N(t) < 1)
      return tree (ERROR, "invalid unquote");
    return exec (t[0]);
  case VAR_UNQUOTE:
    if (N(t) < 1)
      return tree (ERROR, "invalid var-unquote");
    return exec (t[0]);
  case COPY:
    if (N(t) < 1)
      return tree (ERROR, "invalid copy");
    return copy (exec (t[0]));
  case IF:
  case VAR_IF:
    return exec_if (t);
  case CASE:
    return exec_case (t);
  case WHILE:
    return exec_while (t);
  case FOR_EACH:
    return exec_for_each (t);
  case EXTERN:
    return exec_rewrite (t);
  case VAR_INCLUDE:
    return exec_rewrite (t);
  case WITH_PACKAGE:
    return exec_rewrite (t);
  case USE_PACKAGE:
    return exec_use_package (t);
  case USE_MODULE:
    return exec_use_module (t);

  case OR:
    return exec_or (t);
  case XOR:
    return exec_xor (t);
  case AND:
    return exec_and (t);
  case NOT:
    return exec_not (t);
  case PLUS:
  case MINUS:
    return exec_plus_minus (t);
  case TIMES:
  case OVER:
    return exec_times_over (t);
  case DIV:
    return exec_divide (t);
  case MOD:
    return exec_modulo (t);
  case MINIMUM:
  case MAXIMUM:
    return exec_min_max (t);
  case MATH_SQRT:
    return exec_math_sqrt (t);
  case EXP:
    return exec_exp (t);
  case LOG:
    return exec_log (t);
  case POW:
    return exec_pow (t);
  case COS:
    return exec_cos (t);
  case SIN:
    return exec_sin (t);
  case TAN:
    return exec_tan (t);
  case MERGE:
    return exec_merge (t);
  case LENGTH:
    return exec_length (t);
  case RANGE:
    return exec_range (t);
  case NUMBER:
    return exec_number (t);
  case _DATE:
    return exec_date (t);
  case TRANSLATE:
    return exec_translate (t);
  case CHANGE_CASE:
    return exec_change_case (t);
  case FIND_FILE:
    return exec_find_file (t);
  case FIND_FILE_UPWARDS:
    return exec_find_file_upwards (t);
  case IS_TUPLE:
    return exec_is_tuple (t);
  case LOOK_UP:
    return exec_lookup (t);
  case OCCURS_INSIDE:
    return exec_occurs_inside (t);
  case EQUAL:
    return exec_equal (t);
  case UNEQUAL:
    return exec_unequal (t);
  case LESS:
    return exec_less (t);
  case LESSEQ:
    return exec_lesseq (t);
  case GREATER:
    return exec_greater (t);
  case GREATEREQ:
    return exec_greatereq (t);
  case BLEND:
    return exec_blend (t);

  case CM_LENGTH:
    return exec_cm_length ();
  case MM_LENGTH:
    return exec_mm_length ();
  case IN_LENGTH:
    return exec_in_length ();
  case PT_LENGTH:
    return exec_pt_length ();
  case BP_LENGTH:
    return exec_bp_length ();
  case DD_LENGTH:
    return exec_dd_length ();
  case PC_LENGTH:
    return exec_pc_length ();
  case CC_LENGTH:
    return exec_cc_length ();
  case FS_LENGTH:
    return exec_fs_length ();
  case FBS_LENGTH:
    return exec_fbs_length ();
  case EM_LENGTH:
    return exec_em_length ();
  case LN_LENGTH:
    return exec_ln_length ();
  case SEP_LENGTH:
    return exec_sep_length ();
  case YFRAC_LENGTH:
    return exec_yfrac_length ();
  case EX_LENGTH:
    return exec_ex_length ();
  case FN_LENGTH:
    return exec_fn_length ();
  case FNS_LENGTH:
    return exec_fns_length ();
  case BLS_LENGTH:
    return exec_bls_length ();
  case FNBOT_LENGTH:
    return exec_fnbot_length ();
  case FNTOP_LENGTH:
    return exec_fntop_length ();
  case SPC_LENGTH:
    return exec_spc_length ();
  case XSPC_LENGTH:
    return exec_xspc_length ();
  case PAR_LENGTH:
    return exec_par_length ();
  case PAG_LENGTH:
    return exec_pag_length ();
  case GW_LENGTH:
    return exec_gw_length ();
  case GH_LENGTH:
    return exec_gh_length ();
  case GU_LENGTH:
    return exec_gu_length ();
  case TMPT_LENGTH:
    return exec_tmpt_length ();
  case PX_LENGTH:
    return exec_px_length ();
  case LCORNER_LENGTH:
    return exec_lcorner_length ();
  case BCORNER_LENGTH:
    return exec_bcorner_length ();
  case RCORNER_LENGTH:
    return exec_rcorner_length ();
  case TCORNER_LENGTH:
    return exec_tcorner_length ();
  case MS_LENGTH:
    return exec_ms_length ();
  case S_LENGTH:
    return exec_s_length ();
  case MSEC_LENGTH:
    return exec_msec_length ();
  case SEC_LENGTH:
    return exec_sec_length ();
  case MIN_LENGTH:
    return exec_min_length ();
  case HR_LENGTH:
    return exec_hr_length ();

  case STYLE_WITH:
  case VAR_STYLE_WITH:
    if (N(t) < 1)
      return tree (ERROR, "invalid style-with");
    return exec (t[N(t)-1]);
  case STYLE_ONLY:
  case VAR_STYLE_ONLY:
  case ACTIVE:
  case VAR_ACTIVE:
  case INACTIVE:
  case VAR_INACTIVE:
    return exec_compound (t);
  case REWRITE_INACTIVE:
    return exec_rewrite (t);

  case HARD_ID:
    return exec_hard_id (t);
  case SCRIPT:
    return exec_script (t);
  case FIND_ACCESSIBLE:
    return exec_find_accessible (t);
  case HLINK:
  case ACTION:
    return exec_compound (t);
  case SET_BINDING:
    return exec_set_binding (t);
  case GET_BINDING:
    return exec_get_binding (t);
  case GET_ATTACHMENT:
    return exec_get_attachment (t);

  case PATTERN:
    return exec_pattern (t);

  case ANIM_STATIC:
    return exec_anim_static (t);
  case ANIM_DYNAMIC:
    return exec_anim_dynamic (t);
  case MORPH:
    return exec_morph (t);
  case ANIM_TIME:
    return exec_anim_time ();
  case ANIM_PORTION:
    return exec_anim_portion ();

  case _POINT:
    return exec_point (t);

  case EFF_MOVE:
    return exec_eff_move (t);
  case EFF_MAGNIFY:
    return exec_eff_magnify (t);
  case EFF_BUBBLE:
    return exec_eff_bubble (t);
  case EFF_CROP:
    return exec_eff_crop (t);
  case EFF_TURBULENCE:
    return exec_eff_turbulence (t);
  case EFF_FRACTAL_NOISE:
    return exec_eff_fractal_noise (t);
  case EFF_HATCH:
    return exec_eff_hatch (t);
  case EFF_DOTS:
    return exec_eff_dots (t);
  case EFF_GAUSSIAN:
    return exec_eff_gaussian (t);
  case EFF_OVAL:
    return exec_eff_oval (t);
  case EFF_RECTANGULAR:
    return exec_eff_rectangular (t);
  case EFF_MOTION:
    return exec_eff_motion (t);
  case EFF_DEGRADE:
    return exec_eff_degrade (t);
  case EFF_DISTORT:
    return exec_eff_distort (t);
  case EFF_GNAW:
    return exec_eff_gnaw (t);

  case BOX_INFO:
    return exec_box_info (t);
  case FRAME_DIRECT:
    return exec_frame_direct (t);
  case FRAME_INVERSE:
    return exec_frame_inverse (t);

  default:
    if (L(t) < START_EXTENSIONS) {
      int i, n= N(t);
      // cout << "Executing " << t << "\n";
      tree r (t, n);
      for (i=0; i<n; i++) r[i]= exec (t[i]);
      // cout << "Executed " << t << " -> " << r << "\n";
      return r;
    }
    else return exec_compound (t);
  }
}

tree
edit_env_rep::exec_formatting (tree t, string v) {
  int i, n= N(t);
  if (n < 1)
    return tree (ERROR, "bad formatting");
  tree r (t, n);
  for (i=0; i<n-1; i++) r[i]= exec (t[i]);
  tree oldv= read (v);
  tree newv= oldv * r (0, n-1);
  // monitored_write_update (v, newv);
  write_update (v, newv);
  r[n-1]= exec (t[n-1]);
  write_update (v, oldv);
  return r;
}

tree
edit_env_rep::exec_table (tree t) {
  tree oldv= read (CELL_FORMAT);
  // should execute values in oldv
  // monitored_write_update (CELL_FORMAT, tree (TFORMAT));
  write_update (CELL_FORMAT, tree (TFORMAT));
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++) r[i]= exec (t[i]);
  write_update (CELL_FORMAT, oldv);
  return r;
}

tree
edit_env_rep::exec_assign (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad assign");
  tree r= exec (t[0]);
  if (is_compound (r)) return tree (ERROR, "bad assign");
  assign (r->label, copy (t[1]));
  tree v= read (r->label);
  if (is_atomic (v) || is_func (v, MACRO));
  else v= tree (QUOTE, v);
  return tree (ASSIGN, r, v);
}

tree
edit_env_rep::exec_provide (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad provide");
  tree r= exec (t[0]);
  if (is_compound (r)) return tree (ERROR, "bad provide");
  if (provides (t->label)) return "";
  assign (r->label, copy (t[1]));
  tree v= read (r->label);
  if (is_atomic (v) || is_func (v, MACRO));
  else v= tree (QUOTE, v);
  return tree (ASSIGN, r, v);
}

tree
edit_env_rep::exec_with (tree t) {
  int i, n= N(t), k= (n-1)>>1; // is k=0 allowed ?
  if ((n&1) != 1) return tree (ERROR, "bad with");
  STACK_NEW_ARRAY(vars,string,k);
  STACK_NEW_ARRAY(oldv,tree,k);
  STACK_NEW_ARRAY(newv,tree,k);
  for (i=0; i<k; i++) {
    tree var_t= exec (t[i<<1]);
    if (is_atomic (var_t)) {
      string var= var_t->label;
      vars[i]= var;
      oldv[i]= read (var);
      newv[i]= exec (t[(i<<1)+1]);
    }
    else {
      STACK_DELETE_ARRAY(vars);
      STACK_DELETE_ARRAY(oldv);
      STACK_DELETE_ARRAY(newv);
      return tree (ERROR, "bad with");
    }
  }

  // for (i=0; i<k; i++) monitored_write_update (vars[i], newv[i]);
  for (i=0; i<k; i++) write_update (vars[i], newv[i]);
  tree r= exec (t[n-1]);
  for (i=k-1; i>=0; i--) write_update (vars[i], oldv[i]);

  tree u (WITH, n);
  for (i=0; i<k; i++) {
    u[i<<1]    = vars[i];
    u[(i<<1)+1]= tree (QUOTE, newv[i]);
  }
  u[n-1]= r;
  STACK_DELETE_ARRAY(vars);
  STACK_DELETE_ARRAY(oldv);
  STACK_DELETE_ARRAY(newv);
  return u;
}

tree
edit_env_rep::exec_compound (tree t) {
  int d; tree f;
  if (L(t) == COMPOUND) {
    if (N(t)<1) return tree (ERROR, "bad compound");
    d= 1;
    f= t[0];
    if (is_compound (f)) f= exec (f);
    if (is_atomic (f)) {
      string var= f->label;
      if (!provides (var)) return tree (ERROR, "compound " * var);
      f= read (var);
    }
  }
  else {
    string var= as_string (L(t));
    if (!provides (var)) return tree (ERROR, "compound " * var);
    d= 0;
    f= read (var);
  }

  if (is_applicable (f)) {
    int i, n=N(f)-1, m=N(t)-d;
    macro_arg= list<hashmap<string,tree> > (
      hashmap<string,tree> (UNINIT), macro_arg);
    macro_src= list<hashmap<string,path> > (
      hashmap<string,path> (path (DECORATION)), macro_src);
    if (L(f) == XMACRO) {
      if (is_atomic (f[0]))
	macro_arg->item (f[0]->label)= t;
    }
    else for (i=0; i<n; i++)
      if (is_atomic (f[i])) {
	tree st= i<m? t[i+d]: tree (UNINIT);
	macro_arg->item (f[i]->label)= st;
	macro_src->item (f[i]->label)= obtain_ip (st);
      }
    tree r= exec (f[n]);
    macro_arg= macro_arg->next;
    macro_src= macro_src->next;
    return r;
  }
  else return exec (f);
}

tree
edit_env_rep::exec_drd_props (tree t) {
  int i, n= N(t);
  if ((n>=3) && is_atomic (t[0]))
    for (i=1; i<n-1; i+=2) {
      if (!is_atomic (t[i])) continue;
      string var  = t[0]->label;
      string prop = t[i]->label;
      tree   val  = t[i+1];
      tree_label l= make_tree_label (var);
      if (prop == "arity") {
	if (is_tuple (val, "repeat", 2))
	  drd->set_arity (l, as_int (val [1]), as_int (val [2]),
			  ARITY_REPEAT, CHILD_BIFORM);
	else if (is_tuple (val, "repeat*", 2))
	  drd->set_arity (l, as_int (val [1]), as_int (val [2]),
			  ARITY_VAR_REPEAT, CHILD_BIFORM);
	else if (is_tuple (val, "options", 2))
	  drd->set_arity (l, as_int (val [1]), as_int (val [2]),
			  ARITY_OPTIONS, CHILD_BIFORM);
	else
	  drd->set_arity (l, as_int (val), 0,
			  ARITY_NORMAL, CHILD_DETAILED);
	drd->freeze_arity (l);
      }
      else if (prop == "name") {
	if (is_atomic (val))
	  drd->set_attribute (l, prop, val->label);
      }
      else if (prop == "syntax")
        drd->set_syntax (l, val);
      else if (prop == "border") {
	if (val == "yes") drd->set_border (l, BORDER_YES);
	if (val == "inner") drd->set_border (l, BORDER_INNER);
	if (val == "outer") drd->set_border (l, BORDER_OUTER);
	if (val == "no") drd->set_border (l, BORDER_INNER);
	drd->freeze_border (l);
      }
      else if (prop == "with-like") {
	if (val == "yes") drd->set_with_like (l, true);
	if (val == "no") drd->set_with_like (l, false);
	drd->freeze_with_like (l);
      }
      else if (prop == "locals") {
	int i, n= drd->get_nr_indices (l);
	for (i=0; i<n; i++) {
	  drd->set_env (l, i, val);
	  drd->freeze_env (l, i);
	}
      }
      else if (prop == "unaccessible" ||
	       prop == "hidden" ||
	       prop == "accessible")
	{
	  int prop_code= ACCESSIBLE_NEVER;
	  if (prop == "hidden") prop_code= ACCESSIBLE_HIDDEN;
	  if (prop == "accessible") prop_code= ACCESSIBLE_ALWAYS;
	  if (val == "none") prop_code= ACCESSIBLE_NEVER;
	  if (is_int (val)) {
	    int i= as_int (val);
	    drd->set_accessible (l, i, prop_code);
	    drd->freeze_accessible (l, i);
	  }
	  else if (val == "none" || val == "all") {
	    int i, n= drd->get_nr_indices (l);
	    for (i=0; i<n; i++) {
	      drd->set_accessible (l, i, prop_code);
	      drd->freeze_accessible (l, i);
	    }
	  }
	}
      else if (prop == "normal-writability" ||
               prop == "disable-writability" ||
               prop == "enable-writability")
	{
	  int prop_code= WRITABILITY_NORMAL;
	  if (prop == "disable-writability") prop_code= WRITABILITY_DISABLE;
	  if (prop == "enable-writability") prop_code= WRITABILITY_ENABLE;
	  if (is_int (val)) {
	    int i= as_int (val);
	    drd->set_writability (l, i, prop_code);
	    drd->freeze_writability (l, i);
	  }
	  else if (val == "all") {
	    int i, n= drd->get_nr_indices (l);
	    for (i=0; i<n; i++) {
	      drd->set_writability (l, i, prop_code);
	      drd->freeze_writability (l, i);
	    }
	  }
	}
      else if (prop == "returns" && drd_encode_type (as_string (val)) >= 0) {
	drd->set_type (l, drd_encode_type (as_string (val)));
	drd->freeze_type (l);
      }
      else if (prop == "parameter" &&
               drd_encode_type (as_string (val)) >= 0) {
        drd->set_var_type (l, VAR_PARAMETER);
	drd->set_type (l, drd_encode_type (as_string (val)));
	drd->freeze_var_type (l);
	drd->freeze_type (l);
      }
      else if (prop == "macro-parameter" &&
               drd_encode_type (as_string (val)) >= 0) {
        drd->set_var_type (l, VAR_MACRO_PARAMETER);
	drd->set_type (l, drd_encode_type (as_string (val)));
	drd->freeze_var_type (l);
	drd->freeze_type (l);
      }
      else if (drd_encode_type (prop) >= 0) {
	int tp= drd_encode_type (prop);
	if (is_int (val)) {
	  int i= as_int (val);
	  drd->set_type (l, i, tp);
	  drd->freeze_type (l, i);
	}
	else if (val == "all") {
	  int i, n= drd->get_nr_indices (l);
	  for (i=0; i<n; i++) {
	    drd->set_type (l, i, tp);
	    drd->freeze_type (l, i);
	  }
	}
      }
    }
  return t;
}

tree
edit_env_rep::exec_provides (tree t) {
  if (N(t)<1) return tree (ERROR, "bad provides");
  tree r= exec (t[0]);
  if (is_compound (r)) return tree (ERROR, "bad provides");
  if (provides (r->label)) return "true"; else return "false";
}

tree
edit_env_rep::exec_value (tree t) {
  if (N(t)<1) return tree (ERROR, "bad value");
  tree r= exec (t[0]);
  if (is_compound (r)) return tree (ERROR, "bad value");
  return exec (read (r->label));
}

tree
edit_env_rep::exec_quote_value (tree t) {
  if (N(t)<1) return tree (ERROR, "bad quote-value");
  tree r= exec (t[0]);
  if (is_compound (r)) return tree (ERROR, "bad quote-value");
  return read (r->label);
}

tree
edit_env_rep::exec_or_value (tree t) {
  for (int i=0; i<N(t); i++) {
    tree r= exec (t[i]);
    if (is_compound (r)) return tree (ERROR, "bad value");
    string name= r->label;
    if (provides (name))
      if (i == N(t)-1 || L(read (name)) != UNINIT)
        return exec (read (r->label));
  }
  return "";
}

tree
edit_env_rep::exec_arg (tree t) {
  if (N(t)<1) return tree (ERROR, "bad arg");
  tree r= t[0];
  if (is_compound (r))
    return tree (ERROR, "bad arg");
  if (is_nil (macro_arg) || (!macro_arg->item->contains (r->label)))
    return tree (ERROR, "arg " * r->label);
  r= macro_arg->item [r->label];
  list<hashmap<string,tree> > old_var= macro_arg;
  list<hashmap<string,path> > old_src= macro_src;
  if (!is_nil (macro_arg)) macro_arg= macro_arg->next;
  if (!is_nil (macro_src)) macro_src= macro_src->next;
  bool err= false;
  if (N(t) > 1) {
    int i, n= N(t);
    for (i=1; i<n; i++) {
      tree u= exec (t[i]);
      if (!is_int (u)) { err= true; break; }
      int nr= as_int (u);
      if ((!is_compound (r)) || (nr<0) || (nr>=N(r))) { err= true; break; }
      r= r[nr];
    }
  }
  if (err) r= tree (ERROR, "arg " * r->label);
  else r= exec (r);
  macro_arg= old_var;
  macro_src= old_src;
  return r;
}

static bool quote_substitute= false;

tree
edit_env_rep::exec_quote_arg (tree t) {
  if (N(t)<1) return tree (ERROR, "bad quote-arg");
  tree r= t[0];
  if (is_compound (r))
    return tree (ERROR, "bad quote-arg");
  if (is_nil (macro_arg) || (!macro_arg->item->contains (r->label)))
    return tree (ERROR, "quoted argument " * r->label);
  r= macro_arg->item [r->label];
  if (N(t) > 1) {
    int i, n= N(t);
    for (i=1; i<n; i++) {
      tree u= exec (t[i]);
      if (!is_int (u)) break;
      int nr= as_int (u);
      if ((!is_compound (r)) || (nr<0) || (nr>=N(r))) break;
      r= r[nr];
    }
  }
  if (quote_substitute && !is_func (r, ARG)) {
    int i, n= N(r);
    tree s (r, n);
    for (i=0; i<n; i++)
      s[i]= tree (ARG, A(t)) * tree (ARG, as_string (i));
    return s;
  }
  return r;
}

tree
edit_env_rep::exec_get_label (tree t) {
  if (N(t)<1) return tree (ERROR, "bad get-label");
  tree r= exec (t[0]);
  return copy (as_string (L(r)));
}

tree
edit_env_rep::exec_get_arity (tree t) {
  if (N(t)<1) return tree (ERROR, "bad get-arity");
  tree r= exec (t[0]);
  return as_string (arity (r));
}

tree
edit_env_rep::exec_eval_args (tree t) {
  if (N(t)<1) return tree (ERROR, "bad eval-args");
  if(is_nil(macro_arg)) return tree(ERROR, "nil argument");
  tree v= macro_arg->item [as_string (t[0])];
  if (is_atomic (v)) return tree (ERROR, "eval arguments " * t[0]->label);
  list<hashmap<string,tree> > old_var= macro_arg;
  list<hashmap<string,path> > old_src= macro_src;
  if (!is_nil (macro_arg)) macro_arg= macro_arg->next;
  if (!is_nil (macro_src)) macro_src= macro_src->next;

  int i, n= N(v);
  tree r (v, n);
  for (i=0; i<n; i++)
    r[i]= exec (v[i]);

  macro_arg= old_var;
  macro_src= old_src;
  return r;
}

tree
edit_env_rep::exec_new_theme (tree t) {
  if (N(t)<1 || !is_atomic (t[0]))
    return tree (ERROR, "bad new-theme");
  string theme= t[0]->label;
  array<string> attrs, inhs;
  for (int i=1; i<N(t); i++) {
    tree x= exec (t[i]);
    if (!is_atomic (x)) return tree (ERROR, "bad variable in new-theme");
    string var= x->label;
    if (provides ("with-" * var)) inhs << var;
    else attrs << var;
  }
  tree val (WITH), cc (CONCAT);
  for (int i=0; i<N(attrs); i++) {
    string oldv= attrs[i];
    string newv= theme * "-" * attrs[i];
    cc << tree (ASSIGN, newv, tree (VALUE, oldv));
    val << tree (oldv) << tree (VALUE, newv);
  }
  val << tree (ARG, "body");
  for (int i=N(inhs)-1; i>=0; i--) {
    string winh= "with-" * inhs[i];
    val= compound (winh, val);
  }
  val= tree (MACRO, "body", val);
  cc << tree (ASSIGN, "with-" * theme, val);
  return exec (cc);
}

tree
edit_env_rep::exec_copy_theme (tree t) {
  if (N(t)<1 || !is_atomic (t[0]))
    return tree (ERROR, "bad copy-theme");
  string new_theme= t[0]->label;
  tree rew (NEW_THEME, new_theme);
  array<tree> a;
  for (int k=1; k<N(t); k++) {
    string old_theme= t[k]->label;
    if (!provides ("with-" * old_theme))
      return tree (ERROR, "missing theme '" * old_theme * "'");
    tree val= read ("with-" * old_theme);
    if (is_func (val, MACRO, 2)) val= val[1];
    while (is_compound (val, 1) && !is_func (val, WITH)) {
      string lab= as_string (L(val));
      if (starts (lab, "with-")) rew << tree (lab (5, N(lab)));
      val= val[N(val)-1];
    }
  }
  for (int k=1; k<N(t); k++) {
    string old_theme= t[k]->label;
    tree val= read ("with-" * old_theme);
    if (is_func (val, MACRO, 2)) val= val[1];
    while (is_compound (val, 1) && !is_func (val, WITH))
      val= val[N(val)-1];
    if (is_func (val, WITH))
      for (int i=0; i+2<N(val); i+=2)
        if (is_atomic (val[i])) {
          rew << val[i];
          string var= new_theme * "-" * val[i]->label;
          a << tree (ASSIGN, var, val[i+1]);
        }
  }
  rew= tree (CONCAT, rew);
  rew << a;
  return exec (rew);
}

tree
edit_env_rep::exec_apply_theme_sub (string var) {
  tree val= read (var);
  if (!is_func (val, MACRO, 2)) return "";
  val= val[1];
  tree r (CONCAT);
  while (is_compound (val, 1) && !is_func (val, WITH)) {
    string lab= as_string (L(val));
    r << exec_apply_theme_sub (lab);
  }
  if (is_func (val, WITH))
    for (int i=0; i+2<N(val); i+=2)
      r << exec (tree (ASSIGN, val[i], val[i+1]));
  if (N(r) == 0) return "";
  else if (N(r) == 1) return r[0];
  else return r;
}

tree
edit_env_rep::exec_apply_theme (tree t) {
  if (N(t) != 1 || !is_atomic (t[0]))
    return tree (ERROR, "bad apply-theme");
  string theme= t[0]->label;
  if (!provides ("with-" * theme))
    return tree (ERROR, "missing theme '" * theme * "'");
  return exec_apply_theme_sub ("with-" * theme);
}

tree
edit_env_rep::exec_select_theme_sub (string theme, string from) {
  tree r (CONCAT);
  tree val= read ("with-" * from);
  if (is_func (val, MACRO, 2)) val= val[1];
  while (is_compound (val, 1) && !is_func (val, WITH)) {
    string lab= as_string (L(val));
    if (starts (lab, "with-"))
      r << A(exec_select_theme_sub (theme, lab (5, N(lab))));
    val= val[N(val)-1];
  }
  if (is_func (val, WITH))
    for (int i=0; i+2<N(val); i+=2)
      if (is_atomic (val[i]))
        r << tree (ASSIGN, theme * "-" * val[i]->label, val[i+1]);
  return r;
}

tree
edit_env_rep::exec_select_theme (tree t) {
  if (N(t)<1 || !is_atomic (t[0]))
    return tree (ERROR, "bad copy-theme");
  string theme= t[0]->label;
  tree r (CONCAT);
  for (int k=1; k<N(t); k++) {
    string from= t[k]->label;
    if (!provides ("with-" * from))
      return tree (ERROR, "missing theme '" * from * "'");
    r << A(exec_select_theme_sub (theme, from));
  }
  return exec (r);
}

tree
edit_env_rep::exec_quasiquoted (tree t) {
  if (is_atomic (t)) return t;
  else if (is_func (t, UNQUOTE, 1)) return exec (t[0]);
  else {
    int i, n= N(t);
    tree r (L(t));
    for (i=0; i<n; i++) {
      if (is_func (t[i], VAR_UNQUOTE, 1)) {
	tree ins= exec (t[i]);
	if (is_compound (ins)) r << A(ins);
	else r << tree (ERROR, "bad unquote*");
      }
      else r << exec_quasiquoted (t[i]);
    }
    return r;
  }
}

tree
edit_env_rep::exec_if (tree t) {
  // This case must be kept consistent with
  // concater_rep::typeset_if(tree, path)
  // in ../Concat/concat_active.cpp
  if ((N(t)!=2) && (N(t)!=3)) return tree (ERROR, "bad if");
  tree tt= exec (t[0]);
  if (is_compound (tt) || !is_bool (tt->label))
    return tree (ERROR, "bad if");
  if (as_bool (tt->label)) return exec (t[1]);
  if (N(t)==3) return exec (t[2]);
  return "";
}

tree
edit_env_rep::exec_case (tree t) {
  // This case must be kept consistent with
  // concater_rep::typeset_case(tree, path)
  // in ../Concat/concat_active.cpp
  if (N(t)<2) return tree (ERROR, "bad case");
  int i, n= N(t);
  for (i=0; i<(n-1); i+=2) {
    tree tt= exec (t[i]);
    if (is_compound (tt) || ! is_bool (tt->label))
      return tree (ERROR, "bad case");
    if (as_bool (tt->label)) return exec (t[i+1]);
  }
  if (i<n) return exec (t[i]);
  return "";
}

tree
edit_env_rep::exec_while (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad while");
  tree r (CONCAT);
  while (1) {
    tree tt= exec (t[0]);
    if (is_compound (tt)) return tree (ERROR, "bad while");
    if (! is_bool (tt->label)) return tree (ERROR, "bad while");
    if (! as_bool(tt->label)) break;
    r << exec (t[1]);
  }
  if (N(r) == 0) return "";
  if (N(r) == 1) return r[0];
  return r;
}

tree
edit_env_rep::exec_for_each (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad for-each");
  tree fun = exec (t[0]);
  tree args= exec (t[1]);
  if (!is_tuple (args)) return tree (ERROR, "bad for-each");
  int i, n= N(args);
  for (i=0; i<n; i++)
    exec (tree (COMPOUND, fun, args[i]));
  return "";
}

static tree
filter_style (tree t) {
  if (is_atomic (t)) return t;
  else switch (L(t)) {
  case STYLE_WITH:
  case VAR_STYLE_WITH:
    return filter_style (t[N(t)-1]);
  case STYLE_ONLY:
  case VAR_STYLE_ONLY:
    if (is_atomic (t[0])) return "";
    else return filter_style (t[0][N(t[0])-1]);
  case ACTIVE:
  case VAR_ACTIVE:
  case INACTIVE:
  case VAR_INACTIVE:
    return filter_style (t[0]);
  default:
    {
      int i, n= N(t);
      tree r (t, n);
      for (i=0; i<n; i++)
	r[i]= filter_style (t[i]);
      return r;
    }
  }
}

tree
edit_env_rep::exec_use_package (tree t) {
  int i, n= N(t);
  for (i=0; i<n; i++) {
    //cout << "Package " << as_string (t[i]) << "\n";
    url name= url_none ();
    url styp= "$TEXMACS_STYLE_PATH";
    if (is_rooted (base_file_name, "default"))
      styp= styp | ::expand (head (base_file_name) * url_ancestor ());
    else styp= styp | head (base_file_name);
    if (ends (as_string (t[i]), ".ts")) name= as_string (t[i]);
    else name= styp * (as_string (t[i]) * string (".ts"));
    name= resolve (name);
    //cout << as_string (t[i]) << " -> " << name << "\n";
    string doc_s;
    if (!load_string (name, doc_s, false)) {
      tree doc= texmacs_document_to_tree (doc_s);
      if (is_compound (doc))
	exec (filter_style (extract (doc, "body")));
    }
  }
  return "";
}

tree
edit_env_rep::exec_use_module (tree t) {
  int i, n= N(t);
  for (i=0; i<n; i++) {
    string s= exec_string (t[i]);
    if (starts (s, "(")) eval ("(use-modules " * s * ")");
    else if (s != "") eval ("(plugin-initialize '" * s * ")");
    assign (THE_MODULES, read (THE_MODULES) * tuple (s));
  }
  return "";
}

tree
edit_env_rep::exec_or (tree t) {
  if (N(t) < 2) return tree (ERROR, "bad or");
  for (int i=0; i<N(t); i++) {
    tree ti= exec (t[i]);
    if (ti != "false") return ti;
  }
  return as_string_bool (false);
}

tree
edit_env_rep::exec_xor (tree t) {
  if (N(t) != 2) return tree (ERROR, "bad xor");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2)) return tree (ERROR, "bad xor");
  if (!is_bool (t1->label) || !is_bool (t2->label))
    return tree (ERROR, "bad xor");
  return as_string_bool (as_bool (t1->label) ^ as_bool (t2->label));
}

tree
edit_env_rep::exec_and (tree t) {
  if (N(t) < 2) return tree (ERROR, "bad and");
  for (int i=0; i<N(t)-1; i++) {
    tree ti= exec (t[i]);
    if (ti == "false") return ti;
  }
  return exec (t[N(t)-1]);
}

tree
edit_env_rep::exec_not (tree t) {
  if (N(t) != 1) return tree (ERROR, "bad not");
  tree tt= exec (t[0]);
  if (tt == "false") return "true";
  else return "false";
}

tree
edit_env_rep::exec_plus_minus (tree t) {
  int i, n= N(t);
  if (n==0) return tree (ERROR, "bad plus/minus");
  tree inc= exec (t[0]);
  if (is_double (inc)) {
    double acc= as_double (inc);
    if ((n==1) && is_func (t, MINUS))
      acc= -acc;
    for (i=1; i<n; i++) {
      tree inc= exec (t[i]);
      if (!is_double (inc))
	return tree (ERROR, "bad plus/minus");
      if ((i == n-1) && is_func (t, MINUS))
	acc -= as_double (inc);
      else acc += as_double (inc);
    }
    return as_string (acc);
  }
  else if (is_anylen (inc)) {
    tree acc= as_tmlen (inc);
    if ((n==1) && is_func (t, MINUS))
      acc= tmlen_times (-1, acc);
    for (i=1; i<n; i++) {
      tree inc= exec (t[i]);
      if (!is_anylen (inc))
	return tree (ERROR, "bad plus/minus");
      inc= as_tmlen (inc);
      if ((i == n-1) && is_func (t, MINUS))
	inc= tmlen_times (-1, inc);
      acc= tmlen_plus (acc, inc);
    }
    return acc;
  }
  else return tree (ERROR, "bad plus/minus");
}

tree
edit_env_rep::exec_min_max (tree t) {
  int i, n= N(t);
  if (n==0) return tree (ERROR, "bad min/max");
  tree first= exec (t[0]);
  if (is_double (first)) {
    double ret= as_double (first);
    for (i=1; i<n; i++) {
      tree next= exec (t[i]);
      if (!is_double (next))
	return tree (ERROR, "bad min/max");
      if (is_func (t, MINIMUM))
	ret= min (ret, as_double (next));
      else
	ret= max (ret, as_double (next));
    }
    return as_string (ret);
  }
  else if (is_anylen (first)) {
    tree ret= as_tmlen (first);
    if ((n==1) && is_func (t, MINUS))
      ret= tmlen_times (-1, ret);
    for (i=1; i<n; i++) {
      tree next= exec (t[i]);
      if (!is_anylen (next))
	return tree (ERROR, "bad min/max");
      next= as_tmlen (next);
      if (is_func (t, MINIMUM))
	ret= tmlen_min (ret, next);
      else
	ret= tmlen_max (ret, next);
    }
    return ret;
  }
  else return tree (ERROR, "bad min/max");
}

tree
edit_env_rep::exec_times_over (tree t) {
  int i, n= N(t);
  if (n==0) return tree (ERROR, "bad times/over");
  tree prod= exec (t[0]);
  if (is_double (prod));
  else if (is_anylen (prod)) prod= as_tmlen (prod);
  else if (is_percentage (prod)) prod= as_tree (as_percentage (prod));
  else return tree (ERROR, "bad times/over");
  if ((n==1) && is_func (t, OVER)) {
    if (is_double (prod)) return as_string (1 / as_double (prod));
    else return tree (ERROR, "bad times/over");
  }
  // cout << t << "\n";
  // cout << "  0\t" << prod << "\n";
  for (i=1; i<n; i++) {
    tree mul= exec (t[i]);
    if (is_double (mul)) {
      double _mul= as_double (mul);
      if ((i == n-1) && is_func (t, OVER))
	_mul= 1 / _mul;
      if (is_double (prod))
	prod= as_string (_mul * as_double (prod));
      else prod= tmlen_times (_mul, prod);
    }
    else if (is_anylen (mul)) {
      mul= as_tmlen (mul);
      if ((i == n-1) && is_func (t, OVER)) {
	if (!is_func (prod, TMLEN))
	  return tree (ERROR, "bad times/over");
	return tmlen_over (prod, mul);
      }
      if (is_double (prod))
	prod= tmlen_times (as_double (prod), mul);
      else return tree (ERROR, "bad times/over");
    }
    else if (is_percentage (mul)) {
      double _mul= as_percentage (mul);
      if (is_double (prod))
	prod= as_string (_mul * as_double (prod));
      else prod= tmlen_times (_mul, prod);
    }
    else return tree (ERROR, "bad times/over");
    // cout << "  " << i << "\t" << prod << "\n";
  }
  return prod;
}

tree
edit_env_rep::exec_divide (tree t) {
  /* this doesn't match the documentation */
  if (N(t)!=2) return tree (ERROR, "bad divide");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return tree (ERROR, "bad divide");
  if (is_int (t1->label) && (is_int (t2->label))) {
    int den= as_int (t2->label);
    if (den == 0) return tree (ERROR, "division by zero");
    return as_string (as_int (t1->label) / den);
  }
  if (is_double (t1->label) && (is_double (t2->label))) {
    double den= as_double (t2->label);
    if (den == 0) return tree (ERROR, "division by zero");
    return as_string (floor (as_double (t1->label) / den));
  }
  if (is_anylen (t1->label) && (is_anylen (t2->label)))
    return as_string (tmlen_div (as_tmlen (t1), as_tmlen (t2)));
  return tree (ERROR, "bad divide");
}

tree
edit_env_rep::exec_modulo (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad modulo");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return tree (ERROR, "bad modulo");
  if (is_int (t1->label) && (is_int (t2->label))) {
    int den= as_int (t2->label);
    if (den == 0) return tree (ERROR, "modulo zero");
    return as_string (as_int (t1->label) % den);
  }
  if (is_double (t1->label) && (is_double (t2->label))) {
    double num= as_double (t1->label);
    double den= as_double (t2->label);
    if (den == 0) return tree (ERROR, "modulo zero");
    double div= floor (num / den);
    return as_string (num - div * den);
  }
  if (is_anylen (t1->label) && (is_anylen (t2->label)))
    return tmlen_mod (as_tmlen (t1), as_tmlen (t2));
  return tree (ERROR, "bad modulo");
}

tree
edit_env_rep::exec_math_sqrt (tree t) {
  if (N(t)!=1) return tree (ERROR, "bad sqrt");
  tree t1= exec (t[0]);
  if (is_double (t1))
    return as_tree (sqrt (as_double (t1)));
  return tree (ERROR, "bad sqrt");
}

tree
edit_env_rep::exec_exp (tree t) {
  if (N(t)!=1) return tree (ERROR, "bad exp");
  tree t1= exec (t[0]);
  if (is_double (t1))
    return as_tree (exp (as_double (t1)));
  return tree (ERROR, "bad exp");
}

tree
edit_env_rep::exec_log (tree t) {
  if (N(t)!=1) return tree (ERROR, "bad log");
  tree t1= exec (t[0]);
  if (is_double (t1))
    return as_tree (log (as_double (t1)));
  return tree (ERROR, "bad log");
}

tree
edit_env_rep::exec_pow (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad pow");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_double (t1) && is_double (t2))
    return as_tree (pow (as_double (t1), as_double (t2)));
  return tree (ERROR, "bad pow");
}

tree
edit_env_rep::exec_cos (tree t) {
  if (N(t)!=1) return tree (ERROR, "bad cos");
  tree t1= exec (t[0]);
  if (is_double (t1))
    return as_tree (cos (as_double (t1)));
  return tree (ERROR, "bad cos");
}

tree
edit_env_rep::exec_sin (tree t) {
  if (N(t)!=1) return tree (ERROR, "bad sin");
  tree t1= exec (t[0]);
  if (is_double (t1))
    return as_tree (sin (as_double (t1)));
  return tree (ERROR, "bad sin");
}

tree
edit_env_rep::exec_tan (tree t) {
  if (N(t)!=1) return tree (ERROR, "bad tan");
  tree t1= exec (t[0]);
  if (is_double (t1))
    return as_tree (tan (as_double (t1)));
  return tree (ERROR, "bad tan");
}

tree
edit_env_rep::exec_merge (tree t) {
  int i, n= N(t);
  if (n == 0) return "";
  tree acc= exec (t[0]);
  if (is_concat (acc)) acc= tree_as_string (acc);
  for (i=1; i<n; i++) {
    tree add= exec (t[i]);
    if (is_atomic (acc) &&
	(is_atomic (add) || is_concat (add) || is_document (add)))
      acc= acc->label * tree_as_string (add);
    else if (is_tuple (acc) && is_tuple (add))
      acc= acc * add;
    else if (is_func (acc, MACRO) && is_func (add, MACRO) &&
	     (N(acc) == N(add)) &&
	     (acc (0, N(acc)-1) == add (0, N(add)-1)))
      {
	tree r = copy (acc);
	tree u1= copy (acc[N(acc)-1]);
	tree u2= copy (add[N(add)-1]);
	tree u (CONCAT, u1, u2);
	if (u1 == "") u= u2;
	else if (u2 == "") u= u1;
	else if (is_atomic (u1) && is_atomic (u2))
	  u= u1->label * u2->label;
	r[N(r)-1]= u;
	acc= r;
      }
    else {
      //cout << "acc= " << acc << "\n";
      //cout << "add= " << add << "\n";
      return tree (ERROR, "bad merge");
    }
  }
  return acc;
}

tree
edit_env_rep::exec_length (tree t) {
  if (N(t)!=1) return tree (ERROR, "bad length");
  tree t1= exec (t[0]);
  if (is_compound (t1)) {
    if (is_tuple (t1)) return as_string (N (t1));
    return tree (ERROR, "bad length");
  }
  return as_string (N (t1->label));
}

tree
edit_env_rep::exec_range (tree t) {
  if (N(t)!=3) return tree (ERROR, "bad range");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  tree t3= exec (t[2]);
  if (!(is_int (t2) && is_int (t3))) return tree (ERROR, "bad range");
  if (is_compound (t1)) {
    if (is_tuple (t1)) {
      int i1= max (0, as_int (t2));
      int i2= min (N (t1), as_int (t3));
      i2 = max (i1, i2);
      return t1 (i1, i2);
    }
    return tree (ERROR, "bad range");
  }
  int i1= max (0, as_int (t2));
  int i2= min (N(t1->label), as_int (t3));
  i2 = max (i1, i2);
  return t1->label (i1, i2);
}

tree
edit_env_rep::exec_number (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad number");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return tree (ERROR, "bad number");
  string s1= t1->label;
  string s2= t2->label;
  int nr= as_int (s1);
  if (s2 == "arabic") return as_string (nr);
  if (s2 == "roman") return roman_nr (nr);
  if (s2 == "Roman") return Roman_nr (nr);
  if (s2 == "alpha") return alpha_nr (nr);
  if (s2 == "Alpha") return Alpha_nr (nr);
  if (s2 == "fnsymbol")
    return tree (WITH, MODE, "math", tree (RIGID, fnsymbol_nr (nr)));
  return tree (ERROR, "bad number");
}

tree
edit_env_rep::exec_date (tree t) {
  if (N(t)>2) return tree (ERROR, "bad date");
  string lan= get_string (LANGUAGE);
  if (N(t) == 2) {
    tree u= exec (t[1]);
    if (is_compound (u)) return tree (ERROR, "bad date");
    lan= u->label;
  }
  string fm= "";
  if (N(t) != 0) {
    tree u= exec (t[0]);
    if (is_compound (u)) return tree (ERROR, "bad date");
    fm= u->label;
  }
  return get_date (lan, fm);
}

tree
edit_env_rep::exec_translate (tree t) {
  if (N(t)!=3) return tree (ERROR, "bad translate");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  tree t3= exec (t[2]);
  if (is_compound (t1) || is_compound (t2) || is_compound (t3)) return t1;
  return translate (t1->label, t2->label, t3->label);
}

tree
edit_env_rep::exec_change_case (tree t, tree nc, bool exec_flag, bool first) {
  if (is_atomic (t) && nc == "first") {
    string s= t->label;
    if (s == "") return s;
    int pos= 0;
    tm_char_forwards (s, pos);
    return s (0, pos);
  }
  else if (is_atomic (t)) {
    string s= t->label;
    tree   r= copy (s);
    int i, n= N(s);

    bool all= true;
    bool up = false;
    bool lo = false;
    if (nc == "Upcase") { all= false; up= true; }
    else if (nc == "UPCASE") { up= true; }
    else if (nc == "locase") { lo= true; }

    for (i=0; i<n; tm_char_forwards (s, i))
      if (is_iso_alpha (s[i]) && (all || (first && (i==0)))) {
	if (up && is_iso_locase (s[i])) r->label[i]= upcase (s[i]);
	if (lo && is_iso_upcase (s[i])) r->label[i]= locase (s[i]);
      }
    r->obs= list_observer (ip_observer (obtain_ip (t)), r->obs);
    return r;
  }
  else if (is_concat (t)) {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= exec_change_case (t[i], nc, exec_flag, first && (i==0));
    r->obs= list_observer (ip_observer (obtain_ip (t)), r->obs);
    return r;
  }
  else {
    if (exec_flag) return t;
    else return exec_change_case (exec (t), nc, true, first);
  }
}

tree
edit_env_rep::exec_change_case (tree t) {
  if (N(t) < 2) return tree (ERROR, "bad change case");
  return exec_change_case (t[0], exec (t[1]), false, true);
}

tree
edit_env_rep::exec_find_file (tree t) {
  int i, n=N(t);
  array<tree> r (n);
  for (i=0; i<n; i++) {
    r[i]= exec (t[i]);
    if (is_compound (r[i]))
      return tree (ERROR, "bad find file");
  }
  for (i=0; i<(n-1); i++) {
    url u= resolve (url (r[i]->label, r[n-1]->label));
    if (!is_none (u)) {
      url d= delta (base_file_name, u);
      if (!is_rooted (d) && !(is_concat (d) && is_parent (d[1])))
        return as_string (d);
      if (is_rooted (u, "default")) u= reroot (u, "file");
      return as_string (u);
    }
  }
  url u= resolve (base_file_name * url_parent () * r[n-1]->label);
  if (!is_none (u)) {
    url d= delta (base_file_name, u);
    if (!is_rooted (d) && !(is_concat (d) && is_parent (d[1])))
      return as_string (d);
    if (is_rooted (u, "default")) u= reroot (u, "file");
    return as_string (u);
  }
  return "false";
}

tree
edit_env_rep::exec_find_file_upwards (tree t) {
  if (N(t) < 1) return tree (ERROR, "bad find file upwards");
  tree name= exec (t[0]);
  array<string> roots;
  for (int i=1; i<N(t); i++) {
    tree root= exec (t[i]);
    if (!is_atomic (name) || !is_atomic (root))
      return tree (ERROR, "bad find file upwards");
    roots << root->label;
  }
  url u= search_file_upwards (base_file_name, name->label, roots);
  if (!is_none (u)) {
    url d= delta (base_file_name, u);
    if (!is_rooted (d))
      return as_string (d);
    //if (is_rooted (u, "default")) u= reroot (u, "file");
    return as_string (u);
  }
  return "false";
}

tree
edit_env_rep::exec_is_tuple (tree t) {
  if (N(t)!=1) return tree (ERROR, "bad tuple query");
  return as_string_bool(is_tuple (exec (t[0])));
}

tree
edit_env_rep::exec_lookup (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad look up");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (!(is_compound (t1) && is_int (t2))) return tree (ERROR, "bad look up");
  int i= as_int (t2);
  if (i<0 || i>=N(t1)) return tree (ERROR, "index out of range in look up");
  return t1[i];
}

static bool
occurs_inside (tree w, tree t) {
  if (t == w) return true;
  if (is_atomic (t)) return false;
  for (int i=0; i<N(t); i++)
    if (occurs_inside (w, t[i])) return true;
  return false;
}

tree
edit_env_rep::exec_arg_recursive (tree t) {
  if (N(t)<1) return tree (ERROR, "bad arg");
  tree r= t[0];
  if (is_compound (r))
    return tree (ERROR, "bad arg");
  if (is_nil (macro_arg) || (!macro_arg->item->contains (r->label)))
    return tree (ERROR, "arg " * r->label);
  r= macro_arg->item [r->label];
  list<hashmap<string,tree> > old_var= macro_arg;
  list<hashmap<string,path> > old_src= macro_src;
  if (!is_nil (macro_arg)) macro_arg= macro_arg->next;
  if (!is_nil (macro_src)) macro_src= macro_src->next;
  if (is_func (r, ARG, 1)) r= exec_arg_recursive (r);
  macro_arg= old_var;
  macro_src= old_src;
  return r;
}

tree
edit_env_rep::exec_occurs_inside (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad look up");
  tree t1= exec (t[0]);
  tree t2= exec_arg_recursive (tree (ARG, t[1]));
  if (occurs_inside (t1, t2)) return "true";
  else return "false";
}

tree
edit_env_rep::exec_equal (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad equal");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_atomic (t1) && is_atomic (t2)
      && is_length (t1->label) && is_length (t2->label))
    return as_string_bool (as_length (t1) == as_length (t2));
  return as_string_bool (t1 == t2);
}

tree
edit_env_rep::exec_unequal (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad unequal");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_atomic(t1) && is_atomic(t2)
      && is_length(t1->label) && is_length(t2->label))
    return as_string_bool (as_length (t1) != as_length (t2));
  return as_string_bool (t1 != t2);
}

tree
edit_env_rep::exec_less (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad less");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return tree (ERROR, "bad less");
  string s1= t1->label;
  string s2= t2->label;
  if (is_double (s1) && is_double (s2))
    return as_string_bool (as_double (s1) < as_double (s2));
  if (is_length (s1) && is_length (s2))
    return as_string_bool (as_length (s1) < as_length (s2));
  return tree (ERROR, "bad less");
}

tree
edit_env_rep::exec_lesseq (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad less or equal");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return tree (ERROR, "bad less or equal");
  string s1= t1->label;
  string s2= t2->label;
  if (is_double (s1) && (is_double (s2)))
    return as_string_bool (as_double (s1) <= as_double (s2));
  if (is_length (s1) && is_length (s2))
    return as_string_bool (as_length (s1) <= as_length (s2));
  return tree (ERROR, "bad less or equal");
}

tree
edit_env_rep::exec_greater (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad greater");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return tree (ERROR, "bad greater");
  string s1= t1->label;
  string s2= t2->label;
  if (is_double (s1) && (is_double (s2)))
    return as_string_bool (as_double (s1) > as_double (s2));
  if (is_length (s1) && is_length (s2))
    return as_string_bool (as_length (s1) > as_length (s2));
  return tree (ERROR, "bad greater");
}

tree
edit_env_rep::exec_greatereq (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad greater or equal");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return tree (ERROR, "bad greater or equal");
  string s1= t1->label;
  string s2= t2->label;
  if (is_double (s1) && (is_double (s2)))
    return as_string_bool (as_double (s1) >= as_double (s2));
  if (is_length (s1) && is_length (s2))
    return as_string_bool (as_length (s1) >= as_length (s2));
  return tree (ERROR, "bad greater or equal");
}

tree
edit_env_rep::exec_blend (tree t) {
  if (N(t)!=2) return tree (ERROR, "bad blend");
  tree t1= exec (t[0]);
  tree t2= exec (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return tree (ERROR, "bad blend");
  string s1= t1->label;
  string s2= t2->label;
  if (is_color_name (s1) && (is_color_name (s2))) {
    color c1= named_color (s1);
    color c2= named_color (s2);
    return get_hex_color (blend_colors (c1, c2));
  }
  return tree (ERROR, "bad blend");
}

tree
edit_env_rep::exec_hard_id (tree t) {
  pointer ptr= (pointer) this;
  if (N(t) == 0)
    return "%" * as_hexadecimal (ptr);
  else {
    t= expand (t[0], true);
    pointer tptr= (pointer) t.operator -> ();
    if (is_accessible (obtain_ip (t)))
      return "%" * as_hexadecimal (ptr) *
             "-" * as_hexadecimal (tptr);
    else {
      int h= hash (t);
      return "%" * as_hexadecimal (ptr) *
             "-" * as_hexadecimal (tptr) *
             "-" * as_hexadecimal (h);
    }
  }
}

tree
edit_env_rep::exec_script (tree t) {
  int i, n= N(t);
  if (n < 1) return tree (ERROR, "bad script");
  tree r (t, n);
  r[0]= exec (t[0]);
  for (i=1; i<n; i++)
    r[i]= exec (t[i]);
  return r;
}

tree
edit_env_rep::exec_find_accessible (tree t) {
  if (N(t) < 1) return tree (ERROR, "bad find-accessible");
  return expand (t[0], true);
}

tree
edit_env_rep::exec_set_binding (tree t) {
  tree keys, value;
  if (N(t) == 1) {
    keys= read ("the-tags");
    if (!is_tuple (keys)) {
      //cout << "t= " << t << "\n";
      //cout << "keys= " << keys << "\n";
      return tree (ERROR, "bad set binding");
    }
    for (int i=0; i<N(keys); i++)
      if (!is_atomic (keys[i])) {
	//cout << "t= " << t << "\n";
	//cout << "keys= " << keys << "\n";
	return tree (ERROR, "bad set binding");
      }
    value= exec (t[0]);
    assign (string ("the-tags"), tree (TUPLE));
    assign (string ("the-label"), copy (value));
  }
  else if (N(t) >= 2) {
    tree key= exec (t[0]);
    if (!is_atomic (key)) {
      //cout << "t= " << t << "\n";
      //cout << "key= " << key << "\n";
      return tree (ERROR, "bad set binding");
    }
    keys= tuple (key);
    value= exec (t[1]);
  }
  else {
    //cout << "t= " << t << "\n";
    return tree (ERROR, "bad set binding");
  }
  //cout << t << ": " << keys << " -> " << value << "\n";

  for (int i=0; i<N(keys); i++) {
    string key= keys[i]->label;
    tree old_value= local_ref[key];
    string part= as_string (read ("current-part"));
    if (is_func (old_value, TUPLE) && (N(old_value) >= 2))
      local_ref (key)= tuple (copy (value), old_value[1]);
    else local_ref (key)= tuple (copy (value), "?");
    if (cur_file_name != base_file_name || N(part) != 0) {
      string extra;
      if (cur_file_name != base_file_name)
	extra << as_string (delta (base_file_name, cur_file_name));
      if (N(part) != 0)
	extra << "#" << part (1, N(part));
      local_ref (key) << extra;
    }
    touched (key)= true;
    if (complete && is_tuple (old_value) && N(old_value) >= 1) {
      string old_s= tree_as_string (old_value[0]);
      string new_s= tree_as_string (value);
      if (new_s != old_s && !starts (key, "auto-")) {
        redefined << tree (TUPLE, key, new_s);
	//if (new_s == "") typeset_warning << "Redefined " << key << LF;
	//else typeset_warning << "Redefined " << key << " as " << new_s << LF;
      }
    }
  }
  return tree (HIDDEN_BINDING, keys, value);
}

tree
edit_env_rep::exec_get_binding (tree t) {
  if (N(t) != 1 && N(t) != 2) return tree (ERROR, "bad get binding");
  string key= exec_string (t[0]);
  tree value= local_ref->contains (key)? local_ref [key]: global_ref [key];
  int type= (N(t) == 1? 0: as_int (exec_string (t[1])));
  if (type != 0 && type != 1) type= 0;
  if (is_func (value, TUPLE) && (N(value) >= 2)) value= value[type];
  else if (type == 1) value= tree (UNINIT);
  if (complete && value == tree (UNINIT))
    if (get_bool (WARN_MISSING)) {
      missing (key)= tree (GET_BINDING, key);
      //typeset_warning << "Undefined reference " << key << LF;
    }
  //cout << t << ": " << key << " -> " << value << "\n";
  return value;
}

tree
edit_env_rep::exec_get_attachment (tree t) {
  if (N(t) != 1) return tree (ERROR, "bad get attachment");
  string key= exec_string (t[0]);
  tree value= local_att->contains (key)? local_att [key]: global_att [key];
  return value;
}

tree
edit_env_rep::exec_pattern (tree t) {
  if (N(t)<1) return tree (ERROR, "bad pattern");
  if (no_patterns && N(t) == 4 && is_atomic (t[3])) return exec (t[3]);
  url im= url_system (exec_string (t[0]));
  url image= resolve_pattern (relative (base_file_name, im));
  if (is_none (image)) return "white";
  int imw_pt, imh_pt;
  image_size (image, imw_pt, imh_pt);
  double pt= ((double) dpi*PIXEL) / 72.0;
  SI imw= (SI) (((double) imw_pt) * pt);
  SI imh= (SI) (((double) imh_pt) * pt);
  if (imw <= 0 || imh <= 0) return "white";
  if (N(t)<3) return tree (ERROR, "bad pattern");
  string w= exec_string (t[1]);
  string h= exec_string (t[2]);
  if (is_length (w))
    w= as_string (as_length (w));
  else if (is_magnification (w))
    w= as_string ((SI) (get_magnification (w) * ((double) imw)));
  if (is_length (h))
    h= as_string (as_length (h));
  else if (is_magnification (h))
    h= as_string ((SI) (get_magnification (h) * ((double) imh)));
  if (w == "" && h != "") {
    if (is_int (h)) w= as_string ((SI) ((as_double (h) * imw) / imh));
    else if (is_percentage (h))
      w= as_string (100.0 * (as_percentage (h) * imw) / imh) * "@";
    else return "white";
  }
  else if (h == "" && w != "") {
    if (is_int (w)) h= as_string ((SI) ((as_double (w) * imh) / imw));
    else if (is_percentage (w))
      h= as_string (100.0 * (as_percentage (w) * imh) / imw) * "@";
    else return "white";
  }
  else if (w == "" && h == "") {
    w= as_string (imw);
    h= as_string (imh);
  }
  else if ((!is_int (w) && !is_percentage (w) && !is_percentage (w, "@")) ||
	   (!is_int (h) && !is_percentage (h) && !is_percentage (h, "@")))
    return "white";
  tree r (PATTERN, as_string (image), w, h);
  if (N(t) == 4) r << exec (t[3]);
  return r;
}

tree
edit_env_rep::exec_point (tree t) {
  int i, n= N(t);
  tree u (_POINT, n);
  for (i=0; i<n; i++)
    u[i]= exec (t[i]);
  if (n==0 || is_double (u[0])) return u;
  return as_tree (as_point (u));
}

tree
edit_env_rep::exec_eff_move (tree t) {
  if (N(t) < 3) return tree (ERROR, "bad eff-move");
  tree body= exec (t[0]);
  tree dx  = as_tree (as_eff_length (exec (t[1])));
  tree dy  = as_tree (as_eff_length (exec (t[2])));
  return tree (EFF_MOVE, body, dx, dy);
}

tree
edit_env_rep::exec_eff_magnify (tree t) {
  if (N(t) < 3) return tree (ERROR, "bad eff-magnify");
  tree body= exec (t[0]);
  tree sx  = as_tree (as_double (exec (t[1])));
  tree sy  = as_tree (as_double (exec (t[2])));
  return tree (EFF_MAGNIFY, body, sx, sy);
}

tree
edit_env_rep::exec_eff_bubble (tree t) {
  if (N(t) < 3) return tree (ERROR, "bad eff-bubble");
  tree body= exec (t[0]);
  tree r   = as_tree (as_eff_length (exec (t[1])));
  tree a   = exec (t[2]);
  return tree (EFF_BUBBLE, body, r, a);
}

tree
edit_env_rep::exec_eff_crop (tree t) {
  if (N(t) < 5) return tree (ERROR, "bad eff-crop");
  tree body= exec (t[0]);
  tree cx1 = as_tree (as_double (exec (t[1])));
  tree cy1 = as_tree (as_double (exec (t[2])));
  tree cx2 = as_tree (as_double (exec (t[3])));
  tree cy2 = as_tree (as_double (exec (t[4])));
  return tree (EFF_CROP, body, cx1, cy1, cx2, cy2);
}

tree
edit_env_rep::exec_eff_turbulence (tree t) {
  if (N(t) != 5) return tree (ERROR, "bad eff-turbulence");
  tree body= exec (t[0]);
  tree s   = exec (t[1]);
  tree w   = as_tree (as_eff_length (exec (t[2])));
  tree h   = as_tree (as_eff_length (exec (t[3])));
  tree o   = exec (t[4]);
  return tree (EFF_TURBULENCE, body, s, w, h, o);
}

tree
edit_env_rep::exec_eff_fractal_noise (tree t) {
  if (N(t) != 5) return tree (ERROR, "bad eff-fractal-noise");
  tree body= exec (t[0]);
  tree s   = exec (t[1]);
  tree w   = as_tree (as_eff_length (exec (t[2])));
  tree h   = as_tree (as_eff_length (exec (t[3])));
  tree o   = exec (t[4]);
  return tree (EFF_FRACTAL_NOISE, body, s, w, h, o);
}

tree
edit_env_rep::exec_eff_hatch (tree t) {
  if (N(t) < 5) return tree (ERROR, "bad eff-hatch");
  tree body= exec (t[0]);
  tree sx = as_tree (as_int (exec (t[1])));
  tree sy = as_tree (as_int (exec (t[2])));
  tree fp = as_tree (as_double (exec (t[3])));
  tree de = as_tree (as_double (exec (t[4])));
  return tree (EFF_HATCH, body, sx, sy, fp, de);
}

tree
edit_env_rep::exec_eff_dots (tree t) {
  if (N(t) < 7) return tree (ERROR, "bad eff-dots");
  tree body= exec (t[0]);
  tree a  = as_tree (as_int (exec (t[1])));
  tree b  = as_tree (as_int (exec (t[2])));
  tree c  = as_tree (as_int (exec (t[3])));
  tree d  = as_tree (as_int (exec (t[4])));
  tree fp = as_tree (as_double (exec (t[5])));
  tree de = as_tree (as_double (exec (t[6])));
  return tree (EFF_DOTS, body, a, b, c, d, fp, de);
}

tree
edit_env_rep::exec_eff_gaussian (tree t) {
  if (N(t) < 1) return tree (ERROR, "bad eff-gaussian");
  tree rx= as_tree (as_eff_length (exec (t[0])));
  if (N(t) == 1) return tree (EFF_GAUSSIAN, as_tree (rx));
  if (N(t) < 3) return tree (ERROR, "bad eff-gaussian");
  tree ry= as_tree (as_eff_length (exec (t[1])));
  return tree (EFF_GAUSSIAN, rx, ry, exec (t[2]));
}

tree
edit_env_rep::exec_eff_oval (tree t) {
  if (N(t) < 1) return tree (ERROR, "bad eff-oval");
  tree rx= as_tree (as_eff_length (exec (t[0])));
  if (N(t) == 1) return tree (EFF_OVAL, as_tree (rx));
  if (N(t) < 3) return tree (ERROR, "bad eff-oval");
  tree ry= as_tree (as_eff_length (exec (t[1])));
  return tree (EFF_OVAL, rx, ry, exec (t[2]));
}

tree
edit_env_rep::exec_eff_rectangular (tree t) {
  if (N(t) < 1) return tree (ERROR, "bad eff-rectangular");
  tree rx= as_tree (as_eff_length (exec (t[0])));
  if (N(t) == 1) return tree (EFF_RECTANGULAR, as_tree (rx));
  if (N(t) < 3) return tree (ERROR, "bad eff-rectangular");
  tree ry= as_tree (as_eff_length (exec (t[1])));
  return tree (EFF_RECTANGULAR, rx, ry, exec (t[2]));
}

tree
edit_env_rep::exec_eff_motion (tree t) {
  if (N(t) < 2) return tree (ERROR, "bad eff-motion");
  tree dx= as_tree (as_eff_length (exec (t[0])));
  tree dy= as_tree (as_eff_length (exec (t[1])));
  return tree (EFF_MOTION, dx, dy);
}

tree
edit_env_rep::exec_eff_degrade (tree t) {
  if (N(t) < 4) return tree (ERROR, "bad eff-degrade");
  tree b = exec (t[0]);
  tree wx= as_tree (as_eff_length (exec (t[1])));
  tree wy= as_tree (as_eff_length (exec (t[2])));
  tree th= as_tree (as_double (exec (t[3])));
  tree sh= as_tree (as_double (exec (t[4])));
  return tree (EFF_DEGRADE, b, wx, wy, th, sh);
}

tree
edit_env_rep::exec_eff_distort (tree t) {
  if (N(t) < 5) return tree (ERROR, "bad eff-distort");
  tree b = exec (t[0]);
  tree wx= as_tree (as_eff_length (exec (t[1])));
  tree wy= as_tree (as_eff_length (exec (t[2])));
  tree rx= as_tree (as_eff_length (exec (t[3])));
  tree ry= as_tree (as_eff_length (exec (t[4])));
  return tree (EFF_DISTORT, b, wx, wy, rx, ry);
}

tree
edit_env_rep::exec_eff_gnaw (tree t) {
  if (N(t) < 5) return tree (ERROR, "bad eff-gnaw");
  tree b = exec (t[0]);
  tree wx= as_tree (as_eff_length (exec (t[1])));
  tree wy= as_tree (as_eff_length (exec (t[2])));
  tree rx= as_tree (as_eff_length (exec (t[3])));
  tree ry= as_tree (as_eff_length (exec (t[4])));
  return tree (EFF_GNAW, b, wx, wy, rx, ry);
}

tree
edit_env_rep::exec_box_info (tree t) {
  if (N(t)<2) return tree (ERROR, "bad box-info");
  tree t1= t[0];
  tree t2= t[1];
  if (!is_string (t2))
    return tree (ERROR, "bad box info");
  return box_info (edit_env (this), t1, as_string (t2));
}

tree
edit_env_rep::exec_frame_direct (tree t) {
  if (N(t)<1) return tree (ERROR, "bad frame-direct");
  tree t1= exec (t[0]);
  return as_tree (!is_nil (fr) ? fr (::as_point (t1)) : point ());
}

tree
edit_env_rep::exec_frame_inverse (tree t) {
  if (N(t)<1) return tree (ERROR, "bad frame-inverse");
  tree t1= exec (t[0]);
  return as_tree (!is_nil (fr) ? fr [::as_point (t1)] : point ());
}

/******************************************************************************
* Partial evaluation of trees
******************************************************************************/

void
edit_env_rep::exec_until (tree t, path p) {
  // cout << "Execute " << t << " until " << p << "\n";
  if (is_nil (p)) return;
  if (is_atom (p)) {
    if (p->item!=0)
      (void) exec (t);
    return;
  }

  switch (L(t)) {
  case DATOMS:
    exec_until_formatting (t, p, ATOM_DECORATIONS);
    return;
  case DLINES:
    exec_until_formatting (t, p, LINE_DECORATIONS);
    return;
  case DPAGES:
    exec_until_formatting (t, p, PAGE_DECORATIONS);
    return;
  case TFORMAT:
    exec_until_formatting (t, p, CELL_FORMAT);
    return;
  case TABLE:
    exec_until_table (t, p);
    return;
  case WITH:
    exec_until_with (t, p);
    return;
  case COMPOUND:
    exec_until_compound (t, p);
    return;
  case MARK:
    if (p->item == 1) exec_until (t[1], p->next);
    return;
  case STYLE_WITH:
  case VAR_STYLE_WITH:
    if (p->item == (N(t)-1)) exec_until (t[N(t)-1], p->next);
    return;
  case STYLE_ONLY:
  case VAR_STYLE_ONLY:
  case ACTIVE:
  case VAR_ACTIVE:
  case INACTIVE:
  case VAR_INACTIVE:
    exec_until_compound (t, p);
    return;
  case HLINK:
  case ACTION:
    exec_until_compound (t, p);
    return;
  default:
    if (L(t) < START_EXTENSIONS) {
      int i;
      for (i=0; i<p->item; i++) (void) exec (t[i]);
      exec_until (t[p->item], p->next);
    }
    else exec_until_compound (t, p);
    return;
  }
}

void
edit_env_rep::exec_until_formatting (tree t, path p, string v) {
  int n= N(t);
  if (p->item != n-1) return;
  tree oldv= read (v);
  tree newv= oldv * t (0, n-1);
  monitored_write_update (v, newv);
  exec_until (t[n-1], p->next);
}

void
edit_env_rep::exec_until_table (tree t, path p) {
  // should execute values in oldv
  monitored_write_update (CELL_FORMAT, tree (TFORMAT));
  int i;
  for (i=0; i<p->item; i++)
    (void) exec (t[i]);
  exec_until (t[p->item], p->next);
  return;
}

void
edit_env_rep::exec_until_with (tree t, path p) {
  int i, n= N(t), k= (n-1)>>1; // is k=0 allowed ?
  if (((n&1) != 1) || (p->item != n-1)) return;
  STACK_NEW_ARRAY(vars,string,k);
  STACK_NEW_ARRAY(newv,tree,k);
  for (i=0; i<k; i++) {
    tree var_t= exec (t[i<<1]);
    if (is_atomic (var_t)) {
      string var= var_t->label;
      vars[i]= var;
      newv[i]= exec (t[(i<<1)+1]);
    }
    else {
      STACK_DELETE_ARRAY(vars);
      STACK_DELETE_ARRAY(newv);
      return;
    }
  }
  for (i=0; i<k; i++) monitored_write_update (vars[i], newv[i]);
  exec_until (t[n-1], p->next);
  STACK_DELETE_ARRAY(vars);
  STACK_DELETE_ARRAY(newv);
  return;
}

void
edit_env_rep::exec_until_compound (tree t, path p) {
  int d; tree f;
  if (L(t) == COMPOUND) {
    d= 1;
    f= t[0];
    if (is_compound (f)) f= exec (f);
    if (is_compound (f)) return;
    string fname= f->label;
    if (!provides (fname)) return;
    f= read (fname);
  }
  else {
    string fname= as_string (L(t));
    if (!provides (fname)) return;
    d= 0;
    f= read (fname);
  }

  string var;
  if (L(f) == XMACRO) var= f[0]->label;
  else {
    if ((p->item < d) || (p->item >= N(f)) ||
	is_compound (f[p->item-d])) return;
    var= f[p->item-d]->label;
  }

  if (is_applicable (f)) {
    int i, n=N(f)-1, m=N(t)-d;
    macro_arg= list<hashmap<string,tree> >
      (hashmap<string,tree> (UNINIT), macro_arg);
    macro_src= list<hashmap<string,path> >
      (hashmap<string,path> (path (DECORATION)), macro_src);
    if (L(f) == XMACRO) {
      if (is_atomic (f[0])) {
	macro_arg->item (f[0]->label)= t;
	macro_src->item (f[0]->label)= obtain_ip (t);
      }
      (void) exec_until (f[n], p, var, 0);
    }
    else {
      for (i=0; i<n; i++)
	if (is_atomic (f[i])) {
	  tree st= i<m? t[i+d]: tree (UNINIT);
	  macro_arg->item (f[i]->label)= st;
	  macro_src->item (f[i]->label)= obtain_ip (st);
	}
      (void) exec_until (f[n], p->next, var, 0);
    }
    macro_arg= macro_arg->next;
    macro_src= macro_src->next;
  }
}

bool
edit_env_rep::exec_until (tree t, path p, string var, int level) {
  // cout << "Execute " << t << " until " << p
  //      << " inside " << var << " level " << level << "\n";
  if (is_atomic (t)) return false;
  switch (L(t)) {
  case DATOMS:
    return exec_until_formatting (t, p, var, level, ATOM_DECORATIONS);
  case DLINES:
    return exec_until_formatting (t, p, var, level, LINE_DECORATIONS);
  case DPAGES:
    return exec_until_formatting (t, p, var, level, PAGE_DECORATIONS);
  case TFORMAT:
    return exec_until_formatting (t, p, var, level, CELL_FORMAT);
  case TABLE:
    return exec_until_table (t, p, var, level);
  case ASSIGN:
  case PROVIDE:
    (void) exec (t);
    return false;
  case WITH:
    return exec_until_with (t, p, var, level);
  case PROVIDES:
    (void) exec (t);
    return false;
  case VALUE:
    /*
    {
      tree r= t[0];
      if (is_compound (r)) r= exec (r);
      if (is_atomic (r) && (r->label == var)) {
	exec_until (read (r->label), p);
	return true;
      }
    }
    */
    (void) exec (t);
    return false;
  case QUOTE_VALUE:
    (void) exec (t);
    return false;
  case OR_VALUE:
    (void) exec (t);
    return false;
  case MACRO:
  case DRD_PROPS:
    (void) exec (t);
    return false;
  case ARG:
    return exec_until_arg (t, p, var, level);
  case QUOTE_ARG:
    (void) exec (t);
    return false;
  case COMPOUND:
    return exec_until_compound (t, p, var, level);
  case XMACRO:
  case GET_LABEL:
  case GET_ARITY:
    (void) exec (t);
    return false;
  case MAP_ARGS:
  case EVAL_ARGS:
    return exec_until_rewrite (t, p, var, level);
  case NEW_THEME:
  case COPY_THEME:
  case APPLY_THEME:
  case SELECT_THEME:
    (void) exec (t);
    return false;
  case MARK:
    return exec_until_mark (t, p, var, level);
  case EVAL:
    return exec_until (exec (t), p, var, level);
  case QUOTE:
    (void) exec (t);
    return false;
  case QUASI:
    return exec_until_quasi (t, p, var, level);
  case QUASIQUOTE:
  case UNQUOTE:
  case VAR_UNQUOTE:
    (void) exec (t);
    return false;
  case IF:
  case VAR_IF:
    return exec_until_if (t, p, var, level);
  case CASE:
    return exec_until_case (t, p, var, level);
  case WHILE:
    return exec_until_while (t, p, var, level);
  case FOR_EACH:
    (void) exec (t);
    return false;
  case EXTERN:
  case VAR_INCLUDE:
  case WITH_PACKAGE:
    return exec_until_rewrite (t, p, var, level);
  case USE_PACKAGE:
  case USE_MODULE:
  case OR:
  case XOR:
  case AND:
  case NOT:
  case PLUS:
  case MINUS:
  case TIMES:
  case OVER:
  case DIV:
  case MOD:
  case MERGE:
  case LENGTH:
  case RANGE:
  case NUMBER:
  case _DATE:
  case TRANSLATE:
  case FIND_FILE:
  case FIND_FILE_UPWARDS:
  case IS_TUPLE:
  case LOOK_UP:
  case OCCURS_INSIDE:
  case EQUAL:
  case UNEQUAL:
  case LESS:
  case LESSEQ:
  case GREATER:
  case GREATEREQ:
  case BLEND:
    (void) exec (t);
    return false;
  case STYLE_WITH:
  case VAR_STYLE_WITH:
    return exec_until (t[N(t)-1], p, var, level);
  case STYLE_ONLY:
  case VAR_STYLE_ONLY:
  case ACTIVE:
  case VAR_ACTIVE:
  case INACTIVE:
  case VAR_INACTIVE:
    return exec_until_compound (t, p, var, level);
  case REWRITE_INACTIVE:
    return exec_until_rewrite (t, p, var, level);
  case HLINK:
  case ACTION:
    return exec_until_compound (t, p, var, level);
  default:
    if (L(t) < START_EXTENSIONS) {
      int i, n= N(t);
      for (i=0; i<n; i++)
	if (exec_until (t[i], p, var, level))
	  return true;
      return false;
    }
    else return exec_until_compound (t, p, var, level);
  }
}

bool
edit_env_rep::exec_until_formatting (
  tree t, path p, string var, int level, string v)
{
  int n= N(t);
  tree oldv= read (v);
  tree newv= oldv * t (0, n-1);
  monitored_write_update (v, newv);
  if (exec_until (t[n-1], p, var, level)) return true;
  monitored_write_update (v, oldv);
  return false;
}

bool
edit_env_rep::exec_until_table (tree t, path p, string var, int level) {
  tree oldv= read (CELL_FORMAT);
  // should execute values in oldv
  monitored_write_update (CELL_FORMAT, tree (TFORMAT));
  int i, n= N(t);
  for (i=0; i<n; i++)
    if (exec_until (t[i], p, var, level))
      return true;
  monitored_write_update (CELL_FORMAT, oldv);
  return false;
}

bool
edit_env_rep::exec_until_with (tree t, path p, string var, int level) {
  int i, n= N(t), k= (n-1)>>1; // is k=0 allowed ?
  if ((n&1) != 1) return false;
  STACK_NEW_ARRAY(vars,string,k);
  STACK_NEW_ARRAY(oldv,tree,k);
  STACK_NEW_ARRAY(newv,tree,k);
  for (i=0; i<k; i++) {
    tree var_t= exec (t[i<<1]);
    if (is_atomic (var_t)) {
      string var= var_t->label;
      vars[i]= var;
      oldv[i]= read (var);
      newv[i]= exec (t[(i<<1)+1]);
    }
    else {
      STACK_DELETE_ARRAY(vars);
      STACK_DELETE_ARRAY(oldv);
      STACK_DELETE_ARRAY(newv);
      return false;
    }
  }

  for (i=0; i<k; i++) monitored_write_update (vars[i], newv[i]);
  if (exec_until (t[n-1], p, var, level)) {
    STACK_DELETE_ARRAY(vars);
    STACK_DELETE_ARRAY(oldv);
    STACK_DELETE_ARRAY(newv);
    return true;
  }
  for (i=k-1; i>=0; i--) write_update (vars[i], oldv[i]);
  STACK_DELETE_ARRAY(vars);
  STACK_DELETE_ARRAY(oldv);
  STACK_DELETE_ARRAY(newv);
  return false;
}

bool
edit_env_rep::exec_until_compound (tree t, path p, string var, int level) {
  int d; tree f;
  if (L(t) == COMPOUND) {
    d= 1;
    f= t[0];
    if (is_compound (f)) f= exec (f);
    if (is_atomic (f)) {
      string var= f->label;
      if (!provides (var)) return false;
      f= read (var);
    }
  }
  else {
    string fname= as_string (L(t));
    if (!provides (fname)) return false;
    d= 0;
    f= read (fname);
  }

  if (is_applicable (f)) {
    int i, n=N(f)-1, m=N(t)-d;
    macro_arg= list<hashmap<string,tree> >
      (hashmap<string,tree> (UNINIT), macro_arg);
    macro_src= list<hashmap<string,path> >
      (hashmap<string,path> (path (DECORATION)), macro_src);
    if (L(f) == XMACRO) {
      if (is_atomic (f[0]))
	macro_arg->item (f[0]->label)= t;
    }
    for (i=0; i<n; i++)
      if (is_atomic (f[i])) {
	tree st= i<m? t[i+d]: tree (UNINIT);
	macro_arg->item (f[i]->label)= st;
	macro_src->item (f[i]->label)= obtain_ip (st);
      }
    bool done= exec_until (f[n], p, var, level+1);
    macro_arg= macro_arg->next;
    macro_src= macro_src->next;
    return done;
  }
  return false;
}

bool
edit_env_rep::exec_until_arg (tree t, path p, string var, int level) {
  // cout << "  " << macro_arg << "\n";
  tree r= t[0];
  if (is_atomic (r) && (!is_nil (macro_arg)) &&
      macro_arg->item->contains (r->label))
    {
      bool found;
      tree arg= macro_arg->item [r->label];
      list<hashmap<string,tree> > old_var= macro_arg;
      list<hashmap<string,path> > old_src= macro_src;
      if (!is_nil (macro_arg)) macro_arg= macro_arg->next;
      if (!is_nil (macro_src)) macro_src= macro_src->next;
      if (level == 0) {
	found= (r->label == var);
	if ((N(t) > 1) && found) {
	  int i, n= N(t);
	  for (i=1; i<n; i++) {
	    tree u= exec (t[i]);
	    if (!is_int (u)) { found= false; break; }
	    int nr= as_int (u);
	    if ((!is_compound (arg)) || (nr<0) || (nr>=N(arg)) || is_nil (p)) {
	      found= false; break; }
	    if (p->item != nr) found= false;
	    arg= arg[nr];
	    p  = p->next;
	  }
	}
	if (found) exec_until (arg, p);
	else exec (arg);
      }
      else found= exec_until (arg, p, var, level-1);
      macro_arg= old_var;
      macro_src= old_src;
      return found;
    }
  else return false;
  /*
  cout << "  " << macro_arg << "\n";
  tree r= t[0];
  if (is_atomic (r) && (r->label == var) && (!is_nil (macro_arg))) {
    bool found= (level == 0) && macro_arg->item->contains (r->label);
    tree arg  = macro_arg->item [var];
    list<hashmap<string,tree> > old_var= macro_arg;
    list<hashmap<string,path> > old_src= macro_src;
    if (!is_nil (macro_arg)) macro_arg= macro_arg->next;
    if (!is_nil (macro_src)) macro_src= macro_src->next;
    if (found) exec_until (arg, p);
    else found= exec_until (arg, p, var, level-1);
    macro_arg= old_var;
    macro_src= old_src;
    return found;
  }
  */
}

bool
edit_env_rep::exec_until_mark (tree t, path p, string var, int level) {
  bool border= false;
  if ((level == 0) && is_func (t[0], ARG) && (t[0][0] == var)) {
    // cout << "\n\tTest: " << t[0] << ", " << p << "\n";
    path q= p;
    int i, n= N(t[0]);
    for (i=1; (!is_nil (q)) && (i<n); i++, q= q->next)
      if (t[0][i] != as_string (q->item))
	break;
    border= (i == n) && is_atom (q);
    // FIXME: in order to be clean, we should check whether q->item
    // is on the border of the contents of the argument t[0].
    // Nevertheless, this only matters for strings and
    // the present implementation seems to be OK for the moment.
    // cout << "\tBorder= " << border << "\n\n";
  }
  if (border) return exec_until (t[0], p, var, level);
  else return exec_until (t[1], p, var, level);
}

bool
edit_env_rep::exec_until_quasi (tree t, path p, string var, int level) {
  bool old= quote_substitute;
  quote_substitute= true;
  tree u= exec_quasiquoted (t[0]);
  quote_substitute= old;
  return exec_until (u, p, var, level);
}

bool
edit_env_rep::exec_until_if (tree t, path p, string var, int level) {
  if ((N(t)!=2) && (N(t)!=3)) return false;
  tree tt= exec (t[0]);
  if (is_compound (tt) || !is_bool (tt->label)) return false;
  if (as_bool (tt->label)) return exec_until (t[1], p, var, level);
  if (N(t)==3) return exec_until (t[2], p, var, level);
  return false;
}

bool
edit_env_rep::exec_until_case (tree t, path p, string var, int level) {
  if (N(t)<2) return false;
  int i, n= N(t);
  for (i=0; i<(n-1); i+=2) {
    tree tt= exec (t[i]);
    if (is_compound (tt) || ! is_bool (tt->label)) return false;
    if (as_bool (tt->label)) return exec_until (t[i+1], p, var, level);
  }
  if (i<n) return exec_until (t[i], p, var, level);
  return false;
}

bool
edit_env_rep::exec_until_while (tree t, path p, string var, int level) {
  if (N(t)!=2) return false;
  while (1) {
    tree tt= exec (t[0]);
    if (is_compound (tt)) return false;
    if (!is_bool (tt->label)) return false;
    if (!as_bool (tt->label)) break;
    if (exec_until (t[1], p, var, level)) return true;
  }
  return false;
}

/******************************************************************************
* Extra routines for macro expansion and function application
******************************************************************************/

tree
edit_env_rep::expand (tree t, bool search_accessible) {
  if (is_atomic (t) || is_nil (macro_arg)) return t;
  else if (is_func (t, ARG) || is_func (t, QUOTE_ARG)) {
    if (N(t) < 1)
      return tree (ERROR, "bad argument application");
    if (is_compound (t[0]))
      return tree (ERROR, "bad argument application");
    if (!macro_arg->item->contains (t[0]->label))
      return tree (ERROR, "argument " * t[0]->label);
    tree r= macro_arg->item [t[0]->label];
    list<hashmap<string,tree> > old_var= macro_arg;
    list<hashmap<string,path> > old_src= macro_src;
    if (!is_nil (macro_arg)) macro_arg= macro_arg->next;
    if (!is_nil (macro_src)) macro_src= macro_src->next;
    if (N(t) > 1) {
      int i, n= N(t);
      for (i=1; i<n; i++) {
	tree u= exec (t[i]);
	if (!is_int (u)) break;
	int nr= as_int (u);
	if ((!is_compound (r)) || (nr<0) || (nr>=N(r))) break;
	r= r[nr];
      }
    }
    if (is_func (t, ARG))
      r= expand (r, search_accessible);
    macro_arg= old_var;
    macro_src= old_src;
    return r;
  }
  else if (is_func (t, EXPAND_AS, 2)) {
    if (N(t) < 1)
      return tree (ERROR, "bad argument application");
    return expand (t[0], search_accessible);
  } else if (search_accessible && is_accessible (obtain_ip (t)))
    return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++) {
      r[i]= expand (t[i], search_accessible);
      if (search_accessible &&
	  is_accessible (obtain_ip (r[i])) &&
	  drd->is_accessible_child (t, i))
	return r[i];
    }
    if (search_accessible) return t;
    return r;
  }
}

bool
edit_env_rep::depends (tree t, string s, int level) {
  /*
  cout << "Depends? " << t << ", " << s << ", " << level
       << " " << macro_arg << "\n";
  */

  if (is_atomic (t) || is_nil (macro_arg)) return false;
  else if (is_func (t, ARG) ||
	   is_func (t, QUOTE_ARG) ||
	   is_func (t, MAP_ARGS) ||
	   is_func (t, EVAL_ARGS))
    {
      // FIXME: this does not handle more complex dependencies,
      // like those encountered after rewritings (VAR_INCLUDE, EXTERN, etc.)
      tree v= (L(t) == MAP_ARGS? t[2]: t[0]);
      if (is_compound (v)) return false;
      if (!macro_arg->item->contains (v->label)) return false;
      if (level == 0) return v->label == s;
      tree r= macro_arg->item [v->label];
      list<hashmap<string,tree> > old_var= macro_arg;
      list<hashmap<string,path> > old_src= macro_src;
      if (!is_nil (macro_arg)) macro_arg= macro_arg->next;
      if (!is_nil (macro_src)) macro_src= macro_src->next;
      bool dep= depends (r, s, level-1);
      macro_arg= old_var;
      macro_src= old_src;
      return dep;
    }
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (depends (t[i], s, level))
	return true;
    return false;
  }
}
