/* classes: h_files */

#ifndef EVALH
#define EVALH
/*	Copyright (C) 1995, 1996 ,1998, 1999, 2000 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */


#include "libguile/__scm.h"



/* {Options}
 */

GUILE_API extern scm_option scm_eval_opts[];

#define SCM_EVAL_STACK	       scm_eval_opts[0].val
#define SCM_N_EVAL_OPTIONS 1

GUILE_API extern int scm_eval_stack;

GUILE_API extern scm_option scm_evaluator_trap_table[];

GUILE_API extern SCM scm_eval_options_interface (SCM setting);

#define SCM_TRAPS_P	       scm_evaluator_trap_table[0].val
#define SCM_ENTER_FRAME_P      scm_evaluator_trap_table[1].val
#define SCM_APPLY_FRAME_P      scm_evaluator_trap_table[2].val
#define SCM_EXIT_FRAME_P       scm_evaluator_trap_table[3].val
#define SCM_N_EVALUATOR_TRAPS 4



/* {Ilocs}
 *
 * Ilocs are relative pointers into local environment structures.
 * 
 */
#define SCM_ILOCP(n)		(SCM_ITAG8(n)==scm_tc8_iloc)
#define SCM_ILOC00		SCM_MAKE_ITAG8(0L, scm_tc8_iloc)
#define SCM_IDINC		(0x00100000L)
#define SCM_ICDR		(0x00080000L)
#define SCM_IFRINC		(0x00000100L)
#define SCM_IDSTMSK		(-SCM_IDINC)
#define SCM_IFRAME(n) 		((int)((SCM_ICDR-SCM_IFRINC)>>8) \
				 & (SCM_UNPACK (n) >> 8))
#define SCM_IDIST(n) 		(SCM_UNPACK (n) >> 20)
#define SCM_ICDRP(n) 		(SCM_ICDR & SCM_UNPACK (n))




/* {Evaluator}
 *
 * For an explanation of symbols containing "EVAL", see beginning of eval.c.
 */
#ifdef MEMOIZE_LOCALS
#define SCM_EVALIM(x, env) (SCM_ILOCP (x) ? *scm_ilookup ((x), env) : x)
#else
#define SCM_EVALIM(x, env) x
#endif
#ifdef DEBUG_EXTENSIONS
#define SCM_XEVAL(x, env) (SCM_IMP (x) \
			   ? (x) \
			   : (*scm_ceval_ptr) ((x), (env)))
#define SCM_XEVALCAR(x, env) (SCM_NCELLP (SCM_CAR (x)) \
			      ? (SCM_IMP (SCM_CAR (x)) \
				 ? SCM_EVALIM (SCM_CAR (x), env) \
				 : SCM_GLOC_VAL (SCM_CAR (x))) \
			      : (SCM_SYMBOLP (SCM_CAR (x)) \
			         ? *scm_lookupcar (x, env, 1) \
			         : (*scm_ceval_ptr) (SCM_CAR (x), env)))
#else
#define SCM_XEVAL(x, env) (SCM_IMP (x) ? (x) : scm_ceval ((x), (env)))
#define SCM_XEVALCAR(x, env) EVALCAR (x, env)
#endif /* DEBUG_EXTENSIONS */



#define SCM_EXTEND_ENV scm_acons


GUILE_API extern const char scm_s_expression[];
GUILE_API extern const char scm_s_test[];
GUILE_API extern const char scm_s_body[];
GUILE_API extern const char scm_s_bindings[];
GUILE_API extern const char scm_s_variable[];
GUILE_API extern const char scm_s_clauses[];
GUILE_API extern const char scm_s_formals[];
GUILE_API extern const char scm_s_set_x[];

GUILE_API extern SCM scm_sym_and;
GUILE_API extern SCM scm_sym_begin;
GUILE_API extern SCM scm_sym_case;
GUILE_API extern SCM scm_sym_cond;
GUILE_API extern SCM scm_sym_define;
GUILE_API extern SCM scm_sym_do;
GUILE_API extern SCM scm_sym_if;
GUILE_API extern SCM scm_sym_lambda;
GUILE_API extern SCM scm_sym_let;
GUILE_API extern SCM scm_sym_letstar;
GUILE_API extern SCM scm_sym_letrec;
GUILE_API extern SCM scm_sym_quote;
GUILE_API extern SCM scm_sym_quasiquote;
GUILE_API extern SCM scm_sym_unquote;
GUILE_API extern SCM scm_sym_uq_splicing;

GUILE_API extern SCM scm_sym_dot;
GUILE_API extern SCM scm_sym_atapply;
GUILE_API extern SCM scm_sym_atcall_cc;
GUILE_API extern SCM scm_sym_delay;
GUILE_API extern SCM scm_sym_arrow;
GUILE_API extern SCM scm_sym_else;
GUILE_API extern SCM scm_sym_apply;
GUILE_API extern SCM scm_sym_set_x;
GUILE_API extern SCM scm_sym_args;

GUILE_API extern SCM scm_f_apply;

GUILE_API extern long scm_tc16_macro;

/* A resolved global variable reference in the CAR position
 * of a list is stored (in code only) as a pointer to a pair with a 
 * tag of 1.  This is called a "gloc".
 */

#define SCM_GLOC_SYM(x) (SCM_CAR (SCM_PACK (SCM_UNPACK (x) - 1L)))
#define SCM_GLOC_VAL(x) (SCM_CDR (SCM_PACK (SCM_UNPACK (x) - 1L)))
#define SCM_GLOC_VAL_LOC(x) (SCM_CDRLOC (SCM_PACK (SCM_UNPACK (x) - 1L)))



GUILE_API extern SCM * scm_ilookup (SCM iloc, SCM env);
GUILE_API extern SCM * scm_lookupcar (SCM vloc, SCM genv, int check);
GUILE_API extern SCM scm_unmemocar (SCM form, SCM env);
GUILE_API extern SCM scm_unmemocopy (SCM form, SCM env);
GUILE_API extern SCM scm_eval_car (SCM pair, SCM env);
GUILE_API extern SCM scm_eval_body (SCM code, SCM env);
GUILE_API extern SCM scm_eval_args (SCM i, SCM env, SCM proc);
GUILE_API extern SCM scm_deval_args (SCM l, SCM env, SCM proc, SCM *lloc);
GUILE_API extern SCM scm_m_quote (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_begin (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_if (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_set_x (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_vref (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_vset (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_and (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_or (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_case (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_cond (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_lambda (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_letstar (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_do (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_quasiquote (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_delay (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_define (SCM x, SCM env);
GUILE_API extern SCM scm_m_letrec (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_let (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_apply (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_cont (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_nil_cond (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_nil_ify (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_t_ify (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_0_cond (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_0_ify (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_1_ify (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_atfop (SCM xorig, SCM env);
GUILE_API extern SCM scm_m_atbind (SCM xorig, SCM env);
GUILE_API extern int scm_badargsp (SCM formals, SCM args);
GUILE_API extern SCM scm_ceval (SCM x, SCM env);
GUILE_API extern SCM scm_deval (SCM x, SCM env);
GUILE_API extern SCM scm_nconc2last (SCM lst);
GUILE_API extern SCM scm_apply (SCM proc, SCM arg1, SCM args);
GUILE_API extern SCM scm_dapply (SCM proc, SCM arg1, SCM args);
GUILE_API extern SCM scm_m_expand_body (SCM xorig, SCM env);
GUILE_API extern SCM scm_macroexp (SCM x, SCM env);
GUILE_API extern SCM scm_map (SCM proc, SCM arg1, SCM args);
GUILE_API extern SCM scm_for_each (SCM proc, SCM arg1, SCM args);
GUILE_API extern SCM scm_closure (SCM code, SCM env);
GUILE_API extern SCM scm_makprom (SCM code);
GUILE_API extern SCM scm_force (SCM x);
GUILE_API extern SCM scm_promise_p (SCM x);
GUILE_API extern SCM scm_cons_source (SCM xorig, SCM x, SCM y);
GUILE_API extern SCM scm_copy_tree (SCM obj);
GUILE_API extern SCM scm_eval_3 (SCM obj, int copyp, SCM env);
GUILE_API extern SCM scm_eval2 (SCM obj, SCM env_thunk);
GUILE_API extern SCM scm_eval (SCM obj);
GUILE_API extern SCM scm_eval_x (SCM obj);
GUILE_API extern void scm_init_eval (void);

#endif  /* EVALH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
