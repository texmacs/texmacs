/* classes: h_files */

#ifndef SCM_EVAL_H
#define SCM_EVAL_H

/* Copyright (C) 1995,1996,1998,1999,2000,2001,2002,2003,2004
 * Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */



#include "libguile/__scm.h"

#include "libguile/struct.h"



/* {Options}
 */

SCM_API scm_t_option scm_eval_opts[];

#define SCM_EVAL_STACK         scm_eval_opts[0].val
#define SCM_N_EVAL_OPTIONS 1

SCM_API long scm_eval_stack;

SCM_API scm_t_option scm_evaluator_trap_table[];

SCM_API SCM scm_eval_options_interface (SCM setting);

#define SCM_TRAPS_P	       scm_evaluator_trap_table[0].val
#define SCM_ENTER_FRAME_P      scm_evaluator_trap_table[1].val
#define SCM_APPLY_FRAME_P      scm_evaluator_trap_table[2].val
#define SCM_EXIT_FRAME_P       scm_evaluator_trap_table[3].val
#define SCM_ENTER_FRAME_HDLR   (SCM_PACK (scm_evaluator_trap_table[4].val))
#define SCM_APPLY_FRAME_HDLR   (SCM_PACK (scm_evaluator_trap_table[5].val))
#define SCM_EXIT_FRAME_HDLR    (SCM_PACK (scm_evaluator_trap_table[6].val))
#define SCM_N_EVALUATOR_TRAPS 7



/* {Ilocs}
 *
 * Ilocs are relative pointers into local environment structures.
 * 
 */
#define SCM_ILOCP(n)		(SCM_ITAG8(n)==scm_tc8_iloc)



/* {Promises}
 */

#define SCM_F_PROMISE_COMPUTED (1L << 0)
#define SCM_PROMISE_COMPUTED_P(promise) \
  (SCM_F_PROMISE_COMPUTED & SCM_SMOB_FLAGS (promise))
#define SCM_SET_PROMISE_COMPUTED(promise) \
  SCM_SET_SMOB_FLAGS ((promise), SCM_F_PROMISE_COMPUTED)
#define SCM_PROMISE_MUTEX     SCM_SMOB_OBJECT_2
#define SCM_PROMISE_DATA      SCM_SMOB_OBJECT
#define SCM_SET_PROMISE_DATA  SCM_SET_SMOB_OBJECT


SCM_API scm_t_bits scm_tc16_promise;



/* {Evaluator}
 */

typedef SCM (*scm_t_trampoline_0) (SCM proc);
typedef SCM (*scm_t_trampoline_1) (SCM proc, SCM arg1);
typedef SCM (*scm_t_trampoline_2) (SCM proc, SCM arg1, SCM arg2);



#define SCM_EXTEND_ENV scm_acons

/*fixme* This should probably be removed throught the code. */

#define SCM_TOP_LEVEL_LOOKUP_CLOSURE (scm_current_module_lookup_closure())



SCM_API SCM scm_sym_and;
SCM_API SCM scm_sym_begin;
SCM_API SCM scm_sym_case;
SCM_API SCM scm_sym_cond;
SCM_API SCM scm_sym_define;
SCM_API SCM scm_sym_do;
SCM_API SCM scm_sym_if;
SCM_API SCM scm_sym_lambda;
SCM_API SCM scm_sym_let;
SCM_API SCM scm_sym_letstar;
SCM_API SCM scm_sym_letrec;
SCM_API SCM scm_sym_quote;
SCM_API SCM scm_sym_quasiquote;
SCM_API SCM scm_sym_unquote;
SCM_API SCM scm_sym_uq_splicing;

SCM_API SCM scm_sym_atapply;
SCM_API SCM scm_sym_atcall_cc;
SCM_API SCM scm_sym_at_call_with_values;
SCM_API SCM scm_sym_delay;
SCM_API SCM scm_sym_arrow;
SCM_API SCM scm_sym_else;
SCM_API SCM scm_sym_apply;
SCM_API SCM scm_sym_set_x;
SCM_API SCM scm_sym_args;



SCM_API SCM * scm_ilookup (SCM iloc, SCM env);
SCM_API SCM * scm_lookupcar (SCM vloc, SCM genv, int check);
SCM_API SCM scm_eval_car (SCM pair, SCM env);
SCM_API SCM scm_eval_body (SCM code, SCM env);
SCM_API SCM scm_eval_args (SCM i, SCM env, SCM proc);
SCM_API SCM scm_m_quote (SCM xorig, SCM env);
SCM_API SCM scm_m_begin (SCM xorig, SCM env);
SCM_API SCM scm_m_if (SCM xorig, SCM env);
SCM_API SCM scm_m_set_x (SCM xorig, SCM env);
SCM_API SCM scm_m_vref (SCM xorig, SCM env);
SCM_API SCM scm_m_vset (SCM xorig, SCM env);
SCM_API SCM scm_m_and (SCM xorig, SCM env);
SCM_API SCM scm_m_or (SCM xorig, SCM env);
SCM_API SCM scm_m_case (SCM xorig, SCM env);
SCM_API SCM scm_m_cond (SCM xorig, SCM env);
SCM_API SCM scm_m_lambda (SCM xorig, SCM env);
SCM_API SCM scm_m_letstar (SCM xorig, SCM env);
SCM_API SCM scm_m_do (SCM xorig, SCM env);
SCM_API SCM scm_m_quasiquote (SCM xorig, SCM env);
SCM_API SCM scm_m_delay (SCM xorig, SCM env);
SCM_API SCM scm_m_generalized_set_x (SCM xorig, SCM env);
SCM_API SCM scm_m_future (SCM xorig, SCM env);
SCM_API SCM scm_m_define (SCM x, SCM env);
SCM_API SCM scm_m_letrec (SCM xorig, SCM env);
SCM_API SCM scm_m_let (SCM xorig, SCM env);
SCM_API SCM scm_m_apply (SCM xorig, SCM env);
SCM_API SCM scm_m_cont (SCM xorig, SCM env);
#if SCM_ENABLE_ELISP
SCM_API SCM scm_m_nil_cond (SCM xorig, SCM env);
SCM_API SCM scm_m_atfop (SCM xorig, SCM env);
#endif /* SCM_ENABLE_ELISP */
SCM_API SCM scm_m_atbind (SCM xorig, SCM env);
SCM_API SCM scm_m_atslot_ref (SCM xorig, SCM env);
SCM_API SCM scm_m_atslot_set_x (SCM xorig, SCM env);
SCM_API SCM scm_m_atdispatch (SCM xorig, SCM env);
SCM_API SCM scm_m_at_call_with_values (SCM xorig, SCM env);
SCM_API int scm_badargsp (SCM formals, SCM args);
SCM_API SCM scm_call_0 (SCM proc);
SCM_API SCM scm_call_1 (SCM proc, SCM arg1);
SCM_API SCM scm_call_2 (SCM proc, SCM arg1, SCM arg2);
SCM_API SCM scm_call_3 (SCM proc, SCM arg1, SCM arg2, SCM arg3);
SCM_API SCM scm_call_4 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4);
SCM_API SCM scm_apply_0 (SCM proc, SCM args);
SCM_API SCM scm_apply_1 (SCM proc, SCM arg1, SCM args);
SCM_API SCM scm_apply_2 (SCM proc, SCM arg1, SCM arg2, SCM args);
SCM_API SCM scm_apply_3 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM args);
SCM_API SCM scm_i_call_closure_0 (SCM proc);
SCM_API scm_t_trampoline_0 scm_trampoline_0 (SCM proc);
SCM_API scm_t_trampoline_1 scm_trampoline_1 (SCM proc);
SCM_API scm_t_trampoline_2 scm_trampoline_2 (SCM proc);
SCM_API SCM scm_nconc2last (SCM lst);
SCM_API SCM scm_apply (SCM proc, SCM arg1, SCM args);
SCM_API SCM scm_dapply (SCM proc, SCM arg1, SCM args);
SCM_API SCM scm_map (SCM proc, SCM arg1, SCM args);
SCM_API SCM scm_for_each (SCM proc, SCM arg1, SCM args);
SCM_API SCM scm_closure (SCM code, SCM env);
SCM_API SCM scm_makprom (SCM code);
SCM_API SCM scm_force (SCM x);
SCM_API SCM scm_promise_p (SCM x);
SCM_API SCM scm_cons_source (SCM xorig, SCM x, SCM y);
SCM_API SCM scm_copy_tree (SCM obj);
SCM_API SCM scm_i_eval_x (SCM exp, SCM env);
SCM_API SCM scm_i_eval (SCM exp, SCM env);
SCM_API SCM scm_primitive_eval (SCM exp);
SCM_API SCM scm_primitive_eval_x (SCM exp);
SCM_API SCM scm_eval (SCM exp, SCM module);
SCM_API SCM scm_eval_x (SCM exp, SCM module);

SCM_API void scm_i_print_iloc (SCM /*iloc*/, SCM /*port*/);
SCM_API void scm_i_print_isym (SCM /*isym*/, SCM /*port*/);
SCM_API SCM scm_i_unmemocopy_expr (SCM expr, SCM env);
SCM_API SCM scm_i_unmemocopy_body (SCM forms, SCM env);
SCM_API void scm_init_eval (void);


#if (SCM_ENABLE_DEPRECATED == 1)

SCM_API SCM scm_m_undefine (SCM x, SCM env);

/* Deprecated in guile 1.7.0 on 2003-11-09.  */
SCM_API SCM scm_m_expand_body (SCM xorig, SCM env);

/* Deprecated in guile 1.7.0 on 2003-11-16.  */
SCM_API SCM scm_unmemocar (SCM form, SCM env);
SCM_API SCM scm_macroexp (SCM x, SCM env);

/* Deprecated in guile 1.7.0 on 2004-03-29.  */
SCM_API SCM scm_ceval (SCM x, SCM env);
SCM_API SCM scm_deval (SCM x, SCM env);
SCM_API SCM (*scm_ceval_ptr) (SCM x, SCM env);

#endif


#endif  /* SCM_EVAL_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
