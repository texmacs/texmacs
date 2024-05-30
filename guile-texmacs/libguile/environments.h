/* classes: h_files */

#ifndef SCM_ENVIRONMENTS_H
#define SCM_ENVIRONMENTS_H

/* Copyright (C) 1999,2000, 2006 Free Software Foundation, Inc.
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



/* The type for folding functions written in C.  A function meant to be passed
 * to scm_c_environment_fold should have the type scm_environment_folder. 
 */
typedef SCM (*scm_environment_folder) (SCM data, SCM sym, SCM val, SCM tail);


/* The type for observer functions written in C.  A function meant to be
 * passed to scm_c_environment_observe should have the type
 * scm_environment_observer.
 */
typedef void (*scm_environment_observer) (SCM env, SCM data);


struct scm_environment_funcs {
  SCM (*ref) (SCM self, SCM symbol);
  SCM (*fold) (SCM self, scm_environment_folder proc, SCM data, SCM init);

  SCM (*define) (SCM self, SCM symbol, SCM value);
  SCM (*undefine) (SCM self, SCM symbol);
  SCM (*set) (SCM self, SCM symbol, SCM value);

  SCM (*cell) (SCM self, SCM symbol, int for_write);
  SCM (*observe) (SCM self, scm_environment_observer proc, SCM data, int weak_p);
  void (*unobserve) (SCM self, SCM token);

  SCM (*mark) (SCM self);
  void (*free) (SCM self);
  int (*print) (SCM self, SCM port, scm_print_state *pstate);
};



#define SCM_ENVIRONMENT_SUCCESS SCM_BOOL_T
#define SCM_ENVIRONMENT_BINDING_IMMUTABLE scm_from_int (0)
#define SCM_ENVIRONMENT_LOCATION_IMMUTABLE scm_from_int (1)
#define SCM_ENVIRONMENT_LOCATION_NO_CELL SCM_BOOL_F

SCM_API scm_t_bits scm_tc16_environment;

#define SCM_ENVIRONMENT_P(x) \
  (!SCM_IMP (x) && SCM_CELL_TYPE (x) == scm_tc16_environment)
#define SCM_ENVIRONMENT_FUNCS(env) \
  (*((struct scm_environment_funcs **) SCM_CELL_WORD_1 (env)))
#define SCM_ENVIRONMENT_BOUND_P(env, symbol) \
  (!SCM_UNBNDP (SCM_ENVIRONMENT_REF (env, symbol)))
#define SCM_ENVIRONMENT_REF(env, symbol) \
  ((*(SCM_ENVIRONMENT_FUNCS (env)->ref)) (env, symbol))
#define SCM_ENVIRONMENT_FOLD(env, proc, data, init) \
  ((*(SCM_ENVIRONMENT_FUNCS (env)->fold)) (env, proc, data, init))
#define SCM_ENVIRONMENT_DEFINE(env, symbol, value) \
  ((*(SCM_ENVIRONMENT_FUNCS (env)->define)) (env, symbol, value))
#define SCM_ENVIRONMENT_UNDEFINE(env, symbol) \
  ((*(SCM_ENVIRONMENT_FUNCS (env)->undefine)) (env, symbol))
#define SCM_ENVIRONMENT_SET(env, symbol, value) \
  ((*(SCM_ENVIRONMENT_FUNCS (env)->set)) (env, symbol, value))
#define SCM_ENVIRONMENT_CELL(env, symbol, for_write) \
  ((*(SCM_ENVIRONMENT_FUNCS (env)->cell)) (env, symbol, for_write))
#define SCM_ENVIRONMENT_OBSERVE(env, proc, data, weak_p) \
  ((*(SCM_ENVIRONMENT_FUNCS (env)->observe)) (env, proc, data, weak_p))
#define SCM_ENVIRONMENT_UNOBSERVE(env, token) \
  ((*(SCM_ENVIRONMENT_FUNCS (env)->unobserve)) (env, token))

SCM_API scm_t_bits scm_tc16_observer;

#define SCM_OBSERVER_P(x) \
  (!SCM_IMP (x) && (SCM_CELL_TYPE (x) == scm_tc16_observer))
#define SCM_OBSERVER_ENVIRONMENT(x) \
  (SCM_CELL_OBJECT_1 (x))
#define SCM_OBSERVER_DATA(x) \
  (SCM_CELL_OBJECT_2 (x))
#define SCM_OBSERVER_PROC(x) \
  ((scm_environment_observer) SCM_CELL_WORD_3 (x))

SCM_API SCM scm_system_environment;

SCM_API void scm_error_environment_unbound (const char *, SCM, SCM) SCM_NORETURN;
SCM_API void scm_error_environment_immutable_binding (const char *, SCM, SCM) SCM_NORETURN;
SCM_API void scm_error_environment_immutable_location (const char *, SCM, SCM) SCM_NORETURN;

SCM_API SCM scm_make_environment (void *type);
SCM_API SCM scm_environment_p (SCM env);
SCM_API SCM scm_environment_bound_p (SCM env, SCM sym);
SCM_API SCM scm_environment_ref (SCM env, SCM sym);
SCM_API SCM scm_c_environment_ref (SCM env, SCM sym);
SCM_API SCM scm_environment_fold (SCM env, SCM proc, SCM init);
SCM_API SCM scm_c_environment_fold (SCM env, scm_environment_folder proc, SCM data, SCM init);
SCM_API SCM scm_environment_define (SCM env, SCM sym, SCM val);
SCM_API SCM scm_environment_undefine (SCM env, SCM sym);
SCM_API SCM scm_environment_set_x (SCM env, SCM sym, SCM val);
SCM_API SCM scm_environment_cell (SCM env, SCM sym, SCM for_write);
SCM_API SCM scm_c_environment_cell (SCM env, SCM sym, int for_write);
SCM_API SCM scm_environment_observe (SCM env, SCM proc);
SCM_API SCM scm_environment_observe_weak (SCM env, SCM proc);
SCM_API SCM scm_c_environment_observe (SCM env, scm_environment_observer proc, SCM data, int weak_p);
SCM_API SCM scm_environment_unobserve (SCM token);

SCM_API void scm_environments_prehistory (void);
SCM_API void scm_init_environments (void);



SCM_API void *scm_type_leaf_environment;

#define SCM_LEAF_ENVIRONMENT_P(env) \
  (SCM_ENVIRONMENT_P (env) \
   && SCM_ENVIRONMENT_FUNCS (env) == scm_type_leaf_environment)

SCM_API SCM scm_make_leaf_environment (void);
SCM_API SCM scm_leaf_environment_p (SCM env);



SCM_API void *scm_type_eval_environment;

#define SCM_EVAL_ENVIRONMENT_P(env) \
  (SCM_ENVIRONMENT_P (env) \
   && SCM_ENVIRONMENT_FUNCS (env) == scm_type_eval_environment)

SCM_API SCM scm_make_eval_environment (SCM local, SCM imported);
SCM_API SCM scm_eval_environment_p (SCM env);
SCM_API SCM scm_eval_environment_local (SCM env);
SCM_API SCM scm_eval_environment_set_local_x (SCM env, SCM local);
SCM_API SCM scm_eval_environment_imported (SCM env);
SCM_API SCM scm_eval_environment_set_imported_x (SCM env, SCM imported);



SCM_API void *scm_type_import_environment;

#define SCM_IMPORT_ENVIRONMENT_P(env) \
  (SCM_ENVIRONMENT_P (env) \
   && SCM_ENVIRONMENT_FUNCS (env) == scm_type_import_environment)

SCM_API SCM scm_make_import_environment (SCM imports, SCM conflict_proc);
SCM_API SCM scm_import_environment_p (SCM env);
SCM_API SCM scm_import_environment_imports (SCM env);
SCM_API SCM scm_import_environment_set_imports_x (SCM env, SCM imports);



SCM_API void *scm_type_export_environment;

#define SCM_EXPORT_ENVIRONMENT_P(env) \
  (SCM_ENVIRONMENT_P (env) \
   && SCM_ENVIRONMENT_FUNCS (env) == scm_type_export_environment)

SCM_API SCM scm_make_export_environment (SCM private, SCM signature);
SCM_API SCM scm_export_environment_p (SCM env);
SCM_API SCM scm_export_environment_private (SCM env);
SCM_API SCM scm_export_environment_set_private_x (SCM env, SCM private);
SCM_API SCM scm_export_environment_signature (SCM env);
SCM_API SCM scm_export_environment_set_signature_x (SCM env, SCM signature);

#endif  /* SCM_ENVIRONMENTS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
