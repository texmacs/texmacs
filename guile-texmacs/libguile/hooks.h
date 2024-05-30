/* classes: h_files */

#ifndef SCM_HOOKS_H
#define SCM_HOOKS_H

/* Copyright (C) 1995,1996,1999,2000,2001, 2006 Free Software Foundation, Inc.
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

/*
 * C level hooks
 */

/*
 * The interface is designed for and- and or-type hooks which
 * both may want to indicate success/failure and return a result.
 */

typedef enum scm_t_c_hook_type {
  SCM_C_HOOK_NORMAL,
  SCM_C_HOOK_OR,
  SCM_C_HOOK_AND
} scm_t_c_hook_type;

typedef void  *(*scm_t_c_hook_function) (void *hook_data,
					 void *fn_data,
					 void *data);

typedef struct scm_t_c_hook_entry {
  struct scm_t_c_hook_entry *next;
  scm_t_c_hook_function func;
  void *data;
} scm_t_c_hook_entry;

typedef struct scm_t_c_hook {
  scm_t_c_hook_entry *first;
  scm_t_c_hook_type type;
  void *data;
} scm_t_c_hook;

SCM_API void scm_c_hook_init (scm_t_c_hook *hook,
			      void *hook_data,
			      scm_t_c_hook_type type);
SCM_API void scm_c_hook_add (scm_t_c_hook *hook,
			     scm_t_c_hook_function func,
			     void *fn_data, 
			     int appendp);
SCM_API void scm_c_hook_remove (scm_t_c_hook *hook,
				scm_t_c_hook_function func,
				void *fn_data);
SCM_API void *scm_c_hook_run (scm_t_c_hook *hook, void *data);

/*
 * Scheme level hooks
 */

SCM_API scm_t_bits scm_tc16_hook;

#define SCM_HOOKP(x)			SCM_SMOB_PREDICATE (scm_tc16_hook, x)
#define SCM_HOOK_ARITY(hook)		SCM_SMOB_FLAGS (hook)
#define SCM_HOOK_PROCEDURES(hook)	SCM_SMOB_OBJECT (hook)
#define SCM_SET_HOOK_PROCEDURES(hook, procs) SCM_SET_SMOB_OBJECT ((hook), (procs))

SCM_API SCM scm_make_hook (SCM n_args);
SCM_API SCM scm_hook_p (SCM x);
SCM_API SCM scm_hook_empty_p (SCM hook);
SCM_API SCM scm_add_hook_x (SCM hook, SCM thunk, SCM appendp);
SCM_API SCM scm_remove_hook_x (SCM hook, SCM thunk);
SCM_API SCM scm_reset_hook_x (SCM hook);
SCM_API SCM scm_run_hook (SCM hook, SCM args);
SCM_API void scm_c_run_hook (SCM hook, SCM args);
SCM_API SCM scm_hook_to_list (SCM hook);
SCM_API void scm_init_hooks (void);

#endif  /* SCM_HOOKS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
