/* classes: h_files */

#ifndef SCM_MODULES_H
#define SCM_MODULES_H

/* Copyright (C) 1998, 2000, 2001, 2002, 2003, 2006 Free Software Foundation, Inc.
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

#include "libguile/validate.h"



SCM_API int scm_module_system_booted_p;
SCM_API scm_t_bits scm_module_tag;

#define SCM_MODULEP(OBJ) \
  (!SCM_IMP (OBJ) && SCM_CELL_TYPE (OBJ) == scm_module_tag)

#define SCM_VALIDATE_MODULE(pos, scm) SCM_MAKE_VALIDATE_MSG (pos, scm, MODULEP, "module")

/* NOTE: Indexes of module fields are dependent upon the definition of
 *       module-type in boot-9.scm.
 */

#define scm_module_index_obarray	0
#define scm_module_index_uses		1
#define scm_module_index_binder		2
#define scm_module_index_eval_closure	3
#define scm_module_index_transformer	4

#define SCM_MODULE_OBARRAY(module) \
  SCM_PACK (SCM_STRUCT_DATA (module) [scm_module_index_obarray])
#define SCM_MODULE_USES(module) \
  SCM_PACK (SCM_STRUCT_DATA (module) [scm_module_index_uses])
#define SCM_MODULE_BINDER(module) \
  SCM_PACK (SCM_STRUCT_DATA (module) [scm_module_index_binder])
#define SCM_MODULE_EVAL_CLOSURE(module) \
  SCM_PACK (SCM_STRUCT_DATA (module)[scm_module_index_eval_closure])
#define SCM_MODULE_TRANSFORMER(module) \
  SCM_PACK (SCM_STRUCT_DATA (module)[scm_module_index_transformer])

SCM_API scm_t_bits scm_tc16_eval_closure;

#define SCM_EVAL_CLOSURE_P(x)	SCM_TYP16_PREDICATE (scm_tc16_eval_closure, x)



SCM_API SCM scm_current_module (void);
SCM_API SCM scm_interaction_environment (void);
SCM_API SCM scm_set_current_module (SCM module);

SCM_API SCM scm_c_call_with_current_module (SCM module,
					    SCM (*func)(void *), void *data);
SCM_API void scm_dynwind_current_module (SCM module);

SCM_API SCM scm_c_lookup (const char *name);
SCM_API SCM scm_c_define (const char *name, SCM val);
SCM_API SCM scm_lookup (SCM symbol);
SCM_API SCM scm_define (SCM symbol, SCM val);

SCM_API SCM scm_c_module_lookup (SCM module, const char *name);
SCM_API SCM scm_c_module_define (SCM module, const char *name, SCM val);
SCM_API SCM scm_module_lookup (SCM module, SCM symbol);
SCM_API SCM scm_module_define (SCM module, SCM symbol, SCM val);
SCM_API SCM scm_module_reverse_lookup (SCM module, SCM variable);

SCM_API SCM scm_c_resolve_module (const char *name);
SCM_API SCM scm_resolve_module (SCM name);
SCM_API SCM scm_c_define_module (const char *name,
				 void (*init)(void *), void *data);
SCM_API void scm_c_use_module (const char *name);
SCM_API void scm_c_export (const char *name, ...);

SCM_API SCM scm_sym2var (SCM sym, SCM thunk, SCM definep);

SCM_API SCM scm_module_import_interface (SCM module, SCM sym);
SCM_API SCM scm_module_lookup_closure (SCM module);
SCM_API SCM scm_module_transformer (SCM module);
SCM_API SCM scm_current_module_lookup_closure (void);
SCM_API SCM scm_current_module_transformer (void);
SCM_API SCM scm_eval_closure_lookup (SCM eclo, SCM sym, SCM definep);
SCM_API SCM scm_standard_eval_closure (SCM module);
SCM_API SCM scm_standard_interface_eval_closure (SCM module);
SCM_API SCM scm_get_pre_modules_obarray (void);
SCM_API SCM scm_lookup_closure_module (SCM proc);

SCM_API SCM scm_env_top_level (SCM env);
SCM_API SCM scm_env_module (SCM env);
SCM_API SCM scm_top_level_env (SCM thunk);
SCM_API SCM scm_system_module_env_p (SCM env);

SCM_API void scm_modules_prehistory (void);
SCM_API void scm_init_modules (void);

#endif  /* SCM_MODULES_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
