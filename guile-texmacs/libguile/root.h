/* classes: h_files */

#ifndef SCM_ROOT_H
#define SCM_ROOT_H

/* Copyright (C) 1996,1998,2000,2001, 2002, 2006 Free Software Foundation, Inc.
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
#include "libguile/debug.h"
#include "libguile/throw.h"



#define scm_flo0 scm_sys_protects[0]
#define scm_listofnull scm_sys_protects[1]
#define scm_nullvect scm_sys_protects[2]
#define scm_nullstr scm_sys_protects[3]
#define scm_keyword_obarray scm_sys_protects[4]
#define scm_stand_in_procs scm_sys_protects[5]
#define scm_object_whash scm_sys_protects[6]
#define scm_permobjs scm_sys_protects[7]
#define scm_asyncs scm_sys_protects[8]
#define scm_protects scm_sys_protects[9]
#define scm_properties_whash scm_sys_protects[10]
#define scm_gc_registered_roots scm_sys_protects[11]
#define scm_source_whash scm_sys_protects[12]
#define SCM_NUM_PROTECTS 13

SCM_API SCM scm_sys_protects[];



SCM_API SCM scm_internal_cwdr (scm_t_catch_body body,
			       void *body_data,
			       scm_t_catch_handler handler,
			       void *handler_data,
			       SCM_STACKITEM *stack_start);
SCM_API SCM scm_call_with_dynamic_root (SCM thunk, SCM handler);
SCM_API SCM scm_dynamic_root (void);
SCM_API SCM scm_apply_with_dynamic_root (SCM proc, SCM a1, SCM args, SCM handler);
SCM_API void scm_init_root (void);

#endif  /* SCM_ROOT_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
