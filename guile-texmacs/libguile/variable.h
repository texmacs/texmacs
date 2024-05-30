/* classes: h_files */

#ifndef SCM_VARIABLE_H
#define SCM_VARIABLE_H

/* Copyright (C) 1995,1996,2000,2001, 2006 Free Software Foundation, Inc.
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
#include "libguile/smob.h"



/* Variables 
 */
#define SCM_VARIABLEP(X)      (!SCM_IMP (X) && SCM_TYP7(X) == scm_tc7_variable)
#define SCM_VARIABLE_REF(V)   SCM_CELL_OBJECT_1 (V)
#define SCM_VARIABLE_SET(V, X) SCM_SET_CELL_OBJECT_1 (V, X)
#define SCM_VARIABLE_LOC(V)   (SCM_CELL_OBJECT_LOC ((V), 1))



SCM_API SCM scm_make_variable (SCM init);
SCM_API SCM scm_make_undefined_variable (void);
SCM_API SCM scm_variable_p (SCM obj);
SCM_API SCM scm_variable_ref (SCM var);
SCM_API SCM scm_variable_set_x (SCM var, SCM val);
SCM_API SCM scm_variable_bound_p (SCM var);

SCM_API void scm_i_variable_print (SCM var, SCM port, scm_print_state *pstate);

SCM_API void scm_init_variable (void);

#endif  /* SCM_VARIABLE_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
