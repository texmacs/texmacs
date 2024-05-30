/* classes: h_files */

#ifndef SCM_FLUIDS_H
#define SCM_FLUIDS_H

/* Copyright (C) 1996,2000,2001, 2006 Free Software Foundation, Inc.
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
#include "libguile/root.h"
#include "libguile/vectors.h"

/* Fluids.

   Fluids are objects of a certain type (a smob) that can hold one SCM
   value per dynamic state.  That is, modifications to this value are
   only visible to code that executes with the same dynamic state as
   the modifying code.  When a new dynamic state is constructed, it
   inherits the values from its parent.  Because each thread executes
   with its own dynamic state, you can use fluids for thread local
   storage.

   Each fluid is identified by a small integer.  This integer is used
   to index a vector that holds the values of all fluids.  A dynamic
   state consists of this vector, wrapped in a smob so that the vector
   can grow.
 */

/* The fastest way to acces/modify the value of a fluid.  These macros
   do no error checking at all.  The first argument is the index
   number of the fluid, obtained via SCM_FLUID_NUM, not the fluid
   itself.  You must make sure that the fluid remains protected as
   long you use its number since numbers of unused fluids are reused
   eventually.
*/

#define SCM_FLUID_NUM(x)             scm_i_fluid_num (x)
#define SCM_FAST_FLUID_REF(n)        scm_i_fast_fluid_ref (n)
#define SCM_FAST_FLUID_SET_X(n, val) scm_i_fast_fluid_set_x ((n),(val))

SCM_API SCM scm_make_fluid (void);
SCM_API int scm_is_fluid (SCM obj);
SCM_API SCM scm_fluid_p (SCM fl);
SCM_API SCM scm_fluid_ref (SCM fluid);
SCM_API SCM scm_fluid_set_x (SCM fluid, SCM value);
SCM_API size_t scm_i_fluid_num (SCM fl);
SCM_API SCM scm_i_fast_fluid_ref (size_t n);
SCM_API void scm_i_fast_fluid_set_x (size_t n, SCM val);

SCM_API SCM scm_c_with_fluids (SCM fluids, SCM vals,
			       SCM (*cproc)(void *), void *cdata);
SCM_API SCM scm_c_with_fluid (SCM fluid, SCM val,
			      SCM (*cproc)(void *), void *cdata);
SCM_API SCM scm_with_fluids (SCM fluids, SCM vals, SCM thunk);
SCM_API SCM scm_with_fluid (SCM fluid, SCM val, SCM thunk);

SCM_API void scm_dynwind_fluid (SCM fluid, SCM value);

SCM_API SCM scm_make_dynamic_state (SCM parent);
SCM_API SCM scm_dynamic_state_p (SCM obj);
SCM_API int scm_is_dynamic_state (SCM obj);
SCM_API SCM scm_current_dynamic_state (void);
SCM_API SCM scm_set_current_dynamic_state (SCM state);
SCM_API void scm_dynwind_current_dynamic_state (SCM state);
SCM_API void *scm_c_with_dynamic_state (SCM state, 
					void *(*func)(void *), void *data);
SCM_API SCM scm_with_dynamic_state (SCM state, SCM proc);

SCM_API SCM scm_i_make_initial_dynamic_state (void);

SCM_API void scm_fluids_prehistory (void);
SCM_API void scm_init_fluids (void);

#endif  /* SCM_FLUIDS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
