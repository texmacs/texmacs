/* classes: h_files */

#ifndef FLUIDSH
#define FLUIDSH

/*	Copyright (C) 1996, 2000 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */

#include "libguile/__scm.h"
#include "libguile/root.h"
#include "libguile/vectors.h"

/* Fluids.

   Fluids are objects of a certain type (a smob) that can hold one SCM
   value per dynamic root.  That is, modifications to this value are
   only visible to code that executes within the same dynamic root as
   the modifying code.  When a new dynamic root is constructed, it
   inherits the values from its parent.  Because each thread executes
   in its own dynamic root, you can use fluids for thread local
   storage.

   Each fluid is identified by a small integer.  This integer is used
   to index a vector that holds the values of all fluids.  Each root
   has its own vector.

   Currently, you can't get rid a certain fluid if you don't use it
   any longer.  The slot that has been allocated for it in the fluid
   vector will not be reused for other fluids.  Therefore, only use
   SCM_MAKE_FLUID or its Scheme variant `make-fluid' in initialization
   code that is only run once.  Nevertheless, it should be possible to
   implement a more lightweight version of fluids on top of this basic
   mechanism. */

GUILE_API extern long scm_tc16_fluid;

#define SCM_FLUIDP(x)    (!SCM_IMP (x) && (SCM_CELL_TYPE (x) == scm_tc16_fluid))
#define SCM_FLUID_NUM(x) (SCM_CELL_WORD_1 (x))

/* The fastest way to acces/modify the value of a fluid.  These macros
do no error checking at all.  You should only use them when you know
that the relevant fluid already exists in the current dynamic root.
The easiest way to ensure this is to execute a SCM_FLUID_SET_X in the
topmost root, for example right after SCM_MAKE_FLUID in your
SCM_INIT_MUMBLE routine that gets called from SCM_BOOT_GUILE_1.  The
first argument is the index number of the fluid, obtained via
SCM_FLUID_NUM, not the fluid itself. */

#define SCM_FAST_FLUID_REF(n) (SCM_VELTS(scm_root->fluids)[n])
#define SCM_FAST_FLUID_SET_X(n, val) (SCM_VELTS(scm_root->fluids)[n] = val)

GUILE_API SCM scm_make_fluid (void);
GUILE_API SCM scm_fluid_p (SCM fl);
GUILE_API SCM scm_fluid_ref (SCM fluid);
GUILE_API SCM scm_fluid_set_x (SCM fluid, SCM value);

GUILE_API SCM scm_internal_with_fluids (SCM fluids, SCM vals,
                              SCM (*cproc)(void *), void *cdata);
GUILE_API SCM scm_with_fluids (SCM fluids, SCM vals, SCM thunk);

GUILE_API SCM scm_make_initial_fluids (void);
GUILE_API void scm_copy_fluids (scm_root_state *);
GUILE_API void scm_swap_fluids (SCM fluids, SCM vals);
GUILE_API void scm_swap_fluids_reverse (SCM fluids, SCM vals);

GUILE_API void scm_init_fluids (void);

#endif /* !FLUIDSH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
