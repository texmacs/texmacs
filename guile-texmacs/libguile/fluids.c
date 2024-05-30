/* Copyright (C) 1996,1997,2000,2001, 2004, 2006, 2007, 2008 Free Software Foundation, Inc.
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>
#include <string.h>

#include "libguile/_scm.h"
#include "libguile/print.h"
#include "libguile/smob.h"
#include "libguile/dynwind.h"
#include "libguile/fluids.h"
#include "libguile/alist.h"
#include "libguile/eval.h"
#include "libguile/ports.h"
#include "libguile/deprecation.h"
#include "libguile/lang.h"
#include "libguile/validate.h"

#define FLUID_GROW 20

/* A lot of the complexity below stems from the desire to reuse fluid
   slots.  Normally, fluids should be pretty global and long-lived
   things, so that reusing their slots should not be overly critical,
   but it is the right thing to do nevertheless.  The code therefore
   puts the burdon on allocating and collection fluids and keeps
   accessing fluids lock free.  This is achieved by manipulating the
   global state of the fluid machinery mostly in single threaded
   sections.

   Reusing a fluid slot means that it must be reset to #f in all
   dynamic states.  We do this by maintaining a weak list of all
   dynamic states, which is used after a GC to do the resetting.

   Also, the fluid vectors in the dynamic states need to grow from
   time to time when more fluids are created.  We do this in a single
   threaded section so that threads do not need to lock when accessing
   a fluid in the normal way.
*/

static scm_i_pthread_mutex_t fluid_admin_mutex = SCM_I_PTHREAD_MUTEX_INITIALIZER;

/* Protected by fluid_admin_mutex, but also accessed during GC.  See
   next_fluid_num for a discussion of this.
 */
static size_t allocated_fluids_len = 0;
static size_t allocated_fluids_num = 0;
static char *allocated_fluids = NULL;

static scm_t_bits tc16_fluid;

#define IS_FLUID(x)         SCM_SMOB_PREDICATE(tc16_fluid, (x))
#define FLUID_NUM(x)        ((size_t)SCM_SMOB_DATA(x))
#define FLUID_NEXT(x)       SCM_SMOB_OBJECT_2(x)
#define FLUID_NEXT_LOC(x)       SCM_SMOB_OBJECT_2_LOC(x)
#define SET_FLUID_NEXT(x,y) SCM_SET_SMOB_OBJECT_2((x), (y))

static scm_t_bits tc16_dynamic_state;

#define IS_DYNAMIC_STATE(x)        SCM_SMOB_PREDICATE(tc16_dynamic_state, (x))
#define DYNAMIC_STATE_FLUIDS(x)        SCM_SMOB_OBJECT(x)
#define SET_DYNAMIC_STATE_FLUIDS(x, y) SCM_SET_SMOB_OBJECT((x), (y))
#define DYNAMIC_STATE_NEXT(x)          SCM_SMOB_OBJECT_2(x)
#define DYNAMIC_STATE_NEXT_LOC(x)          SCM_SMOB_OBJECT_2_LOC(x)
#define SET_DYNAMIC_STATE_NEXT(x, y)   SCM_SET_SMOB_OBJECT_2((x), (y))

/* Weak lists of all dynamic states and all fluids.
 */
static SCM all_dynamic_states = SCM_EOL;
static SCM all_fluids = SCM_EOL;

/* Make sure that all states have the right size.  This must be called
   while fluid_admin_mutex is held.
*/
static void
resize_all_states ()
{
  SCM new_vectors, state;

  /* Replacing the vector of a dynamic state must be done atomically:
     the old values must be copied into the new vector and the new
     vector must be installed without someone modifying the old vector
     concurrently.  Since accessing a fluid should be lock-free, we
     need to put all threads to sleep when replacing a vector.
     However, when being single threaded, it is best not to do much.
     Therefore, we allocate the new vectors before going single
     threaded.
  */

  new_vectors = SCM_EOL;
  for (state = all_dynamic_states; !scm_is_null (state);
       state = DYNAMIC_STATE_NEXT (state))
    new_vectors = scm_cons (scm_c_make_vector (allocated_fluids_len,
					       SCM_BOOL_F),
			    new_vectors);

  scm_i_thread_put_to_sleep ();
  for (state = all_dynamic_states; !scm_is_null (state);
       state = DYNAMIC_STATE_NEXT (state))
    {
      SCM old_fluids = DYNAMIC_STATE_FLUIDS (state);
      SCM new_fluids = SCM_CAR (new_vectors);
      size_t i, old_len = SCM_SIMPLE_VECTOR_LENGTH (old_fluids);

      for (i = 0; i < old_len; i++)
	SCM_SIMPLE_VECTOR_SET (new_fluids, i,
			       SCM_SIMPLE_VECTOR_REF (old_fluids, i));
      SET_DYNAMIC_STATE_FLUIDS (state, new_fluids);
      new_vectors = SCM_CDR (new_vectors);
    }
  scm_i_thread_wake_up ();
}

/* This is called during GC, that is, while being single threaded.
   See next_fluid_num for a discussion why it is safe to access
   allocated_fluids here.
 */
static void *
scan_dynamic_states_and_fluids (void *dummy1 SCM_UNUSED,
				void *dummy2 SCM_UNUSED,
				void *dummy3 SCM_UNUSED)
{
  SCM *statep, *fluidp;

  /* Scan all fluids and deallocate the unmarked ones.
   */
  fluidp = &all_fluids;
  while (!scm_is_null (*fluidp))
    {
      if (!SCM_GC_MARK_P (*fluidp))
	{
	  allocated_fluids_num -= 1;
	  allocated_fluids[FLUID_NUM (*fluidp)] = 0;
	  *fluidp = FLUID_NEXT (*fluidp);
	}
      else
	fluidp = FLUID_NEXT_LOC (*fluidp);
    }

  /* Scan all dynamic states and remove the unmarked ones.  The live
     ones are updated for unallocated fluids.
  */
  statep = &all_dynamic_states;
  while (!scm_is_null (*statep))
    {
      if (!SCM_GC_MARK_P (*statep))
	*statep = DYNAMIC_STATE_NEXT (*statep);
      else
	{
	  SCM fluids = DYNAMIC_STATE_FLUIDS (*statep);
	  size_t len, i;
	  
	  len = SCM_SIMPLE_VECTOR_LENGTH (fluids);
	  for (i = 0; i < len && i < allocated_fluids_len; i++)
	    if (allocated_fluids[i] == 0)
	      SCM_SIMPLE_VECTOR_SET (fluids, i, SCM_BOOL_F);

	  statep = DYNAMIC_STATE_NEXT_LOC (*statep);
	}
    }

  return NULL;
}

static size_t
fluid_free (SCM fluid)
{
  /* The real work is done in scan_dynamic_states_and_fluids.  We can
     not touch allocated_fluids etc here since a smob free routine can
     be run at any time, in any thread.
  */
  return 0;
}

static int
fluid_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_puts ("#<fluid ", port);
  scm_intprint ((int) FLUID_NUM (exp), 10, port);
  scm_putc ('>', port);
  return 1;
}

static size_t
next_fluid_num ()
{
  size_t n;

  scm_dynwind_begin (0);
  scm_i_dynwind_pthread_mutex_lock (&fluid_admin_mutex);

  if ((allocated_fluids_len > 0) &&
      (allocated_fluids_num == allocated_fluids_len))
    {
      /* All fluid numbers are in use.  Run a GC to try to free some
	 up.
      */
      scm_gc ();
    }

  if (allocated_fluids_num < allocated_fluids_len)
    {
      for (n = 0; n < allocated_fluids_len; n++)
	if (allocated_fluids[n] == 0)
	  break;
    }
  else
    {
      /* During the following call, the GC might run and elements of
	 allocated_fluids might bet set to zero.  Also,
	 allocated_fluids and allocated_fluids_len are used to scan
	 all dynamic states during GC.  Thus we need to make sure that
	 no GC can run while updating these two variables.
      */

      char *prev_allocated_fluids;
      char *new_allocated_fluids =
	scm_malloc (allocated_fluids_len + FLUID_GROW);

      /* Copy over old values and initialize rest.  GC can not run
	 during these two operations since there is no safe point in
	 them.
      */
      memcpy (new_allocated_fluids, allocated_fluids, allocated_fluids_len);
      memset (new_allocated_fluids + allocated_fluids_len, 0, FLUID_GROW);
      n = allocated_fluids_len;

      prev_allocated_fluids = allocated_fluids;
      allocated_fluids = new_allocated_fluids;
      allocated_fluids_len += FLUID_GROW;

      if (prev_allocated_fluids != NULL)
	free (prev_allocated_fluids);

      /* Now allocated_fluids and allocated_fluids_len are valid again
	 and we can allow GCs to occur.
      */
      resize_all_states ();
    }
  
  allocated_fluids_num += 1;
  allocated_fluids[n] = 1;
  
  scm_dynwind_end ();
  return n;
}

SCM_DEFINE (scm_make_fluid, "make-fluid", 0, 0, 0, 
	    (),
	    "Return a newly created fluid.\n"
	    "Fluids are objects that can hold one\n"
	    "value per dynamic state.  That is, modifications to this value are\n"
	    "only visible to code that executes with the same dynamic state as\n"
	    "the modifying code.  When a new dynamic state is constructed, it\n"
	    "inherits the values from its parent.  Because each thread normally executes\n"
	    "with its own dynamic state, you can use fluids for thread local storage.")
#define FUNC_NAME s_scm_make_fluid
{
  SCM fluid;

  SCM_NEWSMOB2 (fluid, tc16_fluid,
		(scm_t_bits) next_fluid_num (), SCM_UNPACK (SCM_EOL));

  /* The GC must not run until the fluid is properly entered into the
     list.
  */
  scm_i_scm_pthread_mutex_lock (&fluid_admin_mutex);
  SET_FLUID_NEXT (fluid, all_fluids);
  all_fluids = fluid;
  scm_i_pthread_mutex_unlock (&fluid_admin_mutex);

  return fluid;
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_p, "fluid?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} iff @var{obj} is a fluid; otherwise, return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_fluid_p
{
  return scm_from_bool (IS_FLUID (obj));
}
#undef FUNC_NAME

int
scm_is_fluid (SCM obj)
{
  return IS_FLUID (obj);
}

size_t
scm_i_fluid_num (SCM fluid)
{
  return FLUID_NUM (fluid);
}

SCM_DEFINE (scm_fluid_ref, "fluid-ref", 1, 0, 0, 
	    (SCM fluid),
	    "Return the value associated with @var{fluid} in the current\n"
	    "dynamic root.  If @var{fluid} has not been set, then return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_fluid_ref
{
  SCM fluids = DYNAMIC_STATE_FLUIDS (SCM_I_CURRENT_THREAD->dynamic_state);

  SCM_VALIDATE_FLUID (1, fluid);
  return SCM_SIMPLE_VECTOR_REF (fluids, FLUID_NUM (fluid));
}
#undef FUNC_NAME

SCM
scm_i_fast_fluid_ref (size_t n)
{
  SCM fluids = DYNAMIC_STATE_FLUIDS (SCM_I_CURRENT_THREAD->dynamic_state);
  return SCM_SIMPLE_VECTOR_REF (fluids, n);
}

SCM_DEFINE (scm_fluid_set_x, "fluid-set!", 2, 0, 0,
	    (SCM fluid, SCM value),
	    "Set the value associated with @var{fluid} in the current dynamic root.")
#define FUNC_NAME s_scm_fluid_set_x
{
  SCM fluids = DYNAMIC_STATE_FLUIDS (SCM_I_CURRENT_THREAD->dynamic_state);

  SCM_VALIDATE_FLUID (1, fluid);
  SCM_SIMPLE_VECTOR_SET (fluids, FLUID_NUM (fluid), value);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_i_fast_fluid_set_x (size_t n, SCM value)
{
  SCM fluids = DYNAMIC_STATE_FLUIDS (SCM_I_CURRENT_THREAD->dynamic_state);
  SCM_SIMPLE_VECTOR_SET (fluids, n, value);
}

static void
swap_fluids (SCM data)
{
  SCM fluids = SCM_CAR (data), vals = SCM_CDR (data);
  
  while (!SCM_NULL_OR_NIL_P (fluids))
    {
      SCM fl = SCM_CAR (fluids);
      SCM old_val = scm_fluid_ref (fl);
      scm_fluid_set_x (fl, SCM_CAR (vals));
      SCM_SETCAR (vals, old_val);
      fluids = SCM_CDR (fluids);
      vals = SCM_CDR (vals);
    }
}

/* Swap the fluid values in reverse order.  This is important when the
   same fluid appears multiple times in the fluids list.
*/

static void
swap_fluids_reverse_aux (SCM fluids, SCM vals)
{
  if (!SCM_NULL_OR_NIL_P (fluids))
    {
      SCM fl, old_val;

      swap_fluids_reverse_aux (SCM_CDR (fluids), SCM_CDR (vals));
      fl = SCM_CAR (fluids);
      old_val = scm_fluid_ref (fl);
      scm_fluid_set_x (fl, SCM_CAR (vals));
      SCM_SETCAR (vals, old_val);
    }
}

static void
swap_fluids_reverse (SCM data)
{
  swap_fluids_reverse_aux (SCM_CAR (data), SCM_CDR (data));
}

static SCM
apply_thunk (void *thunk)
{
  return scm_call_0 (SCM_PACK (thunk));
}

SCM_DEFINE (scm_with_fluids, "with-fluids*", 3, 0, 0, 
	    (SCM fluids, SCM values, SCM thunk),
	    "Set @var{fluids} to @var{values} temporary, and call @var{thunk}.\n"
	    "@var{fluids} must be a list of fluids and @var{values} must be the same\n"
	    "number of their values to be applied.  Each substitution is done\n"
	    "one after another.  @var{thunk} must be a procedure with no argument.")
#define FUNC_NAME s_scm_with_fluids
{
  return scm_c_with_fluids (fluids, values,
			    apply_thunk, (void *) SCM_UNPACK (thunk));
}
#undef FUNC_NAME

SCM
scm_c_with_fluids (SCM fluids, SCM values, SCM (*cproc) (), void *cdata)
#define FUNC_NAME "scm_c_with_fluids"
{
  SCM ans, data;
  long flen, vlen;

  SCM_VALIDATE_LIST_COPYLEN (1, fluids, flen);
  SCM_VALIDATE_LIST_COPYLEN (2, values, vlen);
  if (flen != vlen)
    scm_out_of_range (s_scm_with_fluids, values);

  if (flen == 1)
    return scm_c_with_fluid (SCM_CAR (fluids), SCM_CAR (values),
			     cproc, cdata);
  
  data = scm_cons (fluids, values);
  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  scm_dynwind_rewind_handler_with_scm (swap_fluids, data,
				     SCM_F_WIND_EXPLICITLY);
  scm_dynwind_unwind_handler_with_scm (swap_fluids_reverse, data,
				     SCM_F_WIND_EXPLICITLY);
  ans = cproc (cdata);
  scm_dynwind_end ();
  return ans;
}
#undef FUNC_NAME

SCM_DEFINE (scm_with_fluid, "with-fluid*", 3, 0, 0, 
	    (SCM fluid, SCM value, SCM thunk),
	    "Set @var{fluid} to @var{value} temporarily, and call @var{thunk}.\n"
	    "@var{thunk} must be a procedure with no argument.")
#define FUNC_NAME s_scm_with_fluid
{
  return scm_c_with_fluid (fluid, value,
			   apply_thunk, (void *) SCM_UNPACK (thunk));
}
#undef FUNC_NAME

SCM
scm_c_with_fluid (SCM fluid, SCM value, SCM (*cproc) (), void *cdata)
#define FUNC_NAME "scm_c_with_fluid"
{
  SCM ans;

  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  scm_dynwind_fluid (fluid, value);
  ans = cproc (cdata);
  scm_dynwind_end ();
  return ans;
}
#undef FUNC_NAME

static void
swap_fluid (SCM data)
{
  SCM f = SCM_CAR (data);
  SCM t = scm_fluid_ref (f);
  scm_fluid_set_x (f, SCM_CDR (data));
  SCM_SETCDR (data, t);
}

void
scm_dynwind_fluid (SCM fluid, SCM value)
{
  SCM data = scm_cons (fluid, value);
  scm_dynwind_rewind_handler_with_scm (swap_fluid, data, SCM_F_WIND_EXPLICITLY);
  scm_dynwind_unwind_handler_with_scm (swap_fluid, data, SCM_F_WIND_EXPLICITLY);
}

SCM
scm_i_make_initial_dynamic_state ()
{
  SCM fluids = scm_c_make_vector (allocated_fluids_len, SCM_BOOL_F);
  SCM state;
  SCM_NEWSMOB2 (state, tc16_dynamic_state,
		SCM_UNPACK (fluids), SCM_UNPACK (SCM_EOL));
  all_dynamic_states = state;
  return state;
}

SCM_DEFINE (scm_make_dynamic_state, "make-dynamic-state", 0, 1, 0,
	    (SCM parent),
	    "Return a copy of the dynamic state object @var{parent}\n"
	    "or of the current dynamic state when @var{parent} is omitted.")
#define FUNC_NAME s_scm_make_dynamic_state
{
  SCM fluids, state;

  if (SCM_UNBNDP (parent))
    parent = scm_current_dynamic_state ();

  scm_assert_smob_type (tc16_dynamic_state, parent);
  fluids = scm_vector_copy (DYNAMIC_STATE_FLUIDS (parent));
  SCM_NEWSMOB2 (state, tc16_dynamic_state,
		SCM_UNPACK (fluids), SCM_UNPACK (SCM_EOL));

  /* The GC must not run until the state is properly entered into the
     list. 
  */
  scm_i_scm_pthread_mutex_lock (&fluid_admin_mutex);
  SET_DYNAMIC_STATE_NEXT (state, all_dynamic_states);
  all_dynamic_states = state;
  scm_i_pthread_mutex_unlock (&fluid_admin_mutex);

  return state;
}
#undef FUNC_NAME

SCM_DEFINE (scm_dynamic_state_p, "dynamic-state?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a dynamic state object;\n"
	    "return @code{#f} otherwise")
#define FUNC_NAME s_scm_dynamic_state_p
{
  return scm_from_bool (IS_DYNAMIC_STATE (obj));
}
#undef FUNC_NAME

int
scm_is_dynamic_state (SCM obj)
{
  return IS_DYNAMIC_STATE (obj);
}

SCM_DEFINE (scm_current_dynamic_state, "current-dynamic-state", 0, 0, 0,
	    (),
	    "Return the current dynamic state object.")
#define FUNC_NAME s_scm_current_dynamic_state
{
  return SCM_I_CURRENT_THREAD->dynamic_state;
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_current_dynamic_state, "set-current-dynamic-state", 1,0,0,
	    (SCM state),
	    "Set the current dynamic state object to @var{state}\n"
	    "and return the previous current dynamic state object.")
#define FUNC_NAME s_scm_set_current_dynamic_state
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  SCM old = t->dynamic_state;
  scm_assert_smob_type (tc16_dynamic_state, state);
  t->dynamic_state = state;
  return old;
}
#undef FUNC_NAME

static void
swap_dynamic_state (SCM loc)
{
  SCM_SETCAR (loc, scm_set_current_dynamic_state (SCM_CAR (loc)));
}

void
scm_dynwind_current_dynamic_state (SCM state)
{
  SCM loc = scm_cons (state, SCM_EOL);
  scm_assert_smob_type (tc16_dynamic_state, state);
  scm_dynwind_rewind_handler_with_scm (swap_dynamic_state, loc,
				     SCM_F_WIND_EXPLICITLY);
  scm_dynwind_unwind_handler_with_scm (swap_dynamic_state, loc,
				     SCM_F_WIND_EXPLICITLY);
}

void *
scm_c_with_dynamic_state (SCM state, void *(*func)(void *), void *data)
{
  void *result;
  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  scm_dynwind_current_dynamic_state (state);
  result = func (data);
  scm_dynwind_end ();
  return result;
}

SCM_DEFINE (scm_with_dynamic_state, "with-dynamic-state", 2, 0, 0,
	    (SCM state, SCM proc),
	    "Call @var{proc} while @var{state} is the current dynamic\n"
	    "state object.")
#define FUNC_NAME s_scm_with_dynamic_state
{
  SCM result;
  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  scm_dynwind_current_dynamic_state (state);
  result = scm_call_0 (proc);
  scm_dynwind_end ();
  return result;
}
#undef FUNC_NAME

void
scm_fluids_prehistory ()
{
  tc16_fluid = scm_make_smob_type ("fluid", 0);
  scm_set_smob_free (tc16_fluid, fluid_free);
  scm_set_smob_print (tc16_fluid, fluid_print);

  tc16_dynamic_state = scm_make_smob_type ("dynamic-state", 0);
  scm_set_smob_mark (tc16_dynamic_state, scm_markcdr);

  scm_c_hook_add (&scm_after_sweep_c_hook, scan_dynamic_states_and_fluids,
		  0, 0);
}

void
scm_init_fluids ()
{
#include "libguile/fluids.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
