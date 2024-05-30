/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2002, 2004, 2006, 2008 Free Software Foundation, Inc.
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
#  include <config.h>
#endif

#include <signal.h>
#include "libguile/_scm.h"
#include "libguile/eval.h"
#include "libguile/throw.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/lang.h"
#include "libguile/dynwind.h"
#include "libguile/deprecation.h"

#include "libguile/validate.h"
#include "libguile/async.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


/* {Asynchronous Events}
 *
 * There are two kinds of asyncs: system asyncs and user asyncs.  The
 * two kinds have some concepts in commen but work slightly
 * differently and are not interchangeable.
 *
 * System asyncs are used to run arbitrary code at the next safe point
 * in a specified thread.  You can use them to trigger execution of
 * Scheme code from signal handlers or to interrupt a thread, for
 * example.
 *
 * Each thread has a list of 'activated asyncs', which is a normal
 * Scheme list of procedures with zero arguments.  When a thread
 * executes a SCM_ASYNC_TICK statement (which is included in
 * SCM_TICK), it will call all procedures on this list.
 *
 * Also, a thread will wake up when a procedure is added to its list
 * of active asyncs and call them.  After that, it will go to sleep
 * again.  (Not implemented yet.)
 *
 *
 * User asyncs are a little data structure that consists of a
 * procedure of zero arguments and a mark.  There are functions for
 * setting the mark of a user async and for calling all procedures of
 * marked asyncs in a given list.  Nothing you couldn't quickly
 * implement yourself.
 */




/* User asyncs. */

static scm_t_bits tc16_async;

/* cmm: this has SCM_ prefix because SCM_MAKE_VALIDATE expects it.
   this is ugly.  */
#define SCM_ASYNCP(X)		SCM_TYP16_PREDICATE (tc16_async, X)
#define VALIDATE_ASYNC(pos, a)	SCM_MAKE_VALIDATE_MSG(pos, a, ASYNCP, "user async")

#define ASYNC_GOT_IT(X)        (SCM_CELL_WORD_0 (X) >> 16)
#define SET_ASYNC_GOT_IT(X, V) (SCM_SET_CELL_WORD_0 ((X), SCM_TYP16 (X) | ((V) << 16)))
#define ASYNC_THUNK(X)         SCM_CELL_OBJECT_1 (X)

static SCM
async_gc_mark (SCM obj)
{
  return ASYNC_THUNK (obj);
}

SCM_DEFINE (scm_async, "async", 1, 0, 0,
	    (SCM thunk),
	    "Create a new async for the procedure @var{thunk}.")
#define FUNC_NAME s_scm_async
{
  SCM_RETURN_NEWSMOB (tc16_async, SCM_UNPACK (thunk));
}
#undef FUNC_NAME

SCM_DEFINE (scm_async_mark, "async-mark", 1, 0, 0,
            (SCM a),
	    "Mark the async @var{a} for future execution.")
#define FUNC_NAME s_scm_async_mark
{
  VALIDATE_ASYNC (1, a);
  SET_ASYNC_GOT_IT (a, 1);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_run_asyncs, "run-asyncs", 1, 0, 0,
	    (SCM list_of_a),
	    "Execute all thunks from the asyncs of the list @var{list_of_a}.")
#define FUNC_NAME s_scm_run_asyncs
{
  while (! SCM_NULL_OR_NIL_P (list_of_a))
    {
      SCM a;
      SCM_VALIDATE_CONS (1, list_of_a);
      a = SCM_CAR (list_of_a);
      VALIDATE_ASYNC (SCM_ARG1, a);
      if (ASYNC_GOT_IT (a))
	{
	  SET_ASYNC_GOT_IT (a, 0);
	  scm_call_0 (ASYNC_THUNK (a));
	}
      list_of_a = SCM_CDR (list_of_a);
    }
  return SCM_BOOL_T;
}
#undef FUNC_NAME



static scm_i_pthread_mutex_t async_mutex = SCM_I_PTHREAD_MUTEX_INITIALIZER;

/* System asyncs. */

void
scm_async_click ()
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  SCM asyncs;

  /* Reset pending_asyncs even when asyncs are blocked and not really
     executed since this will avoid future futile calls to this
     function.  When asyncs are unblocked again, this function is
     invoked even when pending_asyncs is zero.
  */

  scm_i_pthread_mutex_lock (&async_mutex);
  t->pending_asyncs = 0;
  if (t->block_asyncs == 0)
    {
      asyncs = t->active_asyncs;
      t->active_asyncs = SCM_EOL;
    }
  else
    asyncs = SCM_EOL;
  scm_i_pthread_mutex_unlock (&async_mutex);

  while (scm_is_pair (asyncs))
    {
      SCM next = SCM_CDR (asyncs);
      SCM_SETCDR (asyncs, SCM_BOOL_F);
      scm_call_0 (SCM_CAR (asyncs));
      asyncs = next;
    }
}

#if (SCM_ENABLE_DEPRECATED == 1)

SCM_DEFINE (scm_system_async, "system-async", 1, 0, 0,
            (SCM thunk),
	    "This function is deprecated.  You can use @var{thunk} directly\n"
            "instead of explicitly creating an async object.\n")
#define FUNC_NAME s_scm_system_async
{
  scm_c_issue_deprecation_warning 
    ("'system-async' is deprecated.  "
     "Use the procedure directly with 'system-async-mark'.");
  return thunk;
}
#undef FUNC_NAME

#endif /* SCM_ENABLE_DEPRECATED == 1 */

void
scm_i_queue_async_cell (SCM c, scm_i_thread *t)
{
  SCM sleep_object;
  scm_i_pthread_mutex_t *sleep_mutex;
  int sleep_fd;
  SCM p;
  
  scm_i_pthread_mutex_lock (&async_mutex);
  p = t->active_asyncs;
  SCM_SETCDR (c, SCM_EOL);
  if (!scm_is_pair (p))
    t->active_asyncs = c;
  else
    {
      SCM pp;
      while (scm_is_pair (pp = SCM_CDR (p)))
	{
	  if (scm_is_eq (SCM_CAR (p), SCM_CAR (c)))
	    {
	      scm_i_pthread_mutex_unlock (&async_mutex);
	      return;
	    }
	  p = pp;
	}
      SCM_SETCDR (p, c);
    }
  t->pending_asyncs = 1;
  sleep_object = t->sleep_object;
  sleep_mutex = t->sleep_mutex;
  sleep_fd = t->sleep_fd;
  scm_i_pthread_mutex_unlock (&async_mutex);

  if (sleep_mutex)
    {
      /* By now, the thread T might be out of its sleep already, or
	 might even be in the next, unrelated sleep.  Interrupting it
	 anyway does no harm, however.

	 The important thing to prevent here is to signal sleep_cond
	 before T waits on it.  This can not happen since T has
	 sleep_mutex locked while setting t->sleep_mutex and will only
	 unlock it again while waiting on sleep_cond.
      */
      scm_i_scm_pthread_mutex_lock (sleep_mutex);
      scm_i_pthread_cond_signal (&t->sleep_cond);
      scm_i_pthread_mutex_unlock (sleep_mutex);
    }

  if (sleep_fd >= 0)
    {
      size_t count;
      char dummy = 0;

      /* Likewise, T might already been done with sleeping here, but
	 interrupting it once too often does no harm.  T might also
	 not yet have started sleeping, but this is no problem either
	 since the data written to a pipe will not be lost, unlike a
	 condition variable signal.  */
      count = write (sleep_fd, &dummy, 1);
    }

  /* This is needed to protect sleep_mutex.
   */
  scm_remember_upto_here_1 (sleep_object);
}

int
scm_i_setup_sleep (scm_i_thread *t,
		   SCM sleep_object, scm_i_pthread_mutex_t *sleep_mutex,
		   int sleep_fd)
{
  int pending;

  scm_i_pthread_mutex_lock (&async_mutex);
  pending = t->pending_asyncs;
  if (!pending)
    {
      t->sleep_object = sleep_object;
      t->sleep_mutex = sleep_mutex;
      t->sleep_fd = sleep_fd;
    }
  scm_i_pthread_mutex_unlock (&async_mutex);
  return pending;
}

void
scm_i_reset_sleep (scm_i_thread *t)
{
  scm_i_pthread_mutex_lock (&async_mutex);
  t->sleep_object = SCM_BOOL_F;
  t->sleep_mutex = NULL;
  t->sleep_fd = -1;
  scm_i_pthread_mutex_unlock (&async_mutex);  
}

SCM_DEFINE (scm_system_async_mark_for_thread, "system-async-mark", 1, 1, 0,
           (SCM proc, SCM thread),
	    "Mark @var{proc} (a procedure with zero arguments) for future execution\n"
	    "in @var{thread}.  If @var{proc} has already been marked for\n"
	    "@var{thread} but has not been executed yet, this call has no effect.\n"
	    "If @var{thread} is omitted, the thread that called\n"
	    "@code{system-async-mark} is used.\n\n"
	    "This procedure is not safe to be called from C signal handlers.  Use\n"
	    "@code{scm_sigaction} or @code{scm_sigaction_for_thread} to install\n"
	    "signal handlers.")
#define FUNC_NAME s_scm_system_async_mark_for_thread
{
  /* The current thread might not have a handle yet.  This can happen
     when the GC runs immediately before allocating the handle.  At
     the end of that GC, a system async might be marked.  Thus, we can
     not use scm_current_thread here.
  */

  scm_i_thread *t;

  if (SCM_UNBNDP (thread))
    t = SCM_I_CURRENT_THREAD;
  else
    {
      SCM_VALIDATE_THREAD (2, thread);
      if (scm_c_thread_exited_p (thread))
	SCM_MISC_ERROR ("thread has already exited", SCM_EOL);
      t = SCM_I_THREAD_DATA (thread);
    }
  scm_i_queue_async_cell (scm_cons (proc, SCM_BOOL_F), t);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
scm_system_async_mark (SCM proc)
#define FUNC_NAME s_scm_system_async_mark_for_thread
{
  return scm_system_async_mark_for_thread (proc, SCM_UNDEFINED);
}
#undef FUNC_NAME




SCM_DEFINE (scm_noop, "noop", 0, 0, 1,
	    (SCM args),
	    "Do nothing.  When called without arguments, return @code{#f},\n"
	    "otherwise return the first argument.")
#define FUNC_NAME s_scm_noop
{
  SCM_VALIDATE_REST_ARGUMENT (args);
  return (SCM_NULL_OR_NIL_P (args) ? SCM_BOOL_F : SCM_CAR (args));
}
#undef FUNC_NAME




#if (SCM_ENABLE_DEPRECATED == 1)

SCM_DEFINE (scm_unmask_signals, "unmask-signals", 0, 0, 0,
	    (),
	    "Unmask signals. The returned value is not specified.")
#define FUNC_NAME s_scm_unmask_signals
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  scm_c_issue_deprecation_warning 
    ("'unmask-signals' is deprecated.  "
     "Use 'call-with-blocked-asyncs' instead.");

  if (t->block_asyncs == 0)
    SCM_MISC_ERROR ("signals already unmasked", SCM_EOL);
  t->block_asyncs = 0;
  scm_async_click ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_mask_signals, "mask-signals", 0, 0, 0,
	    (),
	    "Mask signals. The returned value is not specified.")
#define FUNC_NAME s_scm_mask_signals
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  scm_c_issue_deprecation_warning 
    ("'mask-signals' is deprecated.  Use 'call-with-blocked-asyncs' instead.");

  if (t->block_asyncs > 0)
    SCM_MISC_ERROR ("signals already masked", SCM_EOL);
  t->block_asyncs = 1;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#endif /* SCM_ENABLE_DEPRECATED == 1 */

static void
increase_block (void *data)
{
  ((scm_i_thread *)data)->block_asyncs++;
}

static void
decrease_block (void *data)
{
  if (--((scm_i_thread *)data)->block_asyncs == 0)
    scm_async_click ();
}

SCM_DEFINE (scm_call_with_blocked_asyncs, "call-with-blocked-asyncs", 1, 0, 0,
	    (SCM proc),
	    "Call @var{proc} with no arguments and block the execution\n"
	    "of system asyncs by one level for the current thread while\n"
	    "it is running.  Return the value returned by @var{proc}.\n")
#define FUNC_NAME s_scm_call_with_blocked_asyncs
{
  return scm_internal_dynamic_wind (increase_block,
				    (scm_t_inner) scm_call_0,
				    decrease_block,
				    (void *)proc,
				    SCM_I_CURRENT_THREAD);
}
#undef FUNC_NAME

void *
scm_c_call_with_blocked_asyncs (void *(*proc) (void *data), void *data)
{
  return (void *)scm_internal_dynamic_wind (increase_block,
					    (scm_t_inner) proc,
					    decrease_block,
					    data,
					    SCM_I_CURRENT_THREAD);
}


SCM_DEFINE (scm_call_with_unblocked_asyncs, "call-with-unblocked-asyncs", 1, 0, 0,
	    (SCM proc),
	    "Call @var{proc} with no arguments and unblock the execution\n"
	    "of system asyncs by one level for the current thread while\n"
	    "it is running.  Return the value returned by @var{proc}.\n")
#define FUNC_NAME s_scm_call_with_unblocked_asyncs
{
  if (SCM_I_CURRENT_THREAD->block_asyncs == 0)
    SCM_MISC_ERROR ("asyncs already unblocked", SCM_EOL);
  return scm_internal_dynamic_wind (decrease_block,
				    (scm_t_inner) scm_call_0,
				    increase_block,
				    (void *)proc,
				    SCM_I_CURRENT_THREAD);
}
#undef FUNC_NAME

void *
scm_c_call_with_unblocked_asyncs (void *(*proc) (void *data), void *data)
{
  if (SCM_I_CURRENT_THREAD->block_asyncs == 0)
    scm_misc_error ("scm_c_call_with_unblocked_asyncs",
		    "asyncs already unblocked", SCM_EOL);
  return (void *)scm_internal_dynamic_wind (decrease_block,
					    (scm_t_inner) proc,
					    increase_block,
					    data,
					    SCM_I_CURRENT_THREAD);
}

void
scm_dynwind_block_asyncs ()
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  scm_dynwind_rewind_handler (increase_block, t, SCM_F_WIND_EXPLICITLY);
  scm_dynwind_unwind_handler (decrease_block, t, SCM_F_WIND_EXPLICITLY);
}

void
scm_dynwind_unblock_asyncs ()
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  if (t->block_asyncs == 0)
    scm_misc_error ("scm_with_unblocked_asyncs", 
		    "asyncs already unblocked", SCM_EOL);
  scm_dynwind_rewind_handler (decrease_block, t, SCM_F_WIND_EXPLICITLY);
  scm_dynwind_unwind_handler (increase_block, t, SCM_F_WIND_EXPLICITLY);
}




void
scm_init_async ()
{
  scm_asyncs = SCM_EOL;
  tc16_async = scm_make_smob_type ("async", 0);
  scm_set_smob_mark (tc16_async, async_gc_mark);

#include "libguile/async.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
