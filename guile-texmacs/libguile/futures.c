/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2002, 2003, 2006, 2008 Free Software Foundation, Inc.
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




#if 0

/* This whole file is not being compiled.  See futures.h for the
   reason.
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/eval.h"
#include "libguile/ports.h"
#include "libguile/validate.h"
#include "libguile/stime.h"
#include "libguile/threads.h"

#include "libguile/futures.h"

#define LINK(list, obj)				\
do {						\
  SCM_SET_FUTURE_NEXT (obj, list);		\
  list = obj;					\
} while (0)

#define UNLINK(list, obj)			\
do {						\
  obj = list;					\
  list = SCM_FUTURE_NEXT (list);		\
} while (0)
     
scm_i_pthread_mutex_t future_admin_mutex = SCM_I_PTHREAD_MUTEX_INITIALIZER;

static SCM futures = SCM_EOL;
static SCM young = SCM_EOL;
static SCM old = SCM_EOL;
static SCM undead = SCM_EOL;

static long last_switch;

#ifdef SCM_FUTURES_DEBUG
static int n_dead = 0;

static SCM
count (SCM ls)
{
  int n = 0;
  while (!scm_is_null (ls))
    {
      ++n;
      ls = SCM_FUTURE_NEXT (ls);
    }
  return scm_from_int (n);
}

extern SCM scm_future_cache_status (void);

SCM_DEFINE (scm_future_cache_status, "future-cache-status", 0, 0, 0,
	    (),
	    "Return a list containing number of futures, youngs, olds, undeads and deads.")
#define FUNC_NAME s_scm_future_cache_status
{
  int nd = n_dead;
  n_dead = 0;
  return scm_list_5 (count (futures),
		     count (young),
		     count (old),
		     count (undead),
		     scm_from_int (nd));
}
#undef FUNC_NAME

#endif

SCM *scm_loc_sys_thread_handler;

SCM_DEFINE (scm_make_future, "make-future", 1, 0, 0,
	    (SCM thunk),
	    "Make a future evaluating THUNK.")
#define FUNC_NAME s_scm_make_future
{
  SCM_VALIDATE_THUNK (1, thunk);
  return scm_i_make_future (thunk);
}
#undef FUNC_NAME

static char *s_future = "future";

static void
cleanup (scm_t_future *future)
{
  scm_i_pthread_mutex_destroy (&future->mutex);
  scm_i_pthread_cond_destroy (&future->cond);
  scm_gc_free (future, sizeof (*future), s_future);
#ifdef SCM_FUTURES_DEBUG
  ++n_dead;
#endif
}

static SCM
future_loop (scm_t_future *future)
{
  scm_i_scm_pthread_mutex_lock (&future->mutex);
  do {
    if (future->status == SCM_FUTURE_SIGNAL_ME)
      scm_i_pthread_cond_broadcast (&future->cond);
    future->status = SCM_FUTURE_COMPUTING;
    future->data = (SCM_CLOSUREP (future->data)
		    ? scm_i_call_closure_0 (future->data)
		    : scm_call_0 (future->data));
    scm_i_scm_pthread_cond_wait (&future->cond, &future->mutex);
  } while (!future->die_p);
  future->status = SCM_FUTURE_DEAD;
  scm_i_pthread_mutex_unlock (&future->mutex);
  return SCM_UNSPECIFIED;
}

static SCM
future_handler (scm_t_future *future, SCM key, SCM args)
{
  future->status = SCM_FUTURE_DEAD;
  scm_i_pthread_mutex_unlock (&future->mutex);
  return scm_apply_1 (*scm_loc_sys_thread_handler, key, args);
}

static SCM
alloc_future (SCM thunk)
{
  scm_t_future *f = scm_gc_malloc (sizeof (*f), s_future);
  SCM future;
  f->data = SCM_BOOL_F;
  scm_i_pthread_mutex_init (&f->mutex, NULL);
  scm_i_pthread_cond_init (&f->cond, NULL);
  f->die_p = 0;
  f->status = SCM_FUTURE_TASK_ASSIGNED;
  scm_i_scm_pthread_mutex_lock (&future_admin_mutex);
  SCM_NEWSMOB2 (future, scm_tc16_future, futures, f);
  SCM_SET_FUTURE_DATA (future, thunk);
  futures = future;
  scm_i_pthread_mutex_unlock (&future_admin_mutex);
  scm_spawn_thread ((scm_t_catch_body) future_loop,
		    SCM_FUTURE (future),
		    (scm_t_catch_handler) future_handler,
		    SCM_FUTURE (future));
  return future;
}

static void
kill_future (SCM future)
{
  SCM_FUTURE (future)->die_p = 1;
  LINK (undead, future);
}

SCM
scm_i_make_future (SCM thunk)
{
  SCM future;
  scm_i_scm_pthread_mutex_lock (&future_admin_mutex);
  while (1)
    {
      if (!scm_is_null (old))
	UNLINK (old, future);
      else if (!scm_is_null (young))
	UNLINK (young, future);
      else
	{
	  scm_i_pthread_mutex_unlock (&future_admin_mutex);
	  return alloc_future (thunk);
	}
      if (scm_i_pthread_mutex_trylock (SCM_FUTURE_MUTEX (future)))
	kill_future (future);
      else if (!SCM_FUTURE_ALIVE_P (future))
	{
	  scm_i_pthread_mutex_unlock (SCM_FUTURE_MUTEX (future));
	  cleanup (SCM_FUTURE (future));
	}
      else
	break;
    }
  LINK (futures, future);
  scm_i_pthread_mutex_unlock (&future_admin_mutex);
  SCM_SET_FUTURE_DATA (future, thunk);
  SCM_SET_FUTURE_STATUS (future, SCM_FUTURE_TASK_ASSIGNED);
  scm_i_pthread_cond_signal (SCM_FUTURE_COND (future));
  scm_i_pthread_mutex_unlock (SCM_FUTURE_MUTEX (future));
  return future;
}

static SCM
future_mark (SCM ptr) {
  return SCM_FUTURE_DATA (ptr);
}

static int 
future_print (SCM exp, SCM port, scm_print_state *pstate)
{
  int writingp = SCM_WRITINGP (pstate);
  scm_puts ("#<future ", port);
  SCM_SET_WRITINGP (pstate, 1);
  scm_iprin1 (SCM_FUTURE_DATA (exp), port, pstate);
  SCM_SET_WRITINGP (pstate, writingp);
  scm_putc ('>', port);
  return !0;
}

SCM_DEFINE (scm_future_ref, "future-ref", 1, 0, 0,
	    (SCM future),
	    "If the future @var{x} has not been computed yet, compute and\n"
	    "return @var{x}, otherwise just return the previously computed\n"
	    "value.")
#define FUNC_NAME s_scm_future_ref
{
  SCM res;
  SCM_VALIDATE_FUTURE (1, future);
  scm_i_scm_pthread_mutex_lock (SCM_FUTURE_MUTEX (future));
  if (SCM_FUTURE_STATUS (future) != SCM_FUTURE_COMPUTING)
    {
      SCM_SET_FUTURE_STATUS (future, SCM_FUTURE_SIGNAL_ME);
      scm_i_scm_pthread_cond_wait (SCM_FUTURE_COND (future),
			     SCM_FUTURE_MUTEX (future));
    }
  if (!SCM_FUTURE_ALIVE_P (future))
    {
      scm_i_pthread_mutex_unlock (SCM_FUTURE_MUTEX (future));
      SCM_MISC_ERROR ("requesting result from failed future ~A",
		      scm_list_1 (future));
    }
  res = SCM_FUTURE_DATA (future);
  scm_i_pthread_mutex_unlock (SCM_FUTURE_MUTEX (future));
  return res;
}
#undef FUNC_NAME

static void
kill_futures (SCM victims)
{
  while (!scm_is_null (victims))
    {
      SCM future;
      UNLINK (victims, future);
      kill_future (future);
      scm_i_pthread_cond_signal (SCM_FUTURE_COND (future));
    }
}

static void
cleanup_undead ()
{
  SCM next = undead, *nextloc = &undead;
  while (!scm_is_null (next))
    {
      if (scm_i_pthread_mutex_trylock (SCM_FUTURE_MUTEX (next)))
	goto next;
      else if (SCM_FUTURE_ALIVE_P (next))
	{
	  scm_i_pthread_cond_signal (SCM_FUTURE_COND (next));
	  scm_i_pthread_mutex_unlock (SCM_FUTURE_MUTEX (next));
	next:
	  SCM_SET_GC_MARK (next);
	  nextloc = SCM_FUTURE_NEXTLOC (next);
	  next = *nextloc;
	}
      else
	{
	  SCM future;
	  UNLINK (next, future);
	  scm_i_pthread_mutex_unlock (SCM_FUTURE_MUTEX (future));
	  cleanup (SCM_FUTURE (future));
	  *nextloc = next;
	}
    }
}

static void
mark_futures (SCM futures)
{
  while (!scm_is_null (futures))
    {
      SCM_SET_GC_MARK (futures);
      futures = SCM_FUTURE_NEXT (futures);
    }
}

static void *
scan_futures (void *dummy1, void *dummy2, void *dummy3)
{
  SCM next, *nextloc;
  
  long now = scm_c_get_internal_run_time ();
  if (now - last_switch > SCM_TIME_UNITS_PER_SECOND)
    {
      /* switch out old (> 1 sec), unused futures */
      kill_futures (old);
      old = young;
      young = SCM_EOL;
      last_switch = now;
    }
  else
    mark_futures (young);    

  next = futures;
  nextloc = &futures;
  while (!scm_is_null (next))
    {
      if (!SCM_GC_MARK_P (next))
	goto free;
    keep:
      nextloc = SCM_FUTURE_NEXTLOC (next);
      next = *nextloc;
    }
  goto exit;
  while (!scm_is_null (next))
    {
      if (SCM_GC_MARK_P (next))
	{
	  *nextloc = next;
	  goto keep;
	}
    free:
      {
	SCM future;
	UNLINK (next, future);
	SCM_SET_GC_MARK (future);
	LINK (young, future);
      }
    }
  *nextloc = SCM_EOL;
 exit:
  cleanup_undead ();
  mark_futures (old);
  return 0;
}

scm_t_bits scm_tc16_future;

void
scm_init_futures ()
{
  last_switch = scm_c_get_internal_run_time ();
  
  scm_loc_sys_thread_handler
    = SCM_VARIABLE_LOC (scm_c_define ("%thread-handler", SCM_BOOL_F));

  scm_tc16_future = scm_make_smob_type ("future", 0);
  scm_set_smob_mark (scm_tc16_future, future_mark);
  scm_set_smob_print (scm_tc16_future, future_print);

  scm_c_hook_add (&scm_before_sweep_c_hook, scan_futures, 0, 0);
#include "libguile/futures.x"
}

#endif

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
