/* classes: h_files */

#ifndef SCM_THREADS_H
#define SCM_THREADS_H

/* Copyright (C) 1996,1997,1998,2000,2001, 2002, 2003, 2004, 2006 Free Software Foundation, Inc.
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
#include "libguile/procs.h"
#include "libguile/throw.h"
#include "libguile/root.h"
#include "libguile/iselect.h"
#include "libguile/dynwind.h"
#include "libguile/continuations.h"

#if SCM_USE_PTHREAD_THREADS
#include "libguile/pthread-threads.h"
#endif

#if SCM_USE_NULL_THREADS
#include "libguile/null-threads.h"
#endif



/* smob tags for the thread datatypes */
SCM_API scm_t_bits scm_tc16_thread;
SCM_API scm_t_bits scm_tc16_mutex;
SCM_API scm_t_bits scm_tc16_condvar;

typedef struct scm_i_thread {
  struct scm_i_thread *next_thread;

  SCM handle;
  scm_i_pthread_t pthread;
  
  SCM join_queue;
  SCM result;
  int exited;

  SCM sleep_object;
  scm_i_pthread_mutex_t *sleep_mutex;
  scm_i_pthread_cond_t sleep_cond;
  int sleep_fd, sleep_pipe[2];

  /* This mutex represents this threads right to access the heap.
     That right can temporarily be taken away by the GC.  
  */
  scm_i_pthread_mutex_t heap_mutex;

  /* The freelists of this thread.  Each thread has its own lists so
     that they can all allocate concurrently.
  */
  SCM freelist, freelist2;
  int clear_freelists_p; /* set if GC was done while thread was asleep */
  int gc_running_p;      /* non-zero while this thread does GC or a
			    sweep. */

  /* Other thread local things.
   */
  SCM dynamic_state;
  scm_t_debug_frame *last_debug_frame;
  SCM dynwinds;

  /* For system asyncs.
   */
  SCM active_asyncs;            /* The thunks to be run at the next
                                   safe point */
  unsigned int block_asyncs;    /* Non-zero means that asyncs should 
                                   not be run. */
  unsigned int pending_asyncs;  /* Non-zero means that asyncs might be pending.
				 */

  /* The current continuation root and the stack base for it.

     The continuation root is an arbitrary but unique object that
     identifies a dynamic extent.  Continuations created during that
     extent can also only be invoked during it.

     We use pairs where the car is the thread handle and the cdr links
     to the previous pair.  This might be used for better error
     messages but is not essential for identifying continuation roots.

     The continuation base is the far end of the stack upto which it
     needs to be copied.
  */
  SCM continuation_root;
  SCM_STACKITEM *continuation_base;

  /* For keeping track of the stack and registers. */
  SCM_STACKITEM *base;
  SCM_STACKITEM *top;
  scm_i_jmp_buf regs;
#ifdef __ia64__
  void *register_backing_store_base;
  scm_t_contregs *pending_rbs_continuation;
#endif

  /* Whether this thread is in a critical section. */
  int critical_section_level;

} scm_i_thread;

#define SCM_I_IS_THREAD(x)    SCM_SMOB_PREDICATE (scm_tc16_thread, x)
#define SCM_I_THREAD_DATA(x)  ((scm_i_thread *) SCM_SMOB_DATA (x))

#define SCM_VALIDATE_THREAD(pos, a) \
  scm_assert_smob_type (scm_tc16_thread, (a))
#define SCM_VALIDATE_MUTEX(pos, a) \
  scm_assert_smob_type (scm_tc16_mutex, (a))
#define SCM_VALIDATE_CONDVAR(pos, a) \
  scm_assert_smob_type (scm_tc16_condvar, (a))

SCM_API SCM scm_spawn_thread (scm_t_catch_body body, void *body_data,
			      scm_t_catch_handler handler, void *handler_data);

SCM_API void *scm_without_guile (void *(*func)(void *), void *data);
SCM_API void *scm_with_guile (void *(*func)(void *), void *data);

SCM_API void *scm_i_with_guile_and_parent (void *(*func)(void *), void *data,
					   SCM parent);


extern int scm_i_thread_go_to_sleep;

void scm_i_thread_put_to_sleep (void);
void scm_i_thread_wake_up (void);
void scm_i_thread_invalidate_freelists (void);
void scm_i_thread_sleep_for_gc (void);

void scm_threads_prehistory (SCM_STACKITEM *);
void scm_threads_init_first_thread (void);
SCM_API void scm_threads_mark_stacks (void);
SCM_API void scm_init_threads (void);
SCM_API void scm_init_thread_procs (void);
SCM_API void scm_init_threads_default_dynamic_state (void);


#define SCM_THREAD_SWITCHING_CODE \
do { \
  if (scm_i_thread_go_to_sleep) \
    scm_i_thread_sleep_for_gc (); \
} while (0)

SCM_API SCM scm_call_with_new_thread (SCM thunk, SCM handler);
SCM_API SCM scm_yield (void);
SCM_API SCM scm_join_thread (SCM t);

SCM_API SCM scm_make_mutex (void);
SCM_API SCM scm_make_recursive_mutex (void);
SCM_API SCM scm_lock_mutex (SCM m);
SCM_API void scm_dynwind_lock_mutex (SCM mutex);
SCM_API SCM scm_try_mutex (SCM m);
SCM_API SCM scm_unlock_mutex (SCM m);

SCM_API SCM scm_make_condition_variable (void);
SCM_API SCM scm_wait_condition_variable (SCM cond, SCM mutex);
SCM_API SCM scm_timed_wait_condition_variable (SCM cond, SCM mutex,
					       SCM abstime);
SCM_API SCM scm_signal_condition_variable (SCM cond);
SCM_API SCM scm_broadcast_condition_variable (SCM cond);

SCM_API SCM scm_current_thread (void);
SCM_API SCM scm_all_threads (void);

SCM_API int scm_c_thread_exited_p (SCM thread);
SCM_API SCM scm_thread_exited_p (SCM thread);

SCM_API void scm_dynwind_critical_section (SCM mutex);

#define SCM_I_CURRENT_THREAD \
  ((scm_i_thread *) scm_i_pthread_getspecific (scm_i_thread_key))
SCM_API scm_i_pthread_key_t scm_i_thread_key;

#define scm_i_dynwinds()         (SCM_I_CURRENT_THREAD->dynwinds)
#define scm_i_set_dynwinds(w)    (SCM_I_CURRENT_THREAD->dynwinds = (w))
#define scm_i_last_debug_frame() (SCM_I_CURRENT_THREAD->last_debug_frame)
#define scm_i_set_last_debug_frame(f) \
                                 (SCM_I_CURRENT_THREAD->last_debug_frame = (f))

SCM_API scm_i_pthread_mutex_t scm_i_misc_mutex;

/* Convenience functions for working with the pthread API in guile
   mode.
*/

#if SCM_USE_PTHREAD_THREADS
SCM_API int scm_pthread_mutex_lock (pthread_mutex_t *mutex);
SCM_API void scm_dynwind_pthread_mutex_lock (pthread_mutex_t *mutex);
SCM_API int scm_pthread_cond_wait (pthread_cond_t *cond,
				   pthread_mutex_t *mutex);
SCM_API int scm_pthread_cond_timedwait (pthread_cond_t *cond,
					pthread_mutex_t *mutex,
					const scm_t_timespec *abstime);
#endif

/* More convenience functions.
 */

SCM_API unsigned int scm_std_sleep (unsigned int);
SCM_API unsigned long scm_std_usleep (unsigned long);

#endif  /* SCM_THREADS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
