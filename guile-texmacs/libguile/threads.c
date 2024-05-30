/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2002, 2003, 2004, 2005, 2006, 2008 Free Software Foundation, Inc.
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

#include "libguile/_scm.h"

#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdio.h>
#include <assert.h>

#ifdef HAVE_STRING_H
#include <string.h>   /* for memset used by FD_ZERO on Solaris 10 */
#endif

#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "libguile/validate.h"
#include "libguile/root.h"
#include "libguile/eval.h"
#include "libguile/async.h"
#include "libguile/ports.h"
#include "libguile/threads.h"
#include "libguile/dynwind.h"
#include "libguile/iselect.h"
#include "libguile/fluids.h"
#include "libguile/continuations.h"
#include "libguile/gc.h"
#include "libguile/init.h"

#ifdef __MINGW32__
#ifndef ETIMEDOUT
# define ETIMEDOUT       WSAETIMEDOUT
#endif
# include <fcntl.h>
# include <process.h>
# define pipe(fd) _pipe (fd, 256, O_BINARY)
#endif /* __MINGW32__ */

/*** Queues */

/* Make an empty queue data structure.
 */
static SCM
make_queue ()
{
  return scm_cons (SCM_EOL, SCM_EOL);
}

/* Put T at the back of Q and return a handle that can be used with
   remqueue to remove T from Q again.
 */
static SCM
enqueue (SCM q, SCM t)
{
  SCM c = scm_cons (t, SCM_EOL);
  if (scm_is_null (SCM_CDR (q)))
    SCM_SETCDR (q, c);
  else
    SCM_SETCDR (SCM_CAR (q), c);
  SCM_SETCAR (q, c);
  return c;
}

/* Remove the element that the handle C refers to from the queue Q.  C
   must have been returned from a call to enqueue.  The return value
   is zero when the element referred to by C has already been removed.
   Otherwise, 1 is returned.
*/
static int
remqueue (SCM q, SCM c)
{
  SCM p, prev = q;
  for (p = SCM_CDR (q); !scm_is_null (p); p = SCM_CDR (p))
    {
      if (scm_is_eq (p, c))
	{
	  if (scm_is_eq (c, SCM_CAR (q)))
	    SCM_SETCAR (q, SCM_CDR (c));
	  SCM_SETCDR (prev, SCM_CDR (c));
	  return 1;
	}
      prev = p;
    }
  return 0;
}

/* Remove the front-most element from the queue Q and return it.
   Return SCM_BOOL_F when Q is empty.
*/
static SCM
dequeue (SCM q)
{
  SCM c = SCM_CDR (q);
  if (scm_is_null (c))
    return SCM_BOOL_F;
  else
    {
      SCM_SETCDR (q, SCM_CDR (c));
      if (scm_is_null (SCM_CDR (q)))
	SCM_SETCAR (q, SCM_EOL);
      return SCM_CAR (c);
    }
}

/*** Thread smob routines */

static SCM
thread_mark (SCM obj)
{
  scm_i_thread *t = SCM_I_THREAD_DATA (obj);
  scm_gc_mark (t->result);
  scm_gc_mark (t->join_queue);
  scm_gc_mark (t->dynwinds);
  scm_gc_mark (t->active_asyncs);
  scm_gc_mark (t->continuation_root);
  return t->dynamic_state;
}

static int
thread_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  /* On a Gnu system pthread_t is an unsigned long, but on mingw it's a
     struct.  A cast like "(unsigned long) t->pthread" is a syntax error in
     the struct case, hence we go via a union, and extract according to the
     size of pthread_t.  */
  union {
    scm_i_pthread_t p;
    unsigned short us;
    unsigned int   ui;
    unsigned long  ul;
    scm_t_uintmax  um;
  } u;
  scm_i_thread *t = SCM_I_THREAD_DATA (exp);
  scm_i_pthread_t p = t->pthread;
  scm_t_uintmax id;
  u.p = p;
  if (sizeof (p) == sizeof (unsigned short))
    id = u.us;
  else if (sizeof (p) == sizeof (unsigned int))
    id = u.ui;
  else if (sizeof (p) == sizeof (unsigned long))
    id = u.ul;
  else
    id = u.um;

  scm_puts ("#<thread ", port);
  scm_uintprint (id, 10, port);
  scm_puts (" (", port);
  scm_uintprint ((scm_t_bits)t, 16, port);
  scm_puts (")>", port);
  return 1;
}

static size_t
thread_free (SCM obj)
{
  scm_i_thread *t = SCM_I_THREAD_DATA (obj);
  assert (t->exited);
  scm_gc_free (t, sizeof (*t), "thread");
  return 0;
}

/*** Blocking on queues. */

/* See also scm_i_queue_async_cell for how such a block is
   interrputed.
*/

/* Put the current thread on QUEUE and go to sleep, waiting for it to
   be woken up by a call to 'unblock_from_queue', or to be
   interrupted.  Upon return of this function, the current thread is
   no longer on QUEUE, even when the sleep has been interrupted.

   The QUEUE data structure is assumed to be protected by MUTEX and
   the caller of block_self must hold MUTEX.  It will be atomically
   unlocked while sleeping, just as with scm_i_pthread_cond_wait.

   SLEEP_OBJECT is an arbitrary SCM value that is kept alive as long
   as MUTEX is needed.

   When WAITTIME is not NULL, the sleep will be aborted at that time.

   The return value of block_self is an errno value.  It will be zero
   when the sleep has been successfully completed by a call to
   unblock_from_queue, EINTR when it has been interrupted by the
   delivery of a system async, and ETIMEDOUT when the timeout has
   expired.

   The system asyncs themselves are not executed by block_self.
*/
static int
block_self (SCM queue, SCM sleep_object, scm_i_pthread_mutex_t *mutex,
	    const scm_t_timespec *waittime)
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  SCM q_handle;
  int err;

  if (scm_i_setup_sleep (t, sleep_object, mutex, -1))
    err = EINTR;
  else
    {
      t->block_asyncs++;
      q_handle = enqueue (queue, t->handle);
      if (waittime == NULL)
	err = scm_i_scm_pthread_cond_wait (&t->sleep_cond, mutex);
      else
	err = scm_i_scm_pthread_cond_timedwait (&t->sleep_cond, mutex, waittime);

      /* When we are still on QUEUE, we have been interrupted.  We
	 report this only when no other error (such as a timeout) has
	 happened above.
      */
      if (remqueue (queue, q_handle) && err == 0)
	err = EINTR;
      t->block_asyncs--;
      scm_i_reset_sleep (t);
    }

  return err;
}

/* Wake up the first thread on QUEUE, if any.  The caller must hold
   the mutex that protects QUEUE.  The awoken thread is returned, or
   #f when the queue was empty.
 */
static SCM
unblock_from_queue (SCM queue)
{
  SCM thread = dequeue (queue);
  if (scm_is_true (thread))
    scm_i_pthread_cond_signal (&SCM_I_THREAD_DATA(thread)->sleep_cond);
  return thread;
}

/* Getting into and out of guile mode.
 */

/* Ken Raeburn observes that the implementation of suspend and resume
   (and the things that build on top of them) are very likely not
   correct (see below).  We will need fix this eventually, and that's
   why scm_leave_guile/scm_enter_guile are not exported in the API.

   Ken writes:

   Consider this sequence:

   Function foo, called in Guile mode, calls suspend (maybe indirectly
   through scm_leave_guile), which does this:

      // record top of stack for the GC
      t->top = SCM_STACK_PTR (&t);     // just takes address of automatic
      var 't'
      // save registers.
      SCM_FLUSH_REGISTER_WINDOWS;      // sparc only
      SCM_I_SETJMP (t->regs);          // here's most of the magic

   ... and returns.

   Function foo has a SCM value X, a handle on a non-immediate object, in
   a caller-saved register R, and it's the only reference to the object
   currently.

   The compiler wants to use R in suspend, so it pushes the current
   value, X, into a stack slot which will be reloaded on exit from
   suspend; then it loads stuff into R and goes about its business.  The
   setjmp call saves (some of) the current registers, including R, which
   no longer contains X.  (This isn't a problem for a normal
   setjmp/longjmp situation, where longjmp would be called before
   setjmp's caller returns; the old value for X would be loaded back from
   the stack after the longjmp, before the function returned.)

   So, suspend returns, loading X back into R (and invalidating the jump
   buffer) in the process.  The caller foo then goes off and calls a
   bunch of other functions out of Guile mode, occasionally storing X on
   the stack again, but, say, much deeper on the stack than suspend's
   stack frame went, and the stack slot where suspend had written X has
   long since been overwritten with other values.

   Okay, nothing actively broken so far.  Now, let garbage collection
   run, triggered by another thread.

   The thread calling foo is out of Guile mode at the time, so the
   garbage collector just scans a range of stack addresses.  Too bad that
   X isn't stored there.  So the pointed-to storage goes onto the free
   list, and I think you can see where things go from there.

   Is there anything I'm missing that'll prevent this scenario from
   happening?  I mean, aside from, "well, suspend and scm_leave_guile
   don't have many local variables, so they probably won't need to save
   any registers on most systems, so we hope everything will wind up in
   the jump buffer and we'll just get away with it"?

   (And, going the other direction, if scm_leave_guile and suspend push
   the stack pointer over onto a new page, and foo doesn't make further
   function calls and thus the stack pointer no longer includes that
   page, are we guaranteed that the kernel cannot release the now-unused
   stack page that contains the top-of-stack pointer we just saved?  I
   don't know if any OS actually does that.  If it does, we could get
   faults in garbage collection.)

   I don't think scm_without_guile has to have this problem, as it gets
   more control over the stack handling -- but it should call setjmp
   itself.  I'd probably try something like:

      // record top of stack for the GC
      t->top = SCM_STACK_PTR (&t);
      // save registers.
      SCM_FLUSH_REGISTER_WINDOWS;
      SCM_I_SETJMP (t->regs);
      res = func(data);
      scm_enter_guile (t);

   ... though even that's making some assumptions about the stack
   ordering of local variables versus caller-saved registers.

   For something like scm_leave_guile to work, I don't think it can just
   rely on invalidated jump buffers.  A valid jump buffer, and a handle
   on the stack state at the point when the jump buffer was initialized,
   together, would work fine, but I think then we're talking about macros
   invoking setjmp in the caller's stack frame, and requiring that the
   caller of scm_leave_guile also call scm_enter_guile before returning,
   kind of like pthread_cleanup_push/pop calls that have to be paired up
   in a function.  (In fact, the pthread ones have to be paired up
   syntactically, as if they might expand to a compound statement
   incorporating the user's code, and invoking a compiler's
   exception-handling primitives.  Which might be something to think
   about for cases where Guile is used with C++ exceptions or
   pthread_cancel.)
*/

scm_i_pthread_key_t scm_i_thread_key;

static void
resume (scm_i_thread *t)
{
  t->top = NULL;
  if (t->clear_freelists_p)
    {
      *SCM_FREELIST_LOC (scm_i_freelist) = SCM_EOL;
      *SCM_FREELIST_LOC (scm_i_freelist2) = SCM_EOL;
      t->clear_freelists_p = 0;
    }
}

typedef void* scm_t_guile_ticket;

static void
scm_enter_guile (scm_t_guile_ticket ticket)
{
  scm_i_thread *t = (scm_i_thread *)ticket;
  if (t)
    {
      scm_i_pthread_mutex_lock (&t->heap_mutex);
      resume (t);
    }
}

static scm_i_thread *
suspend (void)
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  /* record top of stack for the GC */
  t->top = SCM_STACK_PTR (&t);
  /* save registers. */
  SCM_FLUSH_REGISTER_WINDOWS;
  SCM_I_SETJMP (t->regs);
  return t;
}

static scm_t_guile_ticket
scm_leave_guile ()
{
  scm_i_thread *t = suspend ();
  scm_i_pthread_mutex_unlock (&t->heap_mutex);
  return (scm_t_guile_ticket) t;
}

static scm_i_pthread_mutex_t thread_admin_mutex = SCM_I_PTHREAD_MUTEX_INITIALIZER;
static scm_i_thread *all_threads = NULL;
static int thread_count;

static SCM scm_i_default_dynamic_state;

/* Perform first stage of thread initialisation, in non-guile mode.
 */
static void
guilify_self_1 (SCM_STACKITEM *base)
{
  scm_i_thread *t = malloc (sizeof (scm_i_thread));

  t->pthread = scm_i_pthread_self ();
  t->handle = SCM_BOOL_F;
  t->result = SCM_BOOL_F;
  t->join_queue = SCM_EOL;
  t->dynamic_state = SCM_BOOL_F;
  t->dynwinds = SCM_EOL;
  t->active_asyncs = SCM_EOL;
  t->block_asyncs = 1;
  t->pending_asyncs = 1;
  t->critical_section_level = 0;
  t->last_debug_frame = NULL;
  t->base = base;
#ifdef __ia64__
  /* Calculate and store off the base of this thread's register
     backing store (RBS).  Unfortunately our implementation(s) of
     scm_ia64_register_backing_store_base are only reliable for the
     main thread.  For other threads, therefore, find out the current
     top of the RBS, and use that as a maximum. */
  t->register_backing_store_base = scm_ia64_register_backing_store_base ();
  {
    ucontext_t ctx;
    void *bsp;
    getcontext (&ctx);
    bsp = scm_ia64_ar_bsp (&ctx);
    if (t->register_backing_store_base > bsp)
      t->register_backing_store_base = bsp;
  }
#endif
  t->continuation_root = SCM_EOL;
  t->continuation_base = base;
  scm_i_pthread_cond_init (&t->sleep_cond, NULL);
  t->sleep_mutex = NULL;
  t->sleep_object = SCM_BOOL_F;
  t->sleep_fd = -1;

  if (pipe (t->sleep_pipe) != 0)
    /* FIXME: Error conditions during the initialization phase are handled
       gracelessly since public functions such as `scm_init_guile ()'
       currently have type `void'.  */
    abort ();

  scm_i_pthread_mutex_init (&t->heap_mutex, NULL);
  t->clear_freelists_p = 0;
  t->gc_running_p = 0;
  t->exited = 0;

  t->freelist = SCM_EOL;
  t->freelist2 = SCM_EOL;
  SCM_SET_FREELIST_LOC (scm_i_freelist, &t->freelist);
  SCM_SET_FREELIST_LOC (scm_i_freelist2, &t->freelist2);

  scm_i_pthread_setspecific (scm_i_thread_key, t);

  /* As soon as this thread adds itself to the global thread list, the
     GC may think that it has a stack that needs marking.  Therefore
     initialize t->top to be the same as t->base, just in case GC runs
     before the thread can lock its heap_mutex for the first time. */
  t->top = t->base;
  scm_i_pthread_mutex_lock (&thread_admin_mutex);
  t->next_thread = all_threads;
  all_threads = t;
  thread_count++;
  scm_i_pthread_mutex_unlock (&thread_admin_mutex);

  /* Enter Guile mode. */
  scm_enter_guile (t);
}

/* Perform second stage of thread initialisation, in guile mode.
 */
static void
guilify_self_2 (SCM parent)
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  SCM_NEWSMOB (t->handle, scm_tc16_thread, t);
  scm_gc_register_collectable_memory (t, sizeof (scm_i_thread), "thread");
  t->continuation_root = scm_cons (t->handle, SCM_EOL);
  t->continuation_base = t->base;

  if (scm_is_true (parent))
    t->dynamic_state = scm_make_dynamic_state (parent);
  else
    t->dynamic_state = scm_i_make_initial_dynamic_state ();

  t->join_queue = make_queue ();
  t->block_asyncs = 0;
}

/* Perform thread tear-down, in guile mode.
 */
static void *
do_thread_exit (void *v)
{
  scm_i_thread *t = (scm_i_thread *)v;

  scm_i_scm_pthread_mutex_lock (&thread_admin_mutex);

  t->exited = 1;
  close (t->sleep_pipe[0]);
  close (t->sleep_pipe[1]);
  while (scm_is_true (unblock_from_queue (t->join_queue)))
    ;

  scm_i_pthread_mutex_unlock (&thread_admin_mutex);
  return NULL;
}

static void
on_thread_exit (void *v)
{
  /* This handler is executed in non-guile mode.  */
  scm_i_thread *t = (scm_i_thread *)v, **tp;

  scm_i_pthread_setspecific (scm_i_thread_key, v);

  /* Unblocking the joining threads needs to happen in guile mode
     since the queue is a SCM data structure.  */
  scm_with_guile (do_thread_exit, v);

  /* Removing ourself from the list of all threads needs to happen in
     non-guile mode since all SCM values on our stack become
     unprotected once we are no longer in the list.  */
  scm_i_pthread_mutex_lock (&thread_admin_mutex);
  for (tp = &all_threads; *tp; tp = &(*tp)->next_thread)
    if (*tp == t)
      {
	*tp = t->next_thread;
	break;
      }
  thread_count--;
  scm_i_pthread_mutex_unlock (&thread_admin_mutex);

  scm_i_pthread_setspecific (scm_i_thread_key, NULL);
}

static scm_i_pthread_once_t init_thread_key_once = SCM_I_PTHREAD_ONCE_INIT;

static void
init_thread_key (void)
{
  scm_i_pthread_key_create (&scm_i_thread_key, on_thread_exit);
}

/* Perform any initializations necessary to bring the current thread
   into guile mode, initializing Guile itself, if necessary.

   BASE is the stack base to use with GC.

   PARENT is the dynamic state to use as the parent, ot SCM_BOOL_F in
   which case the default dynamic state is used.

   Return zero when the thread was in guile mode already; otherwise
   return 1.
*/

static int
scm_i_init_thread_for_guile (SCM_STACKITEM *base, SCM parent)
{
  scm_i_thread *t;

  scm_i_pthread_once (&init_thread_key_once, init_thread_key);

  if ((t = SCM_I_CURRENT_THREAD) == NULL)
    {
      /* This thread has not been guilified yet.
       */

      scm_i_pthread_mutex_lock (&scm_i_init_mutex);
      if (scm_initialized_p == 0)
	{
	  /* First thread ever to enter Guile.  Run the full
	     initialization.
	  */
	  scm_i_init_guile (base);
	  scm_i_pthread_mutex_unlock (&scm_i_init_mutex);
	}
      else
	{
	  /* Guile is already initialized, but this thread enters it for
	     the first time.  Only initialize this thread.
	  */
	  scm_i_pthread_mutex_unlock (&scm_i_init_mutex);
	  guilify_self_1 (base);
	  guilify_self_2 (parent);
	}
      return 1;
    }
  else if (t->top)
    {
      /* This thread is already guilified but not in guile mode, just
	 resume it.

         A user call to scm_with_guile() will lead us to here.  This could
         happen from anywhere on the stack, and in particular lower on the
         stack than when it was when this thread was first guilified.  Thus,
         `base' must be updated.  */
#if SCM_STACK_GROWS_UP
      if (base < t->base)
         t->base = base;
#else
      if (base > t->base)
         t->base = base;
#endif

      scm_enter_guile ((scm_t_guile_ticket) t);
      return 1;
    }
  else
    {
      /* Thread is already in guile mode.  Nothing to do.
      */
      return 0;
    }
}

#if SCM_USE_PTHREAD_THREADS

#if HAVE_PTHREAD_ATTR_GETSTACK && HAVE_PTHREAD_GETATTR_NP
/* This method for GNU/Linux and perhaps some other systems.
   It's not for MacOS X or Solaris 10, since pthread_getattr_np is not
   available on them.  */
#define HAVE_GET_THREAD_STACK_BASE

static SCM_STACKITEM *
get_thread_stack_base ()
{
  pthread_attr_t attr;
  void *start, *end;
  size_t size;

  pthread_getattr_np (pthread_self (), &attr);
  pthread_attr_getstack (&attr, &start, &size);
  end = (char *)start + size;

  /* XXX - pthread_getattr_np from LinuxThreads does not seem to work
     for the main thread, but we can use scm_get_stack_base in that
     case.
  */

#ifndef PTHREAD_ATTR_GETSTACK_WORKS
  if ((void *)&attr < start || (void *)&attr >= end)
    return scm_get_stack_base ();
  else
#endif
    {
#if SCM_STACK_GROWS_UP
      return start;
#else
      return end;
#endif
    }
}

#elif HAVE_PTHREAD_GET_STACKADDR_NP
/* This method for MacOS X.
   It'd be nice if there was some documentation on pthread_get_stackaddr_np,
   but as of 2006 there's nothing obvious at apple.com.  */
#define HAVE_GET_THREAD_STACK_BASE
static SCM_STACKITEM *
get_thread_stack_base ()
{
  return pthread_get_stackaddr_np (pthread_self ());
}

#elif defined (__MINGW32__)
/* This method for mingw.  In mingw the basic scm_get_stack_base can be used
   in any thread.  We don't like hard-coding the name of a system, but there
   doesn't seem to be a cleaner way of knowing scm_get_stack_base can
   work.  */
#define HAVE_GET_THREAD_STACK_BASE
static SCM_STACKITEM *
get_thread_stack_base ()
{
  return scm_get_stack_base ();
}

#endif /* pthread methods of get_thread_stack_base */

#else /* !SCM_USE_PTHREAD_THREADS */

#define HAVE_GET_THREAD_STACK_BASE

static SCM_STACKITEM *
get_thread_stack_base ()
{
  return scm_get_stack_base ();
}

#endif /* !SCM_USE_PTHREAD_THREADS */

#ifdef HAVE_GET_THREAD_STACK_BASE

void
scm_init_guile ()
{
  scm_i_init_thread_for_guile (get_thread_stack_base (),
			       scm_i_default_dynamic_state);
}

#endif

void *
scm_with_guile (void *(*func)(void *), void *data)
{
  return scm_i_with_guile_and_parent (func, data,
				      scm_i_default_dynamic_state);
}

void *
scm_i_with_guile_and_parent (void *(*func)(void *), void *data,
			     SCM parent)
{
  void *res;
  int really_entered;
  SCM_STACKITEM base_item;
  really_entered = scm_i_init_thread_for_guile (&base_item, parent);
  res = scm_c_with_continuation_barrier (func, data);
  if (really_entered)
    scm_leave_guile ();
  return res;
}

void *
scm_without_guile (void *(*func)(void *), void *data)
{
  void *res;
  scm_t_guile_ticket t;
  t = scm_leave_guile ();
  res = func (data);
  scm_enter_guile (t);
  return res;
}

/*** Thread creation */

typedef struct {
  SCM parent;
  SCM thunk;
  SCM handler;
  SCM thread;
  scm_i_pthread_mutex_t mutex;
  scm_i_pthread_cond_t cond;
} launch_data;

static void *
really_launch (void *d)
{
  launch_data *data = (launch_data *)d;
  SCM thunk = data->thunk, handler = data->handler;
  scm_i_thread *t;

  t = SCM_I_CURRENT_THREAD;

  scm_i_scm_pthread_mutex_lock (&data->mutex);
  data->thread = scm_current_thread ();
  scm_i_pthread_cond_signal (&data->cond);
  scm_i_pthread_mutex_unlock (&data->mutex);

  if (SCM_UNBNDP (handler))
    t->result = scm_call_0 (thunk);
  else
    t->result = scm_catch (SCM_BOOL_T, thunk, handler);

  return 0;
}

static void *
launch_thread (void *d)
{
  launch_data *data = (launch_data *)d;
  scm_i_pthread_detach (scm_i_pthread_self ());
  scm_i_with_guile_and_parent (really_launch, d, data->parent);
  return NULL;
}

SCM_DEFINE (scm_call_with_new_thread, "call-with-new-thread", 1, 1, 0,
	    (SCM thunk, SCM handler),
	    "Call @code{thunk} in a new thread and with a new dynamic state,\n"
	    "returning a new thread object representing the thread.  The procedure\n"
	    "@var{thunk} is called via @code{with-continuation-barrier}.\n"
	    "\n"
	    "When @var{handler} is specified, then @var{thunk} is called from\n"
	    "within a @code{catch} with tag @code{#t} that has @var{handler} as its\n"
	    "handler.  This catch is established inside the continuation barrier.\n"
	    "\n"
	    "Once @var{thunk} or @var{handler} returns, the return value is made\n"
	    "the @emph{exit value} of the thread and the thread is terminated.")
#define FUNC_NAME s_scm_call_with_new_thread
{
  launch_data data;
  scm_i_pthread_t id;
  int err;

  SCM_ASSERT (scm_is_true (scm_thunk_p (thunk)), thunk, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (SCM_UNBNDP (handler) || scm_is_true (scm_procedure_p (handler)),
	      handler, SCM_ARG2, FUNC_NAME);

  data.parent = scm_current_dynamic_state ();
  data.thunk = thunk;
  data.handler = handler;
  data.thread = SCM_BOOL_F;
  scm_i_pthread_mutex_init (&data.mutex, NULL);
  scm_i_pthread_cond_init (&data.cond, NULL);

  scm_i_scm_pthread_mutex_lock (&data.mutex);
  err = scm_i_pthread_create (&id, NULL, launch_thread, &data);
  if (err)
    {
      scm_i_pthread_mutex_unlock (&data.mutex);
      errno = err;
      scm_syserror (NULL);
    }
  scm_i_scm_pthread_cond_wait (&data.cond, &data.mutex);
  scm_i_pthread_mutex_unlock (&data.mutex);
  
  return data.thread;
}
#undef FUNC_NAME

typedef struct {
  SCM parent;
  scm_t_catch_body body;
  void *body_data;
  scm_t_catch_handler handler;
  void *handler_data;
  SCM thread;
  scm_i_pthread_mutex_t mutex;
  scm_i_pthread_cond_t cond;
} spawn_data;

static void *
really_spawn (void *d)
{
  spawn_data *data = (spawn_data *)d;
  scm_t_catch_body body = data->body;
  void *body_data = data->body_data;
  scm_t_catch_handler handler = data->handler;
  void *handler_data = data->handler_data;
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  scm_i_scm_pthread_mutex_lock (&data->mutex);
  data->thread = scm_current_thread ();
  scm_i_pthread_cond_signal (&data->cond);
  scm_i_pthread_mutex_unlock (&data->mutex);

  if (handler == NULL)
    t->result = body (body_data);
  else
    t->result = scm_internal_catch (SCM_BOOL_T,
				    body, body_data,
				    handler, handler_data);

  return 0;
}

static void *
spawn_thread (void *d)
{
  spawn_data *data = (spawn_data *)d;
  scm_i_pthread_detach (scm_i_pthread_self ());
  scm_i_with_guile_and_parent (really_spawn, d, data->parent);
  return NULL;
}

SCM
scm_spawn_thread (scm_t_catch_body body, void *body_data,
		  scm_t_catch_handler handler, void *handler_data)
{
  spawn_data data;
  scm_i_pthread_t id;
  int err;

  data.parent = scm_current_dynamic_state ();
  data.body = body;
  data.body_data = body_data;
  data.handler = handler;
  data.handler_data = handler_data;
  data.thread = SCM_BOOL_F;
  scm_i_pthread_mutex_init (&data.mutex, NULL);
  scm_i_pthread_cond_init (&data.cond, NULL);

  scm_i_scm_pthread_mutex_lock (&data.mutex);
  err = scm_i_pthread_create (&id, NULL, spawn_thread, &data);
  if (err)
    {
      scm_i_pthread_mutex_unlock (&data.mutex);
      errno = err;
      scm_syserror (NULL);
    }
  scm_i_scm_pthread_cond_wait (&data.cond, &data.mutex);
  scm_i_pthread_mutex_unlock (&data.mutex);
  
  return data.thread;
}

SCM_DEFINE (scm_yield, "yield", 0, 0, 0,
	    (),
"Move the calling thread to the end of the scheduling queue.")
#define FUNC_NAME s_scm_yield
{
  return scm_from_bool (scm_i_sched_yield ());
}
#undef FUNC_NAME

SCM_DEFINE (scm_join_thread, "join-thread", 1, 0, 0,
	    (SCM thread),
"Suspend execution of the calling thread until the target @var{thread} "
"terminates, unless the target @var{thread} has already terminated. ")
#define FUNC_NAME s_scm_join_thread
{
  scm_i_thread *t;
  SCM res;

  SCM_VALIDATE_THREAD (1, thread);
  if (scm_is_eq (scm_current_thread (), thread))
    SCM_MISC_ERROR ("can not join the current thread", SCM_EOL);

  scm_i_scm_pthread_mutex_lock (&thread_admin_mutex);

  t = SCM_I_THREAD_DATA (thread);
  while (!t->exited)
    {
      block_self (t->join_queue, thread, &thread_admin_mutex, NULL);
      if (t->exited)
	break;
      scm_i_pthread_mutex_unlock (&thread_admin_mutex);
      SCM_TICK;
      scm_i_scm_pthread_mutex_lock (&thread_admin_mutex);
    }
  res = t->result;

  scm_i_pthread_mutex_unlock (&thread_admin_mutex);
  return res;
}
#undef FUNC_NAME

/*** Fat mutexes */

/* We implement our own mutex type since we want them to be 'fair', we
   want to do fancy things while waiting for them (like running
   asyncs) and we might want to add things that are nice for
   debugging.
*/

typedef struct {
  scm_i_pthread_mutex_t lock;
  SCM owner;
  int level;      /* how much the owner owns us.  
		     < 0 for non-recursive mutexes */
  SCM waiting;    /* the threads waiting for this mutex. */
} fat_mutex;

#define SCM_MUTEXP(x)         SCM_SMOB_PREDICATE (scm_tc16_mutex, x)
#define SCM_MUTEX_DATA(x)     ((fat_mutex *) SCM_SMOB_DATA (x))

static SCM
fat_mutex_mark (SCM mx)
{
  fat_mutex *m = SCM_MUTEX_DATA (mx);
  scm_gc_mark (m->owner);
  return m->waiting;
}

static size_t
fat_mutex_free (SCM mx)
{
  fat_mutex *m = SCM_MUTEX_DATA (mx);
  scm_i_pthread_mutex_destroy (&m->lock);
  scm_gc_free (m, sizeof (fat_mutex), "mutex");
  return 0;
}

static int
fat_mutex_print (SCM mx, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  fat_mutex *m = SCM_MUTEX_DATA (mx);
  scm_puts ("#<mutex ", port);
  scm_uintprint ((scm_t_bits)m, 16, port);
  scm_puts (">", port);
  return 1;
}

static SCM
make_fat_mutex (int recursive)
{
  fat_mutex *m;
  SCM mx;

  m = scm_gc_malloc (sizeof (fat_mutex), "mutex");
  scm_i_pthread_mutex_init (&m->lock, NULL);
  m->owner = SCM_BOOL_F;
  m->level = recursive? 0 : -1;
  m->waiting = SCM_EOL;
  SCM_NEWSMOB (mx, scm_tc16_mutex, (scm_t_bits) m);
  m->waiting = make_queue ();
  return mx;
}

SCM_DEFINE (scm_make_mutex, "make-mutex", 0, 0, 0,
	    (void),
	    "Create a new mutex. ")
#define FUNC_NAME s_scm_make_mutex
{
  return make_fat_mutex (0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_recursive_mutex, "make-recursive-mutex", 0, 0, 0,
	    (void),
	    "Create a new recursive mutex. ")
#define FUNC_NAME s_scm_make_recursive_mutex
{
  return make_fat_mutex (1);
}
#undef FUNC_NAME

static char *
fat_mutex_lock (SCM mutex)
{
  fat_mutex *m = SCM_MUTEX_DATA (mutex);
  SCM thread = scm_current_thread ();
  char *msg = NULL;

  scm_i_scm_pthread_mutex_lock (&m->lock);
  if (scm_is_false (m->owner))
    m->owner = thread;
  else if (scm_is_eq (m->owner, thread))
    {
      if (m->level >= 0)
	m->level++;
      else
	msg = "mutex already locked by current thread";
    }
  else
    {
      while (1)
	{
	  if (scm_is_eq (m->owner, thread))
	    break;
	  block_self (m->waiting, mutex, &m->lock, NULL);
	  scm_i_pthread_mutex_unlock (&m->lock);
	  SCM_TICK;
	  scm_i_scm_pthread_mutex_lock (&m->lock);
	}
    }
  scm_i_pthread_mutex_unlock (&m->lock);
  return msg;
}

SCM_DEFINE (scm_lock_mutex, "lock-mutex", 1, 0, 0,
	    (SCM mx),
"Lock @var{mutex}. If the mutex is already locked, the calling thread "
"blocks until the mutex becomes available. The function returns when "
"the calling thread owns the lock on @var{mutex}.  Locking a mutex that "
"a thread already owns will succeed right away and will not block the "
"thread.  That is, Guile's mutexes are @emph{recursive}. ")
#define FUNC_NAME s_scm_lock_mutex
{
  char *msg;

  SCM_VALIDATE_MUTEX (1, mx);
  msg = fat_mutex_lock (mx);
  if (msg)
    scm_misc_error (NULL, msg, SCM_EOL);
  return SCM_BOOL_T;
}
#undef FUNC_NAME

void
scm_dynwind_lock_mutex (SCM mutex)
{
  scm_dynwind_unwind_handler_with_scm ((void(*)(SCM))scm_unlock_mutex, mutex,
				       SCM_F_WIND_EXPLICITLY);
  scm_dynwind_rewind_handler_with_scm ((void(*)(SCM))scm_lock_mutex, mutex,
				       SCM_F_WIND_EXPLICITLY);
}

static char *
fat_mutex_trylock (fat_mutex *m, int *resp)
{
  char *msg = NULL;
  SCM thread = scm_current_thread ();

  *resp = 1;
  scm_i_pthread_mutex_lock (&m->lock);
  if (scm_is_false (m->owner))
    m->owner = thread;
  else if (scm_is_eq (m->owner, thread))
    {
      if (m->level >= 0)
	m->level++;
      else
	msg = "mutex already locked by current thread";
    }
  else
    *resp = 0;
  scm_i_pthread_mutex_unlock (&m->lock);
  return msg;
}

SCM_DEFINE (scm_try_mutex, "try-mutex", 1, 0, 0,
	    (SCM mutex),
"Try to lock @var{mutex}. If the mutex is already locked by someone "
"else, return @code{#f}.  Else lock the mutex and return @code{#t}. ")
#define FUNC_NAME s_scm_try_mutex
{
  char *msg;
  int res;

  SCM_VALIDATE_MUTEX (1, mutex);
  
  msg = fat_mutex_trylock (SCM_MUTEX_DATA (mutex), &res);
  if (msg)
    scm_misc_error (NULL, msg, SCM_EOL);
  return scm_from_bool (res);
}
#undef FUNC_NAME

static char *
fat_mutex_unlock (fat_mutex *m)
{
  char *msg = NULL;

  scm_i_scm_pthread_mutex_lock (&m->lock);
  if (!scm_is_eq (m->owner, scm_current_thread ()))
    {
      if (scm_is_false (m->owner))
	msg = "mutex not locked";
      else
	msg = "mutex not locked by current thread";
    }
  else if (m->level > 0)
    m->level--;
  else
    m->owner = unblock_from_queue (m->waiting);
  scm_i_pthread_mutex_unlock (&m->lock);

  return msg;
}

SCM_DEFINE (scm_unlock_mutex, "unlock-mutex", 1, 0, 0,
	    (SCM mx),
"Unlocks @var{mutex} if the calling thread owns the lock on "
"@var{mutex}.  Calling unlock-mutex on a mutex not owned by the current "
"thread results in undefined behaviour. Once a mutex has been unlocked, "
"one thread blocked on @var{mutex} is awakened and grabs the mutex "
"lock.  Every call to @code{lock-mutex} by this thread must be matched "
"with a call to @code{unlock-mutex}.  Only the last call to "
"@code{unlock-mutex} will actually unlock the mutex. ")
#define FUNC_NAME s_scm_unlock_mutex
{
  char *msg;
  SCM_VALIDATE_MUTEX (1, mx);
  
  msg = fat_mutex_unlock (SCM_MUTEX_DATA (mx));
  if (msg)
    scm_misc_error (NULL, msg, SCM_EOL);
  return SCM_BOOL_T;
}
#undef FUNC_NAME

#if 0

SCM_DEFINE (scm_mutex_owner, "mutex-owner", 1, 0, 0,
	    (SCM mx),
	    "Return the thread owning @var{mx}, or @code{#f}.")
#define FUNC_NAME s_scm_mutex_owner
{
  SCM_VALIDATE_MUTEX (1, mx);
  return (SCM_MUTEX_DATA(mx))->owner;
}
#undef FUNC_NAME

SCM_DEFINE (scm_mutex_level, "mutex-level", 1, 0, 0,
	    (SCM mx),
	    "Return the lock level of a recursive mutex, or -1\n"
	    "for a standard mutex.")
#define FUNC_NAME s_scm_mutex_level
{
  SCM_VALIDATE_MUTEX (1, mx);
  return scm_from_int (SCM_MUTEX_DATA(mx)->level);
}
#undef FUNC_NAME

#endif

/*** Fat condition variables */

typedef struct {
  scm_i_pthread_mutex_t lock;
  SCM waiting;               /* the threads waiting for this condition. */
} fat_cond;

#define SCM_CONDVARP(x)       SCM_SMOB_PREDICATE (scm_tc16_condvar, x)
#define SCM_CONDVAR_DATA(x)   ((fat_cond *) SCM_SMOB_DATA (x))

static SCM
fat_cond_mark (SCM cv)
{
  fat_cond *c = SCM_CONDVAR_DATA (cv);
  return c->waiting;
}

static size_t
fat_cond_free (SCM mx)
{
  fat_cond *c = SCM_CONDVAR_DATA (mx);
  scm_i_pthread_mutex_destroy (&c->lock);
  scm_gc_free (c, sizeof (fat_cond), "condition-variable");
  return 0;
}

static int
fat_cond_print (SCM cv, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  fat_cond *c = SCM_CONDVAR_DATA (cv);
  scm_puts ("#<condition-variable ", port);
  scm_uintprint ((scm_t_bits)c, 16, port);
  scm_puts (">", port);
  return 1;
}

SCM_DEFINE (scm_make_condition_variable, "make-condition-variable", 0, 0, 0,
	    (void),
	    "Make a new condition variable.")
#define FUNC_NAME s_scm_make_condition_variable
{
  fat_cond *c;
  SCM cv;

  c = scm_gc_malloc (sizeof (fat_cond), "condition variable");
  scm_i_pthread_mutex_init (&c->lock, 0);
  c->waiting = SCM_EOL;
  SCM_NEWSMOB (cv, scm_tc16_condvar, (scm_t_bits) c);
  c->waiting = make_queue ();
  return cv;
}
#undef FUNC_NAME

static int
fat_cond_timedwait (SCM cond, SCM mutex,
		    const scm_t_timespec *waittime)
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  fat_cond *c = SCM_CONDVAR_DATA (cond);
  fat_mutex *m = SCM_MUTEX_DATA (mutex);
  const char *msg;
  int err = 0;

  while (1)
    {
      scm_i_scm_pthread_mutex_lock (&c->lock);
      msg = fat_mutex_unlock (m);
      t->block_asyncs++;
      if (msg == NULL)
	{
	  err = block_self (c->waiting, cond, &c->lock, waittime);
	  scm_i_pthread_mutex_unlock (&c->lock);
	  fat_mutex_lock (mutex);
	}
      else
	scm_i_pthread_mutex_unlock (&c->lock);
      t->block_asyncs--;
      scm_async_click ();

      if (msg)
	scm_misc_error (NULL, msg, SCM_EOL);

      scm_remember_upto_here_2 (cond, mutex);

      if (err == 0)
	return 1;
      if (err == ETIMEDOUT)
	return 0;
      if (err != EINTR)
	{
	  errno = err;
	  scm_syserror (NULL);
	}
    }
}

SCM_DEFINE (scm_timed_wait_condition_variable, "wait-condition-variable", 2, 1, 0,
	    (SCM cv, SCM mx, SCM t),
"Wait until @var{cond-var} has been signalled.  While waiting, "
"@var{mutex} is atomically unlocked (as with @code{unlock-mutex}) and "
"is locked again when this function returns.  When @var{time} is given, "
"it specifies a point in time where the waiting should be aborted.  It "
"can be either a integer as returned by @code{current-time} or a pair "
"as returned by @code{gettimeofday}.  When the waiting is aborted the "
"mutex is locked and @code{#f} is returned.  When the condition "
"variable is in fact signalled, the mutex is also locked and @code{#t} "
"is returned. ")
#define FUNC_NAME s_scm_timed_wait_condition_variable
{
  scm_t_timespec waittime, *waitptr = NULL;

  SCM_VALIDATE_CONDVAR (1, cv);
  SCM_VALIDATE_MUTEX (2, mx);
  
  if (!SCM_UNBNDP (t))
    {
      if (scm_is_pair (t))
	{
	  waittime.tv_sec = scm_to_ulong (SCM_CAR (t));
	  waittime.tv_nsec = scm_to_ulong (SCM_CAR (t)) * 1000;
	}
      else
	{
	  waittime.tv_sec = scm_to_ulong (t);
	  waittime.tv_nsec = 0;
	}
      waitptr = &waittime;
    }

  return scm_from_bool (fat_cond_timedwait (cv, mx, waitptr));
}
#undef FUNC_NAME

static void
fat_cond_signal (fat_cond *c)
{
  scm_i_scm_pthread_mutex_lock (&c->lock);
  unblock_from_queue (c->waiting);
  scm_i_pthread_mutex_unlock (&c->lock);
}

SCM_DEFINE (scm_signal_condition_variable, "signal-condition-variable", 1, 0, 0,
	    (SCM cv),
	    "Wake up one thread that is waiting for @var{cv}")
#define FUNC_NAME s_scm_signal_condition_variable
{
  SCM_VALIDATE_CONDVAR (1, cv);
  fat_cond_signal (SCM_CONDVAR_DATA (cv));
  return SCM_BOOL_T;
}
#undef FUNC_NAME

static void
fat_cond_broadcast (fat_cond *c)
{
  scm_i_scm_pthread_mutex_lock (&c->lock);
  while (scm_is_true (unblock_from_queue (c->waiting)))
    ;
  scm_i_pthread_mutex_unlock (&c->lock);
}

SCM_DEFINE (scm_broadcast_condition_variable, "broadcast-condition-variable", 1, 0, 0,
	    (SCM cv),
	    "Wake up all threads that are waiting for @var{cv}. ")
#define FUNC_NAME s_scm_broadcast_condition_variable
{
  SCM_VALIDATE_CONDVAR (1, cv);
  fat_cond_broadcast (SCM_CONDVAR_DATA (cv));
  return SCM_BOOL_T;
}
#undef FUNC_NAME

/*** Marking stacks */

/* XXX - what to do with this?  Do we need to handle this for blocked
   threads as well?
*/
#ifdef __ia64__
# define SCM_MARK_BACKING_STORE() do {                                \
    ucontext_t ctx;                                                   \
    SCM_STACKITEM * top, * bot;                                       \
    getcontext (&ctx);                                                \
    scm_mark_locations ((SCM_STACKITEM *) &ctx.uc_mcontext,           \
      ((size_t) (sizeof (SCM_STACKITEM) - 1 + sizeof ctx.uc_mcontext) \
       / sizeof (SCM_STACKITEM)));                                    \
    bot = (SCM_STACKITEM *) SCM_I_CURRENT_THREAD->register_backing_store_base;  \
    top = (SCM_STACKITEM *) scm_ia64_ar_bsp (&ctx);                   \
    scm_mark_locations (bot, top - bot); } while (0)
#else
# define SCM_MARK_BACKING_STORE()
#endif

void
scm_threads_mark_stacks (void)
{
  scm_i_thread *t;
  for (t = all_threads; t; t = t->next_thread)
    {
      /* Check that thread has indeed been suspended.
       */
      assert (t->top);

      scm_gc_mark (t->handle);

#if SCM_STACK_GROWS_UP
      scm_mark_locations (t->base, t->top - t->base);
#else
      scm_mark_locations (t->top, t->base - t->top);
#endif
      scm_mark_locations ((void *) &t->regs,
			  ((size_t) sizeof(t->regs)
			   / sizeof (SCM_STACKITEM)));
    }

  SCM_MARK_BACKING_STORE ();
}

/*** Select */

int
scm_std_select (int nfds,
		SELECT_TYPE *readfds,
		SELECT_TYPE *writefds,
		SELECT_TYPE *exceptfds,
		struct timeval *timeout)
{
  fd_set my_readfds;
  int res, eno, wakeup_fd;
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  scm_t_guile_ticket ticket;

  if (readfds == NULL)
    {
      FD_ZERO (&my_readfds);
      readfds = &my_readfds;
    }

  while (scm_i_setup_sleep (t, SCM_BOOL_F, NULL, t->sleep_pipe[1]))
    SCM_TICK;

  wakeup_fd = t->sleep_pipe[0];
  ticket = scm_leave_guile ();
  FD_SET (wakeup_fd, readfds);
  if (wakeup_fd >= nfds)
    nfds = wakeup_fd+1;
  res = select (nfds, readfds, writefds, exceptfds, timeout);
  t->sleep_fd = -1;
  eno = errno;
  scm_enter_guile (ticket);

  scm_i_reset_sleep (t);

  if (res > 0 && FD_ISSET (wakeup_fd, readfds))
    {
      char dummy;
      size_t count;

      count = read (wakeup_fd, &dummy, 1);

      FD_CLR (wakeup_fd, readfds);
      res -= 1;
      if (res == 0)
	{
	  eno = EINTR;
	  res = -1;
	}
    }
  errno = eno;
  return res;
}

/* Convenience API for blocking while in guile mode. */

#if SCM_USE_PTHREAD_THREADS

int
scm_pthread_mutex_lock (scm_i_pthread_mutex_t *mutex)
{
  scm_t_guile_ticket t = scm_leave_guile ();
  int res = scm_i_pthread_mutex_lock (mutex);
  scm_enter_guile (t);
  return res;
}

static void
do_unlock (void *data)
{
  scm_i_pthread_mutex_unlock ((scm_i_pthread_mutex_t *)data);
}

void
scm_dynwind_pthread_mutex_lock (scm_i_pthread_mutex_t *mutex)
{
  scm_i_scm_pthread_mutex_lock (mutex);
  scm_dynwind_unwind_handler (do_unlock, mutex, SCM_F_WIND_EXPLICITLY);
}

int
scm_pthread_cond_wait (scm_i_pthread_cond_t *cond, scm_i_pthread_mutex_t *mutex)
{
  scm_t_guile_ticket t = scm_leave_guile ();
  int res = scm_i_pthread_cond_wait (cond, mutex);
  scm_enter_guile (t);
  return res;
}

int
scm_pthread_cond_timedwait (scm_i_pthread_cond_t *cond,
			    scm_i_pthread_mutex_t *mutex,
			    const scm_t_timespec *wt)
{
  scm_t_guile_ticket t = scm_leave_guile ();
  int res = scm_i_pthread_cond_timedwait (cond, mutex, wt);
  scm_enter_guile (t);
  return res;
}

#endif

unsigned long
scm_std_usleep (unsigned long usecs)
{
  struct timeval tv;
  tv.tv_usec = usecs % 1000000;
  tv.tv_sec = usecs / 1000000;
  scm_std_select (0, NULL, NULL, NULL, &tv);
  return tv.tv_sec * 1000000 + tv.tv_usec;
}

unsigned int
scm_std_sleep (unsigned int secs)
{
  struct timeval tv;
  tv.tv_usec = 0;
  tv.tv_sec = secs;
  scm_std_select (0, NULL, NULL, NULL, &tv);
  return tv.tv_sec;
}

/*** Misc */

SCM_DEFINE (scm_current_thread, "current-thread", 0, 0, 0,
	    (void),
	    "Return the thread that called this function.")
#define FUNC_NAME s_scm_current_thread
{
  return SCM_I_CURRENT_THREAD->handle;
}
#undef FUNC_NAME

static SCM
scm_c_make_list (size_t n, SCM fill)
{
  SCM res = SCM_EOL;
  while (n-- > 0)
    res = scm_cons (fill, res);
  return res;
}

SCM_DEFINE (scm_all_threads, "all-threads", 0, 0, 0,
	    (void),
	    "Return a list of all threads.")
#define FUNC_NAME s_scm_all_threads
{
  /* We can not allocate while holding the thread_admin_mutex because
     of the way GC is done.
  */
  int n = thread_count;
  scm_i_thread *t;
  SCM list = scm_c_make_list (n, SCM_UNSPECIFIED), *l;

  scm_i_pthread_mutex_lock (&thread_admin_mutex);
  l = &list;
  for (t = all_threads; t && n > 0; t = t->next_thread)
    {
      SCM_SETCAR (*l, t->handle);
      l = SCM_CDRLOC (*l);
      n--;
    }
  *l = SCM_EOL;
  scm_i_pthread_mutex_unlock (&thread_admin_mutex);
  return list;
}
#undef FUNC_NAME

SCM_DEFINE (scm_thread_exited_p, "thread-exited?", 1, 0, 0,
	    (SCM thread),
	    "Return @code{#t} iff @var{thread} has exited.\n")
#define FUNC_NAME s_scm_thread_exited_p
{
  return scm_from_bool (scm_c_thread_exited_p (thread));
}
#undef FUNC_NAME

int
scm_c_thread_exited_p (SCM thread)
#define FUNC_NAME  s_scm_thread_exited_p
{
  scm_i_thread *t;
  SCM_VALIDATE_THREAD (1, thread);
  t = SCM_I_THREAD_DATA (thread);
  return t->exited;
}
#undef FUNC_NAME

static scm_i_pthread_cond_t wake_up_cond;
int scm_i_thread_go_to_sleep;
static int threads_initialized_p = 0;

void
scm_i_thread_put_to_sleep ()
{
  if (threads_initialized_p)
    {
      scm_i_thread *t;

      scm_leave_guile ();
      scm_i_pthread_mutex_lock (&thread_admin_mutex);

      /* Signal all threads to go to sleep 
       */
      scm_i_thread_go_to_sleep = 1;
      for (t = all_threads; t; t = t->next_thread)
	scm_i_pthread_mutex_lock (&t->heap_mutex);
      scm_i_thread_go_to_sleep = 0;
    }
}

void
scm_i_thread_invalidate_freelists ()
{
  /* thread_admin_mutex is already locked. */

  scm_i_thread *t;
  for (t = all_threads; t; t = t->next_thread)
    if (t != SCM_I_CURRENT_THREAD)
      t->clear_freelists_p = 1;
}

void
scm_i_thread_wake_up ()
{
  if (threads_initialized_p)
    {
      scm_i_thread *t;

      scm_i_pthread_cond_broadcast (&wake_up_cond);
      for (t = all_threads; t; t = t->next_thread)
	scm_i_pthread_mutex_unlock (&t->heap_mutex);
      scm_i_pthread_mutex_unlock (&thread_admin_mutex);
      scm_enter_guile ((scm_t_guile_ticket) SCM_I_CURRENT_THREAD);
    }
}

void
scm_i_thread_sleep_for_gc ()
{
  scm_i_thread *t = suspend ();
  scm_i_pthread_cond_wait (&wake_up_cond, &t->heap_mutex);
  resume (t);
}

/* This mutex is used by SCM_CRITICAL_SECTION_START/END.
 */
scm_i_pthread_mutex_t scm_i_critical_section_mutex;

static SCM dynwind_critical_section_mutex;

void
scm_dynwind_critical_section (SCM mutex)
{
  if (scm_is_false (mutex))
    mutex = dynwind_critical_section_mutex;
  scm_dynwind_lock_mutex (mutex);
  scm_dynwind_block_asyncs ();
}

/*** Initialization */

scm_i_pthread_key_t scm_i_freelist, scm_i_freelist2;
#ifdef __MINGW32__
scm_i_pthread_key_t *scm_i_freelist_ptr = &scm_i_freelist;
scm_i_pthread_key_t *scm_i_freelist2_ptr = &scm_i_freelist2;
#endif
scm_i_pthread_mutex_t scm_i_misc_mutex;

#if SCM_USE_PTHREAD_THREADS
pthread_mutexattr_t scm_i_pthread_mutexattr_recursive[1];
#endif

void
scm_threads_prehistory (SCM_STACKITEM *base)
{
#if SCM_USE_PTHREAD_THREADS
  pthread_mutexattr_init (scm_i_pthread_mutexattr_recursive);
  pthread_mutexattr_settype (scm_i_pthread_mutexattr_recursive,
			     PTHREAD_MUTEX_RECURSIVE);
#endif

  scm_i_pthread_mutex_init (&scm_i_critical_section_mutex,
			    scm_i_pthread_mutexattr_recursive);
  scm_i_pthread_mutex_init (&scm_i_misc_mutex, NULL);
  scm_i_pthread_cond_init (&wake_up_cond, NULL);
  scm_i_pthread_key_create (&scm_i_freelist, NULL);
  scm_i_pthread_key_create (&scm_i_freelist2, NULL);
  
  guilify_self_1 (base);
}

scm_t_bits scm_tc16_thread;
scm_t_bits scm_tc16_mutex;
scm_t_bits scm_tc16_condvar;

void
scm_init_threads ()
{
  scm_tc16_thread = scm_make_smob_type ("thread", sizeof (scm_i_thread));
  scm_set_smob_mark (scm_tc16_thread, thread_mark);
  scm_set_smob_print (scm_tc16_thread, thread_print);
  scm_set_smob_free (scm_tc16_thread, thread_free);

  scm_tc16_mutex = scm_make_smob_type ("mutex", sizeof (fat_mutex));
  scm_set_smob_mark (scm_tc16_mutex, fat_mutex_mark);
  scm_set_smob_print (scm_tc16_mutex, fat_mutex_print);
  scm_set_smob_free (scm_tc16_mutex, fat_mutex_free);

  scm_tc16_condvar = scm_make_smob_type ("condition-variable",
					 sizeof (fat_cond));
  scm_set_smob_mark (scm_tc16_condvar, fat_cond_mark);
  scm_set_smob_print (scm_tc16_condvar, fat_cond_print);
  scm_set_smob_free (scm_tc16_condvar, fat_cond_free);

  scm_i_default_dynamic_state = SCM_BOOL_F;
  guilify_self_2 (SCM_BOOL_F);
  threads_initialized_p = 1;

  dynwind_critical_section_mutex =
    scm_permanent_object (scm_make_recursive_mutex ());
}

void
scm_init_threads_default_dynamic_state ()
{
  SCM state = scm_make_dynamic_state (scm_current_dynamic_state ());
  scm_i_default_dynamic_state = scm_permanent_object (state);
}

void
scm_init_thread_procs ()
{
#include "libguile/threads.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
