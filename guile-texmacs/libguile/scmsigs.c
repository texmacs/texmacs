/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2002, 2004, 2006, 2007 Free Software Foundation, Inc.
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

#include <fcntl.h>      /* for mingw */
#include <signal.h>
#include <stdio.h>
#include <errno.h>

#include "libguile/_scm.h"

#include "libguile/async.h"
#include "libguile/eval.h"
#include "libguile/root.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/scmsigs.h"

#ifdef HAVE_IO_H
#include <io.h>  /* for mingw _pipe() */
#endif

#ifdef HAVE_PROCESS_H
#include <process.h>    /* for mingw */
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef __MINGW32__
#include <windows.h>
#define alarm(sec) (0)
/* This weird comma expression is because Sleep is void under Windows. */
#define sleep(sec) (Sleep ((sec) * 1000), 0)
#define usleep(usec) (Sleep ((usec) / 1000), 0)
#define pipe(fd) _pipe (fd, 256, O_BINARY)
#endif



/* SIGRETTYPE is the type that signal handlers return.  See <signal.h> */

#ifdef RETSIGTYPE
# define SIGRETTYPE RETSIGTYPE
#else
# ifdef STDC_HEADERS
#  define SIGRETTYPE void
# else
#  define SIGRETTYPE int
# endif
#endif



/* take_signal is installed as the C signal handler whenever a Scheme
   handler is set.  When a signal arrives, take_signal will write a
   byte into the 'signal pipe'.  The 'signal delivery thread' will
   read this pipe and queue the appropriate asyncs.

   When Guile is built without threads, the signal handler will
   install the async directly.
*/


/* Scheme vectors with information about a signal.  signal_handlers
   contains the handler procedure or #f, signal_handler_asyncs
   contains the thunk to be marked as an async when the signal arrives
   (or the cell with the thunk in a singlethreaded Guile), and
   signal_handler_threads points to the thread that a signal should be
   delivered to.
*/
static SCM *signal_handlers;
static SCM signal_handler_asyncs;
static SCM signal_handler_threads;

/* saves the original C handlers, when a new handler is installed.
   set to SIG_ERR if the original handler is installed.  */
#ifdef HAVE_SIGACTION
static struct sigaction orig_handlers[NSIG];
#else
static SIGRETTYPE (*orig_handlers[NSIG])(int);
#endif

static SCM
handler_to_async (SCM handler, int signum)
{
  if (scm_is_false (handler))
    return SCM_BOOL_F;
  else
    {
      SCM async = scm_primitive_eval_x (scm_list_3 (scm_sym_lambda, SCM_EOL,
						    scm_list_2 (handler,
								scm_from_int (signum))));
#if !SCM_USE_PTHREAD_THREADS
      async = scm_cons (async, SCM_BOOL_F);
#endif
      return async;
    }
}

#if SCM_USE_PTHREAD_THREADS
/* On mingw there's no notion of inter-process signals, only a raise()
   within the process itself which apparently invokes the registered handler
   immediately.  Not sure how well the following code will cope in this
   case.  It builds but it may not offer quite the same scheme-level
   semantics as on a proper system.  If you're relying on much in the way of
   signal handling on mingw you probably lose anyway.  */

static int signal_pipe[2];

static SIGRETTYPE
take_signal (int signum)
{
  size_t count;
  char sigbyte = signum;

  count = write (signal_pipe[1], &sigbyte, 1);

#ifndef HAVE_SIGACTION
  signal (signum, take_signal);
#endif
}

typedef struct {
  ssize_t res;
  int fd;
  char *buf;
  size_t n;
} read_without_guile_data;

static void *
do_read_without_guile (void *raw_data)
{
  read_without_guile_data *data = (read_without_guile_data *)raw_data;
  data->res = read (data->fd, data->buf, data->n);
  return NULL;
}

static ssize_t
read_without_guile (int fd, char *buf, size_t n)
{
  read_without_guile_data data;
  data.fd = fd;
  data.buf = buf;
  data.n = n;
  scm_without_guile (do_read_without_guile, &data);
  return data.res;
}

static SCM
signal_delivery_thread (void *data)
{
  int n, sig;
  char sigbyte;
#if HAVE_PTHREAD_SIGMASK  /* not on mingw, see notes above */
  sigset_t all_sigs;
  sigfillset (&all_sigs);
  scm_i_pthread_sigmask (SIG_SETMASK, &all_sigs, NULL);
#endif

  while (1)
    {
      n = read_without_guile (signal_pipe[0], &sigbyte, 1);
      sig = sigbyte;
      if (n == 1 && sig >= 0 && sig < NSIG)
	{
	  SCM h, t;

	  h = SCM_SIMPLE_VECTOR_REF (signal_handler_asyncs, sig);
	  t = SCM_SIMPLE_VECTOR_REF (signal_handler_threads, sig);
	  if (scm_is_true (h))
	    scm_system_async_mark_for_thread (h, t);
	}
      else if (n < 0 && errno != EINTR)
	perror ("error in signal delivery thread");
    }

  return SCM_UNSPECIFIED;	/* not reached */
}

static void
start_signal_delivery_thread (void)
{
  if (pipe (signal_pipe) != 0)
    scm_syserror (NULL);
  scm_spawn_thread (signal_delivery_thread, NULL,
		    scm_handle_by_message, "signal delivery thread");
}

static void
ensure_signal_delivery_thread ()
{
  static scm_i_pthread_once_t once = SCM_I_PTHREAD_ONCE_INIT;
  scm_i_pthread_once (&once, start_signal_delivery_thread);
}

#else /* !SCM_USE_PTHREAD_THREADS */

static SIGRETTYPE
take_signal (int signum)
{
  SCM cell = SCM_SIMPLE_VECTOR_REF (signal_handler_asyncs, signum);
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  if (scm_is_false (SCM_CDR (cell)))
    {
      SCM_SETCDR (cell, t->active_asyncs);
      t->active_asyncs = cell;
      t->pending_asyncs = 1;
    }

#ifndef HAVE_SIGACTION
  signal (signum, take_signal);
#endif
}

static void
ensure_signal_delivery_thread ()
{
  return;
}

#endif /* !SCM_USE_PTHREAD_THREADS */

static void
install_handler (int signum, SCM thread, SCM handler, SCM async)
{
  SCM_SIMPLE_VECTOR_SET (*signal_handlers, signum, handler);
  SCM_SIMPLE_VECTOR_SET (signal_handler_asyncs, signum, async);
  SCM_SIMPLE_VECTOR_SET (signal_handler_threads, signum, thread);
}

SCM
scm_sigaction (SCM signum, SCM handler, SCM flags)
{
  return scm_sigaction_for_thread (signum, handler, flags, SCM_UNDEFINED);
}

/* user interface for installation of signal handlers.  */
SCM_DEFINE (scm_sigaction_for_thread, "sigaction", 1, 3, 0,
           (SCM signum, SCM handler, SCM flags, SCM thread),
	    "Install or report the signal handler for a specified signal.\n\n"
	    "@var{signum} is the signal number, which can be specified using the value\n"
	    "of variables such as @code{SIGINT}.\n\n"
	    "If @var{handler} is omitted, @code{sigaction} returns a pair: the\n"
	    "CAR is the current\n"
	    "signal hander, which will be either an integer with the value @code{SIG_DFL}\n"
	    "(default action) or @code{SIG_IGN} (ignore), or the Scheme procedure which\n"
	    "handles the signal, or @code{#f} if a non-Scheme procedure handles the\n"
	    "signal.  The CDR contains the current @code{sigaction} flags for the handler.\n\n"
	    "If @var{handler} is provided, it is installed as the new handler for\n"
	    "@var{signum}.  @var{handler} can be a Scheme procedure taking one\n"
	    "argument, or the value of @code{SIG_DFL} (default action) or\n"
	    "@code{SIG_IGN} (ignore), or @code{#f} to restore whatever signal handler\n"
	    "was installed before @code{sigaction} was first used.  When\n"
	    "a scheme procedure has been specified, that procedure will run\n"
	    "in the given @var{thread}.   When no thread has been given, the\n"
	    "thread that made this call to @code{sigaction} is used.\n"
	    "Flags can optionally be specified for the new handler.\n"
	    "The return value is a pair with information about the\n"
	    "old handler as described above.\n\n"
	    "This interface does not provide access to the \"signal blocking\"\n"
	    "facility.  Maybe this is not needed, since the thread support may\n"
	    "provide solutions to the problem of consistent access to data\n"
	    "structures.")
#define FUNC_NAME s_scm_sigaction_for_thread
{
  int csig;
#ifdef HAVE_SIGACTION
  struct sigaction action;
  struct sigaction old_action;
#else
  SIGRETTYPE (* chandler) (int) = SIG_DFL;
  SIGRETTYPE (* old_chandler) (int);
#endif
  int query_only = 0;
  int save_handler = 0;
      
  SCM old_handler;
  SCM async;

  csig = scm_to_signed_integer (signum, 0, NSIG-1);

#if defined(HAVE_SIGACTION)
  action.sa_flags = 0;
  if (!SCM_UNBNDP (flags))
    action.sa_flags |= scm_to_int (flags);
  sigemptyset (&action.sa_mask);
#endif

  if (SCM_UNBNDP (thread))
    thread = scm_current_thread ();
  else
    {
      SCM_VALIDATE_THREAD (4, thread);
      if (scm_c_thread_exited_p (thread))
	SCM_MISC_ERROR ("thread has already exited", SCM_EOL);
    }

  /* Allocate upfront, as we can't do it inside the critical
     section. */
  async = handler_to_async (handler, csig);

  ensure_signal_delivery_thread ();

  SCM_CRITICAL_SECTION_START;
  old_handler = SCM_SIMPLE_VECTOR_REF (*signal_handlers, csig);
  if (SCM_UNBNDP (handler))
    query_only = 1;
  else if (scm_is_integer (handler))
    {
      long handler_int = scm_to_long (handler);

      if (handler_int == (long) SIG_DFL || handler_int == (long) SIG_IGN)
	{
#ifdef HAVE_SIGACTION
	  action.sa_handler = (SIGRETTYPE (*) (int)) handler_int;
#else
	  chandler = (SIGRETTYPE (*) (int)) handler_int;
#endif
	  install_handler (csig, SCM_BOOL_F, SCM_BOOL_F, async);
	}
      else
	{
	  SCM_CRITICAL_SECTION_END;
	  SCM_OUT_OF_RANGE (2, handler);
	}
    }
  else if (scm_is_false (handler))
    {
      /* restore the default handler.  */
#ifdef HAVE_SIGACTION
      if (orig_handlers[csig].sa_handler == SIG_ERR)
	query_only = 1;
      else
	{
	  action = orig_handlers[csig];
	  orig_handlers[csig].sa_handler = SIG_ERR;
	  install_handler (csig, SCM_BOOL_F, SCM_BOOL_F, async);
	}
#else
      if (orig_handlers[csig] == SIG_ERR)
	query_only = 1;
      else
	{
	  chandler = orig_handlers[csig];
	  orig_handlers[csig] = SIG_ERR;
	  install_handler (csig, SCM_BOOL_F, SCM_BOOL_F, async);
	}
#endif
    }
  else
    {
      SCM_VALIDATE_PROC (2, handler);
#ifdef HAVE_SIGACTION
      action.sa_handler = take_signal;
      if (orig_handlers[csig].sa_handler == SIG_ERR)
	save_handler = 1;
#else
      chandler = take_signal;
      if (orig_handlers[csig] == SIG_ERR)
	save_handler = 1;
#endif
      install_handler (csig, thread, handler, async);
    }

  /* XXX - Silently ignore setting handlers for `program error signals'
     because they can't currently be handled by Scheme code.
  */

  switch (csig)
    {
      /* This list of program error signals is from the GNU Libc
         Reference Manual */
    case SIGFPE:
    case SIGILL:
    case SIGSEGV:
#ifdef SIGBUS
    case SIGBUS:
#endif
    case SIGABRT:
#if defined(SIGIOT) && (SIGIOT != SIGABRT)
    case SIGIOT:
#endif
#ifdef SIGTRAP
    case SIGTRAP:
#endif
#ifdef SIGEMT
    case SIGEMT:
#endif
#ifdef SIGSYS
    case SIGSYS:
#endif
      query_only = 1;
    }

#ifdef HAVE_SIGACTION
  if (query_only)
    {
      if (sigaction (csig, 0, &old_action) == -1)
	SCM_SYSERROR;
    }
  else
    {
      if (sigaction (csig, &action , &old_action) == -1)
	SCM_SYSERROR;
      if (save_handler)
	orig_handlers[csig] = old_action;
    }
  if (old_action.sa_handler == SIG_DFL || old_action.sa_handler == SIG_IGN)
    old_handler = scm_from_long ((long) old_action.sa_handler);
  SCM_CRITICAL_SECTION_END;
  return scm_cons (old_handler, scm_from_int (old_action.sa_flags));
#else
  if (query_only)
    {
      if ((old_chandler = signal (csig, SIG_IGN)) == SIG_ERR)
	SCM_SYSERROR;
      if (signal (csig, old_chandler) == SIG_ERR)
	SCM_SYSERROR;
    }
  else
    {
      if ((old_chandler = signal (csig, chandler)) == SIG_ERR)
	SCM_SYSERROR;
      if (save_handler)
	orig_handlers[csig] = old_chandler;
    }
  if (old_chandler == SIG_DFL || old_chandler == SIG_IGN)
    old_handler = scm_from_long ((long) old_chandler);
  SCM_CRITICAL_SECTION_END;
  return scm_cons (old_handler, scm_from_int (0));
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_restore_signals, "restore-signals", 0, 0, 0,
            (void),
	    "Return all signal handlers to the values they had before any call to\n"
	    "@code{sigaction} was made.  The return value is unspecified.")
#define FUNC_NAME s_scm_restore_signals
{
  int i;
  for (i = 0; i < NSIG; i++)
    {
#ifdef HAVE_SIGACTION
      if (orig_handlers[i].sa_handler != SIG_ERR)
	{
	  if (sigaction (i, &orig_handlers[i], NULL) == -1)
	    SCM_SYSERROR;
	  orig_handlers[i].sa_handler = SIG_ERR;
	  SCM_SIMPLE_VECTOR_SET (*signal_handlers, i, SCM_BOOL_F);
	}
#else
      if (orig_handlers[i] != SIG_ERR)
	{
	  if (signal (i, orig_handlers[i]) == SIG_ERR)
	    SCM_SYSERROR;
	  orig_handlers[i] = SIG_ERR;
	  SCM_SIMPLE_VECTOR_SET (*signal_handlers, i, SCM_BOOL_F);	  
	}
#endif
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_alarm, "alarm", 1, 0, 0,
           (SCM i),
	    "Set a timer to raise a @code{SIGALRM} signal after the specified\n"
	    "number of seconds (an integer).  It's advisable to install a signal\n"
	    "handler for\n"
	    "@code{SIGALRM} beforehand, since the default action is to terminate\n"
	    "the process.\n\n"
	    "The return value indicates the time remaining for the previous alarm,\n"
	    "if any.  The new value replaces the previous alarm.  If there was\n"
	    "no previous alarm, the return value is zero.")
#define FUNC_NAME s_scm_alarm
{
  return scm_from_uint (alarm (scm_to_uint (i)));
}
#undef FUNC_NAME

#ifdef HAVE_SETITIMER
SCM_DEFINE (scm_setitimer, "setitimer", 5, 0, 0,
           (SCM which_timer,
            SCM interval_seconds, SCM interval_microseconds,
            SCM value_seconds, SCM value_microseconds),
            "Set the timer specified by @var{which_timer} according to the given\n"
            "@var{interval_seconds}, @var{interval_microseconds},\n"
            "@var{value_seconds}, and @var{value_microseconds} values.\n"
            "\n"
            "Return information about the timer's previous setting."
            "\n"
            "Errors are handled as described in the guile info pages under ``POSIX\n"
            "Interface Conventions''.\n"
            "\n"
            "The timers available are: @code{ITIMER_REAL}, @code{ITIMER_VIRTUAL},\n"
            "and @code{ITIMER_PROF}.\n"
            "\n"
            "The return value will be a list of two cons pairs representing the\n"
            "current state of the given timer.  The first pair is the seconds and\n"
            "microseconds of the timer @code{it_interval}, and the second pair is\n"
            "the seconds and microseconds of the timer @code{it_value}.")
#define FUNC_NAME s_scm_setitimer
{
  int rv;
  int c_which_timer;
  struct itimerval new_timer;
  struct itimerval old_timer;

  c_which_timer = SCM_NUM2INT(1, which_timer);
  new_timer.it_interval.tv_sec = SCM_NUM2LONG(2, interval_seconds);
  new_timer.it_interval.tv_usec = SCM_NUM2LONG(3, interval_microseconds);
  new_timer.it_value.tv_sec = SCM_NUM2LONG(4, value_seconds);
  new_timer.it_value.tv_usec = SCM_NUM2LONG(5, value_microseconds);

  SCM_SYSCALL(rv = setitimer(c_which_timer, &new_timer, &old_timer));
  
  if(rv != 0)
    SCM_SYSERROR;

  return scm_list_2 (scm_cons (scm_from_long (old_timer.it_interval.tv_sec),
			       scm_from_long (old_timer.it_interval.tv_usec)),
		     scm_cons (scm_from_long (old_timer.it_value.tv_sec),
			       scm_from_long (old_timer.it_value.tv_usec)));
}
#undef FUNC_NAME
#endif /* HAVE_SETITIMER */

#ifdef HAVE_GETITIMER
SCM_DEFINE (scm_getitimer, "getitimer", 1, 0, 0,
  (SCM which_timer),
            "Return information about the timer specified by @var{which_timer}"
            "\n"
            "Errors are handled as described in the guile info pages under ``POSIX\n"
            "Interface Conventions''.\n"
            "\n"
            "The timers available are: @code{ITIMER_REAL}, @code{ITIMER_VIRTUAL},\n"
            "and @code{ITIMER_PROF}.\n"
            "\n"
            "The return value will be a list of two cons pairs representing the\n"
            "current state of the given timer.  The first pair is the seconds and\n"
            "microseconds of the timer @code{it_interval}, and the second pair is\n"
            "the seconds and microseconds of the timer @code{it_value}.")
#define FUNC_NAME s_scm_getitimer
{
  int rv;
  int c_which_timer;
  struct itimerval old_timer;

  c_which_timer = SCM_NUM2INT(1, which_timer);

  SCM_SYSCALL(rv = getitimer(c_which_timer, &old_timer));
  
  if(rv != 0)
    SCM_SYSERROR;
  
  return scm_list_2 (scm_cons (scm_from_long (old_timer.it_interval.tv_sec),
			       scm_from_long (old_timer.it_interval.tv_usec)),
		     scm_cons (scm_from_long (old_timer.it_value.tv_sec),
			       scm_from_long (old_timer.it_value.tv_usec)));
}
#undef FUNC_NAME
#endif /* HAVE_GETITIMER */

#ifdef HAVE_PAUSE
SCM_DEFINE (scm_pause, "pause", 0, 0, 0,
           (),
	    "Pause the current process (thread?) until a signal arrives whose\n"
	    "action is to either terminate the current process or invoke a\n"
	    "handler procedure.  The return value is unspecified.")
#define FUNC_NAME s_scm_pause
{
  pause ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif

SCM_DEFINE (scm_sleep, "sleep", 1, 0, 0,
           (SCM i),
	    "Wait for the given number of seconds (an integer) or until a signal\n"
	    "arrives.  The return value is zero if the time elapses or the number\n"
	    "of seconds remaining otherwise.\n"
	    "\n"
	    "See also @code{usleep}.")
#define FUNC_NAME s_scm_sleep
{
  return scm_from_uint (scm_std_sleep (scm_to_uint (i)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_usleep, "usleep", 1, 0, 0,
           (SCM i),
	    "Wait the given period @var{usecs} microseconds (an integer).\n"
	    "If a signal arrives the wait stops and the return value is the\n"
	    "time remaining, in microseconds.  If the period elapses with no\n"
	    "signal the return is zero.\n"
	    "\n"
	    "On most systems the process scheduler is not microsecond accurate and\n"
	    "the actual period slept by @code{usleep} may be rounded to a system\n"
	    "clock tick boundary.  Traditionally such ticks were 10 milliseconds\n"
	    "apart, and that interval is often still used.\n"
	    "\n"
	    "See also @code{sleep}.")
#define FUNC_NAME s_scm_usleep
{
  return scm_from_ulong (scm_std_usleep (scm_to_ulong (i)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_raise, "raise", 1, 0, 0,
           (SCM sig),
	    "Sends a specified signal @var{sig} to the current process, where\n"
	    "@var{sig} is as described for the kill procedure.")
#define FUNC_NAME s_scm_raise
{
  if (raise (scm_to_int (sig)) != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



void
scm_init_scmsigs ()
{
  int i;

  signal_handlers =
    SCM_VARIABLE_LOC (scm_c_define ("signal-handlers",
				  scm_c_make_vector (NSIG, SCM_BOOL_F)));
  signal_handler_asyncs =
    scm_permanent_object (scm_c_make_vector (NSIG, SCM_BOOL_F));
  signal_handler_threads =
    scm_permanent_object (scm_c_make_vector (NSIG, SCM_BOOL_F));

  for (i = 0; i < NSIG; i++)
    {
#ifdef HAVE_SIGACTION
      orig_handlers[i].sa_handler = SIG_ERR;

#else
      orig_handlers[i] = SIG_ERR;
#endif
    }

  scm_c_define ("NSIG", scm_from_long (NSIG));
  scm_c_define ("SIG_IGN", scm_from_long ((long) SIG_IGN));
  scm_c_define ("SIG_DFL", scm_from_long ((long) SIG_DFL));
#ifdef SA_NOCLDSTOP
  scm_c_define ("SA_NOCLDSTOP", scm_from_long (SA_NOCLDSTOP));
#endif
#ifdef SA_RESTART
  scm_c_define ("SA_RESTART", scm_from_long (SA_RESTART));
#endif

#if defined(HAVE_SETITIMER) || defined(HAVE_GETITIMER)
  /* Stuff needed by setitimer and getitimer. */
  scm_c_define ("ITIMER_REAL", scm_from_int (ITIMER_REAL));
  scm_c_define ("ITIMER_VIRTUAL", scm_from_int (ITIMER_VIRTUAL));
  scm_c_define ("ITIMER_PROF", scm_from_int (ITIMER_PROF));
#endif /* defined(HAVE_SETITIMER) || defined(HAVE_GETITIMER) */

#include "libguile/scmsigs.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
