/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2003, 2004, 2006, 2008 Free Software Foundation, Inc.
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
#include "libguile/_scm.h"
#include "libguile/async.h"
#include "libguile/smob.h"
#include "libguile/alist.h"
#include "libguile/eval.h"
#include "libguile/eq.h"
#include "libguile/dynwind.h"
#include "libguile/backtrace.h"
#include "libguile/debug.h"
#include "libguile/continuations.h"
#include "libguile/stackchk.h"
#include "libguile/stacks.h"
#include "libguile/fluids.h"
#include "libguile/ports.h"
#include "libguile/lang.h"
#include "libguile/validate.h"
#include "libguile/throw.h"
#include "libguile/init.h"


/* the jump buffer data structure */
static scm_t_bits tc16_jmpbuffer;

#define SCM_JMPBUFP(OBJ)	SCM_TYP16_PREDICATE (tc16_jmpbuffer, OBJ)

#define JBACTIVE(OBJ)		(SCM_CELL_WORD_0 (OBJ) & (1L << 16L))
#define ACTIVATEJB(x)	\
  (SCM_SET_CELL_WORD_0 ((x), (SCM_CELL_WORD_0 (x) | (1L << 16L))))
#define DEACTIVATEJB(x) \
  (SCM_SET_CELL_WORD_0 ((x), (SCM_CELL_WORD_0 (x) & ~(1L << 16L))))

#define JBJMPBUF(OBJ)           ((scm_i_jmp_buf *) SCM_CELL_WORD_1 (OBJ))
#define SETJBJMPBUF(x, v)        (SCM_SET_CELL_WORD_1 ((x), (scm_t_bits) (v)))
#define SCM_JBDFRAME(x)         ((scm_t_debug_frame *) SCM_CELL_WORD_2 (x))
#define SCM_SETJBDFRAME(x, v)    (SCM_SET_CELL_WORD_2 ((x), (scm_t_bits) (v)))
#define SCM_JBPREUNWIND(x)      ((struct pre_unwind_data *) SCM_CELL_WORD_3 (x))
#define SCM_SETJBPREUNWIND(x, v) (SCM_SET_CELL_WORD_3 ((x), (scm_t_bits) (v)))

static int
jmpbuffer_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_puts ("#<jmpbuffer ", port);
  scm_puts (JBACTIVE(exp) ? "(active) " : "(inactive) ", port);
  scm_uintprint((scm_t_bits) JBJMPBUF (exp), 16, port);
  scm_putc ('>', port);
  return 1 ;
}

static SCM
make_jmpbuf (void)
{
  SCM answer;
  SCM_NEWSMOB2 (answer, tc16_jmpbuffer, 0, 0);
  SETJBJMPBUF(answer, (scm_i_jmp_buf *)0);
  DEACTIVATEJB(answer);
  return answer;
}


/* scm_c_catch (the guts of catch) */

struct jmp_buf_and_retval	/* use only on the stack, in scm_catch */
{
  scm_i_jmp_buf buf;		/* must be first */
  SCM throw_tag;
  SCM retval;
};

/* These are the structures we use to store pre-unwind handling (aka
   "lazy") information for a regular catch, and put on the wind list
   for a "lazy" catch.  They store the pre-unwind handler function to
   call, and the data pointer to pass through to it.  It's not a
   Scheme closure, but it is a function with data, so the term
   "closure" is appropriate in its broader sense.

   (We don't need anything like this to run the normal (post-unwind)
   catch handler, because the same C frame runs both the body and the
   handler.)  */

struct pre_unwind_data {
  scm_t_catch_handler handler;
  void *handler_data;
  int running;
  int lazy_catch_p;
};


/* scm_c_catch is the guts of catch.  It handles all the mechanics of
   setting up a catch target, invoking the catch body, and perhaps
   invoking the handler if the body does a throw.

   The function is designed to be usable from C code, but is general
   enough to implement all the semantics Guile Scheme expects from
   throw.

   TAG is the catch tag.  Typically, this is a symbol, but this
   function doesn't actually care about that.

   BODY is a pointer to a C function which runs the body of the catch;
   this is the code you can throw from.  We call it like this:
      BODY (BODY_DATA)
   where:
      BODY_DATA is just the BODY_DATA argument we received; we pass it
	 through to BODY as its first argument.  The caller can make
	 BODY_DATA point to anything useful that BODY might need.

   HANDLER is a pointer to a C function to deal with a throw to TAG,
   should one occur.  We call it like this:
      HANDLER (HANDLER_DATA, THROWN_TAG, THROW_ARGS)
   where
      HANDLER_DATA is the HANDLER_DATA argument we recevied; it's the
         same idea as BODY_DATA above.
      THROWN_TAG is the tag that the user threw to; usually this is
         TAG, but it could be something else if TAG was #t (i.e., a
         catch-all), or the user threw to a jmpbuf.
      THROW_ARGS is the list of arguments the user passed to the THROW
         function, after the tag.

   BODY_DATA is just a pointer we pass through to BODY.  HANDLER_DATA
   is just a pointer we pass through to HANDLER.  We don't actually
   use either of those pointers otherwise ourselves.  The idea is
   that, if our caller wants to communicate something to BODY or
   HANDLER, it can pass a pointer to it as MUMBLE_DATA, which BODY and
   HANDLER can then use.  Think of it as a way to make BODY and
   HANDLER closures, not just functions; MUMBLE_DATA points to the
   enclosed variables.

   Of course, it's up to the caller to make sure that any data a
   MUMBLE_DATA needs is protected from GC.  A common way to do this is
   to make MUMBLE_DATA a pointer to data stored in an automatic
   structure variable; since the collector must scan the stack for
   references anyway, this assures that any references in MUMBLE_DATA
   will be found.  */

SCM
scm_c_catch (SCM tag,
	     scm_t_catch_body body, void *body_data,
	     scm_t_catch_handler handler, void *handler_data,
	     scm_t_catch_handler pre_unwind_handler, void *pre_unwind_handler_data)
{
  struct jmp_buf_and_retval jbr;
  SCM jmpbuf;
  SCM answer;
  struct pre_unwind_data pre_unwind;

  jmpbuf = make_jmpbuf ();
  answer = SCM_EOL;
  scm_i_set_dynwinds (scm_acons (tag, jmpbuf, scm_i_dynwinds ()));
  SETJBJMPBUF(jmpbuf, &jbr.buf);
  SCM_SETJBDFRAME(jmpbuf, scm_i_last_debug_frame ());

  pre_unwind.handler = pre_unwind_handler;
  pre_unwind.handler_data = pre_unwind_handler_data;
  pre_unwind.running = 0;
  pre_unwind.lazy_catch_p = 0;
  SCM_SETJBPREUNWIND(jmpbuf, &pre_unwind);

  if (SCM_I_SETJMP (jbr.buf))
    {
      SCM throw_tag;
      SCM throw_args;

#ifdef STACK_CHECKING
      scm_stack_checking_enabled_p = SCM_STACK_CHECKING_P;
#endif
      SCM_CRITICAL_SECTION_START;
      DEACTIVATEJB (jmpbuf);
      scm_i_set_dynwinds (SCM_CDR (scm_i_dynwinds ()));
      SCM_CRITICAL_SECTION_END;
      throw_args = jbr.retval;
      throw_tag = jbr.throw_tag;
      jbr.throw_tag = SCM_EOL;
      jbr.retval = SCM_EOL;
      answer = handler (handler_data, throw_tag, throw_args);
    }
  else
    {
      ACTIVATEJB (jmpbuf);
      answer = body (body_data);
      SCM_CRITICAL_SECTION_START;
      DEACTIVATEJB (jmpbuf);
      scm_i_set_dynwinds (SCM_CDR (scm_i_dynwinds ()));
      SCM_CRITICAL_SECTION_END;
    }
  return answer;
}

SCM
scm_internal_catch (SCM tag,
		    scm_t_catch_body body, void *body_data,
		    scm_t_catch_handler handler, void *handler_data)
{
  return scm_c_catch(tag,
		     body, body_data,
		     handler, handler_data,
		     NULL, NULL);
}



/* The smob tag for pre_unwind_data smobs.  */
static scm_t_bits tc16_pre_unwind_data;

/* Strictly speaking, we could just pass a zero for our print
   function, because we don't need to print them.  They should never
   appear in normal data structures, only in the wind list.  However,
   it might be nice for debugging someday... */
static int
pre_unwind_data_print (SCM closure, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  struct pre_unwind_data *c = (struct pre_unwind_data *) SCM_CELL_WORD_1 (closure);
  char buf[200];

  sprintf (buf, "#<pre-unwind-data 0x%lx 0x%lx>",
	   (long) c->handler, (long) c->handler_data);
  scm_puts (buf, port);

  return 1;
}


/* Given a pointer to a pre_unwind_data structure, return a smob for it,
   suitable for inclusion in the wind list.  ("Ah yes, a Château
   Gollombiere '72, non?").  */
static SCM
make_pre_unwind_data (struct pre_unwind_data *c)
{
  SCM_RETURN_NEWSMOB (tc16_pre_unwind_data, c);
}

#define SCM_PRE_UNWIND_DATA_P(obj) (SCM_TYP16_PREDICATE (tc16_pre_unwind_data, obj))

SCM
scm_c_with_throw_handler (SCM tag,
			  scm_t_catch_body body,
			  void *body_data,
			  scm_t_catch_handler handler,
			  void *handler_data,
			  int lazy_catch_p)
{
  SCM pre_unwind, answer;
  struct pre_unwind_data c;

  c.handler = handler;
  c.handler_data = handler_data;
  c.running = 0;
  c.lazy_catch_p = lazy_catch_p;
  pre_unwind = make_pre_unwind_data (&c);

  SCM_CRITICAL_SECTION_START;
  scm_i_set_dynwinds (scm_acons (tag, pre_unwind, scm_i_dynwinds ()));
  SCM_CRITICAL_SECTION_END;

  answer = (*body) (body_data);

  SCM_CRITICAL_SECTION_START;
  scm_i_set_dynwinds (SCM_CDR (scm_i_dynwinds ()));
  SCM_CRITICAL_SECTION_END;

  return answer;
}

/* Exactly like scm_internal_catch, except:
   - It does not unwind the stack (this is the major difference).
   - The handler is not allowed to return.  */
SCM
scm_internal_lazy_catch (SCM tag, scm_t_catch_body body, void *body_data, scm_t_catch_handler handler, void *handler_data)
{
  return scm_c_with_throw_handler (tag, body, body_data, handler, handler_data, 1);
}


/* scm_internal_stack_catch
   Use this one if you want debugging information to be stored in
   scm_the_last_stack_fluid_var on error. */

static SCM
ss_handler (void *data SCM_UNUSED, SCM tag, SCM throw_args)
{
  /* Save the stack */
  scm_fluid_set_x (SCM_VARIABLE_REF (scm_the_last_stack_fluid_var),
		   scm_make_stack (SCM_BOOL_T, SCM_EOL));
  /* Throw the error */
  return scm_throw (tag, throw_args);
}

struct cwss_data
{
  SCM tag;
  scm_t_catch_body body;
  void *data;
};

static SCM
cwss_body (void *data)
{
  struct cwss_data *d = data;
  return scm_internal_lazy_catch (d->tag, d->body, d->data, ss_handler, NULL);
}

SCM
scm_internal_stack_catch (SCM tag,
			  scm_t_catch_body body,
			  void *body_data,
			  scm_t_catch_handler handler,
			  void *handler_data)
{
  struct cwss_data d;
  d.tag = tag;
  d.body = body;
  d.data = body_data;
  return scm_internal_catch (tag, cwss_body, &d, handler, handler_data);
}



/* body and handler functions for use with any of the above catch variants */

/* This is a body function you can pass to scm_internal_catch if you
   want the body to be like Scheme's `catch' --- a thunk.

   BODY_DATA is a pointer to a scm_body_thunk_data structure, which
   contains the Scheme procedure to invoke as the body, and the tag
   we're catching.  */

SCM
scm_body_thunk (void *body_data)
{
  struct scm_body_thunk_data *c = (struct scm_body_thunk_data *) body_data;

  return scm_call_0 (c->body_proc);
}


/* This is a handler function you can pass to scm_internal_catch if
   you want the handler to act like Scheme's catch: (throw TAG ARGS ...)
   applies a handler procedure to (TAG ARGS ...).

   If the user does a throw to this catch, this function runs a
   handler procedure written in Scheme.  HANDLER_DATA is a pointer to
   an SCM variable holding the Scheme procedure object to invoke.  It
   ought to be a pointer to an automatic variable (i.e., one living on
   the stack), or the procedure object should be otherwise protected
   from GC.  */
SCM
scm_handle_by_proc (void *handler_data, SCM tag, SCM throw_args)
{
  SCM *handler_proc_p = (SCM *) handler_data;

  return scm_apply_1 (*handler_proc_p, tag, throw_args);
}

/* SCM_HANDLE_BY_PROC_CATCHING_ALL is like SCM_HANDLE_BY_PROC but
   catches all throws that the handler might emit itself.  The handler
   used for these `secondary' throws is SCM_HANDLE_BY_MESSAGE_NO_EXIT.  */

struct hbpca_data {
  SCM proc;
  SCM args;
};

static SCM
hbpca_body (void *body_data)
{
  struct hbpca_data *data = (struct hbpca_data *)body_data;
  return scm_apply_0 (data->proc, data->args);
}

SCM
scm_handle_by_proc_catching_all (void *handler_data, SCM tag, SCM throw_args)
{
  SCM *handler_proc_p = (SCM *) handler_data;
  struct hbpca_data data;
  data.proc = *handler_proc_p;
  data.args = scm_cons (tag, throw_args);

  return scm_internal_catch (SCM_BOOL_T,
			     hbpca_body, &data,
			     scm_handle_by_message_noexit, NULL);
}

/* Derive the an exit status from the arguments to (quit ...).  */
int
scm_exit_status (SCM args)
{
  if (!SCM_NULL_OR_NIL_P (args))
    {
      SCM cqa = SCM_CAR (args);
      
      if (scm_is_integer (cqa))
	return (scm_to_int (cqa));
      else if (scm_is_false (cqa))
	return 1;
    }
  return 0;
}
	

static void
handler_message (void *handler_data, SCM tag, SCM args)
{
  char *prog_name = (char *) handler_data;
  SCM p = scm_current_error_port ();

  if (scm_ilength (args) == 4)
    {
      SCM stack   = scm_make_stack (SCM_BOOL_T, SCM_EOL);
      SCM subr    = SCM_CAR (args);
      SCM message = SCM_CADR (args);
      SCM parts   = SCM_CADDR (args);
      SCM rest    = SCM_CADDDR (args);

      if (SCM_BACKTRACE_P && scm_is_true (stack))
	{
	  SCM highlights;

	  if (scm_is_eq (tag, scm_arg_type_key)
	      || scm_is_eq (tag, scm_out_of_range_key))
	    highlights = rest;
	  else
	    highlights = SCM_EOL;

	  scm_puts ("Backtrace:\n", p);
	  scm_display_backtrace_with_highlights (stack, p,
						 SCM_BOOL_F, SCM_BOOL_F,
						 highlights);
	  scm_newline (p);
	}
      scm_i_display_error (stack, p, subr, message, parts, rest);
    }
  else
    {
      if (! prog_name)
	prog_name = "guile";

      scm_puts (prog_name, p);
      scm_puts (": ", p);

      scm_puts ("uncaught throw to ", p);
      scm_prin1 (tag, p, 0);
      scm_puts (": ", p);
      scm_prin1 (args, p, 1);
      scm_putc ('\n', p);
    }
}


/* This is a handler function to use if you want scheme to print a
   message and die.  Useful for dealing with throws to uncaught keys
   at the top level.

   At boot time, we establish a catch-all that uses this as its handler.
   1) If the user wants something different, they can use (catch #t
   ...) to do what they like.
   2) Outside the context of a read-eval-print loop, there isn't
   anything else good to do; libguile should not assume the existence
   of a read-eval-print loop.
   3) Given that we shouldn't do anything complex, it's much more
   robust to do it in C code.

   HANDLER_DATA, if non-zero, is assumed to be a char * pointing to a
   message header to print; if zero, we use "guile" instead.  That
   text is followed by a colon, then the message described by ARGS.  */

/* Dirk:FIXME:: The name of the function should make clear that the
 * application gets terminated.
 */

SCM
scm_handle_by_message (void *handler_data, SCM tag, SCM args)
{
  if (scm_is_true (scm_eq_p (tag, scm_from_locale_symbol ("quit"))))
    exit (scm_exit_status (args));

  handler_message (handler_data, tag, args);
  scm_i_pthread_exit (NULL);

  /* this point not reached, but suppress gcc warning about no return value
     in case scm_i_pthread_exit isn't marked as "noreturn" (which seemed not
     to be the case on cygwin for instance) */
  return SCM_BOOL_F;
}


/* This is just like scm_handle_by_message, but it doesn't exit; it
   just returns #f.  It's useful in cases where you don't really know
   enough about the body to handle things in a better way, but don't
   want to let throws fall off the bottom of the wind list.  */
SCM
scm_handle_by_message_noexit (void *handler_data, SCM tag, SCM args)
{
  if (scm_is_true (scm_eq_p (tag, scm_from_locale_symbol ("quit"))))
    exit (scm_exit_status (args));

  handler_message (handler_data, tag, args);

  return SCM_BOOL_F;
}


SCM
scm_handle_by_throw (void *handler_data SCM_UNUSED, SCM tag, SCM args)
{
  scm_ithrow (tag, args, 1);
  return SCM_UNSPECIFIED;  /* never returns */
}



/* the Scheme-visible CATCH, WITH-THROW-HANDLER and LAZY-CATCH functions */

SCM_DEFINE (scm_catch_with_pre_unwind_handler, "catch", 3, 1, 0,
	    (SCM key, SCM thunk, SCM handler, SCM pre_unwind_handler),
	    "Invoke @var{thunk} in the dynamic context of @var{handler} for\n"
	    "exceptions matching @var{key}.  If thunk throws to the symbol\n"
	    "@var{key}, then @var{handler} is invoked this way:\n"
	    "@lisp\n"
	    "(handler key args ...)\n"
	    "@end lisp\n"
	    "\n"
	    "@var{key} is a symbol or @code{#t}.\n"
	    "\n"
	    "@var{thunk} takes no arguments.  If @var{thunk} returns\n"
	    "normally, that is the return value of @code{catch}.\n"
	    "\n"
	    "Handler is invoked outside the scope of its own @code{catch}.\n"
	    "If @var{handler} again throws to the same key, a new handler\n"
	    "from further up the call chain is invoked.\n"
	    "\n"
	    "If the key is @code{#t}, then a throw to @emph{any} symbol will\n"
	    "match this call to @code{catch}.\n"
	    "\n"
	    "If a @var{pre-unwind-handler} is given and @var{thunk} throws\n"
	    "an exception that matches @var{key}, Guile calls the\n"
	    "@var{pre-unwind-handler} before unwinding the dynamic state and\n"
	    "invoking the main @var{handler}.  @var{pre-unwind-handler} should\n"
	    "be a procedure with the same signature as @var{handler}, that\n"
	    "is @code{(lambda (key . args))}.  It is typically used to save\n"
	    "the stack at the point where the exception occurred, but can also\n"
	    "query other parts of the dynamic state at that point, such as\n"
	    "fluid values.\n"
	    "\n"
	    "A @var{pre-unwind-handler} can exit either normally or non-locally.\n"
	    "If it exits normally, Guile unwinds the stack and dynamic context\n"
	    "and then calls the normal (third argument) handler.  If it exits\n"
	    "non-locally, that exit determines the continuation.")
#define FUNC_NAME s_scm_catch_with_pre_unwind_handler
{
  struct scm_body_thunk_data c;

  SCM_ASSERT (scm_is_symbol (key) || scm_is_eq (key, SCM_BOOL_T),
	      key, SCM_ARG1, FUNC_NAME);

  c.tag = key;
  c.body_proc = thunk;

  /* scm_c_catch takes care of all the mechanics of setting up a catch
     key; we tell it to call scm_body_thunk to run the body, and
     scm_handle_by_proc to deal with any throws to this catch.  The
     former receives a pointer to c, telling it how to behave.  The
     latter receives a pointer to HANDLER, so it knows who to
     call.  */
  return scm_c_catch (key,
		      scm_body_thunk, &c, 
		      scm_handle_by_proc, &handler,
		      SCM_UNBNDP (pre_unwind_handler) ? NULL : scm_handle_by_proc,
		      &pre_unwind_handler);
}
#undef FUNC_NAME

/* The following function exists to provide backwards compatibility
   for the C scm_catch API.  Otherwise we could just change
   "scm_catch_with_pre_unwind_handler" above to "scm_catch". */
SCM
scm_catch (SCM key, SCM thunk, SCM handler)
{
  return scm_catch_with_pre_unwind_handler (key, thunk, handler, SCM_UNDEFINED);
}


SCM_DEFINE (scm_with_throw_handler, "with-throw-handler", 3, 0, 0,
	    (SCM key, SCM thunk, SCM handler),
	    "Add @var{handler} to the dynamic context as a throw handler\n"
	    "for key @var{key}, then invoke @var{thunk}.")
#define FUNC_NAME s_scm_with_throw_handler
{
  struct scm_body_thunk_data c;

  SCM_ASSERT (scm_is_symbol (key) || scm_is_eq (key, SCM_BOOL_T),
	      key, SCM_ARG1, FUNC_NAME);

  c.tag = key;
  c.body_proc = thunk;

  /* scm_c_with_throw_handler takes care of the mechanics of setting
     up a throw handler; we tell it to call scm_body_thunk to run the
     body, and scm_handle_by_proc to deal with any throws to this
     handler.  The former receives a pointer to c, telling it how to
     behave.  The latter receives a pointer to HANDLER, so it knows
     who to call.  */
  return scm_c_with_throw_handler (key,
				   scm_body_thunk, &c, 
				   scm_handle_by_proc, &handler,
				   0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_lazy_catch, "lazy-catch", 3, 0, 0,
	    (SCM key, SCM thunk, SCM handler),
	    "This behaves exactly like @code{catch}, except that it does\n"
	    "not unwind the stack before invoking @var{handler}.\n"
	    "If the @var{handler} procedure returns normally, Guile\n"
	    "rethrows the same exception again to the next innermost catch,\n"
	    "lazy-catch or throw handler.  If the @var{handler} exits\n"
	    "non-locally, that exit determines the continuation.")
#define FUNC_NAME s_scm_lazy_catch
{
  struct scm_body_thunk_data c;

  SCM_ASSERT (scm_is_symbol (key) || scm_is_eq (key, SCM_BOOL_T),
	      key, SCM_ARG1, FUNC_NAME);

  c.tag = key;
  c.body_proc = thunk;

  /* scm_internal_lazy_catch takes care of all the mechanics of
     setting up a lazy catch key; we tell it to call scm_body_thunk to
     run the body, and scm_handle_by_proc to deal with any throws to
     this catch.  The former receives a pointer to c, telling it how
     to behave.  The latter receives a pointer to HANDLER, so it knows
     who to call.  */
  return scm_internal_lazy_catch (key,
				  scm_body_thunk, &c, 
				  scm_handle_by_proc, &handler);
}
#undef FUNC_NAME



/* throwing */

static void toggle_pre_unwind_running (void *data)
{
  struct pre_unwind_data *pre_unwind = (struct pre_unwind_data *)data;
  pre_unwind->running = !pre_unwind->running;
}

SCM_DEFINE (scm_throw, "throw", 1, 0, 1,
           (SCM key, SCM args),
	    "Invoke the catch form matching @var{key}, passing @var{args} to the\n"
	    "@var{handler}.  \n\n"
	    "@var{key} is a symbol.  It will match catches of the same symbol or of\n"
	    "@code{#t}.\n\n"
	    "If there is no handler at all, Guile prints an error and then exits.")
#define FUNC_NAME s_scm_throw
{
  SCM_VALIDATE_SYMBOL (1, key);
  return scm_ithrow (key, args, 1);
}
#undef FUNC_NAME

SCM
scm_ithrow (SCM key, SCM args, int noreturn SCM_UNUSED)
{
  SCM jmpbuf = SCM_UNDEFINED;
  SCM wind_goal;

  SCM dynpair = SCM_UNDEFINED;
  SCM winds;

  if (SCM_I_CURRENT_THREAD->critical_section_level)
    {
      fprintf (stderr, "throw from within critical section.\n");
      abort ();
    }

 rethrow:

  /* Search the wind list for an appropriate catch.
     "Waiter, please bring us the wind list." */
  for (winds = scm_i_dynwinds (); scm_is_pair (winds); winds = SCM_CDR (winds))
    {
      dynpair = SCM_CAR (winds);
      if (scm_is_pair (dynpair))
	{
	  SCM this_key = SCM_CAR (dynpair);

	  if (scm_is_eq (this_key, SCM_BOOL_T) || scm_is_eq (this_key, key))
	    {
	      jmpbuf = SCM_CDR (dynpair);

	      if (!SCM_PRE_UNWIND_DATA_P (jmpbuf))
		break;
	      else
		{
		  struct pre_unwind_data *c =
		    (struct pre_unwind_data *) SCM_CELL_WORD_1 (jmpbuf);
		  if (!c->running)
		    break;
		}
	    }
	}
    }

  /* If we didn't find anything, print a message and abort the process
     right here.  If you don't want this, establish a catch-all around
     any code that might throw up. */
  if (scm_is_null (winds))
    {
      scm_handle_by_message (NULL, key, args);
      abort ();
    }

  /* If the wind list is malformed, bail.  */
  if (!scm_is_pair (winds))
    abort ();
  
  for (wind_goal = scm_i_dynwinds ();
       (!scm_is_pair (SCM_CAR (wind_goal))
	|| !scm_is_eq (SCM_CDAR (wind_goal), jmpbuf));
       wind_goal = SCM_CDR (wind_goal))
    ;

  /* Is this a throw handler (or lazy catch)?  In a wind list entry
     for a throw handler or lazy catch, the key is bound to a
     pre_unwind_data smob, not a jmpbuf.  */
  if (SCM_PRE_UNWIND_DATA_P (jmpbuf))
    {
      struct pre_unwind_data *c =
	(struct pre_unwind_data *) SCM_CELL_WORD_1 (jmpbuf);
      SCM handle, answer;

      /* For old-style lazy-catch behaviour, we unwind the dynamic
	 context before invoking the handler. */
      if (c->lazy_catch_p)
	{
	  scm_dowinds (wind_goal, (scm_ilength (scm_i_dynwinds ())
				   - scm_ilength (wind_goal)));
	  SCM_CRITICAL_SECTION_START;
	  handle = scm_i_dynwinds ();
	  scm_i_set_dynwinds (SCM_CDR (handle));
	  SCM_CRITICAL_SECTION_END;
	}

      /* Call the handler, with framing to set the pre-unwind
	 structure's running field while the handler is running, so we
	 can avoid recursing into the same handler again.  Note that
	 if the handler returns normally, the running flag stays
	 set until some kind of non-local jump occurs. */
      scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
      scm_dynwind_rewind_handler (toggle_pre_unwind_running,
				  c,
				  SCM_F_WIND_EXPLICITLY);
      scm_dynwind_unwind_handler (toggle_pre_unwind_running, c, 0);
      answer = (c->handler) (c->handler_data, key, args);

      /* There is deliberately no scm_dynwind_end call here.  This
	 means that the unwind handler (toggle_pre_unwind_running)
	 stays in place until a non-local exit occurs, and will then
	 reset the pre-unwind structure's running flag.  For sample
	 code where this makes a difference, see the "again but with
	 two chained throw handlers" test case in exceptions.test.  */

      /* If the handler returns, rethrow the same key and args. */
      goto rethrow;
    }

  /* Otherwise, it's a normal catch.  */
  else if (SCM_JMPBUFP (jmpbuf))
    {
      struct pre_unwind_data * pre_unwind;
      struct jmp_buf_and_retval * jbr;

      /* Before unwinding anything, run the pre-unwind handler if
	 there is one, and if it isn't already running. */
      pre_unwind = SCM_JBPREUNWIND (jmpbuf);
      if (pre_unwind->handler && !pre_unwind->running)
	{
	  /* Use framing to detect and avoid possible reentry into
	     this handler, which could otherwise cause an infinite
	     loop. */
	  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
	  scm_dynwind_rewind_handler (toggle_pre_unwind_running,
				      pre_unwind,
				      SCM_F_WIND_EXPLICITLY);
	  scm_dynwind_unwind_handler (toggle_pre_unwind_running,
				      pre_unwind,
				      SCM_F_WIND_EXPLICITLY);
	  (pre_unwind->handler) (pre_unwind->handler_data, key, args);
	  scm_dynwind_end ();
	}

      /* Now unwind and jump. */
      scm_dowinds (wind_goal, (scm_ilength (scm_i_dynwinds ())
			       - scm_ilength (wind_goal)));
      jbr = (struct jmp_buf_and_retval *)JBJMPBUF (jmpbuf);
      jbr->throw_tag = key;
      jbr->retval = args;
      scm_i_set_last_debug_frame (SCM_JBDFRAME (jmpbuf));
      SCM_I_LONGJMP (*JBJMPBUF (jmpbuf), 1);
    }

  /* Otherwise, it's some random piece of junk.  */
  else
    abort ();

#ifdef __ia64__
  /* On IA64, we #define longjmp as setcontext, and GCC appears not to
     know that that doesn't return. */
  return SCM_UNSPECIFIED;
#endif
}


void
scm_init_throw ()
{
  tc16_jmpbuffer = scm_make_smob_type ("jmpbuffer", 0);
  scm_set_smob_print (tc16_jmpbuffer, jmpbuffer_print);

  tc16_pre_unwind_data = scm_make_smob_type ("pre-unwind-data", 0);
  scm_set_smob_print (tc16_pre_unwind_data, pre_unwind_data_print);

#include "libguile/throw.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
