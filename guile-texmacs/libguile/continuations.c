/* Copyright (C) 1995,1996,1998,2000,2001,2004, 2006, 2008 Free Software Foundation, Inc.
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

#include <string.h>
#include <stdio.h>

#include "libguile/async.h"
#include "libguile/debug.h"
#include "libguile/root.h"
#include "libguile/stackchk.h"
#include "libguile/smob.h"
#include "libguile/ports.h"
#include "libguile/dynwind.h"
#include "libguile/values.h"
#include "libguile/eval.h"

#include "libguile/validate.h"
#include "libguile/continuations.h"



/* {Continuations}
 */

scm_t_bits scm_tc16_continuation;

static SCM
continuation_mark (SCM obj)
{
  scm_t_contregs *continuation = SCM_CONTREGS (obj);

  scm_gc_mark (continuation->root);
  scm_gc_mark (continuation->throw_value);
  scm_mark_locations (continuation->stack, continuation->num_stack_items);
#ifdef __ia64__
  if (continuation->backing_store)
    scm_mark_locations (continuation->backing_store, 
                        continuation->backing_store_size / 
                        sizeof (SCM_STACKITEM));
#endif /* __ia64__ */
  return continuation->dynenv;
}

static size_t
continuation_free (SCM obj)
{
  scm_t_contregs *continuation = SCM_CONTREGS (obj);
  /* stack array size is 1 if num_stack_items is 0.  */
  size_t extra_items = (continuation->num_stack_items > 0)
    ? (continuation->num_stack_items - 1)
    : 0;
  size_t bytes_free = sizeof (scm_t_contregs)
    + extra_items * sizeof (SCM_STACKITEM);

#ifdef __ia64__
  scm_gc_free (continuation->backing_store, continuation->backing_store_size,
	       "continuation backing store");
#endif /* __ia64__ */ 
  scm_gc_free (continuation, bytes_free, "continuation");
  return 0;
}

static int
continuation_print (SCM obj, SCM port, scm_print_state *state SCM_UNUSED)
{
  scm_t_contregs *continuation = SCM_CONTREGS (obj);

  scm_puts ("#<continuation ", port);
  scm_intprint (continuation->num_stack_items, 10, port);
  scm_puts (" @ ", port);
  scm_uintprint (SCM_CELL_WORD_1 (obj), 16, port);
  scm_putc ('>', port);
  return 1;
}

/* this may return more than once: the first time with the escape
   procedure, then subsequently with the value to be passed to the
   continuation.  */
#define FUNC_NAME "scm_make_continuation"
SCM 
scm_make_continuation (int *first)
{
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;
  SCM cont;
  scm_t_contregs *continuation;
  long stack_size;
  SCM_STACKITEM * src;

  SCM_FLUSH_REGISTER_WINDOWS;
  stack_size = scm_stack_size (thread->continuation_base);
  continuation = scm_gc_malloc (sizeof (scm_t_contregs)
				+ (stack_size - 1) * sizeof (SCM_STACKITEM),
				"continuation");
  continuation->num_stack_items = stack_size;
  continuation->dynenv = scm_i_dynwinds ();
  continuation->throw_value = SCM_EOL;
  continuation->root = thread->continuation_root;
  continuation->dframe = scm_i_last_debug_frame ();
  src = thread->continuation_base;
  SCM_NEWSMOB (cont, scm_tc16_continuation, continuation);

#if ! SCM_STACK_GROWS_UP
  src -= stack_size;
#endif
  continuation->offset = continuation->stack - src;
  memcpy (continuation->stack, src, sizeof (SCM_STACKITEM) * stack_size);

  *first = !SCM_I_SETJMP (continuation->jmpbuf);
  if (*first)
    {
#ifdef __ia64__
      continuation->backing_store_size =
	(char *) scm_ia64_ar_bsp(&continuation->jmpbuf.ctx)
	-
	(char *) thread->register_backing_store_base;
      continuation->backing_store = NULL;
      continuation->backing_store = 
        scm_gc_malloc (continuation->backing_store_size,
		       "continuation backing store");
      memcpy (continuation->backing_store, 
              (void *) thread->register_backing_store_base, 
              continuation->backing_store_size);
#endif /* __ia64__ */
      return cont;
    }
  else
    {
      SCM ret = continuation->throw_value;
      continuation->throw_value = SCM_BOOL_F;
      return ret;
    }
}
#undef FUNC_NAME


/* Invoking a continuation proceeds as follows:
 *
 * - the stack is made large enough for the called continuation
 * - the old windchain is unwound down to the branching point
 * - the continuation stack is copied into place
 * - the windchain is rewound up to the continuation's context
 * - the continuation is invoked via longjmp (or setcontext)
 *
 * This order is important so that unwind and rewind handlers are run
 * with their correct stack.
 */

static void scm_dynthrow (SCM, SCM);

/* Grow the stack by a fixed amount to provide space to copy in the
 * continuation.  Possibly this function has to be called several times
 * recursively before enough space is available.  Make sure the compiler does
 * not optimize the growth array away by storing it's address into a global
 * variable.
 */

scm_t_bits scm_i_dummy;

static void 
grow_stack (SCM cont, SCM val)
{
  scm_t_bits growth[100];

  scm_i_dummy = (scm_t_bits) growth;
  scm_dynthrow (cont, val);
}


/* Copy the continuation stack into the current stack.  Calling functions from
 * within this function is safe, since only stack frames below this function's
 * own frame are overwritten.  Thus, memcpy can be used for best performance.
 */

typedef struct {
  scm_t_contregs *continuation;
  SCM_STACKITEM *dst;
} copy_stack_data;

static void
copy_stack (void *data)
{
  copy_stack_data *d = (copy_stack_data *)data;
  memcpy (d->dst, d->continuation->stack,
	  sizeof (SCM_STACKITEM) * d->continuation->num_stack_items);
#ifdef __ia64__
  SCM_I_CURRENT_THREAD->pending_rbs_continuation = d->continuation;
#endif
}

static void
copy_stack_and_call (scm_t_contregs *continuation, SCM val,
		     SCM_STACKITEM * dst)
{
  long delta;
  copy_stack_data data;

  delta = scm_ilength (scm_i_dynwinds ()) - scm_ilength (continuation->dynenv);
  data.continuation = continuation;
  data.dst = dst;
  scm_i_dowinds (continuation->dynenv, delta, copy_stack, &data);

  scm_i_set_last_debug_frame (continuation->dframe);

  continuation->throw_value = val;
  SCM_I_LONGJMP (continuation->jmpbuf, 1);
}

#ifdef __ia64__
void
scm_ia64_longjmp (scm_i_jmp_buf *JB, int VAL)
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  if (t->pending_rbs_continuation)
    {
      memcpy (t->register_backing_store_base,
	      t->pending_rbs_continuation->backing_store,
	      t->pending_rbs_continuation->backing_store_size);
      t->pending_rbs_continuation = NULL;
    }
  setcontext (&JB->ctx);
}
#endif

/* Call grow_stack until the stack space is large enough, then, as the current
 * stack frame might get overwritten, let copy_stack_and_call perform the
 * actual copying and continuation calling.
 */
static void 
scm_dynthrow (SCM cont, SCM val)
{
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;
  scm_t_contregs *continuation = SCM_CONTREGS (cont);
  SCM_STACKITEM *dst = thread->continuation_base;
  SCM_STACKITEM stack_top_element;

  if (thread->critical_section_level)
    {
      fprintf (stderr, "continuation invoked from within critical section.\n");
      abort ();
    }

#if SCM_STACK_GROWS_UP
  if (dst + continuation->num_stack_items >= &stack_top_element)
    grow_stack (cont, val);
#else
  dst -= continuation->num_stack_items;
  if (dst <= &stack_top_element)
    grow_stack (cont, val);
#endif /* def SCM_STACK_GROWS_UP */

  SCM_FLUSH_REGISTER_WINDOWS;
  copy_stack_and_call (continuation, val, dst);
}


static SCM
continuation_apply (SCM cont, SCM args)
#define FUNC_NAME "continuation_apply"
{
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;
  scm_t_contregs *continuation = SCM_CONTREGS (cont);

  if (continuation->root != thread->continuation_root)
    {
      SCM_MISC_ERROR 
	("invoking continuation would cross continuation barrier: ~A",
	 scm_list_1 (cont));
    }
  
  scm_dynthrow (cont, scm_values (args));
  return SCM_UNSPECIFIED; /* not reached */
}
#undef FUNC_NAME

SCM
scm_i_with_continuation_barrier (scm_t_catch_body body,
				 void *body_data,
				 scm_t_catch_handler handler,
				 void *handler_data,
				 scm_t_catch_handler pre_unwind_handler,
				 void *pre_unwind_handler_data)
{
  SCM_STACKITEM stack_item;
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;
  SCM old_controot;
  SCM_STACKITEM *old_contbase;
  scm_t_debug_frame *old_lastframe;
  SCM result;

  /* Establish a fresh continuation root.  
   */
  old_controot = thread->continuation_root;
  old_contbase = thread->continuation_base;
  old_lastframe = thread->last_debug_frame;
  thread->continuation_root = scm_cons (thread->handle, old_controot);
  thread->continuation_base = &stack_item;
  thread->last_debug_frame = NULL;

  /* Call FUNC inside a catch all.  This is now guaranteed to return
     directly and exactly once.
  */
  result = scm_c_catch (SCM_BOOL_T,
			body, body_data,
			handler, handler_data,
			pre_unwind_handler, pre_unwind_handler_data);

  /* Return to old continuation root.
   */
  thread->last_debug_frame = old_lastframe;
  thread->continuation_base = old_contbase;
  thread->continuation_root = old_controot;

  return result;
}

struct c_data {
  void *(*func) (void *);
  void *data;
  void *result;
};

static SCM
c_body (void *d)
{
  struct c_data *data = (struct c_data *)d;
  data->result = data->func (data->data);
  return SCM_UNSPECIFIED;
}

static SCM
c_handler (void *d, SCM tag, SCM args)
{
  struct c_data *data = (struct c_data *)d;
  data->result = NULL;
  return SCM_UNSPECIFIED;
}

void *
scm_c_with_continuation_barrier (void *(*func) (void *), void *data)
{
  struct c_data c_data;
  c_data.func = func;
  c_data.data = data;
  scm_i_with_continuation_barrier (c_body, &c_data,
				   c_handler, &c_data,
				   scm_handle_by_message_noexit, NULL);
  return c_data.result;
}

struct scm_data {
  SCM proc;
};

static SCM
scm_body (void *d)
{
  struct scm_data *data = (struct scm_data *)d;
  return scm_call_0 (data->proc);
}

static SCM
scm_handler (void *d, SCM tag, SCM args)
{
  return SCM_BOOL_F;
}

SCM_DEFINE (scm_with_continuation_barrier, "with-continuation-barrier", 1,0,0,
	    (SCM proc),
"Call @var{proc} and return its result.  Do not allow the invocation of\n"
"continuations that would leave or enter the dynamic extent of the call\n"
"to @code{with-continuation-barrier}.  Such an attempt causes an error\n"
"to be signaled.\n"
"\n"
"Throws (such as errors) that are not caught from within @var{proc} are\n"
"caught by @code{with-continuation-barrier}.  In that case, a short\n"
"message is printed to the current error port and @code{#f} is returned.\n"
"\n"
"Thus, @code{with-continuation-barrier} returns exactly once.\n")
#define FUNC_NAME s_scm_with_continuation_barrier
{
  struct scm_data scm_data;
  scm_data.proc = proc;
  return scm_i_with_continuation_barrier (scm_body, &scm_data,
					  scm_handler, &scm_data,
					  scm_handle_by_message_noexit, NULL);
}
#undef FUNC_NAME

void
scm_init_continuations ()
{
  scm_tc16_continuation = scm_make_smob_type ("continuation", 0);
  scm_set_smob_mark (scm_tc16_continuation, continuation_mark);
  scm_set_smob_free (scm_tc16_continuation, continuation_free);
  scm_set_smob_print (scm_tc16_continuation, continuation_print);
  scm_set_smob_apply (scm_tc16_continuation, continuation_apply, 0, 0, 1);
#include "libguile/continuations.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
