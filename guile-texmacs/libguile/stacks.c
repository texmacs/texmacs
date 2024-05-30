/* Representation of stack frame debug information
 * Copyright (C) 1996,1997,2000,2001, 2006, 2007, 2008 Free Software Foundation
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
#include "libguile/eval.h"
#include "libguile/debug.h"
#include "libguile/continuations.h"
#include "libguile/struct.h"
#include "libguile/macros.h"
#include "libguile/procprop.h"
#include "libguile/modules.h"
#include "libguile/root.h"
#include "libguile/strings.h"

#include "libguile/validate.h"
#include "libguile/stacks.h"


/* {Frames and stacks}
 *
 * The debugging evaluator creates debug frames on the stack.  These
 * are linked from the innermost frame and outwards.  The last frame
 * created can always be accessed as SCM_LAST_DEBUG_FRAME.
 * Continuations contain a pointer to the innermost debug frame on the
 * continuation stack.
 *
 * Each debug frame contains a set of flags and information about one
 * or more stack frames.  The case of multiple frames occurs due to
 * tail recursion.  The maximal number of stack frames which can be
 * recorded in one debug frame can be set dynamically with the debug
 * option FRAMES.
 *
 * Stack frame information is of two types: eval information (the
 * expression being evaluated and its environment) and apply
 * information (the procedure being applied and its arguments).  A
 * stack frame normally corresponds to an eval/apply pair, but macros
 * and special forms (which are implemented as macros in Guile) only
 * have eval information and apply calls leads to apply only frames.
 *
 * Since we want to record the total stack information and later
 * manipulate this data at the scheme level in the debugger, we need
 * to transform it into a new representation.  In the following code
 * section you'll find the functions implementing this data type.
 *
 * Representation:
 *
 * The stack is represented as a struct with an id slot and a tail
 * array of scm_t_info_frame structs.
 *
 * A frame is represented as a pair where the car contains a stack and
 * the cdr an inum.  The inum is an index to the first SCM value of
 * the scm_t_info_frame struct.
 *
 * Stacks
 *   Constructor
 *     make-stack
 *   Selectors
 *     stack-id
 *     stack-ref
 *   Inspector
 *     stack-length
 *
 * Frames
 *   Constructor
 *     last-stack-frame
 *   Selectors
 *     frame-number
 *     frame-source
 *     frame-procedure
 *     frame-arguments
 *     frame-previous
 *     frame-next
 *   Predicates
 *     frame-real?
 *     frame-procedure?
 *     frame-evaluating-args?
 *     frame-overflow?  */



/* Some auxiliary functions for reading debug frames off the stack.
 */

/* Stacks often contain pointers to other items on the stack; for
   example, each scm_t_debug_frame structure contains a pointer to the
   next frame out.  When we capture a continuation, we copy the stack
   into the heap, and just leave all the pointers unchanged.  This
   makes it simple to restore the continuation --- just copy the stack
   back!  However, if we retrieve a pointer from the heap copy to
   another item that was originally on the stack, we have to add an
   offset to the pointer to discover the new referent.

   If PTR is a pointer retrieved from a continuation, whose original
   target was on the stack, and OFFSET is the appropriate offset from
   the original stack to the continuation, then RELOC_MUMBLE (PTR,
   OFFSET) is a pointer to the copy in the continuation of the
   original referent, cast to an scm_debug_MUMBLE *.  */
#define RELOC_INFO(ptr, offset) \
  ((scm_t_debug_info *) ((SCM_STACKITEM *) (ptr) + (offset)))
#define RELOC_FRAME(ptr, offset) \
  ((scm_t_debug_frame *) ((SCM_STACKITEM *) (ptr) + (offset)))


/* Count number of debug info frames on a stack, beginning with
 * DFRAME.  OFFSET is used for relocation of pointers when the stack
 * is read from a continuation.
 */
static scm_t_bits
stack_depth (scm_t_debug_frame *dframe, scm_t_ptrdiff offset,
	     SCM *id, int *maxp)
{
  long n;
  long max_depth = SCM_BACKTRACE_MAXDEPTH;
  for (n = 0;
       dframe && !SCM_VOIDFRAMEP (*dframe) && n < max_depth;
       dframe = RELOC_FRAME (dframe->prev, offset))
    {
      if (SCM_EVALFRAMEP (*dframe))
	{
	  scm_t_debug_info *info = RELOC_INFO (dframe->info, offset);
	  scm_t_debug_info *vect = RELOC_INFO (dframe->vect, offset);
	  n += (info - vect) / 2 + 1;
	  /* Data in the apply part of an eval info frame comes from previous
	     stack frame if the scm_t_debug_info vector is overflowed. */
	  if ((((info - vect) & 1) == 0)
	      && SCM_OVERFLOWP (*dframe)
	      && !SCM_UNBNDP (info[1].a.proc))
	    ++n;
	}
      else
	++n;
    }
  if (dframe && SCM_VOIDFRAMEP (*dframe))
    *id = RELOC_INFO(dframe->vect, offset)[0].id;
  else if (dframe)
    *maxp = 1;
  return n;
}

/* Read debug info from DFRAME into IFRAME.
 */
static void
read_frame (scm_t_debug_frame *dframe, scm_t_ptrdiff offset,
	    scm_t_info_frame *iframe)
{
  scm_t_bits flags = SCM_UNPACK (SCM_INUM0); /* UGh. */
  if (SCM_EVALFRAMEP (*dframe))
    {
      scm_t_debug_info *info = RELOC_INFO (dframe->info, offset);
      scm_t_debug_info *vect = RELOC_INFO (dframe->vect, offset);
      if ((info - vect) & 1)
	{
	  /* Debug.vect ends with apply info. */
	  --info;
	  if (!SCM_UNBNDP (info[1].a.proc))
	    {
	      flags |= SCM_FRAMEF_PROC;
	      iframe->proc = info[1].a.proc;
	      iframe->args = info[1].a.args;
	      if (!SCM_ARGS_READY_P (*dframe))
		flags |= SCM_FRAMEF_EVAL_ARGS;
	    }
	}
      iframe->source = scm_make_memoized (info[0].e.exp, info[0].e.env);
    }
  else
    {
      scm_t_debug_info *vect = RELOC_INFO (dframe->vect, offset);
      flags |= SCM_FRAMEF_PROC;
      iframe->proc = vect[0].a.proc;
      iframe->args = vect[0].a.args;
    }
  iframe->flags = flags;
}

/* Look up the first body form of the apply closure.  We'll use this
   below to prevent it from being displayed.
*/
static SCM
get_applybody ()
{
  SCM var = scm_sym2var (scm_sym_apply, SCM_BOOL_F, SCM_BOOL_F);
  if (SCM_VARIABLEP (var) && SCM_CLOSUREP (SCM_VARIABLE_REF (var)))
    return SCM_CAR (SCM_CLOSURE_BODY (SCM_VARIABLE_REF (var)));
  else
    return SCM_UNDEFINED;
}

#define NEXT_FRAME(iframe, n, quit) \
do { \
  if (SCM_MEMOIZEDP (iframe->source) \
      && scm_is_eq (SCM_MEMOIZED_EXP (iframe->source), applybody)) \
    { \
      iframe->source = SCM_BOOL_F; \
      if (scm_is_false (iframe->proc)) \
	{ \
	  --iframe; \
	  ++n; \
	} \
    } \
  ++iframe; \
  if (--n == 0) \
    goto quit; \
} while (0)


/* Fill the scm_t_info_frame vector IFRAME with data from N stack frames
 * starting with the first stack frame represented by debug frame
 * DFRAME.
 */

static scm_t_bits
read_frames (scm_t_debug_frame *dframe, scm_t_ptrdiff offset,
	     long n, scm_t_info_frame *iframes)
{
  scm_t_info_frame *iframe = iframes;
  scm_t_debug_info *info, *vect;
  static SCM applybody = SCM_UNDEFINED;
  
  /* The value of applybody has to be setup after r4rs.scm has executed. */
  if (SCM_UNBNDP (applybody))
    applybody = get_applybody ();
  for (;
       dframe && !SCM_VOIDFRAMEP (*dframe) && n > 0;
       dframe = RELOC_FRAME (dframe->prev, offset))
    {
      read_frame (dframe, offset, iframe);
      if (SCM_EVALFRAMEP (*dframe))
	{
	  /* If current frame is a macro during expansion, we should
	     skip the previously recorded macro transformer
	     application frame.  */
	  if (SCM_MACROEXPP (*dframe) && iframe > iframes)
	    {
	      *(iframe - 1) = *iframe;
	      --iframe;
	    }
	  info =  RELOC_INFO (dframe->info, offset);
	  vect =  RELOC_INFO (dframe->vect, offset);
	  if ((info - vect) & 1)
	    --info;
	  /* Data in the apply part of an eval info frame comes from
	     previous stack frame if the scm_t_debug_info vector is
	     overflowed. */
	  else if (SCM_OVERFLOWP (*dframe)
		   && !SCM_UNBNDP (info[1].a.proc))
	    {
	      NEXT_FRAME (iframe, n, quit);
	      iframe->flags = SCM_UNPACK(SCM_INUM0) | SCM_FRAMEF_PROC;
	      iframe->proc = info[1].a.proc;
	      iframe->args = info[1].a.args;
	    }
	  if (SCM_OVERFLOWP (*dframe))
	    iframe->flags |= SCM_FRAMEF_OVERFLOW;
	  info -= 2;
	  NEXT_FRAME (iframe, n, quit);
	  while (info >= vect)
	    {
	      if (!SCM_UNBNDP (info[1].a.proc))
		{
		  iframe->flags = SCM_UNPACK(SCM_INUM0) | SCM_FRAMEF_PROC;
		  iframe->proc = info[1].a.proc;
		  iframe->args = info[1].a.args;
		}
	      else
		iframe->flags = SCM_UNPACK (SCM_INUM0);
	      iframe->source = scm_make_memoized (info[0].e.exp,
						  info[0].e.env);
	      info -= 2;
	      NEXT_FRAME (iframe, n, quit);
	    }
	}
      else if (scm_is_eq (iframe->proc, scm_f_gsubr_apply))
	/* Skip gsubr apply frames. */
	continue;
      else
	{
	  NEXT_FRAME (iframe, n, quit);
	}
    quit:
      if (iframe > iframes)
	(iframe - 1) -> flags |= SCM_FRAMEF_REAL;
    }
  return iframe - iframes;  /* Number of frames actually read */
}

/* Narrow STACK by cutting away stackframes (mutatingly).
 *
 * Inner frames (most recent) are cut by advancing the frames pointer.
 * Outer frames are cut by decreasing the recorded length.
 *
 * Cut maximally INNER inner frames and OUTER outer frames using
 * the keys INNER_KEY and OUTER_KEY.
 *
 * Frames are cut away starting at the end points and moving towards
 * the center of the stack.  The key is normally compared to the
 * operator in application frames.  Frames up to and including the key
 * are cut.
 *
 * If INNER_KEY is #t a different scheme is used for inner frames:
 *
 * Frames up to but excluding the first source frame originating from
 * a user module are cut, except for possible application frames
 * between the user frame and the last system frame previously
 * encountered.
 */

static void
narrow_stack (SCM stack, long inner, SCM inner_key, long outer, SCM outer_key)
{
  scm_t_stack *s = SCM_STACK (stack);
  unsigned long int i;
  long n = s->length;
  
  /* Cut inner part. */
  if (scm_is_eq (inner_key, SCM_BOOL_T))
    {
      /* Cut all frames up to user module code */
      for (i = 0; inner; ++i, --inner)
	{
	  SCM m = s->frames[i].source;
	  if (SCM_MEMOIZEDP (m)
	      && !SCM_IMP (SCM_MEMOIZED_ENV (m))
	      && scm_is_false (scm_system_module_env_p (SCM_MEMOIZED_ENV (m))))
	    {
	      /* Back up in order to include any non-source frames */
	      while (i > 0)
		{
		  m = s->frames[i - 1].source;
		  if (SCM_MEMOIZEDP (m))
		    break;

		  m = s->frames[i - 1].proc;
		  if (scm_is_true (scm_procedure_p (m))
		      && scm_is_true (scm_procedure_property
				      (m, scm_sym_system_procedure)))
		    break;

		  --i;
		  ++inner;
		}
	      break;
	    }
	}
    }
  else
    /* Use standard cutting procedure. */
    {
      for (i = 0; inner; --inner)
	if (scm_is_eq (s->frames[i++].proc, inner_key))
	  break;
    }
  s->frames = &s->frames[i];
  n -= i;

  /* Cut outer part. */
  for (; n && outer; --outer)
    if (scm_is_eq (s->frames[--n].proc, outer_key))
      break;

  s->length = n;
}



/* Stacks
 */

SCM scm_stack_type;

SCM_DEFINE (scm_stack_p, "stack?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a calling stack.")
#define FUNC_NAME s_scm_stack_p
{
  return scm_from_bool(SCM_STACKP (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_stack, "make-stack", 1, 0, 1, 
            (SCM obj, SCM args),
	    "Create a new stack. If @var{obj} is @code{#t}, the current\n"
	    "evaluation stack is used for creating the stack frames,\n"
	    "otherwise the frames are taken from @var{obj} (which must be\n"
	    "either a debug object or a continuation).\n\n"
	    "@var{args} should be a list containing any combination of\n"
	    "integer, procedure and @code{#t} values.\n\n"
	    "These values specify various ways of cutting away uninteresting\n"
	    "stack frames from the top and bottom of the stack that\n"
	    "@code{make-stack} returns.  They come in pairs like this:\n"
	    "@code{(@var{inner_cut_1} @var{outer_cut_1} @var{inner_cut_2}\n"
	    "@var{outer_cut_2} @dots{})}.\n\n"
	    "Each @var{inner_cut_N} can be @code{#t}, an integer, or a\n"
	    "procedure.  @code{#t} means to cut away all frames up to but\n"
	    "excluding the first user module frame.  An integer means to cut\n"
	    "away exactly that number of frames.  A procedure means to cut\n"
	    "away all frames up to but excluding the application frame whose\n"
	    "procedure matches the specified one.\n\n"
	    "Each @var{outer_cut_N} can be an integer or a procedure.  An\n"
	    "integer means to cut away that number of frames.  A procedure\n"
	    "means to cut away frames down to but excluding the application\n"
	    "frame whose procedure matches the specified one.\n\n"
	    "If the @var{outer_cut_N} of the last pair is missing, it is\n"
	    "taken as 0.")
#define FUNC_NAME s_scm_make_stack
{
  long n, size;
  int maxp;
  scm_t_debug_frame *dframe;
  scm_t_info_frame *iframe;
  long offset = 0;
  SCM stack, id;
  SCM inner_cut, outer_cut;

  /* Extract a pointer to the innermost frame of whatever object
     scm_make_stack was given.  */
  if (scm_is_eq (obj, SCM_BOOL_T))
    {
      dframe = scm_i_last_debug_frame ();
    }
  else if (SCM_DEBUGOBJP (obj))
    {
      dframe = SCM_DEBUGOBJ_FRAME (obj);
    }
  else if (SCM_CONTINUATIONP (obj))
    {
      scm_t_contregs *cont = SCM_CONTREGS (obj);
      offset = cont->offset;
      dframe = RELOC_FRAME (cont->dframe, offset);
    }
  else
    {
      SCM_WRONG_TYPE_ARG (SCM_ARG1, obj);
      /* not reached */
    }

  /* Count number of frames.  Also get stack id tag and check whether
     there are more stackframes than we want to record
     (SCM_BACKTRACE_MAXDEPTH). */
  id = SCM_BOOL_F;
  maxp = 0;
  n = stack_depth (dframe, offset, &id, &maxp);
  size = n * SCM_FRAME_N_SLOTS;

  /* Make the stack object. */
  stack = scm_make_struct (scm_stack_type, scm_from_long (size), SCM_EOL);
  SCM_STACK (stack) -> id = id;
  iframe = &SCM_STACK (stack) -> tail[0];
  SCM_STACK (stack) -> frames = iframe;

  /* Translate the current chain of stack frames into debugging information. */
  n = read_frames (dframe, offset, n, iframe);
  SCM_STACK (stack) -> length = n;

  /* Narrow the stack according to the arguments given to scm_make_stack. */
  SCM_VALIDATE_REST_ARGUMENT (args);
  while (n > 0 && !scm_is_null (args))
    {
      inner_cut = SCM_CAR (args);
      args = SCM_CDR (args);
      if (scm_is_null (args)) 
	{
	  outer_cut = SCM_INUM0;
	} 
      else
	{
	  outer_cut = SCM_CAR (args);
	  args = SCM_CDR (args);
	}
      
      narrow_stack (stack,
		    scm_is_integer (inner_cut) ? scm_to_int (inner_cut) : n,
		    scm_is_integer (inner_cut) ? 0 : inner_cut,
		    scm_is_integer (outer_cut) ? scm_to_int (outer_cut) : n,
		    scm_is_integer (outer_cut) ? 0 : outer_cut);

      n = SCM_STACK (stack) -> length;
    }
  
  if (n > 0)
    {
      if (maxp)
	iframe[n - 1].flags |= SCM_FRAMEF_OVERFLOW;
      return stack;
    }
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_stack_id, "stack-id", 1, 0, 0, 
            (SCM stack),
	    "Return the identifier given to @var{stack} by @code{start-stack}.")
#define FUNC_NAME s_scm_stack_id
{
  scm_t_debug_frame *dframe;
  long offset = 0;
  if (scm_is_eq (stack, SCM_BOOL_T))
    {
      dframe = scm_i_last_debug_frame ();
    }
  else if (SCM_DEBUGOBJP (stack))
    {
      dframe = SCM_DEBUGOBJ_FRAME (stack);
    }
  else if (SCM_CONTINUATIONP (stack))
    {
      scm_t_contregs *cont = SCM_CONTREGS (stack);
      offset = cont->offset;
      dframe = RELOC_FRAME (cont->dframe, offset);
    }
  else if (SCM_STACKP (stack))
    {
      return SCM_STACK (stack) -> id;
    }
  else
    {
      SCM_WRONG_TYPE_ARG (1, stack);
    }

  while (dframe && !SCM_VOIDFRAMEP (*dframe))
    dframe = RELOC_FRAME (dframe->prev, offset);
  if (dframe && SCM_VOIDFRAMEP (*dframe))
    return RELOC_INFO (dframe->vect, offset)[0].id;
  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_stack_ref, "stack-ref", 2, 0, 0,
            (SCM stack, SCM index),
	    "Return the @var{index}'th frame from @var{stack}.")
#define FUNC_NAME s_scm_stack_ref
{
  unsigned long int c_index;

  SCM_VALIDATE_STACK (1, stack);
  c_index = scm_to_unsigned_integer (index, 0, SCM_STACK_LENGTH(stack)-1);
  return scm_cons (stack, index);
}
#undef FUNC_NAME

SCM_DEFINE (scm_stack_length, "stack-length", 1, 0, 0, 
	    (SCM stack),
	    "Return the length of @var{stack}.")
#define FUNC_NAME s_scm_stack_length
{
  SCM_VALIDATE_STACK (1, stack);
  return scm_from_int (SCM_STACK_LENGTH (stack));
}
#undef FUNC_NAME

/* Frames
 */

SCM_DEFINE (scm_frame_p, "frame?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a stack frame.")
#define FUNC_NAME s_scm_frame_p
{
  return scm_from_bool(SCM_FRAMEP (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_last_stack_frame, "last-stack-frame", 1, 0, 0, 
	    (SCM obj),
	    "Return a stack which consists of a single frame, which is the\n"
	    "last stack frame for @var{obj}. @var{obj} must be either a\n"
	    "debug object or a continuation.")
#define FUNC_NAME s_scm_last_stack_frame
{
  scm_t_debug_frame *dframe;
  long offset = 0;
  SCM stack;
  
  if (SCM_DEBUGOBJP (obj))
    {
      dframe = SCM_DEBUGOBJ_FRAME (obj);
    }
  else if (SCM_CONTINUATIONP (obj))
    {
      scm_t_contregs *cont = SCM_CONTREGS (obj);
      offset = cont->offset;
      dframe = RELOC_FRAME (cont->dframe, offset);
    }
  else
    {
      SCM_WRONG_TYPE_ARG (1, obj);
      /* not reached */
    }
  
  if (!dframe || SCM_VOIDFRAMEP (*dframe))
    return SCM_BOOL_F;

  stack = scm_make_struct (scm_stack_type, scm_from_int (SCM_FRAME_N_SLOTS),
			   SCM_EOL);
  SCM_STACK (stack) -> length = 1;
  SCM_STACK (stack) -> frames = &SCM_STACK (stack) -> tail[0];
  read_frame (dframe, offset,
	      (scm_t_info_frame *) &SCM_STACK (stack) -> frames[0]);
  
  return scm_cons (stack, SCM_INUM0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_number, "frame-number", 1, 0, 0, 
	    (SCM frame),
	    "Return the frame number of @var{frame}.")
#define FUNC_NAME s_scm_frame_number
{
  SCM_VALIDATE_FRAME (1, frame);
  return scm_from_int (SCM_FRAME_NUMBER (frame));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_source, "frame-source", 1, 0, 0, 
	    (SCM frame),
	    "Return the source of @var{frame}.")
#define FUNC_NAME s_scm_frame_source
{
  SCM_VALIDATE_FRAME (1, frame);
  return SCM_FRAME_SOURCE (frame);
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_procedure, "frame-procedure", 1, 0, 0, 
	    (SCM frame),
	    "Return the procedure for @var{frame}, or @code{#f} if no\n"
	    "procedure is associated with @var{frame}.")
#define FUNC_NAME s_scm_frame_procedure
{
  SCM_VALIDATE_FRAME (1, frame);
  return (SCM_FRAME_PROC_P (frame)
	  ? SCM_FRAME_PROC (frame)
	  : SCM_BOOL_F);
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_arguments, "frame-arguments", 1, 0, 0, 
	    (SCM frame),
	    "Return the arguments of @var{frame}.")
#define FUNC_NAME s_scm_frame_arguments
{
  SCM_VALIDATE_FRAME (1, frame);
  return SCM_FRAME_ARGS (frame);
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_previous, "frame-previous", 1, 0, 0, 
	    (SCM frame),
	    "Return the previous frame of @var{frame}, or @code{#f} if\n"
	    "@var{frame} is the first frame in its stack.")
#define FUNC_NAME s_scm_frame_previous
{
  unsigned long int n;
  SCM_VALIDATE_FRAME (1, frame);
  n = scm_to_ulong (SCM_CDR (frame)) + 1;
  if (n >= SCM_STACK_LENGTH (SCM_CAR (frame)))
    return SCM_BOOL_F;
  else
    return scm_cons (SCM_CAR (frame), scm_from_ulong (n));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_next, "frame-next", 1, 0, 0, 
           (SCM frame),
	    "Return the next frame of @var{frame}, or @code{#f} if\n"
	    "@var{frame} is the last frame in its stack.")
#define FUNC_NAME s_scm_frame_next
{
  unsigned long int n;
  SCM_VALIDATE_FRAME (1, frame);
  n = scm_to_ulong (SCM_CDR (frame));
  if (n == 0)
    return SCM_BOOL_F;
  else
    return scm_cons (SCM_CAR (frame), scm_from_ulong (n - 1));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_real_p, "frame-real?", 1, 0, 0, 
	    (SCM frame),
	    "Return @code{#t} if @var{frame} is a real frame.")
#define FUNC_NAME s_scm_frame_real_p
{
  SCM_VALIDATE_FRAME (1, frame);
  return scm_from_bool(SCM_FRAME_REAL_P (frame));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_procedure_p, "frame-procedure?", 1, 0, 0, 
	    (SCM frame),
	    "Return @code{#t} if a procedure is associated with @var{frame}.")
#define FUNC_NAME s_scm_frame_procedure_p
{
  SCM_VALIDATE_FRAME (1, frame);
  return scm_from_bool(SCM_FRAME_PROC_P (frame));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_evaluating_args_p, "frame-evaluating-args?", 1, 0, 0, 
	    (SCM frame),
	    "Return @code{#t} if @var{frame} contains evaluated arguments.")
#define FUNC_NAME s_scm_frame_evaluating_args_p
{
  SCM_VALIDATE_FRAME (1, frame);
  return scm_from_bool(SCM_FRAME_EVAL_ARGS_P (frame));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_overflow_p, "frame-overflow?", 1, 0, 0, 
	    (SCM frame),
	    "Return @code{#t} if @var{frame} is an overflow frame.")
#define FUNC_NAME s_scm_frame_overflow_p
{
  SCM_VALIDATE_FRAME (1, frame);
  return scm_from_bool(SCM_FRAME_OVERFLOW_P (frame));
}
#undef FUNC_NAME



void
scm_init_stacks ()
{
  scm_stack_type =
    scm_permanent_object
    (scm_make_vtable (scm_from_locale_string (SCM_STACK_LAYOUT),
                      SCM_UNDEFINED));
  scm_set_struct_vtable_name_x (scm_stack_type,
				scm_from_locale_symbol ("stack"));
#include "libguile/stacks.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
