/* classes: h_files */

#ifndef SCM_STACKS_H
#define SCM_STACKS_H

/* Copyright (C) 1995,1996,2000,2001, 2004, 2006 Free Software Foundation, Inc.
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

/* {Frames and stacks}
 */

typedef struct scm_t_info_frame {
  /* SCM flags; */
  scm_t_bits flags;
  SCM source;
  SCM proc;
  SCM args;
} scm_t_info_frame;
#define SCM_FRAME_N_SLOTS (sizeof (scm_t_info_frame) / sizeof (SCM))

#define SCM_STACK(obj) ((scm_t_stack *) SCM_STRUCT_DATA (obj))
#define SCM_STACK_LAYOUT "pwuourpW"
typedef struct scm_t_stack {
  SCM id;			/* Stack id */
  scm_t_info_frame *frames;	/* Info frames */
  unsigned long length;		/* Stack length */
  unsigned long tail_length;
  scm_t_info_frame tail[1];
} scm_t_stack;

SCM_API SCM scm_stack_type;

#define SCM_STACKP(obj) (SCM_STRUCTP (obj) && scm_is_eq (SCM_STRUCT_VTABLE (obj), scm_stack_type))
#define SCM_STACK_LENGTH(stack) (SCM_STACK (stack) -> length)

#define SCM_FRAMEP(obj) \
  (scm_is_pair (obj) && SCM_STACKP (SCM_CAR (obj)) \
   && scm_is_unsigned_integer (SCM_CDR (obj), \
                               0, SCM_STACK_LENGTH (SCM_CAR (obj))-1))

#define SCM_FRAME_REF(frame, slot) \
(SCM_STACK (SCM_CAR (frame)) -> frames[scm_to_size_t (SCM_CDR (frame))].slot)

#define SCM_FRAME_NUMBER(frame) \
(SCM_BACKWARDS_P \
 ? scm_to_size_t (SCM_CDR (frame)) \
 : (SCM_STACK_LENGTH (SCM_CAR (frame)) \
    - scm_to_size_t (SCM_CDR (frame)) \
    - 1)) \

#define SCM_FRAME_FLAGS(frame) SCM_FRAME_REF (frame, flags)
#define SCM_FRAME_SOURCE(frame) SCM_FRAME_REF (frame, source)
#define SCM_FRAME_PROC(frame) SCM_FRAME_REF (frame, proc)
#define SCM_FRAME_ARGS(frame) SCM_FRAME_REF (frame, args)
#define SCM_FRAME_PREV(frame) scm_frame_previous (frame)
#define SCM_FRAME_NEXT(frame) scm_frame_next (frame)

#define SCM_FRAMEF_VOID		(1L << 2)
#define SCM_FRAMEF_REAL		(1L << 3)
#define SCM_FRAMEF_PROC 	(1L << 4)
#define SCM_FRAMEF_EVAL_ARGS 	(1L << 5)
#define SCM_FRAMEF_OVERFLOW	(1L << 6)

#define SCM_FRAME_VOID_P(f)       (SCM_FRAME_FLAGS (f) & SCM_FRAMEF_VOID)
#define SCM_FRAME_REAL_P(f)       (SCM_FRAME_FLAGS (f) & SCM_FRAMEF_REAL)
#define SCM_FRAME_PROC_P(f)       (SCM_FRAME_FLAGS (f) & SCM_FRAMEF_PROC)
#define SCM_FRAME_EVAL_ARGS_P(f)  (SCM_FRAME_FLAGS (f) & SCM_FRAMEF_EVAL_ARGS)
#define SCM_FRAME_OVERFLOW_P(f)   (SCM_FRAME_FLAGS (f) & SCM_FRAMEF_OVERFLOW)



SCM_API SCM scm_stack_p (SCM obj);
SCM_API SCM scm_make_stack (SCM obj, SCM args);
SCM_API SCM scm_stack_id (SCM stack);
SCM_API SCM scm_stack_ref (SCM stack, SCM i);
SCM_API SCM scm_stack_length (SCM stack);

SCM_API SCM scm_frame_p (SCM obj);
SCM_API SCM scm_last_stack_frame (SCM obj);
SCM_API SCM scm_frame_number (SCM frame);
SCM_API SCM scm_frame_source (SCM frame);
SCM_API SCM scm_frame_procedure (SCM frame);
SCM_API SCM scm_frame_arguments (SCM frame);
SCM_API SCM scm_frame_previous (SCM frame);
SCM_API SCM scm_frame_next (SCM frame);
SCM_API SCM scm_frame_real_p (SCM frame);
SCM_API SCM scm_frame_procedure_p (SCM frame);
SCM_API SCM scm_frame_evaluating_args_p (SCM frame);
SCM_API SCM scm_frame_overflow_p (SCM frame);

SCM_API void scm_init_stacks (void);

#endif  /* SCM_STACKS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
