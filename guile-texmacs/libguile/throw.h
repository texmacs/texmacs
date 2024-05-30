/* classes: h_files */

#ifndef SCM_THROW_H
#define SCM_THROW_H

/* Copyright (C) 1995,1996,1998,2000, 2006 Free Software Foundation, Inc.
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



typedef SCM (*scm_t_catch_body) (void *data);
typedef SCM (*scm_t_catch_handler) (void *data,
                                    SCM tag, SCM throw_args);

SCM_API SCM scm_c_catch (SCM tag,
			 scm_t_catch_body body,
			 void *body_data,
			 scm_t_catch_handler handler,
			 void *handler_data,
			 scm_t_catch_handler pre_unwind_handler,
			 void *pre_unwind_handler_data);

SCM_API SCM scm_c_with_throw_handler (SCM tag,
				      scm_t_catch_body body,
				      void *body_data,
				      scm_t_catch_handler handler,
				      void *handler_data,
				      int lazy_catch_p);

SCM_API SCM scm_internal_catch (SCM tag,
				scm_t_catch_body body,
				void *body_data,
				scm_t_catch_handler handler,
				void *handler_data);

SCM_API SCM scm_internal_lazy_catch (SCM tag,
				     scm_t_catch_body body,
				     void *body_data,
				     scm_t_catch_handler handler,
				     void *handler_data);

SCM_API SCM scm_internal_stack_catch (SCM tag,
				      scm_t_catch_body body,
				      void *body_data,
				      scm_t_catch_handler handler,
				      void *handler_data);

/* The first argument to scm_body_thunk should be a pointer to one of
   these.  See the implementation of catch in throw.c.  */
struct scm_body_thunk_data
{
  /* The tag being caught.  We only use it to figure out what
     arguments to pass to the body procedure; see scm_catch_thunk_body for
     details.  */
  SCM tag;

  /* The Scheme procedure object constituting the catch body.
     scm_body_by_proc invokes this.  */
  SCM body_proc;
};

SCM_API SCM scm_body_thunk (void *);


SCM_API SCM scm_handle_by_proc (void *, SCM, SCM);
SCM_API SCM scm_handle_by_proc_catching_all (void *, SCM, SCM);
SCM_API SCM scm_handle_by_message (void *, SCM, SCM);
SCM_API SCM scm_handle_by_message_noexit (void *, SCM, SCM);
SCM_API SCM scm_handle_by_throw (void *, SCM, SCM);
SCM_API int scm_exit_status (SCM args);

SCM_API SCM scm_catch_with_pre_unwind_handler (SCM tag, SCM thunk, SCM handler, SCM lazy_handler);
SCM_API SCM scm_catch (SCM tag, SCM thunk, SCM handler);
SCM_API SCM scm_with_throw_handler (SCM tag, SCM thunk, SCM handler);
SCM_API SCM scm_lazy_catch (SCM tag, SCM thunk, SCM handler);
SCM_API SCM scm_ithrow (SCM key, SCM args, int noreturn);

SCM_API SCM scm_throw (SCM key, SCM args);
SCM_API void scm_init_throw (void);

#endif  /* SCM_THROW_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
