/* classes: h_files */

#ifndef THROWH
#define THROWH
/*	Copyright (C) 1995,1996,1998, 2000 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */


#include "libguile/__scm.h"



typedef SCM (*scm_catch_body_t) (void *data);
typedef SCM (*scm_catch_handler_t) (void *data,
                                    SCM tag, SCM throw_args);

GUILE_API extern SCM scm_internal_catch (SCM tag,
                               scm_catch_body_t body,
                               void *body_data,
                               scm_catch_handler_t handler,
                               void *handler_data);

GUILE_API extern SCM scm_internal_lazy_catch (SCM tag,
                                    scm_catch_body_t body,
                                    void *body_data,
                                    scm_catch_handler_t handler,
                                    void *handler_data);

GUILE_API extern SCM scm_internal_stack_catch (SCM tag,
                                     scm_catch_body_t body,
                                     void *body_data,
                                     scm_catch_handler_t handler,
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

GUILE_API extern SCM scm_body_thunk (void *);


GUILE_API extern SCM scm_handle_by_proc (void *, SCM, SCM);
GUILE_API extern SCM scm_handle_by_proc_catching_all (void *, SCM, SCM);
GUILE_API extern SCM scm_handle_by_message (void *, SCM, SCM);
GUILE_API extern SCM scm_handle_by_message_noexit (void *, SCM, SCM);
GUILE_API extern SCM scm_handle_by_throw (void *, SCM, SCM);
GUILE_API extern int scm_exit_status (SCM args);

GUILE_API extern SCM scm_catch (SCM tag, SCM thunk, SCM handler);
GUILE_API extern SCM scm_lazy_catch (SCM tag, SCM thunk, SCM handler);
GUILE_API extern SCM scm_ithrow (SCM key, SCM args, int noreturn);

GUILE_API extern SCM scm_throw (SCM key, SCM args);
GUILE_API extern void scm_init_throw (void);
#endif  /* THROWH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
