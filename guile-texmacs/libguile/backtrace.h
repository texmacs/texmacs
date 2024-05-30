/* classes: h_files */

#ifndef SCM_BACKTRACE_H
#define SCM_BACKTRACE_H

/* Copyright (C) 1996,1998,1999,2000,2001, 2004, 2006 Free Software Foundation, Inc.
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

SCM_API SCM scm_the_last_stack_fluid_var;

SCM_API void scm_display_error_message (SCM message, SCM args, SCM port);
SCM_API void scm_i_display_error (SCM stack, SCM port, SCM subr, SCM message, SCM args, SCM rest);
SCM_API SCM scm_display_error (SCM stack, SCM port, SCM subr, SCM message, SCM args, SCM rest);
SCM_API SCM scm_display_application (SCM frame, SCM port, SCM indent);
SCM_API SCM scm_display_backtrace (SCM stack, SCM port, SCM first, SCM depth);
SCM_API SCM scm_display_backtrace_with_highlights (SCM stack, SCM port, SCM first, SCM depth, SCM highlights);
SCM_API SCM scm_backtrace (void);
SCM_API SCM scm_backtrace_with_highlights (SCM highlights);
#ifdef GUILE_DEBUG
SCM_API SCM scm_set_print_params_x (SCM params);
#endif

SCM_API void scm_init_backtrace (void);

#endif  /* SCM_BACKTRACE_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
