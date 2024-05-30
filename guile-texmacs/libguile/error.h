/* classes: h_files */

#ifndef SCM_ERROR_H
#define SCM_ERROR_H

/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2002, 2006 Free Software Foundation, Inc.
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


SCM_API SCM scm_system_error_key;
SCM_API SCM scm_num_overflow_key;
SCM_API SCM scm_out_of_range_key;
SCM_API SCM scm_args_number_key;
SCM_API SCM scm_arg_type_key;
SCM_API SCM scm_memory_alloc_key;
SCM_API SCM scm_misc_error_key;



SCM_API void scm_error (SCM key, const char *subr, const char *message,
			SCM args, SCM rest) SCM_NORETURN;
SCM_API SCM scm_error_scm (SCM key, SCM subr, SCM message,
			   SCM args, SCM rest) SCM_NORETURN;
SCM_API SCM scm_strerror (SCM err);
SCM_API void scm_syserror (const char *subr) SCM_NORETURN;
SCM_API void scm_syserror_msg (const char *subr, const char *message,
			       SCM args, int eno) SCM_NORETURN;
SCM_API void scm_num_overflow (const char *subr) SCM_NORETURN;
SCM_API void scm_out_of_range (const char *subr, SCM bad_value)
     SCM_NORETURN;
SCM_API void scm_out_of_range_pos (const char *subr, SCM bad_value, SCM pos)
     SCM_NORETURN;
SCM_API void scm_wrong_num_args (SCM proc) SCM_NORETURN;
SCM_API void scm_error_num_args_subr (const char* subr) SCM_NORETURN;
SCM_API void scm_wrong_type_arg (const char *subr, int pos,
				 SCM bad_value) SCM_NORETURN;
SCM_API void scm_wrong_type_arg_msg (const char *subr, int pos,
				     SCM bad_value, const char *sz) SCM_NORETURN;
SCM_API void scm_memory_error (const char *subr) SCM_NORETURN;
SCM_API void scm_misc_error (const char *subr, const char *message,
			     SCM args) SCM_NORETURN;
SCM_API void scm_init_error (void);

#endif  /* SCM_ERROR_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
