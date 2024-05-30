/* classes: h_files */

#ifndef SCM_INIT_H
#define SCM_INIT_H

/* Copyright (C) 1995,1996,1997,2000, 2006 Free Software Foundation, Inc.
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
#include "libguile/threads.h"


SCM_API scm_i_pthread_mutex_t scm_i_init_mutex;
SCM_API int scm_initialized_p;

SCM_API void scm_init_guile (void);

SCM_API void scm_boot_guile (int argc, char **argv,
			     void (*main_func) (void *closure,
						int argc,
						char **argv),
			     void *closure);

SCM_API void scm_i_init_guile (SCM_STACKITEM *base);

SCM_API void scm_load_startup_files (void);

#endif  /* SCM_INIT_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
