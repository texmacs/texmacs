/* classes: h_files */

#ifndef SCM_RW_H
#define SCM_RW_H

/* Copyright (C) 2001, 2006 Free Software Foundation, Inc.
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

SCM_API SCM scm_read_string_x_partial (SCM str, SCM port_or_fdes, SCM start,
				       SCM end);
SCM_API SCM scm_write_string_partial (SCM str, SCM port_or_fdes, SCM start,
				      SCM end);

SCM_API SCM scm_init_rw_builtins (void);
SCM_API void scm_init_rw (void);

#endif  /* SCM_RW_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
