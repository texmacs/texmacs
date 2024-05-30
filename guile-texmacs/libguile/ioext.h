/* classes: h_files */

#ifndef SCM_IOEXT_H
#define SCM_IOEXT_H

/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2006 Free Software Foundation, Inc.
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



SCM_API SCM scm_ftell (SCM object);
SCM_API SCM scm_redirect_port (SCM into_pt, SCM from_pt);
SCM_API SCM scm_dup_to_fdes (SCM fd_or_port, SCM newfd);
SCM_API SCM scm_dup2 (SCM oldfd, SCM newfd);
SCM_API SCM scm_fileno (SCM port);
SCM_API SCM scm_isatty_p (SCM port);
SCM_API SCM scm_fdopen (SCM fdes, SCM modes);
SCM_API SCM scm_primitive_move_to_fdes (SCM port, SCM fd);
SCM_API SCM scm_fdes_to_ports (SCM fd);
SCM_API void scm_init_ioext (void);

#endif  /* SCM_IOEXT_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
