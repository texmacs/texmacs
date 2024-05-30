/* classes: h_files */

#ifndef SCM_ISELECT_H
#define SCM_ISELECT_H

/* Copyright (C) 1997,1998,2000,2001, 2002, 2006 Free Software Foundation, Inc.
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

/* Needed for FD_SET on some systems.  */
#include <sys/types.h>

#if SCM_HAVE_SYS_SELECT_H
# include <sys/select.h>
#endif

#if SCM_HAVE_WINSOCK2_H
# include <winsock2.h>
#endif

#ifdef FD_SET

#define SELECT_TYPE fd_set
#if defined(__INTERIX) && FD_SETSIZE == 4096
/* Interix defines FD_SETSIZE 4096 but select rejects that. */
#define SELECT_SET_SIZE 1024
#else
#define SELECT_SET_SIZE FD_SETSIZE
#endif

#else /* no FD_SET */

/* Define the macros to access a single-int bitmap of descriptors.  */
#define SELECT_SET_SIZE 32
#define SELECT_TYPE int
#define FD_SET(n, p) (*(p) |= (1 << (n)))
#define FD_CLR(n, p) (*(p) &= ~(1 << (n)))
#define FD_ISSET(n, p) (*(p) & (1 << (n)))
#define FD_ZERO(p) (*(p) = 0)

#endif /* no FD_SET */

SCM_API int scm_std_select (int fds,
			    SELECT_TYPE *rfds,
			    SELECT_TYPE *wfds,
			    SELECT_TYPE *efds,
			    struct timeval *timeout);

#endif  /* SCM_ISELECT_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
