/* classes: h_files */

#ifndef SCM_SCMSIGS_H
#define SCM_SCMSIGS_H

/* Copyright (C) 1995,1996,1997,1998,2000, 2002, 2006 Free Software Foundation, Inc.
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



SCM_API SCM scm_sigaction (SCM signum, SCM handler, SCM flags);
SCM_API SCM scm_sigaction_for_thread (SCM signum, SCM handler, SCM flags,
				      SCM thread);
SCM_API SCM scm_restore_signals (void);
SCM_API SCM scm_alarm (SCM i);
SCM_API SCM scm_setitimer (SCM which_timer,
			   SCM interval_seconds, SCM interval_microseconds,
			   SCM value_seconds, SCM value_microseconds);
SCM_API SCM scm_getitimer (SCM which_timer);
SCM_API SCM scm_pause (void);
SCM_API SCM scm_sleep (SCM i);
SCM_API SCM scm_usleep (SCM i);
SCM_API SCM scm_raise (SCM sig);
SCM_API void scm_init_scmsigs (void);

#endif  /* SCM_SCMSIGS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
