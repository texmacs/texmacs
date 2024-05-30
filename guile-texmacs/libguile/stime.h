/* classes: h_files */

#ifndef SCM_STIME_H
#define SCM_STIME_H

/* Copyright (C) 1995,1996,1997,1998,2000, 2003, 2006 Free Software Foundation, Inc.
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

#include <unistd.h>  /* for sysconf */



/* This should be figured out by autoconf.

   sysconf(_SC_CLK_TCK) is best, since it's the actual running kernel, not
   some compile-time CLK_TCK.  On glibc 2.3.2 CLK_TCK (when defined) is in
   fact sysconf(_SC_CLK_TCK) anyway.

   CLK_TCK is obsolete in POSIX.  In glibc 2.3.2 it's defined by default,
   but if you define _GNU_SOURCE or _POSIX_C_SOURCE to get other features
   then it goes away.  */

#if ! defined(SCM_TIME_UNITS_PER_SECOND) && defined(_SC_CLK_TCK)
#  define SCM_TIME_UNITS_PER_SECOND ((int) sysconf (_SC_CLK_TCK))
#endif
#if ! defined(SCM_TIME_UNITS_PER_SECOND) && defined(CLK_TCK)
#  define SCM_TIME_UNITS_PER_SECOND ((int) CLK_TCK)
#endif
#if ! defined(SCM_TIME_UNITS_PER_SECOND) && defined(CLOCKS_PER_SEC)
#  define SCM_TIME_UNITS_PER_SECOND ((int) CLOCKS_PER_SEC)
#endif
#if ! defined(SCM_TIME_UNITS_PER_SECOND)
#  define SCM_TIME_UNITS_PER_SECOND 60
#endif


SCM_API long scm_c_get_internal_run_time (void);
SCM_API SCM scm_get_internal_real_time (void);
SCM_API SCM scm_get_internal_run_time (void);
SCM_API SCM scm_current_time (void);
SCM_API SCM scm_gettimeofday (void);
SCM_API SCM scm_localtime (SCM time, SCM zone);
SCM_API SCM scm_gmtime (SCM time);
SCM_API SCM scm_mktime (SCM sbd_time, SCM zone);
SCM_API SCM scm_tzset (void);
SCM_API SCM scm_times (void);
SCM_API SCM scm_strftime (SCM format, SCM stime);
SCM_API SCM scm_strptime (SCM format, SCM string);
SCM_API void scm_init_stime (void);

#endif  /* SCM_STIME_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
