/* classes: h_files */

#ifndef POSIXH
#define POSIXH
/*	Copyright (C) 1995, 1996, 1997, 1998, 2000 Free Software Foundation, Inc.
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






GUILE_API extern SCM scm_tcsetpgrp (SCM port, SCM pgid);
GUILE_API extern SCM scm_tcgetpgrp (SCM port);
GUILE_API extern SCM scm_ctermid (void);
GUILE_API extern SCM scm_setsid (void);
GUILE_API extern SCM scm_setpgid (SCM pid, SCM pgid);
GUILE_API extern SCM scm_pipe (void);
GUILE_API extern SCM scm_getgroups (void);
GUILE_API extern SCM scm_getpgrp (void);
GUILE_API extern SCM scm_getpwuid (SCM user);
GUILE_API extern SCM scm_setpwent (SCM arg);
GUILE_API extern SCM scm_getgrgid (SCM name);
GUILE_API extern SCM scm_setgrent (SCM arg);
GUILE_API extern SCM scm_kill (SCM pid, SCM sig);
GUILE_API extern SCM scm_waitpid (SCM pid, SCM options);
GUILE_API extern SCM scm_status_exit_val (SCM status);
GUILE_API extern SCM scm_status_term_sig (SCM status);
GUILE_API extern SCM scm_status_stop_sig (SCM status);
GUILE_API extern SCM scm_getppid (void);
GUILE_API extern SCM scm_getuid (void);
GUILE_API extern SCM scm_getgid (void);
GUILE_API extern SCM scm_geteuid (void);
GUILE_API extern SCM scm_getegid (void);
GUILE_API extern SCM scm_setuid (SCM id);
GUILE_API extern SCM scm_setgid (SCM id);
GUILE_API extern SCM scm_seteuid (SCM id);
GUILE_API extern SCM scm_setegid (SCM id);
GUILE_API extern SCM scm_ttyname (SCM port);
GUILE_API extern SCM scm_execl (SCM filename, SCM args);
GUILE_API extern SCM scm_execlp (SCM filename, SCM args);
GUILE_API extern SCM scm_execle (SCM filename, SCM env, SCM args);
GUILE_API extern SCM scm_fork (void);
GUILE_API extern SCM scm_uname (void);
GUILE_API extern SCM scm_environ (SCM env);
GUILE_API extern SCM scm_tmpnam (void);
GUILE_API extern SCM scm_open_pipe (SCM pipestr, SCM modes);
GUILE_API extern SCM scm_close_pipe (SCM port);
GUILE_API extern SCM scm_utime (SCM pathname, SCM actime, SCM modtime);
GUILE_API extern SCM scm_access (SCM path, SCM how);
GUILE_API extern SCM scm_getpid (void);
GUILE_API extern SCM scm_putenv (SCM str);
GUILE_API extern SCM scm_setlocale (SCM category, SCM locale);
GUILE_API extern SCM scm_mknod (SCM path, SCM type, SCM perms, SCM dev);
GUILE_API extern SCM scm_nice (SCM incr);
GUILE_API extern SCM scm_sync (void);
GUILE_API extern void scm_init_posix (void);

#endif  /* POSIXH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
