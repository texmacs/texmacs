/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
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



#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include "libguile/_scm.h"
#include "libguile/dynwind.h"
#include "libguile/fports.h"
#include "libguile/scmsigs.h"
#include "libguile/feature.h"
#include "libguile/strings.h"
#include "libguile/srfi-13.h"
#include "libguile/srfi-14.h"
#include "libguile/vectors.h"
#include "libguile/lang.h"

#include "libguile/validate.h"
#include "libguile/posix.h"
#include "libguile/i18n.h"
#include "libguile/threads.h"


#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#else
#ifndef ttyname
extern char *ttyname();
#endif
#endif

#ifdef LIBC_H_WITH_UNISTD_H
#include <libc.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#ifdef HAVE_IO_H
#include <io.h>
#endif
#ifdef HAVE_WINSOCK2_H
#include <winsock2.h>
#endif

#ifdef __MINGW32__
/* Some defines for Windows here. */
# include <process.h>
# define pipe(fd) _pipe (fd, 256, O_BINARY)
#endif /* __MINGW32__ */

#if HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif
#ifndef WEXITSTATUS
# define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif
#ifndef WIFEXITED
# define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif

#include <signal.h>

extern char ** environ;

#ifdef HAVE_GRP_H
#include <grp.h>
#endif
#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif

#ifdef HAVE_SETLOCALE
#include <locale.h>
#endif

#if HAVE_CRYPT_H
#  include <crypt.h>
#endif

#ifdef HAVE_NETDB_H
#include <netdb.h>      /* for MAXHOSTNAMELEN on Solaris */
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>  /* for MAXHOSTNAMELEN */
#endif

#if HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif

#if HAVE_SYS_FILE_H
# include <sys/file.h>
#endif

#if HAVE_CRT_EXTERNS_H
#include <crt_externs.h>  /* for Darwin _NSGetEnviron */
#endif

/* Some Unix systems don't define these.  CPP hair is dangerous, but
   this seems safe enough... */
#ifndef R_OK
#define R_OK 4
#endif

#ifndef W_OK
#define W_OK 2
#endif

#ifndef X_OK
#define X_OK 1
#endif

#ifndef F_OK
#define F_OK 0
#endif

/* No prototype for this on Solaris 10.  The man page says it's in
   <unistd.h> ... but it lies. */
#if ! HAVE_DECL_SETHOSTNAME
int sethostname (char *name, size_t namelen);
#endif

/* On NextStep, <utime.h> doesn't define struct utime, unless we
   #define _POSIX_SOURCE before #including it.  I think this is less
   of a kludge than defining struct utimbuf ourselves.  */
#ifdef UTIMBUF_NEEDS_POSIX
#define _POSIX_SOURCE
#endif

#ifdef HAVE_SYS_UTIME_H
#include <sys/utime.h>
#endif

#ifdef HAVE_UTIME_H
#include <utime.h>
#endif

/* Please don't add any more #includes or #defines here.  The hack
   above means that _POSIX_SOURCE may be #defined, which will
   encourage header files to do strange things.

   FIXME: Maybe should undef _POSIX_SOURCE after it's done its job.

   FIXME: Probably should do all the includes first, then all the fallback
   declarations and defines, in case things are not in the header we
   imagine.  */




/* On Apple Darwin in a shared library there's no "environ" to access
   directly, instead the address of that variable must be obtained with
   _NSGetEnviron().  */
#if HAVE__NSGETENVIRON && defined (PIC)
#define environ (*_NSGetEnviron())
#endif



/* Two often used patterns
 */

#define WITH_STRING(str,cstr,code)             \
  do {                                         \
    char *cstr = scm_to_locale_string (str);   \
    code;                                      \
    free (cstr);                               \
  } while (0)

#define STRING_SYSCALL(str,cstr,code)        \
  do {                                       \
    int eno;                                 \
    char *cstr = scm_to_locale_string (str); \
    SCM_SYSCALL (code);                      \
    eno = errno; free (cstr); errno = eno;   \
  } while (0)



SCM_SYMBOL (sym_read_pipe, "read pipe");
SCM_SYMBOL (sym_write_pipe, "write pipe");

SCM_DEFINE (scm_pipe, "pipe", 0, 0, 0,
            (),
	    "Return a newly created pipe: a pair of ports which are linked\n"
	    "together on the local machine.  The @emph{car} is the input\n"
	    "port and the @emph{cdr} is the output port.  Data written (and\n"
	    "flushed) to the output port can be read from the input port.\n"
	    "Pipes are commonly used for communication with a newly forked\n"
	    "child process.  The need to flush the output port can be\n"
	    "avoided by making it unbuffered using @code{setvbuf}.\n"
	    "\n"
	    "Writes occur atomically provided the size of the data in bytes\n"
	    "is not greater than the value of @code{PIPE_BUF}.  Note that\n"
	    "the output port is likely to block if too much data (typically\n"
	    "equal to @code{PIPE_BUF}) has been written but not yet read\n"
	    "from the input port.")
#define FUNC_NAME s_scm_pipe
{
  int fd[2], rv;
  SCM p_rd, p_wt;

  rv = pipe (fd);
  if (rv)
    SCM_SYSERROR;
  
  p_rd = scm_fdes_to_port (fd[0], "r", sym_read_pipe);
  p_wt = scm_fdes_to_port (fd[1], "w", sym_write_pipe);
  return scm_cons (p_rd, p_wt);
}
#undef FUNC_NAME


#ifdef HAVE_GETGROUPS
SCM_DEFINE (scm_getgroups, "getgroups", 0, 0, 0,
            (),
	    "Return a vector of integers representing the current\n"
	    "supplementary group IDs.")
#define FUNC_NAME s_scm_getgroups
{
  SCM result;
  int ngroups;
  size_t size;
  GETGROUPS_T *groups;

  ngroups = getgroups (0, NULL);
  if (ngroups <= 0)
    SCM_SYSERROR;

  size = ngroups * sizeof (GETGROUPS_T);
  groups = scm_malloc (size);
  ngroups = getgroups (ngroups, groups);

  result = scm_c_make_vector (ngroups, SCM_BOOL_F);
  while (--ngroups >= 0) 
    SCM_SIMPLE_VECTOR_SET (result, ngroups, scm_from_ulong (groups[ngroups]));

  free (groups);
  return result;
}
#undef FUNC_NAME  
#endif

#ifdef HAVE_SETGROUPS
SCM_DEFINE (scm_setgroups, "setgroups", 1, 0, 0,
            (SCM group_vec),
	    "Set the current set of supplementary group IDs to the integers\n"
	    "in the given vector @var{vec}.  The return value is\n"
	    "unspecified.\n"
	    "\n"
	    "Generally only the superuser can set the process group IDs.")
#define FUNC_NAME s_scm_setgroups
{
  size_t ngroups;
  size_t size;
  size_t i;
  int result;
  int save_errno;
  GETGROUPS_T *groups;

  SCM_VALIDATE_VECTOR (SCM_ARG1, group_vec);

  ngroups = SCM_SIMPLE_VECTOR_LENGTH (group_vec);

  /* validate before allocating, so we don't have to worry about leaks */
  for (i = 0; i < ngroups; i++)
    {
      unsigned long ulong_gid;
      GETGROUPS_T gid;
      SCM_VALIDATE_ULONG_COPY (1, SCM_SIMPLE_VECTOR_REF (group_vec, i),
			       ulong_gid);
      gid = ulong_gid;
      if (gid != ulong_gid)
	SCM_OUT_OF_RANGE (1, SCM_SIMPLE_VECTOR_REF (group_vec, i));
    }

  size = ngroups * sizeof (GETGROUPS_T);
  if (size / sizeof (GETGROUPS_T) != ngroups)
    SCM_OUT_OF_RANGE (SCM_ARG1, scm_from_int (ngroups));
  groups = scm_malloc (size);
  for(i = 0; i < ngroups; i++)
    groups [i] = SCM_NUM2ULONG (1, SCM_SIMPLE_VECTOR_REF (group_vec, i));

  result = setgroups (ngroups, groups);
  save_errno = errno; /* don't let free() touch errno */
  free (groups);
  errno = save_errno;
  if (result < 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif

#ifdef HAVE_GETPWENT
SCM_DEFINE (scm_getpwuid, "getpw", 0, 1, 0,
            (SCM user),
	    "Look up an entry in the user database.  @var{obj} can be an integer,\n"
	    "a string, or omitted, giving the behaviour of getpwuid, getpwnam\n"
	    "or getpwent respectively.")
#define FUNC_NAME s_scm_getpwuid
{
  struct passwd *entry;

  SCM result = scm_c_make_vector (7, SCM_UNSPECIFIED);
  if (SCM_UNBNDP (user) || scm_is_false (user))
    {
      SCM_SYSCALL (entry = getpwent ());
      if (! entry)
	{
	  return SCM_BOOL_F;
	}
    }
  else if (scm_is_integer (user))
    {
      entry = getpwuid (scm_to_int (user));
    }
  else
    {
      WITH_STRING (user, c_user,
		   entry = getpwnam (c_user));
    }
  if (!entry)
    SCM_MISC_ERROR ("entry not found", SCM_EOL);

  SCM_SIMPLE_VECTOR_SET(result, 0, scm_from_locale_string (entry->pw_name));
  SCM_SIMPLE_VECTOR_SET(result, 1, scm_from_locale_string (entry->pw_passwd));
  SCM_SIMPLE_VECTOR_SET(result, 2, scm_from_ulong (entry->pw_uid));
  SCM_SIMPLE_VECTOR_SET(result, 3, scm_from_ulong (entry->pw_gid));
  SCM_SIMPLE_VECTOR_SET(result, 4, scm_from_locale_string (entry->pw_gecos));
  if (!entry->pw_dir)
    SCM_SIMPLE_VECTOR_SET(result, 5, scm_from_locale_string (""));
  else
    SCM_SIMPLE_VECTOR_SET(result, 5, scm_from_locale_string (entry->pw_dir));
  if (!entry->pw_shell)
    SCM_SIMPLE_VECTOR_SET(result, 6, scm_from_locale_string (""));
  else
    SCM_SIMPLE_VECTOR_SET(result, 6, scm_from_locale_string (entry->pw_shell));
  return result;
}
#undef FUNC_NAME
#endif /* HAVE_GETPWENT */


#ifdef HAVE_SETPWENT
SCM_DEFINE (scm_setpwent, "setpw", 0, 1, 0,
            (SCM arg),
	    "If called with a true argument, initialize or reset the password data\n"
	    "stream.  Otherwise, close the stream.  The @code{setpwent} and\n"
	    "@code{endpwent} procedures are implemented on top of this.")
#define FUNC_NAME s_scm_setpwent
{
  if (SCM_UNBNDP (arg) || scm_is_false (arg))
    endpwent ();
  else
    setpwent ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif


#ifdef HAVE_GETGRENT
/* Combines getgrgid and getgrnam.  */
SCM_DEFINE (scm_getgrgid, "getgr", 0, 1, 0,
            (SCM name),
	    "Look up an entry in the group database.  @var{obj} can be an integer,\n"
	    "a string, or omitted, giving the behaviour of getgrgid, getgrnam\n"
	    "or getgrent respectively.")
#define FUNC_NAME s_scm_getgrgid
{
  struct group *entry;
  SCM result = scm_c_make_vector (4, SCM_UNSPECIFIED);

  if (SCM_UNBNDP (name) || scm_is_false (name))
    {
      SCM_SYSCALL (entry = getgrent ());
      if (! entry)
	{
	  return SCM_BOOL_F;
	}
    }
  else if (scm_is_integer (name))
    SCM_SYSCALL (entry = getgrgid (scm_to_int (name)));
  else
    STRING_SYSCALL (name, c_name,
		    entry = getgrnam (c_name));
  if (!entry)
    SCM_SYSERROR;

  SCM_SIMPLE_VECTOR_SET(result, 0, scm_from_locale_string (entry->gr_name));
  SCM_SIMPLE_VECTOR_SET(result, 1, scm_from_locale_string (entry->gr_passwd));
  SCM_SIMPLE_VECTOR_SET(result, 2, scm_from_ulong  (entry->gr_gid));
  SCM_SIMPLE_VECTOR_SET(result, 3, scm_makfromstrs (-1, entry->gr_mem));
  return result;
}
#undef FUNC_NAME



SCM_DEFINE (scm_setgrent, "setgr", 0, 1, 0, 
            (SCM arg),
	    "If called with a true argument, initialize or reset the group data\n"
	    "stream.  Otherwise, close the stream.  The @code{setgrent} and\n"
	    "@code{endgrent} procedures are implemented on top of this.")
#define FUNC_NAME s_scm_setgrent
{
  if (SCM_UNBNDP (arg) || scm_is_false (arg))
    endgrent ();
  else
    setgrent ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_GETGRENT */


SCM_DEFINE (scm_kill, "kill", 2, 0, 0,
            (SCM pid, SCM sig),
	    "Sends a signal to the specified process or group of processes.\n\n"
	    "@var{pid} specifies the processes to which the signal is sent:\n\n"
	    "@table @r\n"
	    "@item @var{pid} greater than 0\n"
	    "The process whose identifier is @var{pid}.\n"
	    "@item @var{pid} equal to 0\n"
	    "All processes in the current process group.\n"
	    "@item @var{pid} less than -1\n"
	    "The process group whose identifier is -@var{pid}\n"
	    "@item @var{pid} equal to -1\n"
	    "If the process is privileged, all processes except for some special\n"
	    "system processes.  Otherwise, all processes with the current effective\n"
	    "user ID.\n"
	    "@end table\n\n"
	    "@var{sig} should be specified using a variable corresponding to\n"
	    "the Unix symbolic name, e.g.,\n\n"
	    "@defvar SIGHUP\n"
	    "Hang-up signal.\n"
	    "@end defvar\n\n"
	    "@defvar SIGINT\n"
	    "Interrupt signal.\n"
	    "@end defvar")
#define FUNC_NAME s_scm_kill
{
  /* Signal values are interned in scm_init_posix().  */
#ifdef HAVE_KILL
  if (kill (scm_to_int (pid), scm_to_int  (sig)) != 0)
    SCM_SYSERROR;
#else
  /* Mingw has raise(), but not kill().  (Other raw DOS environments might
     be similar.)  Use raise() when the requested pid is our own process,
     otherwise bomb.  */
  if (scm_to_int (pid) == getpid ())
    {
      if (raise (scm_to_int (sig)) != 0)
        {
        err:
          SCM_SYSERROR;
        }
      else
        {
          errno = ENOSYS;
          goto err;
        }
    }
#endif
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#ifdef HAVE_WAITPID
SCM_DEFINE (scm_waitpid, "waitpid", 1, 1, 0,
            (SCM pid, SCM options),
	    "This procedure collects status information from a child process which\n"
	    "has terminated or (optionally) stopped.  Normally it will\n"
	    "suspend the calling process until this can be done.  If more than one\n"
	    "child process is eligible then one will be chosen by the operating system.\n\n"
	    "The value of @var{pid} determines the behaviour:\n\n"
	    "@table @r\n"
	    "@item @var{pid} greater than 0\n"
	    "Request status information from the specified child process.\n"
	    "@item @var{pid} equal to -1 or WAIT_ANY\n"
	    "Request status information for any child process.\n"
	    "@item @var{pid} equal to 0 or WAIT_MYPGRP\n"
	    "Request status information for any child process in the current process\n"
	    "group.\n"
	    "@item @var{pid} less than -1\n"
	    "Request status information for any child process whose process group ID\n"
	    "is -@var{PID}.\n"
	    "@end table\n\n"
	    "The @var{options} argument, if supplied, should be the bitwise OR of the\n"
	    "values of zero or more of the following variables:\n\n"
	    "@defvar WNOHANG\n"
	    "Return immediately even if there are no child processes to be collected.\n"
	    "@end defvar\n\n"
	    "@defvar WUNTRACED\n"
	    "Report status information for stopped processes as well as terminated\n"
	    "processes.\n"
	    "@end defvar\n\n"
	    "The return value is a pair containing:\n\n"
	    "@enumerate\n"
	    "@item\n"
	    "The process ID of the child process, or 0 if @code{WNOHANG} was\n"
	    "specified and no process was collected.\n"
	    "@item\n"
	    "The integer status value.\n"
	    "@end enumerate")
#define FUNC_NAME s_scm_waitpid
{
  int i;
  int status;
  int ioptions;
  if (SCM_UNBNDP (options))
    ioptions = 0;
  else
    {
      /* Flags are interned in scm_init_posix.  */
      ioptions = scm_to_int (options);
    }
  SCM_SYSCALL (i = waitpid (scm_to_int (pid), &status, ioptions));
  if (i == -1)
    SCM_SYSERROR;
  return scm_cons (scm_from_int (i), scm_from_int (status));
}
#undef FUNC_NAME
#endif /* HAVE_WAITPID */

#ifndef __MINGW32__
SCM_DEFINE (scm_status_exit_val, "status:exit-val", 1, 0, 0, 
            (SCM status),
	    "Return the exit status value, as would be set if a process\n"
	    "ended normally through a call to @code{exit} or @code{_exit},\n"
	    "if any, otherwise @code{#f}.")
#define FUNC_NAME s_scm_status_exit_val
{
  int lstatus;

  /* On Ultrix, the WIF... macros assume their argument is an lvalue;
     go figure.  */
  lstatus = scm_to_int (status);
  if (WIFEXITED (lstatus))
    return (scm_from_int (WEXITSTATUS (lstatus)));
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_status_term_sig, "status:term-sig", 1, 0, 0, 
            (SCM status),
	    "Return the signal number which terminated the process, if any,\n"
	    "otherwise @code{#f}.")
#define FUNC_NAME s_scm_status_term_sig
{
  int lstatus;

  lstatus = scm_to_int (status);
  if (WIFSIGNALED (lstatus))
    return scm_from_int (WTERMSIG (lstatus));
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_status_stop_sig, "status:stop-sig", 1, 0, 0, 
            (SCM status),
	    "Return the signal number which stopped the process, if any,\n"
	    "otherwise @code{#f}.")
#define FUNC_NAME s_scm_status_stop_sig
{
  int lstatus;

  lstatus = scm_to_int (status);
  if (WIFSTOPPED (lstatus))
    return scm_from_int (WSTOPSIG (lstatus));
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME
#endif /* __MINGW32__ */

#ifdef HAVE_GETPPID
SCM_DEFINE (scm_getppid, "getppid", 0, 0, 0,
            (),
	    "Return an integer representing the process ID of the parent\n"
	    "process.")
#define FUNC_NAME s_scm_getppid
{
  return scm_from_int (getppid ());
}
#undef FUNC_NAME
#endif /* HAVE_GETPPID */


#ifndef __MINGW32__
SCM_DEFINE (scm_getuid, "getuid", 0, 0, 0,
            (),
	    "Return an integer representing the current real user ID.")
#define FUNC_NAME s_scm_getuid
{
  return scm_from_int (getuid ());
}
#undef FUNC_NAME



SCM_DEFINE (scm_getgid, "getgid", 0, 0, 0,
            (),
	    "Return an integer representing the current real group ID.")
#define FUNC_NAME s_scm_getgid
{
  return scm_from_int (getgid ());
}
#undef FUNC_NAME



SCM_DEFINE (scm_geteuid, "geteuid", 0, 0, 0,
            (),
	    "Return an integer representing the current effective user ID.\n"
	    "If the system does not support effective IDs, then the real ID\n"
	    "is returned.  @code{(provided? 'EIDs)} reports whether the\n"
	    "system supports effective IDs.")
#define FUNC_NAME s_scm_geteuid
{
#ifdef HAVE_GETEUID
  return scm_from_int (geteuid ());
#else
  return scm_from_int (getuid ());
#endif
}
#undef FUNC_NAME


SCM_DEFINE (scm_getegid, "getegid", 0, 0, 0,
            (),
	    "Return an integer representing the current effective group ID.\n"
	    "If the system does not support effective IDs, then the real ID\n"
	    "is returned.  @code{(provided? 'EIDs)} reports whether the\n"
	    "system supports effective IDs.")
#define FUNC_NAME s_scm_getegid
{
#ifdef HAVE_GETEUID
  return scm_from_int (getegid ());
#else
  return scm_from_int (getgid ());
#endif
}
#undef FUNC_NAME


SCM_DEFINE (scm_setuid, "setuid", 1, 0, 0, 
            (SCM id),
	    "Sets both the real and effective user IDs to the integer @var{id}, provided\n"
	    "the process has appropriate privileges.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_setuid
{
  if (setuid (scm_to_int (id)) != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_setgid, "setgid", 1, 0, 0, 
            (SCM id),
	    "Sets both the real and effective group IDs to the integer @var{id}, provided\n"
	    "the process has appropriate privileges.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_setgid
{
  if (setgid (scm_to_int (id)) != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_seteuid, "seteuid", 1, 0, 0, 
            (SCM id),
	    "Sets the effective user ID to the integer @var{id}, provided the process\n"
	    "has appropriate privileges.  If effective IDs are not supported, the\n"
	    "real ID is set instead -- @code{(provided? 'EIDs)} reports whether the\n"
	    "system supports effective IDs.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_seteuid
{
  int rv;

#ifdef HAVE_SETEUID
  rv = seteuid (scm_to_int (id));
#else
  rv = setuid (scm_to_int (id));
#endif
  if (rv != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* __MINGW32__ */


#ifdef HAVE_SETEGID
SCM_DEFINE (scm_setegid, "setegid", 1, 0, 0, 
            (SCM id),
	    "Sets the effective group ID to the integer @var{id}, provided the process\n"
	    "has appropriate privileges.  If effective IDs are not supported, the\n"
	    "real ID is set instead -- @code{(provided? 'EIDs)} reports whether the\n"
	    "system supports effective IDs.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_setegid
{
  int rv;

#ifdef HAVE_SETEUID
  rv = setegid (scm_to_int (id));
#else
  rv = setgid (scm_to_int (id));
#endif
  if (rv != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
    
}
#undef FUNC_NAME
#endif


#ifdef HAVE_GETPGRP
SCM_DEFINE (scm_getpgrp, "getpgrp", 0, 0, 0,
            (),
	    "Return an integer representing the current process group ID.\n"
	    "This is the POSIX definition, not BSD.")
#define FUNC_NAME s_scm_getpgrp
{
  int (*fn)();
  fn = (int (*) ()) getpgrp;
  return scm_from_int (fn (0));
}
#undef FUNC_NAME
#endif /* HAVE_GETPGRP */


#ifdef HAVE_SETPGID
SCM_DEFINE (scm_setpgid, "setpgid", 2, 0, 0, 
            (SCM pid, SCM pgid),
	    "Move the process @var{pid} into the process group @var{pgid}.  @var{pid} or\n"
	    "@var{pgid} must be integers: they can be zero to indicate the ID of the\n"
	    "current process.\n"
	    "Fails on systems that do not support job control.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_setpgid
{
  /* FIXME(?): may be known as setpgrp.  */
  if (setpgid (scm_to_int (pid), scm_to_int (pgid)) != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_SETPGID */

#ifdef HAVE_SETSID
SCM_DEFINE (scm_setsid, "setsid", 0, 0, 0,
            (),
	    "Creates a new session.  The current process becomes the session leader\n"
	    "and is put in a new process group.  The process will be detached\n"
	    "from its controlling terminal if it has one.\n"
	    "The return value is an integer representing the new process group ID.")
#define FUNC_NAME s_scm_setsid
{
  pid_t sid = setsid ();
  if (sid == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_SETSID */


/* ttyname returns its result in a single static buffer, hence
   scm_i_misc_mutex for thread safety.  In glibc 2.3.2 two threads
   continuously calling ttyname will otherwise get an overwrite quite
   easily.

   ttyname_r (when available) could be used instead of scm_i_misc_mutex, but
   there's probably little to be gained in either speed or parallelism.  */

#ifdef HAVE_TTYNAME
SCM_DEFINE (scm_ttyname, "ttyname", 1, 0, 0, 
            (SCM port),
	    "Return a string with the name of the serial terminal device\n"
	    "underlying @var{port}.")
#define FUNC_NAME s_scm_ttyname
{
  char *result;
  int fd, err;
  SCM ret = SCM_BOOL_F;

  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPPORT (1, port);
  if (!SCM_FPORTP (port))
    return SCM_BOOL_F;
  fd = SCM_FPORT_FDES (port);

  scm_i_scm_pthread_mutex_lock (&scm_i_misc_mutex);

  SCM_SYSCALL (result = ttyname (fd));
  err = errno;
  if (result != NULL)
    result = strdup (result);

  scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);

  if (!result)
    {
      errno = err;
      SCM_SYSERROR;
    }
  else
    ret = scm_take_locale_string (result);

  return ret;
}
#undef FUNC_NAME
#endif /* HAVE_TTYNAME */


/* For thread safety "buf" is used instead of NULL for the ctermid static
   buffer.  Actually it's unlikely the controlling terminal will change
   during program execution, and indeed on glibc (2.3.2) it's always just
   "/dev/tty", but L_ctermid on the stack is easy and fast and guarantees
   safety everywhere.  */
#ifdef HAVE_CTERMID
SCM_DEFINE (scm_ctermid, "ctermid", 0, 0, 0,
            (),
	    "Return a string containing the file name of the controlling\n"
	    "terminal for the current process.")
#define FUNC_NAME s_scm_ctermid
{
  char buf[L_ctermid];
  char *result = ctermid (buf);
  if (*result == '\0')
    SCM_SYSERROR;
  return scm_from_locale_string (result);
}
#undef FUNC_NAME
#endif /* HAVE_CTERMID */

#ifdef HAVE_TCGETPGRP
SCM_DEFINE (scm_tcgetpgrp, "tcgetpgrp", 1, 0, 0, 
            (SCM port),
	    "Return the process group ID of the foreground process group\n"
	    "associated with the terminal open on the file descriptor\n"
	    "underlying @var{port}.\n"
	    "\n"
	    "If there is no foreground process group, the return value is a\n"
	    "number greater than 1 that does not match the process group ID\n"
	    "of any existing process group.  This can happen if all of the\n"
	    "processes in the job that was formerly the foreground job have\n"
	    "terminated, and no other job has yet been moved into the\n"
	    "foreground.")
#define FUNC_NAME s_scm_tcgetpgrp
{
  int fd;
  pid_t pgid;

  port = SCM_COERCE_OUTPORT (port);

  SCM_VALIDATE_OPFPORT (1, port);
  fd = SCM_FPORT_FDES (port);
  if ((pgid = tcgetpgrp (fd)) == -1)
    SCM_SYSERROR;
  return scm_from_int (pgid);
}
#undef FUNC_NAME    
#endif /* HAVE_TCGETPGRP */

#ifdef HAVE_TCSETPGRP
SCM_DEFINE (scm_tcsetpgrp, "tcsetpgrp", 2, 0, 0,
            (SCM port, SCM pgid),
	    "Set the foreground process group ID for the terminal used by the file\n"
	    "descriptor underlying @var{port} to the integer @var{pgid}.\n"
	    "The calling process\n"
	    "must be a member of the same session as @var{pgid} and must have the same\n"
	    "controlling terminal.  The return value is unspecified.")
#define FUNC_NAME s_scm_tcsetpgrp
{
  int fd;

  port = SCM_COERCE_OUTPORT (port);

  SCM_VALIDATE_OPFPORT (1, port);
  fd = SCM_FPORT_FDES (port);
  if (tcsetpgrp (fd, scm_to_int (pgid)) == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_TCSETPGRP */

static void
free_string_pointers (void *data)
{
  scm_i_free_string_pointers ((char **)data);
}

SCM_DEFINE (scm_execl, "execl", 1, 0, 1, 
            (SCM filename, SCM args),
	    "Executes the file named by @var{path} as a new process image.\n"
	    "The remaining arguments are supplied to the process; from a C program\n"
	    "they are accessible as the @code{argv} argument to @code{main}.\n"
	    "Conventionally the first @var{arg} is the same as @var{path}.\n"
	    "All arguments must be strings.\n\n"
	    "If @var{arg} is missing, @var{path} is executed with a null\n"
	    "argument list, which may have system-dependent side-effects.\n\n"
	    "This procedure is currently implemented using the @code{execv} system\n"
	    "call, but we call it @code{execl} because of its Scheme calling interface.")
#define FUNC_NAME s_scm_execl
{
  char *exec_file;
  char **exec_argv;

  scm_dynwind_begin (0);

  exec_file = scm_to_locale_string (filename);
  scm_dynwind_free (exec_file);

  exec_argv = scm_i_allocate_string_pointers (args);
  scm_dynwind_unwind_handler (free_string_pointers, exec_argv, 
			    SCM_F_WIND_EXPLICITLY);

  execv (exec_file,
#ifdef __MINGW32__
         /* extra "const" in mingw formals, provokes warning from gcc */
         (const char * const *)
#endif
         exec_argv);
  SCM_SYSERROR;

  /* not reached.  */
  scm_dynwind_end ();
  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_execlp, "execlp", 1, 0, 1, 
            (SCM filename, SCM args),
	    "Similar to @code{execl}, however if\n"
	    "@var{filename} does not contain a slash\n"
	    "then the file to execute will be located by searching the\n"
	    "directories listed in the @code{PATH} environment variable.\n\n"
	    "This procedure is currently implemented using the @code{execvp} system\n"
	    "call, but we call it @code{execlp} because of its Scheme calling interface.")
#define FUNC_NAME s_scm_execlp
{
  char *exec_file;
  char **exec_argv;

  scm_dynwind_begin (0);

  exec_file = scm_to_locale_string (filename);
  scm_dynwind_free (exec_file);

  exec_argv = scm_i_allocate_string_pointers (args);
  scm_dynwind_unwind_handler (free_string_pointers, exec_argv, 
			    SCM_F_WIND_EXPLICITLY);

  execvp (exec_file,
#ifdef __MINGW32__
          /* extra "const" in mingw formals, provokes warning from gcc */
          (const char * const *)
#endif
          exec_argv);
  SCM_SYSERROR;

  /* not reached.  */
  scm_dynwind_end ();
  return SCM_BOOL_F;
}
#undef FUNC_NAME


/* OPTIMIZE-ME: scm_execle doesn't need malloced copies of the environment
   list strings the way environ_list_to_c gives.  */

SCM_DEFINE (scm_execle, "execle", 2, 0, 1, 
            (SCM filename, SCM env, SCM args),
	    "Similar to @code{execl}, but the environment of the new process is\n"
	    "specified by @var{env}, which must be a list of strings as returned by the\n"
	    "@code{environ} procedure.\n\n"
	    "This procedure is currently implemented using the @code{execve} system\n"
	    "call, but we call it @code{execle} because of its Scheme calling interface.")
#define FUNC_NAME s_scm_execle
{
  char **exec_argv;
  char **exec_env;
  char *exec_file;

  scm_dynwind_begin (0);

  exec_file = scm_to_locale_string (filename);
  scm_dynwind_free (exec_file);

  exec_argv = scm_i_allocate_string_pointers (args);
  scm_dynwind_unwind_handler (free_string_pointers, exec_argv,
			    SCM_F_WIND_EXPLICITLY);

  exec_env = scm_i_allocate_string_pointers (env);
  scm_dynwind_unwind_handler (free_string_pointers, exec_env,
			    SCM_F_WIND_EXPLICITLY);

  execve (exec_file,
#ifdef __MINGW32__
          /* extra "const" in mingw formals, provokes warning from gcc */
          (const char * const *)
#endif
          exec_argv,
#ifdef __MINGW32__
          /* extra "const" in mingw formals, provokes warning from gcc */
          (const char * const *)
#endif
          exec_env);
  SCM_SYSERROR;

  /* not reached.  */
  scm_dynwind_end ();
  return SCM_BOOL_F;
}
#undef FUNC_NAME

#ifdef HAVE_FORK
SCM_DEFINE (scm_fork, "primitive-fork", 0, 0, 0,
            (),
	    "Creates a new \"child\" process by duplicating the current \"parent\" process.\n"
	    "In the child the return value is 0.  In the parent the return value is\n"
	    "the integer process ID of the child.\n\n"
	    "This procedure has been renamed from @code{fork} to avoid a naming conflict\n"
	    "with the scsh fork.")
#define FUNC_NAME s_scm_fork
{
  int pid;
  pid = fork ();
  if (pid == -1)
    SCM_SYSERROR;
  return scm_from_int (pid);
}
#undef FUNC_NAME
#endif /* HAVE_FORK */

#ifdef __MINGW32__
# include "win32-uname.h"
#endif

#if defined (HAVE_UNAME) || defined (__MINGW32__)
SCM_DEFINE (scm_uname, "uname", 0, 0, 0,
            (),
	    "Return an object with some information about the computer\n"
	    "system the program is running on.")
#define FUNC_NAME s_scm_uname
{
  struct utsname buf;
  SCM result = scm_c_make_vector (5, SCM_UNSPECIFIED);
  if (uname (&buf) < 0)
    SCM_SYSERROR;
  SCM_SIMPLE_VECTOR_SET(result, 0, scm_from_locale_string (buf.sysname));
  SCM_SIMPLE_VECTOR_SET(result, 1, scm_from_locale_string (buf.nodename));
  SCM_SIMPLE_VECTOR_SET(result, 2, scm_from_locale_string (buf.release));
  SCM_SIMPLE_VECTOR_SET(result, 3, scm_from_locale_string (buf.version));
  SCM_SIMPLE_VECTOR_SET(result, 4, scm_from_locale_string (buf.machine));
/* 
   a linux special?
  SCM_SIMPLE_VECTOR_SET(result, 5, scm_from_locale_string (buf.domainname));
*/
  return result;
}
#undef FUNC_NAME
#endif /* HAVE_UNAME */

SCM_DEFINE (scm_environ, "environ", 0, 1, 0, 
            (SCM env),
	    "If @var{env} is omitted, return the current environment (in the\n"
	    "Unix sense) as a list of strings.  Otherwise set the current\n"
	    "environment, which is also the default environment for child\n"
	    "processes, to the supplied list of strings.  Each member of\n"
	    "@var{env} should be of the form @code{NAME=VALUE} and values of\n"
	    "@code{NAME} should not be duplicated.  If @var{env} is supplied\n"
	    "then the return value is unspecified.")
#define FUNC_NAME s_scm_environ
{
  if (SCM_UNBNDP (env))
    return scm_makfromstrs (-1, environ);
  else
    {
      char **new_environ;

      new_environ = scm_i_allocate_string_pointers (env);
      /* Free the old environment, except when called for the first
       * time.
       */
      {
	static int first = 1;
	if (!first)
	  scm_i_free_string_pointers (environ);
	first = 0;
      }
      environ = new_environ;
      return SCM_UNSPECIFIED;
    }
}
#undef FUNC_NAME

#ifdef L_tmpnam

SCM_DEFINE (scm_tmpnam, "tmpnam", 0, 0, 0,
            (),
	    "Return a name in the file system that does not match any\n"
	    "existing file.  However there is no guarantee that another\n"
	    "process will not create the file after @code{tmpnam} is called.\n"
	    "Care should be taken if opening the file, e.g., use the\n"
	    "@code{O_EXCL} open flag or use @code{mkstemp!} instead.")
#define FUNC_NAME s_scm_tmpnam
{
  char name[L_tmpnam];
  char *rv;

  SCM_SYSCALL (rv = tmpnam (name));
  if (rv == NULL)
    /* not SCM_SYSERROR since errno probably not set.  */
    SCM_MISC_ERROR ("tmpnam failed", SCM_EOL);
  return scm_from_locale_string (name);
}
#undef FUNC_NAME

#endif

#ifndef HAVE_MKSTEMP
extern int mkstemp (char *);
#endif

SCM_DEFINE (scm_mkstemp, "mkstemp!", 1, 0, 0,
	    (SCM tmpl),
	    "Create a new unique file in the file system and return a new\n"
	    "buffered port open for reading and writing to the file.\n"
	    "\n"
	    "@var{tmpl} is a string specifying where the file should be\n"
	    "created: it must end with @samp{XXXXXX} and those @samp{X}s\n"
	    "will be changed in the string to return the name of the file.\n"
	    "(@code{port-filename} on the port also gives the name.)\n"
	    "\n"
	    "POSIX doesn't specify the permissions mode of the file, on GNU\n"
	    "and most systems it's @code{#o600}.  An application can use\n"
	    "@code{chmod} to relax that if desired.  For example\n"
	    "@code{#o666} less @code{umask}, which is usual for ordinary\n"
	    "file creation,\n"
	    "\n"
	    "@example\n"
	    "(let ((port (mkstemp! (string-copy \"/tmp/myfile-XXXXXX\"))))\n"
	    "  (chmod port (logand #o666 (lognot (umask))))\n"
	    "  ...)\n"
	    "@end example")
#define FUNC_NAME s_scm_mkstemp
{
  char *c_tmpl;
  int rv;
  
  scm_dynwind_begin (0);

  c_tmpl = scm_to_locale_string (tmpl);
  scm_dynwind_free (c_tmpl);

  SCM_SYSCALL (rv = mkstemp (c_tmpl));
  if (rv == -1)
    SCM_SYSERROR;

  scm_substring_move_x (scm_from_locale_string (c_tmpl),
			SCM_INUM0, scm_string_length (tmpl),
			tmpl, SCM_INUM0);

  scm_dynwind_end ();
  return scm_fdes_to_port (rv, "w+", tmpl);
}
#undef FUNC_NAME

SCM_DEFINE (scm_utime, "utime", 1, 2, 0,
            (SCM pathname, SCM actime, SCM modtime),
	    "@code{utime} sets the access and modification times for the\n"
	    "file named by @var{path}.  If @var{actime} or @var{modtime} is\n"
	    "not supplied, then the current time is used.  @var{actime} and\n"
	    "@var{modtime} must be integer time values as returned by the\n"
	    "@code{current-time} procedure.\n"
	    "@lisp\n"
	    "(utime \"foo\" (- (current-time) 3600))\n"
	    "@end lisp\n"
	    "will set the access time to one hour in the past and the\n"
	    "modification time to the current time.")
#define FUNC_NAME s_scm_utime
{
  int rv;
  struct utimbuf utm_tmp;

  if (SCM_UNBNDP (actime))
    SCM_SYSCALL (time (&utm_tmp.actime));
  else
    utm_tmp.actime = SCM_NUM2ULONG (2, actime);

  if (SCM_UNBNDP (modtime))
    SCM_SYSCALL (time (&utm_tmp.modtime));
  else
    utm_tmp.modtime = SCM_NUM2ULONG (3, modtime);

  STRING_SYSCALL (pathname, c_pathname,
		  rv = utime (c_pathname, &utm_tmp));
  if (rv != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_access, "access?", 2, 0, 0,
            (SCM path, SCM how),
	    "Test accessibility of a file under the real UID and GID of the\n"
	    "calling process.  The return is @code{#t} if @var{path} exists\n"
	    "and the permissions requested by @var{how} are all allowed, or\n"
	    "@code{#f} if not.\n"
	    "\n"
	    "@var{how} is an integer which is one of the following values,\n"
	    "or a bitwise-OR (@code{logior}) of multiple values.\n"
	    "\n"
	    "@defvar R_OK\n"
	    "Test for read permission.\n"
	    "@end defvar\n"
	    "@defvar W_OK\n"
	    "Test for write permission.\n"
	    "@end defvar\n"
	    "@defvar X_OK\n"
	    "Test for execute permission.\n"
	    "@end defvar\n"
	    "@defvar F_OK\n"
	    "Test for existence of the file.  This is implied by each of the\n"
	    "other tests, so there's no need to combine it with them.\n"
	    "@end defvar\n"
	    "\n"
	    "It's important to note that @code{access?} does not simply\n"
	    "indicate what will happen on attempting to read or write a\n"
	    "file.  In normal circumstances it does, but in a set-UID or\n"
	    "set-GID program it doesn't because @code{access?} tests the\n"
	    "real ID, whereas an open or execute attempt uses the effective\n"
	    "ID.\n"
	    "\n"
	    "A program which will never run set-UID/GID can ignore the\n"
	    "difference between real and effective IDs, but for maximum\n"
	    "generality, especially in library functions, it's best not to\n"
	    "use @code{access?} to predict the result of an open or execute,\n"
	    "instead simply attempt that and catch any exception.\n"
	    "\n"
	    "The main use for @code{access?} is to let a set-UID/GID program\n"
	    "determine what the invoking user would have been allowed to do,\n"
	    "without the greater (or perhaps lesser) privileges afforded by\n"
	    "the effective ID.  For more on this, see ``Testing File\n"
	    "Access'' in The GNU C Library Reference Manual.")
#define FUNC_NAME s_scm_access
{
  int rv;

  WITH_STRING (path, c_path,
	       rv = access (c_path, scm_to_int (how)));
  return scm_from_bool (!rv);
}
#undef FUNC_NAME

SCM_DEFINE (scm_getpid, "getpid", 0, 0, 0,
            (),
	    "Return an integer representing the current process ID.")
#define FUNC_NAME s_scm_getpid
{
  return scm_from_ulong (getpid ());
}
#undef FUNC_NAME

SCM_DEFINE (scm_putenv, "putenv", 1, 0, 0, 
            (SCM str),
	    "Modifies the environment of the current process, which is\n"
	    "also the default environment inherited by child processes.\n\n"
	    "If @var{string} is of the form @code{NAME=VALUE} then it will be written\n"
	    "directly into the environment, replacing any existing environment string\n"
	    "with\n"
	    "name matching @code{NAME}.  If @var{string} does not contain an equal\n"
	    "sign, then any existing string with name matching @var{string} will\n"
	    "be removed.\n\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_putenv
{
  int rv;
  char *c_str = scm_to_locale_string (str);

  if (strchr (c_str, '=') == NULL)
    {
      /* We want no "=" in the argument to mean remove the variable from the
	 environment, but not all putenv()s understand this, for example
	 FreeBSD 4.8 doesn't.  Getting it happening everywhere is a bit
	 painful.  When unsetenv() exists, we use that, of course.

         Traditionally putenv("NAME") removes a variable, for example that's
         what we have to do on Solaris 9 (it doesn't have an unsetenv).

         But on DOS and on that DOS overlay manager thing called W-whatever,
         putenv("NAME=") must be used (it too doesn't have an unsetenv).

         Supposedly on AIX a putenv("NAME") could cause a segfault, but also
         supposedly AIX 5.3 and up has unsetenv() available so should be ok
         with the latter there.

         For the moment we hard code the DOS putenv("NAME=") style under
         __MINGW32__ and do the traditional everywhere else.  Such
         system-name tests are bad, of course.  It'd be possible to use a
         configure test when doing a a native build.  For example GNU R has
         such a test (see R_PUTENV_AS_UNSETENV in
         https://svn.r-project.org/R/trunk/m4/R.m4).  But when cross
         compiling there'd want to be a guess, one probably based on the
         system name (ie. mingw or not), thus landing back in basically the
         present hard-coded situation.  Another possibility for a cross
         build would be to try "NAME" then "NAME=" at runtime, if that's not
         too much like overkill.  */

#if defined HAVE_UNSETENV && HAVE_DECL_UNSETENV
      /* when unsetenv() exists then we use it */
      unsetenv (c_str);
      free (c_str);
#elif defined (__MINGW32__)
      /* otherwise putenv("NAME=") on DOS */
      int e;
      size_t len = strlen (c_str);
      char *ptr = scm_malloc (len + 2);
      strcpy (ptr, c_str);
      strcpy (ptr+len, "=");
      rv = putenv (ptr);
      e = errno; free (ptr); free (c_str); errno = e;
      if (rv < 0)
	SCM_SYSERROR;
#else
      /* otherwise traditional putenv("NAME") */
      rv = putenv (c_str);
      if (rv < 0)
	SCM_SYSERROR;
#endif
    }
  else
    {
#ifdef __MINGW32__
      /* If str is "FOO=", ie. attempting to set an empty string, then
         we need to see if it's been successful.  On MINGW, "FOO="
         means remove FOO from the environment.  As a workaround, we
         set "FOO= ", ie. a space, and then modify the string returned
         by getenv.  It's not enough just to modify the string we set,
         because MINGW putenv copies it.  */

      {
        size_t len = strlen (c_str);
        if (c_str[len-1] == '=')
          {
            char *ptr = scm_malloc (len+2);
            strcpy (ptr, c_str);
            strcpy (ptr+len, " ");
            rv = putenv (ptr);
            if (rv < 0)
              {
                int eno = errno;
                free (c_str);
                errno = eno;
                SCM_SYSERROR;
              }
            /* truncate to just the name */
            c_str[len-1] = '\0';
            ptr = getenv (c_str);
            if (ptr)
              ptr[0] = '\0';
            return SCM_UNSPECIFIED;
          }
      }
#endif /* __MINGW32__ */

      /* Leave c_str in the environment.  */

      rv = putenv (c_str);
      if (rv < 0)
	SCM_SYSERROR;
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#ifdef HAVE_SETLOCALE
SCM_DEFINE (scm_setlocale, "setlocale", 1, 1, 0,
            (SCM category, SCM locale),
	    "If @var{locale} is omitted, return the current value of the\n"
	    "specified locale category as a system-dependent string.\n"
	    "@var{category} should be specified using the values\n"
	    "@code{LC_COLLATE}, @code{LC_ALL} etc.\n"
	    "\n"
	    "Otherwise the specified locale category is set to the string\n"
	    "@var{locale} and the new value is returned as a\n"
	    "system-dependent string.  If @var{locale} is an empty string,\n"
	    "the locale will be set using environment variables.")
#define FUNC_NAME s_scm_setlocale
{
  char *clocale;
  char *rv;

  scm_dynwind_begin (0);

  if (SCM_UNBNDP (locale))
    {
      clocale = NULL;
    }
  else
    {
      clocale = scm_to_locale_string (locale);
      scm_dynwind_free (clocale);
    }

  rv = setlocale (scm_i_to_lc_category (category, 1), clocale);
  if (rv == NULL)
    {
      /* POSIX and C99 don't say anything about setlocale setting errno, so
         force a sensible value here.  glibc leaves ENOENT, which would be
         fine, but it's not a documented feature.  */
      errno = EINVAL;
      SCM_SYSERROR;
    }

  /* Recompute the standard SRFI-14 character sets in a locale-dependent
     (actually charset-dependent) way.  */
  scm_srfi_14_compute_char_sets ();

  scm_dynwind_end ();
  return scm_from_locale_string (rv);
}
#undef FUNC_NAME
#endif /* HAVE_SETLOCALE */

#ifdef HAVE_MKNOD
SCM_DEFINE (scm_mknod, "mknod", 4, 0, 0,
            (SCM path, SCM type, SCM perms, SCM dev),
	    "Creates a new special file, such as a file corresponding to a device.\n"
	    "@var{path} specifies the name of the file.  @var{type} should\n"
	    "be one of the following symbols:\n"
	    "regular, directory, symlink, block-special, char-special,\n"
	    "fifo, or socket.  @var{perms} (an integer) specifies the file permissions.\n"
	    "@var{dev} (an integer) specifies which device the special file refers\n"
	    "to.  Its exact interpretation depends on the kind of special file\n"
	    "being created.\n\n"
	    "E.g.,\n"
	    "@lisp\n"
	    "(mknod \"/dev/fd0\" 'block-special #o660 (+ (* 2 256) 2))\n"
	    "@end lisp\n\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_mknod
{
  int val;
  const char *p;
  int ctype = 0;

  SCM_VALIDATE_STRING (1, path);
  SCM_VALIDATE_SYMBOL (2, type);

  p = scm_i_symbol_chars (type);
  if (strcmp (p, "regular") == 0)
    ctype = S_IFREG;
  else if (strcmp (p, "directory") == 0)
    ctype = S_IFDIR;
#ifdef S_IFLNK
  /* systems without symlinks probably don't have S_IFLNK defined */
  else if (strcmp (p, "symlink") == 0)
    ctype = S_IFLNK;
#endif
  else if (strcmp (p, "block-special") == 0)
    ctype = S_IFBLK;
  else if (strcmp (p, "char-special") == 0)
    ctype = S_IFCHR;
  else if (strcmp (p, "fifo") == 0)
    ctype = S_IFIFO;
#ifdef S_IFSOCK
  else if (strcmp (p, "socket") == 0)
    ctype = S_IFSOCK;
#endif
  else
    SCM_OUT_OF_RANGE (2, type);

  STRING_SYSCALL (path, c_path,
		  val = mknod (c_path,
			       ctype | scm_to_int (perms),
			       scm_to_int (dev)));
  if (val != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_MKNOD */

#ifdef HAVE_NICE
SCM_DEFINE (scm_nice, "nice", 1, 0, 0, 
            (SCM incr),
	    "Increment the priority of the current process by @var{incr}.  A higher\n"
	    "priority value means that the process runs less often.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_nice
{
  int nice_value;

  /* nice() returns "prio-NZERO" on success or -1 on error, but -1 can arise
     from "prio-NZERO", so an error must be detected from errno changed */
  errno = 0;
  nice_value = nice (scm_to_int (incr));
  if (errno != 0)
    SCM_SYSERROR;

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_NICE */

#ifdef HAVE_SYNC
SCM_DEFINE (scm_sync, "sync", 0, 0, 0,
            (),
	    "Flush the operating system disk buffers.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_sync
{
  sync();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_SYNC */


/* crypt() returns a pointer to a static buffer, so we use scm_i_misc_mutex
   to avoid another thread overwriting it.  A test program running crypt
   continuously in two threads can be quickly seen tripping this problem.
   crypt() is pretty slow normally, so a mutex shouldn't add much overhead.

   glibc has a thread-safe crypt_r, but (in version 2.3.2) it runs a lot
   slower (about 5x) than plain crypt if you pass an uninitialized data
   block each time.  Presumably there's some one-time setups.  The best way
   to use crypt_r for parallel execution in multiple threads would probably
   be to maintain a little pool of initialized crypt_data structures, take
   one and use it, then return it to the pool.  That pool could be garbage
   collected so it didn't add permanently to memory use if only a few crypt
   calls are made.  But we expect crypt will be used rarely, and even more
   rarely will there be any desire for lots of parallel execution on
   multiple cpus.  So for now we don't bother with anything fancy, just
   ensure it works.  */

#if HAVE_CRYPT
SCM_DEFINE (scm_crypt, "crypt", 2, 0, 0,
            (SCM key, SCM salt),
	    "Encrypt @var{key} using @var{salt} as the salt value to the\n"
	    "crypt(3) library call.")
#define FUNC_NAME s_scm_crypt
{
  SCM ret;
  char *c_key, *c_salt, *c_ret;

  scm_dynwind_begin (0);
  scm_i_dynwind_pthread_mutex_lock (&scm_i_misc_mutex);

  c_key = scm_to_locale_string (key);
  scm_dynwind_free (c_key);
  c_salt = scm_to_locale_string (salt);
  scm_dynwind_free (c_salt);

  /* The Linux crypt(3) man page says crypt will return NULL and set errno
     on error.  (Eg. ENOSYS if legal restrictions mean it cannot be
     implemented).  */
  c_ret = crypt (c_key, c_salt);
  if (c_ret == NULL)
    SCM_SYSERROR;

  ret = scm_from_locale_string (c_ret);
  scm_dynwind_end ();
  return ret;
}
#undef FUNC_NAME
#endif /* HAVE_CRYPT */

#if HAVE_CHROOT
SCM_DEFINE (scm_chroot, "chroot", 1, 0, 0, 
            (SCM path),
	    "Change the root directory to that specified in @var{path}.\n"
	    "This directory will be used for path names beginning with\n"
	    "@file{/}.  The root directory is inherited by all children\n"
	    "of the current process.  Only the superuser may change the\n"
	    "root directory.")
#define FUNC_NAME s_scm_chroot
{
  int rv;

  WITH_STRING (path, c_path,
	       rv = chroot (c_path));
  if (rv == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_CHROOT */


#ifdef __MINGW32__
/* Wrapper function to supplying `getlogin()' under Windows.  */
static char * getlogin (void)
{
  static char user[256];
  static unsigned long len = 256;

  if (!GetUserName (user, &len))
    return NULL;
  return user;
}
#endif /* __MINGW32__ */


#if defined (HAVE_GETLOGIN) || defined (__MINGW32__)
SCM_DEFINE (scm_getlogin, "getlogin", 0, 0, 0, 
            (void),
	    "Return a string containing the name of the user logged in on\n"
	    "the controlling terminal of the process, or @code{#f} if this\n"
	    "information cannot be obtained.")
#define FUNC_NAME s_scm_getlogin
{
  char * p;

  p = getlogin ();
  if (!p || !*p)
    return SCM_BOOL_F;
  return scm_from_locale_string (p);
}
#undef FUNC_NAME
#endif /* HAVE_GETLOGIN */

#if HAVE_CUSERID

# if !HAVE_DECL_CUSERID
extern char *cuserid (char *);
# endif

SCM_DEFINE (scm_cuserid, "cuserid", 0, 0, 0, 
            (void),
	    "Return a string containing a user name associated with the\n"
	    "effective user id of the process.  Return @code{#f} if this\n"
	    "information cannot be obtained.")
#define FUNC_NAME s_scm_cuserid
{
  char buf[L_cuserid];
  char * p;

  p = cuserid (buf);
  if (!p || !*p)
    return SCM_BOOL_F;
  return scm_from_locale_string (p);
}
#undef FUNC_NAME
#endif /* HAVE_CUSERID */

#if HAVE_GETPRIORITY
SCM_DEFINE (scm_getpriority, "getpriority", 2, 0, 0, 
            (SCM which, SCM who),
	    "Return the scheduling priority of the process, process group\n"
	    "or user, as indicated by @var{which} and @var{who}. @var{which}\n"
	    "is one of the variables @code{PRIO_PROCESS}, @code{PRIO_PGRP}\n"
	    "or @code{PRIO_USER}, and @var{who} is interpreted relative to\n"
	    "@var{which} (a process identifier for @code{PRIO_PROCESS},\n"
	    "process group identifier for @code{PRIO_PGRP}, and a user\n"
	    "identifier for @code{PRIO_USER}.  A zero value of @var{who}\n"
	    "denotes the current process, process group, or user.  Return\n"
	    "the highest priority (lowest numerical value) of any of the\n"
	    "specified processes.")
#define FUNC_NAME s_scm_getpriority
{
  int cwhich, cwho, ret;

  cwhich = scm_to_int (which);
  cwho = scm_to_int (who);

  /* We have to clear errno and examine it later, because -1 is a
     legal return value for getpriority().  */
  errno = 0;
  ret = getpriority (cwhich, cwho);
  if (errno != 0)
    SCM_SYSERROR;
  return scm_from_int (ret);
}
#undef FUNC_NAME
#endif /* HAVE_GETPRIORITY */

#if HAVE_SETPRIORITY
SCM_DEFINE (scm_setpriority, "setpriority", 3, 0, 0, 
            (SCM which, SCM who, SCM prio),
	    "Set the scheduling priority of the process, process group\n"
	    "or user, as indicated by @var{which} and @var{who}. @var{which}\n"
	    "is one of the variables @code{PRIO_PROCESS}, @code{PRIO_PGRP}\n"
	    "or @code{PRIO_USER}, and @var{who} is interpreted relative to\n"
	    "@var{which} (a process identifier for @code{PRIO_PROCESS},\n"
	    "process group identifier for @code{PRIO_PGRP}, and a user\n"
	    "identifier for @code{PRIO_USER}.  A zero value of @var{who}\n"
	    "denotes the current process, process group, or user.\n"
	    "@var{prio} is a value in the range -20 and 20, the default\n"
	    "priority is 0; lower priorities cause more favorable\n"
	    "scheduling.  Sets the priority of all of the specified\n"
	    "processes.  Only the super-user may lower priorities.\n"
	    "The return value is not specified.")
#define FUNC_NAME s_scm_setpriority
{
  int cwhich, cwho, cprio;

  cwhich = scm_to_int (which);
  cwho = scm_to_int (who);
  cprio = scm_to_int (prio);

  if (setpriority (cwhich, cwho, cprio) == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_SETPRIORITY */

#if HAVE_GETPASS
SCM_DEFINE (scm_getpass, "getpass", 1, 0, 0, 
            (SCM prompt),
	    "Display @var{prompt} to the standard error output and read\n"
	    "a password from @file{/dev/tty}.  If this file is not\n"
	    "accessible, it reads from standard input.  The password may be\n"
	    "up to 127 characters in length.  Additional characters and the\n"
	    "terminating newline character are discarded.  While reading\n"
	    "the password, echoing and the generation of signals by special\n"
	    "characters is disabled.")
#define FUNC_NAME s_scm_getpass
{
  char * p;
  SCM passwd;

  SCM_VALIDATE_STRING (1, prompt);

  WITH_STRING (prompt, c_prompt, 
	       p = getpass(c_prompt));
  passwd = scm_from_locale_string (p);

  /* Clear out the password in the static buffer.  */
  memset (p, 0, strlen (p));

  return passwd;
}
#undef FUNC_NAME
#endif /* HAVE_GETPASS */

/* Wrapper function for flock() support under M$-Windows. */
#ifdef __MINGW32__
# include <io.h>
# include <sys/locking.h>
# include <errno.h>
# ifndef _LK_UNLCK
   /* Current MinGW package fails to define this. *sigh* */
#  define _LK_UNLCK 0
# endif
# define LOCK_EX 1
# define LOCK_UN 2
# define LOCK_SH 4
# define LOCK_NB 8

static int flock (int fd, int operation)
{
  long pos, len;
  int ret, err;

  /* Disable invalid arguments. */
  if (((operation & (LOCK_EX | LOCK_SH)) == (LOCK_EX | LOCK_SH)) ||
      ((operation & (LOCK_EX | LOCK_UN)) == (LOCK_EX | LOCK_UN)) ||
      ((operation & (LOCK_SH | LOCK_UN)) == (LOCK_SH | LOCK_UN)))
    {
      errno = EINVAL;
      return -1;
    }

  /* Determine mode of operation and discard unsupported ones. */
  if (operation == (LOCK_NB | LOCK_EX))
    operation = _LK_NBLCK;
  else if (operation & LOCK_UN)
    operation = _LK_UNLCK;
  else if (operation == LOCK_EX)
    operation = _LK_LOCK;
  else
    {
      errno = EINVAL;
      return -1;
    }

  /* Save current file pointer and seek to beginning. */
  if ((pos = lseek (fd, 0, SEEK_CUR)) == -1 || (len = filelength (fd)) == -1)
    return -1;
  lseek (fd, 0L, SEEK_SET);

  /* Deadlock if necessary. */
  do
    {
      ret = _locking (fd, operation, len);
    }
  while (ret == -1 && errno == EDEADLOCK);

  /* Produce meaningful error message. */
  if (errno == EACCES && operation == _LK_NBLCK)
    err = EDEADLOCK;
  else
    err = errno;

  /* Return to saved file position pointer. */
  lseek (fd, pos, SEEK_SET);
  errno = err;
  return ret;
}
#endif /* __MINGW32__ */

#if HAVE_FLOCK || defined (__MINGW32__)

#ifndef __MINGW32__
# if !HAVE_DECL_FLOCK
extern int flock (int, int);
# endif
#endif

SCM_DEFINE (scm_flock, "flock", 2, 0, 0, 
            (SCM file, SCM operation),
	    "Apply or remove an advisory lock on an open file.\n"
	    "@var{operation} specifies the action to be done:\n"
	    "\n"
	    "@defvar LOCK_SH\n"
	    "Shared lock.  More than one process may hold a shared lock\n"
	    "for a given file at a given time.\n"
	    "@end defvar\n"
	    "@defvar LOCK_EX\n"
	    "Exclusive lock.  Only one process may hold an exclusive lock\n"
	    "for a given file at a given time.\n"
	    "@end defvar\n"
	    "@defvar LOCK_UN\n"
	    "Unlock the file.\n"
	    "@end defvar\n"
	    "@defvar LOCK_NB\n"
	    "Don't block when locking.  This is combined with one of the\n"
	    "other operations using @code{logior}.  If @code{flock} would\n"
	    "block an @code{EWOULDBLOCK} error is thrown.\n"
	    "@end defvar\n"
	    "\n"
	    "The return value is not specified. @var{file} may be an open\n"
	    "file descriptor or an open file descriptor port.\n"
	    "\n"
	    "Note that @code{flock} does not lock files across NFS.")
#define FUNC_NAME s_scm_flock
{
  int fdes;

  if (scm_is_integer (file))
    fdes = scm_to_int (file);
  else
    {
      SCM_VALIDATE_OPFPORT (2, file);

      fdes = SCM_FPORT_FDES (file);
    }
  if (flock (fdes, scm_to_int (operation)) == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_FLOCK */

#if HAVE_SETHOSTNAME
SCM_DEFINE (scm_sethostname, "sethostname", 1, 0, 0, 
            (SCM name),
	    "Set the host name of the current processor to @var{name}. May\n"
	    "only be used by the superuser.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_sethostname
{
  int rv;

  WITH_STRING (name, c_name,
	       rv = sethostname (c_name, strlen(c_name)));
  if (rv == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_SETHOSTNAME */


#if HAVE_GETHOSTNAME
SCM_DEFINE (scm_gethostname, "gethostname", 0, 0, 0, 
            (void),
	    "Return the host name of the current processor.")
#define FUNC_NAME s_scm_gethostname
{
#ifdef MAXHOSTNAMELEN

  /* Various systems define MAXHOSTNAMELEN (including Solaris in fact).
   * On GNU/Linux this doesn't include the terminating '\0', hence "+ 1".  */
  const int len = MAXHOSTNAMELEN + 1;
  char *const p = scm_malloc (len);
  const int res = gethostname (p, len);

  scm_dynwind_begin (0);
  scm_dynwind_unwind_handler (free, p, 0);

#else

  /* Default 256 is for Solaris, under Linux ENAMETOOLONG is returned if not
   * large enough.  SUSv2 specifies 255 maximum too, apparently.  */
  int len = 256;
  int res;
  char *p;

#  if HAVE_SYSCONF && defined (_SC_HOST_NAME_MAX)

  /* POSIX specifies the HOST_NAME_MAX system parameter for the max size,
   * which may reflect a particular kernel configuration.
   * Must watch out for this existing but giving -1, as happens for instance
   * in gnu/linux glibc 2.3.2.  */
  {
    const long int n = sysconf (_SC_HOST_NAME_MAX);
    if (n != -1L)
      len = n;
  }

#  endif

  p = scm_malloc (len);

  scm_dynwind_begin (0);
  scm_dynwind_unwind_handler (free, p, 0);

  res = gethostname (p, len);
  while (res == -1 && errno == ENAMETOOLONG)
    {
      len *= 2;

      /* scm_realloc may throw an exception.  */
      p = scm_realloc (p, len);
      res = gethostname (p, len);
    }

#endif

  if (res == -1)
    {
      const int save_errno = errno;

      /* No guile exceptions can occur before we have freed p's memory. */
      scm_dynwind_end ();
      free (p);

      errno = save_errno;
      SCM_SYSERROR;
    }
  else
    {
      /* scm_from_locale_string may throw an exception.  */
      const SCM name = scm_from_locale_string (p);

      /* No guile exceptions can occur before we have freed p's memory. */
      scm_dynwind_end ();
      free (p);

      return name;
    }
}
#undef FUNC_NAME
#endif /* HAVE_GETHOSTNAME */


void 
scm_init_posix ()
{
  scm_add_feature ("posix");
#ifdef HAVE_GETEUID
  scm_add_feature ("EIDs");
#endif
#ifdef WAIT_ANY
  scm_c_define ("WAIT_ANY", scm_from_int (WAIT_ANY));
#endif
#ifdef WAIT_MYPGRP
  scm_c_define ("WAIT_MYPGRP", scm_from_int (WAIT_MYPGRP));
#endif
#ifdef WNOHANG
  scm_c_define ("WNOHANG", scm_from_int (WNOHANG));
#endif
#ifdef WUNTRACED
  scm_c_define ("WUNTRACED", scm_from_int (WUNTRACED));
#endif

  /* access() symbols.  */
  scm_c_define ("R_OK", scm_from_int (R_OK));
  scm_c_define ("W_OK", scm_from_int (W_OK));
  scm_c_define ("X_OK", scm_from_int (X_OK));
  scm_c_define ("F_OK", scm_from_int (F_OK));

#ifdef LC_COLLATE
  scm_c_define ("LC_COLLATE", scm_from_int (LC_COLLATE));
#endif
#ifdef LC_CTYPE
  scm_c_define ("LC_CTYPE", scm_from_int (LC_CTYPE));
#endif
#ifdef LC_MONETARY
  scm_c_define ("LC_MONETARY", scm_from_int (LC_MONETARY));
#endif
#ifdef LC_NUMERIC
  scm_c_define ("LC_NUMERIC", scm_from_int (LC_NUMERIC));
#endif
#ifdef LC_TIME
  scm_c_define ("LC_TIME", scm_from_int (LC_TIME));
#endif
#ifdef LC_MESSAGES
  scm_c_define ("LC_MESSAGES", scm_from_int (LC_MESSAGES));
#endif
#ifdef LC_ALL
  scm_c_define ("LC_ALL", scm_from_int (LC_ALL));
#endif
#ifdef LC_PAPER
  scm_c_define ("LC_PAPER", scm_from_int (LC_PAPER));
#endif
#ifdef LC_NAME
  scm_c_define ("LC_NAME", scm_from_int (LC_NAME));
#endif
#ifdef LC_ADDRESS
  scm_c_define ("LC_ADDRESS", scm_from_int (LC_ADDRESS));
#endif
#ifdef LC_TELEPHONE
  scm_c_define ("LC_TELEPHONE", scm_from_int (LC_TELEPHONE));
#endif
#ifdef LC_MEASUREMENT
  scm_c_define ("LC_MEASUREMENT", scm_from_int (LC_MEASUREMENT));
#endif
#ifdef LC_IDENTIFICATION
  scm_c_define ("LC_IDENTIFICATION", scm_from_int (LC_IDENTIFICATION));
#endif
#ifdef PIPE_BUF
  scm_c_define ("PIPE_BUF", scm_from_long (PIPE_BUF));
#endif

#ifdef PRIO_PROCESS
  scm_c_define ("PRIO_PROCESS", scm_from_int (PRIO_PROCESS));
#endif
#ifdef PRIO_PGRP
  scm_c_define ("PRIO_PGRP", scm_from_int (PRIO_PGRP));
#endif
#ifdef PRIO_USER
  scm_c_define ("PRIO_USER", scm_from_int (PRIO_USER));
#endif

#ifdef LOCK_SH
  scm_c_define ("LOCK_SH", scm_from_int (LOCK_SH));
#endif
#ifdef LOCK_EX
  scm_c_define ("LOCK_EX", scm_from_int (LOCK_EX));
#endif
#ifdef LOCK_UN
  scm_c_define ("LOCK_UN", scm_from_int (LOCK_UN));
#endif
#ifdef LOCK_NB
  scm_c_define ("LOCK_NB", scm_from_int (LOCK_NB));
#endif

#include "libguile/cpp_sig_symbols.c"
#include "libguile/posix.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
