/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2004, 2006 Free Software Foundation, Inc.
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

#include <stdio.h>
#include <errno.h>

#include "libguile/_scm.h"
#include "libguile/dynwind.h"
#include "libguile/pairs.h"
#include "libguile/strings.h"
#include "libguile/throw.h"

#include "libguile/validate.h"
#include "libguile/error.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* For Windows... */
#ifdef HAVE_IO_H
#include <io.h>
#endif


/* {Errors and Exceptional Conditions}
 */


/* Scheme interface to scm_error_scm.  */
void
scm_error (SCM key, const char *subr, const char *message, SCM args, SCM rest)
{
  scm_error_scm 
    (key,
     (subr == NULL) ? SCM_BOOL_F : scm_from_locale_string (subr),
     (message == NULL) ? SCM_BOOL_F : scm_from_locale_string (message),
     args, rest);
}

/* All errors should pass through here.  */
SCM_DEFINE (scm_error_scm, "scm-error", 5, 0, 0, 
           (SCM key, SCM subr, SCM message, SCM args, SCM data),
	    "Raise an error with key @var{key}.  @var{subr} can be a string\n"
	    "naming the procedure associated with the error, or @code{#f}.\n"
	    "@var{message} is the error message string, possibly containing\n"
	    "@code{~S} and @code{~A} escapes.  When an error is reported,\n"
	    "these are replaced by formatting the corresponding members of\n"
	    "@var{args}: @code{~A} (was @code{%s} in older versions of\n"
	    "Guile) formats using @code{display} and @code{~S} (was\n"
	    "@code{%S}) formats using @code{write}.  @var{data} is a list or\n"
	    "@code{#f} depending on @var{key}: if @var{key} is\n"
	    "@code{system-error} then it should be a list containing the\n"
	    "Unix @code{errno} value; If @var{key} is @code{signal} then it\n"
	    "should be a list containing the Unix signal number; If\n"
	    "@var{key} is @code{out-of-range} or @code{wrong-type-arg},\n"
            "it is a list containing the bad value; otherwise\n"
	    "it will usually be @code{#f}.")
#define FUNC_NAME s_scm_error_scm
{
  if (scm_gc_running_p)
    {
      /* The error occured during GC --- abort */
      fprintf (stderr, "Guile: error during GC.\n"),
      abort ();
    }

  scm_ithrow (key, scm_list_4 (subr, message, args, data), 1);
  
  /* No return, but just in case: */
  fprintf (stderr, "Guile scm_ithrow returned!\n");
  exit (1);
}
#undef FUNC_NAME

#ifdef __MINGW32__
# include "win32-socket.h"
# define SCM_I_STRERROR(err) \
    ((err >= WSABASEERR) ? scm_i_socket_strerror (err) : strerror (err))
# define SCM_I_ERRNO() \
    (errno ? errno : scm_i_socket_errno ())
#else
# define SCM_I_STRERROR(err) strerror (err)
# define SCM_I_ERRNO() errno
#endif /* __MINGW32__ */

/* strerror may not be thread safe, for instance in glibc (version 2.3.2) an
   error number not among the known values results in a string like "Unknown
   error 9999" formed in a static buffer, which will be overwritten by a
   similar call in another thread.  A test program running two threads with
   different unknown error numbers can trip this fairly quickly.

   Some systems don't do what glibc does, instead just giving a single
   "Unknown error" for unrecognised numbers.  It doesn't seem worth trying
   to tell if that's the case, a mutex is reasonably fast, and strerror
   isn't needed very often.

   strerror_r (when available) could be used, it might be a touch faster
   than a frame and a mutex, though there's probably not much
   difference.  */

SCM_DEFINE (scm_strerror, "strerror", 1, 0, 0, 
            (SCM err),
	    "Return the Unix error message corresponding to @var{err}, which\n"
	    "must be an integer value.")
#define FUNC_NAME s_scm_strerror
{
  SCM ret;
  scm_dynwind_begin (0);
  scm_i_dynwind_pthread_mutex_lock (&scm_i_misc_mutex);

  ret = scm_from_locale_string (SCM_I_STRERROR (scm_to_int (err)));

  scm_dynwind_end ();
  return ret;
}
#undef FUNC_NAME

SCM_GLOBAL_SYMBOL (scm_system_error_key, "system-error");
void
scm_syserror (const char *subr)
{
  SCM err = scm_from_int (SCM_I_ERRNO ());
  scm_error (scm_system_error_key,
	     subr,
	     "~A",
	     scm_cons (scm_strerror (err), SCM_EOL),
	     scm_cons (err, SCM_EOL));
}

void
scm_syserror_msg (const char *subr, const char *message, SCM args, int eno)
{
  scm_error (scm_system_error_key,
	     subr,
	     message,
	     args,
	     scm_cons (scm_from_int (eno), SCM_EOL));
}

SCM_GLOBAL_SYMBOL (scm_num_overflow_key, "numerical-overflow");
void
scm_num_overflow (const char *subr)
{
  scm_error (scm_num_overflow_key,
	     subr,
	     "Numerical overflow",
	     SCM_BOOL_F,
	     SCM_BOOL_F);
}

SCM_GLOBAL_SYMBOL (scm_out_of_range_key, "out-of-range");
void
scm_out_of_range (const char *subr, SCM bad_value)
{
  scm_error (scm_out_of_range_key,
	     subr,
	     "Value out of range: ~S",
             scm_list_1 (bad_value),
             scm_list_1 (bad_value));
}

void
scm_out_of_range_pos (const char *subr, SCM bad_value, SCM pos)
{
  scm_error (scm_out_of_range_key,
	     subr,
	     "Argument ~A out of range: ~S",
             scm_list_2 (pos, bad_value),
	     scm_list_1 (bad_value));
}


SCM_GLOBAL_SYMBOL (scm_args_number_key, "wrong-number-of-args");
void
scm_wrong_num_args (SCM proc)
{
  scm_error (scm_args_number_key,
	     NULL,
	     "Wrong number of arguments to ~A",
	     scm_list_1 (proc),
	     SCM_BOOL_F);
}


void
scm_error_num_args_subr (const char *subr)
{
  scm_error (scm_args_number_key,
	     NULL,
	     "Wrong number of arguments to ~A",
	     scm_list_1 (scm_from_locale_string (subr)),
	     SCM_BOOL_F);
}


SCM_GLOBAL_SYMBOL (scm_arg_type_key, "wrong-type-arg");
void
scm_wrong_type_arg (const char *subr, int pos, SCM bad_value)
{
  scm_error (scm_arg_type_key,
	     subr,
	     (pos == 0) ? "Wrong type: ~S"
	     : "Wrong type argument in position ~A: ~S",
	     (pos == 0) ? scm_list_1 (bad_value)
	     : scm_list_2 (scm_from_int (pos), bad_value),
	     scm_list_1 (bad_value));
}

void
scm_wrong_type_arg_msg (const char *subr, int pos, SCM bad_value, const char *szMessage)
{
  SCM msg = scm_from_locale_string (szMessage);
  if (pos == 0)
    {
      scm_error (scm_arg_type_key,
		 subr, "Wrong type (expecting ~A): ~S",
		 scm_list_2 (msg, bad_value),
		 scm_list_1 (bad_value));
    }
  else
    {
      scm_error (scm_arg_type_key,
		 subr,
		 "Wrong type argument in position ~A (expecting ~A): ~S",
		 scm_list_3 (scm_from_int (pos), msg, bad_value),
		 scm_list_1 (bad_value));
    }
}


SCM_GLOBAL_SYMBOL (scm_memory_alloc_key, "memory-allocation-error");
void
scm_memory_error (const char *subr)
{
  fprintf (stderr, "FATAL: memory error in %s\n", subr);
  abort ();
}

SCM_GLOBAL_SYMBOL (scm_misc_error_key, "misc-error");
void
scm_misc_error (const char *subr, const char *message, SCM args)
{
  scm_error (scm_misc_error_key, subr, message, args, SCM_BOOL_F);
}

void
scm_init_error ()
{
#include "libguile/cpp_err_symbols.c"
#include "libguile/error.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
