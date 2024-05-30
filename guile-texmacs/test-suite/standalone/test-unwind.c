/* Copyright (C) 2004, 2005, 2008 Free Software Foundation, Inc.
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

#if HAVE_CONFIG_H
# include <config.h>
#endif

/* This blob per the Autoconf manual (under "Particular Functions"), updated
   to match that of Gnulib.  */
#ifndef alloca
# if HAVE_ALLOCA_H
#  include <alloca.h>
# elif defined __GNUC__
#  define alloca __builtin_alloca
# elif defined _AIX
#  define alloca __alloca
# elif defined _MSC_VER
#  include <malloc.h>
#  define alloca _alloca
# else
#  include <stddef.h>
#  ifdef  __cplusplus
extern "C"
#  endif
void *alloca (size_t);
# endif
#endif

#include <libguile.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#ifdef HAVE_STRING_H
# include <string.h>
#endif


void set_flag (void *data);
void func1 (void);
void func2 (void);
void func3 (void);
void func4 (void);
void check_flag1 (const char *msg, void (*func)(void), int val);
SCM check_flag1_body (void *data);
SCM return_tag (void *data, SCM tag, SCM args);
void check_cont (int rewindable);
SCM check_cont_body (void *data);
void close_port (SCM port);
void delete_file (void *data);
void check_ports (void);
void check_fluid (void);

int flag1, flag2, flag3;

void
set_flag (void *data)
{
  int *f = (int *)data;
  *f = 1;
}

/* FUNC1 should leave flag1 zero.
 */

void
func1 ()
{
  scm_dynwind_begin (0);
  flag1 = 0;
  scm_dynwind_unwind_handler (set_flag, &flag1, 0);
  scm_dynwind_end ();
}

/* FUNC2 should set flag1.
 */

void
func2 ()
{
  scm_dynwind_begin (0);
  flag1 = 0;
  scm_dynwind_unwind_handler (set_flag, &flag1, SCM_F_WIND_EXPLICITLY);
  scm_dynwind_end ();
}

/* FUNC3 should set flag1.
 */

void
func3 ()
{
  scm_dynwind_begin (0);
  flag1 = 0;
  scm_dynwind_unwind_handler (set_flag, &flag1, 0);
  scm_misc_error ("func3", "gratuitous error", SCM_EOL);
  scm_dynwind_end ();
}

/* FUNC4 should set flag1.
 */

void
func4 ()
{
  scm_dynwind_begin (0);
  flag1 = 0;
  scm_dynwind_unwind_handler (set_flag, &flag1, SCM_F_WIND_EXPLICITLY);
  scm_misc_error ("func4", "gratuitous error", SCM_EOL);
  scm_dynwind_end ();
}

SCM
check_flag1_body (void *data)
{
  void (*f)(void) = (void (*)(void))data;
  f ();
  return SCM_UNSPECIFIED;
}

SCM
return_tag (void *data, SCM tag, SCM args)
{
  return tag;
}

void
check_flag1 (const char *tag, void (*func)(void), int val)
{
  scm_internal_catch (SCM_BOOL_T,
		      check_flag1_body, func,
		      return_tag, NULL);
  if (flag1 != val)
    {
      printf ("%s failed\n", tag);
      exit (1);
    }
}

SCM
check_cont_body (void *data)
{
  scm_t_dynwind_flags flags = (data? SCM_F_DYNWIND_REWINDABLE : 0);
  int first;
  SCM val;

  scm_dynwind_begin (flags);

  val = scm_make_continuation (&first);
  scm_dynwind_end ();
  return val;
}

void
check_cont (int rewindable)
{
  SCM res;
  
  res = scm_internal_catch (SCM_BOOL_T,
			    check_cont_body, (void *)(long)rewindable,
			    return_tag, NULL);

  /* RES is now either the created continuation, the value passed to
     the continuation, or a catch-tag, such as 'misc-error.
   */

  if (scm_is_true (scm_procedure_p (res)))
    {
      /* a continuation, invoke it */
      scm_call_1 (res, SCM_BOOL_F);
    }
  else if (scm_is_false (res))
    {
      /* the result of invoking the continuation, dynwind must be
	 rewindable */
      if (rewindable)
	return;
      printf ("continuation not blocked\n");
      exit (1);
    }
  else
    {
      /* the catch tag, dynwind must not have been rewindable. */
      if (!rewindable)
	return;
      printf ("continuation didn't work\n");
      exit (1);
    }
}

void
close_port (SCM port)
{
  scm_close_port (port);
}

void
delete_file (void *data)
{
  unlink ((char *)data);
}

void
check_ports ()
{
#define FILENAME_TEMPLATE "/check-ports.XXXXXX"
  char *filename;
  const char *tmpdir = getenv ("TMPDIR");

  if (tmpdir == NULL)
    tmpdir = "/tmp";

  filename = (char *) alloca (strlen (tmpdir) +
			      sizeof (FILENAME_TEMPLATE) + 1);
  strcpy (filename, tmpdir);
  strcat (filename, FILENAME_TEMPLATE);

  if (mktemp (filename) == NULL)
    exit (1);

  scm_dynwind_begin (0);
  {
    SCM port = scm_open_file (scm_from_locale_string (filename),
			      scm_from_locale_string ("w"));
    scm_dynwind_unwind_handler_with_scm (close_port, port,
				       SCM_F_WIND_EXPLICITLY);

    scm_dynwind_current_output_port (port);
    scm_write (scm_version (), SCM_UNDEFINED);
  }
  scm_dynwind_end ();

  scm_dynwind_begin (0);
  {
    SCM port = scm_open_file (scm_from_locale_string (filename),
			      scm_from_locale_string ("r"));
    SCM res;
    scm_dynwind_unwind_handler_with_scm (close_port, port,
				       SCM_F_WIND_EXPLICITLY);
    scm_dynwind_unwind_handler (delete_file, filename, SCM_F_WIND_EXPLICITLY);

    scm_dynwind_current_input_port (port);
    res = scm_read (SCM_UNDEFINED);
    if (scm_is_false (scm_equal_p (res, scm_version ())))
      {
	printf ("ports didn't work\n");
	exit (1);
      }
  }
  scm_dynwind_end ();
#undef FILENAME_TEMPLATE
}

void
check_fluid ()
{
  SCM f = scm_make_fluid ();
  SCM x;

  scm_fluid_set_x (f, scm_from_int (12));

  scm_dynwind_begin (0);
  scm_dynwind_fluid (f, scm_from_int (13));
  x = scm_fluid_ref (f);
  scm_dynwind_end ();

  if (!scm_is_eq (x, scm_from_int (13)))
    {
      printf ("setting fluid didn't work\n");
      exit (1);
    }

  if (!scm_is_eq (scm_fluid_ref (f), scm_from_int (12)))
    {
      printf ("resetting fluid didn't work\n");
      exit (1);
    }
}

static void
inner_main (void *data, int argc, char **argv)
{
  check_flag1 ("func1", func1, 0);
  check_flag1 ("func2", func2, 1);
  check_flag1 ("func3", func3, 1);
  check_flag1 ("func4", func4, 1);

  check_cont (0);
  check_cont (1);

  check_ports ();

  check_fluid ();

  exit (0);
}

int
main (int argc, char **argv)
{
  scm_boot_guile (argc, argv, inner_main, 0);
  return 0;
}
