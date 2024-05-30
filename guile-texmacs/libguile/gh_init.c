/* Copyright (C) 1995,1996,1997,2000,2001, 2006, 2008 Free Software Foundation, Inc.
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
# include <config.h>
#endif

/* Guile high level (gh_) interface, initialization-related stuff */

#include <stdio.h>

#include "libguile/gh.h"

#if SCM_ENABLE_DEPRECATED

typedef void (*main_prog_t) (int argc, char **argv);
typedef void (*repl_prog_t) (int argc, char **argv);

/* This function takes care of all real GH initialization.  Since it's
   called by scm_boot_guile, it can safely work with heap objects, or
   call functions that do so.  */
static void 
gh_launch_pad (void *closure, int argc, char **argv)
{
  main_prog_t c_main_prog = (main_prog_t) closure;

  c_main_prog (argc, argv);
  exit (0);
}

/* starts up the Scheme interpreter, and stays in it.  c_main_prog()
   is the address of the user's main program, since gh_enter() never
   returns. */
void 
gh_enter (int argc, char *argv[], main_prog_t c_main_prog)
{
  scm_boot_guile (argc, argv, gh_launch_pad, (void *) c_main_prog);
  /* never returns */
}

/* offer a REPL to the C programmer; for now I just invoke the ice-9
   REPL that is written in Scheme */
void 
gh_repl (int argc, char *argv[])
{
/*   gh_eval_str ("(top-repl)"); */
  scm_shell (argc, argv);
}

/* libguile programmers need exception handling mechanisms; here is
   the recommended way of doing it with the gh_ interface */

/* gh_catch() -- set up an exception handler for a particular type of
   error (or any thrown error if tag is SCM_BOOL_T); see
   ../libguile/throw.c for the comments explaining scm_internal_catch */
SCM 
gh_catch (SCM tag, scm_t_catch_body body, void *body_data,
	  scm_t_catch_handler handler, void *handler_data)
{
  return scm_internal_catch (tag, body, body_data, handler, handler_data);
}

SCM 
gh_standard_handler (void *data SCM_UNUSED, SCM tag, SCM throw_args SCM_UNUSED)
{
  fprintf (stderr, "\nJust got an error; tag is\n        ");
  scm_display (tag, scm_current_output_port ());
  scm_newline (scm_current_output_port ());
  scm_newline (scm_current_output_port ());

  return SCM_BOOL_F;
}

#endif /* SCM_ENABLE_DEPRECATED */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
