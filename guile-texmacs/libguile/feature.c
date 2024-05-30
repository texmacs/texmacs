/* Copyright (C) 1995,1996,1998,1999,2000,2001,2002, 2003, 2004, 2006, 2007 Free Software Foundation, Inc.
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

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libguile/_scm.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/validate.h"
#include "libguile/fluids.h"

#include "libguile/feature.h"



static SCM progargs_fluid;
static SCM features_var;

void
scm_add_feature (const char *str)
{
  SCM old = SCM_VARIABLE_REF (features_var);
  SCM new = scm_cons (scm_from_locale_symbol (str), old);
  SCM_VARIABLE_SET (features_var, new);
}



SCM_DEFINE (scm_program_arguments, "program-arguments", 0, 0, 0, 
	    (),
	    "@deffnx {Scheme Procedure} command-line\n"
	    "Return the list of command line arguments passed to Guile, as a list of\n"
	    "strings.  The list includes the invoked program name, which is usually\n"
	    "@code{\"guile\"}, but excludes switches and parameters for command line\n"
	    "options like @code{-e} and @code{-l}.")
#define FUNC_NAME s_scm_program_arguments
{
  return scm_fluid_ref (progargs_fluid);
}
#undef FUNC_NAME

/* Set the value returned by program-arguments, given ARGC and ARGV.

   If FIRST is non-zero, make it the first element; we do this in
   situations where other code (like getopt) has parsed out a few
   arguments, but we still want the script name to be the first
   element.  */
void
scm_set_program_arguments (int argc, char **argv, char *first)
{
  SCM args = scm_makfromstrs (argc, argv);
  if (first)
    args = scm_cons (scm_from_locale_string (first), args);
  scm_fluid_set_x (progargs_fluid, args);
}

SCM_DEFINE (scm_set_program_arguments_scm, "set-program-arguments", 1, 0, 0, 
	    (SCM lst),
	    "Set the command line arguments to be returned by\n"
	    "@code{program-arguments} (and @code{command-line}).  @var{lst}\n"
	    "should be a list of strings, the first of which is the program\n"
	    "name (either a script name, or just @code{\"guile\"}).\n"
	    "\n"
	    "Program arguments are held in a fluid and therefore have a\n"
	    "separate value in each Guile thread.  Neither the list nor the\n"
	    "strings within it are copied, so should not be modified later.")
#define FUNC_NAME s_scm_set_program_arguments_scm
{
  return scm_fluid_set_x (progargs_fluid, lst);
}
#undef FUNC_NAME




void
scm_init_feature()
{
  progargs_fluid = scm_permanent_object (scm_make_fluid ());

  features_var = scm_c_define ("*features*", SCM_EOL);
#ifndef _Windows
  scm_add_feature("system");
#endif
#ifdef vms
  scm_add_feature(s_ed);
#endif
#ifdef SICP
  scm_add_feature("sicp");
#endif
#ifndef GO32
  scm_add_feature("char-ready?");
#endif
#ifndef CHEAP_CONTINUATIONS
  scm_add_feature ("full-continuation");
#endif
#if SCM_USE_PTHREAD_THREADS
  scm_add_feature ("threads");
#endif

  scm_c_define ("char-code-limit", scm_from_int (SCM_CHAR_CODE_LIMIT));

#include "libguile/feature.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
