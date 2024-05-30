/* Copyright (C) 2001, 2006, 2009 Free Software Foundation, Inc.
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
#include <string.h>
#include <stdarg.h>

#include "libguile/_scm.h"

#include "libguile/deprecation.h"
#include "libguile/strings.h"
#include "libguile/ports.h"

/* Windows defines. */
#ifdef __MINGW32__
# define vsnprintf _vsnprintf
#elif !HAVE_DECL_VSNPRINTF
extern int vsnprintf (char *, size_t, const char *, va_list);
#endif



struct issued_warning {
  struct issued_warning *prev;
  const char *message;
};

static struct issued_warning *issued_warnings;
static int print_summary = 0;

void
scm_c_issue_deprecation_warning (const char *msg)
{
  if (!SCM_WARN_DEPRECATED)
    print_summary = 1;
  else
    {
      struct issued_warning *iw;
      for (iw = issued_warnings; iw; iw = iw->prev)
	if (!strcmp (iw->message, msg))
	  return;
      if (scm_gc_running_p)
	fprintf (stderr, "%s\n", msg);
      else
	{
	  scm_puts (msg, scm_current_error_port ());
	  scm_newline (scm_current_error_port ());
	}
      msg = strdup (msg);
      iw = malloc (sizeof (struct issued_warning));
      if (msg == NULL || iw == NULL)
	return;
      iw->message = msg;
      iw->prev = issued_warnings;
      issued_warnings = iw;
    }
}

void
scm_c_issue_deprecation_warning_fmt (const char *msg, ...)
{
  va_list ap;
  char buf[512];

  va_start (ap, msg);
  vsnprintf (buf, 511, msg, ap);
  va_end (ap);
  buf[511] = '\0';
  scm_c_issue_deprecation_warning (buf);
}

SCM_DEFINE(scm_issue_deprecation_warning,
	   "issue-deprecation-warning", 0, 0, 1, 
	   (SCM msgs),
	   "Output @var{msgs} to @code{(current-error-port)} when this "
	   "is the first call to @code{issue-deprecation-warning} with "
	   "this specific @var{msgs}.  Do nothing otherwise. "
	   "The argument @var{msgs} should be a list of strings; "
	   "they are printed in turn, each one followed by a newline.")
#define FUNC_NAME s_scm_issue_deprecation_warning
{
  if (!SCM_WARN_DEPRECATED)
    print_summary = 1;
  else
    {
      SCM nl = scm_from_locale_string ("\n");
      SCM msgs_nl = SCM_EOL;
      char *c_msgs;
      while (scm_is_pair (msgs))
	{
	  if (msgs_nl != SCM_EOL)
	    msgs_nl = scm_cons (nl, msgs_nl);
	  msgs_nl = scm_cons (SCM_CAR (msgs), msgs_nl);
	  msgs = SCM_CDR (msgs);
	}
      msgs_nl = scm_string_append (scm_reverse_x (msgs_nl, SCM_EOL));
      c_msgs = scm_to_locale_string (msgs_nl);
      scm_c_issue_deprecation_warning (c_msgs);
      free (c_msgs);
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static void
print_deprecation_summary (void)
{
  if (print_summary)
    {
      fputs ("\n"
	     "Some deprecated features have been used.  Set the environment\n"
             "variable GUILE_WARN_DEPRECATED to \"detailed\" and rerun the\n"
	     "program to get more information.  Set it to \"no\" to suppress\n"
	     "this message.\n", stderr);
    }
}

SCM_DEFINE(scm_include_deprecated_features,
	   "include-deprecated-features", 0, 0, 0,
	   (),
	   "Return @code{#t} iff deprecated features should be included "
           "in public interfaces.")
#define FUNC_NAME s_scm_include_deprecated_features
{
  return scm_from_bool (SCM_ENABLE_DEPRECATED == 1);
}
#undef FUNC_NAME




void
scm_init_deprecation ()
{
  const char *level = getenv ("GUILE_WARN_DEPRECATED");
  if (level == NULL)
    level = SCM_WARN_DEPRECATED_DEFAULT;
  if (!strcmp (level, "detailed"))
    SCM_WARN_DEPRECATED = 1;
  else if (!strcmp (level, "no"))
    SCM_WARN_DEPRECATED = 0;
  else
    {
      SCM_WARN_DEPRECATED = 0;
      atexit (print_deprecation_summary);
    }
#include "libguile/deprecation.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End: */
