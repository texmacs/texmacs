/* Copyright (C) 1991, 2000, 2001, 2004, 2005, 2006 Free Software Foundation, Inc.

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

#include "libguile/scmconfig.h"

#include <sys/types.h>
#include <errno.h>

/* Don't include stdlib.h for non-GNU C libraries because some of them
   contain conflicting prototypes for getopt.
   This needs to come after some library #include
   to get __GNU_LIBRARY__ defined.  */
#ifdef	__GNU_LIBRARY__
#include <stdlib.h>
#else
char *malloc ();
#endif	/* GNU C library.  */

#if defined(STDC_HEADERS) || defined(HAVE_STRING_H)
#include <string.h>
#else
#include <strings.h>
#ifndef strchr
#define strchr index
#endif
#ifndef memcpy
#define memcpy(d, s, n) bcopy((s), (d), (n))
#endif
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#if HAVE_CRT_EXTERNS_H
#include <crt_externs.h>  /* for Darwin _NSGetEnviron */
#endif

#ifndef NULL
#define NULL 0
#endif

extern char **environ;

/* On Apple Darwin in a shared library there's no "environ" to access
   directly, instead the address of that variable must be obtained with
   _NSGetEnviron().  */
#if HAVE__NSGETENVIRON && defined (PIC)
#define environ (*_NSGetEnviron())
#endif

/* Put STRING, which is of the form "NAME=VALUE", in the environment.  */
int
putenv (const char *string)
{
  char *name_end = strchr (string, '=');
  register size_t size;
  register char **ep;

  if (name_end == NULL)
    {
      /* Remove the variable from the environment.  */
      size = strlen (string);
      for (ep = environ; *ep != NULL; ++ep)
	if (!strncmp (*ep, string, size) && (*ep)[size] == '=')
	  {
	    while (ep[1] != NULL)
	      {
		ep[0] = ep[1];
		++ep;
	      }
	    *ep = NULL;
	    return 0;
	  }
    }

  size = 0;
  for (ep = environ; *ep != NULL; ++ep)
    if (!strncmp (*ep, string, name_end - string) &&
	(*ep)[name_end - string] == '=')
      break;
    else
      ++size;

  if (*ep == NULL)
    {
      static char **last_environ = NULL;
      char **new_environ = (char **) scm_malloc ((size + 2) * sizeof (char *));
      memcpy ((char *) new_environ, (char *) environ, size * sizeof (char *));
      new_environ[size] = (char *) string;
      new_environ[size + 1] = NULL;
      if (last_environ != NULL)
	free ((char *) last_environ);
      last_environ = new_environ;
      environ = new_environ;
    }
  else
    *ep = (char *) string;

  return 0;
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
