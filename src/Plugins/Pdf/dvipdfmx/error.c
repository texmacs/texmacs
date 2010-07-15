/*  $Header: /home/cvsroot/dvipdfmx/src/error.c,v 1.4 2004/03/11 11:50:20 hirata Exp $

    This is dvipdfmx, an eXtended version of dvipdfm by Mark A. Wicks.

    Copyright (C) 2002 by Jin-Hwan Cho and Shunsaku Hirata,
    the dvipdfmx project team <dvipdfmx@project.ktug.or.kr>

    Copyright (C) 1998, 1999 by Mark A. Wicks <mwicks@kettering.edu>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
*/

#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>

#include "error.h"

#define DPX_MESG        0
#define DPX_MESG_WARN   1
#define DPX_MESG_ERROR  2

static int _mesg_type = DPX_MESG;
#define WANT_NEWLINE() (_mesg_type != DPX_MESG_WARN && _mesg_type != DPX_MESG_ERROR)

static int  really_quiet = 0;

void
shut_up (void)
{
  really_quiet = 1;
}

void
MESG (const char *fmt, ...)
{
  va_list argp;

  if (!really_quiet) {
    va_start(argp, fmt);
    vfprintf(stderr, fmt, argp);
    va_end(argp);
    _mesg_type = DPX_MESG;
  }
}

void
WARN (const char *fmt, ...)
{
  va_list argp;

  if (!really_quiet) {
    if (WANT_NEWLINE())
      fprintf(stderr, "\n");
    fprintf(stderr, "** WARNING ** ");
    va_start(argp, fmt);
    vfprintf(stderr, fmt, argp);
    va_end(argp);
    fprintf(stderr, "\n");

    _mesg_type = DPX_MESG_WARN;
  }
}

extern void error_cleanup (void);


void
ERROR (const char *fmt, ...)
{
  va_list argp;

  if (!really_quiet) {
    if (WANT_NEWLINE())
      fprintf(stderr, "\n");
    fprintf(stderr, "** ERROR ** ");
    va_start(argp, fmt);
    vfprintf(stderr, fmt, argp);
    va_end(argp);
    fprintf(stderr, "\n");
  }
  error_cleanup();
  exit( 1 );
}
