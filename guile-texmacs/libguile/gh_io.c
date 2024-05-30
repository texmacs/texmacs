/*      Copyright (C) 1995,1996,1997, 2000, 2001, 2006, 2008 Free Software Foundation, Inc.

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

#include "libguile/gh.h"

#if SCM_ENABLE_DEPRECATED

void 
gh_display (SCM x)
{
  scm_display (x, scm_current_output_port ());
}

void 
gh_write (SCM x)
{
  scm_write (x, scm_current_output_port ());
}

void 
gh_newline ()
{
  scm_newline (scm_current_output_port ());
}

#endif /* SCM_ENABLE_DEPRECATED */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
