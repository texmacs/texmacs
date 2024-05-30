/*      Copyright (C) 1995,1996,1997,1998, 2000, 2001, 2006, 2008 Free Software Foundation, Inc.

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


/* Defining Scheme functions implemented by C functions --- subrs.  */

#include "libguile/gh.h"

#if SCM_ENABLE_DEPRECATED

/* allows you to define new scheme primitives written in C */
SCM
gh_new_procedure (const char *proc_name, SCM (*fn) (),
		  int n_required_args, int n_optional_args, int varp)
{
  return scm_c_define_gsubr (proc_name, n_required_args, n_optional_args,
			     varp, fn);
}

SCM
gh_new_procedure0_0 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 0, 0, 0);
}

SCM
gh_new_procedure0_1 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 0, 1, 0);
}

SCM
gh_new_procedure0_2 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 0, 2, 0);
}

SCM
gh_new_procedure1_0 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 1, 0, 0);
}

SCM
gh_new_procedure1_1 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 1, 1, 0);
}

SCM
gh_new_procedure1_2 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 1, 2, 0);
}

SCM
gh_new_procedure2_0 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 2, 0, 0);
}

SCM
gh_new_procedure2_1 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 2, 1, 0);
}

SCM
gh_new_procedure2_2 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 2, 2, 0);
}

SCM
gh_new_procedure3_0 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 3, 0, 0);
}

SCM
gh_new_procedure4_0 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 4, 0, 0);
}

SCM
gh_new_procedure5_0 (const char *proc_name, SCM (*fn) ())
{
  return gh_new_procedure (proc_name, fn, 5, 0, 0);
}

/* some (possibly most) Scheme functions available from C */
SCM
gh_define (const char *name, SCM val)
{
  scm_c_define (name, val);
  return SCM_UNSPECIFIED;
}


/* Calling Scheme functions from C.  */

SCM
gh_apply (SCM proc, SCM args)
{
  return scm_apply (proc, args, SCM_EOL);
}

SCM
gh_call0 (SCM proc)
{
  return scm_apply (proc, SCM_EOL, SCM_EOL);
}

SCM
gh_call1 (SCM proc, SCM arg)
{
  return scm_apply (proc, arg, scm_listofnull);
}

SCM
gh_call2 (SCM proc, SCM arg1, SCM arg2)
{
  return scm_apply (proc, arg1, scm_cons (arg2, scm_listofnull));
}

SCM
gh_call3 (SCM proc, SCM arg1, SCM arg2, SCM arg3)
{
  return scm_apply (proc, arg1, scm_cons2 (arg2, arg3, scm_listofnull));
}

#endif /* SCM_ENABLE_DEPRECATED */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
