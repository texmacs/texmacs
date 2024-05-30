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


/* routines to evaluate Scheme code */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libguile/gh.h"

#if SCM_ENABLE_DEPRECATED

typedef SCM (*gh_eval_t) (void *data, SCM jmpbuf);

/* Evaluate the string; toss the value.  */
SCM
gh_eval_str (const char *scheme_code)
{
  return scm_c_eval_string (scheme_code);
}

/* evaluate the file by passing it to the lower level scm_primitive_load() */
SCM
gh_eval_file (const char *fname)
{
  return scm_primitive_load (gh_str02scm (fname));
}

static SCM
eval_str_wrapper (void *data)
{
/*   gh_eval_t real_eval_proc = (gh_eval_t) (* ((gh_eval_t *) data)); */

  char *scheme_code = (char *) data;
  return gh_eval_str (scheme_code);
}

SCM
gh_eval_str_with_catch (const char *scheme_code, scm_t_catch_handler handler)
{
  /* FIXME: not there yet */
  return gh_catch (SCM_BOOL_T, (scm_t_catch_body) eval_str_wrapper, (void *) scheme_code,
		   (scm_t_catch_handler) handler, (void *) scheme_code);
}

SCM
gh_eval_str_with_standard_handler (const char *scheme_code)
{
  return gh_eval_str_with_catch (scheme_code, gh_standard_handler);
}

SCM
gh_eval_str_with_stack_saving_handler (const char *scheme_code)
{
  return scm_internal_stack_catch (SCM_BOOL_T,
				   (scm_t_catch_body) eval_str_wrapper,
				   (void *) scheme_code,
				   (scm_t_catch_handler)
				   gh_standard_handler,
				   (void *) scheme_code);
}

static SCM
eval_file_wrapper (void *data)
{
/*   gh_eval_t real_eval_proc = (gh_eval_t) (* ((gh_eval_t *) data)); */

  char *scheme_code = (char *) data;
  return gh_eval_file (scheme_code);
}

SCM
gh_eval_file_with_catch (const char *scheme_code, scm_t_catch_handler handler)
{
  /* FIXME: not there yet */
  return gh_catch (SCM_BOOL_T, (scm_t_catch_body) eval_file_wrapper,
		   (void *) scheme_code, (scm_t_catch_handler) handler,
		   (void *) scheme_code);
}

SCM
gh_eval_file_with_standard_handler (const char *scheme_code)
{
  return gh_eval_file_with_catch (scheme_code, gh_standard_handler);
}

#endif /* SCM_ENABLE_DEPRECATED */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
