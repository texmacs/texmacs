/* Copyright (C) 2000, 2001, 2006, 2008 Free Software Foundation, Inc.
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

#include "libguile/_scm.h"
#include "libguile/eval.h"
#include "libguile/feature.h"
#include "libguile/gc.h"
#include "libguile/numbers.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/struct.h"
#include "libguile/validate.h"

#include "libguile/values.h"

SCM scm_values_vtable;

static SCM
print_values (SCM obj, SCM pwps)
{
  SCM values = scm_struct_ref (obj, SCM_INUM0);
  SCM port = SCM_PORT_WITH_PS_PORT (pwps);
  scm_print_state *ps = SCM_PRINT_STATE (SCM_PORT_WITH_PS_PS (pwps));

  scm_puts ("#<values ", port);
  scm_iprin1 (values, port, ps);
  scm_puts (">", port);

  return SCM_UNSPECIFIED;
}

SCM_DEFINE (scm_values, "values", 0, 0, 1,
	    (SCM args),
	    "Delivers all of its arguments to its continuation.  Except for\n"
	    "continuations created by the @code{call-with-values} procedure,\n"
	    "all continuations take exactly one value.  The effect of\n"
	    "passing no value or more than one value to continuations that\n"
	    "were not created by @code{call-with-values} is unspecified.")
#define FUNC_NAME s_scm_values
{
  long n;
  SCM result;

  SCM_VALIDATE_LIST_COPYLEN (1, args, n);
  if (n == 1)
    result = SCM_CAR (args);
  else
    {
      result = scm_make_struct (scm_values_vtable, SCM_INUM0,
				scm_list_1 (args));
    }

  return result;
}
#undef FUNC_NAME

void
scm_init_values (void)
{
  SCM print = scm_c_define_subr ("%print-values", scm_tc7_subr_2,
				 print_values);

  scm_values_vtable 
    = scm_permanent_object (
        scm_make_vtable_vtable (scm_from_locale_string ("pr"),
				SCM_INUM0, SCM_EOL));

  SCM_SET_STRUCT_PRINTER (scm_values_vtable, print);

  scm_add_feature ("values");

#include "libguile/values.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
