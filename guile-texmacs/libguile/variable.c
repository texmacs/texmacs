/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2006, 2008 Free Software Foundation, Inc.
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
#include "libguile/eq.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/deprecation.h"

#include "libguile/validate.h"
#include "libguile/variable.h"


void
scm_i_variable_print (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<variable ", port);
  scm_uintprint (SCM_UNPACK (exp), 16, port);
  scm_puts (" value: ", port);
  scm_iprin1 (SCM_VARIABLE_REF (exp), port, pstate);
  scm_putc('>', port);
}



static SCM
make_variable (SCM init)
{
  return scm_cell (scm_tc7_variable, SCM_UNPACK (init));
}

SCM_DEFINE (scm_make_variable, "make-variable", 1, 0, 0, 
            (SCM init),
            "Return a variable initialized to value @var{init}.")
#define FUNC_NAME s_scm_make_variable
{
  return make_variable (init);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_undefined_variable, "make-undefined-variable", 0, 0, 0, 
            (),
            "Return a variable that is initially unbound.")
#define FUNC_NAME s_scm_make_undefined_variable
{
  return make_variable (SCM_UNDEFINED);
}
#undef FUNC_NAME


SCM_DEFINE (scm_variable_p, "variable?", 1, 0, 0, 
            (SCM obj),
            "Return @code{#t} iff @var{obj} is a variable object, else\n"
	    "return @code{#f}.")
#define FUNC_NAME s_scm_variable_p
{
  return scm_from_bool (SCM_VARIABLEP (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_variable_ref, "variable-ref", 1, 0, 0, 
            (SCM var),
            "Dereference @var{var} and return its value.\n"
            "@var{var} must be a variable object; see @code{make-variable}\n"
	    "and @code{make-undefined-variable}.")
#define FUNC_NAME s_scm_variable_ref
{
  SCM val;
  SCM_VALIDATE_VARIABLE (1, var);
  val = SCM_VARIABLE_REF (var);
  if (val == SCM_UNDEFINED)
    SCM_MISC_ERROR ("variable is unbound: ~S", scm_list_1 (var));
  return val;
}
#undef FUNC_NAME

SCM_DEFINE (scm_variable_set_x, "variable-set!", 2, 0, 0,
            (SCM var, SCM val),
            "Set the value of the variable @var{var} to @var{val}.\n"
            "@var{var} must be a variable object, @var{val} can be any\n"
	    "value. Return an unspecified value.")
#define FUNC_NAME s_scm_variable_set_x
{
  SCM_VALIDATE_VARIABLE (1, var);
  SCM_VARIABLE_SET (var, val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_variable_bound_p, "variable-bound?", 1, 0, 0, 
            (SCM var),
            "Return @code{#t} iff @var{var} is bound to a value.\n"
            "Throws an error if @var{var} is not a variable object.")
#define FUNC_NAME s_scm_variable_bound_p
{
  SCM_VALIDATE_VARIABLE (1, var);
  return scm_from_bool (SCM_VARIABLE_REF (var) != SCM_UNDEFINED);
}
#undef FUNC_NAME


void
scm_init_variable ()
{
#include "libguile/variable.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
