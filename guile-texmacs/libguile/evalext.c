/* Copyright (C) 1998,1999,2000,2001,2002,2003, 2006, 2008 Free Software Foundation, Inc.
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
#include "libguile/fluids.h"
#include "libguile/modules.h"

#include "libguile/validate.h"
#include "libguile/evalext.h"

SCM_DEFINE (scm_defined_p, "defined?", 1, 1, 0,
            (SCM sym, SCM env),
	    "Return @code{#t} if @var{sym} is defined in the lexical "
	    "environment @var{env}.  When @var{env} is not specified, "
	    "look in the top-level environment as defined by the "
	    "current module.")
#define FUNC_NAME s_scm_defined_p
{
  SCM var;

  SCM_VALIDATE_SYMBOL (1, sym);

  if (SCM_UNBNDP (env))
    var = scm_sym2var (sym, scm_current_module_lookup_closure (),
			 SCM_BOOL_F);
  else
    {
      SCM frames = env;
      register SCM b;
      for (; SCM_NIMP (frames); frames = SCM_CDR (frames))
	{
	  SCM_ASSERT (scm_is_pair (frames), env, SCM_ARG2, FUNC_NAME);
	  b = SCM_CAR (frames);
	  if (scm_is_true (scm_procedure_p (b)))
	    break;
	  SCM_ASSERT (scm_is_pair (b), env, SCM_ARG2, FUNC_NAME);
	  for (b = SCM_CAR (b); SCM_NIMP (b); b = SCM_CDR (b))
	    {
	      if (!scm_is_pair (b))
		{
		  if (scm_is_eq (b, sym))
		    return SCM_BOOL_T;
		  else
		    break;
		}
	      if (scm_is_eq (SCM_CAR (b), sym))
		return SCM_BOOL_T;
	    }
	}
      var = scm_sym2var (sym,
			 SCM_NIMP (frames) ? SCM_CAR (frames) : SCM_BOOL_F,
			 SCM_BOOL_F);
    }
	      
  return (scm_is_false (var) || SCM_UNBNDP (SCM_VARIABLE_REF (var))
	  ? SCM_BOOL_F
	  : SCM_BOOL_T);
}
#undef FUNC_NAME


SCM_REGISTER_PROC (s_map_in_order, "map-in-order", 2, 0, 1, scm_map);


SCM_DEFINE (scm_self_evaluating_p, "self-evaluating?", 1, 0, 0,
	    (SCM obj),
	    "Return #t for objects which Guile considers self-evaluating")
#define FUNC_NAME s_scm_self_evaluating_p
{
  switch (SCM_ITAG3 (obj))
    {
    case scm_tc3_int_1:
    case scm_tc3_int_2:
      /* inum */
      return SCM_BOOL_T;
    case scm_tc3_imm24:
	/* characters, booleans, other immediates */
      return scm_from_bool (!scm_is_null (obj));
    case scm_tc3_cons:
      switch (SCM_TYP7 (obj))
	{
	case scm_tcs_closures:
	case scm_tc7_vector:
	case scm_tc7_wvect:
	case scm_tc7_number:
	case scm_tc7_string:
	case scm_tc7_smob:
	case scm_tc7_cclo:
	case scm_tc7_pws:
	case scm_tcs_subrs:
	case scm_tcs_struct:
	  return SCM_BOOL_T;
	default:
	  return SCM_BOOL_F;
	}
    }
  SCM_MISC_ERROR ("Internal error: Object ~S has unknown type",
		  scm_list_1 (obj));
  return SCM_UNSPECIFIED; /* never reached */
}
#undef FUNC_NAME

void 
scm_init_evalext ()
{
#include "libguile/evalext.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
