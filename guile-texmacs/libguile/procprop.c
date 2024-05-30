/* Copyright (C) 1995,1996,1998,2000,2001,2003,2004, 2006, 2008 Free Software Foundation, Inc.
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

#include "libguile/alist.h"
#include "libguile/eval.h"
#include "libguile/procs.h"
#include "libguile/gsubr.h"
#include "libguile/objects.h"
#include "libguile/smob.h"
#include "libguile/root.h"
#include "libguile/vectors.h"
#include "libguile/hashtab.h"

#include "libguile/validate.h"
#include "libguile/procprop.h"


SCM_GLOBAL_SYMBOL (scm_sym_system_procedure, "system-procedure");
SCM_GLOBAL_SYMBOL (scm_sym_arity, "arity");

SCM
scm_i_procedure_arity (SCM proc)
{
  int a = 0, o = 0, r = 0;
  if (SCM_IMP (proc))
    return SCM_BOOL_F;
 loop:
  switch (SCM_TYP7 (proc))
    {
    case scm_tc7_subr_1o:
      o = 1;
    case scm_tc7_subr_0:
      break;
    case scm_tc7_subr_2o:
      o = 1;
    case scm_tc7_subr_1:
    case scm_tc7_dsubr:
    case scm_tc7_cxr:
      a += 1;
      break;
    case scm_tc7_subr_2:
      a += 2;
      break;
    case scm_tc7_subr_3:
      a += 3;
      break;
    case scm_tc7_asubr:
    case scm_tc7_rpsubr:
    case scm_tc7_lsubr:
      r = 1;
      break;
    case scm_tc7_lsubr_2:
      a += 2;
      r = 1;
      break;
    case scm_tc7_smob:
      if (SCM_SMOB_APPLICABLE_P (proc))
	{
	  int type = SCM_SMOB_DESCRIPTOR (proc).gsubr_type;
	  a += SCM_GSUBR_REQ (type);
	  o = SCM_GSUBR_OPT (type);
	  r = SCM_GSUBR_REST (type);
	  break;
	}
      else
	{
	  return SCM_BOOL_F;
	}
    case scm_tc7_cclo:
      if (scm_is_eq (SCM_CCLO_SUBR (proc), scm_f_gsubr_apply))
	{
	  int type = scm_to_int (SCM_GSUBR_TYPE (proc));
	  a += SCM_GSUBR_REQ (type);
	  o = SCM_GSUBR_OPT (type);
	  r = SCM_GSUBR_REST (type);
	  break;
	}
      else
	{
	  proc = SCM_CCLO_SUBR (proc);
	  a -= 1;
	  goto loop;
	}
    case scm_tc7_pws:
      proc = SCM_PROCEDURE (proc);
      goto loop;
    case scm_tcs_closures:
      proc = SCM_CLOSURE_FORMALS (proc);
      if (scm_is_null (proc))
	break;
      while (scm_is_pair (proc))
	{
	  ++a;
	  proc = SCM_CDR (proc);
	}
      if (!scm_is_null (proc))
	r = 1;
      break;
    case scm_tcs_struct:
      if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	{
	  r = 1;
	  break;
	}
      else if (!SCM_I_OPERATORP (proc))
	return SCM_BOOL_F;
      proc = (SCM_I_ENTITYP (proc)
	      ? SCM_ENTITY_PROCEDURE (proc)
	      : SCM_OPERATOR_PROCEDURE (proc));
      a -= 1;
      goto loop;
    default:
      return SCM_BOOL_F;
    }
  return scm_list_3 (scm_from_int (a), scm_from_int (o), scm_from_bool(r));
}

/* XXX - instead of using a stand-in value for everything except
   closures, we should find other ways to store the procedure
   properties for those other kinds of procedures.  For example, subrs
   have their own property slot, which is unused at present.
*/

static SCM
scm_stand_in_scm_proc(SCM proc)
{
  SCM handle, answer;
  handle = scm_hashq_get_handle (scm_stand_in_procs, proc);
  if (scm_is_false (handle))
    {
      answer = scm_closure (scm_list_2 (SCM_EOL, SCM_BOOL_F), SCM_EOL);
      scm_hashq_set_x (scm_stand_in_procs, proc, answer);
    }
  else
    answer = SCM_CDR (handle);
  return answer;
}

SCM_DEFINE (scm_procedure_properties, "procedure-properties", 1, 0, 0, 
           (SCM proc),
	    "Return @var{obj}'s property list.")
#define FUNC_NAME s_scm_procedure_properties
{
  SCM_VALIDATE_PROC (1, proc);
  return scm_acons (scm_sym_arity, scm_i_procedure_arity (proc),
		    SCM_PROCPROPS (SCM_CLOSUREP (proc)
				   ? proc
				   : scm_stand_in_scm_proc (proc)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_procedure_properties_x, "set-procedure-properties!", 2, 0, 0,
           (SCM proc, SCM new_val),
	    "Set @var{obj}'s property list to @var{alist}.")
#define FUNC_NAME s_scm_set_procedure_properties_x
{
  if (!SCM_CLOSUREP (proc))
    proc = scm_stand_in_scm_proc(proc);
  SCM_VALIDATE_CLOSURE (1, proc);
  SCM_SETPROCPROPS (proc, new_val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_procedure_property, "procedure-property", 2, 0, 0,
           (SCM p, SCM k),
	    "Return the property of @var{obj} with name @var{key}.")
#define FUNC_NAME s_scm_procedure_property
{
  SCM assoc;
  if (scm_is_eq (k, scm_sym_arity))
    {
      SCM arity;
      SCM_ASSERT (scm_is_true (arity = scm_i_procedure_arity (p)),
		  p, SCM_ARG1, FUNC_NAME);
      return arity;
    }
  SCM_VALIDATE_PROC (1, p);
  assoc = scm_sloppy_assq (k,
			   SCM_PROCPROPS (SCM_CLOSUREP (p)
					  ? p
					  : scm_stand_in_scm_proc (p)));
  return (SCM_NIMP (assoc) ? SCM_CDR (assoc) : SCM_BOOL_F);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_procedure_property_x, "set-procedure-property!", 3, 0, 0,
           (SCM p, SCM k, SCM v),
	    "In @var{obj}'s property list, set the property named @var{key} to\n"
	    "@var{value}.")
#define FUNC_NAME s_scm_set_procedure_property_x
{
  SCM assoc;
  if (!SCM_CLOSUREP (p))
    p = scm_stand_in_scm_proc(p);
  SCM_VALIDATE_CLOSURE (1, p);
  if (scm_is_eq (k, scm_sym_arity))
    SCM_MISC_ERROR ("arity is a read-only property", SCM_EOL);
  assoc = scm_sloppy_assq (k, SCM_PROCPROPS (p));
  if (SCM_NIMP (assoc))
    SCM_SETCDR (assoc, v);
  else
    SCM_SETPROCPROPS (p, scm_acons (k, v, SCM_PROCPROPS (p)));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




void
scm_init_procprop ()
{
#include "libguile/procprop.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
