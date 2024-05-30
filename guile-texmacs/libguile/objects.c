/* Copyright (C) 1995,1996,1999,2000,2001, 2003, 2004, 2006, 2008 Free Software Foundation, Inc.
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




/* This file and objects.h contains those minimal pieces of the Guile
 * Object Oriented Programming System which need to be included in
 * libguile.  See the comments in objects.h.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libguile/_scm.h"

#include "libguile/struct.h"
#include "libguile/procprop.h"
#include "libguile/chars.h"
#include "libguile/keywords.h"
#include "libguile/smob.h"
#include "libguile/eval.h"
#include "libguile/alist.h"
#include "libguile/ports.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/objects.h"
#include "libguile/goops.h"



SCM scm_metaclass_standard;
SCM scm_metaclass_operator;

/* The cache argument for scm_mcache_lookup_cmethod has one of two possible
 * formats:
 *
 * Format #1:
 * (SCM_IM_DISPATCH ARGS N-SPECIALIZED
 *   #((TYPE1 ... ENV FORMALS FORM ...) ...)
 *   GF)
 *
 * Format #2:
 * (SCM_IM_HASH_DISPATCH ARGS N-SPECIALIZED HASHSET MASK
 *   #((TYPE1 ... ENV FORMALS FORM ...) ...)
 *   GF)
 *
 * ARGS is either a list of expressions, in which case they
 * are interpreted as the arguments of an application, or
 * a non-pair, which is interpreted as a single expression
 * yielding all arguments.
 *
 * SCM_IM_DISPATCH expressions in generic functions always
 * have ARGS = the symbol `args' or the iloc #@0-0.
 *
 * Need FORMALS in order to support varying arity.  This
 * also avoids the need for renaming of bindings.
 *
 * We should probably not complicate this mechanism by
 * introducing "optimizations" for getters and setters or
 * primitive methods.  Getters and setter will normally be
 * compiled into @slot-[ref|set!] or a procedure call.
 * They rely on the dispatch performed before executing
 * the code which contains them.
 *
 * We might want to use a more efficient representation of
 * this form in the future, perhaps after we have introduced
 * low-level support for syntax-case macros.
 */

SCM
scm_mcache_lookup_cmethod (SCM cache, SCM args)
{
  unsigned long i, mask, n, end;
  SCM ls, methods, z = SCM_CDDR (cache);
  n = scm_to_ulong (SCM_CAR (z)); /* maximum number of specializers */
  methods = SCM_CADR (z);

  if (scm_is_simple_vector (methods))
    {
      /* cache format #1: prepare for linear search */
      mask = -1;
      i = 0;
      end = SCM_SIMPLE_VECTOR_LENGTH (methods);
    }
  else
    {
      /* cache format #2: compute a hash value */
      unsigned long hashset = scm_to_ulong (methods);
      long j = n;
      z = SCM_CDDR (z);
      mask = scm_to_ulong (SCM_CAR (z));
      methods = SCM_CADR (z);
      i = 0;
      ls = args;
      if (!scm_is_null (ls))
	do
	  {
	    i += SCM_STRUCT_DATA (scm_class_of (SCM_CAR (ls)))
		 [scm_si_hashsets + hashset];
	    ls = SCM_CDR (ls);
	  }
	while (j-- && !scm_is_null (ls));
      i &= mask;
      end = i;
    }

  /* Search for match  */
  do
    {
      long j = n;
      z = SCM_SIMPLE_VECTOR_REF (methods, i);
      ls = args; /* list of arguments */
      if (!scm_is_null (ls))
	do
	  {
	    /* More arguments than specifiers => CLASS != ENV */
	    if (! scm_is_eq (scm_class_of (SCM_CAR (ls)), SCM_CAR (z)))
	      goto next_method;
	    ls = SCM_CDR (ls);
	    z = SCM_CDR (z);
	  }
	while (j-- && !scm_is_null (ls));
      /* Fewer arguments than specifiers => CAR != ENV */
      if (scm_is_null (SCM_CAR (z)) || scm_is_pair (SCM_CAR (z)))
	return z;
    next_method:
      i = (i + 1) & mask;
    } while (i != end);
  return SCM_BOOL_F;
}

SCM
scm_mcache_compute_cmethod (SCM cache, SCM args)
{
  SCM cmethod = scm_mcache_lookup_cmethod (cache, args);
  if (scm_is_false (cmethod))
    /* No match - memoize */
    return scm_memoize_method (cache, args);
  return cmethod;
}

SCM
scm_apply_generic (SCM gf, SCM args)
{
  SCM cmethod = scm_mcache_compute_cmethod (SCM_ENTITY_PROCEDURE (gf), args);
  return scm_eval_body (SCM_CDR (SCM_CMETHOD_CODE (cmethod)),
			SCM_EXTEND_ENV (SCM_CAR (SCM_CMETHOD_CODE (cmethod)),
					args,
					SCM_CMETHOD_ENV (cmethod)));
}

SCM
scm_call_generic_0 (SCM gf)
{
  return scm_apply_generic (gf, SCM_EOL);
}

SCM
scm_call_generic_1 (SCM gf, SCM a1)
{
  return scm_apply_generic (gf, scm_list_1 (a1));
}

SCM
scm_call_generic_2 (SCM gf, SCM a1, SCM a2)
{
  return scm_apply_generic (gf, scm_list_2 (a1, a2));
}

SCM
scm_call_generic_3 (SCM gf, SCM a1, SCM a2, SCM a3)
{
  return scm_apply_generic (gf, scm_list_3 (a1, a2, a3));
}

SCM_DEFINE (scm_entity_p, "entity?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is an entity.")
#define FUNC_NAME s_scm_entity_p
{
  return scm_from_bool(SCM_STRUCTP (obj) && SCM_I_ENTITYP (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_operator_p, "operator?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is an operator.")
#define FUNC_NAME s_scm_operator_p
{
  return scm_from_bool(SCM_STRUCTP (obj)
                  && SCM_I_OPERATORP (obj)
                  && !SCM_I_ENTITYP (obj));
}
#undef FUNC_NAME

/* XXX - What code requires the object procedure to be only of certain
         types? */

SCM_DEFINE (scm_valid_object_procedure_p, "valid-object-procedure?", 1, 0, 0,
	    (SCM proc),
	    "Return @code{#t} iff @var{proc} is a procedure that can be used "
	    "with @code{set-object-procedure}.  It is always valid to use "
            "a closure constructed by @code{lambda}.")
#define FUNC_NAME s_scm_valid_object_procedure_p
{
  if (SCM_IMP (proc))
    return SCM_BOOL_F;
  switch (SCM_TYP7 (proc))
    {
    default:
      return SCM_BOOL_F;
    case scm_tcs_closures:
    case scm_tc7_subr_1:
    case scm_tc7_subr_2:
    case scm_tc7_subr_3:
    case scm_tc7_lsubr_2:
      return SCM_BOOL_T;
    }
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_object_procedure_x, "set-object-procedure!", 2, 0, 0, 
            (SCM obj, SCM proc),
	    "Set the object procedure of @var{obj} to @var{proc}.\n"
	    "@var{obj} must be either an entity or an operator.")
#define FUNC_NAME s_scm_set_object_procedure_x
{
  SCM_ASSERT (SCM_STRUCTP (obj)
	      && ((SCM_CLASS_FLAGS (obj) & SCM_CLASSF_OPERATOR)
		  || (SCM_I_ENTITYP (obj)
		      && !(SCM_OBJ_CLASS_FLAGS (obj)
			   & SCM_CLASSF_PURE_GENERIC))),
	      obj,
	      SCM_ARG1,
              FUNC_NAME);
  SCM_ASSERT (scm_valid_object_procedure_p (proc), proc, SCM_ARG2, FUNC_NAME);
  if (SCM_I_ENTITYP (obj))
    SCM_SET_ENTITY_PROCEDURE (obj, proc);
  else
    SCM_OPERATOR_CLASS (obj)->procedure = proc;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#ifdef GUILE_DEBUG
SCM_DEFINE (scm_object_procedure, "object-procedure", 1, 0, 0, 
            (SCM obj),
	    "Return the object procedure of @var{obj}. @var{obj} must be\n"
	    "an entity or an operator.")
#define FUNC_NAME s_scm_object_procedure
{
  SCM_ASSERT (SCM_STRUCTP (obj)
	      && ((SCM_CLASS_FLAGS (obj) & SCM_CLASSF_OPERATOR)
		  || SCM_I_ENTITYP (obj)),
	      obj, SCM_ARG1, FUNC_NAME);
  return (SCM_I_ENTITYP (obj)
	  ? SCM_ENTITY_PROCEDURE (obj)
	  : SCM_OPERATOR_CLASS (obj)->procedure);
}
#undef FUNC_NAME
#endif /* GUILE_DEBUG */

/* The following procedures are not a part of Goops but a minimal
 * object system built upon structs.  They are here for those who
 * want to implement their own object system.
 */

SCM
scm_i_make_class_object (SCM meta,
			 SCM layout_string,
			 unsigned long flags)
{
  SCM c;
  SCM layout = scm_make_struct_layout (layout_string);
  c = scm_make_struct (meta,
		       SCM_INUM0,
		       scm_list_4 (layout, SCM_BOOL_F, SCM_EOL, SCM_EOL));
  SCM_SET_CLASS_FLAGS (c, flags);
  return c;
}

SCM_DEFINE (scm_make_class_object, "make-class-object", 2, 0, 0, 
            (SCM metaclass, SCM layout),
	    "Create a new class object of class @var{metaclass}, with the\n"
	    "slot layout specified by @var{layout}.")
#define FUNC_NAME s_scm_make_class_object
{
  unsigned long flags = 0;
  SCM_VALIDATE_STRUCT (1, metaclass);
  SCM_VALIDATE_STRING (2, layout);
  if (scm_is_eq (metaclass, scm_metaclass_operator))
    flags = SCM_CLASSF_OPERATOR;
  return scm_i_make_class_object (metaclass, layout, flags);
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_subclass_object, "make-subclass-object", 2, 0, 0, 
            (SCM class, SCM layout),
	    "Create a subclass object of @var{class}, with the slot layout\n"
	    "specified by @var{layout}.")
#define FUNC_NAME s_scm_make_subclass_object
{
  SCM pl;
  SCM_VALIDATE_STRUCT (1, class);
  SCM_VALIDATE_STRING (2, layout);
  pl = SCM_PACK (SCM_STRUCT_DATA (class) [scm_vtable_index_layout]);
  pl = scm_symbol_to_string (pl);
  return scm_i_make_class_object (SCM_STRUCT_VTABLE (class),
				  scm_string_append (scm_list_2 (pl, layout)),
				  SCM_CLASS_FLAGS (class));
}
#undef FUNC_NAME

void
scm_init_objects ()
{
  SCM ms = scm_from_locale_string (SCM_METACLASS_STANDARD_LAYOUT);
  SCM mt = scm_make_vtable_vtable (ms, SCM_INUM0,
				   scm_list_3 (SCM_BOOL_F, SCM_EOL, SCM_EOL));
  
  SCM os = scm_from_locale_string (SCM_METACLASS_OPERATOR_LAYOUT);
  SCM ot = scm_make_vtable_vtable (os, SCM_INUM0,
				   scm_list_3 (SCM_BOOL_F, SCM_EOL, SCM_EOL));
  
  SCM es = scm_from_locale_string (SCM_ENTITY_LAYOUT);
  SCM el = scm_make_struct_layout (es);
  SCM et = scm_make_struct (mt, SCM_INUM0,
			    scm_list_4 (el, SCM_BOOL_F, SCM_EOL, SCM_EOL));

  scm_c_define ("<class>", mt);
  scm_metaclass_standard = mt;
  scm_c_define ("<operator-class>", ot);
  scm_metaclass_operator = ot;
  SCM_SET_CLASS_FLAGS (et, SCM_CLASSF_OPERATOR | SCM_CLASSF_ENTITY);
  SCM_SET_CLASS_DESTRUCTOR (et, scm_struct_free_entity);
  scm_c_define ("<entity>", et);

#include "libguile/objects.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
