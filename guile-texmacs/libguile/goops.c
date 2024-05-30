/* Copyright (C) 1998,1999,2000,2001,2002,2003,2004,2008
 * Free Software Foundation, Inc.
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


/* This software is a derivative work of other copyrighted softwares; the
 * copyright notices of these softwares are placed in the file COPYRIGHTS
 *
 * This file is based upon stklos.c from the STk distribution by
 * Erick Gallesio <eg@unice.fr>.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>
#include <assert.h>

#include "libguile/_scm.h"
#include "libguile/alist.h"
#include "libguile/async.h"
#include "libguile/chars.h"
#include "libguile/debug.h"
#include "libguile/dynl.h"
#include "libguile/dynwind.h"
#include "libguile/eval.h"
#include "libguile/hashtab.h"
#include "libguile/keywords.h"
#include "libguile/macros.h"
#include "libguile/modules.h"
#include "libguile/objects.h"
#include "libguile/ports.h"
#include "libguile/procprop.h"
#include "libguile/random.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/strings.h"
#include "libguile/strports.h"
#include "libguile/vectors.h"
#include "libguile/weaks.h"

#include "libguile/validate.h"
#include "libguile/goops.h"

#define SPEC_OF(x)  SCM_SLOT (x, scm_si_specializers)

#define DEFVAR(v, val) \
{ scm_eval (scm_list_3 (scm_sym_define_public, (v), (val)), \
	    scm_module_goops); }
/* Temporary hack until we get the new module system */
/*fixme* Should optimize by keeping track of the variable object itself */
#define GETVAR(v) (SCM_VARIABLE_REF (scm_call_2 (scm_goops_lookup_closure,  \
						 (v), SCM_BOOL_F)))

/* Fixme: Should use already interned symbols */

#define CALL_GF1(name, a)	(scm_call_1 (GETVAR (scm_from_locale_symbol (name)), \
					     a))
#define CALL_GF2(name, a, b)	(scm_call_2 (GETVAR (scm_from_locale_symbol (name)), \
					     a, b))
#define CALL_GF3(name, a, b, c)	(scm_call_3 (GETVAR (scm_from_locale_symbol (name)), \
					     a, b, c))
#define CALL_GF4(name, a, b, c, d)	(scm_call_4 (GETVAR (scm_from_locale_symbol (name)), \
					     a, b, c, d))

/* Class redefinition protocol:

   A class is represented by a heap header h1 which points to a
   malloc:ed memory block m1.

   When a new version of a class is created, a new header h2 and
   memory block m2 are allocated.  The headers h1 and h2 then switch
   pointers so that h1 refers to m2 and h2 to m1.  In this way, names
   bound to h1 will point to the new class at the same time as h2 will
   be a handle which the GC will use to free m1.

   The `redefined' slot of m1 will be set to point to h1.  An old
   instance will have its class pointer (the CAR of the heap header)
   pointing to m1.  The non-immediate `redefined'-slot in m1 indicates
   the class modification and the new class pointer can be found via
   h1.
*/

/* The following definition is located in libguile/objects.h:
#define SCM_OBJ_CLASS_REDEF(x) (SCM_STRUCT_VTABLE_DATA(x)[scm_si_redefined])
*/

#define TEST_CHANGE_CLASS(obj, class)				       \
	{							       \
	  class = SCM_CLASS_OF (obj);				       \
          if (scm_is_true (SCM_OBJ_CLASS_REDEF (obj)))		       \
	    {							       \
	      scm_change_object_class (obj, class, SCM_OBJ_CLASS_REDEF (obj));\
	      class = SCM_CLASS_OF (obj);			       \
	    }							       \
	}

#define NXT_MTHD_METHODS(m)	(SCM_VELTS (m)[1])
#define NXT_MTHD_ARGS(m)	(SCM_VELTS (m)[2])

#define SCM_GOOPS_UNBOUND SCM_UNBOUND
#define SCM_GOOPS_UNBOUNDP(x) ((x) == SCM_GOOPS_UNBOUND)

static int goops_loaded_p = 0;
static scm_t_rstate *goops_rstate;

static SCM scm_goops_lookup_closure;

/* These variables are filled in by the object system when loaded. */
SCM scm_class_boolean, scm_class_char, scm_class_pair;
SCM scm_class_procedure, scm_class_string, scm_class_symbol;
SCM scm_class_procedure_with_setter, scm_class_primitive_generic;
SCM scm_class_vector, scm_class_null;
SCM scm_class_integer, scm_class_real, scm_class_complex, scm_class_fraction;
SCM scm_class_unknown;
SCM scm_class_top, scm_class_object, scm_class_class;
SCM scm_class_applicable;
SCM scm_class_entity, scm_class_entity_with_setter;
SCM scm_class_generic, scm_class_generic_with_setter;
SCM scm_class_accessor;
SCM scm_class_extended_generic, scm_class_extended_generic_with_setter;
SCM scm_class_extended_accessor;
SCM scm_class_method;
SCM scm_class_simple_method, scm_class_accessor_method;
SCM scm_class_procedure_class;
SCM scm_class_operator_class, scm_class_operator_with_setter_class;
SCM scm_class_entity_class;
SCM scm_class_number, scm_class_list;
SCM scm_class_keyword;
SCM scm_class_port, scm_class_input_output_port;
SCM scm_class_input_port, scm_class_output_port;
SCM scm_class_foreign_class, scm_class_foreign_object;
SCM scm_class_foreign_slot;
SCM scm_class_self, scm_class_protected;
SCM scm_class_opaque, scm_class_read_only;
SCM scm_class_protected_opaque, scm_class_protected_read_only;
SCM scm_class_scm;
SCM scm_class_int, scm_class_float, scm_class_double;

SCM *scm_port_class = 0;
SCM *scm_smob_class = 0;

SCM scm_no_applicable_method;

SCM_SYMBOL (scm_sym_define_public, "define-public");

static SCM scm_make_unbound (void);
static SCM scm_unbound_p (SCM obj);
static SCM scm_assert_bound (SCM value, SCM obj);
static SCM scm_at_assert_bound_ref (SCM obj, SCM index);
static SCM scm_sys_goops_loaded (void);

/* This function is used for efficient type dispatch.  */
SCM_DEFINE (scm_class_of, "class-of", 1, 0, 0,
	    (SCM x),
	    "Return the class of @var{x}.")
#define FUNC_NAME s_scm_class_of
{
  switch (SCM_ITAG3 (x))
    {
    case scm_tc3_int_1:
    case scm_tc3_int_2:
      return scm_class_integer;

    case scm_tc3_imm24:
      if (SCM_CHARP (x))
	return scm_class_char;
      else if (scm_is_bool (x))
        return scm_class_boolean;
      else if (scm_is_null (x))
        return scm_class_null;
      else
        return scm_class_unknown;

    case scm_tc3_cons:
      switch (SCM_TYP7 (x))
	{
	case scm_tcs_cons_nimcar:
	  return scm_class_pair;
	case scm_tcs_closures:
	  return scm_class_procedure;
	case scm_tc7_symbol:
	  return scm_class_symbol;
	case scm_tc7_vector:
	case scm_tc7_wvect:
	  return scm_class_vector;
	case scm_tc7_string:
	  return scm_class_string;
        case scm_tc7_number:
          switch SCM_TYP16 (x) {
          case scm_tc16_big:
            return scm_class_integer;
          case scm_tc16_real:
            return scm_class_real;
          case scm_tc16_complex:
            return scm_class_complex;
	  case scm_tc16_fraction:
	    return scm_class_fraction;
          }
	case scm_tc7_asubr:
	case scm_tc7_subr_0:
	case scm_tc7_subr_1:
	case scm_tc7_dsubr:
	case scm_tc7_cxr:
	case scm_tc7_subr_3:
	case scm_tc7_subr_2:
	case scm_tc7_rpsubr:
	case scm_tc7_subr_1o:
	case scm_tc7_subr_2o:
	case scm_tc7_lsubr_2:
	case scm_tc7_lsubr:
	  if (SCM_SUBR_GENERIC (x) && *SCM_SUBR_GENERIC (x))
	    return scm_class_primitive_generic;
	  else
	    return scm_class_procedure;
	case scm_tc7_cclo:
	  return scm_class_procedure;
	case scm_tc7_pws:
	  return scm_class_procedure_with_setter;

	case scm_tc7_smob:
	  {
	    scm_t_bits type = SCM_TYP16 (x);
	    if (type != scm_tc16_port_with_ps)
	      return scm_smob_class[SCM_TC2SMOBNUM (type)];
	    x = SCM_PORT_WITH_PS_PORT (x);
	    /* fall through to ports */
	  }
	case scm_tc7_port:
	  return scm_port_class[(SCM_WRTNG & SCM_CELL_WORD_0 (x)
				 ? (SCM_RDNG & SCM_CELL_WORD_0 (x)
				    ? SCM_INOUT_PCLASS_INDEX | SCM_PTOBNUM (x)
				    : SCM_OUT_PCLASS_INDEX | SCM_PTOBNUM (x))
				 : SCM_IN_PCLASS_INDEX | SCM_PTOBNUM (x))];
	case scm_tcs_struct:
	  if (SCM_OBJ_CLASS_FLAGS (x) & SCM_CLASSF_GOOPS_VALID)
	    return SCM_CLASS_OF (x);
	  else if (SCM_OBJ_CLASS_FLAGS (x) & SCM_CLASSF_GOOPS)
	    {
	      /* Goops object */
	      if (! scm_is_false (SCM_OBJ_CLASS_REDEF (x)))
		scm_change_object_class (x,
					 SCM_CLASS_OF (x),         /* old */
					 SCM_OBJ_CLASS_REDEF (x)); /* new */
	      return SCM_CLASS_OF (x);
	    }
	  else
	    {
	      /* ordinary struct */
	      SCM handle = scm_struct_create_handle (SCM_STRUCT_VTABLE (x));
	      if (scm_is_true (SCM_STRUCT_TABLE_CLASS (SCM_CDR (handle))))
		return SCM_STRUCT_TABLE_CLASS (SCM_CDR (handle));
	      else
		{
		  SCM name = SCM_STRUCT_TABLE_NAME (SCM_CDR (handle));
		  SCM class = scm_make_extended_class (scm_is_true (name)
						       ? scm_i_symbol_chars (name)
						       : 0,
						       SCM_I_OPERATORP (x));
		  SCM_SET_STRUCT_TABLE_CLASS (SCM_CDR (handle), class);
		  return class;
		}
	    }
	default:
	  if (scm_is_pair (x))
	    return scm_class_pair;
	  else
	    return scm_class_unknown;
	}

    case scm_tc3_struct:
    case scm_tc3_tc7_1:
    case scm_tc3_tc7_2:
    case scm_tc3_closure:
      /* Never reached */
      break;
    }
  return scm_class_unknown;
}
#undef FUNC_NAME

/******************************************************************************
 *
 * Compute-cpl
 *
 *   This version doesn't fully handle multiple-inheritance. It serves
 *   only for booting classes and will be overloaded in Scheme
 *
 ******************************************************************************/

static SCM
map (SCM (*proc) (SCM), SCM ls)
{
  if (scm_is_null (ls))
    return ls;
  else
    {
      SCM res = scm_cons (proc (SCM_CAR (ls)), SCM_EOL);
      SCM h = res;
      ls = SCM_CDR (ls);
      while (!scm_is_null (ls))
	{
	  SCM_SETCDR (h, scm_cons (proc (SCM_CAR (ls)), SCM_EOL));
	  h = SCM_CDR (h);
	  ls = SCM_CDR (ls);
	}
      return res;
    }
}

static SCM
filter_cpl (SCM ls)
{
  SCM res = SCM_EOL;
  while (!scm_is_null (ls))
    {
      SCM el = SCM_CAR (ls);
      if (scm_is_false (scm_c_memq (el, res)))
	res = scm_cons (el, res);
      ls = SCM_CDR (ls);
    }
  return res;
}

static SCM
compute_cpl (SCM class)
{
  if (goops_loaded_p)
    return CALL_GF1 ("compute-cpl", class);
  else
    {
      SCM supers = SCM_SLOT (class, scm_si_direct_supers);
      SCM ls = scm_append (scm_acons (class, supers,
				      map (compute_cpl, supers)));
      return scm_reverse_x (filter_cpl (ls), SCM_EOL);
    }
}

/******************************************************************************
 *
 * compute-slots
 *
 ******************************************************************************/

static SCM
remove_duplicate_slots (SCM l, SCM res, SCM slots_already_seen)
{
  SCM tmp;

  if (scm_is_null (l))
    return res;

  tmp = SCM_CAAR (l);
  if (!scm_is_symbol (tmp))
    scm_misc_error ("%compute-slots", "bad slot name ~S", scm_list_1 (tmp));

  if (scm_is_false (scm_c_memq (tmp, slots_already_seen))) {
    res 	       = scm_cons (SCM_CAR (l), res);
    slots_already_seen = scm_cons (tmp, slots_already_seen);
  }

  return remove_duplicate_slots (SCM_CDR (l), res, slots_already_seen);
}

static SCM
build_slots_list (SCM dslots, SCM cpl)
{
  register SCM res = dslots;

  for (cpl = SCM_CDR (cpl); !scm_is_null (cpl); cpl = SCM_CDR (cpl))
    res = scm_append (scm_list_2 (SCM_SLOT (SCM_CAR (cpl),
					    scm_si_direct_slots),
				  res));

  /* res contains a list of slots. Remove slots which appears more than once */
  return remove_duplicate_slots (scm_reverse (res), SCM_EOL, SCM_EOL);
}

static SCM
maplist (SCM ls)
{
  SCM orig = ls;
  while (!scm_is_null (ls))
    {
      if (!scm_is_pair (SCM_CAR (ls)))
	SCM_SETCAR (ls, scm_cons (SCM_CAR (ls), SCM_EOL));
      ls = SCM_CDR (ls);
    }
  return orig;
}


SCM_DEFINE (scm_sys_compute_slots, "%compute-slots", 1, 0, 0,
	    (SCM class),
	    "Return a list consisting of the names of all slots belonging to\n"
	    "class @var{class}, i. e. the slots of @var{class} and of all of\n"
	    "its superclasses.")
#define FUNC_NAME s_scm_sys_compute_slots
{
  SCM_VALIDATE_CLASS (1, class);
  return build_slots_list (SCM_SLOT (class, scm_si_direct_slots),
			   SCM_SLOT (class, scm_si_cpl));
}
#undef FUNC_NAME


/******************************************************************************
 *
 * compute-getters-n-setters
 *
 *   This version doesn't handle slot options. It serves only for booting
 * classes and will be overloaded in Scheme.
 *
 ******************************************************************************/

SCM_KEYWORD (k_init_value, "init-value");
SCM_KEYWORD (k_init_thunk, "init-thunk");

static SCM
compute_getters_n_setters (SCM slots)
{
  SCM res = SCM_EOL;
  SCM *cdrloc = &res;
  long i   = 0;

  for (  ; !scm_is_null (slots); slots = SCM_CDR (slots))
    {
      SCM init = SCM_BOOL_F;
      SCM options = SCM_CDAR (slots);
      if (!scm_is_null (options))
	{
	  init = scm_get_keyword (k_init_value, options, 0);
	  if (init)
            {
              init = scm_i_eval_x (scm_list_3 (scm_sym_lambda,
                                               SCM_EOL,
                                               scm_list_2 (scm_sym_quote,
                                                           init)),
                                   SCM_EOL);
            }
	  else
	    init = scm_get_keyword (k_init_thunk, options, SCM_BOOL_F);
	}
      *cdrloc = scm_cons (scm_cons (SCM_CAAR (slots),
				    scm_cons (init,
					      scm_from_int (i++))),
			  SCM_EOL);
      cdrloc = SCM_CDRLOC (*cdrloc);
    }
  return res;
}

/******************************************************************************
 *
 * initialize-object
 *
 ******************************************************************************/

/*fixme* Manufacture keywords in advance */
SCM
scm_i_get_keyword (SCM key, SCM l, long len, SCM default_value, const char *subr)
{
  long i;

  for (i = 0; i != len; i += 2)
    {
      SCM obj = SCM_CAR (l);

      if (!scm_is_keyword (obj))
	scm_misc_error (subr, "bad keyword: ~S", scm_list_1 (obj));
      else if (scm_is_eq (obj, key))
	return SCM_CADR (l);
      else
	l = SCM_CDDR (l);
    }

  return default_value;
}


SCM_DEFINE (scm_get_keyword, "get-keyword", 3, 0, 0,
	    (SCM key, SCM l, SCM default_value),
	    "Determine an associated value for the keyword @var{key} from\n"
	    "the list @var{l}.  The list @var{l} has to consist of an even\n"
	    "number of elements, where, starting with the first, every\n"
	    "second element is a keyword, followed by its associated value.\n"
	    "If @var{l} does not hold a value for @var{key}, the value\n"
	    "@var{default_value} is returned.")
#define FUNC_NAME s_scm_get_keyword
{
  long len;

  SCM_ASSERT (scm_is_keyword (key), key, SCM_ARG1, FUNC_NAME);
  len = scm_ilength (l);
  if (len < 0 || len % 2 == 1)
    scm_misc_error (FUNC_NAME, "Bad keyword-value list: ~S", scm_list_1 (l));

  return scm_i_get_keyword (key, l, len, default_value, FUNC_NAME);
}
#undef FUNC_NAME


SCM_KEYWORD (k_init_keyword, "init-keyword");

static SCM get_slot_value (SCM class, SCM obj, SCM slotdef);
static SCM set_slot_value (SCM class, SCM obj, SCM slotdef, SCM value);

SCM_DEFINE (scm_sys_initialize_object, "%initialize-object", 2, 0, 0,
	    (SCM obj, SCM initargs),
	    "Initialize the object @var{obj} with the given arguments\n"
	    "@var{initargs}.")
#define FUNC_NAME s_scm_sys_initialize_object
{
  SCM tmp, get_n_set, slots;
  SCM class       = SCM_CLASS_OF (obj);
  long n_initargs;

  SCM_VALIDATE_INSTANCE (1, obj);
  n_initargs = scm_ilength (initargs);
  SCM_ASSERT ((n_initargs & 1) == 0, initargs, SCM_ARG2, FUNC_NAME);

  get_n_set = SCM_SLOT (class, scm_si_getters_n_setters);
  slots     = SCM_SLOT (class, scm_si_slots);

  /* See for each slot how it must be initialized */
  for (;
       !scm_is_null (slots);
       get_n_set = SCM_CDR (get_n_set), slots = SCM_CDR (slots))
    {
      SCM slot_name  = SCM_CAR (slots);
      SCM slot_value = 0;

      if (!scm_is_null (SCM_CDR (slot_name)))
	{
	  /* This slot admits (perhaps) to be initialized at creation time */
	  long n = scm_ilength (SCM_CDR (slot_name));
	  if (n & 1) /* odd or -1 */
	    SCM_MISC_ERROR ("class contains bogus slot definition: ~S",
			    scm_list_1 (slot_name));
	  tmp 	= scm_i_get_keyword (k_init_keyword,
				     SCM_CDR (slot_name),
				     n,
				     0,
				     FUNC_NAME);
	  slot_name = SCM_CAR (slot_name);
	  if (tmp)
	    {
	      /* an initarg was provided for this slot */
	      if (!scm_is_keyword (tmp))
		SCM_MISC_ERROR ("initarg must be a keyword. It was ~S",
				scm_list_1 (tmp));
	      slot_value = scm_i_get_keyword (tmp,
					      initargs,
					      n_initargs,
					      0,
					      FUNC_NAME);
	    }
	}

      if (slot_value)
	/* set slot to provided value */
	set_slot_value (class, obj, SCM_CAR (get_n_set), slot_value);
      else
	{
	  /* set slot to its :init-form if it exists */
	  tmp = SCM_CADAR (get_n_set);
	  if (scm_is_true (tmp))
	    {
	      slot_value = get_slot_value (class, obj, SCM_CAR (get_n_set));
	      if (SCM_GOOPS_UNBOUNDP (slot_value))
		{
		  SCM env = SCM_EXTEND_ENV (SCM_EOL, SCM_EOL, SCM_ENV (tmp));
		  set_slot_value (class,
				  obj,
				  SCM_CAR (get_n_set),
				  scm_eval_body (SCM_CLOSURE_BODY (tmp), env));
		}
	    }
	}
    }

  return obj;
}
#undef FUNC_NAME

/* NOTE: The following macros are interdependent with code
 *       in goops.scm:compute-getters-n-setters
 */
#define SCM_GNS_INSTANCE_ALLOCATED_P(gns)	\
  (SCM_I_INUMP (SCM_CDDR (gns))			\
   || (scm_is_pair (SCM_CDDR (gns))		\
       && scm_is_pair (SCM_CDDDR (gns))		\
       && scm_is_pair (SCM_CDDDDR (gns))))
#define SCM_GNS_INDEX(gns)			\
  (SCM_I_INUMP (SCM_CDDR (gns))			\
   ? SCM_I_INUM (SCM_CDDR (gns))		\
   : scm_to_long (SCM_CAR (SCM_CDDDDR (gns))))
#define SCM_GNS_SIZE(gns)			\
  (SCM_I_INUMP (SCM_CDDR (gns))			\
   ? 1						\
   : scm_to_long (SCM_CADR (SCM_CDDDDR (gns))))

SCM_KEYWORD (k_class, "class");
SCM_KEYWORD (k_allocation, "allocation");
SCM_KEYWORD (k_instance, "instance");

SCM_DEFINE (scm_sys_prep_layout_x, "%prep-layout!", 1, 0, 0,
	    (SCM class),
	    "")
#define FUNC_NAME s_scm_sys_prep_layout_x
{
  SCM slots, getters_n_setters, nfields;
  unsigned long int n, i;
  char *s;
  SCM layout;

  SCM_VALIDATE_INSTANCE (1, class);
  slots = SCM_SLOT (class, scm_si_slots);
  getters_n_setters = SCM_SLOT (class, scm_si_getters_n_setters);
  nfields = SCM_SLOT (class, scm_si_nfields);
  if (!SCM_I_INUMP (nfields) || SCM_I_INUM (nfields) < 0)
    SCM_MISC_ERROR ("bad value in nfields slot: ~S",
		    scm_list_1 (nfields));
  n = 2 * SCM_I_INUM (nfields);
  if (n < sizeof (SCM_CLASS_CLASS_LAYOUT) - 1
      && SCM_SUBCLASSP (class, scm_class_class))
    SCM_MISC_ERROR ("class object doesn't have enough fields: ~S",
		    scm_list_1 (nfields));

  layout = scm_i_make_string (n, &s);
  i = 0;
  while (scm_is_pair (getters_n_setters))
    {
      if (SCM_GNS_INSTANCE_ALLOCATED_P (SCM_CAR (getters_n_setters)))
	{
	  SCM type;
	  int len, index, size;
	  char p, a;

	  if (i >= n || !scm_is_pair (slots))
	    goto inconsistent;
	  
	  /* extract slot type */
	  len = scm_ilength (SCM_CDAR (slots));
	  type = scm_i_get_keyword (k_class, SCM_CDAR (slots),
				    len, SCM_BOOL_F, FUNC_NAME);
	  /* determine slot GC protection and access mode */
	  if (scm_is_false (type))
	    {
	      p = 'p';
	      a = 'w';
	    }
	  else
	    {
	      if (!SCM_CLASSP (type))
		SCM_MISC_ERROR ("bad slot class", SCM_EOL);
	      else if (SCM_SUBCLASSP (type, scm_class_foreign_slot))
		{
		  if (SCM_SUBCLASSP (type, scm_class_self))
		    p = 's';
		  else if (SCM_SUBCLASSP (type, scm_class_protected))
		    p = 'p';
		  else
		    p = 'u';

		  if (SCM_SUBCLASSP (type, scm_class_opaque))
		    a = 'o';
		  else if (SCM_SUBCLASSP (type, scm_class_read_only))
		    a = 'r';
		  else
		    a = 'w';
		}
	      else
		{
		  p = 'p';
		  a = 'w';
		}
	    }
      
	  index = SCM_GNS_INDEX (SCM_CAR (getters_n_setters));
	  if (index != (i >> 1))
	    goto inconsistent;
	  size = SCM_GNS_SIZE (SCM_CAR (getters_n_setters));
	  while (size)
	    {
	      s[i++] = p;
	      s[i++] = a;
	      --size;
	    }
	}
      slots = SCM_CDR (slots);
      getters_n_setters = SCM_CDR (getters_n_setters);
    }
  if (!scm_is_null (slots))
    {
    inconsistent:
      SCM_MISC_ERROR ("inconsistent getters-n-setters", SCM_EOL);
    }
  SCM_SET_SLOT (class, scm_si_layout, scm_string_to_symbol (layout));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static void prep_hashsets (SCM);

SCM_DEFINE (scm_sys_inherit_magic_x, "%inherit-magic!", 2, 0, 0,
	    (SCM class, SCM dsupers),
	    "")
#define FUNC_NAME s_scm_sys_inherit_magic_x
{
  SCM ls = dsupers;
  long flags = 0;
  SCM_VALIDATE_INSTANCE (1, class);
  while (!scm_is_null (ls))
    {
      SCM_ASSERT (scm_is_pair (ls)
		  && SCM_INSTANCEP (SCM_CAR (ls)),
		  dsupers,
		  SCM_ARG2,
		  FUNC_NAME);
      flags |= SCM_CLASS_FLAGS (SCM_CAR (ls));
      ls = SCM_CDR (ls);
    }
  flags &= SCM_CLASSF_INHERIT;
  if (flags & SCM_CLASSF_ENTITY)
    SCM_SET_CLASS_DESTRUCTOR (class, scm_struct_free_entity);
  else
    {
      long n = SCM_I_INUM (SCM_SLOT (class, scm_si_nfields));
#if 0
      /*
       * We could avoid calling scm_gc_malloc in the allocation code
       * (in which case the following two lines are needed).  Instead
       * we make 0-slot instances non-light, so that the light case
       * can be handled without special cases.
       */
      if (n == 0)
	SCM_SET_CLASS_DESTRUCTOR (class, scm_struct_free_0);
#endif
      if (n > 0 && !(flags & SCM_CLASSF_METACLASS))
	{
	  /* NOTE: The following depends on scm_struct_i_size. */
	  flags |= SCM_STRUCTF_LIGHT + n * sizeof (SCM); /* use light representation */
	  SCM_SET_CLASS_DESTRUCTOR (class, scm_struct_free_light);
	}
    }
  SCM_SET_CLASS_FLAGS (class, flags);

  prep_hashsets (class);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static void
prep_hashsets (SCM class)
{
  unsigned int i;

  for (i = 0; i < 7; ++i)
    SCM_SET_HASHSET (class, i, scm_c_uniform32 (goops_rstate));
}

/******************************************************************************/

SCM
scm_basic_basic_make_class (SCM class, SCM name, SCM dsupers, SCM dslots)
{
  SCM z, cpl, slots, nfields, g_n_s;

  /* Allocate one instance */
  z = scm_make_struct (class, SCM_INUM0, SCM_EOL);

  /* Initialize its slots */
  SCM_SET_SLOT (z, scm_si_direct_supers, dsupers);
  cpl   = compute_cpl (z);
  slots = build_slots_list (maplist (dslots), cpl);
  nfields = scm_from_int (scm_ilength (slots));
  g_n_s = compute_getters_n_setters (slots);

  SCM_SET_SLOT (z, scm_si_name, name);
  SCM_SET_SLOT (z, scm_si_direct_slots, dslots);
  SCM_SET_SLOT (z, scm_si_direct_subclasses, SCM_EOL);
  SCM_SET_SLOT (z, scm_si_direct_methods, SCM_EOL);
  SCM_SET_SLOT (z, scm_si_cpl, cpl);
  SCM_SET_SLOT (z, scm_si_slots, slots);
  SCM_SET_SLOT (z, scm_si_nfields, nfields);
  SCM_SET_SLOT (z, scm_si_getters_n_setters, g_n_s);
  SCM_SET_SLOT (z, scm_si_redefined, SCM_BOOL_F);
  SCM_SET_SLOT (z, scm_si_environment,
		scm_top_level_env (SCM_TOP_LEVEL_LOOKUP_CLOSURE));

  /* Add this class in the direct-subclasses slot of dsupers */
  {
    SCM tmp;
    for (tmp = dsupers; !scm_is_null (tmp); tmp = SCM_CDR (tmp))
      SCM_SET_SLOT (SCM_CAR (tmp), scm_si_direct_subclasses,
		    scm_cons (z, SCM_SLOT (SCM_CAR (tmp),
					   scm_si_direct_subclasses)));
  }

  /* Support for the underlying structs: */
  SCM_SET_CLASS_FLAGS (z, (class == scm_class_entity_class
			   ? (SCM_CLASSF_GOOPS_OR_VALID
			      | SCM_CLASSF_OPERATOR
			      | SCM_CLASSF_ENTITY)
			   : class == scm_class_operator_class
			   ? SCM_CLASSF_GOOPS_OR_VALID | SCM_CLASSF_OPERATOR
			   : SCM_CLASSF_GOOPS_OR_VALID));
  return z;
}

SCM
scm_basic_make_class (SCM class, SCM name, SCM dsupers, SCM dslots)
{
  SCM z = scm_basic_basic_make_class (class, name, dsupers, dslots);
  scm_sys_inherit_magic_x (z, dsupers);
  scm_sys_prep_layout_x (z);
  return z;
}

/******************************************************************************/

SCM_SYMBOL (sym_layout, "layout");
SCM_SYMBOL (sym_vcell, "vcell");
SCM_SYMBOL (sym_vtable, "vtable");
SCM_SYMBOL (sym_print, "print");
SCM_SYMBOL (sym_procedure, "procedure");
SCM_SYMBOL (sym_setter, "setter");
SCM_SYMBOL (sym_redefined, "redefined");
SCM_SYMBOL (sym_h0, "h0");
SCM_SYMBOL (sym_h1, "h1");
SCM_SYMBOL (sym_h2, "h2");
SCM_SYMBOL (sym_h3, "h3");
SCM_SYMBOL (sym_h4, "h4");
SCM_SYMBOL (sym_h5, "h5");
SCM_SYMBOL (sym_h6, "h6");
SCM_SYMBOL (sym_h7, "h7");
SCM_SYMBOL (sym_name, "name");
SCM_SYMBOL (sym_direct_supers, "direct-supers");
SCM_SYMBOL (sym_direct_slots, "direct-slots");
SCM_SYMBOL (sym_direct_subclasses, "direct-subclasses");
SCM_SYMBOL (sym_direct_methods, "direct-methods");
SCM_SYMBOL (sym_cpl, "cpl");
SCM_SYMBOL (sym_default_slot_definition_class, "default-slot-definition-class");
SCM_SYMBOL (sym_slots, "slots");
SCM_SYMBOL (sym_getters_n_setters, "getters-n-setters");
SCM_SYMBOL (sym_keyword_access, "keyword-access");
SCM_SYMBOL (sym_nfields, "nfields");
SCM_SYMBOL (sym_environment, "environment");


static SCM
build_class_class_slots ()
{
  return scm_list_n (
    scm_list_3 (sym_layout, k_class, scm_class_protected_read_only),
    scm_list_3 (sym_vtable, k_class, scm_class_self),
    scm_list_1 (sym_print),
    scm_list_3 (sym_procedure, k_class, scm_class_protected_opaque),
    scm_list_3 (sym_setter, k_class, scm_class_protected_opaque),
    scm_list_1 (sym_redefined),
    scm_list_3 (sym_h0, k_class, scm_class_int),
    scm_list_3 (sym_h1, k_class, scm_class_int),
    scm_list_3 (sym_h2, k_class, scm_class_int),
    scm_list_3 (sym_h3, k_class, scm_class_int),
    scm_list_3 (sym_h4, k_class, scm_class_int),
    scm_list_3 (sym_h5, k_class, scm_class_int),
    scm_list_3 (sym_h6, k_class, scm_class_int),
    scm_list_3 (sym_h7, k_class, scm_class_int),
    scm_list_1 (sym_name),
    scm_list_1 (sym_direct_supers),
    scm_list_1 (sym_direct_slots),
    scm_list_1 (sym_direct_subclasses),
    scm_list_1 (sym_direct_methods),
    scm_list_1 (sym_cpl),
    scm_list_1 (sym_default_slot_definition_class),
    scm_list_1 (sym_slots),
    scm_list_1 (sym_getters_n_setters),
    scm_list_1 (sym_keyword_access),
    scm_list_1 (sym_nfields),
    scm_list_1 (sym_environment),
    SCM_UNDEFINED);
}

static void
create_basic_classes (void)
{
  /* SCM slots_of_class = build_class_class_slots (); */

  /**** <scm_class_class> ****/
  SCM cs = scm_from_locale_string (SCM_CLASS_CLASS_LAYOUT
				   + 2 * scm_vtable_offset_user);
  SCM name = scm_from_locale_symbol ("<class>");
  scm_class_class = scm_permanent_object (scm_make_vtable_vtable (cs,
								  SCM_INUM0,
								  SCM_EOL));
  SCM_SET_CLASS_FLAGS (scm_class_class, (SCM_CLASSF_GOOPS_OR_VALID
					 | SCM_CLASSF_METACLASS));

  SCM_SET_SLOT (scm_class_class, scm_si_name, name);
  SCM_SET_SLOT (scm_class_class, scm_si_direct_supers, SCM_EOL);  /* will be changed */
  /* SCM_SET_SLOT (scm_class_class, scm_si_direct_slots, slots_of_class); */
  SCM_SET_SLOT (scm_class_class, scm_si_direct_subclasses, SCM_EOL);
  SCM_SET_SLOT (scm_class_class, scm_si_direct_methods, SCM_EOL);
  SCM_SET_SLOT (scm_class_class, scm_si_cpl, SCM_EOL);  /* will be changed */
  /* SCM_SET_SLOT (scm_class_class, scm_si_slots, slots_of_class); */
  SCM_SET_SLOT (scm_class_class, scm_si_nfields, scm_from_int (SCM_N_CLASS_SLOTS));
  /* SCM_SET_SLOT (scm_class_class, scm_si_getters_n_setters,
                   compute_getters_n_setters (slots_of_class)); */
  SCM_SET_SLOT (scm_class_class, scm_si_redefined, SCM_BOOL_F);
  SCM_SET_SLOT (scm_class_class, scm_si_environment,
		scm_top_level_env (SCM_TOP_LEVEL_LOOKUP_CLOSURE));

  prep_hashsets (scm_class_class);

  DEFVAR(name, scm_class_class);

  /**** <scm_class_top> ****/
  name = scm_from_locale_symbol ("<top>");
  scm_class_top = scm_permanent_object (scm_basic_make_class (scm_class_class,
						    name,
						    SCM_EOL,
						    SCM_EOL));

  DEFVAR(name, scm_class_top);

  /**** <scm_class_object> ****/
  name	 = scm_from_locale_symbol ("<object>");
  scm_class_object = scm_permanent_object (scm_basic_make_class (scm_class_class,
						       name,
						       scm_list_1 (scm_class_top),
						       SCM_EOL));

  DEFVAR (name, scm_class_object);

  /* <top> <object> and <class> were partially initialized. Correct them here */
  SCM_SET_SLOT (scm_class_object, scm_si_direct_subclasses, scm_list_1 (scm_class_class));

  SCM_SET_SLOT (scm_class_class, scm_si_direct_supers, scm_list_1 (scm_class_object));
  SCM_SET_SLOT (scm_class_class, scm_si_cpl, scm_list_3 (scm_class_class, scm_class_object, scm_class_top));
}

/******************************************************************************/

SCM_DEFINE (scm_instance_p, "instance?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is an instance.")
#define FUNC_NAME s_scm_instance_p
{
  return scm_from_bool (SCM_INSTANCEP (obj));
}
#undef FUNC_NAME


/******************************************************************************
 *
 * Meta object accessors
 *
 ******************************************************************************/
SCM_DEFINE (scm_class_name, "class-name",  1, 0, 0,
	    (SCM obj),
	    "Return the class name of @var{obj}.")
#define FUNC_NAME s_scm_class_name
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref (obj, sym_name);
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_direct_supers, "class-direct-supers", 1, 0, 0,
	    (SCM obj),
	    "Return the direct superclasses of the class @var{obj}.")
#define FUNC_NAME s_scm_class_direct_supers
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref (obj, sym_direct_supers);
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_direct_slots, "class-direct-slots", 1, 0, 0,
	    (SCM obj),
	    "Return the direct slots of the class @var{obj}.")
#define FUNC_NAME s_scm_class_direct_slots
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref (obj, sym_direct_slots);
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_direct_subclasses, "class-direct-subclasses", 1, 0, 0,
	    (SCM obj),
	    "Return the direct subclasses of the class @var{obj}.")
#define FUNC_NAME s_scm_class_direct_subclasses
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref(obj, sym_direct_subclasses);
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_direct_methods, "class-direct-methods", 1, 0, 0,
	    (SCM obj),
	    "Return the direct methods of the class @var{obj}")
#define FUNC_NAME s_scm_class_direct_methods
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref (obj, sym_direct_methods);
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_precedence_list, "class-precedence-list", 1, 0, 0,
	    (SCM obj),
	    "Return the class precedence list of the class @var{obj}.")
#define FUNC_NAME s_scm_class_precedence_list
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref (obj, sym_cpl);
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_slots, "class-slots", 1, 0, 0,
	    (SCM obj),
	    "Return the slot list of the class @var{obj}.")
#define FUNC_NAME s_scm_class_slots
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref (obj, sym_slots);
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_environment, "class-environment", 1, 0, 0,
	    (SCM obj),
	    "Return the environment of the class @var{obj}.")
#define FUNC_NAME s_scm_class_environment
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref(obj, sym_environment);
}
#undef FUNC_NAME


SCM_DEFINE (scm_generic_function_name, "generic-function-name", 1, 0, 0,
	    (SCM obj),
	    "Return the name of the generic function @var{obj}.")
#define FUNC_NAME s_scm_generic_function_name
{
  SCM_VALIDATE_GENERIC (1, obj);
  return scm_procedure_property (obj, scm_sym_name);
}
#undef FUNC_NAME

SCM_SYMBOL (sym_methods, "methods");
SCM_SYMBOL (sym_extended_by, "extended-by");
SCM_SYMBOL (sym_extends, "extends");

static
SCM fold_downward_gf_methods (SCM method_lists, SCM gf)
{
  SCM gfs = scm_slot_ref (gf, sym_extended_by);
  method_lists = scm_cons (scm_slot_ref (gf, sym_methods), method_lists);
  while (!scm_is_null (gfs))
    {
      method_lists = fold_downward_gf_methods (method_lists, SCM_CAR (gfs));
      gfs = SCM_CDR (gfs);
    }
  return method_lists;
}

static
SCM fold_upward_gf_methods (SCM method_lists, SCM gf)
{
  if (SCM_IS_A_P (gf, scm_class_extended_generic))
    {
      SCM gfs = scm_slot_ref (gf, sym_extends);
      while (!scm_is_null (gfs))
	{
	  SCM methods = scm_slot_ref (SCM_CAR (gfs), sym_methods);
	  method_lists = fold_upward_gf_methods (scm_cons (methods,
							   method_lists),
						 SCM_CAR (gfs));
	  gfs = SCM_CDR (gfs);
	}
    }
  return method_lists;
}

SCM_DEFINE (scm_generic_function_methods, "generic-function-methods", 1, 0, 0,
	    (SCM obj),
	    "Return the methods of the generic function @var{obj}.")
#define FUNC_NAME s_scm_generic_function_methods
{
  SCM methods;
  SCM_VALIDATE_GENERIC (1, obj);
  methods = fold_upward_gf_methods (SCM_EOL, obj);
  methods = fold_downward_gf_methods (methods, obj);
  return scm_append (methods);
}
#undef FUNC_NAME

SCM_DEFINE (scm_method_generic_function, "method-generic-function", 1, 0, 0,
	    (SCM obj),
	    "Return the generic function for the method @var{obj}.")
#define FUNC_NAME s_scm_method_generic_function
{
  SCM_VALIDATE_METHOD (1, obj);
  return scm_slot_ref (obj, scm_from_locale_symbol ("generic-function"));
}
#undef FUNC_NAME

SCM_DEFINE (scm_method_specializers, "method-specializers", 1, 0, 0,
	    (SCM obj),
	    "Return specializers of the method @var{obj}.")
#define FUNC_NAME s_scm_method_specializers
{
  SCM_VALIDATE_METHOD (1, obj);
  return scm_slot_ref (obj, scm_from_locale_symbol ("specializers"));
}
#undef FUNC_NAME

SCM_DEFINE (scm_method_procedure, "method-procedure", 1, 0, 0,
	    (SCM obj),
	    "Return the procedure of the method @var{obj}.")
#define FUNC_NAME s_scm_method_procedure
{
  SCM_VALIDATE_METHOD (1, obj);
  return scm_slot_ref (obj, sym_procedure);
}
#undef FUNC_NAME

SCM_DEFINE (scm_accessor_method_slot_definition, "accessor-method-slot-definition", 1, 0, 0,
	    (SCM obj),
	    "Return the slot definition of the accessor @var{obj}.")
#define FUNC_NAME s_scm_accessor_method_slot_definition
{
  SCM_VALIDATE_ACCESSOR (1, obj);
  return scm_slot_ref (obj, scm_from_locale_symbol ("slot-definition"));
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_tag_body, "%tag-body", 1, 0, 0,
	    (SCM body),
	    "Internal GOOPS magic---don't use this function!")
#define FUNC_NAME s_scm_sys_tag_body
{
  return scm_cons (SCM_IM_LAMBDA, body);
}
#undef FUNC_NAME

/******************************************************************************
 *
 * S l o t   a c c e s s
 *
 ******************************************************************************/

SCM_DEFINE (scm_make_unbound, "make-unbound", 0, 0, 0,
	    (),
	    "Return the unbound value.")
#define FUNC_NAME s_scm_make_unbound
{
  return SCM_GOOPS_UNBOUND;
}
#undef FUNC_NAME

SCM_DEFINE (scm_unbound_p, "unbound?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is unbound.")
#define FUNC_NAME s_scm_unbound_p
{
  return SCM_GOOPS_UNBOUNDP (obj) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_assert_bound, "assert-bound", 2, 0, 0,
	    (SCM value, SCM obj),
	    "Return @var{value} if it is bound, and invoke the\n"
	    "@var{slot-unbound} method of @var{obj} if it is not.")
#define FUNC_NAME s_scm_assert_bound
{
  if (SCM_GOOPS_UNBOUNDP (value))
    return CALL_GF1 ("slot-unbound", obj);
  return value;
}
#undef FUNC_NAME

SCM_DEFINE (scm_at_assert_bound_ref, "@assert-bound-ref", 2, 0, 0,
	    (SCM obj, SCM index),
	    "Like @code{assert-bound}, but use @var{index} for accessing\n"
	    "the value from @var{obj}.")
#define FUNC_NAME s_scm_at_assert_bound_ref
{
  SCM value = SCM_SLOT (obj, scm_to_int (index));
  if (SCM_GOOPS_UNBOUNDP (value))
    return CALL_GF1 ("slot-unbound", obj);
  return value;
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_fast_slot_ref, "%fast-slot-ref", 2, 0, 0,
	    (SCM obj, SCM index),
	    "Return the slot value with index @var{index} from @var{obj}.")
#define FUNC_NAME s_scm_sys_fast_slot_ref
{
  unsigned long int i;

  SCM_VALIDATE_INSTANCE (1, obj);
  i = scm_to_unsigned_integer (index, 0,
			       SCM_I_INUM (SCM_SLOT (SCM_CLASS_OF (obj),
						     scm_si_nfields))
			       - 1);
  return SCM_SLOT (obj, i);
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_fast_slot_set_x, "%fast-slot-set!", 3, 0, 0,
	    (SCM obj, SCM index, SCM value),
	    "Set the slot with index @var{index} in @var{obj} to\n"
	    "@var{value}.")
#define FUNC_NAME s_scm_sys_fast_slot_set_x
{
  unsigned long int i;

  SCM_VALIDATE_INSTANCE (1, obj);
  i = scm_to_unsigned_integer (index, 0,
			       SCM_I_INUM (SCM_SLOT (SCM_CLASS_OF (obj),
						     scm_si_nfields))
			       - 1);

  SCM_SET_SLOT (obj, i, value);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_SYNTAX (s_atslot_ref, "@slot-ref", scm_i_makbimacro, scm_m_atslot_ref);
SCM_SYNTAX (s_atslot_set_x, "@slot-set!", scm_i_makbimacro, scm_m_atslot_set_x);


/** Utilities **/

/* In the future, this function will return the effective slot
 * definition associated with SLOT_NAME.  Now it just returns some of
 * the information which will be stored in the effective slot
 * definition.
 */

static SCM
slot_definition_using_name (SCM class, SCM slot_name)
{
  register SCM slots = SCM_SLOT (class, scm_si_getters_n_setters);
  for (; !scm_is_null (slots); slots = SCM_CDR (slots))
    if (SCM_CAAR (slots) == slot_name)
      return SCM_CAR (slots);
  return SCM_BOOL_F;
}

static SCM
get_slot_value (SCM class SCM_UNUSED, SCM obj, SCM slotdef)
#define FUNC_NAME "%get-slot-value"
{
  SCM access = SCM_CDDR (slotdef);
  /* Two cases here:
   *	- access is an integer (the offset of this slot in the slots vector)
   *	- otherwise (car access) is the getter function to apply
   *
   * Instances have never more than SCM_MOST_POSITIVE_FIXNUM slots, so
   * we can just assume fixnums here.
   */
  if (SCM_I_INUMP (access))
    /* Don't poke at the slots directly, because scm_struct_ref handles the
       access bits for us. */
    return scm_struct_ref (obj, access);
  else
    {
      /* We must evaluate (apply (car access) (list obj))
       * where (car access) is known to be a closure of arity 1  */
      register SCM code, env;

      code = SCM_CAR (access);
      if (!SCM_CLOSUREP (code))
	return SCM_SUBRF (code) (obj);
      env  = SCM_EXTEND_ENV (SCM_CLOSURE_FORMALS (code),
			     scm_list_1 (obj),
			     SCM_ENV (code));
      /* Evaluate the closure body */
      return scm_eval_body (SCM_CLOSURE_BODY (code), env);
    }
}
#undef FUNC_NAME

static SCM
get_slot_value_using_name (SCM class, SCM obj, SCM slot_name)
{
  SCM slotdef = slot_definition_using_name (class, slot_name);
  if (scm_is_true (slotdef))
    return get_slot_value (class, obj, slotdef);
  else
    return CALL_GF3 ("slot-missing", class, obj, slot_name);
}

static SCM
set_slot_value (SCM class SCM_UNUSED, SCM obj, SCM slotdef, SCM value)
#define FUNC_NAME "%set-slot-value"
{
  SCM access = SCM_CDDR (slotdef);
  /* Two cases here:
   *	- access is an integer (the offset of this slot in the slots vector)
   *	- otherwise (cadr access) is the setter function to apply
   *
   * Instances have never more than SCM_MOST_POSITIVE_FIXNUM slots, so
   * we can just assume fixnums here.
   */
  if (SCM_I_INUMP (access))
    /* obey permissions bits via going through struct-set! */
    scm_struct_set_x (obj, access, value);
  else
    {
      /* We must evaluate (apply (cadr l) (list obj value))
       * where (cadr l) is known to be a closure of arity 2  */
      register SCM code, env;

      code = SCM_CADR (access);
      if (!SCM_CLOSUREP (code))
	SCM_SUBRF (code) (obj, value);
      else
	{
	  env  = SCM_EXTEND_ENV (SCM_CLOSURE_FORMALS (code),
				 scm_list_2 (obj, value),
				 SCM_ENV (code));
	  /* Evaluate the closure body */
	  scm_eval_body (SCM_CLOSURE_BODY (code), env);
	}
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static SCM
set_slot_value_using_name (SCM class, SCM obj, SCM slot_name, SCM value)
{
  SCM slotdef = slot_definition_using_name (class, slot_name);
  if (scm_is_true (slotdef))
    return set_slot_value (class, obj, slotdef, value);
  else
    return CALL_GF4 ("slot-missing", class, obj, slot_name, value);
}

static SCM
test_slot_existence (SCM class SCM_UNUSED, SCM obj, SCM slot_name)
{
  register SCM l;

  for (l = SCM_ACCESSORS_OF (obj); !scm_is_null (l); l = SCM_CDR (l))
    if (scm_is_eq (SCM_CAAR (l), slot_name))
      return SCM_BOOL_T;

  return SCM_BOOL_F;
}

		/* ======================================== */

SCM_DEFINE (scm_slot_ref_using_class, "slot-ref-using-class", 3, 0, 0,
	    (SCM class, SCM obj, SCM slot_name),
	    "")
#define FUNC_NAME s_scm_slot_ref_using_class
{
  SCM res;

  SCM_VALIDATE_CLASS (1, class);
  SCM_VALIDATE_INSTANCE (2, obj);
  SCM_VALIDATE_SYMBOL (3, slot_name);

  res = get_slot_value_using_name (class, obj, slot_name);
  if (SCM_GOOPS_UNBOUNDP (res))
    return CALL_GF3 ("slot-unbound", class, obj, slot_name);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_slot_set_using_class_x, "slot-set-using-class!", 4, 0, 0,
	    (SCM class, SCM obj, SCM slot_name, SCM value),
	    "")
#define FUNC_NAME s_scm_slot_set_using_class_x
{
  SCM_VALIDATE_CLASS (1, class);
  SCM_VALIDATE_INSTANCE (2, obj);
  SCM_VALIDATE_SYMBOL (3, slot_name);

  return set_slot_value_using_name (class, obj, slot_name, value);
}
#undef FUNC_NAME


SCM_DEFINE (scm_slot_bound_using_class_p, "slot-bound-using-class?", 3, 0, 0,
	    (SCM class, SCM obj, SCM slot_name),
	    "")
#define FUNC_NAME s_scm_slot_bound_using_class_p
{
  SCM_VALIDATE_CLASS (1, class);
  SCM_VALIDATE_INSTANCE (2, obj);
  SCM_VALIDATE_SYMBOL (3, slot_name);

  return (SCM_GOOPS_UNBOUNDP (get_slot_value_using_name (class, obj, slot_name))
	  ? SCM_BOOL_F
	  : SCM_BOOL_T);
}
#undef FUNC_NAME

SCM_DEFINE (scm_slot_exists_using_class_p, "slot-exists-using-class?", 3, 0, 0,
	    (SCM class, SCM obj, SCM slot_name),
	    "")
#define FUNC_NAME s_scm_slot_exists_using_class_p
{
  SCM_VALIDATE_CLASS (1, class);
  SCM_VALIDATE_INSTANCE (2, obj);
  SCM_VALIDATE_SYMBOL (3, slot_name);
  return test_slot_existence (class, obj, slot_name);
}
#undef FUNC_NAME


		/* ======================================== */

SCM_DEFINE (scm_slot_ref, "slot-ref", 2, 0, 0,
	    (SCM obj, SCM slot_name),
	    "Return the value from @var{obj}'s slot with the name\n"
	    "@var{slot_name}.")
#define FUNC_NAME s_scm_slot_ref
{
  SCM res, class;

  SCM_VALIDATE_INSTANCE (1, obj);
  TEST_CHANGE_CLASS (obj, class);

  res = get_slot_value_using_name (class, obj, slot_name);
  if (SCM_GOOPS_UNBOUNDP (res))
    return CALL_GF3 ("slot-unbound", class, obj, slot_name);
  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_slot_set_x, "slot-set!", 3, 0, 0,
	    (SCM obj, SCM slot_name, SCM value),
	    "Set the slot named @var{slot_name} of @var{obj} to @var{value}.")
#define FUNC_NAME s_scm_slot_set_x
{
  SCM class;

  SCM_VALIDATE_INSTANCE (1, obj);
  TEST_CHANGE_CLASS(obj, class);

  return set_slot_value_using_name (class, obj, slot_name, value);
}
#undef FUNC_NAME

const char *scm_s_slot_set_x = s_scm_slot_set_x;

SCM_DEFINE (scm_slot_bound_p, "slot-bound?", 2, 0, 0,
	    (SCM obj, SCM slot_name),
	    "Return @code{#t} if the slot named @var{slot_name} of @var{obj}\n"
	    "is bound.")
#define FUNC_NAME s_scm_slot_bound_p
{
  SCM class;

  SCM_VALIDATE_INSTANCE (1, obj);
  TEST_CHANGE_CLASS(obj, class);

  return (SCM_GOOPS_UNBOUNDP (get_slot_value_using_name (class,
							 obj,
							 slot_name))
	  ? SCM_BOOL_F
	  : SCM_BOOL_T);
}
#undef FUNC_NAME

SCM_DEFINE (scm_slot_exists_p, "slot-exists?", 2, 0, 0,
	    (SCM obj, SCM slot_name),
	    "Return @code{#t} if @var{obj} has a slot named @var{slot_name}.")
#define FUNC_NAME s_scm_slot_exists_p
{
  SCM class;

  SCM_VALIDATE_INSTANCE (1, obj);
  SCM_VALIDATE_SYMBOL (2, slot_name);
  TEST_CHANGE_CLASS (obj, class);

  return test_slot_existence (class, obj, slot_name);
}
#undef FUNC_NAME


/******************************************************************************
 *
 * %allocate-instance (the low level instance allocation primitive)
 *
 ******************************************************************************/

static void clear_method_cache (SCM);

static SCM
wrap_init (SCM class, SCM *m, long n)
{
  long i;
  scm_t_bits slayout = SCM_STRUCT_DATA (class)[scm_vtable_index_layout];
  const char *layout = scm_i_symbol_chars (SCM_PACK (slayout));

  /* Set all SCM-holding slots to unbound */
  for (i = 0; i < n; i++)
    if (layout[i*2] == 'p')
      m[i] = SCM_GOOPS_UNBOUND;
    else
      m[i] = 0;

  return scm_double_cell ((((scm_t_bits) SCM_STRUCT_DATA (class))
			   | scm_tc3_struct),
			  (scm_t_bits) m, 0, 0);
}

SCM_DEFINE (scm_sys_allocate_instance, "%allocate-instance", 2, 0, 0,
	    (SCM class, SCM initargs),
	    "Create a new instance of class @var{class} and initialize it\n"
	    "from the arguments @var{initargs}.")
#define FUNC_NAME s_scm_sys_allocate_instance
{
  SCM *m;
  long n;

  SCM_VALIDATE_CLASS (1, class);

  /* Most instances */
  if (SCM_CLASS_FLAGS (class) & SCM_STRUCTF_LIGHT)
    {
      n = SCM_I_INUM (SCM_SLOT (class, scm_si_nfields));
      m = (SCM *) scm_gc_malloc (n * sizeof (SCM), "struct");
      return wrap_init (class, m, n);
    }

  /* Foreign objects */
  if (SCM_CLASS_FLAGS (class) & SCM_CLASSF_FOREIGN)
    return scm_make_foreign_object (class, initargs);

  n = SCM_I_INUM (SCM_SLOT (class, scm_si_nfields));

  /* Entities */
  if (SCM_CLASS_FLAGS (class) & SCM_CLASSF_ENTITY)
    {
      m = (SCM *) scm_alloc_struct (n, scm_struct_entity_n_extra_words,
				    "entity struct");
      m[scm_struct_i_setter] = SCM_BOOL_F;
      m[scm_struct_i_procedure] = SCM_BOOL_F;
      /* Generic functions */
      if (SCM_CLASS_FLAGS (class) & SCM_CLASSF_PURE_GENERIC)
	{
	  SCM gf = wrap_init (class, m, n);
	  clear_method_cache (gf);
	  return gf;
	}
      else
	return wrap_init (class, m, n);
    }

  /* Class objects */
  if (SCM_CLASS_FLAGS (class) & SCM_CLASSF_METACLASS)
    {
      long i;

      /* allocate class object */
      SCM z = scm_make_struct (class, SCM_INUM0, SCM_EOL);

      SCM_SET_SLOT (z, scm_si_print, SCM_GOOPS_UNBOUND);
      for (i = scm_si_goops_fields; i < n; i++)
	SCM_SET_SLOT (z, i, SCM_GOOPS_UNBOUND);

      if (SCM_SUBCLASSP (class, scm_class_entity_class))
	SCM_SET_CLASS_FLAGS (z, SCM_CLASSF_OPERATOR | SCM_CLASSF_ENTITY);
      else if (SCM_SUBCLASSP (class, scm_class_operator_class))
	SCM_SET_CLASS_FLAGS (z, SCM_CLASSF_OPERATOR);

      return z;
    }

  /* Non-light instances */
  {
    m = (SCM *) scm_alloc_struct (n, scm_struct_n_extra_words, "heavy struct");
    return wrap_init (class, m, n);
  }
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_set_object_setter_x, "%set-object-setter!", 2, 0, 0,
	    (SCM obj, SCM setter),
	    "")
#define FUNC_NAME s_scm_sys_set_object_setter_x
{
  SCM_ASSERT (SCM_STRUCTP (obj)
	      && ((SCM_CLASS_FLAGS (obj) & SCM_CLASSF_OPERATOR)
		  || SCM_I_ENTITYP (obj)),
	      obj,
	      SCM_ARG1,
	      FUNC_NAME);
  if (SCM_I_ENTITYP (obj))
    SCM_SET_ENTITY_SETTER (obj, setter);
  else
    SCM_OPERATOR_CLASS (obj)->setter = setter;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/******************************************************************************
 *
 * %modify-instance (used by change-class to modify in place)
 *
 ******************************************************************************/

SCM_DEFINE (scm_sys_modify_instance, "%modify-instance", 2, 0, 0,
	    (SCM old, SCM new),
	    "")
#define FUNC_NAME s_scm_sys_modify_instance
{
  SCM_VALIDATE_INSTANCE (1, old);
  SCM_VALIDATE_INSTANCE (2, new);

  /* Exchange the data contained in old and new. We exchange rather than
   * scratch the old value with new to be correct with GC.
   * See "Class redefinition protocol above".
   */
  SCM_CRITICAL_SECTION_START;
  {
    SCM car = SCM_CAR (old);
    SCM cdr = SCM_CDR (old);
    SCM_SETCAR (old, SCM_CAR (new));
    SCM_SETCDR (old, SCM_CDR (new));
    SCM_SETCAR (new, car);
    SCM_SETCDR (new, cdr);
  }
  SCM_CRITICAL_SECTION_END;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_modify_class, "%modify-class", 2, 0, 0,
	    (SCM old, SCM new),
	    "")
#define FUNC_NAME s_scm_sys_modify_class
{
  SCM_VALIDATE_CLASS (1, old);
  SCM_VALIDATE_CLASS (2, new);

  SCM_CRITICAL_SECTION_START;
  {
    SCM car = SCM_CAR (old);
    SCM cdr = SCM_CDR (old);
    SCM_SETCAR (old, SCM_CAR (new));
    SCM_SETCDR (old, SCM_CDR (new));
    SCM_STRUCT_DATA (old)[scm_vtable_index_vtable] = SCM_UNPACK (old);
    SCM_SETCAR (new, car);
    SCM_SETCDR (new, cdr);
    SCM_STRUCT_DATA (new)[scm_vtable_index_vtable] = SCM_UNPACK (new);
  }
  SCM_CRITICAL_SECTION_END;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_invalidate_class, "%invalidate-class", 1, 0, 0,
	    (SCM class),
	    "")
#define FUNC_NAME s_scm_sys_invalidate_class
{
  SCM_VALIDATE_CLASS (1, class);
  SCM_CLEAR_CLASS_FLAGS (class, SCM_CLASSF_GOOPS_VALID);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* When instances change class, they finally get a new body, but
 * before that, they go through purgatory in hell.  Odd as it may
 * seem, this data structure saves us from eternal suffering in
 * infinite recursions.
 */

static scm_t_bits **hell;
static long n_hell = 1;		/* one place for the evil one himself */
static long hell_size = 4;
static SCM hell_mutex;

static long
burnin (SCM o)
{
  long i;
  for (i = 1; i < n_hell; ++i)
    if (SCM_STRUCT_DATA (o) == hell[i])
      return i;
  return 0;
}

static void
go_to_hell (void *o)
{
  SCM obj = SCM_PACK ((scm_t_bits) o);
  scm_lock_mutex (hell_mutex);
  if (n_hell >= hell_size)
    {
      hell_size *= 2;
      hell = scm_realloc (hell, hell_size * sizeof(*hell));
    }
  hell[n_hell++] = SCM_STRUCT_DATA (obj);
  scm_unlock_mutex (hell_mutex);
}

static void
go_to_heaven (void *o)
{
  scm_lock_mutex (hell_mutex);
  hell[burnin (SCM_PACK ((scm_t_bits) o))] = hell[--n_hell];
  scm_unlock_mutex (hell_mutex);
}


SCM_SYMBOL (scm_sym_change_class, "change-class");

static SCM
purgatory (void *args)
{
  return scm_apply_0 (GETVAR (scm_sym_change_class),
		      SCM_PACK ((scm_t_bits) args));
}

/* This function calls the generic function change-class for all
 * instances which aren't currently undergoing class change.
 */

void
scm_change_object_class (SCM obj, SCM old_class SCM_UNUSED, SCM new_class)
{
  if (!burnin (obj))
    scm_internal_dynamic_wind (go_to_hell, purgatory, go_to_heaven,
			       (void *) SCM_UNPACK (scm_list_2 (obj, new_class)),
			       (void *) SCM_UNPACK (obj));
}

/******************************************************************************
 *
 *   GGGG                FFFFF
 *  G                    F
 *  G  GG                FFF
 *  G   G                F
 *   GGG  E N E R I C    F    U N C T I O N S
 *
 * This implementation provides
 *	- generic functions (with class specializers)
 *	- multi-methods
 *	- next-method
 *	- a hard-coded MOP for standard gf, which can be overloaded for non-std gf
 *
 ******************************************************************************/

SCM_KEYWORD (k_name, "name");

SCM_SYMBOL (sym_no_method, "no-method");

static SCM list_of_no_method;

SCM_GLOBAL_SYMBOL (scm_sym_args, "args");


SCM
scm_make_method_cache (SCM gf)
{
  return scm_list_5 (SCM_IM_DISPATCH,
		     scm_sym_args,
		     scm_from_int (1),
		     scm_c_make_vector (SCM_INITIAL_MCACHE_SIZE,
					list_of_no_method),
		     gf);
}

static void
clear_method_cache (SCM gf)
{
  SCM cache = scm_make_method_cache (gf);
  SCM_SET_ENTITY_PROCEDURE (gf, cache);
  SCM_SET_SLOT (gf, scm_si_used_by, SCM_BOOL_F);
}

SCM_DEFINE (scm_sys_invalidate_method_cache_x, "%invalidate-method-cache!", 1, 0, 0,
	    (SCM gf),
	    "")
#define FUNC_NAME s_scm_sys_invalidate_method_cache_x
{
  SCM used_by;
  SCM_ASSERT (SCM_PUREGENERICP (gf), gf, SCM_ARG1, FUNC_NAME);
  used_by = SCM_SLOT (gf, scm_si_used_by);
  if (scm_is_true (used_by))
    {
      SCM methods = SCM_SLOT (gf, scm_si_methods);
      for (; scm_is_pair (used_by); used_by = SCM_CDR (used_by))
	scm_sys_invalidate_method_cache_x (SCM_CAR (used_by));
      clear_method_cache (gf);
      for (; scm_is_pair (methods); methods = SCM_CDR (methods))
	SCM_SET_SLOT (SCM_CAR (methods), scm_si_code_table, SCM_EOL);
    }
  {
    SCM n = SCM_SLOT (gf, scm_si_n_specialized);
    /* The sign of n is a flag indicating rest args. */
    SCM_SET_MCACHE_N_SPECIALIZED (SCM_ENTITY_PROCEDURE (gf), n);
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_generic_capability_p, "generic-capability?", 1, 0, 0,
	    (SCM proc),
	    "")
#define FUNC_NAME s_scm_generic_capability_p
{
  SCM_ASSERT (scm_is_true (scm_procedure_p (proc)),
	      proc, SCM_ARG1, FUNC_NAME);
  return (scm_subr_p (proc) && SCM_SUBR_GENERIC (proc)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}
#undef FUNC_NAME

SCM_DEFINE (scm_enable_primitive_generic_x, "enable-primitive-generic!", 0, 0, 1,
	    (SCM subrs),
	    "")
#define FUNC_NAME s_scm_enable_primitive_generic_x
{
  SCM_VALIDATE_REST_ARGUMENT (subrs);
  while (!scm_is_null (subrs))
    {
      SCM subr = SCM_CAR (subrs);
      SCM_ASSERT (scm_subr_p (subr) && SCM_SUBR_GENERIC (subr),
		  subr, SCM_ARGn, FUNC_NAME);
      *SCM_SUBR_GENERIC (subr)
	= scm_make (scm_list_3 (scm_class_generic,
				k_name,
				SCM_SNAME (subr)));
      subrs = SCM_CDR (subrs);
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_primitive_generic_generic, "primitive-generic-generic", 1, 0, 0,
	    (SCM subr),
	    "")
#define FUNC_NAME s_scm_primitive_generic_generic
{
  if (scm_subr_p (subr) && SCM_SUBR_GENERIC (subr))
    {
      if (!*SCM_SUBR_GENERIC (subr))
	scm_enable_primitive_generic_x (scm_list_1 (subr));
      return *SCM_SUBR_GENERIC (subr);
    }
  SCM_WRONG_TYPE_ARG (SCM_ARG1, subr);
}
#undef FUNC_NAME

typedef struct t_extension {
  struct t_extension *next;
  SCM extended;
  SCM extension;
} t_extension;

static t_extension *extensions = 0;

SCM_VARIABLE (scm_var_make_extended_generic, "make-extended-generic");

void
scm_c_extend_primitive_generic (SCM extended, SCM extension)
{
  if (goops_loaded_p)
    {
      SCM gf, gext;
      if (!*SCM_SUBR_GENERIC (extended))
	scm_enable_primitive_generic_x (scm_list_1 (extended));
      gf = *SCM_SUBR_GENERIC (extended);
      gext = scm_call_2 (SCM_VARIABLE_REF (scm_var_make_extended_generic),
			 gf,
			 SCM_SNAME (extension));
      *SCM_SUBR_GENERIC (extension) = gext;
    }
  else
    {
      t_extension *e = scm_malloc (sizeof (t_extension));
      t_extension **loc = &extensions;
      /* Make sure that extensions are placed before their own
       * extensions in the extensions list.  O(N^2) algorithm, but
       * extensions of primitive generics are rare.
       */
      while (*loc && extension != (*loc)->extended)
	loc = &(*loc)->next;
      e->next = *loc;
      e->extended = extended;
      e->extension = extension;
      *loc = e;
    }
}

static void
setup_extended_primitive_generics ()
{
  while (extensions)
    {
      t_extension *e = extensions;
      scm_c_extend_primitive_generic (e->extended, e->extension);
      extensions = e->next;
      free (e);
    }
}

/******************************************************************************
 *
 * Protocol for calling a generic fumction
 * This protocol is roughly equivalent to (parameter are a little bit different
 * for efficiency reasons):
 *
 * 	+ apply-generic (gf args)
 *		+ compute-applicable-methods (gf args ...)
 *			+ sort-applicable-methods (methods args)
 *		+ apply-methods (gf methods args)
 *
 * apply-methods calls make-next-method to build the "continuation" of a a
 * method.  Applying a next-method will call apply-next-method which in
 * turn will call  apply again to call effectively the following method.
 *
 ******************************************************************************/

static int
applicablep (SCM actual, SCM formal)
{
  /* We already know that the cpl is well formed. */
  return scm_is_true (scm_c_memq (formal, SCM_SLOT (actual, scm_si_cpl)));
}

static int
more_specificp (SCM m1, SCM m2, SCM const *targs)
{
  register SCM s1, s2;
  register long i;
  /*
   * Note:
   *   m1 and m2 can have != length (i.e. one can be one element longer than the
   * other when we have a dotted parameter list). For instance, with the call
   *   (M 1)
   * with
   *   (define-method M (a . l) ....)
   *   (define-method M (a) ....)
   *
   * we consider that the second method is more specific.
   *
   * BTW, targs is an array of types. We don't need it's size since
   * we already know that m1 and m2 are applicable (no risk to go past
   * the end of this array).
   *
   */
  for (i=0, s1=SPEC_OF(m1), s2=SPEC_OF(m2); ; i++, s1=SCM_CDR(s1), s2=SCM_CDR(s2)) {
    if (scm_is_null(s1)) return 1;
    if (scm_is_null(s2)) return 0;
    if (SCM_CAR(s1) != SCM_CAR(s2)) {
      register SCM l, cs1 = SCM_CAR(s1), cs2 = SCM_CAR(s2);

      for (l = SCM_SLOT (targs[i], scm_si_cpl);   ; l = SCM_CDR(l)) {
	if (cs1 == SCM_CAR(l))
	  return 1;
	if (cs2 == SCM_CAR(l))
	  return 0;
      }
      return 0;/* should not occur! */
    }
  }
  return 0; /* should not occur! */
}

#define BUFFSIZE 32		/* big enough for most uses */

static SCM
scm_i_vector2list (SCM l, long len)
{
  long j;
  SCM z = scm_c_make_vector (len, SCM_UNDEFINED);

  for (j = 0; j < len; j++, l = SCM_CDR (l)) {
    SCM_SIMPLE_VECTOR_SET (z, j, SCM_CAR (l));
  }
  return z;
}

static SCM
sort_applicable_methods (SCM method_list, long size, SCM const *targs)
{
  long i, j, incr;
  SCM *v, vector = SCM_EOL;
  SCM buffer[BUFFSIZE];
  SCM save = method_list;
  scm_t_array_handle handle;

  /* For reasonably sized method_lists we can try to avoid all the
   * consing and reorder the list in place...
   * This idea is due to David McClain <Dave_McClain@msn.com>
   */
  if (size <= BUFFSIZE)
    {
      for (i = 0;  i < size; i++)
	{
	  buffer[i]   = SCM_CAR (method_list);
	  method_list = SCM_CDR (method_list);
	}
      v = buffer;
    }
  else
    {
      /* Too many elements in method_list to keep everything locally */
      vector = scm_i_vector2list (save, size);
      v = scm_vector_writable_elements (vector, &handle, NULL, NULL);
    }

  /* Use a simple shell sort since it is generally faster than qsort on
   * small vectors (which is probably mostly the case when we have to
   * sort a list of applicable methods).
   */
  for (incr = size / 2; incr; incr /= 2)
    {
      for (i = incr; i < size; i++)
	{
	  for (j = i - incr; j >= 0; j -= incr)
	    {
	      if (more_specificp (v[j], v[j+incr], targs))
		break;
	      else
		{
		  SCM tmp = v[j + incr];
		  v[j + incr] = v[j];
		  v[j] = tmp;
		}
	    }
	}
    }

  if (size <= BUFFSIZE)
    {
      /* We did it in locally, so restore the original list (reordered) in-place */
      for (i = 0, method_list = save; i < size; i++, v++)
	{
	  SCM_SETCAR (method_list, *v);
	  method_list = SCM_CDR (method_list);
	}
      return save;
    }

  /* If we are here, that's that we did it the hard way... */
  scm_array_handle_release (&handle);
  return scm_vector_to_list (vector);
}

SCM
scm_compute_applicable_methods (SCM gf, SCM args, long len, int find_method_p)
{
  register long i;
  long count = 0;
  SCM l, fl, applicable = SCM_EOL;
  SCM save = args;
  SCM buffer[BUFFSIZE];
  SCM const *types;
  SCM *p;
  SCM tmp = SCM_EOL;
  scm_t_array_handle handle;

  /* Build the list of arguments types */
  if (len >= BUFFSIZE) 
    {
      tmp = scm_c_make_vector (len, SCM_UNDEFINED);
      types = p = scm_vector_writable_elements (tmp, &handle, NULL, NULL);

    /*
      note that we don't have to work to reset the generation
      count. TMP is a new vector anyway, and it is found
      conservatively.
    */
    }
  else
    types = p = buffer;

  for (  ; !scm_is_null (args); args = SCM_CDR (args))
    *p++ = scm_class_of (SCM_CAR (args));
  
  /* Build a list of all applicable methods */
  for (l = scm_generic_function_methods (gf); !scm_is_null (l); l = SCM_CDR (l))
    {
      fl = SPEC_OF (SCM_CAR (l));
      /* Only accept accessors which match exactly in first arg. */
      if (SCM_ACCESSORP (SCM_CAR (l))
	  && (scm_is_null (fl) || types[0] != SCM_CAR (fl)))
	continue;
      for (i = 0; ; i++, fl = SCM_CDR (fl))
	{
	  if (SCM_INSTANCEP (fl)
	      /* We have a dotted argument list */
	      || (i >= len && scm_is_null (fl)))
	    {	/* both list exhausted */
	      applicable = scm_cons (SCM_CAR (l), applicable);
	      count     += 1;
	      break;
	    }
	  if (i >= len
	      || scm_is_null (fl)
	      || !applicablep (types[i], SCM_CAR (fl)))
	    break;
	}
    }

  if (len >= BUFFSIZE)
      scm_array_handle_release (&handle);

  if (count == 0)
    {
      if (find_method_p)
	return SCM_BOOL_F;
      CALL_GF2 ("no-applicable-method", gf, save);
      /* if we are here, it's because no-applicable-method hasn't signaled an error */
      return SCM_BOOL_F;
    }

  return (count == 1
	  ? applicable
	  : sort_applicable_methods (applicable, count, types));
}

#if 0
SCM_PROC (s_sys_compute_applicable_methods, "%compute-applicable-methods", 2, 0, 0, scm_sys_compute_applicable_methods);
#endif

static const char s_sys_compute_applicable_methods[] = "%compute-applicable-methods";

SCM
scm_sys_compute_applicable_methods (SCM gf, SCM args)
#define FUNC_NAME s_sys_compute_applicable_methods
{
  long n;
  SCM_VALIDATE_GENERIC (1, gf);
  n = scm_ilength (args);
  SCM_ASSERT (n >= 0, args, SCM_ARG2, FUNC_NAME);
  return scm_compute_applicable_methods (gf, args, n, 1);
}
#undef FUNC_NAME

SCM_SYMBOL (sym_compute_applicable_methods, "compute-applicable-methods");
SCM_VARIABLE_INIT (var_compute_applicable_methods, "compute-applicable-methods", scm_c_define_gsubr (s_sys_compute_applicable_methods, 2, 0, 0, scm_sys_compute_applicable_methods));

static void
lock_cache_mutex (void *m)
{
  SCM mutex = SCM_PACK ((scm_t_bits) m);
  scm_lock_mutex (mutex);
}

static void
unlock_cache_mutex (void *m)
{
  SCM mutex = SCM_PACK ((scm_t_bits) m);
  scm_unlock_mutex (mutex);
}

static SCM
call_memoize_method (void *a)
{
  SCM args = SCM_PACK ((scm_t_bits) a);
  SCM gf = SCM_CAR (args);
  SCM x = SCM_CADR (args);
  /* First check if another thread has inserted a method between
   * the cache miss and locking the mutex.
   */
  SCM cmethod = scm_mcache_lookup_cmethod (x, SCM_CDDR (args));
  if (scm_is_true (cmethod))
    return cmethod;
  /*fixme* Use scm_apply */
  return CALL_GF3 ("memoize-method!", gf, SCM_CDDR (args), x);
}

SCM
scm_memoize_method (SCM x, SCM args)
{
  SCM gf = SCM_CAR (scm_last_pair (x));
  return scm_internal_dynamic_wind (
    lock_cache_mutex,
    call_memoize_method,
    unlock_cache_mutex,
    (void *) SCM_UNPACK (scm_cons2 (gf, x, args)),
    (void *) SCM_UNPACK (SCM_SLOT (gf, scm_si_cache_mutex)));
}

/******************************************************************************
 *
 * A simple make (which will be redefined later in Scheme)
 * This version handles only creation of gf, methods and classes (no instances)
 *
 * Since this code will disappear when Goops will be fully booted,
 * no precaution is taken to be efficient.
 *
 ******************************************************************************/

SCM_KEYWORD (k_setter,		"setter");
SCM_KEYWORD (k_specializers,	"specializers");
SCM_KEYWORD (k_procedure,	"procedure");
SCM_KEYWORD (k_dsupers,		"dsupers");
SCM_KEYWORD (k_slots,		"slots");
SCM_KEYWORD (k_gf,		"generic-function");

SCM_DEFINE (scm_make, "make",  0, 0, 1,
	    (SCM args),
	    "Make a new object.  @var{args} must contain the class and\n"
	    "all necessary initialization information.")
#define FUNC_NAME s_scm_make
{
  SCM class, z;
  long len = scm_ilength (args);

  if (len <= 0 || (len & 1) == 0)
    SCM_WRONG_NUM_ARGS ();

  class = SCM_CAR(args);
  args  = SCM_CDR(args);

  if (class == scm_class_generic || class == scm_class_accessor)
    {
      z = scm_make_struct (class, SCM_INUM0,
			   scm_list_5 (SCM_EOL,
				       SCM_INUM0,
				       SCM_BOOL_F,
				       scm_make_mutex (),
				       SCM_EOL));
      scm_set_procedure_property_x (z, scm_sym_name,
				    scm_get_keyword (k_name,
						     args,
						     SCM_BOOL_F));
      clear_method_cache (z);
      if (class == scm_class_accessor)
	{
	  SCM setter = scm_get_keyword (k_setter, args, SCM_BOOL_F);
	  if (scm_is_true (setter))
	    scm_sys_set_object_setter_x (z, setter);
	}
    }
  else
    {
      z = scm_sys_allocate_instance (class, args);

      if (class == scm_class_method
	  || class == scm_class_simple_method
	  || class == scm_class_accessor_method)
	{
	  SCM_SET_SLOT (z, scm_si_generic_function,
	    scm_i_get_keyword (k_gf,
			       args,
			       len - 1,
			       SCM_BOOL_F,
			       FUNC_NAME));
	  SCM_SET_SLOT (z, scm_si_specializers,
	    scm_i_get_keyword (k_specializers,
			       args,
			       len - 1,
			       SCM_EOL,
			       FUNC_NAME));
	  SCM_SET_SLOT (z, scm_si_procedure,
	    scm_i_get_keyword (k_procedure,
			       args,
			       len - 1,
			       SCM_EOL,
			       FUNC_NAME));
	  SCM_SET_SLOT (z, scm_si_code_table, SCM_EOL);
	}
      else
	{
	  /* In all the others case, make a new class .... No instance here */
	  SCM_SET_SLOT (z, scm_si_name,
	    scm_i_get_keyword (k_name,
			       args,
			       len - 1,
			       scm_from_locale_symbol ("???"),
			       FUNC_NAME));
	  SCM_SET_SLOT (z, scm_si_direct_supers,
	    scm_i_get_keyword (k_dsupers,
			       args,
			       len - 1,
			       SCM_EOL,
			       FUNC_NAME));
	  SCM_SET_SLOT (z, scm_si_direct_slots,
	    scm_i_get_keyword (k_slots,
			       args,
			       len - 1,
			       SCM_EOL,
			       FUNC_NAME));
	}
    }
  return z;
}
#undef FUNC_NAME

SCM_DEFINE (scm_find_method, "find-method", 0, 0, 1,
	    (SCM l),
	    "")
#define FUNC_NAME s_scm_find_method
{
  SCM gf;
  long len = scm_ilength (l);

  if (len == 0)
    SCM_WRONG_NUM_ARGS ();

  gf = SCM_CAR(l); l = SCM_CDR(l);
  SCM_VALIDATE_GENERIC (1, gf);
  if (scm_is_null (SCM_SLOT (gf, scm_si_methods)))
    SCM_MISC_ERROR ("no methods for generic ~S", scm_list_1 (gf));

  return scm_compute_applicable_methods (gf, l, len - 1, 1);
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_method_more_specific_p, "%method-more-specific?", 3, 0, 0,
	    (SCM m1, SCM m2, SCM targs),
	    "Return true if method @var{m1} is more specific than @var{m2} "
	    "given the argument types (classes) listed in @var{targs}.")
#define FUNC_NAME s_scm_sys_method_more_specific_p
{
  SCM l, v, result;
  SCM *v_elts;
  long i, len, m1_specs, m2_specs;
  scm_t_array_handle handle;

  SCM_VALIDATE_METHOD (1, m1);
  SCM_VALIDATE_METHOD (2, m2);

  len = scm_ilength (targs);
  m1_specs = scm_ilength (SPEC_OF (m1));
  m2_specs = scm_ilength (SPEC_OF (m2));
  SCM_ASSERT ((len >= m1_specs) || (len >= m2_specs),
	      targs, SCM_ARG3, FUNC_NAME);

  /* Verify that all the arguments of TARGS are classes and place them
     in a vector.  */

  v = scm_c_make_vector (len, SCM_EOL);
  v_elts = scm_vector_writable_elements (v, &handle, NULL, NULL);

  for (i = 0, l = targs;
       i < len && scm_is_pair (l);
       i++, l = SCM_CDR (l))
    {
      SCM_ASSERT (SCM_CLASSP (SCM_CAR (l)), targs, SCM_ARG3, FUNC_NAME);
      v_elts[i] = SCM_CAR (l);
    }
  result = more_specificp (m1, m2, v_elts) ? SCM_BOOL_T: SCM_BOOL_F;

  scm_array_handle_release (&handle);

  return result;
}
#undef FUNC_NAME



/******************************************************************************
 *
 * Initializations
 *
 ******************************************************************************/

static void
fix_cpl (SCM c, SCM before, SCM after)
{
  SCM cpl = SCM_SLOT (c, scm_si_cpl);
  SCM ls = scm_c_memq (after, cpl);
  SCM tail = scm_delq1_x (before, SCM_CDR (ls));
  if (scm_is_false (ls))
    /* if this condition occurs, fix_cpl should not be applied this way */
    abort ();
  SCM_SETCAR (ls, before);
  SCM_SETCDR (ls, scm_cons (after, tail));
  {
    SCM dslots = SCM_SLOT (c, scm_si_direct_slots);
    SCM slots = build_slots_list (maplist (dslots), cpl);
    SCM g_n_s = compute_getters_n_setters (slots);
    SCM_SET_SLOT (c, scm_si_slots, slots);
    SCM_SET_SLOT (c, scm_si_getters_n_setters, g_n_s);
  }
}


static void
make_stdcls (SCM *var, char *name, SCM meta, SCM super, SCM slots)
{
   SCM tmp = scm_from_locale_symbol (name);

   *var = scm_permanent_object (scm_basic_make_class (meta,
						      tmp,
						      scm_is_pair (super)
						      ? super
						      : scm_list_1 (super),
						      slots));
   DEFVAR(tmp, *var);
}


SCM_KEYWORD (k_slot_definition, "slot-definition");

static void
create_standard_classes (void)
{
  SCM slots;
  SCM method_slots = scm_list_4 (scm_from_locale_symbol ("generic-function"),
				 scm_from_locale_symbol ("specializers"),
				 sym_procedure,
				 scm_from_locale_symbol ("code-table"));
  SCM amethod_slots = scm_list_1 (scm_list_3 (scm_from_locale_symbol ("slot-definition"),
					      k_init_keyword,
					      k_slot_definition));
  SCM mutex_slot = scm_list_1 (scm_from_locale_symbol ("make-mutex"));
  SCM mutex_closure = scm_i_eval_x (scm_list_3 (scm_sym_lambda,
                                                SCM_EOL,
                                                mutex_slot),
                                    SCM_EOL);
  SCM gf_slots = scm_list_5 (scm_from_locale_symbol ("methods"),
			     scm_list_3 (scm_from_locale_symbol ("n-specialized"),
					 k_init_value,
					 SCM_INUM0),
			     scm_list_3 (scm_from_locale_symbol ("used-by"),
					 k_init_value,
					 SCM_BOOL_F),
			     scm_list_3 (scm_from_locale_symbol ("cache-mutex"),
					 k_init_thunk,
                                         mutex_closure),
			     scm_list_3 (scm_from_locale_symbol ("extended-by"),
					 k_init_value,
					 SCM_EOL));
  SCM egf_slots = scm_list_1 (scm_list_3 (scm_from_locale_symbol ("extends"),
					  k_init_value,
					  SCM_EOL));
  /* Foreign class slot classes */
  make_stdcls (&scm_class_foreign_slot,	   "<foreign-slot>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_protected,	   "<protected-slot>",
	       scm_class_class, scm_class_foreign_slot,	   SCM_EOL);
  make_stdcls (&scm_class_opaque,	   "<opaque-slot>",
	       scm_class_class, scm_class_foreign_slot,	   SCM_EOL);
  make_stdcls (&scm_class_read_only,	   "<read-only-slot>",
	       scm_class_class, scm_class_foreign_slot,	   SCM_EOL);
  make_stdcls (&scm_class_self,		   "<self-slot>",
	       scm_class_class,
	       scm_class_read_only,
	       SCM_EOL);
  make_stdcls (&scm_class_protected_opaque, "<protected-opaque-slot>",
	       scm_class_class,
	       scm_list_2 (scm_class_protected, scm_class_opaque),
	       SCM_EOL);
  make_stdcls (&scm_class_protected_read_only, "<protected-read-only-slot>",
	       scm_class_class,
	       scm_list_2 (scm_class_protected, scm_class_read_only),
	       SCM_EOL);
  make_stdcls (&scm_class_scm,		   "<scm-slot>",
	       scm_class_class, scm_class_protected, SCM_EOL);
  make_stdcls (&scm_class_int,		   "<int-slot>",
	       scm_class_class, scm_class_foreign_slot,	   SCM_EOL);
  make_stdcls (&scm_class_float,	   "<float-slot>",
	       scm_class_class, scm_class_foreign_slot,	   SCM_EOL);
  make_stdcls (&scm_class_double,	   "<double-slot>",
	       scm_class_class, scm_class_foreign_slot,	   SCM_EOL);

  /* Continue initialization of class <class> */

  slots = build_class_class_slots ();
  SCM_SET_SLOT (scm_class_class, scm_si_direct_slots, slots);
  SCM_SET_SLOT (scm_class_class, scm_si_slots, slots);
  SCM_SET_SLOT (scm_class_class, scm_si_getters_n_setters,
		compute_getters_n_setters (slots));

  make_stdcls (&scm_class_foreign_class, "<foreign-class>",
	       scm_class_class, scm_class_class,
	       scm_list_2 (scm_list_3 (scm_from_locale_symbol ("constructor"),
				       k_class,
				       scm_class_opaque),
			   scm_list_3 (scm_from_locale_symbol ("destructor"),
				       k_class,
				       scm_class_opaque)));
  make_stdcls (&scm_class_foreign_object,  "<foreign-object>",
	       scm_class_foreign_class, scm_class_object,   SCM_EOL);
  SCM_SET_CLASS_FLAGS (scm_class_foreign_object, SCM_CLASSF_FOREIGN);

  /* scm_class_generic functions classes */
  make_stdcls (&scm_class_procedure_class, "<procedure-class>",
	       scm_class_class, scm_class_class, SCM_EOL);
  make_stdcls (&scm_class_entity_class,    "<entity-class>",
	       scm_class_class, scm_class_procedure_class, SCM_EOL);
  make_stdcls (&scm_class_operator_class,  "<operator-class>",
	       scm_class_class, scm_class_procedure_class, SCM_EOL);
  make_stdcls (&scm_class_operator_with_setter_class,
	       "<operator-with-setter-class>",
	       scm_class_class, scm_class_operator_class, SCM_EOL);
  make_stdcls (&scm_class_method,	   "<method>",
	       scm_class_class, scm_class_object,	   method_slots);
  make_stdcls (&scm_class_simple_method,   "<simple-method>",
	       scm_class_class, scm_class_method,	   SCM_EOL);
  SCM_SET_CLASS_FLAGS (scm_class_simple_method, SCM_CLASSF_SIMPLE_METHOD);
  make_stdcls (&scm_class_accessor_method, "<accessor-method>",
	       scm_class_class, scm_class_simple_method,   amethod_slots);
  SCM_SET_CLASS_FLAGS (scm_class_accessor_method, SCM_CLASSF_ACCESSOR_METHOD);
  make_stdcls (&scm_class_applicable,	   "<applicable>",
	       scm_class_class, scm_class_top, SCM_EOL);
  make_stdcls (&scm_class_entity,	   "<entity>",
	       scm_class_entity_class,
	       scm_list_2 (scm_class_object, scm_class_applicable),
	       SCM_EOL);
  make_stdcls (&scm_class_entity_with_setter, "<entity-with-setter>",
	       scm_class_entity_class, scm_class_entity,   SCM_EOL);
  make_stdcls (&scm_class_generic,	   "<generic>",
	       scm_class_entity_class, scm_class_entity,   gf_slots);
  SCM_SET_CLASS_FLAGS (scm_class_generic, SCM_CLASSF_PURE_GENERIC);
  make_stdcls (&scm_class_extended_generic, "<extended-generic>",
	       scm_class_entity_class, scm_class_generic, egf_slots);
  SCM_SET_CLASS_FLAGS (scm_class_extended_generic, SCM_CLASSF_PURE_GENERIC);
  make_stdcls (&scm_class_generic_with_setter, "<generic-with-setter>",
	       scm_class_entity_class,
	       scm_list_2 (scm_class_generic, scm_class_entity_with_setter),
	       SCM_EOL);
  SCM_SET_CLASS_FLAGS (scm_class_generic_with_setter, SCM_CLASSF_PURE_GENERIC);
  make_stdcls (&scm_class_accessor,	   "<accessor>",
	       scm_class_entity_class, scm_class_generic_with_setter, SCM_EOL);
  SCM_SET_CLASS_FLAGS (scm_class_accessor, SCM_CLASSF_PURE_GENERIC);
  make_stdcls (&scm_class_extended_generic_with_setter,
	       "<extended-generic-with-setter>",
	       scm_class_entity_class,
	       scm_list_2 (scm_class_generic_with_setter,
			   scm_class_extended_generic),
	       SCM_EOL);
  SCM_SET_CLASS_FLAGS (scm_class_extended_generic_with_setter,
		       SCM_CLASSF_PURE_GENERIC);
  make_stdcls (&scm_class_extended_accessor, "<extended-accessor>",
	       scm_class_entity_class,
	       scm_list_2 (scm_class_accessor,
			   scm_class_extended_generic_with_setter),
	       SCM_EOL);
  fix_cpl (scm_class_extended_accessor,
	   scm_class_extended_generic, scm_class_generic);
  SCM_SET_CLASS_FLAGS (scm_class_extended_accessor, SCM_CLASSF_PURE_GENERIC);

  /* Primitive types classes */
  make_stdcls (&scm_class_boolean, 	   "<boolean>",
	       scm_class_class, scm_class_top, 	    	   SCM_EOL);
  make_stdcls (&scm_class_char,		   "<char>",
	       scm_class_class, scm_class_top,	    	   SCM_EOL);
  make_stdcls (&scm_class_list,	   	   "<list>",
	       scm_class_class, scm_class_top,	    	   SCM_EOL);
  make_stdcls (&scm_class_pair,		   "<pair>",
	       scm_class_class, scm_class_list,		   SCM_EOL);
  make_stdcls (&scm_class_null,		   "<null>",
	       scm_class_class, scm_class_list,		   SCM_EOL);
  make_stdcls (&scm_class_string,	   "<string>",
	       scm_class_class, scm_class_top,	    	   SCM_EOL);
  make_stdcls (&scm_class_symbol,	   "<symbol>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_vector,	   "<vector>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_number,	   "<number>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_complex,	   "<complex>",
	       scm_class_class, scm_class_number, 	   SCM_EOL);
  make_stdcls (&scm_class_real,		   "<real>",
	       scm_class_class, scm_class_complex,	   SCM_EOL);
  make_stdcls (&scm_class_integer,	   "<integer>",
	       scm_class_class, scm_class_real,		   SCM_EOL);
  make_stdcls (&scm_class_fraction,	   "<fraction>",
	       scm_class_class, scm_class_real,		   SCM_EOL);
  make_stdcls (&scm_class_keyword,	   "<keyword>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_unknown,	   "<unknown>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_procedure,	   "<procedure>",
	       scm_class_procedure_class, scm_class_applicable, SCM_EOL);
  make_stdcls (&scm_class_procedure_with_setter, "<procedure-with-setter>",
	       scm_class_procedure_class, scm_class_procedure, SCM_EOL);
  make_stdcls (&scm_class_primitive_generic, "<primitive-generic>",
	       scm_class_procedure_class, scm_class_procedure, SCM_EOL);
  make_stdcls (&scm_class_port,		   "<port>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_input_port,	   "<input-port>",
	       scm_class_class, scm_class_port,		   SCM_EOL);
  make_stdcls (&scm_class_output_port,	   "<output-port>",
	       scm_class_class, scm_class_port,		   SCM_EOL);
  make_stdcls (&scm_class_input_output_port, "<input-output-port>",
	       scm_class_class,
	       scm_list_2 (scm_class_input_port, scm_class_output_port),
	       SCM_EOL);
}

/**********************************************************************
 *
 * Smob classes
 *
 **********************************************************************/

static SCM
make_class_from_template (char const *template, char const *type_name, SCM supers, int applicablep)
{
  SCM class, name;
  if (type_name)
    {
      char buffer[100];
      sprintf (buffer, template, type_name);
      name = scm_from_locale_symbol (buffer);
    }
  else
    name = SCM_GOOPS_UNBOUND;

  class = scm_permanent_object (scm_basic_make_class (applicablep
						      ? scm_class_procedure_class
						      : scm_class_class,
						      name,
						      supers,
						      SCM_EOL));

  /* Only define name if doesn't already exist. */
  if (!SCM_GOOPS_UNBOUNDP (name)
      && scm_is_false (scm_call_2 (scm_goops_lookup_closure, name, SCM_BOOL_F)))
    DEFVAR (name, class);
  return class;
}

SCM
scm_make_extended_class (char const *type_name, int applicablep)
{
  return make_class_from_template ("<%s>",
				   type_name,
				   scm_list_1 (applicablep
					       ? scm_class_applicable
					       : scm_class_top),
				   applicablep);
}

void
scm_i_inherit_applicable (SCM c)
{
  if (!SCM_SUBCLASSP (c, scm_class_applicable))
    {
      SCM dsupers = SCM_SLOT (c, scm_si_direct_supers);
      SCM cpl = SCM_SLOT (c, scm_si_cpl);
      /* patch scm_class_applicable into direct-supers */
      SCM top = scm_c_memq (scm_class_top, dsupers);
      if (scm_is_false (top))
	dsupers = scm_append (scm_list_2 (dsupers,
					  scm_list_1 (scm_class_applicable)));
      else
	{
	  SCM_SETCAR (top, scm_class_applicable);
	  SCM_SETCDR (top, scm_cons (scm_class_top, SCM_CDR (top)));
	}
      SCM_SET_SLOT (c, scm_si_direct_supers, dsupers);
      /* patch scm_class_applicable into cpl */
      top = scm_c_memq (scm_class_top, cpl);
      if (scm_is_false (top))
	abort ();
      else
	{
	  SCM_SETCAR (top, scm_class_applicable);
	  SCM_SETCDR (top, scm_cons (scm_class_top, SCM_CDR (top)));
	}
      /* add class to direct-subclasses of scm_class_applicable */
      SCM_SET_SLOT (scm_class_applicable,
		    scm_si_direct_subclasses,
		    scm_cons (c, SCM_SLOT (scm_class_applicable,
					   scm_si_direct_subclasses)));
    }
}

static void
create_smob_classes (void)
{
  long i;

  scm_smob_class = (SCM *) scm_malloc (255 * sizeof (SCM));
  for (i = 0; i < 255; ++i)
    scm_smob_class[i] = 0;

  scm_smob_class[SCM_TC2SMOBNUM (scm_tc16_keyword)] = scm_class_keyword;

  for (i = 0; i < scm_numsmob; ++i)
    if (!scm_smob_class[i])
      scm_smob_class[i] = scm_make_extended_class (SCM_SMOBNAME (i),
						   scm_smobs[i].apply != 0);
}

void
scm_make_port_classes (long ptobnum, char *type_name)
{
  SCM c, class = make_class_from_template ("<%s-port>",
					   type_name,
					   scm_list_1 (scm_class_port),
					   0);
  scm_port_class[SCM_IN_PCLASS_INDEX + ptobnum]
    = make_class_from_template ("<%s-input-port>",
				type_name,
				scm_list_2 (class, scm_class_input_port),
				0);
  scm_port_class[SCM_OUT_PCLASS_INDEX + ptobnum]
    = make_class_from_template ("<%s-output-port>",
				type_name,
				scm_list_2 (class, scm_class_output_port),
				0);
  scm_port_class[SCM_INOUT_PCLASS_INDEX + ptobnum]
    = c
    = make_class_from_template ("<%s-input-output-port>",
				type_name,
				scm_list_2 (class, scm_class_input_output_port),
				0);
  /* Patch cpl (since this tree is too complex for the C level compute-cpl) */
  SCM_SET_SLOT (c, scm_si_cpl,
		scm_cons2 (c, class, SCM_SLOT (scm_class_input_output_port, scm_si_cpl)));
}

static void
create_port_classes (void)
{
  long i;

  scm_port_class = (SCM *) scm_malloc (3 * 256 * sizeof (SCM));
  for (i = 0; i < 3 * 256; ++i)
    scm_port_class[i] = 0;

  for (i = 0; i < scm_numptob; ++i)
    scm_make_port_classes (i, SCM_PTOBNAME (i));
}

static SCM
make_struct_class (void *closure SCM_UNUSED,
		   SCM vtable, SCM data, SCM prev SCM_UNUSED)
{
  if (scm_is_true (SCM_STRUCT_TABLE_NAME (data)))
    SCM_SET_STRUCT_TABLE_CLASS (data,
				scm_make_extended_class
				(scm_i_symbol_chars (SCM_STRUCT_TABLE_NAME (data)),
				 SCM_CLASS_FLAGS (vtable) & SCM_CLASSF_OPERATOR));
  return SCM_UNSPECIFIED;
}

static void
create_struct_classes (void)
{
  scm_internal_hash_fold (make_struct_class, 0, SCM_BOOL_F, scm_struct_table);
}

/**********************************************************************
 *
 * C interface
 *
 **********************************************************************/

void
scm_load_goops ()
{
  if (!goops_loaded_p)
    scm_c_resolve_module ("oop goops");
}


SCM
scm_make_foreign_object (SCM class, SCM initargs)
#define FUNC_NAME s_scm_make
{
  void * (*constructor) (SCM)
    = (void * (*) (SCM)) SCM_SLOT (class, scm_si_constructor);
  if (constructor == 0)
    SCM_MISC_ERROR ("Can't make instances of class ~S", scm_list_1 (class));
  return scm_wrap_object (class, constructor (initargs));
}
#undef FUNC_NAME


static size_t
scm_free_foreign_object (SCM *class, SCM *data)
{
  size_t (*destructor) (void *)
    = (size_t (*) (void *)) class[scm_si_destructor];
  return destructor (data);
}

SCM
scm_make_class (SCM meta, char *s_name, SCM supers, size_t size,
		void * (*constructor) (SCM initargs),
		size_t (*destructor) (void *))
{
  SCM name, class;
  name = scm_from_locale_symbol (s_name);
  if (scm_is_null (supers))
    supers = scm_list_1 (scm_class_foreign_object);
  class = scm_basic_basic_make_class (meta, name, supers, SCM_EOL);
  scm_sys_inherit_magic_x (class, supers);

  if (destructor != 0)
    {
      SCM_SET_SLOT (class, scm_si_destructor, (SCM) destructor);
      SCM_SET_CLASS_DESTRUCTOR (class, scm_free_foreign_object);
    }
  else if (size > 0)
    {
      SCM_SET_CLASS_DESTRUCTOR (class, scm_struct_free_light);
      SCM_SET_CLASS_INSTANCE_SIZE (class, size);
    }

  SCM_SET_SLOT (class, scm_si_layout, scm_from_locale_symbol (""));
  SCM_SET_SLOT (class, scm_si_constructor, (SCM) constructor);

  return class;
}

SCM_SYMBOL (sym_o, "o");
SCM_SYMBOL (sym_x, "x");

SCM_KEYWORD (k_accessor, "accessor");
SCM_KEYWORD (k_getter, "getter");

static SCM
default_setter (SCM obj SCM_UNUSED, SCM c SCM_UNUSED)
{
  scm_misc_error ("slot-set!", "read-only slot", SCM_EOL);
  return 0;
}

void
scm_add_slot (SCM class, char *slot_name, SCM slot_class,
	      SCM (*getter) (SCM obj),
	      SCM (*setter) (SCM obj, SCM x),
	      char *accessor_name)
{
  {
    SCM get = scm_c_make_subr ("goops:get", scm_tc7_subr_1, getter);
    SCM set = scm_c_make_subr ("goops:set", scm_tc7_subr_2,
			       setter ? setter : default_setter);

    /* Dirk:FIXME:: The following two expressions make use of the fact that
     * the memoizer will accept a subr-object in the place of a function.
     * This is not guaranteed to stay this way.  */
    SCM getm = scm_i_eval_x (scm_list_3 (scm_sym_lambda,
                                         scm_list_1 (sym_o),
                                         scm_list_2 (get, sym_o)),
                             SCM_EOL);
    SCM setm = scm_i_eval_x (scm_list_3 (scm_sym_lambda,
                                         scm_list_2 (sym_o, sym_x),
                                         scm_list_3 (set, sym_o, sym_x)),
                             SCM_EOL);

    {
      SCM name = scm_from_locale_symbol (slot_name);
      SCM aname = scm_from_locale_symbol (accessor_name);
      SCM gf = scm_ensure_accessor (aname);
      SCM slot = scm_list_5 (name,
			     k_class,
			     slot_class,
			     setter ? k_accessor : k_getter,
			     gf);
      scm_add_method (gf, scm_make (scm_list_5 (scm_class_accessor_method,
						k_specializers,
						scm_list_1 (class),
						k_procedure,
						getm)));
      scm_add_method (scm_setter (gf),
		      scm_make (scm_list_5 (scm_class_accessor_method,
					    k_specializers,
					    scm_list_2 (class, scm_class_top),
					    k_procedure,
					    setm)));
      DEFVAR (aname, gf);

      SCM_SET_SLOT (class, scm_si_slots,
		    scm_append_x (scm_list_2 (SCM_SLOT (class, scm_si_slots),
					      scm_list_1 (slot))));
      {
	SCM n = SCM_SLOT (class, scm_si_nfields);
	SCM gns = scm_list_n (name, SCM_BOOL_F, get, set, n, scm_from_int (1),
			      SCM_UNDEFINED);
	SCM_SET_SLOT (class, scm_si_getters_n_setters,
		      scm_append_x (scm_list_2 (SCM_SLOT (class, scm_si_getters_n_setters),
						scm_list_1 (gns))));
	SCM_SET_SLOT (class, scm_si_nfields, scm_sum (n, scm_from_int (1)));
      }
    }
  }
}

SCM
scm_wrap_object (SCM class, void *data)
{
  return scm_double_cell (SCM_UNPACK (SCM_CDR (class)) | scm_tc3_struct,
			  (scm_t_bits) data,
			  0, 0);
}

SCM scm_components;

SCM
scm_wrap_component (SCM class, SCM container, void *data)
{
  SCM obj = scm_wrap_object (class, data);
  SCM handle = scm_hash_fn_create_handle_x (scm_components,
					    obj,
					    SCM_BOOL_F,
					    scm_struct_ihashq,
					    scm_sloppy_assq,
					    0);
  SCM_SETCDR (handle, container);
  return obj;
}

SCM
scm_ensure_accessor (SCM name)
{
  SCM gf = scm_call_2 (SCM_TOP_LEVEL_LOOKUP_CLOSURE, name, SCM_BOOL_F);
  if (!SCM_IS_A_P (gf, scm_class_accessor))
    {
      gf = scm_make (scm_list_3 (scm_class_generic, k_name, name));
      gf = scm_make (scm_list_5 (scm_class_accessor,
				 k_name, name, k_setter, gf));
    }
  return gf;
}

SCM_SYMBOL (sym_internal_add_method_x, "internal-add-method!");

void
scm_add_method (SCM gf, SCM m)
{
  scm_eval (scm_list_3 (sym_internal_add_method_x, gf, m), scm_module_goops);
}

#ifdef GUILE_DEBUG
/*
 * Debugging utilities
 */

SCM_DEFINE (scm_pure_generic_p, "pure-generic?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a pure generic.")
#define FUNC_NAME s_scm_pure_generic_p
{
  return scm_from_bool (SCM_PUREGENERICP (obj));
}
#undef FUNC_NAME

#endif /* GUILE_DEBUG */

/*
 * Initialization
 */

SCM_DEFINE (scm_sys_goops_loaded, "%goops-loaded", 0, 0, 0,
	    (),
	    "Announce that GOOPS is loaded and perform initialization\n"
	    "on the C level which depends on the loaded GOOPS modules.")
#define FUNC_NAME s_scm_sys_goops_loaded
{
  goops_loaded_p = 1;
  var_compute_applicable_methods =
    scm_sym2var (sym_compute_applicable_methods, scm_goops_lookup_closure,
		 SCM_BOOL_F);
  setup_extended_primitive_generics ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM scm_module_goops;

SCM
scm_init_goops_builtins (void)
{
  scm_module_goops = scm_current_module ();
  scm_goops_lookup_closure = scm_module_lookup_closure (scm_module_goops);

  /* Not really necessary right now, but who knows...
   */
  scm_permanent_object (scm_module_goops);
  scm_permanent_object (scm_goops_lookup_closure);

  scm_components = scm_permanent_object (scm_make_weak_key_hash_table
					 (scm_from_int (37)));

  goops_rstate = scm_c_make_rstate ("GOOPS", 5);

#include "libguile/goops.x"

  list_of_no_method = scm_permanent_object (scm_list_1 (sym_no_method));

  hell = scm_calloc (hell_size * sizeof (*hell));
  hell_mutex = scm_permanent_object (scm_make_mutex ());

  create_basic_classes ();
  create_standard_classes ();
  create_smob_classes ();
  create_struct_classes ();
  create_port_classes ();

  {
    SCM name = scm_from_locale_symbol ("no-applicable-method");
    scm_no_applicable_method
      = scm_permanent_object (scm_make (scm_list_3 (scm_class_generic,
						    k_name,
						    name)));
    DEFVAR (name, scm_no_applicable_method);
  }

  return SCM_UNSPECIFIED;
}

void
scm_init_goops ()
{
  scm_c_define_gsubr ("%init-goops-builtins", 0, 0, 0,
		      scm_init_goops_builtins);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
