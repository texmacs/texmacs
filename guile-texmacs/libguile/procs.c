/* Copyright (C) 1995,1996,1997,1999,2000,2001, 2006, 2008 Free Software Foundation, Inc.
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

#include "libguile/objects.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"
#include "libguile/smob.h"
#include "libguile/deprecation.h"

#include "libguile/validate.h"
#include "libguile/procs.h"



/* {Procedures}
 */

scm_t_subr_entry *scm_subr_table;

/* libguile contained approx. 700 primitive procedures on 24 Aug 1999. */

/* Increased to 800 on 2001-05-07 -- Guile now has 779 primitives on
   startup, 786 with guile-readline.  'martin */

long scm_subr_table_size = 0;
long scm_subr_table_room = 800;

SCM 
scm_c_make_subr (const char *name, long type, SCM (*fcn) ())
{
  register SCM z;
  long entry;

  if (scm_subr_table_size == scm_subr_table_room)
    {
      long new_size = scm_subr_table_room * 3 / 2;
      void *new_table
	= scm_realloc ((char *) scm_subr_table,
		       sizeof (scm_t_subr_entry) * new_size);
      scm_subr_table = new_table;
      scm_subr_table_room = new_size;
    }

  entry = scm_subr_table_size;
  z = scm_cell ((entry << 8) + type, (scm_t_bits) fcn);
  scm_subr_table[entry].handle = z;
  scm_subr_table[entry].name = scm_from_locale_symbol (name);
  scm_subr_table[entry].generic = 0;
  scm_subr_table[entry].properties = SCM_EOL;
  scm_subr_table_size++;
  
  return z;
}

SCM
scm_c_define_subr (const char *name, long type, SCM (*fcn) ())
{
  SCM subr = scm_c_make_subr (name, type, fcn);
  scm_define (SCM_SUBR_ENTRY(subr).name, subr);
  return subr;
}

/* This function isn't currently used since subrs are never freed. */
/* *fixme* Need mutex here. */
void
scm_free_subr_entry (SCM subr)
{
  long entry = SCM_SUBRNUM (subr);
  /* Move last entry in table to the free position */
  scm_subr_table[entry] = scm_subr_table[scm_subr_table_size - 1];
  SCM_SET_SUBRNUM (scm_subr_table[entry].handle, entry);
  scm_subr_table_size--;
}

SCM
scm_c_make_subr_with_generic (const char *name, 
			      long type, SCM (*fcn) (), SCM *gf)
{
  SCM subr = scm_c_make_subr (name, type, fcn);
  SCM_SUBR_ENTRY(subr).generic = gf;
  return subr;
}

SCM
scm_c_define_subr_with_generic (const char *name, 
				long type, SCM (*fcn) (), SCM *gf)
{
  SCM subr = scm_c_make_subr_with_generic (name, type, fcn, gf);
  scm_define (SCM_SUBR_ENTRY(subr).name, subr);
  return subr;
}

void
scm_mark_subr_table ()
{
  long i;
  for (i = 0; i < scm_subr_table_size; ++i)
    {
      scm_gc_mark (scm_subr_table[i].name);
      if (scm_subr_table[i].generic && *scm_subr_table[i].generic)
	scm_gc_mark (*scm_subr_table[i].generic);
      if (SCM_NIMP (scm_subr_table[i].properties))
	scm_gc_mark (scm_subr_table[i].properties);
    }
}


#ifdef CCLO
SCM 
scm_makcclo (SCM proc, size_t len)
{
  scm_t_bits *base = scm_gc_malloc (len * sizeof (scm_t_bits),
				    "compiled closure");
  unsigned long i;
  SCM s;

  for (i = 0; i < len; ++i)
    base [i] = SCM_UNPACK (SCM_UNSPECIFIED);

  s = scm_cell (SCM_MAKE_CCLO_TAG (len), (scm_t_bits) base);
  SCM_SET_CCLO_SUBR (s, proc);
  return s;
}

/* Undocumented debugging procedure */
#ifdef GUILE_DEBUG
SCM_DEFINE (scm_make_cclo, "make-cclo", 2, 0, 0,
            (SCM proc, SCM len),
	    "Create a compiled closure for @var{proc}, which reserves\n"
	    "@var{len} objects for its usage.")
#define FUNC_NAME s_scm_make_cclo
{
  return scm_makcclo (proc, scm_to_size_t (len));
}
#undef FUNC_NAME
#endif
#endif



SCM_DEFINE (scm_procedure_p, "procedure?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a procedure.")
#define FUNC_NAME s_scm_procedure_p
{
  if (SCM_NIMP (obj))
    switch (SCM_TYP7 (obj))
      {
      case scm_tcs_struct:
	if (!SCM_I_OPERATORP (obj))
	  break;
      case scm_tcs_closures:
      case scm_tcs_subrs:
#ifdef CCLO
      case scm_tc7_cclo:
#endif
      case scm_tc7_pws:
	return SCM_BOOL_T;
      case scm_tc7_smob:
	return scm_from_bool (SCM_SMOB_DESCRIPTOR (obj).apply);
      default:
	return SCM_BOOL_F;
      }
  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_closure_p, "closure?", 1, 0, 0, 
           (SCM obj),
	    "Return @code{#t} if @var{obj} is a closure.")
#define FUNC_NAME s_scm_closure_p
{
  return scm_from_bool (SCM_CLOSUREP (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_thunk_p, "thunk?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a thunk.")
#define FUNC_NAME s_scm_thunk_p
{
  if (SCM_NIMP (obj))
    {
    again:
      switch (SCM_TYP7 (obj))
	{
	case scm_tcs_closures:
	  return scm_from_bool (!scm_is_pair (SCM_CLOSURE_FORMALS (obj)));
	case scm_tc7_subr_0:
	case scm_tc7_subr_1o:
	case scm_tc7_lsubr:
	case scm_tc7_rpsubr:
	case scm_tc7_asubr:
#ifdef CCLO
	case scm_tc7_cclo:
#endif
	  return SCM_BOOL_T;
	case scm_tc7_pws:
	  obj = SCM_PROCEDURE (obj);
	  goto again;
	default:
	  ;
	}
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME

/* Only used internally. */
int
scm_subr_p (SCM obj)
{
  if (SCM_NIMP (obj))
    switch (SCM_TYP7 (obj))
      {
      case scm_tcs_subrs:
	return 1;
      default:
	;
      }
  return 0;
}

SCM_DEFINE (scm_procedure_documentation, "procedure-documentation", 1, 0, 0, 
           (SCM proc),
	    "Return the documentation string associated with @code{proc}.  By\n"
	    "convention, if a procedure contains more than one expression and the\n"
	    "first expression is a string constant, that string is assumed to contain\n"
	    "documentation for that procedure.")
#define FUNC_NAME s_scm_procedure_documentation
{
  SCM code;
  SCM_ASSERT (scm_is_true (scm_procedure_p (proc)),
	      proc, SCM_ARG1, FUNC_NAME);
  switch (SCM_TYP7 (proc))
    {
    case scm_tcs_closures:
      code = SCM_CLOSURE_BODY (proc);
      if (scm_is_null (SCM_CDR (code)))
	return SCM_BOOL_F;
      code = SCM_CAR (code);
      if (scm_is_string (code))
	return code;
      else
	return SCM_BOOL_F;
    default:
      return SCM_BOOL_F;
/*
  case scm_tcs_subrs:
#ifdef CCLO
  case scm_tc7_cclo:
#endif
*/
    }
}
#undef FUNC_NAME


/* Procedure-with-setter
 */

SCM_DEFINE (scm_procedure_with_setter_p, "procedure-with-setter?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a procedure with an\n"
	    "associated setter procedure.")
#define FUNC_NAME s_scm_procedure_with_setter_p
{
  return scm_from_bool(SCM_PROCEDURE_WITH_SETTER_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_procedure_with_setter, "make-procedure-with-setter", 2, 0, 0, 
            (SCM procedure, SCM setter),
	    "Create a new procedure which behaves like @var{procedure}, but\n"
	    "with the associated setter @var{setter}.")
#define FUNC_NAME s_scm_make_procedure_with_setter
{
  SCM_VALIDATE_PROC (1, procedure);
  SCM_VALIDATE_PROC (2, setter);
  return scm_double_cell (scm_tc7_pws,
			  SCM_UNPACK (procedure),
			  SCM_UNPACK (setter), 0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_procedure, "procedure", 1, 0, 0, 
            (SCM proc),
	    "Return the procedure of @var{proc}, which must be either a\n"
	    "procedure with setter, or an operator struct.")
#define FUNC_NAME s_scm_procedure
{
  SCM_VALIDATE_NIM (1, proc);
  if (SCM_PROCEDURE_WITH_SETTER_P (proc))
    return SCM_PROCEDURE (proc);
  else if (SCM_STRUCTP (proc))
    {
      SCM_ASSERT (SCM_I_OPERATORP (proc), proc, SCM_ARG1, FUNC_NAME);
      return proc;
    }
  SCM_WRONG_TYPE_ARG (1, proc);
  return SCM_BOOL_F; /* not reached */
}
#undef FUNC_NAME

SCM_GPROC (s_setter, "setter", 1, 0, 0, scm_setter, g_setter);

SCM
scm_setter (SCM proc)
{
  SCM_GASSERT1 (SCM_NIMP (proc), g_setter, proc, SCM_ARG1, s_setter);
  if (SCM_PROCEDURE_WITH_SETTER_P (proc))
    return SCM_SETTER (proc);
  else if (SCM_STRUCTP (proc))
    {
      SCM setter;
      SCM_GASSERT1 (SCM_I_OPERATORP (proc),
		    g_setter, proc, SCM_ARG1, s_setter);
      setter = (SCM_I_ENTITYP (proc)
		? SCM_ENTITY_SETTER (proc)
		: SCM_OPERATOR_SETTER (proc));
      if (SCM_NIMP (setter))
	return setter;
      /* fall through */
    }
  SCM_WTA_DISPATCH_1 (g_setter, proc, SCM_ARG1, s_setter);
  return SCM_BOOL_F; /* not reached */
}


void
scm_init_subr_table ()
{
  scm_subr_table
    = ((scm_t_subr_entry *)
       scm_malloc (sizeof (scm_t_subr_entry) * scm_subr_table_room));
}

void
scm_init_procs ()
{
#include "libguile/procs.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
