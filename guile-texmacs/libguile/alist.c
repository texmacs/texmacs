/* Copyright (C) 1995, 96, 97, 98, 99, 2000, 2001, 2004, 2006, 2008 Free Software Foundation, Inc.
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
#include "libguile/list.h"
#include "libguile/lang.h"

#include "libguile/validate.h"
#include "libguile/pairs.h"
#include "libguile/alist.h"



SCM_DEFINE (scm_acons, "acons", 3, 0, 0,
           (SCM key, SCM value, SCM alist),
	    "Add a new key-value pair to @var{alist}.  A new pair is\n"
	    "created whose car is @var{key} and whose cdr is @var{value}, and the\n"
	    "pair is consed onto @var{alist}, and the new list is returned.  This\n"
	    "function is @emph{not} destructive; @var{alist} is not modified.")
#define FUNC_NAME s_scm_acons
{
  return scm_cell (SCM_UNPACK (scm_cell (SCM_UNPACK (key),
					 SCM_UNPACK (value))),
		   SCM_UNPACK (alist));
}
#undef FUNC_NAME



SCM_DEFINE (scm_sloppy_assq, "sloppy-assq", 2, 0, 0,
            (SCM key, SCM alist),
	    "Behaves like @code{assq} but does not do any error checking.\n"
	    "Recommended only for use in Guile internals.")
#define FUNC_NAME s_scm_sloppy_assq
{
  for (; scm_is_pair (alist); alist = SCM_CDR (alist))
    {
      SCM tmp = SCM_CAR (alist);
      if (scm_is_pair (tmp) && scm_is_eq (SCM_CAR (tmp), key))
	return tmp;
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME



SCM_DEFINE (scm_sloppy_assv, "sloppy-assv", 2, 0, 0,
            (SCM key, SCM alist),
	    "Behaves like @code{assv} but does not do any error checking.\n"
	    "Recommended only for use in Guile internals.")
#define FUNC_NAME s_scm_sloppy_assv
{
  for (; scm_is_pair (alist); alist = SCM_CDR (alist))
    {
      SCM tmp = SCM_CAR (alist);
      if (scm_is_pair (tmp)
	  && scm_is_true (scm_eqv_p (SCM_CAR (tmp), key)))
	return tmp;
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_sloppy_assoc, "sloppy-assoc", 2, 0, 0,
            (SCM key, SCM alist),
	    "Behaves like @code{assoc} but does not do any error checking.\n"
	    "Recommended only for use in Guile internals.")
#define FUNC_NAME s_scm_sloppy_assoc
{
  for (; scm_is_pair (alist); alist = SCM_CDR (alist))
    {
      SCM tmp = SCM_CAR (alist);
      if (scm_is_pair (tmp)
	  && scm_is_true (scm_equal_p (SCM_CAR (tmp), key)))
	return tmp;
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME




SCM_DEFINE (scm_assq, "assq", 2, 0, 0,
           (SCM key, SCM alist),
	    "@deffnx {Scheme Procedure} assv key alist\n"
	    "@deffnx {Scheme Procedure} assoc key alist\n"
	    "Fetch the entry in @var{alist} that is associated with @var{key}.  To\n"
	    "decide whether the argument @var{key} matches a particular entry in\n"
	    "@var{alist}, @code{assq} compares keys with @code{eq?}, @code{assv}\n"
	    "uses @code{eqv?} and @code{assoc} uses @code{equal?}.  If @var{key}\n"
	    "cannot be found in @var{alist} (according to whichever equality\n"
	    "predicate is in use), then return @code{#f}.  These functions\n"
	    "return the entire alist entry found (i.e. both the key and the value).")
#define FUNC_NAME s_scm_assq
{
  SCM ls = alist;
  for(; scm_is_pair (ls); ls = SCM_CDR (ls)) 
    {
      SCM tmp = SCM_CAR (ls);
      SCM_ASSERT_TYPE (scm_is_pair (tmp), alist, SCM_ARG2, FUNC_NAME,
		       "association list");
      if (scm_is_eq (SCM_CAR (tmp), key))
	return tmp;
    }
  SCM_ASSERT_TYPE (SCM_NULL_OR_NIL_P (ls), alist, SCM_ARG2, FUNC_NAME,
		   "association list");
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_assv, "assv", 2, 0, 0,
           (SCM key, SCM alist),
	    "Behaves like @code{assq} but uses @code{eqv?} for key comparison.")
#define FUNC_NAME s_scm_assv
{
  SCM ls = alist;
  for(; scm_is_pair (ls); ls = SCM_CDR (ls)) 
    {
      SCM tmp = SCM_CAR (ls);
      SCM_ASSERT_TYPE (scm_is_pair (tmp), alist, SCM_ARG2, FUNC_NAME,
		       "association list");
      if (scm_is_true (scm_eqv_p (SCM_CAR (tmp), key)))
	return tmp;
    }
  SCM_ASSERT_TYPE (SCM_NULL_OR_NIL_P (ls), alist, SCM_ARG2, FUNC_NAME,
		   "association list");
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_assoc, "assoc", 2, 0, 0,
           (SCM key, SCM alist),
	    "Behaves like @code{assq} but uses @code{equal?} for key comparison.")
#define FUNC_NAME s_scm_assoc
{
  SCM ls = alist;
  for(; scm_is_pair (ls); ls = SCM_CDR (ls)) 
    {
      SCM tmp = SCM_CAR (ls);
      SCM_ASSERT_TYPE (scm_is_pair (tmp), alist, SCM_ARG2, FUNC_NAME,
		       "association list");
      if (scm_is_true (scm_equal_p (SCM_CAR (tmp), key)))
	return tmp;
    }
  SCM_ASSERT_TYPE (SCM_NULL_OR_NIL_P (ls), alist, SCM_ARG2, FUNC_NAME,
		   "association list");
  return SCM_BOOL_F;
}
#undef FUNC_NAME




/* Dirk:API2.0:: We should not return #f if the key was not found.  In the
 * current solution we can not distinguish between finding a (key . #f) pair
 * and not finding the key at all.
 *
 * Possible alternative solutions:
 * 1) Remove assq-ref from the API:  assq is sufficient.
 * 2) Signal an error (what error type?) if the key is not found.
 * 3) provide an additional 'default' parameter.
 * 3.1) The default parameter is mandatory.
 * 3.2) The default parameter is optional, but if no default is given and
 *      the key is not found, signal an error (what error type?).
 */
SCM_DEFINE (scm_assq_ref, "assq-ref", 2, 0, 0,
            (SCM alist, SCM key),
	    "@deffnx {Scheme Procedure} assv-ref alist key\n"
	    "@deffnx {Scheme Procedure} assoc-ref alist key\n"
	    "Like @code{assq}, @code{assv} and @code{assoc}, except that only the\n"
	    "value associated with @var{key} in @var{alist} is returned.  These\n"
	    "functions are equivalent to\n\n"
	    "@lisp\n"
	    "(let ((ent (@var{associator} @var{key} @var{alist})))\n"
	    "  (and ent (cdr ent)))\n"
	    "@end lisp\n\n"
	    "where @var{associator} is one of @code{assq}, @code{assv} or @code{assoc}.")
#define FUNC_NAME s_scm_assq_ref
{
  SCM handle;

  handle = scm_sloppy_assq (key, alist);
  if (scm_is_pair (handle))
    {
      return SCM_CDR (handle);
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_assv_ref, "assv-ref", 2, 0, 0,
            (SCM alist, SCM key),
	    "Behaves like @code{assq-ref} but uses @code{eqv?} for key comparison.")
#define FUNC_NAME s_scm_assv_ref
{
  SCM handle;

  handle = scm_sloppy_assv (key, alist);
  if (scm_is_pair (handle))
    {
      return SCM_CDR (handle);
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_assoc_ref, "assoc-ref", 2, 0, 0,
            (SCM alist, SCM key),
	    "Behaves like @code{assq-ref} but uses @code{equal?} for key comparison.")
#define FUNC_NAME s_scm_assoc_ref
{
  SCM handle;

  handle = scm_sloppy_assoc (key, alist);
  if (scm_is_pair (handle))
    {
      return SCM_CDR (handle);
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME






SCM_DEFINE (scm_assq_set_x, "assq-set!", 3, 0, 0,
            (SCM alist, SCM key, SCM val),
	    "@deffnx {Scheme Procedure} assv-set! alist key value\n"
	    "@deffnx {Scheme Procedure} assoc-set! alist key value\n"
	    "Reassociate @var{key} in @var{alist} with @var{value}: find any existing\n"
	    "@var{alist} entry for @var{key} and associate it with the new\n"
	    "@var{value}.  If @var{alist} does not contain an entry for @var{key},\n"
	    "add a new one.  Return the (possibly new) alist.\n\n"
	    "These functions do not attempt to verify the structure of @var{alist},\n"
	    "and so may cause unusual results if passed an object that is not an\n"
	    "association list.")
#define FUNC_NAME s_scm_assq_set_x
{
  SCM handle;

  handle = scm_sloppy_assq (key, alist);
  if (scm_is_pair (handle))
    {
      SCM_SETCDR (handle, val);
      return alist;
    }
  else
    return scm_acons (key, val, alist);
}
#undef FUNC_NAME

SCM_DEFINE (scm_assv_set_x, "assv-set!", 3, 0, 0,
            (SCM alist, SCM key, SCM val),
	    "Behaves like @code{assq-set!} but uses @code{eqv?} for key comparison.")
#define FUNC_NAME s_scm_assv_set_x
{
  SCM handle;

  handle = scm_sloppy_assv (key, alist);
  if (scm_is_pair (handle))
    {
      SCM_SETCDR (handle, val);
      return alist;
    }
  else
    return scm_acons (key, val, alist);
}
#undef FUNC_NAME

SCM_DEFINE (scm_assoc_set_x, "assoc-set!", 3, 0, 0,
            (SCM alist, SCM key, SCM val),
	    "Behaves like @code{assq-set!} but uses @code{equal?} for key comparison.")
#define FUNC_NAME s_scm_assoc_set_x
{
  SCM handle;

  handle = scm_sloppy_assoc (key, alist);
  if (scm_is_pair (handle))
    {
      SCM_SETCDR (handle, val);
      return alist;
    }
  else
    return scm_acons (key, val, alist);
}
#undef FUNC_NAME




SCM_DEFINE (scm_assq_remove_x, "assq-remove!", 2, 0, 0,
            (SCM alist, SCM key),
	    "@deffnx {Scheme Procedure} assv-remove! alist key\n"
	    "@deffnx {Scheme Procedure} assoc-remove! alist key\n"
	    "Delete the first entry in @var{alist} associated with @var{key}, and return\n"
	    "the resulting alist.")
#define FUNC_NAME s_scm_assq_remove_x
{
  SCM handle;

  handle = scm_sloppy_assq (key, alist);
  if (scm_is_pair (handle))
    alist = scm_delq1_x (handle, alist);

  return alist;
}
#undef FUNC_NAME


SCM_DEFINE (scm_assv_remove_x, "assv-remove!", 2, 0, 0,
            (SCM alist, SCM key),
	    "Behaves like @code{assq-remove!} but uses @code{eqv?} for key comparison.")
#define FUNC_NAME s_scm_assv_remove_x
{
  SCM handle;

  handle = scm_sloppy_assv (key, alist);
  if (scm_is_pair (handle))
    alist = scm_delq1_x (handle, alist);

  return alist;
}
#undef FUNC_NAME


SCM_DEFINE (scm_assoc_remove_x, "assoc-remove!", 2, 0, 0,
            (SCM alist, SCM key),
	    "Behaves like @code{assq-remove!} but uses @code{equal?} for key comparison.")
#define FUNC_NAME s_scm_assoc_remove_x
{
  SCM handle;

  handle = scm_sloppy_assoc (key, alist);
  if (scm_is_pair (handle))
    alist = scm_delq1_x (handle, alist);

  return alist;
}
#undef FUNC_NAME






void
scm_init_alist ()
{
#include "libguile/alist.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
