/* Copyright (C) 1995,1996,1997,1998,2000,2001,2003, 2004, 2006 Free Software Foundation, Inc.
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
#  include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/ramap.h"
#include "libguile/stackchk.h"
#include "libguile/strorder.h"
#include "libguile/async.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/unif.h"
#include "libguile/vectors.h"

#include "libguile/struct.h"
#include "libguile/goops.h"
#include "libguile/objects.h"

#include "libguile/validate.h"
#include "libguile/eq.h"


#ifdef HAVE_STRING_H
#include <string.h>
#endif


SCM_DEFINE1 (scm_eq_p, "eq?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	    "Return @code{#t} if @var{x} and @var{y} are the same object,\n"
	    "except for numbers and characters.  For example,\n"
	    "\n"
	    "@example\n"
	    "(define x (vector 1 2 3))\n"
	    "(define y (vector 1 2 3))\n"
	    "\n"
	    "(eq? x x)  @result{} #t\n"
	    "(eq? x y)  @result{} #f\n"
	    "@end example\n"
	    "\n"
	    "Numbers and characters are not equal to any other object, but\n"
	    "the problem is they're not necessarily @code{eq?} to themselves\n"
	    "either.  This is even so when the number comes directly from a\n"
	    "variable,\n"
	    "\n"
	    "@example\n"
	    "(let ((n (+ 2 3)))\n"
	    "  (eq? n n))       @result{} *unspecified*\n"
	    "@end example\n"
	    "\n"
	    "Generally @code{eqv?} should be used when comparing numbers or\n"
	    "characters.  @code{=} or @code{char=?} can be used too.\n"
	    "\n"
	    "It's worth noting that end-of-list @code{()}, @code{#t},\n"
	    "@code{#f}, a symbol of a given name, and a keyword of a given\n"
	    "name, are unique objects.  There's just one of each, so for\n"
	    "instance no matter how @code{()} arises in a program, it's the\n"
	    "same object and can be compared with @code{eq?},\n"
	    "\n"
	    "@example\n"
	    "(define x (cdr '(123)))\n"
	    "(define y (cdr '(456)))\n"
	    "(eq? x y) @result{} #t\n"
	    "\n"
	    "(define x (string->symbol \"foo\"))\n"
	    "(eq? x 'foo) @result{} #t\n"
	    "@end example")
#define FUNC_NAME s_scm_eq_p
{
  return scm_from_bool (scm_is_eq (x, y));
}
#undef FUNC_NAME

/* We compare doubles in a special way for 'eqv?' to be able to
   distinguish plus and minus zero and to identify NaNs.
*/

static int
real_eqv (double x, double y)
{
  return !memcmp (&x, &y, sizeof(double)) || (x != x && y != y);
}

#include <stdio.h>
SCM_PRIMITIVE_GENERIC_1 (scm_eqv_p, "eqv?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	    "Return @code{#t} if @var{x} and @var{y} are the same object, or\n"
	    "for characters and numbers the same value.\n"
	    "\n"
	    "On objects except characters and numbers, @code{eqv?} is the\n"
	    "same as @code{eq?}, it's true if @var{x} and @var{y} are the\n"
	    "same object.\n"
	    "\n"
	    "If @var{x} and @var{y} are numbers or characters, @code{eqv?}\n"
	    "compares their type and value.  An exact number is not\n"
	    "@code{eqv?} to an inexact number (even if their value is the\n"
	    "same).\n"
	    "\n"
	    "@example\n"
	    "(eqv? 3 (+ 1 2)) @result{} #t\n"
	    "(eqv? 1 1.0)     @result{} #f\n"
	    "@end example")
#define FUNC_NAME s_scm_eqv_p
{
  if (scm_is_eq (x, y))
    return SCM_BOOL_T;
  if (SCM_IMP (x))
    return SCM_BOOL_F;
  if (SCM_IMP (y))
    return SCM_BOOL_F;
  /* this ensures that types and scm_length are the same. */

  if (SCM_CELL_TYPE (x) != SCM_CELL_TYPE (y))
    {
      /* fractions use 0x10000 as a flag (at the suggestion of Marius Vollmer),
	 but this checks the entire type word, so fractions may be accidentally
	 flagged here as unequal.  Perhaps I should use the 4th double_cell word?
      */

      /* treat mixes of real and complex types specially */
      if (SCM_INEXACTP (x))
	{
	  if (SCM_REALP (x))
	    return scm_from_bool (SCM_COMPLEXP (y)
			     && real_eqv (SCM_REAL_VALUE (x),
					  SCM_COMPLEX_REAL (y))
			     && SCM_COMPLEX_IMAG (y) == 0.0);
	  else
	    return scm_from_bool (SCM_REALP (y)
			     && real_eqv (SCM_COMPLEX_REAL (x),
					  SCM_REAL_VALUE (y))
			     && SCM_COMPLEX_IMAG (x) == 0.0);
	}

      if (SCM_FRACTIONP (x) && SCM_FRACTIONP (y))
	return scm_i_fraction_equalp (x, y);
      return SCM_BOOL_F;
    }
  if (SCM_NUMP (x))
    {
      if (SCM_BIGP (x)) {
	return scm_from_bool (scm_i_bigcmp (x, y) == 0);
      } else if (SCM_REALP (x)) {
	return scm_from_bool (real_eqv (SCM_REAL_VALUE (x), SCM_REAL_VALUE (y)));
      } else if (SCM_FRACTIONP (x)) {
	return scm_i_fraction_equalp (x, y);
      } else { /* complex */
	return scm_from_bool (real_eqv (SCM_COMPLEX_REAL (x),
				   SCM_COMPLEX_REAL (y)) 
			 && real_eqv (SCM_COMPLEX_IMAG (x),
				      SCM_COMPLEX_IMAG (y)));
      }
    }
  if (SCM_UNPACK (g_scm_eqv_p))
    return scm_call_generic_2 (g_scm_eqv_p, x, y);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_PRIMITIVE_GENERIC_1 (scm_equal_p, "equal?", scm_tc7_rpsubr,
			 (SCM x, SCM y),
	    "Return @code{#t} if @var{x} and @var{y} are the same type, and\n"
	    "their contents or value are equal.\n"
	    "\n"
	    "For a pair, string, vector or array, @code{equal?} compares the\n"
	    "contents, and does so using using the same @code{equal?}\n"
	    "recursively, so a deep structure can be traversed.\n"
	    "\n"
	    "@example\n"
	    "(equal? (list 1 2 3) (list 1 2 3))   @result{} #t\n"
	    "(equal? (list 1 2 3) (vector 1 2 3)) @result{} #f\n"
	    "@end example\n"
	    "\n"
	    "For other objects, @code{equal?} compares as per @code{eqv?},\n"
	    "which means characters and numbers are compared by type and\n"
	    "value (and like @code{eqv?}, exact and inexact numbers are not\n"
	    "@code{equal?}, even if their value is the same).\n"
	    "\n"
	    "@example\n"
	    "(equal? 3 (+ 1 2)) @result{} #t\n"
	    "(equal? 1 1.0)     @result{} #f\n"
	    "@end example\n"
	    "\n"
	    "Hash tables are currently only compared as per @code{eq?}, so\n"
	    "two different tables are not @code{equal?}, even if their\n"
	    "contents are the same.\n"
	    "\n"
	    "@code{equal?} does not support circular data structures, it may\n"
	    "go into an infinite loop if asked to compare two circular lists\n"
	    "or similar.\n"
	    "\n"
	    "New application-defined object types (Smobs) have an\n"
	    "@code{equalp} handler which is called by @code{equal?}.  This\n"
	    "lets an application traverse the contents or control what is\n"
	    "considered @code{equal?} for two such objects.  If there's no\n"
	    "handler, the default is to just compare as per @code{eq?}.")
#define FUNC_NAME s_scm_equal_p
{
  SCM_CHECK_STACK;
 tailrecurse:
  SCM_TICK;
  if (scm_is_eq (x, y))
    return SCM_BOOL_T;
  if (SCM_IMP (x))
    return SCM_BOOL_F;
  if (SCM_IMP (y))
    return SCM_BOOL_F;
  if (scm_is_pair (x) && scm_is_pair (y))
    {
      if (scm_is_false (scm_equal_p (SCM_CAR (x), SCM_CAR (y))))
	return SCM_BOOL_F;
      x = SCM_CDR(x);
      y = SCM_CDR(y);
      goto tailrecurse;
    }
  if (SCM_TYP7 (x) == scm_tc7_string && SCM_TYP7 (y) == scm_tc7_string)
    return scm_string_equal_p (x, y);
  if (SCM_TYP7 (x) == scm_tc7_smob && SCM_TYP16 (x) == SCM_TYP16 (y))
    {
      int i = SCM_SMOBNUM (x);
      if (!(i < scm_numsmob))
	return SCM_BOOL_F;
      if (scm_smobs[i].equalp)
	return (scm_smobs[i].equalp) (x, y);
      else
	goto generic_equal;
    }
  /* This ensures that types and scm_length are the same.  */
  if (SCM_CELL_TYPE (x) != SCM_CELL_TYPE (y))
    {
      /* treat mixes of real and complex types specially */
      if (SCM_INEXACTP (x) && SCM_INEXACTP (y))
	{
	  if (SCM_REALP (x))
	    return scm_from_bool (SCM_COMPLEXP (y)
			     && SCM_REAL_VALUE (x) == SCM_COMPLEX_REAL (y)
			     && SCM_COMPLEX_IMAG (y) == 0.0);
	  else
	    return scm_from_bool (SCM_REALP (y)
			     && SCM_COMPLEX_REAL (x) == SCM_REAL_VALUE (y)
			     && SCM_COMPLEX_IMAG (x) == 0.0);
	}

      /* Vectors can be equal to one-dimensional arrays.
       */
      if (SCM_I_ARRAYP (x) || SCM_I_ARRAYP (y))
	return scm_array_equal_p (x, y);

      return SCM_BOOL_F;
    }
  switch (SCM_TYP7 (x))
    {
    default:
      break;
    case scm_tc7_number:
      switch SCM_TYP16 (x)
        {
        case scm_tc16_big:
          return scm_bigequal (x, y);
        case scm_tc16_real:
          return scm_real_equalp (x, y);
        case scm_tc16_complex:
          return scm_complex_equalp (x, y);
	case scm_tc16_fraction:
          return scm_i_fraction_equalp (x, y);
        }
    case scm_tc7_vector:
    case scm_tc7_wvect:
      return scm_i_vector_equal_p (x, y);
    }

  /* Check equality between structs of equal type (see cell-type test above)
     that are not GOOPS instances.  GOOPS instances are treated via the
     generic function.  */
  if ((SCM_STRUCTP (x)) && (!SCM_INSTANCEP (x)))
    return scm_i_struct_equalp (x, y);

 generic_equal:
  if (SCM_UNPACK (g_scm_equal_p))
    return scm_call_generic_2 (g_scm_equal_p, x, y);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME






void
scm_init_eq ()
{
#include "libguile/eq.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
