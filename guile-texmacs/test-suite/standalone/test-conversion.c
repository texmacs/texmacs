/* Copyright (C) 1999,2000,2001,2003,2004, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
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

#if HAVE_CONFIG_H
# include <config.h>
#endif

#include <libguile.h>

#include <stdio.h>
#include <assert.h>
#include <string.h>

#ifdef HAVE_INTTYPES_H
# include <inttypes.h>
#endif

#ifndef PRIiMAX
# if (defined SIZEOF_LONG_LONG) && (SIZEOF_LONG_LONG >= 8)
#  define PRIiMAX "lli"
#  define PRIuMAX "llu"
# else
#  define PRIiMAX "li"
#  define PRIuMAX "lu"
# endif
#endif


static void
test_1 (const char *str, scm_t_intmax min, scm_t_intmax max,
	int result)
{
  int r = scm_is_signed_integer (scm_c_eval_string (str), min, max);
  if (r != result)
    {
      fprintf (stderr, "fail: scm_is_signed_integer (%s, "
	       "%" PRIiMAX ", %" PRIiMAX ") == %d\n",
	       str, min, max, result);
      exit (1);
    }
}

static void
test_is_signed_integer ()
{
  test_1 ("'foo", 
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  0);
  test_1 ("3.0", 
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  0);
  test_1 ("(inexact->exact 3.0)", 
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  1);
  test_1 ("3.5",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  0);
  test_1 ("most-positive-fixnum",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  1);
  test_1 ("(+ most-positive-fixnum 1)",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  1);
  test_1 ("most-negative-fixnum",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  1);
  test_1 ("(- most-negative-fixnum 1)",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  1);
  if (sizeof (scm_t_intmax) == 8)
    {
      test_1 ("(- (expt 2 63) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      1);
      test_1 ("(expt 2 63)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      0);
      test_1 ("(- (expt 2 63))",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      1);
      test_1 ("(- (- (expt 2 63)) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      0);
    }
  else if (sizeof (scm_t_intmax) == 4)
    {
      test_1 ("(- (expt 2 31) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      1);
      test_1 ("(expt 2 31)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      0);
      test_1 ("(- (expt 2 31))",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      1);
      test_1 ("(- (- (expt 2 31)) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      0);
    }
  else
    fprintf (stderr, "NOTE: skipped some tests.\n");

  /* bignum with range that fits into fixnum. */
  test_1 ("(+ most-positive-fixnum 1)",
	  -32768, 32767,
	  0);

  /* bignum with range that doesn't fit into fixnum, but probably
     fits into long. */
  test_1 ("(+ most-positive-fixnum 1)",
	  SCM_MOST_NEGATIVE_FIXNUM-1, SCM_MOST_POSITIVE_FIXNUM+1,
	  1);
}

static void
test_2 (const char *str, scm_t_uintmax min, scm_t_uintmax max,
	int result)
{
  int r = scm_is_unsigned_integer (scm_c_eval_string (str), min, max);
  if (r != result)
    {
      fprintf (stderr, "fail: scm_is_unsigned_integer (%s, "
	       "%" PRIuMAX ", %" PRIuMAX ") == %d\n",
	       str, min, max, result);
      exit (1);
    }
}

static void
test_is_unsigned_integer ()
{
  test_2 ("'foo", 
	  0, SCM_T_UINTMAX_MAX,
	  0);
  test_2 ("3.0", 
	  0, SCM_T_UINTMAX_MAX,
	  0);
  test_2 ("(inexact->exact 3.0)", 
	  0, SCM_T_UINTMAX_MAX,
	  1);
  test_2 ("3.5",
	  0, SCM_T_UINTMAX_MAX,
	  0);
  test_2 ("most-positive-fixnum",
	  0, SCM_T_UINTMAX_MAX,
	  1);
  test_2 ("(+ most-positive-fixnum 1)",
	  0, SCM_T_UINTMAX_MAX,
	  1);
  test_2 ("most-negative-fixnum",
	  0, SCM_T_UINTMAX_MAX,
	  0);
  test_2 ("(- most-negative-fixnum 1)",
	  0, SCM_T_UINTMAX_MAX,
	  0);
  if (sizeof (scm_t_intmax) == 8)
    {
      test_2 ("(- (expt 2 64) 1)",
	      0, SCM_T_UINTMAX_MAX,
	      1);
      test_2 ("(expt 2 64)",
	      0, SCM_T_UINTMAX_MAX,
	      0);
    }
  else if (sizeof (scm_t_intmax) == 4)
    {
      test_2 ("(- (expt 2 32) 1)",
	      0, SCM_T_UINTMAX_MAX,
	      1);
      test_2 ("(expt 2 32)",
	      0, SCM_T_UINTMAX_MAX,
	      0);
    }
  else
    fprintf (stderr, "NOTE: skipped some tests.\n");

  /* bignum with range that fits into fixnum. */
  test_2 ("(+ most-positive-fixnum 1)",
	  0, 32767,
	  0);

  /* bignum with range that doesn't fit into fixnum, but probably
     fits into long. */
  test_2 ("(+ most-positive-fixnum 1)",
	  0, SCM_MOST_POSITIVE_FIXNUM+1,
	  1);
}

typedef struct {
  SCM val;
  scm_t_intmax min, max;
  scm_t_intmax result;
} to_signed_data;

static SCM
out_of_range_handler (void *data, SCM key, SCM args)
{
  return scm_equal_p (key, scm_from_locale_symbol ("out-of-range"));
}

static SCM
wrong_type_handler (void *data, SCM key, SCM args)
{
  return scm_equal_p (key, scm_from_locale_symbol ("wrong-type-arg"));
}

static SCM
misc_error_handler (void *data, SCM key, SCM args)
{
  return scm_equal_p (key, scm_from_locale_symbol ("misc-error"));
}

static SCM
any_handler (void *data, SCM key, SCM args)
{
  return SCM_BOOL_T;
}

static SCM
to_signed_integer_body (void *data)
{
  to_signed_data *d = (to_signed_data *)data;
  d->result = scm_to_signed_integer (d->val, d->min, d->max);
  return SCM_BOOL_F;
}

static void
test_3 (const char *str, scm_t_intmax min, scm_t_intmax max,
	scm_t_intmax result, int range_error, int type_error)
{
  to_signed_data data;
  data.val = scm_c_eval_string (str);
  data.min = min;
  data.max = max;
  
  if (range_error)
    {
      if (scm_is_false (scm_internal_catch (SCM_BOOL_T,
					    to_signed_integer_body, &data,
					    out_of_range_handler, NULL)))
	{
	  fprintf (stderr,
		   "fail: scm_to_signed_int (%s, "
		   "%" PRIiMAX ", %" PRIiMAX ") -> out of range\n",
		   str, min, max);
	  exit (1);
	}
    }
  else if (type_error)
    {
      if (scm_is_false (scm_internal_catch (SCM_BOOL_T,
					    to_signed_integer_body, &data,
					    wrong_type_handler, NULL)))
	{
	  fprintf (stderr,
		   "fail: scm_to_signed_int (%s, "
		   "%" PRIiMAX", %" PRIiMAX ") -> wrong type\n",
		   str, min, max);
	  exit (1);
	}
    }
  else
    {
      if (scm_is_true (scm_internal_catch (SCM_BOOL_T,
					   to_signed_integer_body, &data,
					   any_handler, NULL))
	  || data.result != result)
	{
	  fprintf (stderr,
		   "fail: scm_to_signed_int (%s, "
		   "%" PRIiMAX ", %" PRIiMAX ") = %" PRIiMAX "\n",
		   str, min, max, result);
	  exit (1);
	}
    }
}

static void
test_to_signed_integer ()
{
  test_3 ("'foo",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  0, 0, 1);
  test_3 ("3.5",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  0, 0, 1);
  test_3 ("12",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  12, 0, 0);
  test_3 ("1000",
	  -999, 999,
	  0, 1, 0);
  test_3 ("-1000",
	  -999, 999,
	  0, 1, 0);
  test_3 ("most-positive-fixnum",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  SCM_MOST_POSITIVE_FIXNUM, 0, 0);
  test_3 ("most-negative-fixnum",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  SCM_MOST_NEGATIVE_FIXNUM, 0, 0);
  test_3 ("(+ most-positive-fixnum 1)",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  SCM_MOST_POSITIVE_FIXNUM+1, 0, 0);
  test_3 ("(- most-negative-fixnum 1)",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  SCM_MOST_NEGATIVE_FIXNUM-1, 0, 0);
  if (sizeof (scm_t_intmax) == 8)
    {
      test_3 ("(- (expt 2 63) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      SCM_T_INTMAX_MAX, 0, 0);
      test_3 ("(+ (- (expt 2 63)) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      SCM_T_INTMAX_MIN+1, 0, 0);
      test_3 ("(- (expt 2 63))",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      SCM_T_INTMAX_MIN, 0, 0);
      test_3 ("(expt 2 63)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      0, 1, 0);
      test_3 ("(- (- (expt 2 63)) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      0, 1, 0);
    }
  else if (sizeof (scm_t_intmax) == 4)
    {
      test_3 ("(- (expt 2 31) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      SCM_T_INTMAX_MAX, 0, 0);
      test_3 ("(+ (- (expt 2 31)) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      SCM_T_INTMAX_MIN+1, 0, 0);
      test_3 ("(- (expt 2 31))",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      SCM_T_INTMAX_MIN, 0, 0);
      test_3 ("(expt 2 31)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      0, 1, 0);
      test_3 ("(- (- (expt 2 31)) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      0, 1, 0);
    }
  else
    fprintf (stderr, "NOTE: skipped some tests.\n");
}

typedef struct {
  SCM val;
  scm_t_uintmax min, max;
  scm_t_uintmax result;
} to_unsigned_data;

static SCM
to_unsigned_integer_body (void *data)
{
  to_unsigned_data *d = (to_unsigned_data *)data;
  d->result = scm_to_unsigned_integer (d->val, d->min, d->max);
  return SCM_BOOL_F;
}

static void
test_4 (const char *str, scm_t_uintmax min, scm_t_uintmax max,
	scm_t_uintmax result, int range_error, int type_error)
{
  to_unsigned_data data;
  data.val = scm_c_eval_string (str);
  data.min = min;
  data.max = max;
  
  if (range_error)
    {
      if (scm_is_false (scm_internal_catch (SCM_BOOL_T,
					    to_unsigned_integer_body, &data,
					    out_of_range_handler, NULL)))
	{
	  fprintf (stderr,
		   "fail: scm_to_unsigned_int (%s, "
		   "%" PRIuMAX ", %" PRIuMAX ") -> out of range\n",
		   str, min, max);
	  exit (1);
	}
    }
  else if (type_error)
    {
      if (scm_is_false (scm_internal_catch (SCM_BOOL_T,
					    to_unsigned_integer_body, &data,
					    wrong_type_handler, NULL)))
	{
	  fprintf (stderr,
		   "fail: scm_to_unsigned_int (%s, "
		   "%" PRIuMAX ", %" PRIuMAX ") -> wrong type\n",
		   str, min, max);
	  exit (1);
	}
    }
  else
    {
      if (scm_is_true (scm_internal_catch (SCM_BOOL_T,
					   to_unsigned_integer_body, &data,
					   any_handler, NULL))
	  || data.result != result)
	{
	  fprintf (stderr,
		   "fail: scm_to_unsigned_int (%s, "
		   "%" PRIuMAX ", %" PRIuMAX ") == %" PRIuMAX "\n",
		   str, min, max, result);
	  exit (1);
	}
    }
}

static void
test_to_unsigned_integer ()
{
  test_4 ("'foo",
	  0, SCM_T_UINTMAX_MAX,
	  0, 0, 1);
  test_4 ("3.5",
	  0, SCM_T_UINTMAX_MAX,
	  0, 0, 1);
  test_4 ("12",
	  0, SCM_T_UINTMAX_MAX,
	  12, 0, 0);
  test_4 ("1000",
	  0, 999,
	  0, 1, 0);
  test_4 ("most-positive-fixnum",
	  0, SCM_T_UINTMAX_MAX,
	  SCM_MOST_POSITIVE_FIXNUM, 0, 0);
  test_4 ("(+ most-positive-fixnum 1)",
	  0, SCM_T_UINTMAX_MAX,
	  SCM_MOST_POSITIVE_FIXNUM+1, 0, 0);
  if (sizeof (scm_t_intmax) == 8)
    {
      test_4 ("(- (expt 2 64) 1)",
	      0, SCM_T_UINTMAX_MAX,
	      SCM_T_UINTMAX_MAX, 0, 0);
      test_4 ("(expt 2 64)",
	      0, SCM_T_UINTMAX_MAX,
	      0, 1, 0);
    }
  else if (sizeof (scm_t_intmax) == 4)
    {
      test_4 ("(- (expt 2 32) 1)",
	      0, SCM_T_UINTMAX_MAX,
	      SCM_T_UINTMAX_MAX, 0, 0);
      test_4 ("(expt 2 32)",
	      0, SCM_T_UINTMAX_MAX,
	      0, 1, 0);
    }
  else
    fprintf (stderr, "NOTE: skipped some tests.\n");
}

static void
test_5 (scm_t_intmax val, const char *result)
{
  SCM res = scm_c_eval_string (result);
  if (scm_is_false (scm_equal_p (scm_from_signed_integer (val), res)))
    {
      fprintf (stderr, "fail: scm_from_signed_integer (%" PRIiMAX ") == %s\n",
	       val, result);
      exit (1);
    }
}

static void
test_from_signed_integer ()
{
  test_5 (12, "12");
  if (sizeof (scm_t_intmax) == 8)
    {
      test_5 (SCM_T_INTMAX_MAX, "(- (expt 2 63) 1)");
      test_5 (SCM_T_INTMAX_MIN, "(- (expt 2 63))");
    }
  else if (sizeof (scm_t_intmax) == 4)
    {
      test_5 (SCM_T_INTMAX_MAX, "(- (expt 2 31) 1)");
      test_5 (SCM_T_INTMAX_MIN, "(- (expt 2 31))");
    }
  test_5 (SCM_MOST_POSITIVE_FIXNUM, "most-positive-fixnum");
  test_5 (SCM_MOST_NEGATIVE_FIXNUM, "most-negative-fixnum");
  test_5 (SCM_MOST_POSITIVE_FIXNUM+1, "(+ most-positive-fixnum 1)");
  test_5 (SCM_MOST_NEGATIVE_FIXNUM-1, "(- most-negative-fixnum 1)");
}

static void
test_6 (scm_t_uintmax val, const char *result)
{
  SCM res = scm_c_eval_string (result);
  if (scm_is_false (scm_equal_p (scm_from_unsigned_integer (val), res)))
    {
      fprintf (stderr, "fail: scm_from_unsigned_integer (%"
	       PRIuMAX ") == %s\n",
	       val, result);
      exit (1);
    }
}

static void
test_from_unsigned_integer ()
{
  test_6 (12, "12");
  if (sizeof (scm_t_intmax) == 8)
    {
      test_6 (SCM_T_UINTMAX_MAX, "(- (expt 2 64) 1)");
    }
  else if (sizeof (scm_t_intmax) == 4)
    {
      test_6 (SCM_T_UINTMAX_MAX, "(- (expt 2 32) 1)");
    }
  test_6 (SCM_MOST_POSITIVE_FIXNUM, "most-positive-fixnum");
  test_6 (SCM_MOST_POSITIVE_FIXNUM+1, "(+ most-positive-fixnum 1)");
}

static void
test_7s (SCM n, scm_t_intmax c_n, const char *result, const char *func)
{
  SCM r = scm_c_eval_string (result);

  if (scm_is_false (scm_equal_p (n, r)))
    {
      fprintf (stderr, "fail: %s (%" PRIiMAX ") == %s\n", func, c_n, result);
      exit (1);
    }
}

#define TEST_7S(func,arg,res) test_7s (func(arg), arg, res, #func)

static void
test_7u (SCM n, scm_t_uintmax c_n, const char *result, const char *func)
{
  SCM r = scm_c_eval_string (result);

  if (scm_is_false (scm_equal_p (n, r)))
    {
      fprintf (stderr, "fail: %s (%" PRIuMAX ") == %s\n", func, c_n, result);
      exit (1);
    }
}

#define TEST_7U(func,arg,res) test_7u (func(arg), arg, res, #func)

typedef struct {
  SCM val;
  scm_t_intmax (*func) (SCM);
  scm_t_intmax result;
} to_signed_func_data;

static SCM
to_signed_func_body (void *data)
{
  to_signed_func_data *d = (to_signed_func_data *)data;
  d->result = d->func (d->val);
  return SCM_BOOL_F;
}

static void
test_8s (const char *str, scm_t_intmax (*func) (SCM), const char *func_name,
	 scm_t_intmax result, int range_error, int type_error)
{
  to_signed_func_data data;
  data.val = scm_c_eval_string (str);
  data.func = func;
  
  if (range_error)
    {
      if (scm_is_false (scm_internal_catch (SCM_BOOL_T,
					    to_signed_func_body, &data,
					    out_of_range_handler, NULL)))
	{
	  fprintf (stderr,
		   "fail: %s (%s) -> out of range\n", func_name, str);
	  exit (1);
	}
    }
  else if (type_error)
    {
      if (scm_is_false (scm_internal_catch (SCM_BOOL_T,
					    to_signed_func_body, &data,
					    wrong_type_handler, NULL)))
	{
	  fprintf (stderr,
		   "fail: %s (%s) -> wrong type\n", func_name, str);
	  exit (1);
	}
    }
  else
    {
      if (scm_is_true (scm_internal_catch (SCM_BOOL_T,
					   to_signed_func_body, &data,
					   any_handler, NULL))
	  || data.result != result)
	{
	  fprintf (stderr,
		   "fail: %s (%s) = %" PRIiMAX "\n", func_name, str, result);
	  exit (1);
	}
    }
}

typedef struct {
  SCM val;
  scm_t_uintmax (*func) (SCM);
  scm_t_uintmax result;
} to_unsigned_func_data;

static SCM
to_unsigned_func_body (void *data)
{
  to_unsigned_func_data *d = (to_unsigned_func_data *)data;
  d->result = d->func (d->val);
  return SCM_BOOL_F;
}

static void
test_8u (const char *str, scm_t_uintmax (*func) (SCM), const char *func_name,
	 scm_t_uintmax result, int range_error, int type_error)
{
  to_unsigned_func_data data;
  data.val = scm_c_eval_string (str);
  data.func = func;
  
  if (range_error)
    {
      if (scm_is_false (scm_internal_catch (SCM_BOOL_T,
					    to_unsigned_func_body, &data,
					    out_of_range_handler, NULL)))
	{
	  fprintf (stderr,
		   "fail: %s (%s) -> out of range\n", func_name, str);
	  exit (1);
	}
    }
  else if (type_error)
    {
      if (scm_is_false (scm_internal_catch (SCM_BOOL_T,
					    to_unsigned_func_body, &data,
					    wrong_type_handler, NULL)))
	{
	  fprintf (stderr,
		   "fail: %s (%s) -> wrong type\n", func_name, str);
	  exit (1);
	}
    }
  else
    {
      if (scm_is_true (scm_internal_catch (SCM_BOOL_T,
					   to_unsigned_func_body, &data,
					   any_handler, NULL))
	  || data.result != result)
	{
	  fprintf (stderr,
		   "fail: %s (%s) = %" PRIiMAX "\n", func_name, str, result);
	  exit (1);
	}
    }
}

/* We can't rely on the scm_to functions being proper functions but we
   want to pass them to test_8s and test_8u, so we wrap'em.  Also, we
   need to give them a common return type.
*/

#define DEFSTST(f) static scm_t_intmax  tst_##f (SCM x) { return f(x); }
#define DEFUTST(f) static scm_t_uintmax tst_##f (SCM x) { return f(x); }

DEFSTST (scm_to_schar)
DEFUTST (scm_to_uchar)
DEFSTST (scm_to_char)
DEFSTST (scm_to_short)
DEFUTST (scm_to_ushort)
DEFSTST (scm_to_int)
DEFUTST (scm_to_uint)
DEFSTST (scm_to_long)
DEFUTST (scm_to_ulong)
#if SCM_SIZEOF_LONG_LONG != 0
DEFSTST (scm_to_long_long)
DEFUTST (scm_to_ulong_long)
#endif
DEFSTST (scm_to_ssize_t)
DEFUTST (scm_to_size_t)

DEFSTST (scm_to_int8)
DEFUTST (scm_to_uint8)
DEFSTST (scm_to_int16)
DEFUTST (scm_to_uint16)
DEFSTST (scm_to_int32)
DEFUTST (scm_to_uint32)
DEFSTST (scm_to_int64)
DEFUTST (scm_to_uint64)

#define TEST_8S(v,f,r,re,te) test_8s (v, tst_##f, #f, r, re, te)
#define TEST_8U(v,f,r,re,te) test_8u (v, tst_##f, #f, r, re, te)


static void
test_int_sizes ()
{
  TEST_7U (scm_from_uchar,  91, "91");
  TEST_7S (scm_from_schar,  91, "91");
  TEST_7S (scm_from_char,   91, "91");
  TEST_7S (scm_from_short, -911, "-911");
  TEST_7U (scm_from_ushort, 911, "911");
  TEST_7S (scm_from_int,    911, "911");
  TEST_7U (scm_from_uint,   911, "911");
  TEST_7S (scm_from_long,   911, "911");
  TEST_7U (scm_from_ulong,  911, "911");
#if SCM_SIZEOF_LONG_LONG != 0
  TEST_7S (scm_from_long_long,   911, "911");
  TEST_7U (scm_from_ulong_long,  911, "911");
#endif
  TEST_7U (scm_from_size_t,  911, "911");
  TEST_7S (scm_from_ssize_t, 911, "911");

  TEST_7S (scm_from_int8, -128, "-128");
  TEST_7S (scm_from_int8,  127,  "127");
  TEST_7S (scm_from_int8,  128, "-128");
  TEST_7U (scm_from_uint8, 255,  "255");

  TEST_7S (scm_from_int16, -32768, "-32768");
  TEST_7S (scm_from_int16,  32767,  "32767");
  TEST_7S (scm_from_int16,  32768, "-32768");
  TEST_7U (scm_from_uint16, 65535,  "65535");

  TEST_7S (scm_from_int32,  SCM_T_INT32_MIN,     "-2147483648");
  TEST_7S (scm_from_int32,  SCM_T_INT32_MAX,      "2147483647");
  TEST_7S (scm_from_int32,  SCM_T_INT32_MAX+1LL, "-2147483648");
  TEST_7U (scm_from_uint32, SCM_T_UINT32_MAX,     "4294967295");

  TEST_7S (scm_from_int64,  SCM_T_INT64_MIN,  "-9223372036854775808");
  TEST_7S (scm_from_int64,  SCM_T_INT64_MAX,   "9223372036854775807");
  TEST_7U (scm_from_uint64, SCM_T_UINT64_MAX, "18446744073709551615");

  TEST_8S ("91",   scm_to_schar,   91, 0, 0);
  TEST_8U ("91",   scm_to_uchar,   91, 0, 0);
  TEST_8S ("91",   scm_to_char,    91, 0, 0);
  TEST_8S ("-911", scm_to_short, -911, 0, 0);
  TEST_8U ("911",  scm_to_ushort, 911, 0, 0);
  TEST_8S ("-911", scm_to_int,   -911, 0, 0);
  TEST_8U ("911",  scm_to_uint,   911, 0, 0);
  TEST_8S ("-911", scm_to_long,  -911, 0, 0);
  TEST_8U ("911",  scm_to_ulong,  911, 0, 0);
#if SCM_SIZEOF_LONG_LONG != 0
  TEST_8S ("-911", scm_to_long_long, -911, 0, 0);
  TEST_8U ("911",  scm_to_ulong_long, 911, 0, 0);
#endif
  TEST_8U ("911",  scm_to_size_t,   911, 0, 0);
  TEST_8S ("911",  scm_to_ssize_t,  911, 0, 0);

  TEST_8S ("-128", scm_to_int8,   SCM_T_INT8_MIN, 0, 0);
  TEST_8S ("127",  scm_to_int8,   SCM_T_INT8_MAX, 0, 0);
  TEST_8S ("128",  scm_to_int8,                0, 1, 0);
  TEST_8S ("#f",   scm_to_int8,                0, 0, 1);
  TEST_8U ("255",  scm_to_uint8, SCM_T_UINT8_MAX, 0, 0);
  TEST_8U ("256",  scm_to_uint8,               0, 1, 0);
  TEST_8U ("-1",   scm_to_uint8,               0, 1, 0);
  TEST_8U ("#f",   scm_to_uint8,               0, 0, 1);

  TEST_8S ("-32768", scm_to_int16,   SCM_T_INT16_MIN, 0, 0);
  TEST_8S ("32767",  scm_to_int16,   SCM_T_INT16_MAX, 0, 0);
  TEST_8S ("32768",  scm_to_int16,                 0, 1, 0);
  TEST_8S ("#f",     scm_to_int16,                 0, 0, 1);
  TEST_8U ("65535",  scm_to_uint16, SCM_T_UINT16_MAX, 0, 0);
  TEST_8U ("65536",  scm_to_uint16,                0, 1, 0);
  TEST_8U ("-1",     scm_to_uint16,                0, 1, 0);
  TEST_8U ("#f",     scm_to_uint16,                0, 0, 1);

  TEST_8S ("-2147483648", scm_to_int32,   SCM_T_INT32_MIN, 0, 0);
  TEST_8S ("2147483647",  scm_to_int32,   SCM_T_INT32_MAX, 0, 0);
  TEST_8S ("2147483648",  scm_to_int32,                 0, 1, 0);
  TEST_8S ("#f",          scm_to_int32,                 0, 0, 1);
  TEST_8U ("4294967295",  scm_to_uint32, SCM_T_UINT32_MAX, 0, 0);
  TEST_8U ("4294967296",  scm_to_uint32,                0, 1, 0);
  TEST_8U ("-1",          scm_to_uint32,                0, 1, 0);
  TEST_8U ("#f",          scm_to_uint32,                0, 0, 1);

  TEST_8S ("-9223372036854775808", scm_to_int64,   SCM_T_INT64_MIN, 0, 0);
  TEST_8S ("9223372036854775807",  scm_to_int64,   SCM_T_INT64_MAX, 0, 0);
  TEST_8S ("9223372036854775808",  scm_to_int64,                 0, 1, 0);
  TEST_8S ("#f",                   scm_to_int64,                 0, 0, 1);
  TEST_8U ("18446744073709551615", scm_to_uint64, SCM_T_UINT64_MAX, 0, 0);
  TEST_8U ("18446744073709551616", scm_to_uint64,                0, 1, 0);
  TEST_8U ("-1",                   scm_to_uint64,                0, 1, 0);
  TEST_8U ("#f",                   scm_to_uint64,                0, 0, 1);

}

static void
test_9 (double val, const char *result)
{
  SCM res = scm_c_eval_string (result);
  if (scm_is_false (scm_eqv_p (res, scm_from_double (val))))
    {
      fprintf (stderr, "fail: scm_from_double (%g) == %s\n", val, result);
      exit (1);
    }
}

/* The `infinity' and `not-a-number' values.  */
static double guile_Inf, guile_NaN;

/* Initialize GUILE_INF and GUILE_NAN.  Taken from `guile_ieee_init ()' in
   `libguile/numbers.c'.  */
static void
ieee_init (void)
{
#ifdef INFINITY
  /* C99 INFINITY, when available.
     FIXME: The standard allows for INFINITY to be something that overflows
     at compile time.  We ought to have a configure test to check for that
     before trying to use it.  (But in practice we believe this is not a
     problem on any system guile is likely to target.)  */
  guile_Inf = INFINITY;
#elif HAVE_DINFINITY
  /* OSF */
  extern unsigned int DINFINITY[2];
  guile_Inf = (*((double *) (DINFINITY)));
#else
  double tmp = 1e+10;
  guile_Inf = tmp;
  for (;;)
    {
      guile_Inf *= 1e+10;
      if (guile_Inf == tmp)
	break;
      tmp = guile_Inf;
    }
#endif

#ifdef NAN
  /* C99 NAN, when available */
  guile_NaN = NAN;
#elif HAVE_DQNAN
  {
    /* OSF */
    extern unsigned int DQNAN[2];
    guile_NaN = (*((double *)(DQNAN)));
  }
#else
  guile_NaN = guile_Inf / guile_Inf;
#endif
}

static void
test_from_double ()
{
  test_9 (12, "12.0");
  test_9 (0.25, "0.25");
  test_9 (0.1, "0.1");
  test_9 (guile_Inf, "+inf.0");
  test_9 (-guile_Inf, "-inf.0");
  test_9 (guile_NaN, "+nan.0");
}

typedef struct {
  SCM val;
  double result;
} to_double_data;

static SCM
to_double_body (void *data)
{
  to_double_data *d = (to_double_data *)data;
  d->result = scm_to_double (d->val);
  return SCM_BOOL_F;
}

static void
test_10 (const char *val, double result, int type_error)
{
  to_double_data data;
  data.val = scm_c_eval_string (val);
  
  if (type_error)
    {
      if (scm_is_false (scm_internal_catch (SCM_BOOL_T,
					    to_double_body, &data,
					    wrong_type_handler, NULL)))
	{
	  fprintf (stderr,
		   "fail: scm_double (%s) -> wrong type\n", val);
	  exit (1);
	}
    }
  else
    {
      if (scm_is_true (scm_internal_catch (SCM_BOOL_T,
					   to_double_body, &data,
					   any_handler, NULL))
	  || data.result != result)
	{
	  fprintf (stderr,
		   "fail: scm_to_double (%s) = %g\n", val, result);
	  exit (1);
	}
    }
}

static void
test_to_double ()
{
  test_10 ("#f",          0.0,  1);
  test_10 ("12",         12.0,  0);
  test_10 ("0.25",       0.25,  0);
  test_10 ("1/4",        0.25,  0);
  test_10 ("+inf.0", guile_Inf, 0);
  test_10 ("-inf.0",-guile_Inf, 0);
  test_10 ("+1i",         0.0,  1);
}

typedef struct {
  SCM val;
  char *result;
} to_locale_string_data;

static SCM
to_locale_string_body (void *data)
{
  to_locale_string_data *d = (to_locale_string_data *)data;
  d->result = scm_to_locale_string (d->val);
  return SCM_BOOL_F;
}

static void
test_11 (const char *str, const char *result, int misc_error, int type_error)
{
  to_locale_string_data data;
  data.val = scm_c_eval_string (str);
  data.result = NULL;

  if (misc_error)
    {
      if (scm_is_false (scm_internal_catch (SCM_BOOL_T,
					    to_locale_string_body, &data,
					    misc_error_handler, NULL)))
	{
	  fprintf (stderr,
		   "fail: scm_to_locale_string (%s) -> misc error\n", str);
	  exit (1);
	}
    }
  else if (type_error)
    {
      if (scm_is_false (scm_internal_catch (SCM_BOOL_T,
					    to_locale_string_body, &data,
					    wrong_type_handler, NULL)))
	{
	  fprintf (stderr,
		   "fail: scm_to_locale_string (%s) -> wrong type\n", str);
	  exit (1);
	}
    }
  else
    {
      if (scm_is_true (scm_internal_catch (SCM_BOOL_T,
					   to_locale_string_body, &data,
					   any_handler, NULL))
	  || data.result == NULL || strcmp (data.result, result))
	{
	  fprintf (stderr,
		   "fail: scm_to_locale_string (%s) = %s\n", str, result);
	  exit (1);
	}
    }

  free (data.result);
}

static void
test_locale_strings ()
{
  const char *lstr = "This is not a string.";
  char *lstr2;
  SCM str, str2;
  char buf[20];
  size_t len;

  if (!scm_is_string (scm_c_eval_string ("\"foo\"")))
    {
      fprintf (stderr, "fail: scm_is_string (\"foo\") = true\n");
      exit (1);
    }

  str = scm_from_locale_string (lstr);

  if (!scm_is_string (str))
    {
      fprintf (stderr, "fail: scm_is_string (str) = true\n");
      exit (1);
    }

  lstr2 = scm_to_locale_string (str);
  if (strcmp (lstr, lstr2))
    {
      fprintf (stderr, "fail: lstr = lstr2\n");
      exit (1);
    }
  free (lstr2);

  buf[15] = 'x';
  len = scm_to_locale_stringbuf (str, buf, 15);
  if (len != strlen (lstr))
    {
      fprintf (stderr, "fail: scm_to_locale_stringbuf (...) = strlen(lstr)\n");
      exit (1);
    }
  if (buf[15] != 'x')
    {
      fprintf (stderr, "fail: scm_to_locale_stringbuf (...) no overrun\n");
      exit (1);
    }
  if (strncmp (lstr, buf, 15))
    {
      fprintf (stderr, "fail: scm_to_locale_stringbuf (...) = lstr\n");
      exit (1);
    }

  str2 = scm_from_locale_stringn (lstr, 10);

  if (!scm_is_string (str2))
    {
      fprintf (stderr, "fail: scm_is_string (str2) = true\n");
      exit (1);
    }

  lstr2 = scm_to_locale_string (str2);
  if (strncmp (lstr, lstr2, 10))
    {
      fprintf (stderr, "fail: lstr = lstr2\n");
      exit (1);
    }
  free (lstr2);

  buf[10] = 'x';
  len = scm_to_locale_stringbuf (str2, buf, 20);
  if (len != 10)
    {
      fprintf (stderr, "fail: scm_to_locale_stringbuf (...) = 10\n");
      exit (1);
    }
  if (buf[10] != 'x')
    {
      fprintf (stderr, "fail: scm_to_locale_stringbuf (...) no overrun\n");
      exit (1);
    }
  if (strncmp (lstr, buf, 10))
    {
      fprintf (stderr, "fail: scm_to_locale_stringbuf (...) = lstr\n");
      exit (1);
    }

  lstr2 = scm_to_locale_stringn (str2, &len);
  if (len != 10)
    {
      fprintf (stderr, "fail: scm_to_locale_stringn, len = 10\n");
      exit (1);
    }

  test_11 ("#f", NULL, 0, 1);
  test_11 ("\"foo\"", "foo", 0, 0);
  test_11 ("(string #\\f #\\nul)", NULL, 1, 0);
}

static void
tests (void *data, int argc, char **argv)
{
  test_is_signed_integer ();
  test_is_unsigned_integer ();
  test_to_signed_integer ();
  test_to_unsigned_integer ();
  test_from_signed_integer ();
  test_from_unsigned_integer ();
  test_int_sizes ();
  test_from_double ();
  test_to_double ();
  test_locale_strings ();
}

int
main (int argc, char *argv[])
{
  ieee_init ();
  scm_boot_guile (argc, argv, tests, NULL);
  return 0;
}
