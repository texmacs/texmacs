/* Copyright (C) 1999,2000,2001,2003,2004, 2006, 2008 Free Software Foundation, Inc.
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

#ifndef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libguile.h>

#include <stdio.h>
#include <assert.h>

#if SCM_ENABLE_DISCOURAGED == 1

SCM out_of_range_handler (void *data, SCM key, SCM args);
SCM call_num2long_long_body (void *data);
SCM call_num2ulong_long_body (void *data);

/* expect to catch an `out-of-range' exception */
SCM
out_of_range_handler (void *data, SCM key, SCM args)
{
  assert (scm_equal_p (key, scm_str2symbol ("out-of-range")));
  return SCM_BOOL_T;
}

SCM
call_num2long_long_body (void *data)
{
  scm_num2long_long (* (SCM *) data, SCM_ARG1, "call_num2long_long_body");
  return SCM_BOOL_F;
}

SCM
call_num2ulong_long_body (void *data)
{
  scm_num2ulong_long (* (SCM *) data, SCM_ARG1, "call_num2ulong_long_body");
  return SCM_BOOL_F;
}

static void
test_long_long ()
{
#if SCM_SIZEOF_LONG_LONG != 0
  {
    SCM n = scm_long_long2num (SCM_I_LLONG_MIN);
    long long result = scm_num2long_long(n, 0, "main");
    assert (result == SCM_I_LLONG_MIN);
  }

  /* LLONG_MIN - 1 */
  {
    SCM n = scm_difference (scm_long_long2num (SCM_I_LLONG_MIN), scm_from_int (1));
    SCM caught = scm_internal_catch (SCM_BOOL_T, call_num2long_long_body, &n,
                                     out_of_range_handler, NULL);
    assert (scm_is_true (caught));
  }

  /* SCM_I_LLONG_MIN + SCM_I_LLONG_MIN/2 */
  {
    SCM n = scm_sum (scm_long_long2num (SCM_I_LLONG_MIN),
                     scm_long_long2num (SCM_I_LLONG_MIN / 2));
    SCM caught = scm_internal_catch (SCM_BOOL_T, call_num2long_long_body, &n,
                                     out_of_range_handler, NULL);
    assert (scm_is_true (caught));
  }

  /* SCM_I_LLONG_MAX + 1 */
  {
    SCM n = scm_sum (scm_long_long2num (SCM_I_LLONG_MAX), scm_from_int (1));
    SCM caught = scm_internal_catch (SCM_BOOL_T, call_num2long_long_body, &n,
                                     out_of_range_handler, NULL);
    assert (scm_is_true (caught));
  }

  /* 2^1024 */
  {
    SCM n = scm_ash (scm_from_int (1), scm_from_int (1024));
    SCM caught = scm_internal_catch (SCM_BOOL_T, call_num2long_long_body, &n,
                                     out_of_range_handler, NULL);
    assert (scm_is_true (caught));
  }

  /* -2^1024 */
  {
    SCM n = scm_difference (scm_from_int (0),
                            scm_ash (scm_from_int (1), scm_from_int (1024)));
    SCM caught = scm_internal_catch (SCM_BOOL_T, call_num2long_long_body, &n,
                                     out_of_range_handler, NULL);
    assert (scm_is_true (caught));
  }

#endif /* SCM_SIZEOF_LONG_LONG != 0 */
}

static void
test_ulong_long ()
{
#if SCM_SIZEOF_LONG_LONG != 0

  {
    SCM n = scm_ulong_long2num (SCM_I_ULLONG_MAX);
    unsigned long long result = scm_num2ulong_long(n, 0, "main");
    assert (result == SCM_I_ULLONG_MAX);
  }

  /* -1 */
  {
    SCM n = scm_from_int (-1);
    SCM caught = scm_internal_catch (SCM_BOOL_T, call_num2ulong_long_body, &n,
                                     out_of_range_handler, NULL);
    assert (scm_is_true (caught));
  }

  /* SCM_I_ULLONG_MAX + 1 */
  {
    SCM n = scm_sum (scm_ulong_long2num (SCM_I_ULLONG_MAX), scm_from_int (1));
    SCM caught = scm_internal_catch (SCM_BOOL_T, call_num2ulong_long_body, &n,
                                     out_of_range_handler, NULL);
    assert (scm_is_true (caught));
  }

  /* 2^1024 */
  {
    SCM n = scm_ash (scm_from_int (1), scm_from_int (1024));
    SCM caught = scm_internal_catch (SCM_BOOL_T, call_num2long_long_body, &n,
                                     out_of_range_handler, NULL);
    assert (scm_is_true (caught));
  }

#endif /* SCM_SIZEOF_LONG_LONG != 0 */
}

static void
tests (void *data, int argc, char **argv)
{
  test_long_long ();
  test_ulong_long ();
}

int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, tests, NULL);
  return 0;
}

#else  /* SCM_ENABLE_DISCOURAGED == 0 */

int
main (int argc, char *argv[])
{
  return 0;
}

#endif /* SCM_ENABLE_DISCOURAGED == 0 */
