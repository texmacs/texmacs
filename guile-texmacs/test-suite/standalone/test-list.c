/* test-list.c - exercise libguile/list.c functions */

/* Copyright (C) 2006, 2008 Free Software Foundation, Inc.
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
#include <string.h>

/* pretty trivial, but ensure this entrypoint exists, since it was
   documented in Guile 1.6 and earlier */
static void
test_scm_list (void)
{
  {
    if (! scm_is_eq (SCM_EOL, scm_list (SCM_EOL)))
      {
        fprintf (stderr, "fail: scm_list SCM_EOL\n");
        exit (1);
      }
  }

  {
    SCM lst = scm_list_2 (scm_from_int (1), scm_from_int (2));
    if (! scm_is_true (scm_equal_p (lst, scm_list (lst))))
      {
        fprintf (stderr, "fail: scm_list '(1 2)\n");
        exit (1);
      }
  }
}

static void
tests (void *data, int argc, char **argv)
{
  test_scm_list ();
}

int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, tests, NULL);
  return 0;
}
