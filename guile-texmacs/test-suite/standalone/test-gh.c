/* Copyright (C) 1999,2000,2001,2003, 2006, 2008 Free Software Foundation, Inc.
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

/* some bits originally by Jim Blandy <jimb@red-bean.com> */

#ifndef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libguile.h>
#include <libguile/gh.h>

#include <assert.h>
#include <string.h>

#if SCM_ENABLE_DEPRECATED

static int
string_equal (SCM str, char *lit)
{
  int len = strlen (lit);
  int result;
 
  result = ((scm_i_string_length (str) == len)
            && (!memcmp (scm_i_string_chars (str), lit, len)));
  scm_remember_upto_here_1 (str);
  return result;
}

static void
test_gh_set_substr ()
{
  SCM string;

  string = gh_str02scm ("Free, darnit!");
  assert (gh_string_p (string));

  gh_set_substr ("dammit", string, 6, 6);
  assert (string_equal (string, "Free, dammit!"));
  
  /* Make sure that we can use the string itself as a source.

     I guess this behavior isn't really visible, since the GH API
     doesn't provide any direct access to the string contents.  But I
     think it should, eventually.  You can't write efficient string
     code if you have to copy the string just to look at it.  */

  /* Copy a substring to an overlapping region to its right.  */
  gh_set_substr (scm_i_string_chars (string), string, 4, 6);
  assert (string_equal (string, "FreeFree, it!"));
  
  string = gh_str02scm ("Free, darnit!");
  assert (gh_string_p (string));

  /* Copy a substring to an overlapping region to its left.  */
  gh_set_substr (scm_i_string_chars (string) + 6, string, 2, 6);
  assert (string_equal (string, "Frdarnitrnit!"));
}

static void
tests (void *data, int argc, char **argv)
{
  test_gh_set_substr ();
}

int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, tests, NULL);
  return 0;
}

#else

int 
main (int argc, char *argv[])
{
  return 0;
}

#endif /* !SCM_ENABLE_DEPRECATED */
