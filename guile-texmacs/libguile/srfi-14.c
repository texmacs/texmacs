/* srfi-14.c --- SRFI-14 procedures for Guile
 *
 * Copyright (C) 2001, 2004, 2006, 2008 Free Software Foundation, Inc.
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

#include <string.h>
#include <ctype.h>

#include "libguile.h"
#include "libguile/srfi-14.h"


#define SCM_CHARSET_SET(cs, idx)				\
  (((long *) SCM_SMOB_DATA (cs))[(idx) / SCM_BITS_PER_LONG] |=	\
    (1L << ((idx) % SCM_BITS_PER_LONG)))

#define SCM_CHARSET_UNSET(cs, idx)				\
  (((long *) SCM_SMOB_DATA (cs))[(idx) / SCM_BITS_PER_LONG] &=	\
    (~(1L << ((idx) % SCM_BITS_PER_LONG))))

#define BYTES_PER_CHARSET (SCM_CHARSET_SIZE / 8)
#define LONGS_PER_CHARSET (SCM_CHARSET_SIZE / SCM_BITS_PER_LONG)


/* Smob type code for character sets.  */
int scm_tc16_charset = 0;


/* Smob print hook for character sets.  */
static int
charset_print (SCM charset, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  int i;
  int first = 1;

  scm_puts ("#<charset {", port);
  for (i = 0; i < SCM_CHARSET_SIZE; i++)
    if (SCM_CHARSET_GET (charset, i))
      {
	if (first)
	  first = 0;
	else
	  scm_puts (" ", port);
	scm_write (SCM_MAKE_CHAR (i), port);
      }
  scm_puts ("}>", port);
  return 1;
}


/* Smob free hook for character sets. */
static size_t
charset_free (SCM charset)
{
  return scm_smob_free (charset);
}


/* Create a new, empty character set.  */
static SCM
make_char_set (const char * func_name)
{
  long * p;

  p = scm_gc_malloc (BYTES_PER_CHARSET, "character-set");
  memset (p, 0, BYTES_PER_CHARSET);
  SCM_RETURN_NEWSMOB (scm_tc16_charset, p);
}


SCM_DEFINE (scm_char_set_p, "char-set?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a character set, @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_char_set_p
{
  return scm_from_bool (SCM_SMOB_PREDICATE (scm_tc16_charset, obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_eq, "char-set=", 0, 0, 1,
	    (SCM char_sets),
	    "Return @code{#t} if all given character sets are equal.")
#define FUNC_NAME s_scm_char_set_eq
{
  int argnum = 1;
  long *cs1_data = NULL;

  SCM_VALIDATE_REST_ARGUMENT (char_sets);

  while (!scm_is_null (char_sets))
    {
      SCM csi = SCM_CAR (char_sets);
      long *csi_data;

      SCM_VALIDATE_SMOB (argnum, csi, charset);
      argnum++;
      csi_data = (long *) SCM_SMOB_DATA (csi);
      if (cs1_data == NULL)
	cs1_data = csi_data;
      else if (memcmp (cs1_data, csi_data, BYTES_PER_CHARSET) != 0)
	return SCM_BOOL_F;
      char_sets = SCM_CDR (char_sets);
    }
  return SCM_BOOL_T;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_leq, "char-set<=", 0, 0, 1,
	    (SCM char_sets),
	    "Return @code{#t} if every character set @var{cs}i is a subset\n"
	    "of character set @var{cs}i+1.")
#define FUNC_NAME s_scm_char_set_leq
{
  int argnum = 1;
  long *prev_data = NULL;

  SCM_VALIDATE_REST_ARGUMENT (char_sets);

  while (!scm_is_null (char_sets))
    {
      SCM csi = SCM_CAR (char_sets);
      long *csi_data;

      SCM_VALIDATE_SMOB (argnum, csi, charset);
      argnum++;
      csi_data = (long *) SCM_SMOB_DATA (csi);
      if (prev_data)
	{
	  int k;

	  for (k = 0; k < LONGS_PER_CHARSET; k++)
	    {
	      if ((prev_data[k] & csi_data[k]) != prev_data[k])
		return SCM_BOOL_F;
	    }
	}
      prev_data = csi_data;
      char_sets = SCM_CDR (char_sets);
    }
  return SCM_BOOL_T;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_hash, "char-set-hash", 1, 1, 0,
	    (SCM cs, SCM bound),
	    "Compute a hash value for the character set @var{cs}.  If\n"
	    "@var{bound} is given and non-zero, it restricts the\n"
	    "returned value to the range 0 @dots{} @var{bound - 1}.")
#define FUNC_NAME s_scm_char_set_hash
{
  const unsigned long default_bnd = 871;
  unsigned long bnd;
  long * p;
  unsigned long val = 0;
  int k;

  SCM_VALIDATE_SMOB (1, cs, charset);

  if (SCM_UNBNDP (bound))
    bnd = default_bnd;
  else
    {
      bnd = scm_to_ulong (bound);
      if (bnd == 0)
	bnd = default_bnd;
    }

  p = (long *) SCM_SMOB_DATA (cs);
  for (k = 0; k < LONGS_PER_CHARSET; k++)
    {
      if (p[k] != 0)
        val = p[k] + (val << 1);
    }
  return scm_from_ulong (val % bnd);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_cursor, "char-set-cursor", 1, 0, 0,
	    (SCM cs),
	    "Return a cursor into the character set @var{cs}.")
#define FUNC_NAME s_scm_char_set_cursor
{
  int idx;

  SCM_VALIDATE_SMOB (1, cs, charset);
  for (idx = 0; idx < SCM_CHARSET_SIZE; idx++)
    {
      if (SCM_CHARSET_GET (cs, idx))
	break;
    }
  return SCM_I_MAKINUM (idx);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_ref, "char-set-ref", 2, 0, 0,
	    (SCM cs, SCM cursor),
	    "Return the character at the current cursor position\n"
	    "@var{cursor} in the character set @var{cs}.  It is an error to\n"
	    "pass a cursor for which @code{end-of-char-set?} returns true.")
#define FUNC_NAME s_scm_char_set_ref
{
  size_t ccursor = scm_to_size_t (cursor);
  SCM_VALIDATE_SMOB (1, cs, charset);

  if (ccursor >= SCM_CHARSET_SIZE || !SCM_CHARSET_GET (cs, ccursor))
    SCM_MISC_ERROR ("invalid character set cursor: ~A", scm_list_1 (cursor));
  return SCM_MAKE_CHAR (ccursor);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_cursor_next, "char-set-cursor-next", 2, 0, 0,
	    (SCM cs, SCM cursor),
	    "Advance the character set cursor @var{cursor} to the next\n"
	    "character in the character set @var{cs}.  It is an error if the\n"
	    "cursor given satisfies @code{end-of-char-set?}.")
#define FUNC_NAME s_scm_char_set_cursor_next
{
  size_t ccursor = scm_to_size_t (cursor);
  SCM_VALIDATE_SMOB (1, cs, charset);

  if (ccursor >= SCM_CHARSET_SIZE || !SCM_CHARSET_GET (cs, ccursor))
    SCM_MISC_ERROR ("invalid character set cursor: ~A", scm_list_1 (cursor));
  for (ccursor++; ccursor < SCM_CHARSET_SIZE; ccursor++)
    {
      if (SCM_CHARSET_GET (cs, ccursor))
	break;
    }
  return SCM_I_MAKINUM (ccursor);
}
#undef FUNC_NAME


SCM_DEFINE (scm_end_of_char_set_p, "end-of-char-set?", 1, 0, 0,
	    (SCM cursor),
	    "Return @code{#t} if @var{cursor} has reached the end of a\n"
	    "character set, @code{#f} otherwise.")
#define FUNC_NAME s_scm_end_of_char_set_p
{
  size_t ccursor = scm_to_size_t (cursor);
  return scm_from_bool (ccursor >= SCM_CHARSET_SIZE);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_fold, "char-set-fold", 3, 0, 0,
	    (SCM kons, SCM knil, SCM cs),
	    "Fold the procedure @var{kons} over the character set @var{cs},\n"
	    "initializing it with @var{knil}.")
#define FUNC_NAME s_scm_char_set_fold
{
  int k;

  SCM_VALIDATE_PROC (1, kons);
  SCM_VALIDATE_SMOB (3, cs, charset);

  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    if (SCM_CHARSET_GET (cs, k))
      {
	knil = scm_call_2 (kons, SCM_MAKE_CHAR (k), knil);
      }
  return knil;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_unfold, "char-set-unfold", 4, 1, 0,
	    (SCM p, SCM f, SCM g, SCM seed, SCM base_cs),
	    "This is a fundamental constructor for character sets.\n"
	    "@itemize @bullet\n"
	    "@item @var{g} is used to generate a series of ``seed'' values\n"
	    "from the initial seed: @var{seed}, (@var{g} @var{seed}),\n"
	    "(@var{g}^2 @var{seed}), (@var{g}^3 @var{seed}), @dots{}\n"
	    "@item @var{p} tells us when to stop -- when it returns true\n"
	    "when applied to one of the seed values.\n"
	    "@item @var{f} maps each seed value to a character. These\n"
	    "characters are added to the base character set @var{base_cs} to\n"
	    "form the result; @var{base_cs} defaults to the empty set.\n"
	    "@end itemize")
#define FUNC_NAME s_scm_char_set_unfold
{
  SCM result, tmp;

  SCM_VALIDATE_PROC (1, p);
  SCM_VALIDATE_PROC (2, f);
  SCM_VALIDATE_PROC (3, g);
  if (!SCM_UNBNDP (base_cs))
    {
      SCM_VALIDATE_SMOB (5, base_cs, charset);
      result = scm_char_set_copy (base_cs);
    }
  else
    result = make_char_set (FUNC_NAME);

  tmp = scm_call_1 (p, seed);
  while (scm_is_false (tmp))
    {
      SCM ch = scm_call_1 (f, seed);
      if (!SCM_CHARP (ch))
	SCM_MISC_ERROR ("procedure ~S returned non-char", scm_list_1 (f));
      SCM_CHARSET_SET (result, SCM_CHAR (ch));

      seed = scm_call_1 (g, seed);
      tmp = scm_call_1 (p, seed);
    }
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_unfold_x, "char-set-unfold!", 5, 0, 0,
	    (SCM p, SCM f, SCM g, SCM seed, SCM base_cs),
	    "This is a fundamental constructor for character sets.\n"
	    "@itemize @bullet\n"
	    "@item @var{g} is used to generate a series of ``seed'' values\n"
	    "from the initial seed: @var{seed}, (@var{g} @var{seed}),\n"
	    "(@var{g}^2 @var{seed}), (@var{g}^3 @var{seed}), @dots{}\n"
	    "@item @var{p} tells us when to stop -- when it returns true\n"
	    "when applied to one of the seed values.\n"
	    "@item @var{f} maps each seed value to a character. These\n"
	    "characters are added to the base character set @var{base_cs} to\n"
	    "form the result; @var{base_cs} defaults to the empty set.\n"
	    "@end itemize")
#define FUNC_NAME s_scm_char_set_unfold_x
{
  SCM tmp;

  SCM_VALIDATE_PROC (1, p);
  SCM_VALIDATE_PROC (2, f);
  SCM_VALIDATE_PROC (3, g);
  SCM_VALIDATE_SMOB (5, base_cs, charset);

  tmp = scm_call_1 (p, seed);
  while (scm_is_false (tmp))
    {
      SCM ch = scm_call_1 (f, seed);
      if (!SCM_CHARP (ch))
	SCM_MISC_ERROR ("procedure ~S returned non-char", scm_list_1 (f));
      SCM_CHARSET_SET (base_cs, SCM_CHAR (ch));

      seed = scm_call_1 (g, seed);
      tmp = scm_call_1 (p, seed);
    }
  return base_cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_for_each, "char-set-for-each", 2, 0, 0,
	    (SCM proc, SCM cs),
	    "Apply @var{proc} to every character in the character set\n"
	    "@var{cs}.  The return value is not specified.")
#define FUNC_NAME s_scm_char_set_for_each
{
  int k;

  SCM_VALIDATE_PROC (1, proc);
  SCM_VALIDATE_SMOB (2, cs, charset);

  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    if (SCM_CHARSET_GET (cs, k))
      scm_call_1 (proc, SCM_MAKE_CHAR (k));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_map, "char-set-map", 2, 0, 0,
	    (SCM proc, SCM cs),
	    "Map the procedure @var{proc} over every character in @var{cs}.\n"
	    "@var{proc} must be a character -> character procedure.")
#define FUNC_NAME s_scm_char_set_map
{
  SCM result;
  int k;

  SCM_VALIDATE_PROC (1, proc);
  SCM_VALIDATE_SMOB (2, cs, charset);

  result = make_char_set (FUNC_NAME);
  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    if (SCM_CHARSET_GET (cs, k))
      {
	SCM ch = scm_call_1 (proc, SCM_MAKE_CHAR (k));
	if (!SCM_CHARP (ch))
	  SCM_MISC_ERROR ("procedure ~S returned non-char", scm_list_1 (proc));
	SCM_CHARSET_SET (result, SCM_CHAR (ch));
      }
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_copy, "char-set-copy", 1, 0, 0,
	    (SCM cs),
	    "Return a newly allocated character set containing all\n"
	    "characters in @var{cs}.")
#define FUNC_NAME s_scm_char_set_copy
{
  SCM ret;
  long * p1, * p2;
  int k;

  SCM_VALIDATE_SMOB (1, cs, charset);
  ret = make_char_set (FUNC_NAME);
  p1 = (long *) SCM_SMOB_DATA (cs);
  p2 = (long *) SCM_SMOB_DATA (ret);
  for (k = 0; k < LONGS_PER_CHARSET; k++)
    p2[k] = p1[k];
  return ret;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set, "char-set", 0, 0, 1,
	    (SCM rest),
	    "Return a character set containing all given characters.")
#define FUNC_NAME s_scm_char_set
{
  SCM cs;
  long * p;
  int argnum = 1;

  SCM_VALIDATE_REST_ARGUMENT (rest);
  cs = make_char_set (FUNC_NAME);
  p = (long *) SCM_SMOB_DATA (cs);
  while (!scm_is_null (rest))
    {
      int c;

      SCM_VALIDATE_CHAR_COPY (argnum, SCM_CAR (rest), c);
      argnum++;
      rest = SCM_CDR (rest);
      p[c / SCM_BITS_PER_LONG] |= 1L << (c % SCM_BITS_PER_LONG);
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_char_set, "list->char-set", 1, 1, 0,
	    (SCM list, SCM base_cs),
	    "Convert the character list @var{list} to a character set.  If\n"
	    "the character set @var{base_cs} is given, the character in this\n"
	    "set are also included in the result.")
#define FUNC_NAME s_scm_list_to_char_set
{
  SCM cs;
  long * p;

  SCM_VALIDATE_LIST (1, list);
  if (SCM_UNBNDP (base_cs))
    cs = make_char_set (FUNC_NAME);
  else
    {
      SCM_VALIDATE_SMOB (2, base_cs, charset);
      cs = scm_char_set_copy (base_cs);
    }
  p = (long *) SCM_SMOB_DATA (cs);
  while (!scm_is_null (list))
    {
      SCM chr = SCM_CAR (list);
      int c;

      SCM_VALIDATE_CHAR_COPY (0, chr, c);
      list = SCM_CDR (list);

      p[c / SCM_BITS_PER_LONG] |= 1L << (c % SCM_BITS_PER_LONG);
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_char_set_x, "list->char-set!", 2, 0, 0,
	    (SCM list, SCM base_cs),
	    "Convert the character list @var{list} to a character set.  The\n"
	    "characters are added to @var{base_cs} and @var{base_cs} is\n"
	    "returned.")
#define FUNC_NAME s_scm_list_to_char_set_x
{
  long * p;

  SCM_VALIDATE_LIST (1, list);
  SCM_VALIDATE_SMOB (2, base_cs, charset);
  p = (long *) SCM_SMOB_DATA (base_cs);
  while (!scm_is_null (list))
    {
      SCM chr = SCM_CAR (list);
      int c;

      SCM_VALIDATE_CHAR_COPY (0, chr, c);
      list = SCM_CDR (list);

      p[c / SCM_BITS_PER_LONG] |= 1L << (c % SCM_BITS_PER_LONG);
    }
  return base_cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_to_char_set, "string->char-set", 1, 1, 0,
	    (SCM str, SCM base_cs),
	    "Convert the string @var{str} to a character set.  If the\n"
	    "character set @var{base_cs} is given, the characters in this\n"
	    "set are also included in the result.")
#define FUNC_NAME s_scm_string_to_char_set
{
  SCM cs;
  long * p;
  const char * s;
  size_t k = 0, len;

  SCM_VALIDATE_STRING (1, str);
  if (SCM_UNBNDP (base_cs))
    cs = make_char_set (FUNC_NAME);
  else
    {
      SCM_VALIDATE_SMOB (2, base_cs, charset);
      cs = scm_char_set_copy (base_cs);
    }
  p = (long *) SCM_SMOB_DATA (cs);
  s = scm_i_string_chars (str);
  len = scm_i_string_length (str);
  while (k < len)
    {
      int c = s[k++];
      p[c / SCM_BITS_PER_LONG] |= 1L << (c % SCM_BITS_PER_LONG);
    }
  scm_remember_upto_here_1 (str);
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_to_char_set_x, "string->char-set!", 2, 0, 0,
	    (SCM str, SCM base_cs),
	    "Convert the string @var{str} to a character set.  The\n"
	    "characters from the string are added to @var{base_cs}, and\n"
	    "@var{base_cs} is returned.")
#define FUNC_NAME s_scm_string_to_char_set_x
{
  long * p;
  const char * s;
  size_t k = 0, len;

  SCM_VALIDATE_STRING (1, str);
  SCM_VALIDATE_SMOB (2, base_cs, charset);
  p = (long *) SCM_SMOB_DATA (base_cs);
  s = scm_i_string_chars (str);
  len = scm_i_string_length (str);
  while (k < len)
    {
      int c = s[k++];
      p[c / SCM_BITS_PER_LONG] |= 1L << (c % SCM_BITS_PER_LONG);
    }
  scm_remember_upto_here_1 (str);
  return base_cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_filter, "char-set-filter", 2, 1, 0,
	    (SCM pred, SCM cs, SCM base_cs),
	    "Return a character set containing every character from @var{cs}\n"
	    "so that it satisfies @var{pred}.  If provided, the characters\n"
	    "from @var{base_cs} are added to the result.")
#define FUNC_NAME s_scm_char_set_filter
{
  SCM ret;
  int k;
  long * p;

  SCM_VALIDATE_PROC (1, pred);
  SCM_VALIDATE_SMOB (2, cs, charset);
  if (!SCM_UNBNDP (base_cs))
    {
      SCM_VALIDATE_SMOB (3, base_cs, charset);
      ret = scm_char_set_copy (base_cs);
    }
  else
    ret = make_char_set (FUNC_NAME);
  p = (long *) SCM_SMOB_DATA (ret);
  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    {
      if (SCM_CHARSET_GET (cs, k))
	{
	  SCM res = scm_call_1 (pred, SCM_MAKE_CHAR (k));

	  if (scm_is_true (res))
	    p[k / SCM_BITS_PER_LONG] |= 1L << (k % SCM_BITS_PER_LONG);
	}
    }
  return ret;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_filter_x, "char-set-filter!", 3, 0, 0,
	    (SCM pred, SCM cs, SCM base_cs),
	    "Return a character set containing every character from @var{cs}\n"
	    "so that it satisfies @var{pred}.  The characters are added to\n"
	    "@var{base_cs} and @var{base_cs} is returned.")
#define FUNC_NAME s_scm_char_set_filter_x
{
  int k;
  long * p;

  SCM_VALIDATE_PROC (1, pred);
  SCM_VALIDATE_SMOB (2, cs, charset);
  SCM_VALIDATE_SMOB (3, base_cs, charset);
  p = (long *) SCM_SMOB_DATA (base_cs);
  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    {
      if (SCM_CHARSET_GET (cs, k))
	{
	  SCM res = scm_call_1 (pred, SCM_MAKE_CHAR (k));

	  if (scm_is_true (res))
	    p[k / SCM_BITS_PER_LONG] |= 1L << (k % SCM_BITS_PER_LONG);
	}
    }
  return base_cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_ucs_range_to_char_set, "ucs-range->char-set", 2, 2, 0,
	    (SCM lower, SCM upper, SCM error, SCM base_cs),
	    "Return a character set containing all characters whose\n"
	    "character codes lie in the half-open range\n"
	    "[@var{lower},@var{upper}).\n"
	    "\n"
	    "If @var{error} is a true value, an error is signalled if the\n"
	    "specified range contains characters which are not contained in\n"
	    "the implemented character range.  If @var{error} is @code{#f},\n"
	    "these characters are silently left out of the resultung\n"
	    "character set.\n"
	    "\n"
	    "The characters in @var{base_cs} are added to the result, if\n"
	    "given.")
#define FUNC_NAME s_scm_ucs_range_to_char_set
{
  SCM cs;
  size_t clower, cupper;
  long * p;

  clower = scm_to_size_t (lower);
  cupper = scm_to_size_t (upper);
  SCM_ASSERT_RANGE (2, upper, cupper >= clower);
  if (!SCM_UNBNDP (error))
    {
      if (scm_is_true (error))
	{
	  SCM_ASSERT_RANGE (1, lower, clower <= SCM_CHARSET_SIZE);
	  SCM_ASSERT_RANGE (2, upper, cupper <= SCM_CHARSET_SIZE);
	}
    }
  if (clower > SCM_CHARSET_SIZE)
    clower = SCM_CHARSET_SIZE;
  if (cupper > SCM_CHARSET_SIZE)
    cupper = SCM_CHARSET_SIZE;
  if (SCM_UNBNDP (base_cs))
    cs = make_char_set (FUNC_NAME);
  else
    {
      SCM_VALIDATE_SMOB (4, base_cs, charset);
      cs = scm_char_set_copy (base_cs);
    }
  p = (long *) SCM_SMOB_DATA (cs);
  while (clower < cupper)
    {
      p[clower / SCM_BITS_PER_LONG] |= 1L << (clower % SCM_BITS_PER_LONG);
      clower++;
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_ucs_range_to_char_set_x, "ucs-range->char-set!", 4, 0, 0,
	    (SCM lower, SCM upper, SCM error, SCM base_cs),
	    "Return a character set containing all characters whose\n"
	    "character codes lie in the half-open range\n"
	    "[@var{lower},@var{upper}).\n"
	    "\n"
	    "If @var{error} is a true value, an error is signalled if the\n"
	    "specified range contains characters which are not contained in\n"
	    "the implemented character range.  If @var{error} is @code{#f},\n"
	    "these characters are silently left out of the resultung\n"
	    "character set.\n"
	    "\n"
	    "The characters are added to @var{base_cs} and @var{base_cs} is\n"
	    "returned.")
#define FUNC_NAME s_scm_ucs_range_to_char_set_x
{
  size_t clower, cupper;
  long * p;

  clower = scm_to_size_t (lower);
  cupper = scm_to_size_t (upper);
  SCM_ASSERT_RANGE (2, upper, cupper >= clower);
  if (scm_is_true (error))
    {
      SCM_ASSERT_RANGE (1, lower, clower <= SCM_CHARSET_SIZE);
      SCM_ASSERT_RANGE (2, upper, cupper <= SCM_CHARSET_SIZE);
    }
  if (clower > SCM_CHARSET_SIZE)
    clower = SCM_CHARSET_SIZE;
  if (cupper > SCM_CHARSET_SIZE)
    cupper = SCM_CHARSET_SIZE;
  p = (long *) SCM_SMOB_DATA (base_cs);
  while (clower < cupper)
    {
      p[clower / SCM_BITS_PER_LONG] |= 1L << (clower % SCM_BITS_PER_LONG);
      clower++;
    }
  return base_cs;
}
#undef FUNC_NAME

SCM_DEFINE (scm_to_char_set, "->char-set", 1, 0, 0,
	    (SCM x),
	    "Coerces x into a char-set. @var{x} may be a string, character or char-set. A string is converted to the set of its constituent characters; a character is converted to a singleton set; a char-set is returned as-is.")
#define FUNC_NAME s_scm_to_char_set
{
  if (scm_is_string (x))
    return scm_string_to_char_set (x, SCM_UNDEFINED);
  else if (SCM_CHARP (x))
    return scm_char_set (scm_list_1 (x));
  else if (SCM_SMOB_PREDICATE (scm_tc16_charset, x))
    return x;
  else
    scm_wrong_type_arg (NULL, 0, x);
}
#undef FUNC_NAME

SCM_DEFINE (scm_char_set_size, "char-set-size", 1, 0, 0,
	    (SCM cs),
	    "Return the number of elements in character set @var{cs}.")
#define FUNC_NAME s_scm_char_set_size
{
  int k, count = 0;

  SCM_VALIDATE_SMOB (1, cs, charset);
  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    if (SCM_CHARSET_GET (cs, k))
      count++;
  return SCM_I_MAKINUM (count);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_count, "char-set-count", 2, 0, 0,
	    (SCM pred, SCM cs),
	    "Return the number of the elements int the character set\n"
	    "@var{cs} which satisfy the predicate @var{pred}.")
#define FUNC_NAME s_scm_char_set_count
{
  int k, count = 0;

  SCM_VALIDATE_PROC (1, pred);
  SCM_VALIDATE_SMOB (2, cs, charset);

  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    if (SCM_CHARSET_GET (cs, k))
      {
	SCM res = scm_call_1 (pred, SCM_MAKE_CHAR (k));
	if (scm_is_true (res))
	  count++;
      }
  return SCM_I_MAKINUM (count);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_to_list, "char-set->list", 1, 0, 0,
	    (SCM cs),
	    "Return a list containing the elements of the character set\n"
	    "@var{cs}.")
#define FUNC_NAME s_scm_char_set_to_list
{
  int k;
  SCM result = SCM_EOL;

  SCM_VALIDATE_SMOB (1, cs, charset);
  for (k = SCM_CHARSET_SIZE; k > 0; k--)
    if (SCM_CHARSET_GET (cs, k - 1))
      result = scm_cons (SCM_MAKE_CHAR (k - 1), result);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_to_string, "char-set->string", 1, 0, 0,
	    (SCM cs),
	    "Return a string containing the elements of the character set\n"
	    "@var{cs}.  The order in which the characters are placed in the\n"
	    "string is not defined.")
#define FUNC_NAME s_scm_char_set_to_string
{
  int k;
  int count = 0;
  int idx = 0;
  SCM result;
  char * p;

  SCM_VALIDATE_SMOB (1, cs, charset);
  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    if (SCM_CHARSET_GET (cs, k))
      count++;
  result = scm_i_make_string (count, &p);
  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    if (SCM_CHARSET_GET (cs, k))
      p[idx++] = k;
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_contains_p, "char-set-contains?", 2, 0, 0,
	    (SCM cs, SCM ch),
	    "Return @code{#t} iff the character @var{ch} is contained in the\n"
	    "character set @var{cs}.")
#define FUNC_NAME s_scm_char_set_contains_p
{
  SCM_VALIDATE_SMOB (1, cs, charset);
  SCM_VALIDATE_CHAR (2, ch);
  return scm_from_bool (SCM_CHARSET_GET (cs, SCM_CHAR (ch)));
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_every, "char-set-every", 2, 0, 0,
	    (SCM pred, SCM cs),
	    "Return a true value if every character in the character set\n"
	    "@var{cs} satisfies the predicate @var{pred}.")
#define FUNC_NAME s_scm_char_set_every
{
  int k;
  SCM res = SCM_BOOL_T;

  SCM_VALIDATE_PROC (1, pred);
  SCM_VALIDATE_SMOB (2, cs, charset);

  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    if (SCM_CHARSET_GET (cs, k))
      {
	res = scm_call_1 (pred, SCM_MAKE_CHAR (k));
	if (scm_is_false (res))
	  return res;
      }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_any, "char-set-any", 2, 0, 0,
	    (SCM pred, SCM cs),
	    "Return a true value if any character in the character set\n"
	    "@var{cs} satisfies the predicate @var{pred}.")
#define FUNC_NAME s_scm_char_set_any
{
  int k;

  SCM_VALIDATE_PROC (1, pred);
  SCM_VALIDATE_SMOB (2, cs, charset);

  for (k = 0; k < SCM_CHARSET_SIZE; k++)
    if (SCM_CHARSET_GET (cs, k))
      {
	SCM res = scm_call_1 (pred, SCM_MAKE_CHAR (k));
	if (scm_is_true (res))
	  return res;
      }
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_adjoin, "char-set-adjoin", 1, 0, 1,
	    (SCM cs, SCM rest),
	    "Add all character arguments to the first argument, which must\n"
	    "be a character set.")
#define FUNC_NAME s_scm_char_set_adjoin
{
  long * p;

  SCM_VALIDATE_SMOB (1, cs, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);
  cs = scm_char_set_copy (cs);

  p = (long *) SCM_SMOB_DATA (cs);
  while (!scm_is_null (rest))
    {
      SCM chr = SCM_CAR (rest);
      int c;

      SCM_VALIDATE_CHAR_COPY (1, chr, c);
      rest = SCM_CDR (rest);

      p[c / SCM_BITS_PER_LONG] |= 1L << (c % SCM_BITS_PER_LONG);
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_delete, "char-set-delete", 1, 0, 1,
	    (SCM cs, SCM rest),
	    "Delete all character arguments from the first argument, which\n"
	    "must be a character set.")
#define FUNC_NAME s_scm_char_set_delete
{
  long * p;

  SCM_VALIDATE_SMOB (1, cs, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);
  cs = scm_char_set_copy (cs);

  p = (long *) SCM_SMOB_DATA (cs);
  while (!scm_is_null (rest))
    {
      SCM chr = SCM_CAR (rest);
      int c;

      SCM_VALIDATE_CHAR_COPY (1, chr, c);
      rest = SCM_CDR (rest);

      p[c / SCM_BITS_PER_LONG] &= ~(1L << (c % SCM_BITS_PER_LONG));
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_adjoin_x, "char-set-adjoin!", 1, 0, 1,
	    (SCM cs, SCM rest),
	    "Add all character arguments to the first argument, which must\n"
	    "be a character set.")
#define FUNC_NAME s_scm_char_set_adjoin_x
{
  long * p;

  SCM_VALIDATE_SMOB (1, cs, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  p = (long *) SCM_SMOB_DATA (cs);
  while (!scm_is_null (rest))
    {
      SCM chr = SCM_CAR (rest);
      int c;

      SCM_VALIDATE_CHAR_COPY (1, chr, c);
      rest = SCM_CDR (rest);

      p[c / SCM_BITS_PER_LONG] |= 1L << (c % SCM_BITS_PER_LONG);
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_delete_x, "char-set-delete!", 1, 0, 1,
	    (SCM cs, SCM rest),
	    "Delete all character arguments from the first argument, which\n"
	    "must be a character set.")
#define FUNC_NAME s_scm_char_set_delete_x
{
  long * p;

  SCM_VALIDATE_SMOB (1, cs, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  p = (long *) SCM_SMOB_DATA (cs);
  while (!scm_is_null (rest))
    {
      SCM chr = SCM_CAR (rest);
      int c;

      SCM_VALIDATE_CHAR_COPY (1, chr, c);
      rest = SCM_CDR (rest);

      p[c / SCM_BITS_PER_LONG] &= ~(1L << (c % SCM_BITS_PER_LONG));
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_complement, "char-set-complement", 1, 0, 0,
	    (SCM cs),
	    "Return the complement of the character set @var{cs}.")
#define FUNC_NAME s_scm_char_set_complement
{
  int k;
  SCM res;
  long * p, * q;

  SCM_VALIDATE_SMOB (1, cs, charset);

  res = make_char_set (FUNC_NAME);
  p = (long *) SCM_SMOB_DATA (res);
  q = (long *) SCM_SMOB_DATA (cs);
  for (k = 0; k < LONGS_PER_CHARSET; k++)
    p[k] = ~q[k];
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_union, "char-set-union", 0, 0, 1,
	    (SCM rest),
	    "Return the union of all argument character sets.")
#define FUNC_NAME s_scm_char_set_union
{
  int c = 1;
  SCM res;
  long * p;

  SCM_VALIDATE_REST_ARGUMENT (rest);

  res = make_char_set (FUNC_NAME);
  p = (long *) SCM_SMOB_DATA (res);
  while (!scm_is_null (rest))
    {
      int k;
      SCM cs = SCM_CAR (rest);
      SCM_VALIDATE_SMOB (c, cs, charset);
      c++;
      rest = SCM_CDR (rest);

      for (k = 0; k < LONGS_PER_CHARSET; k++)
	p[k] |= ((long *) SCM_SMOB_DATA (cs))[k];
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_intersection, "char-set-intersection", 0, 0, 1,
	    (SCM rest),
	    "Return the intersection of all argument character sets.")
#define FUNC_NAME s_scm_char_set_intersection
{
  SCM res;

  SCM_VALIDATE_REST_ARGUMENT (rest);

  if (scm_is_null (rest))
    res = make_char_set (FUNC_NAME);
  else
    {
      long *p;
      int argnum = 2;

      res = scm_char_set_copy (SCM_CAR (rest));
      p = (long *) SCM_SMOB_DATA (res);
      rest = SCM_CDR (rest);

      while (scm_is_pair (rest))
	{
	  int k;
	  SCM cs = SCM_CAR (rest);
	  long *cs_data;

	  SCM_VALIDATE_SMOB (argnum, cs, charset);
	  argnum++;
	  cs_data = (long *) SCM_SMOB_DATA (cs);
	  rest = SCM_CDR (rest);
	  for (k = 0; k < LONGS_PER_CHARSET; k++)
	    p[k] &= cs_data[k];
	}
    }

  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_difference, "char-set-difference", 1, 0, 1,
	    (SCM cs1, SCM rest),
	    "Return the difference of all argument character sets.")
#define FUNC_NAME s_scm_char_set_difference
{
  int c = 2;
  SCM res;
  long * p;

  SCM_VALIDATE_SMOB (1, cs1, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  res = scm_char_set_copy (cs1);
  p = (long *) SCM_SMOB_DATA (res);
  while (!scm_is_null (rest))
    {
      int k;
      SCM cs = SCM_CAR (rest);
      SCM_VALIDATE_SMOB (c, cs, charset);
      c++;
      rest = SCM_CDR (rest);

      for (k = 0; k < LONGS_PER_CHARSET; k++)
	p[k] &= ~((long *) SCM_SMOB_DATA (cs))[k];
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_xor, "char-set-xor", 0, 0, 1,
	    (SCM rest),
	    "Return the exclusive-or of all argument character sets.")
#define FUNC_NAME s_scm_char_set_xor
{
  SCM res;

  SCM_VALIDATE_REST_ARGUMENT (rest);

  if (scm_is_null (rest))
    res = make_char_set (FUNC_NAME);
  else
    {
      int argnum = 2;
      long * p;

      res = scm_char_set_copy (SCM_CAR (rest));
      p = (long *) SCM_SMOB_DATA (res);
      rest = SCM_CDR (rest);

      while (scm_is_pair (rest))
	{
	  SCM cs = SCM_CAR (rest);
	  long *cs_data;
	  int k;

	  SCM_VALIDATE_SMOB (argnum, cs, charset);
	  argnum++;
	  cs_data = (long *) SCM_SMOB_DATA (cs);
	  rest = SCM_CDR (rest);

	  for (k = 0; k < LONGS_PER_CHARSET; k++)
	    p[k] ^= cs_data[k];
	}
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_diff_plus_intersection, "char-set-diff+intersection", 1, 0, 1,
	    (SCM cs1, SCM rest),
	    "Return the difference and the intersection of all argument\n"
	    "character sets.")
#define FUNC_NAME s_scm_char_set_diff_plus_intersection
{
  int c = 2;
  SCM res1, res2;
  long * p, * q;

  SCM_VALIDATE_SMOB (1, cs1, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  res1 = scm_char_set_copy (cs1);
  res2 = make_char_set (FUNC_NAME);
  p = (long *) SCM_SMOB_DATA (res1);
  q = (long *) SCM_SMOB_DATA (res2);
  while (!scm_is_null (rest))
    {
      int k;
      SCM cs = SCM_CAR (rest);
      long *r;

      SCM_VALIDATE_SMOB (c, cs, charset);
      c++;
      r = (long *) SCM_SMOB_DATA (cs);

      for (k = 0; k < LONGS_PER_CHARSET; k++)
	{
	  q[k] |= p[k] & r[k];
	  p[k] &= ~r[k];
	}
      rest = SCM_CDR (rest);
    }
  return scm_values (scm_list_2 (res1, res2));
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_complement_x, "char-set-complement!", 1, 0, 0,
	    (SCM cs),
	    "Return the complement of the character set @var{cs}.")
#define FUNC_NAME s_scm_char_set_complement_x
{
  int k;
  long * p;

  SCM_VALIDATE_SMOB (1, cs, charset);
  p = (long *) SCM_SMOB_DATA (cs);
  for (k = 0; k < LONGS_PER_CHARSET; k++)
    p[k] = ~p[k];
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_union_x, "char-set-union!", 1, 0, 1,
	    (SCM cs1, SCM rest),
	    "Return the union of all argument character sets.")
#define FUNC_NAME s_scm_char_set_union_x
{
  int c = 2;
  long * p;

  SCM_VALIDATE_SMOB (1, cs1, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  p = (long *) SCM_SMOB_DATA (cs1);
  while (!scm_is_null (rest))
    {
      int k;
      SCM cs = SCM_CAR (rest);
      SCM_VALIDATE_SMOB (c, cs, charset);
      c++;
      rest = SCM_CDR (rest);

      for (k = 0; k < LONGS_PER_CHARSET; k++)
	p[k] |= ((long *) SCM_SMOB_DATA (cs))[k];
    }
  return cs1;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_intersection_x, "char-set-intersection!", 1, 0, 1,
	    (SCM cs1, SCM rest),
	    "Return the intersection of all argument character sets.")
#define FUNC_NAME s_scm_char_set_intersection_x
{
  int c = 2;
  long * p;

  SCM_VALIDATE_SMOB (1, cs1, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  p = (long *) SCM_SMOB_DATA (cs1);
  while (!scm_is_null (rest))
    {
      int k;
      SCM cs = SCM_CAR (rest);
      SCM_VALIDATE_SMOB (c, cs, charset);
      c++;
      rest = SCM_CDR (rest);

      for (k = 0; k < LONGS_PER_CHARSET; k++)
	p[k] &= ((long *) SCM_SMOB_DATA (cs))[k];
    }
  return cs1;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_difference_x, "char-set-difference!", 1, 0, 1,
	    (SCM cs1, SCM rest),
	    "Return the difference of all argument character sets.")
#define FUNC_NAME s_scm_char_set_difference_x
{
  int c = 2;
  long * p;

  SCM_VALIDATE_SMOB (1, cs1, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  p = (long *) SCM_SMOB_DATA (cs1);
  while (!scm_is_null (rest))
    {
      int k;
      SCM cs = SCM_CAR (rest);
      SCM_VALIDATE_SMOB (c, cs, charset);
      c++;
      rest = SCM_CDR (rest);

      for (k = 0; k < LONGS_PER_CHARSET; k++)
	p[k] &= ~((long *) SCM_SMOB_DATA (cs))[k];
    }
  return cs1;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_xor_x, "char-set-xor!", 1, 0, 1,
	    (SCM cs1, SCM rest),
	    "Return the exclusive-or of all argument character sets.")
#define FUNC_NAME s_scm_char_set_xor_x
{
  /* a side-effecting variant should presumably give consistent results:
     (define a (char-set #\a))
     (char-set-xor a a a) -> char set #\a
     (char-set-xor! a a a) -> char set #\a
  */
  return scm_char_set_xor (scm_cons (cs1, rest));

#if 0
  /* this would give (char-set-xor! a a a) -> empty char set.  */
  int c = 2;
  long * p;

  SCM_VALIDATE_SMOB (1, cs1, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  p = (long *) SCM_SMOB_DATA (cs1);
  while (!scm_is_null (rest))
    {
      int k;
      SCM cs = SCM_CAR (rest);
      SCM_VALIDATE_SMOB (c, cs, charset);
      c++;
      rest = SCM_CDR (rest);

      for (k = 0; k < LONGS_PER_CHARSET; k++)
	p[k] ^= ((long *) SCM_SMOB_DATA (cs))[k];
    }
  return cs1;
#endif
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_diff_plus_intersection_x, "char-set-diff+intersection!", 2, 0, 1,
	    (SCM cs1, SCM cs2, SCM rest),
	    "Return the difference and the intersection of all argument\n"
	    "character sets.")
#define FUNC_NAME s_scm_char_set_diff_plus_intersection_x
{
  int c = 3;
  long * p, * q;
  int k;

  SCM_VALIDATE_SMOB (1, cs1, charset);
  SCM_VALIDATE_SMOB (2, cs2, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  p = (long *) SCM_SMOB_DATA (cs1);
  q = (long *) SCM_SMOB_DATA (cs2);
  if (p == q)
    {
      /* (char-set-diff+intersection! a a ...): can't share storage,
	 but we know the answer without checking for further
	 arguments.  */
      return scm_values (scm_list_2 (make_char_set (FUNC_NAME), cs1));
    }
  for (k = 0; k < LONGS_PER_CHARSET; k++)
    {
      long t = p[k];

      p[k] &= ~q[k];
      q[k] = t & q[k];
    }
  while (!scm_is_null (rest))
    {
      SCM cs = SCM_CAR (rest);
      long *r;

      SCM_VALIDATE_SMOB (c, cs, charset);
      c++;
      r = (long *) SCM_SMOB_DATA (cs);

      for (k = 0; k < LONGS_PER_CHARSET; k++)
	{
	  q[k] |= p[k] & r[k];
	  p[k] &= ~r[k];
	}
      rest = SCM_CDR (rest);
    }
  return scm_values (scm_list_2 (cs1, cs2));
}
#undef FUNC_NAME


/* Standard character sets.  */

SCM scm_char_set_lower_case;
SCM scm_char_set_upper_case;
SCM scm_char_set_title_case;
SCM scm_char_set_letter;
SCM scm_char_set_digit;
SCM scm_char_set_letter_and_digit;
SCM scm_char_set_graphic;
SCM scm_char_set_printing;
SCM scm_char_set_whitespace;
SCM scm_char_set_iso_control;
SCM scm_char_set_punctuation;
SCM scm_char_set_symbol;
SCM scm_char_set_hex_digit;
SCM scm_char_set_blank;
SCM scm_char_set_ascii;
SCM scm_char_set_empty;
SCM scm_char_set_full;


/* Create an empty character set and return it after binding it to NAME.  */
static inline SCM
define_charset (const char *name)
{
  SCM cs = make_char_set (NULL);
  scm_c_define (name, cs);
  return scm_permanent_object (cs);
}

/* Membership predicates for the various char sets.

   XXX: The `punctuation' and `symbol' char sets have no direct equivalent in
   <ctype.h>.  Thus, the predicates below yield correct results for ASCII,
   but they do not provide the result described by the SRFI for Latin-1.  The
   correct Latin-1 result could only be obtained by hard-coding the
   characters listed by the SRFI, but the problem would remain for other
   8-bit charsets.

   Similarly, character 0xA0 in Latin-1 (unbreakable space, `#\0240') should
   be part of `char-set:blank'.  However, glibc's current (2006/09) Latin-1
   locales (which use the ISO 14652 "i18n" FDCC-set) do not consider it
   `blank' so it ends up in `char-set:punctuation'.  */
#ifdef HAVE_ISBLANK
# define CSET_BLANK_PRED(c)  (isblank (c))
#else
# define CSET_BLANK_PRED(c)			\
   (((c) == ' ') || ((c) == '\t'))
#endif

#define CSET_SYMBOL_PRED(c)					\
  (((c) != '\0') && (strchr ("$+<=>^`|~", (c)) != NULL))
#define CSET_PUNCT_PRED(c)					\
  ((ispunct (c)) && (!CSET_SYMBOL_PRED (c)))

#define CSET_LOWER_PRED(c)       (islower (c))
#define CSET_UPPER_PRED(c)       (isupper (c))
#define CSET_LETTER_PRED(c)      (isalpha (c))
#define CSET_DIGIT_PRED(c)       (isdigit (c))
#define CSET_WHITESPACE_PRED(c)  (isspace (c))
#define CSET_CONTROL_PRED(c)     (iscntrl (c))
#define CSET_HEX_DIGIT_PRED(c)   (isxdigit (c))
#define CSET_ASCII_PRED(c)       (isascii (c))

/* Some char sets are explicitly defined by the SRFI as a union of other char
   sets so we try to follow this closely.  */

#define CSET_LETTER_AND_DIGIT_PRED(c)		\
  (CSET_LETTER_PRED (c) || CSET_DIGIT_PRED (c))

#define CSET_GRAPHIC_PRED(c)				\
  (CSET_LETTER_PRED (c) || CSET_DIGIT_PRED (c)		\
   || CSET_PUNCT_PRED (c) || CSET_SYMBOL_PRED (c))

#define CSET_PRINTING_PRED(c)				\
  (CSET_GRAPHIC_PRED (c) || CSET_WHITESPACE_PRED (c))

/* False and true predicates.  */
#define CSET_TRUE_PRED(c)    (1)
#define CSET_FALSE_PRED(c)   (0)


/* Compute the contents of all the standard character sets.  Computation may
   need to be re-done at `setlocale'-time because some char sets (e.g.,
   `char-set:letter') need to reflect the character set supported by Guile.

   For instance, at startup time, the "C" locale is used, thus Guile supports
   only ASCII; therefore, `char-set:letter' only contains English letters.
   The user can change this by invoking `setlocale' and specifying a locale
   with an 8-bit charset, thereby augmenting some of the SRFI-14 standard
   character sets.

   This works because some of the predicates used below to construct
   character sets (e.g., `isalpha(3)') are locale-dependent (so
   charset-dependent, though generally not language-dependent).  For details,
   please see the `guile-devel' mailing list archive of September 2006.  */
void
scm_srfi_14_compute_char_sets (void)
{
#define UPDATE_CSET(c, cset, pred)		\
  do						\
    {						\
      if (pred (c))				\
	SCM_CHARSET_SET ((cset), (c));		\
      else					\
	SCM_CHARSET_UNSET ((cset), (c));	\
    }						\
  while (0)

  register int ch;

  for (ch = 0; ch < 256; ch++)
    {
      UPDATE_CSET (ch, scm_char_set_upper_case, CSET_UPPER_PRED);
      UPDATE_CSET (ch, scm_char_set_lower_case, CSET_LOWER_PRED);
      UPDATE_CSET (ch, scm_char_set_title_case, CSET_FALSE_PRED);
      UPDATE_CSET (ch, scm_char_set_letter, CSET_LETTER_PRED);
      UPDATE_CSET (ch, scm_char_set_digit, CSET_DIGIT_PRED);
      UPDATE_CSET (ch, scm_char_set_letter_and_digit,
		   CSET_LETTER_AND_DIGIT_PRED);
      UPDATE_CSET (ch, scm_char_set_graphic, CSET_GRAPHIC_PRED);
      UPDATE_CSET (ch, scm_char_set_printing, CSET_PRINTING_PRED);
      UPDATE_CSET (ch, scm_char_set_whitespace, CSET_WHITESPACE_PRED);
      UPDATE_CSET (ch, scm_char_set_iso_control, CSET_CONTROL_PRED);
      UPDATE_CSET (ch, scm_char_set_punctuation, CSET_PUNCT_PRED);
      UPDATE_CSET (ch, scm_char_set_symbol, CSET_SYMBOL_PRED);
      UPDATE_CSET (ch, scm_char_set_hex_digit, CSET_HEX_DIGIT_PRED);
      UPDATE_CSET (ch, scm_char_set_blank, CSET_BLANK_PRED);
      UPDATE_CSET (ch, scm_char_set_ascii, CSET_ASCII_PRED);
      UPDATE_CSET (ch, scm_char_set_empty, CSET_FALSE_PRED);
      UPDATE_CSET (ch, scm_char_set_full, CSET_TRUE_PRED);
    }

#undef UPDATE_CSET
}


void
scm_init_srfi_14 (void)
{
  scm_tc16_charset = scm_make_smob_type ("character-set",
					 BYTES_PER_CHARSET);
  scm_set_smob_free (scm_tc16_charset, charset_free);
  scm_set_smob_print (scm_tc16_charset, charset_print);

  scm_char_set_upper_case = define_charset ("char-set:upper-case");
  scm_char_set_lower_case = define_charset ("char-set:lower-case");
  scm_char_set_title_case = define_charset ("char-set:title-case");
  scm_char_set_letter = define_charset ("char-set:letter");
  scm_char_set_digit = define_charset ("char-set:digit");
  scm_char_set_letter_and_digit = define_charset ("char-set:letter+digit");
  scm_char_set_graphic = define_charset ("char-set:graphic");
  scm_char_set_printing = define_charset ("char-set:printing");
  scm_char_set_whitespace = define_charset ("char-set:whitespace");
  scm_char_set_iso_control = define_charset ("char-set:iso-control");
  scm_char_set_punctuation = define_charset ("char-set:punctuation");
  scm_char_set_symbol = define_charset ("char-set:symbol");
  scm_char_set_hex_digit = define_charset ("char-set:hex-digit");
  scm_char_set_blank = define_charset ("char-set:blank");
  scm_char_set_ascii = define_charset ("char-set:ascii");
  scm_char_set_empty = define_charset ("char-set:empty");
  scm_char_set_full = define_charset ("char-set:full");

  scm_srfi_14_compute_char_sets ();

#include "libguile/srfi-14.x"
}

/* End of srfi-14.c.  */
