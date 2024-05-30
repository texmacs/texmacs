/* This file contains definitions for discouraged features.  When you
   discourage something, move it here when that is feasible.
*/

/* Copyright (C) 2003, 2004, 2006, 2008 Free Software Foundation, Inc.
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

#include <libguile.h>


#if (SCM_ENABLE_DISCOURAGED == 1)

SCM
scm_short2num (short x)
{
  return scm_from_short (x);
}

SCM
scm_ushort2num (unsigned short x)
{
  return scm_from_ushort (x);
}

SCM
scm_int2num (int x)
{
  return scm_from_int (x);
}

SCM
scm_uint2num (unsigned int x)
{
  return scm_from_uint (x);
}

SCM
scm_long2num (long x)
{
  return scm_from_long (x);
}

SCM
scm_ulong2num (unsigned long x)
{
  return scm_from_ulong (x);
}

SCM
scm_size2num (size_t x)
{
  return scm_from_size_t (x);
}

SCM
scm_ptrdiff2num (ptrdiff_t x)
{
  return scm_from_ssize_t (x);
}

short
scm_num2short (SCM x, unsigned long pos, const char *s_caller)
{
  return scm_to_short (x);
}

unsigned short
scm_num2ushort (SCM x, unsigned long pos, const char *s_caller)
{
  return scm_to_ushort (x);
}

int
scm_num2int (SCM x, unsigned long pos, const char *s_caller)
{
  return scm_to_int (x);
}

unsigned int
scm_num2uint (SCM x, unsigned long pos, const char *s_caller)
{
  return scm_to_uint (x);
}

long
scm_num2long (SCM x, unsigned long pos, const char *s_caller)
{
  return scm_to_long (x);
}

unsigned long
scm_num2ulong (SCM x, unsigned long pos, const char *s_caller)
{
  return scm_to_ulong (x);
}

size_t
scm_num2size (SCM x, unsigned long pos, const char *s_caller)
{
  return scm_to_size_t (x);
}

ptrdiff_t
scm_num2ptrdiff (SCM x, unsigned long pos, const char *s_caller)
{
  return scm_to_ssize_t (x);
}

#if SCM_SIZEOF_LONG_LONG != 0

SCM
scm_long_long2num (long long x)
{
  return scm_from_long_long (x);
}

SCM
scm_ulong_long2num (unsigned long long x)
{
  return scm_from_ulong_long (x);
}

long long
scm_num2long_long (SCM x, unsigned long pos, const char *s_caller)
{
  return scm_to_long_long (x);
}

unsigned long long
scm_num2ulong_long (SCM x, unsigned long pos, const char *s_caller)
{
  return scm_to_ulong_long (x);
}

#endif

SCM
scm_make_real (double x)
{
  return scm_from_double (x);
}

double
scm_num2dbl (SCM a, const char *why)
{
  return scm_to_double (a);
}

SCM
scm_float2num (float n)
{
  return scm_from_double ((double) n);
}

SCM
scm_double2num (double n)
{
  return scm_from_double (n);
}

SCM
scm_make_complex (double x, double y)
{
  return scm_c_make_rectangular (x, y);
}

SCM
scm_mem2symbol (const char *mem, size_t len)
{
  return scm_from_locale_symboln (mem, len);
}

SCM
scm_mem2uninterned_symbol (const char *mem, size_t len)
{
  return scm_make_symbol (scm_from_locale_stringn (mem, len));
}

SCM
scm_str2symbol (const char *str)
{
  return scm_from_locale_symbol (str);
}


/* This function must only be applied to memory obtained via malloc,
   since the GC is going to apply `free' to it when the string is
   dropped.

   Also, s[len] must be `\0', since we promise that strings are
   null-terminated.  Perhaps we could handle non-null-terminated
   strings by claiming they're shared substrings of a string we just
   made up.  */
SCM
scm_take_str (char *s, size_t len)
{
  SCM answer = scm_from_locale_stringn (s, len);
  free (s);
  return answer;
}

/* `s' must be a malloc'd string.  See scm_take_str.  */
SCM
scm_take0str (char *s)
{
  return scm_take_locale_string (s);
}

SCM 
scm_mem2string (const char *src, size_t len)
{
  return scm_from_locale_stringn (src, len);
}

SCM
scm_str2string (const char *src)
{
  return scm_from_locale_string (src);
}

SCM 
scm_makfrom0str (const char *src)
{
  if (!src) return SCM_BOOL_F;
  return scm_from_locale_string (src);
}

SCM 
scm_makfrom0str_opt (const char *src)
{
  return scm_makfrom0str (src);
}


SCM
scm_allocate_string (size_t len)
{
  return scm_i_make_string (len, NULL);
}

SCM_DEFINE (scm_make_keyword_from_dash_symbol, "make-keyword-from-dash-symbol", 1, 0, 0, 
            (SCM symbol),
            "Make a keyword object from a @var{symbol} that starts with a dash.")
#define FUNC_NAME s_scm_make_keyword_from_dash_symbol
{
  SCM dash_string, non_dash_symbol;

  SCM_ASSERT (scm_is_symbol (symbol)
	      && ('-' == scm_i_symbol_chars(symbol)[0]),
	      symbol, SCM_ARG1, FUNC_NAME);

  dash_string = scm_symbol_to_string (symbol);
  non_dash_symbol =
    scm_string_to_symbol (scm_c_substring (dash_string,
					   1,
					   scm_c_string_length (dash_string)));

  return scm_symbol_to_keyword (non_dash_symbol);
}
#undef FUNC_NAME

SCM_DEFINE (scm_keyword_dash_symbol, "keyword-dash-symbol", 1, 0, 0, 
            (SCM keyword),
	    "Return the dash symbol for @var{keyword}.\n"
	    "This is the inverse of @code{make-keyword-from-dash-symbol}.")
#define FUNC_NAME s_scm_keyword_dash_symbol
{
  SCM symbol = scm_keyword_to_symbol (keyword);
  SCM parts = scm_list_2 (scm_from_locale_string ("-"),
			  scm_symbol_to_string (symbol));
  return scm_string_to_symbol (scm_string_append (parts));
}
#undef FUNC_NAME

SCM
scm_c_make_keyword (const char *s)
{
  return scm_from_locale_keyword (s);
}


void
scm_i_init_discouraged (void)
{
#include "libguile/discouraged.x"
}

#endif
