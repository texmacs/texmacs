/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2004, 2006 Free Software Foundation, Inc.
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


/* data initialization and C<->Scheme data conversion */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "libguile/gh.h"
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <assert.h>

#if SCM_ENABLE_DEPRECATED

/* data conversion C->scheme */

SCM 
gh_bool2scm (int x)
{
  return scm_from_bool(x);
}
SCM 
gh_int2scm (int x)
{
  return scm_from_long ((long) x);
}
SCM 
gh_ulong2scm (unsigned long x)
{
  return scm_from_ulong (x);
}
SCM 
gh_long2scm (long x)
{
  return scm_from_long (x);
}
SCM 
gh_double2scm (double x)
{
  return scm_from_double (x);
}
SCM 
gh_char2scm (char c)
{
 return SCM_MAKE_CHAR (c);
}
SCM 
gh_str2scm (const char *s, size_t len)
{
  return scm_from_locale_stringn (s, len);
}
SCM 
gh_str02scm (const char *s)
{
  return scm_from_locale_string (s);
}
/* Copy LEN characters at SRC into the *existing* Scheme string DST,
   starting at START.  START is an index into DST; zero means the
   beginning of the string.

   If START + LEN is off the end of DST, signal an out-of-range
   error.  */
void 
gh_set_substr (const char *src, SCM dst, long start, size_t len)
{
  char *dst_ptr;
  size_t dst_len;

  SCM_ASSERT (scm_is_string (dst), dst, SCM_ARG3, "gh_set_substr");

  dst_len = scm_i_string_length (dst);
  SCM_ASSERT (start + len <= dst_len, dst, SCM_ARG4, "gh_set_substr");

  dst_ptr = scm_i_string_writable_chars (dst);
  memmove (dst_ptr + start, src, len);
  scm_i_string_stop_writing ();
  scm_remember_upto_here_1 (dst);
}

/* Return the symbol named SYMBOL_STR.  */
SCM 
gh_symbol2scm (const char *symbol_str)
{
  return scm_from_locale_symbol(symbol_str);
}

SCM
gh_ints2scm (const int *d, long n)
{
  long i;
  SCM v = scm_c_make_vector (n, SCM_UNSPECIFIED);
  for (i = 0; i < n; ++i)
    SCM_SIMPLE_VECTOR_SET (v, i, scm_from_int (d[i]));

  return v;
}

SCM
gh_doubles2scm (const double *d, long n)
{
  long i;
  SCM v = scm_c_make_vector (n, SCM_UNSPECIFIED);

  for(i = 0; i < n; i++) 
    SCM_SIMPLE_VECTOR_SET (v, i, scm_from_double (d[i]));
  return v;
}


SCM
gh_chars2byvect (const char *d, long n)
{
  char *m = scm_malloc (n);
  memcpy (m, d, n * sizeof (char));
  return scm_take_s8vector ((scm_t_int8 *)m, n);
}

SCM
gh_shorts2svect (const short *d, long n)
{
  char *m = scm_malloc (n * sizeof (short));
  memcpy (m, d, n * sizeof (short));
  assert (sizeof (scm_t_int16) == sizeof (short));
  return scm_take_s16vector ((scm_t_int16 *)m, n);
}

SCM
gh_longs2ivect (const long *d, long n)
{
  char *m = scm_malloc (n * sizeof (long));
  memcpy (m, d, n * sizeof (long));
  assert (sizeof (scm_t_int32) == sizeof (long));
  return scm_take_s32vector ((scm_t_int32 *)m, n);
}

SCM
gh_ulongs2uvect (const unsigned long *d, long n)
{
  char *m = scm_malloc (n * sizeof (unsigned long));
  memcpy (m, d, n * sizeof (unsigned long));
  assert (sizeof (scm_t_uint32) == sizeof (unsigned long));
  return scm_take_u32vector ((scm_t_uint32 *)m, n);
}

SCM
gh_floats2fvect (const float *d, long n)
{
  char *m = scm_malloc (n * sizeof (float));
  memcpy (m, d, n * sizeof (float));
  return scm_take_f32vector ((float *)m, n);
}

SCM
gh_doubles2dvect (const double *d, long n)
{
  char *m = scm_malloc (n * sizeof (double));
  memcpy (m, d, n * sizeof (double));
  return scm_take_f64vector ((double *)m, n);
}

/* data conversion scheme->C */
int 
gh_scm2bool (SCM obj)
{
  return (scm_is_false (obj)) ? 0 : 1;
}
unsigned long 
gh_scm2ulong (SCM obj)
{
  return scm_to_ulong (obj);
}
long 
gh_scm2long (SCM obj)
{
  return scm_to_long (obj);
}
int 
gh_scm2int (SCM obj)
{
  return scm_to_int (obj);
}
double 
gh_scm2double (SCM obj)
{
  return scm_to_double (obj);
}
char 
gh_scm2char (SCM obj)
#define FUNC_NAME "gh_scm2char"
{
  SCM_VALIDATE_CHAR (SCM_ARG1, obj);
  return SCM_CHAR (obj);
}
#undef FUNC_NAME

/* Convert a vector, weak vector, string, substring or uniform vector
   into an array of chars.  If result array in arg 2 is NULL, malloc a
   new one.  If out of memory, return NULL.  */
char *
gh_scm2chars (SCM obj, char *m)
{
  long i, n;
  long v;
  SCM val;
  if (SCM_IMP (obj))
    scm_wrong_type_arg (0, 0, obj);
  switch (SCM_TYP7 (obj))
    {
    case scm_tc7_vector:
    case scm_tc7_wvect:
      n = SCM_SIMPLE_VECTOR_LENGTH (obj);
      for (i = 0; i < n; ++i)
	{
	  val = SCM_SIMPLE_VECTOR_REF (obj, i);
	  if (SCM_I_INUMP (val))
	    {
	      v = SCM_I_INUM (val);
	      if (v < -128 || v > 255)
		scm_out_of_range (0, obj);
	    }
	  else
	    scm_wrong_type_arg (0, 0, obj);
	}
      if (m == 0)
	m = (char *) malloc (n * sizeof (char));
      if (m == NULL)
	return NULL;
      for (i = 0; i < n; ++i)
	m[i] = SCM_I_INUM (SCM_SIMPLE_VECTOR_REF (obj, i));
      break;
    case scm_tc7_smob:
      if (scm_is_true (scm_s8vector_p (obj)))
	{
	  scm_t_array_handle handle;
	  size_t len;
	  ssize_t inc;
	  const scm_t_int8 *elts;

	  elts = scm_s8vector_elements (obj, &handle, &len, &inc);
	  if (inc != 1)
	    scm_misc_error (NULL, "only contiguous vectors are supported: ~a",
			    scm_list_1 (obj));
	  if (m == 0)
	    m = (char *) malloc (len);
	  if (m != NULL)
	    memcpy (m, elts, len);
	  scm_array_handle_release (&handle);
	  if (m == NULL)
	    return NULL;
	  break;
	}
      else
	goto wrong_type;
    case scm_tc7_string:
      n = scm_i_string_length (obj);
      if (m == 0)
	m = (char *) malloc (n * sizeof (char));
      if (m == NULL)
	return NULL;
      memcpy (m, scm_i_string_chars (obj), n * sizeof (char));
      break;
    default:
    wrong_type:
      scm_wrong_type_arg (0, 0, obj);
    }
  return m;
}

static void *
scm2whatever (SCM obj, void *m, size_t size)
{
  scm_t_array_handle handle;
  size_t len;
  ssize_t inc;
  const void *elts;

  elts = scm_uniform_vector_elements (obj, &handle, &len, &inc);

  if (inc != 1)
    scm_misc_error (NULL, "only contiguous vectors can be converted: ~a",
		    scm_list_1 (obj));

  if (m == 0)
    m = malloc (len * sizeof (size));
  if (m != NULL)
    memcpy (m, elts, len * size);

  scm_array_handle_release (&handle);

  return m;
}

#define SCM2WHATEVER(obj,pred,utype,mtype)                   \
  if (scm_is_true (pred (obj)))                              \
    {                                                        \
      assert (sizeof (utype) == sizeof (mtype));             \
      return (mtype *)scm2whatever (obj, m, sizeof (utype)); \
    }

/* Convert a vector, weak vector or uniform vector into an array of
   shorts.  If result array in arg 2 is NULL, malloc a new one.  If
   out of memory, return NULL.  */
short *
gh_scm2shorts (SCM obj, short *m)
{
  long i, n;
  long v;
  SCM val;
  if (SCM_IMP (obj))
    scm_wrong_type_arg (0, 0, obj);

  SCM2WHATEVER (obj, scm_s16vector_p, scm_t_int16, short)

  switch (SCM_TYP7 (obj))
    {
    case scm_tc7_vector:
    case scm_tc7_wvect:
      n = SCM_SIMPLE_VECTOR_LENGTH (obj);
      for (i = 0; i < n; ++i)
	{
	  val = SCM_SIMPLE_VECTOR_REF (obj, i);
	  if (SCM_I_INUMP (val))
	    {
	      v = SCM_I_INUM (val);
	      if (v < -32768 || v > 65535)
		scm_out_of_range (0, obj);
	    }
	  else
	    scm_wrong_type_arg (0, 0, obj);
	}
      if (m == 0)
	m = (short *) malloc (n * sizeof (short));
      if (m == NULL)
	return NULL;
      for (i = 0; i < n; ++i)
	m[i] = SCM_I_INUM (SCM_SIMPLE_VECTOR_REF (obj, i));
      break;
    default:
      scm_wrong_type_arg (0, 0, obj);
    }
  return m;
}

/* Convert a vector, weak vector or uniform vector into an array of
   longs.  If result array in arg 2 is NULL, malloc a new one.  If out
   of memory, return NULL.  */
long *
gh_scm2longs (SCM obj, long *m)
{
  long i, n;
  SCM val;
  if (SCM_IMP (obj))
    scm_wrong_type_arg (0, 0, obj);

  SCM2WHATEVER (obj, scm_s32vector_p, scm_t_int32, long)

  switch (SCM_TYP7 (obj))
    {
    case scm_tc7_vector:
    case scm_tc7_wvect:
      n = SCM_SIMPLE_VECTOR_LENGTH (obj);
      for (i = 0; i < n; ++i)
	{
	  val = SCM_SIMPLE_VECTOR_REF (obj, i);
	  if (!SCM_I_INUMP (val) && !SCM_BIGP (val))
	    scm_wrong_type_arg (0, 0, obj);
	}
      if (m == 0)
	m = (long *) malloc (n * sizeof (long));
      if (m == NULL)
	return NULL;
      for (i = 0; i < n; ++i)
	{
	  val = SCM_SIMPLE_VECTOR_REF (obj, i);
	  m[i] = SCM_I_INUMP (val) 
	    ? SCM_I_INUM (val) 
	    : scm_to_long (val);
	}
      break;
    default:
      scm_wrong_type_arg (0, 0, obj);
    }
  return m;
}

/* Convert a vector, weak vector or uniform vector into an array of
   floats.  If result array in arg 2 is NULL, malloc a new one.  If
   out of memory, return NULL.  */
float *
gh_scm2floats (SCM obj, float *m)
{
  long i, n;
  SCM val;
  if (SCM_IMP (obj))
    scm_wrong_type_arg (0, 0, obj);

  /* XXX - f64vectors are rejected now.
   */
  SCM2WHATEVER (obj, scm_f32vector_p, float, float)

  switch (SCM_TYP7 (obj))
    {
    case scm_tc7_vector:
    case scm_tc7_wvect:
      n = SCM_SIMPLE_VECTOR_LENGTH (obj);
      for (i = 0; i < n; ++i)
	{
	  val = SCM_SIMPLE_VECTOR_REF (obj, i);
	  if (!SCM_I_INUMP (val)
	      && !(SCM_BIGP (val) || SCM_REALP (val)))
	    scm_wrong_type_arg (0, 0, val);
	}
      if (m == 0)
	m = (float *) malloc (n * sizeof (float));
      if (m == NULL)
	return NULL;
      for (i = 0; i < n; ++i)
	{
	  val = SCM_SIMPLE_VECTOR_REF (obj, i);
	  if (SCM_I_INUMP (val))
	    m[i] = SCM_I_INUM (val);
	  else if (SCM_BIGP (val))
	    m[i] = scm_to_long (val);
	  else
	    m[i] = SCM_REAL_VALUE (val);
	}
      break;
    default:
      scm_wrong_type_arg (0, 0, obj);
    }
  return m;
}

/* Convert a vector, weak vector or uniform vector into an array of
   doubles.  If result array in arg 2 is NULL, malloc a new one.  If
   out of memory, return NULL.  */
double *
gh_scm2doubles (SCM obj, double *m)
{
  long i, n;
  SCM val;
  if (SCM_IMP (obj))
    scm_wrong_type_arg (0, 0, obj);

  /* XXX - f32vectors are rejected now.
   */
  SCM2WHATEVER (obj, scm_f64vector_p, double, double)

  switch (SCM_TYP7 (obj))
    {
    case scm_tc7_vector:
    case scm_tc7_wvect:
      n = SCM_SIMPLE_VECTOR_LENGTH (obj);
      for (i = 0; i < n; ++i)
	{
	  val = SCM_SIMPLE_VECTOR_REF (obj, i);
	  if (!SCM_I_INUMP (val)
	      && !(SCM_BIGP (val) || SCM_REALP (val)))
	    scm_wrong_type_arg (0, 0, val);
	}
      if (m == 0)
	m = (double *) malloc (n * sizeof (double));
      if (m == NULL)
	return NULL;
      for (i = 0; i < n; ++i)
	{
	  val = SCM_SIMPLE_VECTOR_REF (obj, i);
	  if (SCM_I_INUMP (val))
	    m[i] = SCM_I_INUM (val);
	  else if (SCM_BIGP (val))
	    m[i] = scm_to_long (val);
	  else
	    m[i] = SCM_REAL_VALUE (val);
	}
      break;

    default:
      scm_wrong_type_arg (0, 0, obj);
    }
  return m;
}

/* string conversions between C and Scheme */

/* gh_scm2newstr() -- Given a Scheme string STR, return a pointer to a
   new copy of its contents, followed by a null byte.  If lenp is
   non-null, set *lenp to the string's length.

   This function uses malloc to obtain storage for the copy; the
   caller is responsible for freeing it.  If out of memory, NULL is
   returned.

   Note that Scheme strings may contain arbitrary data, including null
   characters.  This means that null termination is not a reliable way
   to determine the length of the returned value.  However, the
   function always copies the complete contents of STR, and sets
   *LEN_P to the true length of the string (when LEN_P is non-null).  */
char *
gh_scm2newstr (SCM str, size_t *lenp)
{
  char *ret_str;

  /* We can't use scm_to_locale_stringn directly since it does not
     guarantee null-termination when lenp is non-NULL.
   */

  ret_str = scm_to_locale_string (str);
  if (lenp)
    *lenp = scm_i_string_length (str);
  return ret_str;
}

/* Copy LEN characters at START from the Scheme string SRC to memory
   at DST.  START is an index into SRC; zero means the beginning of
   the string.  DST has already been allocated by the caller.

   If START + LEN is off the end of SRC, silently truncate the source
   region to fit the string.  If truncation occurs, the corresponding
   area of DST is left unchanged.  */
void 
gh_get_substr (SCM src, char *dst, long start, size_t len)
{
  size_t src_len, effective_length;
  SCM_ASSERT (scm_is_string (src), src, SCM_ARG3, "gh_get_substr");

  src_len = scm_i_string_length (src);
  effective_length = (len < src_len) ? len : src_len;
  memcpy (dst + start, scm_i_string_chars (src), effective_length * sizeof (char));
  /* FIXME: must signal an error if len > src_len */
  scm_remember_upto_here_1 (src);
}


/* gh_scm2newsymbol() -- Given a Scheme symbol 'identifier, return a
   pointer to a string with the symbol characters "identifier",
   followed by a null byte.  If lenp is non-null, set *lenp to the
   string's length.

   This function uses malloc to obtain storage for the copy; the
   caller is responsible for freeing it.  If out of memory, NULL is
   returned.*/
char *
gh_symbol2newstr (SCM sym, size_t *lenp)
{
  return gh_scm2newstr (scm_symbol_to_string (sym), lenp);
}


/* create a new vector of the given length, all initialized to the
   given value */
SCM
gh_make_vector (SCM len, SCM fill)
{
  return scm_make_vector (len, fill);
}

/* set the given element of the given vector to the given value */
SCM 
gh_vector_set_x (SCM vec, SCM pos, SCM val)
{
  return scm_vector_set_x (vec, pos, val);
}

/* retrieve the given element of the given vector */
SCM 
gh_vector_ref (SCM vec, SCM pos)
{
  return scm_vector_ref (vec, pos);
}

/* returns the length of the given vector */
unsigned long 
gh_vector_length (SCM v)
{
  return (unsigned long) scm_c_vector_length (v);
}

/* uniform vector support */

/* returns the length as a C unsigned long integer */
unsigned long
gh_uniform_vector_length (SCM v)
{
  return (unsigned long) scm_c_uniform_vector_length (v);
}

/* gets the given element from a uniform vector; ilist is a list (or
   possibly a single integer) of indices, and its length is the
   dimension of the uniform vector */
SCM
gh_uniform_vector_ref (SCM v, SCM ilist)
{
  return scm_uniform_vector_ref (v, ilist);
}

/* sets an individual element in a uniform vector */
/* SCM */
/* gh_list_to_uniform_array ( */

/* Data lookups between C and Scheme

   Look up a symbol with a given name, and return the object to which
   it is bound.  gh_lookup examines the Guile top level, and
   gh_module_lookup checks the module namespace specified by the
   `vec' argument.

   The return value is the Scheme object to which SNAME is bound, or
   SCM_UNDEFINED if SNAME is not bound in the given context.
 */

SCM
gh_lookup (const char *sname)
{
  return gh_module_lookup (scm_current_module (), sname);
}


SCM
gh_module_lookup (SCM module, const char *sname)
#define FUNC_NAME "gh_module_lookup"
{
  SCM sym, var;

  SCM_VALIDATE_MODULE (SCM_ARG1, module);

  sym = scm_from_locale_symbol (sname);
  var = scm_sym2var (sym, scm_module_lookup_closure (module), SCM_BOOL_F);
  if (var != SCM_BOOL_F)
    return SCM_VARIABLE_REF (var);
  else
    return SCM_UNDEFINED;
}
#undef FUNC_NAME

#endif /* SCM_ENABLE_DEPRECATED */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
