/* srfi-4.c --- Uniform numeric vector datatypes.
 *
 * 	Copyright (C) 2001, 2004, 2006, 2010 Free Software Foundation, Inc.
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
#include <errno.h>
#include <stdio.h>

#include "libguile/_scm.h"
#include "libguile/__scm.h"
#include "libguile/srfi-4.h"
#include "libguile/error.h"
#include "libguile/read.h"
#include "libguile/ports.h"
#include "libguile/chars.h"
#include "libguile/vectors.h"
#include "libguile/unif.h"
#include "libguile/strings.h"
#include "libguile/strports.h"
#include "libguile/dynwind.h"
#include "libguile/deprecation.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_IO_H
#include <io.h>
#endif

/* Smob type code for uniform numeric vectors.  */
int scm_tc16_uvec = 0;

#define SCM_IS_UVEC(obj) SCM_SMOB_PREDICATE (scm_tc16_uvec, (obj))

/* Accessor macros for the three components of a uniform numeric
   vector:
   - The type tag (one of the symbolic constants below).
   - The vector's length (counted in elements).
   - The address of the data area (holding the elements of the
     vector). */
#define SCM_UVEC_TYPE(u)   (SCM_CELL_WORD_1(u))
#define SCM_UVEC_LENGTH(u) ((size_t)SCM_CELL_WORD_2(u))
#define SCM_UVEC_BASE(u)   ((void *)SCM_CELL_WORD_3(u))


/* Symbolic constants encoding the various types of uniform
   numeric vectors.  */
#define SCM_UVEC_U8  	0
#define SCM_UVEC_S8  	1
#define SCM_UVEC_U16 	2
#define SCM_UVEC_S16 	3
#define SCM_UVEC_U32 	4
#define SCM_UVEC_S32 	5
#define SCM_UVEC_U64 	6
#define SCM_UVEC_S64 	7
#define SCM_UVEC_F32 	8
#define SCM_UVEC_F64 	9
#define SCM_UVEC_C32   10
#define SCM_UVEC_C64   11


/* This array maps type tags to the size of the elements.  */
static const int uvec_sizes[12] = {
  1, 1,
  2, 2,
  4, 4,
  8, 8,
  sizeof(float), sizeof(double),
  2*sizeof(float), 2*sizeof(double)
};

static const char *uvec_tags[12] = {
  "u8", "s8",
  "u16", "s16",
  "u32", "s32",
  "u64", "s64",
  "f32", "f64",
  "c32", "c64",
};

static const char *uvec_names[12] = {
  "u8vector", "s8vector",
  "u16vector", "s16vector",
  "u32vector", "s32vector",
  "u64vector", "s64vector",
  "f32vector", "f64vector",
  "c32vector", "c64vector"
};

/* ================================================================ */
/* SMOB procedures.                                                 */
/* ================================================================ */


/* Smob print hook for uniform vectors.  */
static int
uvec_print (SCM uvec, SCM port, scm_print_state *pstate)
{
  union {
    scm_t_uint8 *u8;
    scm_t_int8 *s8;
    scm_t_uint16 *u16;
    scm_t_int16 *s16;
    scm_t_uint32 *u32;
    scm_t_int32 *s32;
    scm_t_uint64 *u64;
    scm_t_int64 *s64;
    float *f32;
    double *f64;
    SCM *fake_64;
  } np;

  size_t i = 0;
  const size_t uvlen = SCM_UVEC_LENGTH (uvec);
  void *uptr = SCM_UVEC_BASE (uvec);

  switch (SCM_UVEC_TYPE (uvec))
  {
    case SCM_UVEC_U8: np.u8 = (scm_t_uint8 *) uptr; break;
    case SCM_UVEC_S8: np.s8 = (scm_t_int8 *) uptr; break;
    case SCM_UVEC_U16: np.u16 = (scm_t_uint16 *) uptr; break;
    case SCM_UVEC_S16: np.s16 = (scm_t_int16 *) uptr; break;
    case SCM_UVEC_U32: np.u32 = (scm_t_uint32 *) uptr; break;
    case SCM_UVEC_S32: np.s32 = (scm_t_int32 *) uptr; break;
    case SCM_UVEC_U64: np.u64 = (scm_t_uint64 *) uptr; break;
    case SCM_UVEC_S64: np.s64 = (scm_t_int64 *) uptr; break;
    case SCM_UVEC_F32: np.f32 = (float *) uptr; break;
    case SCM_UVEC_F64: np.f64 = (double *) uptr; break;
    case SCM_UVEC_C32: np.f32 = (float *) uptr; break;
    case SCM_UVEC_C64: np.f64 = (double *) uptr; break;
    default:
      abort ();			/* Sanity check.  */
      break;
  }

  scm_putc ('#', port);
  scm_puts (uvec_tags [SCM_UVEC_TYPE (uvec)], port);
  scm_putc ('(', port);

  while (i < uvlen)
    {
      if (i != 0) scm_puts (" ", port);
      switch (SCM_UVEC_TYPE (uvec))
	{
	case SCM_UVEC_U8: scm_uintprint (*np.u8, 10, port); np.u8++; break;
	case SCM_UVEC_S8: scm_intprint (*np.s8, 10, port); np.s8++; break;
	case SCM_UVEC_U16: scm_uintprint (*np.u16, 10, port); np.u16++; break;
	case SCM_UVEC_S16: scm_intprint (*np.s16, 10, port); np.s16++; break;
	case SCM_UVEC_U32: scm_uintprint (*np.u32, 10, port); np.u32++; break;
	case SCM_UVEC_S32: scm_intprint (*np.s32, 10, port); np.s32++; break;
	case SCM_UVEC_U64: scm_uintprint (*np.u64, 10, port); np.u64++; break;
	case SCM_UVEC_S64: scm_intprint (*np.s64, 10, port); np.s64++; break;
	case SCM_UVEC_F32: scm_i_print_double (*np.f32, port); np.f32++; break;
	case SCM_UVEC_F64: scm_i_print_double (*np.f64, port); np.f64++; break;
	case SCM_UVEC_C32:
	  scm_i_print_complex (np.f32[0], np.f32[1], port);
	  np.f32 += 2;
	  break;
	case SCM_UVEC_C64:
	  scm_i_print_complex (np.f64[0], np.f64[1], port);
	  np.f64 += 2;
	  break;
	default:
	  abort ();			/* Sanity check.  */
	  break;
	}
      i++;
    }
  scm_remember_upto_here_1 (uvec);
  scm_puts (")", port);
  return 1;
}

const char *
scm_i_uniform_vector_tag (SCM uvec)
{
  return uvec_tags[SCM_UVEC_TYPE (uvec)];
}

static SCM
uvec_equalp (SCM a, SCM b)
{
  SCM result = SCM_BOOL_T;
  if (SCM_UVEC_TYPE (a) != SCM_UVEC_TYPE (b))
    result = SCM_BOOL_F;
  else if (SCM_UVEC_LENGTH (a) != SCM_UVEC_LENGTH (b))
    result = SCM_BOOL_F;
  else if (memcmp (SCM_UVEC_BASE (a), SCM_UVEC_BASE (b),
		   SCM_UVEC_LENGTH (a) * uvec_sizes[SCM_UVEC_TYPE(a)]) != 0)
    result = SCM_BOOL_F;

  scm_remember_upto_here_2 (a, b);
  return result;
}

/* Smob free hook for uniform numeric vectors. */
static size_t
uvec_free (SCM uvec)
{
  int type = SCM_UVEC_TYPE (uvec);
  scm_gc_free (SCM_UVEC_BASE (uvec),
	       SCM_UVEC_LENGTH (uvec) * uvec_sizes[type],
	       uvec_names[type]);
  return 0;
}

/* ================================================================ */
/* Utility procedures.                                              */
/* ================================================================ */

static SCM_C_INLINE_KEYWORD int
is_uvec (int type, SCM obj)
{
  if (SCM_IS_UVEC (obj))
    return SCM_UVEC_TYPE (obj) == type;
  if (SCM_I_ARRAYP (obj) && SCM_I_ARRAY_NDIM (obj) == 1)
    {
      SCM v = SCM_I_ARRAY_V (obj);
      return SCM_IS_UVEC (v) && SCM_UVEC_TYPE (v) == type;
    }
  return 0;
}

static SCM_C_INLINE_KEYWORD SCM
uvec_p (int type, SCM obj)
{
  return scm_from_bool (is_uvec (type, obj));
}

static SCM_C_INLINE_KEYWORD void
uvec_assert (int type, SCM obj)
{
  if (!is_uvec (type, obj))
    scm_wrong_type_arg_msg (NULL, 0, obj, uvec_names[type]);
}

static SCM
take_uvec (int type, void *base, size_t len)
{
  SCM_RETURN_NEWSMOB3 (scm_tc16_uvec, type, len, (scm_t_bits) base);
}
  
/* Create a new, uninitialized uniform numeric vector of type TYPE
   with space for LEN elements.  */
static SCM
alloc_uvec (int type, size_t len)
{
  void *base;
  if (len > SCM_I_SIZE_MAX / uvec_sizes[type])
    scm_out_of_range (NULL, scm_from_size_t (len));
  base = scm_gc_malloc (len * uvec_sizes[type], uvec_names[type]);
  return take_uvec (type, base, len);
}

/* GCC doesn't seem to want to optimize unused switch clauses away,
   so we use a big 'if' in the next two functions.
*/

static SCM_C_INLINE_KEYWORD SCM
uvec_fast_ref (int type, const void *base, size_t c_idx)
{
  if (type == SCM_UVEC_U8)
    return scm_from_uint8 (((scm_t_uint8*)base)[c_idx]);
  else if (type == SCM_UVEC_S8)
    return scm_from_int8 (((scm_t_int8*)base)[c_idx]);
  else if (type == SCM_UVEC_U16)
    return scm_from_uint16 (((scm_t_uint16*)base)[c_idx]);
  else if (type == SCM_UVEC_S16)
    return scm_from_int16 (((scm_t_int16*)base)[c_idx]);
  else if (type == SCM_UVEC_U32)
    return scm_from_uint32 (((scm_t_uint32*)base)[c_idx]);
  else if (type == SCM_UVEC_S32)
    return scm_from_int32 (((scm_t_int32*)base)[c_idx]);
  else if (type == SCM_UVEC_U64)
    return scm_from_uint64 (((scm_t_uint64*)base)[c_idx]);
  else if (type == SCM_UVEC_S64)
    return scm_from_int64 (((scm_t_int64*)base)[c_idx]);
  else if (type == SCM_UVEC_F32)
    return scm_from_double (((float*)base)[c_idx]);
  else if (type == SCM_UVEC_F64)
    return scm_from_double (((double*)base)[c_idx]);
  else if (type == SCM_UVEC_C32)
    return scm_c_make_rectangular (((float*)base)[2*c_idx],
				   ((float*)base)[2*c_idx+1]);
  else if (type == SCM_UVEC_C64)
    return scm_c_make_rectangular (((double*)base)[2*c_idx],
				   ((double*)base)[2*c_idx+1]);
  else
    return SCM_BOOL_F;
}

static SCM_C_INLINE_KEYWORD void
uvec_fast_set_x (int type, void *base, size_t c_idx, SCM val)
{
  if (type == SCM_UVEC_U8)
    (((scm_t_uint8*)base)[c_idx]) = scm_to_uint8 (val);
  else if (type == SCM_UVEC_S8)
    (((scm_t_int8*)base)[c_idx]) = scm_to_int8 (val);
  else if (type == SCM_UVEC_U16)
    (((scm_t_uint16*)base)[c_idx]) = scm_to_uint16 (val);
  else if (type == SCM_UVEC_S16)
    (((scm_t_int16*)base)[c_idx]) = scm_to_int16 (val);
  else if (type == SCM_UVEC_U32)
    (((scm_t_uint32*)base)[c_idx]) = scm_to_uint32 (val);
  else if (type == SCM_UVEC_S32)
    (((scm_t_int32*)base)[c_idx]) = scm_to_int32 (val);
  else if (type == SCM_UVEC_U64)
    (((scm_t_uint64*)base)[c_idx]) = scm_to_uint64 (val);
  else if (type == SCM_UVEC_S64)
    (((scm_t_int64*)base)[c_idx]) = scm_to_int64 (val);
  else if (type == SCM_UVEC_F32)
    (((float*)base)[c_idx]) = scm_to_double (val);
  else if (type == SCM_UVEC_F64)
    (((double*)base)[c_idx]) = scm_to_double (val);
  else if (type == SCM_UVEC_C32)
    {
      (((float*)base)[2*c_idx])   = scm_c_real_part (val);
      (((float*)base)[2*c_idx+1]) = scm_c_imag_part (val);
    }
  else if (type == SCM_UVEC_C64)
    {
      (((double*)base)[2*c_idx])   = scm_c_real_part (val);
      (((double*)base)[2*c_idx+1]) = scm_c_imag_part (val);
    }
}

static SCM_C_INLINE_KEYWORD SCM
make_uvec (int type, SCM len, SCM fill)
{
  size_t c_len = scm_to_size_t (len);
  SCM uvec = alloc_uvec (type, c_len);
  if (!SCM_UNBNDP (fill))
    {
      size_t idx;
      void *base = SCM_UVEC_BASE (uvec);
      for (idx = 0; idx < c_len; idx++)
	uvec_fast_set_x (type, base, idx, fill);
    }
  return uvec;
}

static SCM_C_INLINE_KEYWORD void *
uvec_writable_elements (int type, SCM uvec, scm_t_array_handle *handle,
			size_t *lenp, ssize_t *incp)
{
  if (type >= 0)
    {
      SCM v = uvec;
      if (SCM_I_ARRAYP (v))
	v = SCM_I_ARRAY_V (v);
      uvec_assert (type, v);
    }

  return scm_uniform_vector_writable_elements (uvec, handle, lenp, incp);
}

static SCM_C_INLINE_KEYWORD const void *
uvec_elements (int type, SCM uvec, scm_t_array_handle *handle,
	       size_t *lenp, ssize_t *incp)
{
  return uvec_writable_elements (type, uvec, handle, lenp, incp);
}

static int
uvec_type (scm_t_array_handle *h)
{
  SCM v = h->array;
  if (SCM_I_ARRAYP (v))
    v = SCM_I_ARRAY_V (v);
  return SCM_UVEC_TYPE (v);
}

static SCM
uvec_to_list (int type, SCM uvec)
{
  scm_t_array_handle handle;
  size_t len;
  ssize_t i, inc;
  const void *elts;
  SCM res = SCM_EOL;

  elts = uvec_elements (type, uvec, &handle, &len, &inc);
  for (i = len*inc; i > 0;)
    {
      i -= inc;
      res = scm_cons (scm_array_handle_ref (&handle, i), res);
    }
  scm_array_handle_release (&handle);
  return res;
}

static SCM_C_INLINE_KEYWORD SCM
uvec_length (int type, SCM uvec)
{
  scm_t_array_handle handle;
  size_t len;
  ssize_t inc;
  uvec_elements (type, uvec, &handle, &len, &inc);
  scm_array_handle_release (&handle);
  return scm_from_size_t (len);
}

static SCM_C_INLINE_KEYWORD SCM
uvec_ref (int type, SCM uvec, SCM idx)
{
  scm_t_array_handle handle;
  size_t i, len;
  ssize_t inc;
  const void *elts;
  SCM res;

  elts = uvec_elements (type, uvec, &handle, &len, &inc);
  if (type < 0)
    type = uvec_type (&handle);
  i = scm_to_unsigned_integer (idx, 0, len-1);
  res = uvec_fast_ref (type, elts, i*inc);
  scm_array_handle_release (&handle);
  return res;
}

static SCM_C_INLINE_KEYWORD SCM
uvec_set_x (int type, SCM uvec, SCM idx, SCM val)
{
  scm_t_array_handle handle;
  size_t i, len;
  ssize_t inc;
  void *elts;

  elts = uvec_writable_elements (type, uvec, &handle, &len, &inc);
  if (type < 0)
    type = uvec_type (&handle);
  i = scm_to_unsigned_integer (idx, 0, len-1);
  uvec_fast_set_x (type, elts, i*inc, val);
  scm_array_handle_release (&handle);
  return SCM_UNSPECIFIED;
}

static SCM_C_INLINE_KEYWORD SCM
list_to_uvec (int type, SCM list)
{
  SCM uvec;
  void *base;
  long idx;
  long len = scm_ilength (list);
  if (len < 0)
    scm_wrong_type_arg_msg (NULL, 0, list, "proper list");

  uvec = alloc_uvec (type, len);
  base = SCM_UVEC_BASE (uvec);
  idx = 0;
  while (scm_is_pair (list) && idx < len)
    {
      uvec_fast_set_x (type, base, idx, SCM_CAR (list));
      list = SCM_CDR (list);
      idx++;
    }
  return uvec;
}

static SCM
coerce_to_uvec (int type, SCM obj)
{
  if (is_uvec (type, obj))
    return obj;
  else if (scm_is_pair (obj))
    return list_to_uvec (type, obj);
  else if (scm_is_generalized_vector (obj))
    {
      scm_t_array_handle handle;
      size_t len = scm_c_generalized_vector_length (obj), i;
      SCM uvec = alloc_uvec (type, len);
      scm_array_get_handle (uvec, &handle);
      for (i = 0; i < len; i++)
	scm_array_handle_set (&handle, i,
			      scm_c_generalized_vector_ref (obj, i));
      scm_array_handle_release (&handle);
      return uvec;
    }
  else
    scm_wrong_type_arg_msg (NULL, 0, obj, "list or generalized vector");
}

SCM_SYMBOL (scm_sym_a, "a");
SCM_SYMBOL (scm_sym_b, "b");

SCM
scm_i_generalized_vector_type (SCM v)
{
  if (scm_is_vector (v))
    return SCM_BOOL_T;
  else if (scm_is_string (v))
    return scm_sym_a;
  else if (scm_is_bitvector (v))
    return scm_sym_b;
  else if (scm_is_uniform_vector (v))
    return scm_from_locale_symbol (uvec_tags[SCM_UVEC_TYPE(v)]);
  else
    return SCM_BOOL_F;
}

int
scm_is_uniform_vector (SCM obj)
{
  if (SCM_IS_UVEC (obj))
    return 1;
  if (SCM_I_ARRAYP (obj) && SCM_I_ARRAY_NDIM (obj) == 1)
    {
      SCM v = SCM_I_ARRAY_V (obj);
      return SCM_IS_UVEC (v);
    }
  return 0;
}

size_t
scm_c_uniform_vector_length (SCM uvec)
{
  /* scm_generalized_vector_get_handle will ultimately call us to get
     the length of uniform vectors, so we can't use uvec_elements for
     naked vectors.
  */

  if (SCM_IS_UVEC (uvec))
    return SCM_UVEC_LENGTH (uvec);
  else
    {
      scm_t_array_handle handle;
      size_t len;
      ssize_t inc;
      uvec_elements (-1, uvec, &handle, &len, &inc);
      scm_array_handle_release (&handle);
      return len;
    }
}

SCM_DEFINE (scm_uniform_vector_p, "uniform-vector?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a uniform vector.")
#define FUNC_NAME s_scm_uniform_vector_p
{
  return scm_from_bool (scm_is_uniform_vector (obj));
}
#undef FUNC_NAME

SCM
scm_c_uniform_vector_ref (SCM v, size_t idx)
{
  scm_t_array_handle handle;
  size_t len;
  ssize_t inc;
  SCM res;

  uvec_elements (-1, v, &handle, &len, &inc);
  if (idx >= len)
    scm_out_of_range (NULL, scm_from_size_t (idx));
  res = scm_array_handle_ref (&handle, idx*inc);
  scm_array_handle_release (&handle);
  return res;
}

SCM_DEFINE (scm_uniform_vector_ref, "uniform-vector-ref", 2, 0, 0,
	    (SCM v, SCM idx),
	    "Return the element at index @var{idx} of the\n"
	    "homogenous numeric vector @var{v}.")
#define FUNC_NAME s_scm_uniform_vector_ref
{
#if SCM_ENABLE_DEPRECATED
  /* Support old argument convention.
   */
  if (scm_is_pair (idx))
    {
      scm_c_issue_deprecation_warning
	("Using a list as the index to uniform-vector-ref is deprecated.");
      if (!scm_is_null (SCM_CDR (idx)))
	scm_wrong_num_args (NULL);
      idx = SCM_CAR (idx);
    }
#endif

  return scm_c_uniform_vector_ref (v, scm_to_size_t (idx));
}
#undef FUNC_NAME

void
scm_c_uniform_vector_set_x (SCM v, size_t idx, SCM val)
{
  scm_t_array_handle handle;
  size_t len;
  ssize_t inc;

  uvec_writable_elements (-1, v, &handle, &len, &inc);
  if (idx >= len)
    scm_out_of_range (NULL, scm_from_size_t (idx));
  scm_array_handle_set (&handle, idx*inc, val);
  scm_array_handle_release (&handle);
}

SCM_DEFINE (scm_uniform_vector_set_x, "uniform-vector-set!", 3, 0, 0,
	    (SCM v, SCM idx, SCM val),
	    "Set the element at index @var{idx} of the\n"
	    "homogenous numeric vector @var{v} to @var{val}.")
#define FUNC_NAME s_scm_uniform_vector_set_x
{
#if SCM_ENABLE_DEPRECATED
  /* Support old argument convention.
   */
  if (scm_is_pair (idx))
    {
      scm_c_issue_deprecation_warning
	("Using a list as the index to uniform-vector-set! is deprecated.");
      if (!scm_is_null (SCM_CDR (idx)))
	scm_wrong_num_args (NULL);
      idx = SCM_CAR (idx);
    }
#endif

  scm_c_uniform_vector_set_x (v, scm_to_size_t (idx), val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_uniform_vector_to_list, "uniform-vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the uniform numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_uniform_vector_to_list
{
  return uvec_to_list (-1, uvec);
}
#undef FUNC_NAME

size_t
scm_array_handle_uniform_element_size (scm_t_array_handle *h)
{
  SCM vec = h->array;
  if (SCM_I_ARRAYP (vec))
    vec = SCM_I_ARRAY_V (vec);
  if (scm_is_uniform_vector (vec))
    return uvec_sizes[SCM_UVEC_TYPE(vec)];
  scm_wrong_type_arg_msg (NULL, 0, h->array, "uniform array");
}

#if SCM_ENABLE_DEPRECATED
 
/* return the size of an element in a uniform array or 0 if type not
   found.  */
size_t
scm_uniform_element_size (SCM obj)
{
  scm_c_issue_deprecation_warning 
    ("scm_uniform_element_size is deprecated.  "
     "Use scm_array_handle_uniform_element_size instead.");

  if (SCM_IS_UVEC (obj))
    return uvec_sizes[SCM_UVEC_TYPE(obj)];
  else
    return 0;
}

#endif

const void *
scm_array_handle_uniform_elements (scm_t_array_handle *h)
{
  return scm_array_handle_uniform_writable_elements (h);
}

void *
scm_array_handle_uniform_writable_elements (scm_t_array_handle *h)
{
  SCM vec = h->array;
  if (SCM_I_ARRAYP (vec))
    vec = SCM_I_ARRAY_V (vec);
  if (SCM_IS_UVEC (vec))
    {
      size_t size = uvec_sizes[SCM_UVEC_TYPE(vec)];
      char *elts = SCM_UVEC_BASE (vec);
      return (void *) (elts + size*h->base);
    }
  scm_wrong_type_arg_msg (NULL, 0, h->array, "uniform array");
}

const void *
scm_uniform_vector_elements (SCM uvec, 
			     scm_t_array_handle *h,
			     size_t *lenp, ssize_t *incp)
{
  return scm_uniform_vector_writable_elements (uvec, h, lenp, incp);
}

void *
scm_uniform_vector_writable_elements (SCM uvec, 
				      scm_t_array_handle *h,
				      size_t *lenp, ssize_t *incp)
{
  scm_generalized_vector_get_handle (uvec, h);
  if (lenp)
    {
      scm_t_array_dim *dim = scm_array_handle_dims (h);
      *lenp = dim->ubnd - dim->lbnd + 1;
      *incp = dim->inc;
    }
  return scm_array_handle_uniform_writable_elements (h);
}

SCM_DEFINE (scm_uniform_vector_length, "uniform-vector-length", 1, 0, 0, 
	    (SCM v),
	    "Return the number of elements in the uniform vector @var{v}.")
#define FUNC_NAME s_scm_uniform_vector_length
{
  return uvec_length (-1, v);
}
#undef FUNC_NAME

SCM_DEFINE (scm_uniform_vector_read_x, "uniform-vector-read!", 1, 3, 0,
           (SCM uvec, SCM port_or_fd, SCM start, SCM end),
	    "Fill the elements of @var{uvec} by reading\n"
	    "raw bytes from @var{port-or-fdes}, using host byte order.\n\n"
	    "The optional arguments @var{start} (inclusive) and @var{end}\n"
	    "(exclusive) allow a specified region to be read,\n"
	    "leaving the remainder of the vector unchanged.\n\n"
	    "When @var{port-or-fdes} is a port, all specified elements\n"
	    "of @var{uvec} are attempted to be read, potentially blocking\n"
	    "while waiting formore input or end-of-file.\n"
	    "When @var{port-or-fd} is an integer, a single call to\n"
	    "read(2) is made.\n\n"
	    "An error is signalled when the last element has only\n"
	    "been partially filled before reaching end-of-file or in\n"
	    "the single call to read(2).\n\n"
	    "@code{uniform-vector-read!} returns the number of elements\n"
	    "read.\n\n"
	    "@var{port-or-fdes} may be omitted, in which case it defaults\n"
	    "to the value returned by @code{(current-input-port)}.")
#define FUNC_NAME s_scm_uniform_vector_read_x
{
  scm_t_array_handle handle;
  size_t vlen, sz, ans;
  ssize_t inc;
  size_t cstart, cend;
  size_t remaining, off;
  char *base;

  if (SCM_UNBNDP (port_or_fd))
    port_or_fd = scm_current_input_port ();
  else
    SCM_ASSERT (scm_is_integer (port_or_fd)
		|| (SCM_OPINPORTP (port_or_fd)),
		port_or_fd, SCM_ARG2, FUNC_NAME);

  if (!scm_is_uniform_vector (uvec))
    scm_wrong_type_arg_msg (NULL, 0, uvec, "uniform vector");

  base = scm_uniform_vector_writable_elements (uvec, &handle, &vlen, &inc);
  sz = scm_array_handle_uniform_element_size (&handle);

  if (inc != 1)
    {
      /* XXX - we should of course support non contiguous vectors. */
      scm_misc_error (NULL, "only contiguous vectors are supported: ~a",
		      scm_list_1 (uvec));
    }

  cstart = 0;
  cend = vlen;
  if (!SCM_UNBNDP (start))
    {
      cstart = scm_to_unsigned_integer (start, 0, vlen);
      if (!SCM_UNBNDP (end))
	cend = scm_to_unsigned_integer (end, cstart, vlen);
    }

  remaining = (cend - cstart) * sz;
  off = cstart * sz;

  if (SCM_NIMP (port_or_fd))
    {
      ans = cend - cstart;
      remaining -= scm_c_read (port_or_fd, base + off, remaining);
      if (remaining % sz != 0)
        SCM_MISC_ERROR ("unexpected EOF", SCM_EOL);
      ans -= remaining / sz;
    }
  else /* file descriptor.  */
    {
      int fd = scm_to_int (port_or_fd);
      int n;

      SCM_SYSCALL (n = read (fd, base + off, remaining));
      if (n == -1)
	SCM_SYSERROR;
      if (n % sz != 0)
	SCM_MISC_ERROR ("unexpected EOF", SCM_EOL);
      ans = n / sz;
    }

  scm_array_handle_release (&handle);

  return scm_from_size_t (ans);
}
#undef FUNC_NAME

SCM_DEFINE (scm_uniform_vector_write, "uniform-vector-write", 1, 3, 0,
           (SCM uvec, SCM port_or_fd, SCM start, SCM end),
	    "Write the elements of @var{uvec} as raw bytes to\n"
	    "@var{port-or-fdes}, in the host byte order.\n\n"
	    "The optional arguments @var{start} (inclusive)\n"
	    "and @var{end} (exclusive) allow\n"
	    "a specified region to be written.\n\n"
	    "When @var{port-or-fdes} is a port, all specified elements\n"
	    "of @var{uvec} are attempted to be written, potentially blocking\n"
	    "while waiting for more room.\n"
	    "When @var{port-or-fd} is an integer, a single call to\n"
	    "write(2) is made.\n\n"
	    "An error is signalled when the last element has only\n"
	    "been partially written in the single call to write(2).\n\n"
	    "The number of objects actually written is returned.\n"
	    "@var{port-or-fdes} may be\n"
	    "omitted, in which case it defaults to the value returned by\n"
	    "@code{(current-output-port)}.")
#define FUNC_NAME s_scm_uniform_vector_write
{
  scm_t_array_handle handle;
  size_t vlen, sz, ans;
  ssize_t inc;
  size_t cstart, cend;
  size_t amount, off;
  const char *base;

  port_or_fd = SCM_COERCE_OUTPORT (port_or_fd);

  if (SCM_UNBNDP (port_or_fd))
    port_or_fd = scm_current_output_port ();
  else
    SCM_ASSERT (scm_is_integer (port_or_fd)
		|| (SCM_OPOUTPORTP (port_or_fd)),
		port_or_fd, SCM_ARG2, FUNC_NAME);

  base = scm_uniform_vector_elements (uvec, &handle, &vlen, &inc);
  sz = scm_array_handle_uniform_element_size (&handle);

  if (inc != 1)
    {
      /* XXX - we should of course support non contiguous vectors. */
      scm_misc_error (NULL, "only contiguous vectors are supported: ~a",
		      scm_list_1 (uvec));
    }

  cstart = 0;
  cend = vlen;
  if (!SCM_UNBNDP (start))
    {
      cstart = scm_to_unsigned_integer (start, 0, vlen);
      if (!SCM_UNBNDP (end))
	cend = scm_to_unsigned_integer (end, cstart, vlen);
    }

  amount = (cend - cstart) * sz;
  off = cstart * sz;

  if (SCM_NIMP (port_or_fd))
    {
      scm_lfwrite (base + off, amount, port_or_fd);
      ans = cend - cstart;
    }
  else /* file descriptor.  */
    {
      int fd = scm_to_int (port_or_fd), n;
      SCM_SYSCALL (n = write (fd, base + off, amount));
      if (n == -1)
	SCM_SYSERROR;
      if (n % sz != 0)
	SCM_MISC_ERROR ("last element only written partially", SCM_EOL);
      ans = n / sz;
    }

  scm_array_handle_release (&handle);

  return scm_from_size_t (ans);
}
#undef FUNC_NAME

/* ================================================================ */
/* Exported procedures.                                             */
/* ================================================================ */

#define TYPE  SCM_UVEC_U8
#define TAG   u8
#define CTYPE scm_t_uint8
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_S8
#define TAG   s8
#define CTYPE scm_t_int8
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_U16
#define TAG   u16
#define CTYPE scm_t_uint16
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_S16
#define TAG   s16
#define CTYPE scm_t_int16
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_U32
#define TAG   u32
#define CTYPE scm_t_uint32
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_S32
#define TAG   s32
#define CTYPE scm_t_int32
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_U64
#define TAG   u64
#define CTYPE scm_t_uint64
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_S64
#define TAG   s64
#define CTYPE scm_t_int64
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_F32
#define TAG   f32
#define CTYPE float
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_F64
#define TAG   f64
#define CTYPE double
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_C32
#define TAG   c32
#define CTYPE float
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_C64
#define TAG   c64
#define CTYPE double
#include "libguile/srfi-4.i.c"

static scm_i_t_array_ref uvec_reffers[12] = {
  u8ref, s8ref,
  u16ref, s16ref,
  u32ref, s32ref,
  u64ref, s64ref,
  f32ref, f64ref,
  c32ref, c64ref
};

static scm_i_t_array_set uvec_setters[12] = {
  u8set, s8set,
  u16set, s16set,
  u32set, s32set,
  u64set, s64set,
  f32set, f64set,
  c32set, c64set
};

scm_i_t_array_ref
scm_i_uniform_vector_ref_proc (SCM uvec)
{
  return uvec_reffers[SCM_UVEC_TYPE(uvec)];
}

scm_i_t_array_set
scm_i_uniform_vector_set_proc (SCM uvec)
{
  return uvec_setters[SCM_UVEC_TYPE(uvec)];
}

void
scm_init_srfi_4 (void)
{
  scm_tc16_uvec = scm_make_smob_type ("uvec", 0);
  scm_set_smob_equalp (scm_tc16_uvec, uvec_equalp);
  scm_set_smob_free (scm_tc16_uvec, uvec_free);
  scm_set_smob_print (scm_tc16_uvec, uvec_print);

#include "libguile/srfi-4.x"

}

/* End of srfi-4.c.  */
