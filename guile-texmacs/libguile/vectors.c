/* Copyright (C) 1995,1996,1998,1999,2000,2001, 2006, 2008, 2010 Free Software Foundation, Inc.
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
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/lang.h"

#include "libguile/validate.h"
#include "libguile/vectors.h"
#include "libguile/unif.h"
#include "libguile/ramap.h"
#include "libguile/srfi-4.h"
#include "libguile/strings.h"
#include "libguile/srfi-13.h"
#include "libguile/dynwind.h"
#include "libguile/deprecation.h"



#define VECTOR_MAX_LENGTH (SCM_T_BITS_MAX >> 8)

int
scm_is_vector (SCM obj)
{
  if (SCM_I_IS_VECTOR (obj))
    return 1;
  if  (SCM_I_ARRAYP (obj) && SCM_I_ARRAY_NDIM (obj) == 1)
    {
      SCM v = SCM_I_ARRAY_V (obj);
      return SCM_I_IS_VECTOR (v);
    }
  return 0;
}

int
scm_is_simple_vector (SCM obj)
{
  return SCM_I_IS_VECTOR (obj);
}

const SCM *
scm_vector_elements (SCM vec, scm_t_array_handle *h,
		     size_t *lenp, ssize_t *incp)
{
  scm_generalized_vector_get_handle (vec, h);
  if (lenp)
    {
      scm_t_array_dim *dim = scm_array_handle_dims (h);
      *lenp = dim->ubnd - dim->lbnd + 1;
      *incp = dim->inc;
    }
  return scm_array_handle_elements (h);
}

SCM *
scm_vector_writable_elements (SCM vec, scm_t_array_handle *h,
			      size_t *lenp, ssize_t *incp)
{
  scm_generalized_vector_get_handle (vec, h);
  if (lenp)
    {
      scm_t_array_dim *dim = scm_array_handle_dims (h);
      *lenp = dim->ubnd - dim->lbnd + 1;
      *incp = dim->inc;
    }
  return scm_array_handle_writable_elements (h);
}

SCM_DEFINE (scm_vector_p, "vector?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector, otherwise return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_vector_p
{
  return scm_from_bool (scm_is_vector (obj));
}
#undef FUNC_NAME

SCM_GPROC (s_vector_length, "vector-length", 1, 0, 0, scm_vector_length, g_vector_length);
/* Returns the number of elements in @var{vector} as an exact integer.  */
SCM
scm_vector_length (SCM v)
{
  if (SCM_I_IS_VECTOR (v))
    return scm_from_size_t (SCM_I_VECTOR_LENGTH (v));
  else if (SCM_I_ARRAYP (v) && SCM_I_ARRAY_NDIM (v) == 1)
    {
      scm_t_array_dim *dim = SCM_I_ARRAY_DIMS (v);
      return scm_from_size_t (dim->ubnd - dim->lbnd + 1);
    }
  else
    SCM_WTA_DISPATCH_1 (g_vector_length, v, 1, NULL);
}

size_t
scm_c_vector_length (SCM v)
{
  if (SCM_I_IS_VECTOR (v))
    return SCM_I_VECTOR_LENGTH (v);
  else
    return scm_to_size_t (scm_vector_length (v));
}

SCM_REGISTER_PROC (s_list_to_vector, "list->vector", 1, 0, 0, scm_vector);
/*
	    "Return a newly created vector initialized to the elements of"
	    "the list @var{list}.\n\n"
	    "@lisp\n"
	    "(vector->list '#(dah dah didah)) @result{} (dah dah didah)\n"
	    "(list->vector '(dididit dah)) @result{}   #(dididit dah)\n"
	    "@end lisp")
*/
SCM_DEFINE (scm_vector, "vector", 0, 0, 1, 
	    (SCM l),
	    "@deffnx {Scheme Procedure} list->vector l\n"
	    "Return a newly allocated vector composed of the\n"
	    "given arguments.  Analogous to @code{list}.\n"
	    "\n"
	    "@lisp\n"
	    "(vector 'a 'b 'c) @result{} #(a b c)\n"
	    "@end lisp")
#define FUNC_NAME s_scm_vector
{
  SCM res;
  SCM *data;
  long i, len;
  scm_t_array_handle handle;

  SCM_VALIDATE_LIST_COPYLEN (1, l, len);

  res = scm_c_make_vector (len, SCM_UNSPECIFIED);
  data = scm_vector_writable_elements (res, &handle, NULL, NULL);
  i = 0;
  while (scm_is_pair (l) && i < len) 
    {
      data[i] = SCM_CAR (l);
      l = SCM_CDR (l);
      i += 1;
    }

  scm_array_handle_release (&handle);

  return res;
}
#undef FUNC_NAME

SCM_GPROC (s_vector_ref, "vector-ref", 2, 0, 0, scm_vector_ref, g_vector_ref);

/*
           "@var{k} must be a valid index of @var{vector}.\n"
	   "@samp{Vector-ref} returns the contents of element @var{k} of\n"
	   "@var{vector}.\n\n"
	   "@lisp\n"
	   "(vector-ref '#(1 1 2 3 5 8 13 21) 5) @result{} 8\n"
	   "(vector-ref '#(1 1 2 3 5 8 13 21)\n"
	   "    (let ((i (round (* 2 (acos -1)))))\n"
	   "      (if (inexact? i)\n"
	   "        (inexact->exact i)\n"
	   "           i))) @result{} 13\n"
	   "@end lisp"
*/

SCM
scm_vector_ref (SCM v, SCM k)
#define FUNC_NAME s_vector_ref
{
  return scm_c_vector_ref (v, scm_to_size_t (k));
}
#undef FUNC_NAME

SCM
scm_c_vector_ref (SCM v, size_t k)
{
  if (SCM_I_IS_VECTOR (v))
    {
      if (k >= SCM_I_VECTOR_LENGTH (v))
	scm_out_of_range (NULL, scm_from_size_t (k)); 
      return (SCM_I_VECTOR_ELTS(v))[k];
    }
  else if (SCM_I_ARRAYP (v) && SCM_I_ARRAY_NDIM (v) == 1)
    {
      scm_t_array_dim *dim = SCM_I_ARRAY_DIMS (v);
      SCM vv = SCM_I_ARRAY_V (v);
      if (SCM_I_IS_VECTOR (vv))
	{
	  if (k >= dim->ubnd - dim->lbnd + 1)
	    scm_out_of_range (NULL, scm_from_size_t (k));
	  k = SCM_I_ARRAY_BASE (v) + k*dim->inc;
	  return (SCM_I_VECTOR_ELTS (vv))[k];
	}
      scm_wrong_type_arg_msg (NULL, 0, v, "non-uniform vector");
    }
  else
    SCM_WTA_DISPATCH_2 (g_vector_ref, v, scm_from_size_t (k), 2, NULL);
}

SCM_GPROC (s_vector_set_x, "vector-set!", 3, 0, 0, scm_vector_set_x, g_vector_set_x);

/* "@var{k} must be a valid index of @var{vector}.\n"
   "@code{Vector-set!} stores @var{obj} in element @var{k} of @var{vector}.\n"
   "The value returned by @samp{vector-set!} is unspecified.\n"
   "@lisp\n"
   "(let ((vec (vector 0 '(2 2 2 2) "Anna")))\n"
   "  (vector-set! vec 1 '("Sue" "Sue"))\n"
   "  vec) @result{}  #(0 ("Sue" "Sue") "Anna")\n"
   "(vector-set! '#(0 1 2) 1 "doe") @result{} @emph{error} ; constant vector\n"
   "@end lisp"
*/

SCM
scm_vector_set_x (SCM v, SCM k, SCM obj)
#define FUNC_NAME s_vector_set_x
{
  scm_c_vector_set_x (v, scm_to_size_t (k), obj);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_c_vector_set_x (SCM v, size_t k, SCM obj)
{
  if (SCM_I_IS_VECTOR (v))
    {
      if (k >= SCM_I_VECTOR_LENGTH (v))
	scm_out_of_range (NULL, scm_from_size_t (k)); 
      (SCM_I_VECTOR_WELTS(v))[k] = obj;
    }
  else if (SCM_I_ARRAYP (v) && SCM_I_ARRAY_NDIM (v) == 1)
    {
      scm_t_array_dim *dim = SCM_I_ARRAY_DIMS (v);
      SCM vv = SCM_I_ARRAY_V (v);
      if (SCM_I_IS_VECTOR (vv))
	{
	  if (k >= dim->ubnd - dim->lbnd + 1)
	    scm_out_of_range (NULL, scm_from_size_t (k));
	  k = SCM_I_ARRAY_BASE (v) + k*dim->inc;
	  (SCM_I_VECTOR_WELTS (vv))[k] = obj;
	}
      else
	scm_wrong_type_arg_msg (NULL, 0, v, "non-uniform vector");
    }
  else
    {
      if (SCM_UNPACK (g_vector_set_x))
	scm_apply_generic (g_vector_set_x,
			   scm_list_3 (v, scm_from_size_t (k), obj));
      else
	scm_wrong_type_arg_msg (NULL, 0, v, "vector");
    }
}

SCM_DEFINE (scm_make_vector, "make-vector", 1, 1, 0,
            (SCM k, SCM fill),
	    "Return a newly allocated vector of @var{k} elements.  If a\n"
	    "second argument is given, then each position is initialized to\n"
	    "@var{fill}.  Otherwise the initial contents of each position is\n"
	    "unspecified.")
#define FUNC_NAME s_scm_make_vector
{
  size_t l = scm_to_unsigned_integer (k, 0, VECTOR_MAX_LENGTH);

  if (SCM_UNBNDP (fill))
    fill = SCM_UNSPECIFIED;
  
  return scm_c_make_vector (l, fill);
}
#undef FUNC_NAME


SCM
scm_c_make_vector (size_t k, SCM fill)
#define FUNC_NAME s_scm_make_vector
{
  SCM v;
  SCM *base;

  if (k > 0) 
    {
      unsigned long int j;

      SCM_ASSERT_RANGE (1, scm_from_ulong (k), k <= VECTOR_MAX_LENGTH);

      base = scm_gc_malloc (k * sizeof (SCM), "vector");
      for (j = 0; j != k; ++j)
	base[j] = fill;
    }
  else
    base = NULL;

  v = scm_cell ((k << 8) | scm_tc7_vector, (scm_t_bits) base);
  scm_remember_upto_here_1 (fill);

  return v;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vector_copy, "vector-copy", 1, 0, 0,
	    (SCM vec),
	    "Return a copy of @var{vec}.")
#define FUNC_NAME s_scm_vector_copy
{
  scm_t_array_handle handle;
  size_t i, len;
  ssize_t inc;
  const SCM *src;
  SCM *dst;

  src = scm_vector_elements (vec, &handle, &len, &inc);
  dst = scm_gc_malloc (len * sizeof (SCM), "vector");
  for (i = 0; i < len; i++, src += inc)
    dst[i] = *src;
  scm_array_handle_release (&handle);

  return scm_cell ((len << 8) | scm_tc7_vector, (scm_t_bits) dst);
}
#undef FUNC_NAME

void
scm_i_vector_free (SCM vec)
{
  scm_gc_free (SCM_I_VECTOR_WELTS (vec),
	       SCM_I_VECTOR_LENGTH (vec) * sizeof(SCM),
	       "vector");
}

/* Allocate memory for a weak vector on behalf of the caller.  The allocated
 * vector will be of the given weak vector subtype.  It will contain size
 * elements which are initialized with the 'fill' object, or, if 'fill' is
 * undefined, with an unspecified object.
 */
SCM
scm_i_allocate_weak_vector (scm_t_bits type, SCM size, SCM fill)
{
  size_t c_size;
  SCM *base;
  SCM v;

  c_size = scm_to_unsigned_integer (size, 0, VECTOR_MAX_LENGTH);

  if (c_size > 0)
    {
      size_t j;
      
      if (SCM_UNBNDP (fill))
	fill = SCM_UNSPECIFIED;
      
      base = scm_gc_malloc (c_size * sizeof (SCM), "weak vector");
      for (j = 0; j != c_size; ++j)
	base[j] = fill;
    }
  else
    base = NULL;

  v = scm_double_cell ((c_size << 8) | scm_tc7_wvect,
		       (scm_t_bits) base,
		       type,
		       SCM_UNPACK (SCM_EOL));
  scm_remember_upto_here_1 (fill);

  return v;
}

SCM_DEFINE (scm_vector_to_list, "vector->list", 1, 0, 0, 
	    (SCM v),
	    "Return a newly allocated list composed of the elements of @var{v}.\n"
	    "\n"
	    "@lisp\n"
	    "(vector->list '#(dah dah didah)) @result{}  (dah dah didah)\n"
	    "(list->vector '(dididit dah)) @result{}  #(dididit dah)\n"
	    "@end lisp")
#define FUNC_NAME s_scm_vector_to_list
{
  SCM res = SCM_EOL;
  const SCM *data;
  scm_t_array_handle handle;
  size_t i, count, len;
  ssize_t inc;

  data = scm_vector_elements (v, &handle, &len, &inc);
  for (i = (len - 1) * inc, count = 0;
       count < len;
       i -= inc, count++)
    res = scm_cons (data[i], res);

  scm_array_handle_release (&handle);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_vector_fill_x, "vector-fill!", 2, 0, 0,
            (SCM v, SCM fill),
	    "Store @var{fill} in every position of @var{vector}.  The value\n"
	    "returned by @code{vector-fill!} is unspecified.")
#define FUNC_NAME s_scm_vector_fill_x
{
  scm_t_array_handle handle;
  SCM *data;
  size_t i, len;
  ssize_t inc;

  data = scm_vector_writable_elements (v, &handle, &len, &inc);
  for (i = 0; i < len; i += inc)
    data[i] = fill;
  scm_array_handle_release (&handle);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM
scm_i_vector_equal_p (SCM x, SCM y)
{
  long i;
  for (i = SCM_I_VECTOR_LENGTH (x) - 1; i >= 0; i--)
    if (scm_is_false (scm_equal_p (SCM_I_VECTOR_ELTS (x)[i],
				   SCM_I_VECTOR_ELTS (y)[i])))
      return SCM_BOOL_F;
  return SCM_BOOL_T;
}


SCM_DEFINE (scm_vector_move_left_x, "vector-move-left!", 5, 0, 0, 
            (SCM vec1, SCM start1, SCM end1, SCM vec2, SCM start2),
	    "Copy elements from @var{vec1}, positions @var{start1} to @var{end1},\n"
	    "to @var{vec2} starting at position @var{start2}.  @var{start1} and\n"
	    "@var{start2} are inclusive indices; @var{end1} is exclusive.\n\n"
	    "@code{vector-move-left!} copies elements in leftmost order.\n"
	    "Therefore, in the case where @var{vec1} and @var{vec2} refer to the\n"
	    "same vector, @code{vector-move-left!} is usually appropriate when\n"
	    "@var{start1} is greater than @var{start2}.")
#define FUNC_NAME s_scm_vector_move_left_x
{
  scm_t_array_handle handle1, handle2;
  const SCM *elts1;
  SCM *elts2;
  size_t len1, len2;
  ssize_t inc1, inc2;
  size_t i, j, e;
  
  elts1 = scm_vector_elements (vec1, &handle1, &len1, &inc1);
  elts2 = scm_vector_writable_elements (vec2, &handle2, &len2, &inc2);

  i = scm_to_unsigned_integer (start1, 0, len1);
  e = scm_to_unsigned_integer (end1, i, len1);
  SCM_ASSERT_RANGE (SCM_ARG3, end1, (e-i) < len2);
  j = scm_to_unsigned_integer (start2, 0, len2);
  SCM_ASSERT_RANGE (SCM_ARG5, start2, j <= len2 - (e - i));
  
  i *= inc1;
  e *= inc1;
  j *= inc2;
  for (; i < e; i += inc1, j += inc2)
    elts2[j] = elts1[i];

  scm_array_handle_release (&handle2);
  scm_array_handle_release (&handle1);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vector_move_right_x, "vector-move-right!", 5, 0, 0, 
            (SCM vec1, SCM start1, SCM end1, SCM vec2, SCM start2),
	    "Copy elements from @var{vec1}, positions @var{start1} to @var{end1},\n"
	    "to @var{vec2} starting at position @var{start2}.  @var{start1} and\n"
	    "@var{start2} are inclusive indices; @var{end1} is exclusive.\n\n"
	    "@code{vector-move-right!} copies elements in rightmost order.\n"
	    "Therefore, in the case where @var{vec1} and @var{vec2} refer to the\n"
	    "same vector, @code{vector-move-right!} is usually appropriate when\n"
	    "@var{start1} is less than @var{start2}.")
#define FUNC_NAME s_scm_vector_move_right_x
{
  scm_t_array_handle handle1, handle2;
  const SCM *elts1;
  SCM *elts2;
  size_t len1, len2;
  ssize_t inc1, inc2;
  size_t i, j, e;
  
  elts1 = scm_vector_elements (vec1, &handle1, &len1, &inc1);
  elts2 = scm_vector_writable_elements (vec2, &handle2, &len2, &inc2);

  i = scm_to_unsigned_integer (start1, 0, len1);
  e = scm_to_unsigned_integer (end1, i, len1);
  SCM_ASSERT_RANGE (SCM_ARG3, end1, (e-i) < len2);
  j = scm_to_unsigned_integer (start2, 0, len2);
  SCM_ASSERT_RANGE (SCM_ARG5, start2, j <= len2 - (e - i));
  
  j += (e - i);
  
  i *= inc1;
  e *= inc1;
  j *= inc2;
  while (i < e)
    {
      e -= inc1;
      j -= inc2;
      elts2[j] = elts1[e];
    }

  scm_array_handle_release (&handle2);
  scm_array_handle_release (&handle1);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* Generalized vectors. */

int
scm_is_generalized_vector (SCM obj)
{
  return (scm_is_vector (obj)
	  || scm_is_string (obj)
	  || scm_is_bitvector (obj)
	  || scm_is_uniform_vector (obj));
}

SCM_DEFINE (scm_generalized_vector_p, "generalized-vector?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector, string,\n"
	    "bitvector, or uniform numeric vector.")
#define FUNC_NAME s_scm_generalized_vector_p
{
  return scm_from_bool (scm_is_generalized_vector (obj));
}
#undef FUNC_NAME

void
scm_generalized_vector_get_handle (SCM vec, scm_t_array_handle *h)
{
  scm_array_get_handle (vec, h);
  if (scm_array_handle_rank (h) != 1)
    scm_wrong_type_arg_msg (NULL, 0, vec, "vector");
}

size_t
scm_c_generalized_vector_length (SCM v)
{
  if (scm_is_vector (v))
    return scm_c_vector_length (v);
  else if (scm_is_string (v))
    return scm_c_string_length (v);
  else if (scm_is_bitvector (v))
    return scm_c_bitvector_length (v);
  else if (scm_is_uniform_vector (v))
    return scm_c_uniform_vector_length (v);
  else
    scm_wrong_type_arg_msg (NULL, 0, v, "generalized vector");
}

SCM_DEFINE (scm_generalized_vector_length, "generalized-vector-length", 1, 0, 0,
	    (SCM v),
	    "Return the length of the generalized vector @var{v}.")
#define FUNC_NAME s_scm_generalized_vector_length
{
  return scm_from_size_t (scm_c_generalized_vector_length (v));
}
#undef FUNC_NAME

SCM
scm_c_generalized_vector_ref (SCM v, size_t idx)
{
  if (scm_is_vector (v))
    return scm_c_vector_ref (v, idx);
  else if (scm_is_string (v))
    return scm_c_string_ref (v, idx);
  else if (scm_is_bitvector (v))
    return scm_c_bitvector_ref (v, idx);
  else if (scm_is_uniform_vector (v))
    return scm_c_uniform_vector_ref (v, idx);
  else
    scm_wrong_type_arg_msg (NULL, 0, v, "generalized vector");
}

SCM_DEFINE (scm_generalized_vector_ref, "generalized-vector-ref", 2, 0, 0,
	    (SCM v, SCM idx),
	    "Return the element at index @var{idx} of the\n"
	    "generalized vector @var{v}.")
#define FUNC_NAME s_scm_generalized_vector_ref
{
  return scm_c_generalized_vector_ref (v, scm_to_size_t (idx));
}
#undef FUNC_NAME

void
scm_c_generalized_vector_set_x (SCM v, size_t idx, SCM val)
{
  if (scm_is_vector (v))
    scm_c_vector_set_x (v, idx, val);
  else if (scm_is_string (v))
    scm_c_string_set_x (v, idx, val);
  else if (scm_is_bitvector (v))
    scm_c_bitvector_set_x (v, idx, val);
  else if (scm_is_uniform_vector (v))
    scm_c_uniform_vector_set_x (v, idx, val);
  else
    scm_wrong_type_arg_msg (NULL, 0, v, "generalized vector");
}

SCM_DEFINE (scm_generalized_vector_set_x, "generalized-vector-set!", 3, 0, 0,
	    (SCM v, SCM idx, SCM val),
	    "Set the element at index @var{idx} of the\n"
	    "generalized vector @var{v} to @var{val}.")
#define FUNC_NAME s_scm_generalized_vector_set_x
{
  scm_c_generalized_vector_set_x (v, scm_to_size_t (idx), val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_generalized_vector_to_list, "generalized-vector->list", 1, 0, 0,
	    (SCM v),
	    "Return a new list whose elements are the elements of the\n"
	    "generalized vector @var{v}.")
#define FUNC_NAME s_scm_generalized_vector_to_list
{
  if (scm_is_vector (v))
    return scm_vector_to_list (v);
  else if (scm_is_string (v))
    return scm_string_to_list (v);
  else if (scm_is_bitvector (v))
    return scm_bitvector_to_list (v);
  else if (scm_is_uniform_vector (v))
    return scm_uniform_vector_to_list (v);
  else
    scm_wrong_type_arg_msg (NULL, 0, v, "generalized vector");
}
#undef FUNC_NAME


void
scm_init_vectors ()
{
  scm_nullvect = scm_c_make_vector (0, SCM_UNDEFINED);

#include "libguile/vectors.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
