/* Copyright (C) 1995,1996,1998,2000,2001, 2003, 2006, 2008 Free Software Foundation, Inc.
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

#include <stdio.h>

#include "libguile/_scm.h"
#include "libguile/vectors.h"
#include "libguile/lang.h"
#include "libguile/hashtab.h"

#include "libguile/validate.h"
#include "libguile/weaks.h"



/* 1. The current hash table implementation in hashtab.c uses weak alist
 *    vectors (formerly called weak hash tables) internally.
 *
 * 2. All hash table operations still work on alist vectors.
 *
 * 3. The weak vector and alist vector Scheme API is accessed through
 *    the module (ice-9 weak-vector).
 */


/* {Weak Vectors}
 */


SCM_DEFINE (scm_make_weak_vector, "make-weak-vector", 1, 1, 0,
	    (SCM size, SCM fill),
	    "Return a weak vector with @var{size} elements. If the optional\n"
	    "argument @var{fill} is given, all entries in the vector will be\n"
	    "set to @var{fill}. The default value for @var{fill} is the\n"
	    "empty list.")
#define FUNC_NAME s_scm_make_weak_vector
{
  return scm_i_allocate_weak_vector (0, size, fill);
}
#undef FUNC_NAME


SCM_REGISTER_PROC(s_list_to_weak_vector, "list->weak-vector", 1, 0, 0, scm_weak_vector);

SCM_DEFINE (scm_weak_vector, "weak-vector", 0, 0, 1, 
           (SCM l),
	    "@deffnx {Scheme Procedure} list->weak-vector l\n"
	    "Construct a weak vector from a list: @code{weak-vector} uses\n"
	    "the list of its arguments while @code{list->weak-vector} uses\n"
	    "its only argument @var{l} (a list) to construct a weak vector\n"
	    "the same way @code{list->vector} would.")
#define FUNC_NAME s_scm_weak_vector
{
  scm_t_array_handle handle;
  SCM res, *data;
  long i;

  i = scm_ilength (l);
  SCM_ASSERT (i >= 0, l, SCM_ARG1, FUNC_NAME);

  res = scm_make_weak_vector (scm_from_int (i), SCM_UNSPECIFIED);
  data = scm_vector_writable_elements (res, &handle, NULL, NULL);

  while (scm_is_pair (l) && i > 0)
    {
      *data++ = SCM_CAR (l);
      l = SCM_CDR (l);
      i--;
    }

  scm_array_handle_release (&handle);

  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_vector_p, "weak-vector?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a weak vector. Note that all\n"
	    "weak hashes are also weak vectors.")
#define FUNC_NAME s_scm_weak_vector_p
{
  return scm_from_bool (SCM_I_WVECTP (obj) && !SCM_IS_WHVEC (obj));
}
#undef FUNC_NAME



SCM_DEFINE (scm_make_weak_key_alist_vector, "make-weak-key-alist-vector", 0, 1, 0, 
	    (SCM size),
	    "@deffnx {Scheme Procedure} make-weak-value-alist-vector size\n"
	    "@deffnx {Scheme Procedure} make-doubly-weak-alist-vector size\n"
	    "Return a weak hash table with @var{size} buckets. As with any\n"
	    "hash table, choosing a good size for the table requires some\n"
	    "caution.\n"
	    "\n"
	    "You can modify weak hash tables in exactly the same way you\n"
	    "would modify regular hash tables. (@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_weak_key_alist_vector
{
  return scm_i_allocate_weak_vector
    (1, SCM_UNBNDP (size) ? scm_from_int (31) : size, SCM_EOL);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_weak_value_alist_vector, "make-weak-value-alist-vector", 0, 1, 0, 
            (SCM size),
	    "Return a hash table with weak values with @var{size} buckets.\n"
	    "(@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_weak_value_alist_vector
{
  return scm_i_allocate_weak_vector
    (2, SCM_UNBNDP (size) ? scm_from_int (31) : size, SCM_EOL);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_doubly_weak_alist_vector, "make-doubly-weak-alist-vector", 1, 0, 0, 
            (SCM size),
	    "Return a hash table with weak keys and values with @var{size}\n"
	    "buckets.  (@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_doubly_weak_alist_vector
{
  return scm_i_allocate_weak_vector
    (3, SCM_UNBNDP (size) ? scm_from_int (31) : size, SCM_EOL);
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_key_alist_vector_p, "weak-key-alist-vector?", 1, 0, 0, 
           (SCM obj),
	    "@deffnx {Scheme Procedure} weak-value-alist-vector? obj\n"
	    "@deffnx {Scheme Procedure} doubly-weak-alist-vector? obj\n"
	    "Return @code{#t} if @var{obj} is the specified weak hash\n"
	    "table. Note that a doubly weak hash table is neither a weak key\n"
	    "nor a weak value hash table.")
#define FUNC_NAME s_scm_weak_key_alist_vector_p
{
  return scm_from_bool (SCM_I_WVECTP (obj) && SCM_IS_WHVEC (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_value_alist_vector_p, "weak-value-alist-vector?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a weak value hash table.")
#define FUNC_NAME s_scm_weak_value_alist_vector_p
{
  return scm_from_bool (SCM_I_WVECTP (obj) && SCM_IS_WHVEC_V (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_doubly_weak_alist_vector_p, "doubly-weak-alist-vector?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a doubly weak hash table.")
#define FUNC_NAME s_scm_doubly_weak_alist_vector_p
{
  return scm_from_bool (SCM_I_WVECTP (obj) && SCM_IS_WHVEC_B (obj));
}
#undef FUNC_NAME

#define UNMARKED_CELL_P(x) (SCM_NIMP(x) && !SCM_GC_MARK_P (x))

static SCM weak_vectors;

void
scm_i_init_weak_vectors_for_gc ()
{
  weak_vectors = SCM_EOL;
}

void
scm_i_mark_weak_vector (SCM w)
{
  SCM_I_SET_WVECT_GC_CHAIN (w, weak_vectors);
  weak_vectors = w;
}

static int
scm_i_mark_weak_vector_non_weaks (SCM w)
{
  int again = 0;

  if (SCM_IS_WHVEC_ANY (w))
    {
      SCM *ptr;
      long n = SCM_I_WVECT_LENGTH (w);
      long j;
      int weak_keys = SCM_IS_WHVEC (w) || SCM_IS_WHVEC_B (w);
      int weak_values = SCM_IS_WHVEC_V (w) || SCM_IS_WHVEC_B (w);

      ptr = SCM_I_WVECT_GC_WVELTS (w);

      for (j = 0; j < n; ++j)
	{
	  SCM alist, slow_alist;
	  int slow_toggle = 0;

	  /* We do not set the mark bits of the alist spine cells here
	     since we do not want to ever create the situation where a
	     marked cell references an unmarked cell (except in
	     scm_gc_mark, where the referenced cells will be marked
	     immediately).  Thus, we can not use mark bits to stop us
	     from looping indefinitely over a cyclic alist.  Instead,
	     we use the standard tortoise and hare trick to catch
	     cycles.  The fast walker does the work, and stops when it
	     catches the slow walker to ensure that the whole cycle
	     has been worked on.
	  */

	  alist = slow_alist = ptr[j];

	  while (scm_is_pair (alist))
	    {
	      SCM elt = SCM_CAR (alist);

	      if (UNMARKED_CELL_P (elt))
		{
		  if (scm_is_pair (elt))
		    {
		      SCM key = SCM_CAR (elt);
		      SCM value = SCM_CDR (elt);
		  
		      if (!((weak_keys && UNMARKED_CELL_P (key))
			    || (weak_values && UNMARKED_CELL_P (value))))
			{
			  /* The item should be kept.  We need to mark it
			     recursively.
			  */ 
			  scm_gc_mark (elt);
			  again = 1;
			}
		    }
		  else
		    {
		      /* A non-pair cell element.  This should not
			 appear in a real alist, but when it does, we
			 need to keep it.
		      */
		      scm_gc_mark (elt);
		      again = 1;
		    }
		}

	      alist = SCM_CDR (alist);

	      if (slow_toggle && scm_is_pair (slow_alist))
		{
		  slow_alist = SCM_CDR (slow_alist);
		  slow_toggle = !slow_toggle;
		  if (scm_is_eq (slow_alist, alist))
		    break;
		}
	    }
	  if (!scm_is_pair (alist))
	    scm_gc_mark (alist);
	}
    }

  return again;
}

int
scm_i_mark_weak_vectors_non_weaks ()
{
  int again = 0;
  SCM w = weak_vectors;
  while (!scm_is_null (w))
    {
      if (scm_i_mark_weak_vector_non_weaks (w))
	again = 1;
      w = SCM_I_WVECT_GC_CHAIN (w);
    }
  return again;
}

static void
scm_i_remove_weaks (SCM w)
{
  SCM *ptr = SCM_I_WVECT_GC_WVELTS (w);
  size_t n = SCM_I_WVECT_LENGTH (w);
  size_t i;

  if (!SCM_IS_WHVEC_ANY (w))
    {
      for (i = 0; i < n; ++i)
	if (UNMARKED_CELL_P (ptr[i]))
	  ptr[i] = SCM_BOOL_F;
    }
  else
    {
      size_t delta = 0;

      for (i = 0; i < n; ++i)
	{
	  SCM alist, *fixup;

	  fixup = ptr + i;
	  alist = *fixup;
	  while (scm_is_pair (alist) && !SCM_GC_MARK_P (alist))
	    {
	      if (UNMARKED_CELL_P (SCM_CAR (alist)))
		{
		  *fixup = SCM_CDR (alist);
		  delta++;
		}
	      else
		{
		  SCM_SET_GC_MARK (alist);
		  fixup = SCM_CDRLOC (alist);
		}
	      alist = *fixup;
	    }
	}
#if 0
      if (delta)
	fprintf (stderr, "vector %p, delta %d\n", w, delta);
#endif
      SCM_I_SET_WVECT_DELTA (w, delta);
    }
}

void
scm_i_remove_weaks_from_weak_vectors ()
{
  SCM w = weak_vectors;
  while (!scm_is_null (w))
    {
      scm_i_remove_weaks (w);
      w = SCM_I_WVECT_GC_CHAIN (w);
    }
}



SCM
scm_init_weaks_builtins ()
{
#include "libguile/weaks.x"
  return SCM_UNSPECIFIED;
}

void
scm_init_weaks ()
{
  scm_c_define_gsubr ("%init-weaks-builtins", 0, 0, 0,
		      scm_init_weaks_builtins);
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
