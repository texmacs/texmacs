/* Copyright (C) 1999,2000,2001,2002, 2004, 2006, 2007, 2008 Free Software Foundation, Inc.
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



/* Written in December 1998 by Roland Orre <orre@nada.kth.se>
 * This implements the same sort interface as slib/sort.scm
 * for lists and vectors where slib defines:
 * sorted?, merge, merge!, sort, sort!
 * For scsh compatibility sort-list and sort-list! are also defined.
 * In cases where a stable-sort is required use stable-sort or
 * stable-sort!.  An additional feature is
 * (restricted-vector-sort! vector less? startpos endpos)
 * which allows you to sort part of a vector.
 * Thanks to Aubrey Jaffer for the slib/sort.scm library.
 * Thanks to Richard A. O'Keefe (based on Prolog code by D.H.D.Warren)
 * for the merge sort inspiration.
 * Thanks to Douglas C. Schmidt (schmidt@ics.uci.edu) for the
 * quicksort code.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/eval.h"
#include "libguile/unif.h"
#include "libguile/ramap.h"
#include "libguile/feature.h"
#include "libguile/vectors.h"
#include "libguile/lang.h"
#include "libguile/async.h"
#include "libguile/dynwind.h"

#include "libguile/validate.h"
#include "libguile/sort.h"

/* We have two quicksort variants: one for contigous vectors and one
   for vectors with arbitrary increments between elements.  Note that
   increments can be negative.
*/

#define NAME        quicksort1
#define INC_PARAM   /* empty */
#define INC         1
#include "libguile/quicksort.i.c"

#define NAME        quicksort
#define INC_PARAM   ssize_t inc,
#define INC         inc
#include "libguile/quicksort.i.c"

static scm_t_trampoline_2
compare_function (SCM less, unsigned int arg_nr, const char* fname)
{
  const scm_t_trampoline_2 cmp = scm_trampoline_2 (less);
  SCM_ASSERT_TYPE (cmp != NULL, less, arg_nr, fname, "less predicate");
  return cmp;
}


SCM_DEFINE (scm_restricted_vector_sort_x, "restricted-vector-sort!", 4, 0, 0, 
            (SCM vec, SCM less, SCM startpos, SCM endpos),
	    "Sort the vector @var{vec}, using @var{less} for comparing\n"
	    "the vector elements.  @var{startpos} (inclusively) and\n"
	    "@var{endpos} (exclusively) delimit\n"
	    "the range of the vector which gets sorted.  The return value\n"
	    "is not specified.")
#define FUNC_NAME s_scm_restricted_vector_sort_x
{
  const scm_t_trampoline_2 cmp = compare_function (less, 2, FUNC_NAME);
  size_t vlen, spos, len;
  ssize_t vinc;
  scm_t_array_handle handle;
  SCM *velts;

  velts = scm_vector_writable_elements (vec, &handle, &vlen, &vinc);
  spos = scm_to_unsigned_integer (startpos, 0, vlen);
  len = scm_to_unsigned_integer (endpos, spos, vlen) - spos;

  if (vinc == 1)
    quicksort1 (velts + spos*vinc, len, cmp, less);
  else
    quicksort (velts + spos*vinc, len, vinc, cmp, less);

  scm_array_handle_release (&handle);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* (sorted? sequence less?)
 * is true when sequence is a list (x0 x1 ... xm) or a vector #(x0 ... xm)
 * such that for all 1 <= i <= m,
 * (not (less? (list-ref list i) (list-ref list (- i 1)))). */
SCM_DEFINE (scm_sorted_p, "sorted?", 2, 0, 0,
            (SCM items, SCM less),
	    "Return @code{#t} iff @var{items} is a list or a vector such that\n"
	    "for all 1 <= i <= m, the predicate @var{less} returns true when\n"
	    "applied to all elements i - 1 and i")
#define FUNC_NAME s_scm_sorted_p
{
  const scm_t_trampoline_2 cmp = compare_function (less, 2, FUNC_NAME);
  long len, j;			/* list/vector length, temp j */
  SCM item, rest;		/* rest of items loop variable */

  if (SCM_NULL_OR_NIL_P (items))
    return SCM_BOOL_T;

  if (scm_is_pair (items))
    {
      len = scm_ilength (items); /* also checks that it's a pure list */
      SCM_ASSERT_RANGE (1, items, len >= 0);
      if (len <= 1)
	return SCM_BOOL_T;

      item = SCM_CAR (items);
      rest = SCM_CDR (items);
      j = len - 1;
      while (j > 0)
	{
	  if (scm_is_true ((*cmp) (less, SCM_CAR (rest), item)))
	    return SCM_BOOL_F;
	  else
	    {
	      item = SCM_CAR (rest);
	      rest = SCM_CDR (rest);
	      j--;
	    }
	}
      return SCM_BOOL_T;
    }
  else
    {
      scm_t_array_handle handle;
      size_t i, len;
      ssize_t inc;
      const SCM *elts;
      SCM result = SCM_BOOL_T;

      elts = scm_vector_elements (items, &handle, &len, &inc);

      for (i = 1; i < len; i++, elts += inc)
	{
	  if (scm_is_true ((*cmp) (less, elts[inc], elts[0])))
	    {
	      result = SCM_BOOL_F;
	      break;
	    }
	}

      scm_array_handle_release (&handle);

      return result;
    }

  return SCM_BOOL_F;
}
#undef FUNC_NAME


/* (merge a b less?)
   takes two lists a and b such that (sorted? a less?) and (sorted? b less?)
   and returns a new list in which the elements of a and b have been stably
   interleaved so that (sorted? (merge a b less?) less?).
   Note:  this does _not_ accept vectors. */
SCM_DEFINE (scm_merge, "merge", 3, 0, 0, 
            (SCM alist, SCM blist, SCM less),
	    "Merge two already sorted lists into one.\n"
	    "Given two lists @var{alist} and @var{blist}, such that\n"
	    "@code{(sorted? alist less?)} and @code{(sorted? blist less?)},\n"
	    "return a new list in which the elements of @var{alist} and\n"
	    "@var{blist} have been stably interleaved so that\n"
	    "@code{(sorted? (merge alist blist less?) less?)}.\n"
	    "Note:  this does _not_ accept vectors.")
#define FUNC_NAME s_scm_merge
{
  SCM build;

  if (SCM_NULL_OR_NIL_P (alist))
    return blist;
  else if (SCM_NULL_OR_NIL_P (blist))
    return alist;
  else
    {
      const scm_t_trampoline_2 cmp = compare_function (less, 3, FUNC_NAME);
      long alen, blen;		/* list lengths */
      SCM last;

      SCM_VALIDATE_NONEMPTYLIST_COPYLEN (1, alist, alen);
      SCM_VALIDATE_NONEMPTYLIST_COPYLEN (2, blist, blen);
      if (scm_is_true ((*cmp) (less, SCM_CAR (blist), SCM_CAR (alist))))
	{
	  build = scm_cons (SCM_CAR (blist), SCM_EOL);
	  blist = SCM_CDR (blist);
	  blen--;
	}
      else
	{
	  build = scm_cons (SCM_CAR (alist), SCM_EOL);
	  alist = SCM_CDR (alist);
	  alen--;
	}
      last = build;
      while ((alen > 0) && (blen > 0))
	{
	  SCM_TICK;
	  if (scm_is_true ((*cmp) (less, SCM_CAR (blist), SCM_CAR (alist))))
	    {
	      SCM_SETCDR (last, scm_cons (SCM_CAR (blist), SCM_EOL));
	      blist = SCM_CDR (blist);
	      blen--;
	    }
	  else
	    {
	      SCM_SETCDR (last, scm_cons (SCM_CAR (alist), SCM_EOL));
	      alist = SCM_CDR (alist);
	      alen--;
	    }
	  last = SCM_CDR (last);
	}
      if ((alen > 0) && (blen == 0))
	SCM_SETCDR (last, alist);
      else if ((alen == 0) && (blen > 0))
	SCM_SETCDR (last, blist);
    }
  return build;
}
#undef FUNC_NAME


static SCM 
scm_merge_list_x (SCM alist, SCM blist,
		  long alen, long blen,
		  scm_t_trampoline_2 cmp, SCM less)
{
  SCM build, last;

  if (SCM_NULL_OR_NIL_P (alist))
    return blist;
  else if (SCM_NULL_OR_NIL_P (blist))
    return alist;
  else
    {
      if (scm_is_true ((*cmp) (less, SCM_CAR (blist), SCM_CAR (alist))))
	{
	  build = blist;
	  blist = SCM_CDR (blist);
	  blen--;
	}
      else
	{
	  build = alist;
	  alist = SCM_CDR (alist);
	  alen--;
	}
      last = build;
      while ((alen > 0) && (blen > 0))
	{
	  SCM_TICK;
	  if (scm_is_true ((*cmp) (less, SCM_CAR (blist), SCM_CAR (alist))))
	    {
	      SCM_SETCDR (last, blist);
	      blist = SCM_CDR (blist);
	      blen--;
	    }
	  else
	    {
	      SCM_SETCDR (last, alist);
	      alist = SCM_CDR (alist);
	      alen--;
	    }
	  last = SCM_CDR (last);
	}
      if ((alen > 0) && (blen == 0))
	SCM_SETCDR (last, alist);
      else if ((alen == 0) && (blen > 0))
	SCM_SETCDR (last, blist);
    }
  return build;
}				/* scm_merge_list_x */


SCM_DEFINE (scm_merge_x, "merge!", 3, 0, 0, 
            (SCM alist, SCM blist, SCM less),
	    "Takes two lists @var{alist} and @var{blist} such that\n"
	    "@code{(sorted? alist less?)} and @code{(sorted? blist less?)} and\n"
	    "returns a new list in which the elements of @var{alist} and\n"
	    "@var{blist} have been stably interleaved so that\n"
	    " @code{(sorted? (merge alist blist less?) less?)}.\n"
	    "This is the destructive variant of @code{merge}\n"
	    "Note:  this does _not_ accept vectors.")
#define FUNC_NAME s_scm_merge_x
{
  if (SCM_NULL_OR_NIL_P (alist))
    return blist;
  else if (SCM_NULL_OR_NIL_P (blist))
    return alist;
  else
    {
      const scm_t_trampoline_2 cmp = compare_function (less, 3, FUNC_NAME);
      long alen, blen;		/* list lengths */
      SCM_VALIDATE_NONEMPTYLIST_COPYLEN (1, alist, alen);
      SCM_VALIDATE_NONEMPTYLIST_COPYLEN (2, blist, blen);
      return scm_merge_list_x (alist, blist, alen, blen, cmp, less);
    }
}
#undef FUNC_NAME


/* This merge sort algorithm is same as slib's by Richard A. O'Keefe.
   The algorithm is stable. We also tried to use the algorithm used by
   scsh's merge-sort but that algorithm showed to not be stable, even
   though it claimed to be.
*/
static SCM 
scm_merge_list_step (SCM * seq, scm_t_trampoline_2 cmp, SCM less, long n)
{
  SCM a, b;

  if (n > 2)
    {
      long mid = n / 2;
      SCM_TICK;
      a = scm_merge_list_step (seq, cmp, less, mid);
      b = scm_merge_list_step (seq, cmp, less, n - mid);
      return scm_merge_list_x (a, b, mid, n - mid, cmp, less);
    }
  else if (n == 2)
    {
      SCM p = *seq;
      SCM rest = SCM_CDR (*seq);
      SCM x = SCM_CAR (*seq);
      SCM y = SCM_CAR (SCM_CDR (*seq));
      *seq = SCM_CDR (rest);
      SCM_SETCDR (rest, SCM_EOL);
      if (scm_is_true ((*cmp) (less, y, x)))
	{
	  SCM_SETCAR (p, y);
	  SCM_SETCAR (rest, x);
	}
      return p;
    }
  else if (n == 1)
    {
      SCM p = *seq;
      *seq = SCM_CDR (p);
      SCM_SETCDR (p, SCM_EOL);
      return p;
    }
  else
    return SCM_EOL;
}				/* scm_merge_list_step */


SCM_DEFINE (scm_sort_x, "sort!", 2, 0, 0, 
            (SCM items, SCM less),
	    "Sort the sequence @var{items}, which may be a list or a\n"
	    "vector.  @var{less} is used for comparing the sequence\n"
	    "elements.  The sorting is destructive, that means that the\n"
	    "input sequence is modified to produce the sorted result.\n"
	    "This is not a stable sort.")
#define FUNC_NAME s_scm_sort_x
{
  long len;			/* list/vector length */
  if (SCM_NULL_OR_NIL_P (items))
    return items;

  if (scm_is_pair (items))
    {
      const scm_t_trampoline_2 cmp = compare_function (less, 2, FUNC_NAME);
      SCM_VALIDATE_LIST_COPYLEN (1, items, len);
      return scm_merge_list_step (&items, cmp, less, len);
    }
  else if (scm_is_vector (items))
    {
      scm_restricted_vector_sort_x (items,
				    less,
				    scm_from_int (0),
				    scm_vector_length (items));
      return items;
    }
  else
    SCM_WRONG_TYPE_ARG (1, items);
}
#undef FUNC_NAME


SCM_DEFINE (scm_sort, "sort", 2, 0, 0, 
            (SCM items, SCM less),
	    "Sort the sequence @var{items}, which may be a list or a\n"
	    "vector.  @var{less} is used for comparing the sequence\n"
	    "elements.  This is not a stable sort.")
#define FUNC_NAME s_scm_sort
{
  if (SCM_NULL_OR_NIL_P (items))
    return items;

  if (scm_is_pair (items))
    return scm_sort_x (scm_list_copy (items), less);
  else if (scm_is_vector (items))
    return scm_sort_x (scm_vector_copy (items), less);
  else
    SCM_WRONG_TYPE_ARG (1, items);
}
#undef FUNC_NAME


static void
scm_merge_vector_x (SCM *vec,
		    SCM *temp,
		    scm_t_trampoline_2 cmp,
		    SCM less,
		    size_t low,
		    size_t mid,
		    size_t high,
		    ssize_t inc)
{
  size_t it;	     	/* Index for temp vector */
  size_t i1 = low;      /* Index for lower vector segment */
  size_t i2 = mid + 1; 	/* Index for upper vector segment */

#define VEC(i) vec[(i)*inc]

  /* Copy while both segments contain more characters */
  for (it = low; (i1 <= mid) && (i2 <= high); ++it)
    {
      if (scm_is_true ((*cmp) (less, VEC(i2), VEC(i1))))
	temp[it] = VEC(i2++);
      else
	temp[it] = VEC(i1++);
    }

  {
    /* Copy while first segment contains more characters */
    while (i1 <= mid)
      temp[it++] = VEC(i1++);

    /* Copy while second segment contains more characters */
    while (i2 <= high)
      temp[it++] = VEC(i2++);

    /* Copy back from temp to vp */
    for (it = low; it <= high; it++)
      VEC(it) = temp[it];
  }
} 	        		/* scm_merge_vector_x */


static void
scm_merge_vector_step (SCM *vec,
		       SCM *temp,
		       scm_t_trampoline_2 cmp,
		       SCM less,
		       size_t low,
		       size_t high,
		       ssize_t inc)
{
  if (high > low)
    {
      size_t mid = (low + high) / 2;
      SCM_TICK;
      scm_merge_vector_step (vec, temp, cmp, less, low, mid, inc);
      scm_merge_vector_step (vec, temp, cmp, less, mid+1, high, inc);
      scm_merge_vector_x (vec, temp, cmp, less, low, mid, high, inc);
    }
}				/* scm_merge_vector_step */


SCM_DEFINE (scm_stable_sort_x, "stable-sort!", 2, 0, 0, 
            (SCM items, SCM less),
	    "Sort the sequence @var{items}, which may be a list or a\n"
	    "vector. @var{less} is used for comparing the sequence elements.\n"
	    "The sorting is destructive, that means that the input sequence\n"
	    "is modified to produce the sorted result.\n"
	    "This is a stable sort.")
#define FUNC_NAME s_scm_stable_sort_x
{
  const scm_t_trampoline_2 cmp = compare_function (less, 2, FUNC_NAME);
  long len;			/* list/vector length */

  if (SCM_NULL_OR_NIL_P (items))
    return items;

  if (scm_is_pair (items))
    {
      SCM_VALIDATE_LIST_COPYLEN (1, items, len);
      return scm_merge_list_step (&items, cmp, less, len);
    }
  else if (scm_is_vector (items))
    {
      scm_t_array_handle temp_handle, vec_handle;
      SCM temp, *temp_elts, *vec_elts;
      size_t len;
      ssize_t inc;
      
      vec_elts = scm_vector_writable_elements (items, &vec_handle,
					       &len, &inc);
      temp = scm_c_make_vector (len, SCM_UNDEFINED);
      temp_elts = scm_vector_writable_elements (temp, &temp_handle,
						NULL, NULL);

      scm_merge_vector_step (vec_elts, temp_elts, cmp, less, 0, len-1, inc);

      scm_array_handle_release (&temp_handle);
      scm_array_handle_release (&vec_handle);

      return items;
    }
  else
    SCM_WRONG_TYPE_ARG (1, items);
}
#undef FUNC_NAME


SCM_DEFINE (scm_stable_sort, "stable-sort", 2, 0, 0, 
            (SCM items, SCM less),
	    "Sort the sequence @var{items}, which may be a list or a\n"
	    "vector. @var{less} is used for comparing the sequence elements.\n"
	    "This is a stable sort.")
#define FUNC_NAME s_scm_stable_sort
{
  if (SCM_NULL_OR_NIL_P (items))
    return SCM_EOL;

  if (scm_is_pair (items))
    return scm_stable_sort_x (scm_list_copy (items), less);
  else if (scm_is_vector (items))
    return scm_stable_sort_x (scm_vector_copy (items), less);
  else
    SCM_WRONG_TYPE_ARG (1, items);
}
#undef FUNC_NAME


SCM_DEFINE (scm_sort_list_x, "sort-list!", 2, 0, 0, 
            (SCM items, SCM less),
	    "Sort the list @var{items}, using @var{less} for comparing the\n"
	    "list elements. The sorting is destructive, that means that the\n"
	    "input list is modified to produce the sorted result.\n"
	    "This is a stable sort.")
#define FUNC_NAME s_scm_sort_list_x
{
  const scm_t_trampoline_2 cmp = compare_function (less, 2, FUNC_NAME);
  long len;

  SCM_VALIDATE_LIST_COPYLEN (1, items, len);
  return scm_merge_list_step (&items, cmp, less, len);
}
#undef FUNC_NAME


SCM_DEFINE (scm_sort_list, "sort-list", 2, 0, 0, 
	    (SCM items, SCM less),
	    "Sort the list @var{items}, using @var{less} for comparing the\n"
	    "list elements. This is a stable sort.")
#define FUNC_NAME s_scm_sort_list
{
  const scm_t_trampoline_2 cmp = compare_function (less, 2, FUNC_NAME);
  long len;

  SCM_VALIDATE_LIST_COPYLEN (1, items, len);
  items = scm_list_copy (items);
  return scm_merge_list_step (&items, cmp, less, len);
}
#undef FUNC_NAME


void
scm_init_sort ()
{
#include "libguile/sort.x"

  scm_add_feature ("sort");
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
