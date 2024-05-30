/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc.
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

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <assert.h>

#ifdef __ia64__
#include <ucontext.h>
extern unsigned long * __libc_ia64_register_backing_store_base;
#endif

#include "libguile/_scm.h"
#include "libguile/eval.h"
#include "libguile/stime.h"
#include "libguile/stackchk.h"
#include "libguile/struct.h"
#include "libguile/smob.h"
#include "libguile/unif.h"
#include "libguile/async.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"
#include "libguile/weaks.h"
#include "libguile/hashtab.h"
#include "libguile/tags.h"
#include "libguile/private-gc.h"
#include "libguile/validate.h"
#include "libguile/deprecation.h"
#include "libguile/gc.h"
#include "libguile/guardians.h"

#ifdef GUILE_DEBUG_MALLOC
#include "libguile/debug-malloc.h"
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/*
  Entry point for this file.
 */
void
scm_mark_all (void)
{
  long j;
  int loops;

  scm_i_init_weak_vectors_for_gc ();
  scm_i_init_guardians_for_gc ();
  
  scm_i_clear_mark_space ();
  
  /* Mark every thread's stack and registers */
  scm_threads_mark_stacks ();

  j = SCM_NUM_PROTECTS;
  while (j--)
    scm_gc_mark (scm_sys_protects[j]);

  /* mark the registered roots */
  {
    size_t i;
    for (i = 0; i < SCM_HASHTABLE_N_BUCKETS (scm_gc_registered_roots); ++i)
      {
	SCM l = SCM_HASHTABLE_BUCKET (scm_gc_registered_roots, i);
	for (; !scm_is_null (l); l = SCM_CDR (l))
	  {
	    SCM *p = (SCM *) (scm_to_ulong (SCM_CAAR (l)));
	    scm_gc_mark (*p);
	  }
      }
  }
  
  scm_mark_subr_table ();

  loops = 0;
  while (1)
    {
      int again;
      loops++;

      /* Mark the non-weak references of weak vectors.  For a weak key
	 alist vector, this would mark the values for keys that are
	 marked.  We need to do this in a loop until everything
	 settles down since the newly marked values might be keys in
	 other weak key alist vectors, for example.
      */
      again = scm_i_mark_weak_vectors_non_weaks ();
      if (again)
	continue;

      /* Now we scan all marked guardians and move all unmarked objects
	 from the accessible to the inaccessible list.
      */
      scm_i_identify_inaccessible_guardeds ();

      /* When we have identified all inaccessible objects, we can mark
	 them.
      */
      again = scm_i_mark_inaccessible_guardeds ();

      /* This marking might have changed the situation for weak vectors
	 and might have turned up new guardians that need to be processed,
	 so we do it all over again.
      */
      if (again)
	continue;
      
      /* Nothing new marked in this round, we are done.
       */
      break;
    }

  /* fprintf (stderr, "%d loops\n", loops); */

  /* Remove all unmarked entries from the weak vectors.
   */
  scm_i_remove_weaks_from_weak_vectors ();
  
  /* Bring hashtables upto date.
   */
  scm_i_scan_weak_hashtables ();
}

/* {Mark/Sweep}
 */

/*
  Mark an object precisely, then recurse.
 */
void
scm_gc_mark (SCM ptr)
{
  if (SCM_IMP (ptr))
    return;
  
  if (SCM_GC_MARK_P (ptr))
    return;

  SCM_SET_GC_MARK (ptr);
  scm_gc_mark_dependencies (ptr);
}

/*

Mark the dependencies of an object.

Prefetching:

Should prefetch objects before marking, i.e. if marking a cell, we
should prefetch the car, and then mark the cdr. This will improve CPU
cache misses, because the car is more likely to be in core when we
finish the cdr.

See http://www.hpl.hp.com/techreports/2000/HPL-2000-99.pdf, reducing
garbage collector cache misses.

Prefetch is supported on GCC >= 3.1 

(Some time later.)

Tried this with GCC 3.1.1 -- the time differences are barely measurable.
Perhaps this would work better with an explicit markstack?


*/

void
scm_gc_mark_dependencies (SCM p)
#define FUNC_NAME "scm_gc_mark_dependencies"
{
  register long i;
  register SCM ptr;
  SCM cell_type;

  ptr = p;
 scm_mark_dependencies_again:
  
  cell_type = SCM_GC_CELL_TYPE (ptr);
  switch (SCM_ITAG7 (cell_type))
    {
    case scm_tcs_cons_nimcar:
      if (SCM_IMP (SCM_CDR (ptr)))
	{
	  ptr = SCM_CAR (ptr);
	  goto gc_mark_nimp;
	}


      scm_gc_mark (SCM_CAR (ptr));
      ptr = SCM_CDR (ptr);
      goto gc_mark_nimp;
    case scm_tcs_cons_imcar:
      ptr = SCM_CDR (ptr);
      goto gc_mark_loop;
    case scm_tc7_pws:

      scm_gc_mark (SCM_SETTER (ptr));
      ptr = SCM_PROCEDURE (ptr);
      goto gc_mark_loop;
    case scm_tcs_struct:
      {
	/* XXX - use less explicit code. */
	scm_t_bits word0 = SCM_CELL_WORD_0 (ptr) - scm_tc3_struct;
	scm_t_bits * vtable_data = (scm_t_bits *) word0;
	SCM layout = SCM_PACK (vtable_data [scm_vtable_index_layout]);
	long len = scm_i_symbol_length (layout);
	const char *fields_desc = scm_i_symbol_chars (layout);
	scm_t_bits *struct_data = (scm_t_bits *) SCM_STRUCT_DATA (ptr);

	if (vtable_data[scm_struct_i_flags] & SCM_STRUCTF_ENTITY)
	  {
	    scm_gc_mark (SCM_PACK (struct_data[scm_struct_i_procedure]));
	    scm_gc_mark (SCM_PACK (struct_data[scm_struct_i_setter]));
	  }
	if (len)
	  {
	    long x;

	    for (x = 0; x < len - 2; x += 2, ++struct_data)
	      if (fields_desc[x] == 'p')
		scm_gc_mark (SCM_PACK (*struct_data));
	    if (fields_desc[x] == 'p')
	      {
		if (SCM_LAYOUT_TAILP (fields_desc[x + 1]))
		  for (x = *struct_data++; x; --x, ++struct_data)
		    scm_gc_mark (SCM_PACK (*struct_data));
		else
		  scm_gc_mark (SCM_PACK (*struct_data));
	      }
	  }
	/* mark vtable */
	ptr = SCM_PACK (vtable_data [scm_vtable_index_vtable]);
	goto gc_mark_loop;
      }
      break;
    case scm_tcs_closures:
      if (SCM_IMP (SCM_ENV (ptr)))
	{
	  ptr = SCM_CLOSCAR (ptr);
	  goto gc_mark_nimp;
	}
      scm_gc_mark (SCM_CLOSCAR (ptr));
      ptr = SCM_ENV (ptr);
      goto gc_mark_nimp;
    case scm_tc7_vector:
      i = SCM_SIMPLE_VECTOR_LENGTH (ptr);
      if (i == 0)
	break;
      while (--i > 0)
	{
	  SCM elt = SCM_SIMPLE_VECTOR_REF (ptr, i);
	  if (SCM_NIMP (elt))
	    scm_gc_mark (elt);
	}
      ptr = SCM_SIMPLE_VECTOR_REF (ptr, 0);
      goto gc_mark_loop;
#ifdef CCLO
    case scm_tc7_cclo:
      {
	size_t i = SCM_CCLO_LENGTH (ptr);
	size_t j;
	for (j = 1; j != i; ++j)
	  {
	    SCM obj = SCM_CCLO_REF (ptr, j);
	    if (!SCM_IMP (obj))
	      scm_gc_mark (obj);
	  }
	ptr = SCM_CCLO_REF (ptr, 0);
	goto gc_mark_loop;
      }
#endif

    case scm_tc7_string:
      ptr = scm_i_string_mark (ptr);
      goto gc_mark_loop;
    case scm_tc7_stringbuf:
      ptr = scm_i_stringbuf_mark (ptr);
      goto gc_mark_loop;

    case scm_tc7_number:
      if (SCM_TYP16 (ptr) == scm_tc16_fraction)
	{
	  scm_gc_mark (SCM_CELL_OBJECT_1 (ptr));
	  ptr = SCM_CELL_OBJECT_2 (ptr);
	  goto gc_mark_loop;
	}
      break;

    case scm_tc7_wvect:
      scm_i_mark_weak_vector (ptr);
      break;

    case scm_tc7_symbol:
      ptr = scm_i_symbol_mark (ptr);
      goto gc_mark_loop;
    case scm_tc7_variable:
      ptr = SCM_CELL_OBJECT_1 (ptr);
      goto gc_mark_loop;
    case scm_tcs_subrs:
      break;
    case scm_tc7_port:
      i = SCM_PTOBNUM (ptr);
#if (SCM_DEBUG_CELL_ACCESSES == 1) 
      if (!(i < scm_numptob))
	{
	  fprintf (stderr, "undefined port type");
	  abort();
	}
#endif
      if (SCM_PTAB_ENTRY(ptr))
	scm_gc_mark (SCM_FILENAME (ptr));
      if (scm_ptobs[i].mark)
	{
	  ptr = (scm_ptobs[i].mark) (ptr);
	  goto gc_mark_loop;
	}
      else
	return;
      break;
    case scm_tc7_smob:
      switch (SCM_TYP16 (ptr))
	{ /* should be faster than going through scm_smobs */
	case scm_tc_free_cell:
	  /* We have detected a free cell.  This can happen if non-object data
	   * on the C stack points into guile's heap and is scanned during
	   * conservative marking.  */
	  break;
	default:
	  i = SCM_SMOBNUM (ptr);
#if (SCM_DEBUG_CELL_ACCESSES == 1)
	  if (!(i < scm_numsmob))
	    {
	      fprintf (stderr, "undefined smob type");
	      abort();
	    }
#endif
	  if (scm_smobs[i].mark)
	    {
	      ptr = (scm_smobs[i].mark) (ptr);
	      goto gc_mark_loop;
	    }
	  else
	    return;
	}
      break;
    default:
      fprintf (stderr, "unknown type");
      abort();
    }

  /*
    If we got here, then exhausted recursion options for PTR.  we
    return (careful not to mark PTR, it might be the argument that we
    were called with.)
   */
  return ;

 gc_mark_loop:
  if (SCM_IMP (ptr))
    return;

 gc_mark_nimp:
  {
    int valid_cell = CELL_P (ptr);

    
#if (SCM_DEBUG_CELL_ACCESSES == 1)
    if (scm_debug_cell_accesses_p)
      {
    /* We are in debug mode.  Check the ptr exhaustively. */
	
	valid_cell = valid_cell && (scm_i_find_heap_segment_containing_object (ptr) >= 0);
      }
    
#endif
    if (!valid_cell)
      {
	fprintf (stderr, "rogue pointer in heap");
	abort();
      }
  }
  
 if (SCM_GC_MARK_P (ptr))
  {
    return;
  }
  
  SCM_SET_GC_MARK (ptr);

  goto   scm_mark_dependencies_again;
  
}
#undef FUNC_NAME




/* Mark a region conservatively */
void
scm_mark_locations (SCM_STACKITEM x[], unsigned long n)
{
  unsigned long m;

  for (m = 0; m < n; ++m)
    {
      SCM obj = * (SCM *) &x[m];
      long int segment = scm_i_find_heap_segment_containing_object (obj);
      if (segment >= 0)
	scm_gc_mark (obj);
    }
}


/* The function scm_in_heap_p determines whether an SCM value can be regarded as a
 * pointer to a cell on the heap.
 */
int
scm_in_heap_p (SCM value)
{
  long int segment = scm_i_find_heap_segment_containing_object (value);
  return (segment >= 0);
}


#if SCM_ENABLE_DEPRECATED == 1

/* If an allocated cell is detected during garbage collection, this
 * means that some code has just obtained the object but was preempted
 * before the initialization of the object was completed.  This meanst
 * that some entries of the allocated cell may already contain SCM
 * objects.  Therefore, allocated cells are scanned conservatively.
 */

scm_t_bits scm_tc16_allocated;

static SCM
allocated_mark (SCM cell)
{
  unsigned long int cell_segment = scm_i_find_heap_segment_containing_object (cell);
  unsigned int span = scm_i_heap_segment_table[cell_segment]->span;
  unsigned int i;

  for (i = 1; i != span * 2; ++i)
    {
      SCM obj = SCM_CELL_OBJECT (cell, i);
      long int obj_segment = scm_i_find_heap_segment_containing_object (obj);
      if (obj_segment >= 0)
	scm_gc_mark (obj);
    }
  return SCM_BOOL_F;
}

SCM
scm_deprecated_newcell (void)
{
  scm_c_issue_deprecation_warning 
    ("SCM_NEWCELL is deprecated.  Use `scm_cell' instead.\n");

  return scm_cell (scm_tc16_allocated, 0);
}

SCM
scm_deprecated_newcell2 (void)
{
  scm_c_issue_deprecation_warning 
    ("SCM_NEWCELL2 is deprecated.  Use `scm_double_cell' instead.\n");

  return scm_double_cell (scm_tc16_allocated, 0, 0, 0);
}

#endif /* SCM_ENABLE_DEPRECATED == 1 */


void
scm_gc_init_mark(void)
{
#if SCM_ENABLE_DEPRECATED == 1
  scm_tc16_allocated = scm_make_smob_type ("allocated cell", 0);
  scm_set_smob_mark (scm_tc16_allocated, allocated_mark);
#endif
}

