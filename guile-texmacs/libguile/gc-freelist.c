/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2002, 2006, 2008 Free Software Foundation, Inc.
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

#include <assert.h>
#include <stdio.h>

#include "libguile/private-gc.h"
#include "libguile/gc.h"
#include "libguile/deprecation.h"
#include "libguile/private-gc.h"

scm_t_cell_type_statistics scm_i_master_freelist;
scm_t_cell_type_statistics scm_i_master_freelist2;
#ifdef __MINGW32__
scm_t_cell_type_statistics *scm_i_master_freelist_ptr = &scm_i_master_freelist;
scm_t_cell_type_statistics *scm_i_master_freelist2_ptr = &scm_i_master_freelist2;
#endif



/*

In older versions of GUILE GC there was extensive support for
debugging freelists. This was useful, since the freelist was kept
inside the heap, and writing to an object that was GC'd would mangle
the list. Mark bits are now separate, and checking for sane cell
access can be done much more easily by simply checking if the mark bit
is unset before allocation.  --hwn



*/

#if (SCM_ENABLE_DEPRECATED == 1)
#if defined(GUILE_DEBUG_FREELIST)

SCM_DEFINE (scm_map_free_list, "map-free-list", 0, 0, 0,
            (),
	    "DEPRECATED\n")
#define FUNC_NAME "s_scm_map_free_list"
{
  scm_c_issue_deprecation_warning ("map-free-list has been removed from GUILE. Doing nothing\n");
  return SCM_UNSPECIFIED;
}  
#undef FUNC_NAME

SCM_DEFINE (scm_gc_set_debug_check_freelist_x, "gc-set-debug-check-freelist!", 1, 0, 0,
            (SCM flag),
	    "DEPRECATED.\n")
#define FUNC_NAME "s_scm_gc_set_debug_check_freelist_x"
{
  scm_c_issue_deprecation_warning ("gc-set-debug-check-freelist! has been removed from GUILE. Doing nothing\n");
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


#endif /* defined (GUILE_DEBUG) */
#endif /* deprecated */




/*
  This adjust FREELIST variables to decide wether or not to allocate
  more heap in the next GC run. It uses scm_gc_cells_collected and scm_gc_cells_collected1
 */

void
scm_i_adjust_min_yield (scm_t_cell_type_statistics *freelist)
{
  /* min yield is adjusted upwards so that next predicted total yield
   * (allocated cells actually freed by GC) becomes
   * `min_yield_fraction' of total heap size.  Note, however, that
   * the absolute value of min_yield will correspond to `collected'
   * on one master (the one which currently is triggering GC).
   *
   * The reason why we look at total yield instead of cells collected
   * on one list is that we want to take other freelists into account.
   * On this freelist, we know that (local) yield = collected cells,
   * but that's probably not the case on the other lists.
   *
   * (We might consider computing a better prediction, for example
   *  by computing an average over multiple GC:s.)
   */
  if (freelist->min_yield_fraction)
    {
      /* Pick largest of last two yields. */
      long delta = ((SCM_HEAP_SIZE * freelist->min_yield_fraction / 100)
		   - (long) SCM_MAX (scm_gc_cells_collected_1, scm_gc_cells_collected));
#ifdef DEBUGINFO
      fprintf (stderr, " after GC = %lu, delta = %ld\n",
	       (unsigned long) scm_cells_allocated,
	       (long) delta);
#endif
      if (delta > 0)
	freelist->min_yield += delta;
    }
}


static void
scm_init_freelist (scm_t_cell_type_statistics *freelist,
	       int span,
	       int min_yield)
{
  if (min_yield < 1)
    min_yield = 1;
  if (min_yield > 99)
    min_yield = 99;

  freelist->heap_segment_idx = -1;
  freelist->min_yield = 0;
  freelist->min_yield_fraction = min_yield;
  freelist->span = span;
  freelist->collected = 0;
  freelist->collected_1 = 0;
  freelist->heap_size = 0;
}

#if (SCM_ENABLE_DEPRECATED == 1)
 size_t scm_default_init_heap_size_1;
 int scm_default_min_yield_1;
 size_t scm_default_init_heap_size_2;
 int scm_default_min_yield_2;
 size_t scm_default_max_segment_size;
#endif

void
scm_gc_init_freelist (void)
{
  int init_heap_size_1
    = scm_getenv_int ("GUILE_INIT_SEGMENT_SIZE_1", SCM_DEFAULT_INIT_HEAP_SIZE_1);
  int init_heap_size_2
    = scm_getenv_int ("GUILE_INIT_SEGMENT_SIZE_2", SCM_DEFAULT_INIT_HEAP_SIZE_2);

  scm_init_freelist (&scm_i_master_freelist2, 2, 
		     scm_getenv_int ("GUILE_MIN_YIELD_2", SCM_DEFAULT_MIN_YIELD_2));
  scm_init_freelist (&scm_i_master_freelist, 1,
		     scm_getenv_int ("GUILE_MIN_YIELD_1", SCM_DEFAULT_MIN_YIELD_1));

  scm_max_segment_size = scm_getenv_int ("GUILE_MAX_SEGMENT_SIZE", SCM_DEFAULT_MAX_SEGMENT_SIZE);

  if (scm_max_segment_size <= 0)
    scm_max_segment_size = SCM_DEFAULT_MAX_SEGMENT_SIZE;
  
  
  scm_i_make_initial_segment (init_heap_size_1, &scm_i_master_freelist);
  scm_i_make_initial_segment (init_heap_size_2, &scm_i_master_freelist2);
  
#if (SCM_ENABLE_DEPRECATED == 1)
  if ( scm_default_init_heap_size_1 ||
       scm_default_min_yield_1||
       scm_default_init_heap_size_2||
       scm_default_min_yield_2||
       scm_default_max_segment_size)
    {
      scm_c_issue_deprecation_warning ("Tuning heap parameters with C variables is deprecated. Use environment variables instead.");
    }
#endif
}


void
scm_i_gc_sweep_freelist_reset (scm_t_cell_type_statistics *freelist)
{
  freelist->collected_1 = freelist->collected;
  freelist->collected = 0;
  
  /*
    at the end we simply start with the lowest segment again.
   */
  freelist->heap_segment_idx = -1;
}

int
scm_i_gc_grow_heap_p (scm_t_cell_type_statistics * freelist)
{
  return SCM_MAX (freelist->collected,freelist->collected_1)  < freelist->min_yield;
}
