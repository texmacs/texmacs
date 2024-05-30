/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2002, 2003, 2006, 2008 Free Software Foundation, Inc.
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

/* #define DEBUGINFO */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <assert.h>

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
#include "libguile/dynwind.h"

#ifdef GUILE_DEBUG_MALLOC
#include "libguile/debug-malloc.h"
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* Lock this mutex before doing lazy sweeping.
 */
scm_i_pthread_mutex_t scm_i_sweep_mutex = SCM_I_PTHREAD_MUTEX_INITIALIZER;

/* Set this to != 0 if every cell that is accessed shall be checked:
 */
int scm_debug_cell_accesses_p = 0;
int scm_expensive_debug_cell_accesses_p = 0;

/* Set this to 0 if no additional gc's shall be performed, otherwise set it to
 * the number of cell accesses after which a gc shall be called.
 */
int scm_debug_cells_gc_interval = 0;

/*
  Global variable, so you can switch it off at runtime by setting
  scm_i_cell_validation_already_running.
 */
int scm_i_cell_validation_already_running ;

#if (SCM_DEBUG_CELL_ACCESSES == 1)


/*
  
  Assert that the given object is a valid reference to a valid cell.  This
  test involves to determine whether the object is a cell pointer, whether
  this pointer actually points into a heap segment and whether the cell
  pointed to is not a free cell.  Further, additional garbage collections may
  get executed after a user defined number of cell accesses.  This helps to
  find places in the C code where references are dropped for extremely short
  periods.

*/
void
scm_i_expensive_validation_check (SCM cell)
{
  if (!scm_in_heap_p (cell))
    {
      fprintf (stderr, "scm_assert_cell_valid: this object does not live in the heap: %lux\n",
	       (unsigned long) SCM_UNPACK (cell));
      abort ();
    }

  /* If desired, perform additional garbage collections after a user
   * defined number of cell accesses.
   */
  if (scm_debug_cells_gc_interval)
    {
      static unsigned int counter = 0;

      if (counter != 0)
	{
	  --counter;
	}
      else
	{
	  counter = scm_debug_cells_gc_interval;
	  scm_gc ();
	}
    }
}

void
scm_assert_cell_valid (SCM cell)
{
  if (!scm_i_cell_validation_already_running && scm_debug_cell_accesses_p)
    {
      scm_i_cell_validation_already_running = 1;  /* set to avoid recursion */

      /*
	During GC, no user-code should be run, and the guile core
	should use non-protected accessors.
      */
      if (scm_gc_running_p)
	return;

      /*
	Only scm_in_heap_p and rescanning the heap is wildly
	expensive.
      */
      if (scm_expensive_debug_cell_accesses_p)
	scm_i_expensive_validation_check (cell);
      
      if (!SCM_GC_MARK_P (cell))
	{
	  fprintf (stderr,
		   "scm_assert_cell_valid: this object is unmarked. \n"
		   "It has been garbage-collected in the last GC run: "
		   "%lux\n",
                   (unsigned long) SCM_UNPACK (cell));
	  abort ();
	}

      scm_i_cell_validation_already_running = 0;  /* re-enable */
    }
}



SCM_DEFINE (scm_set_debug_cell_accesses_x, "set-debug-cell-accesses!", 1, 0, 0,
	    (SCM flag),
	    "If @var{flag} is @code{#f}, cell access checking is disabled.\n"
	    "If @var{flag} is @code{#t}, cheap cell access checking is enabled,\n"
	    "but no additional calls to garbage collection are issued.\n"
	    "If @var{flag} is a number, strict cell access checking is enabled,\n"
	    "with an additional garbage collection after the given\n"
	    "number of cell accesses.\n"
	    "This procedure only exists when the compile-time flag\n"
	    "@code{SCM_DEBUG_CELL_ACCESSES} was set to 1.")
#define FUNC_NAME s_scm_set_debug_cell_accesses_x
{
  if (scm_is_false (flag))
    {
      scm_debug_cell_accesses_p = 0;
    }
  else if (scm_is_eq (flag, SCM_BOOL_T))
    {
      scm_debug_cells_gc_interval = 0;
      scm_debug_cell_accesses_p = 1;
      scm_expensive_debug_cell_accesses_p = 0;
    }
  else
    {
      scm_debug_cells_gc_interval = scm_to_signed_integer (flag, 0, INT_MAX);
      scm_debug_cell_accesses_p = 1;
      scm_expensive_debug_cell_accesses_p = 1;
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


#endif  /* SCM_DEBUG_CELL_ACCESSES == 1 */




/* scm_mtrigger
 * is the number of bytes of malloc allocation needed to trigger gc.
 */
unsigned long scm_mtrigger;

/* GC Statistics Keeping
 */
unsigned long scm_cells_allocated = 0;
unsigned long scm_last_cells_allocated;
unsigned long scm_mallocated = 0;
unsigned long scm_gc_cells_collected;
unsigned long scm_gc_cells_collected_1 = 0; /* previous GC yield */
unsigned long scm_gc_malloc_collected;
unsigned long scm_gc_ports_collected;
unsigned long scm_gc_time_taken = 0;
static unsigned long t_before_gc;
unsigned long scm_gc_mark_time_taken = 0;
unsigned long scm_gc_times = 0;
unsigned long scm_gc_cells_swept = 0;
double scm_gc_cells_marked_acc = 0.;
double scm_gc_cells_swept_acc = 0.;
double scm_gc_cells_allocated_acc = 0.;
int scm_gc_cell_yield_percentage =0;
int scm_gc_malloc_yield_percentage = 0;
unsigned long protected_obj_count = 0;


SCM_SYMBOL (sym_cells_allocated, "cells-allocated");
SCM_SYMBOL (sym_heap_size, "cell-heap-size");
SCM_SYMBOL (sym_mallocated, "bytes-malloced");
SCM_SYMBOL (sym_mtrigger, "gc-malloc-threshold");
SCM_SYMBOL (sym_heap_segments, "cell-heap-segments");
SCM_SYMBOL (sym_gc_time_taken, "gc-time-taken");
SCM_SYMBOL (sym_gc_mark_time_taken, "gc-mark-time-taken");
SCM_SYMBOL (sym_times, "gc-times");
SCM_SYMBOL (sym_cells_marked, "cells-marked");
SCM_SYMBOL (sym_cells_swept, "cells-swept");
SCM_SYMBOL (sym_malloc_yield, "malloc-yield");
SCM_SYMBOL (sym_cell_yield, "cell-yield");
SCM_SYMBOL (sym_protected_objects, "protected-objects");
SCM_SYMBOL (sym_total_cells_allocated, "total-cells-allocated");




/* Number of calls to SCM_NEWCELL since startup.  */
unsigned scm_newcell_count;
unsigned scm_newcell2_count;


/* {Scheme Interface to GC}
 */
static SCM
tag_table_to_type_alist (void *closure, SCM key, SCM val, SCM acc)
{
  if (scm_is_integer (key))
    {
      int c_tag = scm_to_int (key);

      char const * name = scm_i_tag_name (c_tag);
      if (name != NULL)
	{
	  key = scm_from_locale_string (name);
	}
      else
	{
	  char s[100];
	  sprintf (s, "tag %d", c_tag);
	  key = scm_from_locale_string (s);
	}
    }
  
  return scm_cons (scm_cons (key, val), acc);
}

SCM_DEFINE (scm_gc_live_object_stats, "gc-live-object-stats", 0, 0, 0,
            (),
	    "Return an alist of statistics of the current live objects. ")
#define FUNC_NAME s_scm_gc_live_object_stats
{
  SCM tab = scm_make_hash_table (scm_from_int (57));
  SCM alist;

  scm_i_all_segments_statistics (tab);
  
  alist
    = scm_internal_hash_fold (&tag_table_to_type_alist, NULL, SCM_EOL, tab);
  
  return alist;
}
#undef FUNC_NAME     

extern int scm_gc_malloc_yield_percentage;
SCM_DEFINE (scm_gc_stats, "gc-stats", 0, 0, 0,
            (),
	    "Return an association list of statistics about Guile's current\n"
	    "use of storage.\n")
#define FUNC_NAME s_scm_gc_stats
{
  long i = 0;
  SCM heap_segs = SCM_EOL ;
  unsigned long int local_scm_mtrigger;
  unsigned long int local_scm_mallocated;
  unsigned long int local_scm_heap_size;
  int local_scm_gc_cell_yield_percentage;
  int local_scm_gc_malloc_yield_percentage;
  unsigned long int local_scm_cells_allocated;
  unsigned long int local_scm_gc_time_taken;
  unsigned long int local_scm_gc_times;
  unsigned long int local_scm_gc_mark_time_taken;
  unsigned long int local_protected_obj_count;
  double local_scm_gc_cells_swept;
  double local_scm_gc_cells_marked;
  double local_scm_total_cells_allocated;
  SCM answer;
  unsigned long *bounds = 0;
  int table_size = scm_i_heap_segment_table_size;  
  SCM_CRITICAL_SECTION_START;

  /*
    temporarily store the numbers, so as not to cause GC.
   */
 
  bounds = malloc (sizeof (unsigned long)  * table_size * 2);
  if (!bounds)
    abort();
  for (i = table_size; i--; )
    {
      bounds[2*i] = (unsigned long)scm_i_heap_segment_table[i]->bounds[0];
      bounds[2*i+1] = (unsigned long)scm_i_heap_segment_table[i]->bounds[1];
    }


  /* Below, we cons to produce the resulting list.  We want a snapshot of
   * the heap situation before consing.
   */
  local_scm_mtrigger = scm_mtrigger;
  local_scm_mallocated = scm_mallocated;
  local_scm_heap_size = SCM_HEAP_SIZE;

  local_scm_cells_allocated = scm_cells_allocated;
  
  local_scm_gc_time_taken = scm_gc_time_taken;
  local_scm_gc_mark_time_taken = scm_gc_mark_time_taken;
  local_scm_gc_times = scm_gc_times;
  local_scm_gc_malloc_yield_percentage = scm_gc_malloc_yield_percentage;
  local_scm_gc_cell_yield_percentage=  scm_gc_cell_yield_percentage;
  local_protected_obj_count = protected_obj_count;
  local_scm_gc_cells_swept =
    (double) scm_gc_cells_swept_acc
    + (double) scm_gc_cells_swept;
  local_scm_gc_cells_marked = scm_gc_cells_marked_acc 
    +(double) scm_gc_cells_swept 
    -(double) scm_gc_cells_collected;

  local_scm_total_cells_allocated = scm_gc_cells_allocated_acc
    + (double) (scm_cells_allocated - scm_last_cells_allocated);

  for (i = table_size; i--;)
    {
      heap_segs = scm_cons (scm_cons (scm_from_ulong (bounds[2*i]),
				      scm_from_ulong (bounds[2*i+1])),
			    heap_segs);
    }
  /* njrev: can any of these scm_cons's or scm_list_n signal a memory
     error?  If so we need a frame here. */
  answer =
    scm_list_n (scm_cons (sym_gc_time_taken,
			  scm_from_ulong (local_scm_gc_time_taken)),
		scm_cons (sym_cells_allocated,
			  scm_from_ulong (local_scm_cells_allocated)),
		scm_cons (sym_total_cells_allocated,
			  scm_from_ulong (local_scm_total_cells_allocated)),
		scm_cons (sym_heap_size,
			  scm_from_ulong (local_scm_heap_size)),
		scm_cons (sym_mallocated,
			  scm_from_ulong (local_scm_mallocated)),
		scm_cons (sym_mtrigger,
			  scm_from_ulong (local_scm_mtrigger)),
		scm_cons (sym_times,
			  scm_from_ulong (local_scm_gc_times)),
		scm_cons (sym_gc_mark_time_taken,
			  scm_from_ulong (local_scm_gc_mark_time_taken)),
		scm_cons (sym_cells_marked,
			  scm_from_double (local_scm_gc_cells_marked)),
		scm_cons (sym_cells_swept,
			  scm_from_double (local_scm_gc_cells_swept)),
		scm_cons (sym_malloc_yield,
			  scm_from_long(local_scm_gc_malloc_yield_percentage)),
		scm_cons (sym_cell_yield,
			  scm_from_long (local_scm_gc_cell_yield_percentage)),
		scm_cons (sym_protected_objects,
			  scm_from_ulong (local_protected_obj_count)),
		scm_cons (sym_heap_segments, heap_segs),
		SCM_UNDEFINED);
  SCM_CRITICAL_SECTION_END;
  
  free (bounds);
  return answer;
}
#undef FUNC_NAME

static void
gc_start_stats (const char *what SCM_UNUSED)
{
  t_before_gc = scm_c_get_internal_run_time ();

  scm_gc_cells_marked_acc += (double) scm_gc_cells_swept
    - (double) scm_gc_cells_collected;
  scm_gc_cells_swept_acc += (double) scm_gc_cells_swept;

  scm_gc_cell_yield_percentage = ( scm_gc_cells_collected * 100 ) / SCM_HEAP_SIZE; 
  
  scm_gc_cells_swept = 0;
  scm_gc_cells_collected_1 = scm_gc_cells_collected;

  /*
    CELLS SWEPT is another word for the number of cells that were
    examined during GC. YIELD is the number that we cleaned
    out. MARKED is the number that weren't cleaned. 
   */
  scm_gc_cells_collected = 0;
  scm_gc_malloc_collected = 0;
  scm_gc_ports_collected = 0;
}

static void
gc_end_stats ()
{
  unsigned long t = scm_c_get_internal_run_time ();
  scm_gc_time_taken += (t - t_before_gc);

  ++scm_gc_times;
}


SCM_DEFINE (scm_object_address, "object-address", 1, 0, 0,
            (SCM obj),
	    "Return an integer that for the lifetime of @var{obj} is uniquely\n"
	    "returned by this function for @var{obj}")
#define FUNC_NAME s_scm_object_address
{
  return scm_from_ulong (SCM_UNPACK (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_gc, "gc", 0, 0, 0,
           (),
	    "Scans all of SCM objects and reclaims for further use those that are\n"
	    "no longer accessible.")
#define FUNC_NAME s_scm_gc
{
  scm_i_scm_pthread_mutex_lock (&scm_i_sweep_mutex);
  scm_gc_running_p = 1;
  scm_i_gc ("call");
  /* njrev: It looks as though other places, e.g. scm_realloc,
     can call scm_i_gc without acquiring the sweep mutex.  Does this
     matter?  Also scm_i_gc (or its descendants) touch the
     scm_sys_protects, which are protected in some cases
     (e.g. scm_permobjs above in scm_gc_stats) by a critical section,
     not by the sweep mutex.  Shouldn't all the GC-relevant objects be
     protected in the same way? */
  scm_gc_running_p = 0;
  scm_i_pthread_mutex_unlock (&scm_i_sweep_mutex);
  scm_c_hook_run (&scm_after_gc_c_hook, 0);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




/* The master is global and common while the freelist will be
 * individual for each thread.
 */

SCM
scm_gc_for_newcell (scm_t_cell_type_statistics *freelist, SCM *free_cells)
{
  SCM cell;
  int did_gc = 0;
 
  scm_i_scm_pthread_mutex_lock (&scm_i_sweep_mutex);
  scm_gc_running_p = 1;

  *free_cells = scm_i_sweep_some_segments (freelist);
  if (*free_cells == SCM_EOL && scm_i_gc_grow_heap_p (freelist))
    {
      freelist->heap_segment_idx = scm_i_get_new_heap_segment (freelist, abort_on_error);
      *free_cells = scm_i_sweep_some_segments (freelist);
    }

  if (*free_cells == SCM_EOL)
    {
      /*
	with the advent of lazy sweep, GC yield is only known just
	before doing the GC.
      */
      scm_i_adjust_min_yield (freelist);

      /*
	out of fresh cells. Try to get some new ones.
       */

      did_gc = 1;
      scm_i_gc ("cells");

      *free_cells = scm_i_sweep_some_segments (freelist);
    }
  
  if (*free_cells == SCM_EOL)
    {
      /*
	failed getting new cells. Get new juice or die.
       */
      freelist->heap_segment_idx = scm_i_get_new_heap_segment (freelist, abort_on_error);
      *free_cells = scm_i_sweep_some_segments (freelist);
    }
  
  if (*free_cells == SCM_EOL)
    abort ();

  cell = *free_cells;

  *free_cells = SCM_FREE_CELL_CDR (cell);

  scm_gc_running_p = 0;
  scm_i_pthread_mutex_unlock (&scm_i_sweep_mutex);

  if (did_gc)
    scm_c_hook_run (&scm_after_gc_c_hook, 0);

  return cell;
}


scm_t_c_hook scm_before_gc_c_hook;
scm_t_c_hook scm_before_mark_c_hook;
scm_t_c_hook scm_before_sweep_c_hook;
scm_t_c_hook scm_after_sweep_c_hook;
scm_t_c_hook scm_after_gc_c_hook;

/* Must be called while holding scm_i_sweep_mutex.
 */

void
scm_i_gc (const char *what)
{
  scm_i_thread_put_to_sleep ();

  scm_c_hook_run (&scm_before_gc_c_hook, 0);

#ifdef DEBUGINFO
  fprintf (stderr,"gc reason %s\n", what);
  
  fprintf (stderr,
	   scm_is_null (*SCM_FREELIST_LOC (scm_i_freelist))
	   ? "*"
	   : (scm_is_null (*SCM_FREELIST_LOC (scm_i_freelist2)) ? "o" : "m"));
#endif

  gc_start_stats (what);

  /*
    Set freelists to NULL so scm_cons() always triggers gc, causing
    the assertion above to fail.
  */
  *SCM_FREELIST_LOC (scm_i_freelist) = SCM_EOL;
  *SCM_FREELIST_LOC (scm_i_freelist2) = SCM_EOL;
  
  /*
    Let's finish the sweep. The conservative GC might point into the
    garbage, and marking that would create a mess.
   */
  scm_i_sweep_all_segments("GC");
  if (scm_mallocated < scm_i_deprecated_memory_return)
    {
      /* The byte count of allocated objects has underflowed.  This is
	 probably because you forgot to report the sizes of objects you
	 have allocated, by calling scm_done_malloc or some such.  When
	 the GC freed them, it subtracted their size from
	 scm_mallocated, which underflowed.  */
      fprintf (stderr,
	       "scm_gc_sweep: Byte count of allocated objects has underflowed.\n"
	       "This is probably because the GC hasn't been correctly informed\n"
	       "about object sizes\n");
      abort ();
    }
  scm_mallocated -= scm_i_deprecated_memory_return;

  
  /* Mark */

  scm_c_hook_run (&scm_before_mark_c_hook, 0);
  scm_mark_all ();
  scm_gc_mark_time_taken += (scm_c_get_internal_run_time () - t_before_gc);

  /* Sweep

    TODO: the after_sweep hook should probably be moved to just before
    the mark, since that's where the sweep is finished in lazy
    sweeping.

    MDJ 030219 <djurfeldt@nada.kth.se>: No, probably not.  The
    original meaning implied at least two things: that it would be
    called when

      1. the freelist is re-initialized (no evaluation possible, though)
      
    and
    
      2. the heap is "fresh"
         (it is well-defined what data is used and what is not)

    Neither of these conditions would hold just before the mark phase.
    
    Of course, the lazy sweeping has muddled the distinction between
    scm_before_sweep_c_hook and scm_after_sweep_c_hook, but even if
    there were no difference, it would still be useful to have two
    distinct classes of hook functions since this can prevent some
    bad interference when several modules adds gc hooks.
   */

  scm_c_hook_run (&scm_before_sweep_c_hook, 0);
  scm_gc_sweep ();
  scm_c_hook_run (&scm_after_sweep_c_hook, 0);

  gc_end_stats ();

  scm_i_thread_wake_up ();

  /*
    For debugging purposes, you could do
    scm_i_sweep_all_segments("debug"), but then the remains of the
    cell aren't left to analyse.
   */
}


/* {GC Protection Helper Functions}
 */


/*
 * If within a function you need to protect one or more scheme objects from
 * garbage collection, pass them as parameters to one of the
 * scm_remember_upto_here* functions below.  These functions don't do
 * anything, but since the compiler does not know that they are actually
 * no-ops, it will generate code that calls these functions with the given
 * parameters.  Therefore, you can be sure that the compiler will keep those
 * scheme values alive (on the stack or in a register) up to the point where
 * scm_remember_upto_here* is called.  In other words, place the call to
 * scm_remember_upto_here* _behind_ the last code in your function, that
 * depends on the scheme object to exist.
 *
 * Example: We want to make sure that the string object str does not get
 * garbage collected during the execution of 'some_function' in the code
 * below, because otherwise the characters belonging to str would be freed and
 * 'some_function' might access freed memory.  To make sure that the compiler
 * keeps str alive on the stack or in a register such that it is visible to
 * the conservative gc we add the call to scm_remember_upto_here_1 _after_ the
 * call to 'some_function'.  Note that this would not be necessary if str was
 * used anyway after the call to 'some_function'.
 *   char *chars = scm_i_string_chars (str);
 *   some_function (chars);
 *   scm_remember_upto_here_1 (str);  // str will be alive up to this point.
 */

/* Remove any macro versions of these while defining the functions.
   Functions are always included in the library, for upward binary
   compatibility and in case combinations of GCC and non-GCC are used.  */
#undef scm_remember_upto_here_1
#undef scm_remember_upto_here_2

void
scm_remember_upto_here_1 (SCM obj SCM_UNUSED)
{
  /* Empty.  Protects a single object from garbage collection. */
}

void
scm_remember_upto_here_2 (SCM obj1 SCM_UNUSED, SCM obj2 SCM_UNUSED)
{
  /* Empty.  Protects two objects from garbage collection. */
}

void
scm_remember_upto_here (SCM obj SCM_UNUSED, ...)
{
  /* Empty.  Protects any number of objects from garbage collection. */
}

/*
  These crazy functions prevent garbage collection
  of arguments after the first argument by
  ensuring they remain live throughout the
  function because they are used in the last
  line of the code block.
  It'd be better to have a nice compiler hint to
  aid the conservative stack-scanning GC. --03/09/00 gjb */
SCM
scm_return_first (SCM elt, ...)
{
  return elt;
}

int
scm_return_first_int (int i, ...)
{
  return i;
}


SCM
scm_permanent_object (SCM obj)
{
  SCM cell = scm_cons (obj, SCM_EOL);
  SCM_CRITICAL_SECTION_START;
  SCM_SETCDR (cell, scm_permobjs);
  scm_permobjs = cell;
  SCM_CRITICAL_SECTION_END;
  return obj;
}


/* Protect OBJ from the garbage collector.  OBJ will not be freed, even if all
   other references are dropped, until the object is unprotected by calling
   scm_gc_unprotect_object (OBJ).  Calls to scm_gc_protect/unprotect_object nest,
   i. e. it is possible to protect the same object several times, but it is
   necessary to unprotect the object the same number of times to actually get
   the object unprotected.  It is an error to unprotect an object more often
   than it has been protected before.  The function scm_protect_object returns
   OBJ.
*/

/* Implementation note:  For every object X, there is a counter which
   scm_gc_protect_object(X) increments and scm_gc_unprotect_object(X) decrements.
*/



SCM
scm_gc_protect_object (SCM obj)
{
  SCM handle;

  /* This critical section barrier will be replaced by a mutex. */
  /* njrev: Indeed; if my comment above is correct, there is the same
     critsec/mutex inconsistency here. */
  SCM_CRITICAL_SECTION_START;

  handle = scm_hashq_create_handle_x (scm_protects, obj, scm_from_int (0));
  SCM_SETCDR (handle, scm_sum (SCM_CDR (handle), scm_from_int (1)));

  protected_obj_count ++;
  
  SCM_CRITICAL_SECTION_END;

  return obj;
}


/* Remove any protection for OBJ established by a prior call to
   scm_protect_object.  This function returns OBJ.

   See scm_protect_object for more information.  */
SCM
scm_gc_unprotect_object (SCM obj)
{
  SCM handle;

  /* This critical section barrier will be replaced by a mutex. */
  /* njrev: and again. */
  SCM_CRITICAL_SECTION_START;

  if (scm_gc_running_p)
    {
      fprintf (stderr, "scm_unprotect_object called during GC.\n");
      abort ();
    }
 
  handle = scm_hashq_get_handle (scm_protects, obj);

  if (scm_is_false (handle))
    {
      fprintf (stderr, "scm_unprotect_object called on unprotected object\n");
      abort ();
    }
  else
    {
      SCM count = scm_difference (SCM_CDR (handle), scm_from_int (1));
      if (scm_is_eq (count, scm_from_int (0)))
	scm_hashq_remove_x (scm_protects, obj);
      else
	SCM_SETCDR (handle, count);
    }
  protected_obj_count --;

  SCM_CRITICAL_SECTION_END;

  return obj;
}

void
scm_gc_register_root (SCM *p)
{
  SCM handle;
  SCM key = scm_from_ulong ((unsigned long) p);

  /* This critical section barrier will be replaced by a mutex. */
  /* njrev: and again. */
  SCM_CRITICAL_SECTION_START;

  handle = scm_hashv_create_handle_x (scm_gc_registered_roots, key,
				      scm_from_int (0));
  /* njrev: note also that the above can probably signal an error */
  SCM_SETCDR (handle, scm_sum (SCM_CDR (handle), scm_from_int (1)));

  SCM_CRITICAL_SECTION_END;
}

void
scm_gc_unregister_root (SCM *p)
{
  SCM handle;
  SCM key = scm_from_ulong ((unsigned long) p);

  /* This critical section barrier will be replaced by a mutex. */
  /* njrev: and again. */
  SCM_CRITICAL_SECTION_START;

  handle = scm_hashv_get_handle (scm_gc_registered_roots, key);

  if (scm_is_false (handle))
    {
      fprintf (stderr, "scm_gc_unregister_root called on unregistered root\n");
      abort ();
    }
  else
    {
      SCM count = scm_difference (SCM_CDR (handle), scm_from_int (1));
      if (scm_is_eq (count, scm_from_int (0)))
	scm_hashv_remove_x (scm_gc_registered_roots, key);
      else
	SCM_SETCDR (handle, count);
    }

  SCM_CRITICAL_SECTION_END;
}

void
scm_gc_register_roots (SCM *b, unsigned long n)
{
  SCM *p = b;
  for (; p < b + n; ++p)
    scm_gc_register_root (p);
}

void
scm_gc_unregister_roots (SCM *b, unsigned long n)
{
  SCM *p = b;
  for (; p < b + n; ++p)
    scm_gc_unregister_root (p);
}

int scm_i_terminating;




/*
  MOVE THIS FUNCTION. IT DOES NOT HAVE ANYTHING TODO WITH GC.
 */

/* Get an integer from an environment variable.  */
int
scm_getenv_int (const char *var, int def)
{
  char *end = 0;
  char *val = getenv (var);
  long res = def;
  if (!val)
    return def;
  res = strtol (val, &end, 10);
  if (end == val)
    return def;
  return res;
}

void
scm_storage_prehistory ()
{
  scm_c_hook_init (&scm_before_gc_c_hook, 0, SCM_C_HOOK_NORMAL);
  scm_c_hook_init (&scm_before_mark_c_hook, 0, SCM_C_HOOK_NORMAL);
  scm_c_hook_init (&scm_before_sweep_c_hook, 0, SCM_C_HOOK_NORMAL);
  scm_c_hook_init (&scm_after_sweep_c_hook, 0, SCM_C_HOOK_NORMAL);
  scm_c_hook_init (&scm_after_gc_c_hook, 0, SCM_C_HOOK_NORMAL);
}

scm_i_pthread_mutex_t scm_i_gc_admin_mutex = SCM_I_PTHREAD_MUTEX_INITIALIZER;

int
scm_init_storage ()
{
  size_t j;

  j = SCM_NUM_PROTECTS;
  while (j)
    scm_sys_protects[--j] = SCM_BOOL_F;

  scm_gc_init_freelist();
  scm_gc_init_malloc ();

  j = SCM_HEAP_SEG_SIZE;

  
  /* Initialise the list of ports.  */
  scm_i_port_table = (scm_t_port **)
    malloc (sizeof (scm_t_port *) * scm_i_port_table_room);
  if (!scm_i_port_table)
    return 1;

#if 0
  /* We can't have a cleanup handler since we have no thread to run it
     in. */

#ifdef HAVE_ATEXIT
  atexit (cleanup);
#else
#ifdef HAVE_ON_EXIT
  on_exit (cleanup, 0);
#endif
#endif

#endif

  scm_stand_in_procs = scm_make_weak_key_hash_table (scm_from_int (257));
  scm_permobjs = SCM_EOL;
  scm_protects = scm_c_make_hash_table (31);
  scm_gc_registered_roots = scm_c_make_hash_table (31);

  return 0;
}



SCM scm_after_gc_hook;

static SCM gc_async;

/* The function gc_async_thunk causes the execution of the after-gc-hook.  It
 * is run after the gc, as soon as the asynchronous events are handled by the
 * evaluator.
 */
static SCM
gc_async_thunk (void)
{
  scm_c_run_hook (scm_after_gc_hook, SCM_EOL);
  return SCM_UNSPECIFIED;
}


/* The function mark_gc_async is run by the scm_after_gc_c_hook at the end of
 * the garbage collection.  The only purpose of this function is to mark the
 * gc_async (which will eventually lead to the execution of the
 * gc_async_thunk).
 */
static void *
mark_gc_async (void * hook_data SCM_UNUSED,
	       void *fn_data SCM_UNUSED,
	       void *data SCM_UNUSED)
{
  /* If cell access debugging is enabled, the user may choose to perform
   * additional garbage collections after an arbitrary number of cell
   * accesses.  We don't want the scheme level after-gc-hook to be performed
   * for each of these garbage collections for the following reason: The
   * execution of the after-gc-hook causes cell accesses itself.  Thus, if the
   * after-gc-hook was performed with every gc, and if the gc was performed
   * after a very small number of cell accesses, then the number of cell
   * accesses during the execution of the after-gc-hook will suffice to cause
   * the execution of the next gc.  Then, guile would keep executing the
   * after-gc-hook over and over again, and would never come to do other
   * things.
   *
   * To overcome this problem, if cell access debugging with additional
   * garbage collections is enabled, the after-gc-hook is never run by the
   * garbage collecter.  When running guile with cell access debugging and the
   * execution of the after-gc-hook is desired, then it is necessary to run
   * the hook explicitly from the user code.  This has the effect, that from
   * the scheme level point of view it seems that garbage collection is
   * performed with a much lower frequency than it actually is.  Obviously,
   * this will not work for code that depends on a fixed one to one
   * relationship between the execution counts of the C level garbage
   * collection hooks and the execution count of the scheme level
   * after-gc-hook.
   */

#if (SCM_DEBUG_CELL_ACCESSES == 1)
  if (scm_debug_cells_gc_interval == 0)
    scm_system_async_mark (gc_async);
#else
  scm_system_async_mark (gc_async);
#endif

  return NULL;
}

void
scm_init_gc ()
{
  scm_gc_init_mark ();

  scm_after_gc_hook = scm_permanent_object (scm_make_hook (SCM_INUM0));
  scm_c_define ("after-gc-hook", scm_after_gc_hook);

  gc_async = scm_c_make_subr ("%gc-thunk", scm_tc7_subr_0,
			      gc_async_thunk);

  scm_c_hook_add (&scm_after_gc_c_hook, mark_gc_async, NULL, 0);

#include "libguile/gc.x"
}

#ifdef __ia64__
# ifdef __hpux
#  include <sys/param.h>
#  include <sys/pstat.h>
void *
scm_ia64_register_backing_store_base (void)
{
  struct pst_vm_status vm_status;
  int i = 0;
  while (pstat_getprocvm (&vm_status, sizeof (vm_status), 0, i++) == 1)
    if (vm_status.pst_type == PS_RSESTACK)
      return (void *) vm_status.pst_vaddr;
  abort ();
}
void *
scm_ia64_ar_bsp (const void *ctx)
{
  uint64_t bsp;
  __uc_get_ar_bsp(ctx, &bsp);
  return (void *) bsp;
}
# endif /* hpux */
# ifdef linux
#  include <ucontext.h>
void *
scm_ia64_register_backing_store_base (void)
{
  extern void *__libc_ia64_register_backing_store_base;
  return __libc_ia64_register_backing_store_base;
}
void *
scm_ia64_ar_bsp (const void *opaque)
{
  const ucontext_t *ctx = opaque;
  return (void *) ctx->uc_mcontext.sc_ar_bsp;
}
# endif	/* linux */
#endif /* __ia64__ */

void
scm_gc_sweep (void)
#define FUNC_NAME "scm_gc_sweep"
{
  scm_i_deprecated_memory_return = 0;

  scm_i_gc_sweep_freelist_reset (&scm_i_master_freelist);
  scm_i_gc_sweep_freelist_reset (&scm_i_master_freelist2);

  /*
    NOTHING HERE: LAZY SWEEPING ! 
   */
  scm_i_reset_segments ();
  
  *SCM_FREELIST_LOC (scm_i_freelist) = SCM_EOL;
  *SCM_FREELIST_LOC (scm_i_freelist2) = SCM_EOL;

  /* Invalidate the freelists of other threads. */
  scm_i_thread_invalidate_freelists ();
}

#undef FUNC_NAME



/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
