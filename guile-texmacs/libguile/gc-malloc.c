/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2002, 2003, 2004, 2006, 2008 Free Software Foundation, Inc.
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

#include "libguile/validate.h"
#include "libguile/deprecation.h"
#include "libguile/gc.h"

#include "libguile/private-gc.h"

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
  INIT_MALLOC_LIMIT is the initial amount of malloc usage which will
  trigger a GC.
  
  After startup (at the guile> prompt), we have approximately 100k of
  alloced memory, which won't go away on GC. Let's set the init such
  that we get a nice yield on the next allocation:
*/
#define SCM_DEFAULT_INIT_MALLOC_LIMIT 200*1024
#define SCM_DEFAULT_MALLOC_MINYIELD 40

/* #define DEBUGINFO */

static int scm_i_minyield_malloc;

void
scm_gc_init_malloc (void)
{
  scm_mtrigger = scm_getenv_int ("GUILE_INIT_MALLOC_LIMIT",
				 SCM_DEFAULT_INIT_MALLOC_LIMIT);
  scm_i_minyield_malloc =  scm_getenv_int ("GUILE_MIN_YIELD_MALLOC",
					   SCM_DEFAULT_MALLOC_MINYIELD);

  if (scm_i_minyield_malloc >= 100)
    scm_i_minyield_malloc = 99;
  if (scm_i_minyield_malloc < 1)
    scm_i_minyield_malloc = 1;

  if (scm_mtrigger < 0)
    scm_mtrigger = SCM_DEFAULT_INIT_MALLOC_LIMIT;
}



/* Function for non-cell memory management.
 */

void *
scm_realloc (void *mem, size_t size)
{
  void *ptr;

  SCM_SYSCALL (ptr = realloc (mem, size));
  if (ptr)
    return ptr;

  scm_i_scm_pthread_mutex_lock (&scm_i_sweep_mutex);
  scm_gc_running_p = 1;

  scm_i_sweep_all_segments ("realloc");
  
  SCM_SYSCALL (ptr = realloc (mem, size));
  if (ptr)
    { 
      scm_gc_running_p = 0;
      scm_i_pthread_mutex_unlock (&scm_i_sweep_mutex);
      return ptr;
    }

  scm_i_gc ("realloc");
  scm_i_sweep_all_segments ("realloc");
  
  scm_gc_running_p = 0;
  scm_i_pthread_mutex_unlock (&scm_i_sweep_mutex);
  
  SCM_SYSCALL (ptr = realloc (mem, size));
  if (ptr)
    return ptr;

  scm_memory_error ("realloc");
}

void *
scm_malloc (size_t sz)
{
  return scm_realloc (NULL, sz);
}

/*
  Hmm. Should we use the C convention for arguments (i.e. N_ELTS,
  SIZEOF_ELT)? --hwn
 */
void *
scm_calloc (size_t sz)
{
  void * ptr;

  /*
    By default, try to use calloc, as it is likely more efficient than
    calling memset by hand.
   */
  SCM_SYSCALL (ptr = calloc (sz, 1));
  if (ptr)
    return ptr;
  
  ptr = scm_realloc (NULL, sz);
  memset (ptr, 0x0, sz);
  return ptr;
}


char *
scm_strndup (const char *str, size_t n)
{
  char *dst = scm_malloc (n + 1);
  memcpy (dst, str, n);
  dst[n] = 0;
  return dst;
}

char *
scm_strdup (const char *str)
{
  return scm_strndup (str, strlen (str));
}

static void
decrease_mtrigger (size_t size, const char * what)
{
  scm_i_pthread_mutex_lock (&scm_i_gc_admin_mutex);

  if (size > scm_mallocated)
    {
      fprintf (stderr, "`scm_mallocated' underflow.  This means that more "
	       "memory was unregistered\n"
	       "via `scm_gc_unregister_collectable_memory ()' than "
	       "registered.\n");
      abort ();
    }

  scm_mallocated -= size;
  scm_gc_malloc_collected += size;
  scm_i_pthread_mutex_unlock (&scm_i_gc_admin_mutex);
}

static void
increase_mtrigger (size_t size, const char *what)
{
  size_t mallocated = 0;
  int overflow = 0, triggered = 0;

  scm_i_pthread_mutex_lock (&scm_i_gc_admin_mutex);
  if (ULONG_MAX - size < scm_mallocated)
    overflow = 1;
  else
    {
      scm_mallocated += size;
      mallocated = scm_mallocated;
      if (scm_mallocated > scm_mtrigger)
	triggered = 1;
    }
  scm_i_pthread_mutex_unlock (&scm_i_gc_admin_mutex);

  if (overflow)
    scm_memory_error ("Overflow of scm_mallocated: too much memory in use.");

  /*
    A program that uses a lot of malloced collectable memory (vectors,
    strings), will use a lot of memory off the cell-heap; it needs to
    do GC more often (before cells are exhausted), otherwise swapping
    and malloc management will tie it down.
   */
  if (triggered)
    {
      unsigned long prev_alloced;
      float yield;
      
      scm_i_scm_pthread_mutex_lock (&scm_i_sweep_mutex);
      scm_gc_running_p = 1;
      
      prev_alloced  = mallocated;
      scm_i_gc (what);
      scm_i_sweep_all_segments ("mtrigger");

      yield = (((float) prev_alloced - (float) scm_mallocated)
	       / (float) prev_alloced);
      
      scm_gc_malloc_yield_percentage = (int) (100  * yield);

#ifdef DEBUGINFO
      fprintf (stderr,  "prev %lud , now %lud, yield %4.2lf, want %d",
	       prev_alloced,
	       scm_mallocated,
	       100.0 * yield,
	       scm_i_minyield_malloc);
#endif
      
      if (yield < scm_i_minyield_malloc /  100.0)
	{
	  /*
	    We make the trigger a little larger, even; If you have a
	    program that builds up a lot of data in strings, then the
	    desired yield will never be satisfied.

	    Instead of getting bogged down, we let the mtrigger grow
	    strongly with it.
	   */
	  float no_overflow_trigger = scm_mallocated * 110.0;

	  no_overflow_trigger /= (float)  (100.0 - scm_i_minyield_malloc);

	  
	  if (no_overflow_trigger >= (float) ULONG_MAX)
	    scm_mtrigger = ULONG_MAX;
	  else
	    scm_mtrigger =  (unsigned long) no_overflow_trigger;
	  
#ifdef DEBUGINFO
	  fprintf (stderr, "Mtrigger sweep: ineffective. New trigger %d\n",
		   scm_mtrigger);
#endif
	}

      scm_gc_running_p = 0;
      scm_i_pthread_mutex_unlock (&scm_i_sweep_mutex);
    }
}

void
scm_gc_register_collectable_memory (void *mem, size_t size, const char *what)
{
  increase_mtrigger (size, what); 
#ifdef GUILE_DEBUG_MALLOC
  if (mem)
    scm_malloc_register (mem, what);
#endif
}


void
scm_gc_unregister_collectable_memory (void *mem, size_t size, const char *what)
{
  decrease_mtrigger (size, what);
#ifdef GUILE_DEBUG_MALLOC
  if (mem)
    scm_malloc_unregister (mem);
#endif
}

void *
scm_gc_malloc (size_t size, const char *what)
{
  /*
    The straightforward implementation below has the problem
     that it might call the GC twice, once in scm_malloc and then
     again in scm_gc_register_collectable_memory.  We don't really
     want the second GC since it will not find new garbage.

     Note: this is a theoretical peeve. In reality, malloc() never
     returns NULL. Usually, memory is overcommitted, and when you try
     to write it the program is killed with signal 11. --hwn
  */

  void *ptr = size ? scm_malloc (size) : NULL;
  scm_gc_register_collectable_memory (ptr, size, what);
  return ptr;
}

void *
scm_gc_calloc (size_t size, const char *what)
{
  void *ptr = scm_gc_malloc (size, what);
  memset (ptr, 0x0, size);
  return ptr;
}


void *
scm_gc_realloc (void *mem, size_t old_size, size_t new_size, const char *what)
{
  void *ptr;

  /* XXX - see scm_gc_malloc. */


  /*    
  scm_realloc() may invalidate the block pointed to by WHERE, eg. by
  unmapping it from memory or altering the contents.  Since
  increase_mtrigger() might trigger a GC that would scan
  MEM, it is crucial that this call precedes realloc().
  */

  decrease_mtrigger (old_size, what);
  increase_mtrigger (new_size, what);

  ptr = scm_realloc (mem, new_size);

#ifdef GUILE_DEBUG_MALLOC
  if (mem)
    scm_malloc_reregister (mem, ptr, what);
#endif
  
  return ptr;
}

void
scm_gc_free (void *mem, size_t size, const char *what)
{
  scm_gc_unregister_collectable_memory (mem, size, what);
  if (mem)
    free (mem);
}

char *
scm_gc_strndup (const char *str, size_t n, const char *what)
{
  char *dst = scm_gc_malloc (n+1, what);
  memcpy (dst, str, n);
  dst[n] = 0;
  return dst;
}

char *
scm_gc_strdup (const char *str, const char *what)
{
  return scm_gc_strndup (str, strlen (str), what);
}

#if SCM_ENABLE_DEPRECATED == 1

/* {Deprecated front end to malloc}
 *
 * scm_must_malloc, scm_must_realloc, scm_must_free, scm_done_malloc,
 * scm_done_free
 *
 * These functions provide services comparable to malloc, realloc, and
 * free.  They should be used when allocating memory that will be under
 * control of the garbage collector, i.e., if the memory may be freed
 * during garbage collection.
 *
 * They are deprecated because they weren't really used the way
 * outlined above, and making sure to return the right amount from
 * smob free routines was sometimes difficult when dealing with nested
 * data structures.  We basically want everybody to review their code
 * and use the more symmetrical scm_gc_malloc/scm_gc_free functions
 * instead.  In some cases, where scm_must_malloc has been used
 * incorrectly (i.e. for non-GC-able memory), use scm_malloc/free.
 */

void *
scm_must_malloc (size_t size, const char *what)
{
  scm_c_issue_deprecation_warning
    ("scm_must_malloc is deprecated.  "
     "Use scm_gc_malloc and scm_gc_free instead.");

  return scm_gc_malloc (size, what);
}

void *
scm_must_realloc (void *where,
		  size_t old_size,
		  size_t size,
		  const char *what)
{
  scm_c_issue_deprecation_warning
    ("scm_must_realloc is deprecated.  "
     "Use scm_gc_realloc and scm_gc_free instead.");

  return scm_gc_realloc (where, old_size, size, what);
}

char *
scm_must_strndup (const char *str, size_t length)
{
  scm_c_issue_deprecation_warning
    ("scm_must_strndup is deprecated.  "
     "Use scm_gc_strndup and scm_gc_free instead.");

  return scm_gc_strndup (str, length, "string");
}

char *
scm_must_strdup (const char *str)
{
  scm_c_issue_deprecation_warning
    ("scm_must_strdup is deprecated.  "
     "Use scm_gc_strdup and scm_gc_free instead.");

  return scm_gc_strdup (str, "string");
}

void
scm_must_free (void *obj)
#define FUNC_NAME "scm_must_free"
{
  scm_c_issue_deprecation_warning
    ("scm_must_free is deprecated.  "
     "Use scm_gc_malloc and scm_gc_free instead.");

#ifdef GUILE_DEBUG_MALLOC
  scm_malloc_unregister (obj);
#endif
  if (obj)
    free (obj);
  else
    {
      fprintf (stderr,"freeing NULL pointer");
      abort ();
    }
}
#undef FUNC_NAME


void
scm_done_malloc (long size)
{
  scm_c_issue_deprecation_warning
    ("scm_done_malloc is deprecated.  "
     "Use scm_gc_register_collectable_memory instead.");

  if (size >= 0)
    scm_gc_register_collectable_memory (NULL, size, "foreign mallocs");
  else
    scm_gc_unregister_collectable_memory (NULL, -size, "foreign mallocs");
}

void
scm_done_free (long size)
{
  scm_c_issue_deprecation_warning
    ("scm_done_free is deprecated.  "
     "Use scm_gc_unregister_collectable_memory instead.");

  if (size >= 0)
    scm_gc_unregister_collectable_memory (NULL, size, "foreign mallocs");
  else
    scm_gc_register_collectable_memory (NULL, -size, "foreign mallocs");
}

#endif /* SCM_ENABLE_DEPRECATED == 1 */
