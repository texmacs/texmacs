/* Copyright (C) 1998,1999,2000,2001, 2006, 2008 Free Software Foundation, Inc.
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



/* This is an implementation of guardians as described in
 * R. Kent Dybvig, Carl Bruggeman, and David Eby (1993) "Guardians in
 * a Generation-Based Garbage Collector" ACM SIGPLAN Conference on
 * Programming Language Design and Implementation, June 1993
 * ftp://ftp.cs.indiana.edu/pub/scheme-repository/doc/pubs/guardians.ps.gz
 *
 * Original design:          Mikael Djurfeldt
 * Original implementation:  Michael Livshin
 * Hacked on since by:       everybody
 *
 * By this point, the semantics are actually quite different from
 * those described in the abovementioned paper.  The semantic changes
 * are there to improve safety and intuitiveness.  The interface is
 * still (mostly) the one described by the paper, however.
 *
 * Boiled down again:        Marius Vollmer
 *
 * Now they should again behave like those described in the paper.
 * Scheme guardians should be simple and friendly, not like the greedy
 * monsters we had...
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/async.h"
#include "libguile/ports.h"
#include "libguile/print.h"
#include "libguile/smob.h"
#include "libguile/validate.h"
#include "libguile/root.h"
#include "libguile/hashtab.h"
#include "libguile/weaks.h"
#include "libguile/deprecation.h"
#include "libguile/eval.h"

#include "libguile/guardians.h"


/* The live and zombies FIFOs are implemented as tconcs as described
   in Dybvig's paper.  This decouples addition and removal of elements
   so that no synchronization between these needs to take place.
*/

typedef struct t_tconc
{
  SCM head;
  SCM tail;
} t_tconc;

#define TCONC_EMPTYP(tc) (scm_is_eq ((tc).head, (tc).tail))

#define TCONC_IN(tc, obj, pair) \
do { \
  SCM_SETCAR ((tc).tail, obj); \
  SCM_SET_CELL_OBJECT_1 (pair, SCM_EOL); \
  SCM_SET_CELL_OBJECT_0 (pair, SCM_BOOL_F); \
  SCM_SETCDR ((tc).tail, pair); \
  (tc).tail = pair; \
} while (0)

#define TCONC_OUT(tc, res) \
do { \
  (res) = SCM_CAR ((tc).head); \
  (tc).head = SCM_CDR ((tc).head); \
} while (0)


static scm_t_bits tc16_guardian;

typedef struct t_guardian
{
  t_tconc live;
  t_tconc zombies;
  struct t_guardian *next;
} t_guardian;

#define GUARDIAN_P(x)    SCM_SMOB_PREDICATE(tc16_guardian, x)
#define GUARDIAN_DATA(x) ((t_guardian *) SCM_CELL_WORD_1 (x))

static t_guardian *guardians;

void
scm_i_init_guardians_for_gc ()
{
  guardians = NULL;
}

/* mark a guardian by adding it to the live guardian list.  */
static SCM
guardian_mark (SCM ptr)
{
  t_guardian *g = GUARDIAN_DATA (ptr);
  g->next = guardians;
  guardians = g;

  return SCM_BOOL_F;
}

/* Identify inaccessible objects and move them from the live list to
   the zombie list.  An object is inaccessible when it is unmarked at
   this point.  Therefore, the inaccessible objects are not marked yet
   since that would prevent them from being recognized as
   inaccessible.

   The pairs that form the life list itself are marked, tho.
*/
void
scm_i_identify_inaccessible_guardeds ()
{
  t_guardian *g;

  for (g = guardians; g; g = g->next)
    {
      SCM pair, next_pair;
      SCM *prev_ptr;

      for (pair = g->live.head, prev_ptr = &g->live.head;
	   !scm_is_eq (pair, g->live.tail);
	   pair = next_pair)
	{
	  SCM obj = SCM_CAR (pair);
	  next_pair = SCM_CDR (pair);
	  if (!SCM_GC_MARK_P (obj))
	    {
	      /* Unmarked, move to 'inaccessible' list.
	       */
	      *prev_ptr = next_pair;
	      TCONC_IN (g->zombies, obj, pair);
	    }
	  else
	    {
	      SCM_SET_GC_MARK (pair);
	      prev_ptr = SCM_CDRLOC (pair);
	    }
	}
      SCM_SET_GC_MARK (pair);
    }
}

int
scm_i_mark_inaccessible_guardeds ()
{
  t_guardian *g;
  int again = 0;

  /* We never need to see the guardians again that are processed here,
     so we clear the list.  Calling scm_gc_mark below might find new
     guardians, however (and other things), and we inform the GC about
     this by returning non-zero.  See scm_mark_all in gc-mark.c
  */

  g = guardians;
  guardians = NULL;

  for (; g; g = g->next)
    {
      SCM pair;

      for (pair = g->zombies.head;
	   !scm_is_eq (pair, g->zombies.tail);
	   pair = SCM_CDR (pair))
	{
	  if (!SCM_GC_MARK_P (pair))
	    {
	      scm_gc_mark (SCM_CAR (pair));
	      SCM_SET_GC_MARK (pair);
	      again = 1;
	    }
	}
      SCM_SET_GC_MARK (pair);
    }
  return again;
}

static size_t
guardian_free (SCM ptr)
{
  scm_gc_free (GUARDIAN_DATA (ptr), sizeof (t_guardian), "guardian");
  return 0;
}

static int
guardian_print (SCM guardian, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  t_guardian *g = GUARDIAN_DATA (guardian);
  
  scm_puts ("#<guardian ", port);
  scm_uintprint ((scm_t_bits) g, 16, port);

  scm_puts (" (reachable: ", port);
  scm_display (scm_length (SCM_CDR (g->live.head)), port);
  scm_puts (" unreachable: ", port);
  scm_display (scm_length (SCM_CDR (g->zombies.head)), port);
  scm_puts (")", port);

  scm_puts (">", port);

  return 1;
}

static void
scm_i_guard (SCM guardian, SCM obj)
{
  t_guardian *g = GUARDIAN_DATA (guardian);
  
  if (!SCM_IMP (obj))
    {
      SCM z;
      z = scm_cons (SCM_BOOL_F, SCM_BOOL_F);
      TCONC_IN (g->live, obj, z);
    }
}

static SCM
scm_i_get_one_zombie (SCM guardian)
{
  t_guardian *g = GUARDIAN_DATA (guardian);
  SCM res = SCM_BOOL_F;

  if (!TCONC_EMPTYP (g->zombies))
    TCONC_OUT (g->zombies, res);

  return res;
}

/* This is the Scheme entry point for each guardian: If OBJ is an
 * object, it's added to the guardian's live list.  If OBJ is unbound,
 * the next available unreachable object (or #f if none) is returned.
 *
 * If the second optional argument THROW_P is true (the default), then
 * an error is raised if GUARDIAN is greedy and OBJ is already greedily
 * guarded.  If THROW_P is false, #f is returned instead of raising the
 * error, and #t is returned if everything is fine.
 */ 
static SCM
guardian_apply (SCM guardian, SCM obj, SCM throw_p)
{
#if ENABLE_DEPRECATED
  if (!SCM_UNBNDP (throw_p))
    scm_c_issue_deprecation_warning
      ("Using the 'throw?' argument of a guardian is deprecated "
       "and ineffective.");
#endif

  if (!SCM_UNBNDP (obj))
    {
      scm_i_guard (guardian, obj);
      return SCM_UNSPECIFIED;
    }
  else
    return scm_i_get_one_zombie (guardian);
}

SCM_DEFINE (scm_make_guardian, "make-guardian", 0, 0, 0, 
	    (),
"Create a new guardian.  A guardian protects a set of objects from\n"
"garbage collection, allowing a program to apply cleanup or other\n"
"actions.\n"
"\n"
"@code{make-guardian} returns a procedure representing the guardian.\n"
"Calling the guardian procedure with an argument adds the argument to\n"
"the guardian's set of protected objects.  Calling the guardian\n"
"procedure without an argument returns one of the protected objects\n"
"which are ready for garbage collection, or @code{#f} if no such object\n"
"is available.  Objects which are returned in this way are removed from\n"
"the guardian.\n"
"\n"
"You can put a single object into a guardian more than once and you can\n"
"put a single object into more than one guardian.  The object will then\n"
"be returned multiple times by the guardian procedures.\n"
"\n"
"An object is eligible to be returned from a guardian when it is no\n"
"longer referenced from outside any guardian.\n"
"\n"
"There is no guarantee about the order in which objects are returned\n"
"from a guardian.  If you want to impose an order on finalization\n"
"actions, for example, you can do that by keeping objects alive in some\n"
"global data structure until they are no longer needed for finalizing\n"
"other objects.\n"
"\n"
"Being an element in a weak vector, a key in a hash table with weak\n"
"keys, or a value in a hash table with weak value does not prevent an\n"
"object from being returned by a guardian.  But as long as an object\n"
"can be returned from a guardian it will not be removed from such a\n"
"weak vector or hash table.  In other words, a weak link does not\n"
"prevent an object from being considered collectable, but being inside\n"
"a guardian prevents a weak link from being broken.\n"
"\n"
"A key in a weak key hash table can be though of as having a strong\n"
"reference to its associated value as long as the key is accessible.\n"
"Consequently, when the key only accessible from within a guardian, the\n"
"reference from the key to the value is also considered to be coming\n"
"from within a guardian.  Thus, if there is no other reference to the\n"
	    "value, it is eligible to be returned from a guardian.\n")
#define FUNC_NAME s_scm_make_guardian
{
  t_guardian *g = scm_gc_malloc (sizeof (t_guardian), "guardian");
  SCM z1 = scm_cons (SCM_BOOL_F, SCM_EOL);
  SCM z2 = scm_cons (SCM_BOOL_F, SCM_EOL);
  SCM z;

  /* A tconc starts out with one tail pair. */
  g->live.head = g->live.tail = z1;
  g->zombies.head = g->zombies.tail = z2;

  g->next = NULL;

  SCM_NEWSMOB (z, tc16_guardian, g);

  return z;
}
#undef FUNC_NAME

void
scm_init_guardians ()
{
  tc16_guardian = scm_make_smob_type ("guardian", 0);
  scm_set_smob_mark (tc16_guardian, guardian_mark);
  scm_set_smob_free (tc16_guardian, guardian_free);
  scm_set_smob_print (tc16_guardian, guardian_print);
#if ENABLE_DEPRECATED
  scm_set_smob_apply (tc16_guardian, guardian_apply, 0, 2, 0);
#else
  scm_set_smob_apply (tc16_guardian, guardian_apply, 0, 1, 0);
#endif

#include "libguile/guardians.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
