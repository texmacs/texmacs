/*	Copyright (C) 1995,1996, 1997, 2000, 2001, 2004, 2005, 2006, 2008 Free Software Foundation, Inc.
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
#include "libguile/ports.h"
#include "libguile/smob.h"

#include "libguile/validate.h"
#include "libguile/arbiters.h"


/* FETCH_STORE sets "fet" to the value fetched from "mem" and then stores
   "sto" there.  The fetch and store are done atomically, so once the fetch
   has been done no other thread or processor can fetch from there before
   the store is done.

   The operands are scm_t_bits, fet and sto are plain variables, mem is a
   memory location (ie. an lvalue).

   ENHANCE-ME: Add more cpu-specifics.  glibc atomicity.h has some of the
   sort of thing required.  FETCH_STORE could become some sort of
   compare-and-store if that better suited what various cpus do.  */

#if defined (__GNUC__) && defined (i386) && SIZEOF_SCM_T_BITS == 4
/* This is for i386 with the normal 32-bit scm_t_bits.  The xchg instruction
   is atomic on a single processor, and it automatically asserts the "lock"
   bus signal so it's atomic on a multi-processor (no need for the lock
   prefix on the instruction).

   The mem operand is read-write but "+" is not used since old gcc
   (eg. 2.7.2) doesn't support that.  "1" for the mem input doesn't work
   (eg. gcc 3.3) when mem is a pointer dereference like current usage below.
   Having mem as a plain input should be ok though.  It tells gcc the value
   is live, but as an "m" gcc won't fetch it itself (though that would be
   harmless).  */

#define FETCH_STORE(fet,mem,sto)                \
  do {                                          \
    asm ("xchg %0, %1"                          \
         : "=r" (fet), "=m" (mem)               \
         : "0"  (sto), "m"  (mem));             \
  } while (0)
#endif

#ifndef FETCH_STORE
/* This is a generic version, with a mutex to ensure the operation is
   atomic.  Unfortunately this approach probably makes arbiters no faster
   than mutexes (though still using less memory of course), so some
   CPU-specifics are highly desirable.  */
#define FETCH_STORE(fet,mem,sto)                        \
  do {                                                  \
    scm_i_scm_pthread_mutex_lock (&scm_i_misc_mutex);   \
    (fet) = (mem);                                      \
    (mem) = (sto);                                      \
    scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);     \
  } while (0)
#endif


static scm_t_bits scm_tc16_arbiter;


#define SCM_LOCK_VAL         (scm_tc16_arbiter | (1L << 16))
#define SCM_UNLOCK_VAL       scm_tc16_arbiter
#define SCM_ARB_LOCKED(arb)  ((SCM_CELL_WORD_0 (arb)) & (1L << 16))


static int 
arbiter_print (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<arbiter ", port);
  if (SCM_ARB_LOCKED (exp))
    scm_puts ("locked ", port);
  scm_iprin1 (SCM_PACK (SCM_SMOB_DATA (exp)), port, pstate);
  scm_putc ('>', port);
  return !0;
}

SCM_DEFINE (scm_make_arbiter, "make-arbiter", 1, 0, 0, 
	    (SCM name),
	    "Return an arbiter object, initially unlocked.  Currently\n"
	    "@var{name} is only used for diagnostic output.")
#define FUNC_NAME s_scm_make_arbiter
{
  SCM_RETURN_NEWSMOB (scm_tc16_arbiter, SCM_UNPACK (name));
}
#undef FUNC_NAME


/* The atomic FETCH_STORE here is so two threads can't both see the arbiter
   unlocked and return #t.  The arbiter itself wouldn't be corrupted by
   this, but two threads both getting #t would be contrary to the intended
   semantics.  */

SCM_DEFINE (scm_try_arbiter, "try-arbiter", 1, 0, 0, 
	    (SCM arb),
	    "If @var{arb} is unlocked, then lock it and return @code{#t}.\n"
	    "If @var{arb} is already locked, then do nothing and return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_try_arbiter
{
  scm_t_bits old;
  SCM_VALIDATE_SMOB (1, arb, arbiter);
  FETCH_STORE (old, * (scm_t_bits *) SCM_CELL_OBJECT_LOC(arb,0), SCM_LOCK_VAL);
  return scm_from_bool (old == SCM_UNLOCK_VAL);
}
#undef FUNC_NAME


/* The atomic FETCH_STORE here is so two threads can't both see the arbiter
   locked and return #t.  The arbiter itself wouldn't be corrupted by this,
   but we don't want two threads both thinking they were the unlocker.  The
   intended usage is for the code which locked to be responsible for
   unlocking, but we guarantee the return value even if multiple threads
   compete.  */

SCM_DEFINE (scm_release_arbiter, "release-arbiter", 1, 0, 0,
	    (SCM arb),
	    "If @var{arb} is locked, then unlock it and return @code{#t}.\n"
	    "If @var{arb} is already unlocked, then do nothing and return\n"
	    "@code{#f}.\n"
	    "\n"
	    "Typical usage is for the thread which locked an arbiter to\n"
	    "later release it, but that's not required, any thread can\n"
	    "release it.")
#define FUNC_NAME s_scm_release_arbiter
{
  scm_t_bits old;
  SCM_VALIDATE_SMOB (1, arb, arbiter);
  FETCH_STORE (old, *(scm_t_bits*)SCM_CELL_OBJECT_LOC(arb,0), SCM_UNLOCK_VAL);
  return scm_from_bool (old == SCM_LOCK_VAL);
}
#undef FUNC_NAME



void
scm_init_arbiters ()
{
  scm_tc16_arbiter = scm_make_smob_type ("arbiter", 0);
  scm_set_smob_mark (scm_tc16_arbiter, scm_markcdr);
  scm_set_smob_print (scm_tc16_arbiter, arbiter_print);
#include "libguile/arbiters.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
