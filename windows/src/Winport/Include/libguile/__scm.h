/* classes: h_files */

#ifndef __SCMH
#define __SCMH
/*	Copyright (C) 1995, 1996, 1998, 1999, 2000 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */


/* "What's the difference between _scm.h and __scm.h?"

   _scm.h is not installed; it's only visible to the libguile sources
   themselves.

   __scm.h is installed, and is #included by <libguile.h>.  If both
   the client and libguile need some piece of information, and it
   doesn't fit well into the header file for any particular module, it
   should go in __scm.h.  */

#if defined (__GUILE_EXPORT__)
# define GUILE_API __declspec (dllexport)
#elif defined (__GUILE_IMPORT__)
# define GUILE_API __declspec (dllimport)
#else
# define GUILE_API __declspec(dllimport)
#endif

/* {Supported Options}
 *
 * These may be defined or undefined.
 */

/* Old async mechanism */
/* #define GUILE_OLD_ASYNC_CLICK */

/* #define GUILE_DEBUG_FREELIST */

/* If the compile FLAG `SCM_CAUTIOUS' is #defined then the number of
 * arguments is always checked for application of closures.  If the
 * compile FLAG `SCM_RECKLESS' is #defined then they are not checked.
 * Otherwise, number of argument checks for closures are made only when
 * the function position (whose value is the closure) of a combination is
 * not an ILOC or GLOC.  When the function position of a combination is a
 * symbol it will be checked only the first time it is evaluated because
 * it will then be replaced with an ILOC or GLOC.
 */
#undef SCM_RECKLESS
#define SCM_CAUTIOUS

/* After looking up a local for the first time, rewrite the
 * code graph, caching its position.
 */
#define MEMOIZE_LOCALS

/* All the number support there is.
 */
#define BIGNUMS

/* GC should relinquish empty cons-pair arenas. */
/* cmm:FIXME look at this after done mangling the GC */
/* #define GC_FREE_SEGMENTS */

/* Provide a scheme-accessible count-down timer that
 * generates a pseudo-interrupt.
 */
#define TICKS


/* Use engineering notation when converting numbers strings?
 */
#undef ENGNOT

#undef SCM_CAREFUL_INTS

/* {Unsupported Options}
 *
 * These must be defined as given here.
 */


#define CCLO

/* Guile Scheme supports the #f/() distinction; Guile Lisp won't.  We
   have horrible plans for their unification.  */
#undef SICP



/* Random options (not yet supported or in final form). */

#define STACK_CHECKING
#undef NO_CEVAL_STACK_CHECKING



/* What did the configure script discover about the outside world?  */
#include "libguile/scmconfig.h"



/* {Debugging Options}
 *
 * These compile time options determine whether to include code that is only
 * useful for debugging guile itself or C level extensions to guile.  The
 * common prefix for all option macros of this kind is "SCM_DEBUG_".  It is
 * guaranteed that a macro named SCM_DEBUG_XXX is defined to be either 0 or 1,
 * i. e. there is no need to test for the undefined case.  This allows to use
 * these definitions comfortably in macro code, as in the following example:
 *   #define FOO do { if (SCM_DEBUG_XXX) bar(); else baz(); } while (0)
 * Any sane compiler will remove the unused branch without any performance
 * penalty for the resulting code.
 *
 * Note:  Some SCM_DEBUG_XXX options are not settable at configure time.
 * To change the value of such options you will have to edit this header
 * file or give suitable options to make, like:
 *   make all CFLAGS="-DSCM_DEBUG_XXX=1 ..."
 */


/* The value of SCM_DEBUG determines the default for most of the not yet
 * defined debugging options.  This allows, for example, to enable most of the
 * debugging options by simply defining SCM_DEBUG as 1.
 */
#ifndef SCM_DEBUG
#define SCM_DEBUG 0
#endif

/* If SCM_DEBUG_CELL_ACCESSES is set to 1, cell accesses will perform
 * exhaustive parameter checking:  It will be verified that cell parameters
 * actually point to a valid heap cell.  Note:  If this option is enabled,
 * guile will run about ten times slower than normally.
 */
#ifndef SCM_DEBUG_CELL_ACCESSES
#define SCM_DEBUG_CELL_ACCESSES SCM_DEBUG
#endif

/* If SCM_DEBUG_DEPRECATED is set to 1, deprecated code is not compiled.  This
 * can be used by developers to get rid of references to deprecated code.
 */
#ifndef SCM_DEBUG_DEPRECATED
#define SCM_DEBUG_DEPRECATED SCM_DEBUG
#endif

/* If SCM_DEBUG_REST_ARGUMENT is set to 1, functions that take rest arguments
 * will check whether the rest arguments are actually passed as a proper list.
 * Otherwise, if SCM_DEBUG_REST_ARGUMENT is 0, functions that take rest
 * arguments will take it for granted that these are passed as a proper list.
 */
#ifndef SCM_DEBUG_REST_ARGUMENT
#define SCM_DEBUG_REST_ARGUMENT SCM_DEBUG
#endif

/* Use this for _compile time_ type checking only, since the compiled result
 * will be quite inefficient.  The right way to make use of this option is to
 * do a 'make clean; make CFLAGS=-DSCM_DEBUG_TYPING_STRICTNESS=1', fix your
 * errors, and then do 'make clean; make'.
 */
#ifndef SCM_DEBUG_TYPING_STRICTNESS
#define SCM_DEBUG_TYPING_STRICTNESS 0
#endif



#ifdef HAVE_LONG_LONGS

/* Some auto-generated .h files contain unused prototypes
 * that need these typedefs.
 */
#if defined (_MSC_VER) || defined (__BORLANDC__)
typedef __int64 long_long;
typedef unsigned __int64 ulong_long;
#else
typedef long long long_long;
typedef unsigned long long ulong_long;
#endif

#endif /* HAVE_LONG_LONGS */



/* Define
 *
 * SCM_CHAR_CODE_LIMIT		== UCHAR_MAX + 1
 * SCM_MOST_POSITIVE_FIXNUM 	(LONG_MAX>>2)
 * SCM_MOST_NEGATIVE_FIXNUM 	== SCM_SRS((long)LONG_MIN, 2)
 */

#ifdef HAVE_LIMITS_H
# include <limits.h>
# ifdef UCHAR_MAX
#  define SCM_CHAR_CODE_LIMIT (UCHAR_MAX+1L)
# else
#  define SCM_CHAR_CODE_LIMIT 256L
# endif /* def UCHAR_MAX */
# define SCM_MOST_POSITIVE_FIXNUM (LONG_MAX>>2)
# ifdef _UNICOS			/* Stupid cray bug */
#  define SCM_MOST_NEGATIVE_FIXNUM ((long)LONG_MIN/4)
# else
#  define SCM_MOST_NEGATIVE_FIXNUM SCM_SRS((long)LONG_MIN, 2)
# endif				/* UNICOS */
#else
# define SCM_CHAR_CODE_LIMIT 256L
# define SCM_MOST_POSITIVE_FIXNUM ((long)((unsigned long)~0L>>3))
# if (0 != ~0)
#  define SCM_MOST_NEGATIVE_FIXNUM (-SCM_MOST_POSITIVE_FIXNUM-1)
# else
#  define SCM_MOST_NEGATIVE_FIXNUM (-SCM_MOST_POSITIVE_FIXNUM)
# endif /*  (0 != ~0) */
#endif /* def HAVE_LIMITS_H */


#ifdef STDC_HEADERS
# include <stdlib.h>
# ifdef AMIGA
#  include <stddef.h>
# endif /* def AMIGA */
# define scm_sizet size_t
#else
# ifdef _SIZE_T
#  define scm_sizet size_t
# else
#  define scm_sizet unsigned int
# endif /* def _SIZE_T */
#endif /* def STDC_HEADERS */



#include "libguile/tags.h"


#ifdef vms
# ifndef CHEAP_CONTINUATIONS
   typedef int jmp_buf[17];
   extern int setjump(jmp_buf env);
   extern int longjump(jmp_buf env, int ret);
#  define setjmp setjump
#  define longjmp longjump
# else
#  include <setjmp.h>
# endif
#else				/* ndef vms */
# ifdef _CRAY1
    typedef int jmp_buf[112];
    extern int setjump(jmp_buf env);
    extern int longjump(jmp_buf env, int ret);
#  define setjmp setjump
#  define longjmp longjump
# else				/* ndef _CRAY1 */
#  include <setjmp.h>
# endif				/* ndef _CRAY1 */
#endif				/* ndef vms */

/* James Clark came up with this neat one instruction fix for
 * continuations on the SPARC.  It flushes the register windows so
 * that all the state of the process is contained in the stack. 
 */

#ifdef sparc
# define SCM_FLUSH_REGISTER_WINDOWS asm("ta 3")
#else
# define SCM_FLUSH_REGISTER_WINDOWS /* empty */
#endif

/* If stack is not longword aligned then 
 */

/* #define SHORT_ALIGN */
#ifdef THINK_C
# define SHORT_ALIGN
#endif
#ifdef MSDOS
# define SHORT_ALIGN
#endif
#ifdef atarist
# define SHORT_ALIGN
#endif

#ifdef SHORT_ALIGN
typedef short SCM_STACKITEM;
#else
typedef long SCM_STACKITEM;
#endif


#ifndef USE_THREADS
#define SCM_THREAD_DEFER
#define SCM_THREAD_ALLOW
#define SCM_THREAD_REDEFER
#define SCM_THREAD_SWITCHING_CODE
#endif

#ifdef GUILE_OLD_ASYNC_CLICK
extern unsigned int scm_async_clock;

#define SCM_ASYNC_TICK \
do { \
  if (0 == --scm_async_clock) \
    scm_async_click (); \
} while(0)
#else
GUILE_API extern int scm_asyncs_pending_p;

#define SCM_ASYNC_TICK /*fixme* should change names */ \
do { \
  if (scm_asyncs_pending_p) \
    scm_async_click (); \
} while (0)
#endif

#ifdef SCM_CAREFUL_INTS
#define SCM_CHECK_NOT_DISABLED \
  if (scm_ints_disabled) \
    fputs("ints already disabled\n", stderr); \

#define SCM_CHECK_NOT_ENABLED \
  if (!scm_ints_disabled) \
    fputs("ints already enabled\n", stderr); \

#else
#define SCM_CHECK_NOT_DISABLED
#define SCM_CHECK_NOT_ENABLED
#endif


/* Anthony Green writes:
   When the compiler sees...
	   DEFER_INTS;
	   [critical code here]
	   ALLOW_INTS;
   ...it doesn't actually promise to keep the critical code within the
   boundries of the DEFER/ALLOW_INTS instructions. It may very well
   schedule it outside of the magic defined in those macros.

   However, GCC's volatile asm feature forms a barrier over which code is
   never moved. So if you add...
	   asm ("");
   ...to each of the DEFER_INTS and ALLOW_INTS macros, the critical
   code will always remain in place.  asm's without inputs or outputs
   are implicitly volatile. */
#ifdef __GNUC__
#define SCM_FENCE asm /* volatile */ ("")
#else
#define SCM_FENCE
#endif

#define SCM_DEFER_INTS \
do { \
  SCM_FENCE; \
  SCM_CHECK_NOT_DISABLED; \
  SCM_THREAD_DEFER; \
  SCM_FENCE; \
  scm_ints_disabled = 1; \
  SCM_FENCE; \
} while (0)


#define SCM_ALLOW_INTS_ONLY \
do { \
  SCM_THREAD_ALLOW; \
  scm_ints_disabled = 0; \
} while (0)


#define SCM_ALLOW_INTS \
do { \
  SCM_FENCE; \
  SCM_CHECK_NOT_ENABLED; \
  SCM_THREAD_SWITCHING_CODE; \
  SCM_FENCE; \
  scm_ints_disabled = 0; \
  SCM_FENCE; \
  SCM_THREAD_ALLOW; \
  SCM_FENCE; \
} while (0)


#define SCM_REDEFER_INTS  \
do { \
  SCM_FENCE; \
  SCM_THREAD_REDEFER; \
  ++scm_ints_disabled; \
  SCM_FENCE; \
} while (0)


#define SCM_REALLOW_INTS \
do { \
  SCM_FENCE; \
  SCM_THREAD_SWITCHING_CODE; \
  SCM_FENCE; \
  --scm_ints_disabled; \
  SCM_FENCE; \
} while (0)


#define SCM_TICK \
do { \
  SCM_DEFER_INTS; \
  SCM_ALLOW_INTS; \
  SCM_ASYNC_TICK; \
} while (0)



/* Classification of critical sections
 *
 * When Guile moves to POSIX threads, it won't be possible to prevent
 * context switching.  In fact, the whole idea of context switching is
 * bogus if threads are run by different processors.  Therefore, we
 * must ultimately eliminate all critical sections or enforce them by
 * use of mutecis.
 *
 * All instances of SCM_DEFER_INTS and SCM_ALLOW_INTS should therefore
 * be classified and replaced by one of the delimiters below.  If you
 * understand what this is all about, I'd like to encourage you to
 * help with this task.  The set of classes below must of course be
 * incrementally augmented.
 *
 * MDJ 980419 <djurfeldt@nada.kth.se>
 */

/* A sections
 *
 * Allocation of a cell with type tag in the CAR.
 *
 * With POSIX threads, each thread will have a private pool of free
 * cells.  Therefore, this type of section can be removed.  But!  It
 * is important that the CDR is initialized first (with the CAR still
 * indicating a free cell) so that we can guarantee a consistent heap
 * at all times.
 */

#ifdef SCM_POSIX_THREADS
#define SCM_ENTER_A_SECTION
#define SCM_EXIT_A_SECTION
#else
#define SCM_ENTER_A_SECTION SCM_DEFER_INTS
#define SCM_EXIT_A_SECTION SCM_ALLOW_INTS
#endif



/** SCM_ASSERT
 ** 
 **/


#ifdef SCM_RECKLESS
#define SCM_ASSERT(_cond, _arg, _pos, _subr)
#define SCM_ASSERT_TYPE(_cond, _arg, _pos, _subr)
#define SCM_ASRTGO(_cond, _label)
#else
#define SCM_ASSERT(_cond, _arg, _pos, _subr) \
	if (!(_cond)) \
          scm_wta(_arg, (char *)(_pos), _subr)
#define SCM_ASSERT_TYPE(_cond, _arg, _pos, _subr, _msg) \
	if (!(_cond)) \
          scm_wrong_type_arg_msg(_subr, _pos, _arg, _msg)
#define SCM_ASRTGO(_cond, _label) \
        if (!(_cond)) \
          goto _label
#endif

/*
 * SCM_WTA_DISPATCH
 */

/* Dirk:FIXME:: In all of the SCM_WTA_DISPATCH_* macros it is assumed that
 * 'gf' is zero if uninitialized.  It would be cleaner if some valid SCM value
 * like SCM_BOOL_F or SCM_UNDEFINED was chosen.
 */

GUILE_API extern SCM scm_call_generic_0 (SCM gf);

#define SCM_WTA_DISPATCH_0(gf, arg, pos, subr) \
  return (SCM_UNPACK (gf) \
          ? scm_call_generic_0 ((gf)) \
          : scm_wta ((arg), (char *) (pos), (subr)))
#define SCM_GASSERT0(cond, gf, arg, pos, subr) \
  if (!(cond)) SCM_WTA_DISPATCH_0((gf), (arg), (pos), (subr))

GUILE_API extern SCM scm_call_generic_1 (SCM gf, SCM a1);

#define SCM_WTA_DISPATCH_1(gf, a1, pos, subr) \
  return (SCM_UNPACK (gf) \
          ? scm_call_generic_1 ((gf), (a1)) \
          : scm_wta ((a1), (char *) (pos), (subr)))
#define SCM_GASSERT1(cond, gf, a1, pos, subr) \
  if (!(cond)) SCM_WTA_DISPATCH_1((gf), (a1), (pos), (subr))

GUILE_API extern SCM scm_call_generic_2 (SCM gf, SCM a1, SCM a2);

#define SCM_WTA_DISPATCH_2(gf, a1, a2, pos, subr) \
  return (SCM_UNPACK (gf) \
          ? scm_call_generic_2 ((gf), (a1), (a2)) \
          : scm_wta ((pos) == SCM_ARG1 ? (a1) : (a2), (char *) (pos), (subr)))
#define SCM_GASSERT2(cond, gf, a1, a2, pos, subr) \
  if (!(cond)) SCM_WTA_DISPATCH_2((gf), (a1), (a2), (pos), (subr))

GUILE_API extern SCM scm_apply_generic (SCM gf, SCM args);

#define SCM_WTA_DISPATCH_n(gf, args, pos, subr) \
  return (SCM_UNPACK (gf) \
          ? scm_apply_generic ((gf), (args)) \
          : scm_wta (scm_list_ref ((args), SCM_MAKINUM ((pos) - 1)), \
		     (char *) (pos), \
		     (subr)))
#define SCM_GASSERTn(cond, gf, args, pos, subr) \
  if (!(cond)) SCM_WTA_DISPATCH_n((gf), (args), (pos), (subr))

#ifndef SCM_MAGIC_SNARFER
/* Let these macros pass through if
   we are snarfing;  thus we can tell the
   difference between the use of an actual
   number vs. the use of one of these macros --
   actual numbers in SCM_VALIDATE_* and SCM_ASSERT
   constructs must match the formal argument name,
   but using SCM_ARG* avoids the test */

#define SCM_ARGn 		0
#define SCM_ARG1 		1
#define SCM_ARG2 		2
#define SCM_ARG3 		3
#define SCM_ARG4 		4
#define SCM_ARG5 		5
#define SCM_ARG6 		6
#define SCM_ARG7 		7 
     /* #define SCM_ARGERR(X) 		((X) < SCM_WNA \
				 ? (char *)(X) \
				 : "wrong type argument")
				 */

/* Following must match entry indexes in scm_errmsgs[].
 * Also, SCM_WNA must follow the last SCM_ARGn in sequence.
 */
#define SCM_WNA 		8
     /* #define SCM_OVSCM_FLOW 		9 */
#define SCM_OUTOFRANGE 		10
#define SCM_NALLOC 		11
     /* #define SCM_STACK_OVFLOW	12 */
     /* #define SCM_EXIT 		13 */

#endif /* SCM_MAGIC_SNARFER */

/* (...still matching scm_errmsgs)  These
 * are signals.  Signals may become errors
 * but are distinguished because they first
 * try to invoke a handler that can resume
 * the interrupted routine.
 */
#define SCM_HUP_SIGNAL 		14
#define SCM_INT_SIGNAL 		15
#define SCM_FPE_SIGNAL 		16
#define SCM_BUS_SIGNAL 		17
#define SCM_SEGV_SIGNAL 	18
#define SCM_ALRM_SIGNAL 	19
#define SCM_GC_SIGNAL		20
#define SCM_TICK_SIGNAL		21

#define SCM_SIG_ORD(X)		((X) - SCM_HUP_SIGNAL)
#define SCM_ORD_SIG(X)		((X) + SCM_HUP_SIGNAL)
#define SCM_NUM_SIGS		(SCM_SIG_ORD (SCM_TICK_SIGNAL) + 1)

#if 0
struct errdesc
{
  char *msg;
  char *s_response;
  short parent_err;
};


extern struct errdesc scm_errmsgs[];
#endif



/* SCM_EXIT_SUCCESS is the default code to return from SCM if no errors
 * were encountered.  SCM_EXIT_FAILURE is the default code to return from
 * SCM if errors were encountered.  The return code can be explicitly
 * specified in a SCM program with (scm_quit <n>).
 */

#ifndef SCM_EXIT_SUCCESS
#ifdef vms
#define SCM_EXIT_SUCCESS 1
#else
#define SCM_EXIT_SUCCESS 0
#endif /* def vms */
#endif /* ndef SCM_EXIT_SUCCESS */
#ifndef SCM_EXIT_FAILURE
#ifdef vms
#define SCM_EXIT_FAILURE 2
#else
#define SCM_EXIT_FAILURE 1
#endif /* def vms */
#endif /* ndef SCM_EXIT_FAILURE */





#endif  /* __SCMH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
