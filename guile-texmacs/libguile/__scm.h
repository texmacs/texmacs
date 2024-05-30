/* classes: h_files */

#ifndef SCM___SCM_H
#define SCM___SCM_H

/* Copyright (C) 1995,1996,1998,1999,2000,2001,2002,2003, 2006, 2007, 2008, 2010 Free Software Foundation, Inc.
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



/**********************************************************************
 This file is Guile's central public header.

 When included by other files, this file should preceed any include
 other than __scm.h.

 Under *NO* circumstances should new items be added to the global
 namespace (via adding #define, typedef, or similar to this file) with
 generic names.  This usually means that any new names should be
 prefixed by either SCM_ or GUILE_.  i.e. do *not* #define HAVE_FOO or
 SIZEOF_BAR.  See configure.in, gen-scmconfig.h.in, and
 gen-scmconfig.c for examples of how to properly handle this issue.
 The main documentation is in gen-scmconfig.c.

 "What's the difference between _scm.h and __scm.h?"

   _scm.h is not installed; it's only visible to the libguile sources
   themselves, and it includes config.h, the private config header.

   __scm.h is installed, and is #included by <libguile.h>.  If both
   the client and libguile need some piece of information, and it
   doesn't fit well into the header file for any particular module, it
   should go in __scm.h.  __scm.h includes scmconfig.h, the public
   config header.
 **********************************************************************/

/* What did the configure script discover about the outside world?  */
#include "libguile/scmconfig.h"



/* {Compiler hints}
 *
 * The following macros are used to provide additional information for the
 * compiler, which may help to do better error checking and code
 * optimization.  A second benefit of these macros is, that they also provide
 * additional information to the developers.
 */

/* The macro SCM_NORETURN indicates that a function will never return.
 * Examples:
 *   1) int foo (char arg) SCM_NORETURN;
 */
#ifdef __GNUC__
#define SCM_NORETURN __attribute__ ((noreturn))
#else
#define SCM_NORETURN
#endif

/* The macro SCM_UNUSED indicates that a function, function argument or
 * variable may potentially be unused.
 * Examples:
 *   1) static int unused_function (char arg) SCM_UNUSED;
 *   2) int foo (char unused_argument SCM_UNUSED);
 *   3) int unused_variable SCM_UNUSED;
 */
#ifdef __GNUC__
#define SCM_UNUSED __attribute__ ((unused))
#else
#define SCM_UNUSED
#endif


/* The SCM_EXPECT macros provide branch prediction hints to the compiler.  To
 * use only in places where the result of the expression under "normal"
 * circumstances is known.  */
#if defined(__GNUC__) && (__GNUC__ >= 3)
# define SCM_EXPECT    __builtin_expect
#else
# define SCM_EXPECT(_expr, _value) (_expr)
#endif

#define SCM_LIKELY(_expr)    SCM_EXPECT ((_expr), 1)
#define SCM_UNLIKELY(_expr)  SCM_EXPECT ((_expr), 0)



/* {Supported Options}
 *
 * These may be defined or undefined.
 */

/* #define GUILE_DEBUG_FREELIST */

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



/* SCM_API is a macro prepended to all function and data definitions
   which should be exported or imported in the resulting dynamic link
   library (DLL) in the Win32 port. */

#if defined (SCM_IMPORT)
# define SCM_API __declspec (dllimport) extern
#elif defined (SCM_EXPORT) || defined (DLL_EXPORT)
# define SCM_API __declspec (dllexport) extern
#else
# define SCM_API extern
#endif



/* {Debugging Options}
 *
 * These compile time options determine whether to include code that is only
 * useful for debugging guile itself or C level extensions to guile.  The
 * common prefix for all option macros of this kind is "SCM_DEBUG_".  It is
 * guaranteed that a macro named SCM_DEBUG_XXX is always defined (typically to
 * either 0 or 1), i. e. there is no need to test for the undefined case.
 * This allows to use these definitions comfortably within code, as in the
 * following example:
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

/* If SCM_DEBUG_INTERRUPTS is set to 1, with every deferring and allowing of
 * interrupts a consistency check will be performed.
 */
#ifndef SCM_DEBUG_INTERRUPTS
#define SCM_DEBUG_INTERRUPTS SCM_DEBUG
#endif

/* If SCM_DEBUG_PAIR_ACCESSES is set to 1, accesses to cons cells will be
 * exhaustively checked.  Note:  If this option is enabled, guile will run
 * slower than normally.
 */
#ifndef SCM_DEBUG_PAIR_ACCESSES
#define SCM_DEBUG_PAIR_ACCESSES SCM_DEBUG
#endif

/* If SCM_DEBUG_REST_ARGUMENT is set to 1, functions that take rest arguments
 * will check whether the rest arguments are actually passed as a proper list.
 * Otherwise, if SCM_DEBUG_REST_ARGUMENT is 0, functions that take rest
 * arguments will take it for granted that these are passed as a proper list.
 */
#ifndef SCM_DEBUG_REST_ARGUMENT
#define SCM_DEBUG_REST_ARGUMENT SCM_DEBUG
#endif

/* The macro SCM_DEBUG_TYPING_STRICTNESS indicates what level of type checking
 * shall be performed with respect to the use of the SCM datatype.  The macro
 * may be defined to one of the values 0, 1 and 2.
 *
 * A value of 0 means that there will be no compile time type checking, since
 * the SCM datatype will be declared as an integral type.  This setting should
 * only be used on systems, where casting from integral types to pointers may
 * lead to loss of bit information.
 *
 * A value of 1 means that there will an intermediate level of compile time
 * type checking, since the SCM datatype will be declared as a pointer to an
 * undefined struct.  This setting is the default, since it does not cost
 * anything in terms of performance or code size.
 *
 * A value of 2 provides a maximum level of compile time type checking since
 * the SCM datatype will be declared as a struct.  This setting should be used
 * for _compile time_ type checking only, since the compiled result is likely
 * to be quite inefficient.  The right way to make use of this option is to do
 * a 'make clean; make CFLAGS=-DSCM_DEBUG_TYPING_STRICTNESS=2', fix your
 * errors, and then do 'make clean; make'.
 */
#ifndef SCM_DEBUG_TYPING_STRICTNESS
#define SCM_DEBUG_TYPING_STRICTNESS 1
#endif

/* If SCM_DEBUG_DEBUGGING_SUPPORT is set to 1, guile will provide a set of
 * special functions that support debugging with a debugger like gdb or
 * debugging of guile internals on the scheme level.  The behaviour of guile
 * is not changed by this macro, only the set of functions that are available
 * will differ.  All functions that are introduced this way have the prefix
 * 'scm_dbg_' on the C level and the prefix 'dbg-' on the scheme level.  This
 * allows to easily determine the set of support functions, given that your
 * debugger or repl provide automatic name completion.  Note that these
 * functions are intended to be used during interactive debugging sessions
 * only.  They are not considered part of guile's official API.  They may
 * change or disappear without notice or deprecation phase.
 */
#ifndef SCM_DEBUG_DEBUGGING_SUPPORT
#define SCM_DEBUG_DEBUGGING_SUPPORT SCM_DEBUG
#endif



/* {Feature Options}
 *
 * These compile time options determine whether code for certain features
 * should be compiled into guile.  The common prefix for all option macros
 * of this kind is "SCM_ENABLE_".  It is guaranteed that a macro named
 * SCM_ENABLE_XXX is defined to be either 0 or 1, i. e. there is no need to
 * test for the undefined case.  This allows to use these definitions
 * comfortably within code, as in the following example:
 *   #define FOO do { if (SCM_ENABLE_XXX) bar(); else baz(); } while (0)
 * Any sane compiler will remove the unused branch without any performance
 * penalty for the resulting code.
 *
 * Note:  Some SCM_ENABLE_XXX options are not settable at configure time.
 * To change the value of such options you will have to edit this header
 * file or give suitable options to make, like:
 *   make all CFLAGS="-DSCM_ENABLE_XXX=1 ..."
 */

/* If SCM_ENABLE_DEPRECATED is set to 1, deprecated code will be included in
 * guile, as well as some functions to issue run-time warnings about uses of
 * deprecated functions.
 */
#ifndef SCM_ENABLE_DEPRECATED
#define SCM_ENABLE_DEPRECATED 0
#endif



/* {Architecture and compiler properties}
 *
 * Guile as of today can only work on systems which fulfill at least the
 * following requirements:
 *
 * - scm_t_bits and SCM variables have at least 32 bits.
 *   Guile's type system is based on this assumption.
 *
 * - sizeof (scm_t_bits) >= sizeof (void*) and sizeof (SCM) >= sizeof (void*)
 *   Guile's type system is based on this assumption, since it must be
 *   possible to store pointers to cells on the heap in scm_t_bits and SCM
 *   variables.
 *
 * - sizeof (scm_t_bits) >= 4 and sizeof (scm_t_bits) is a power of 2.
 *   Guile's type system is based on this assumption.  In particular, it is
 *   assumed that cells, i. e. pairs of scm_t_bits variables, are eight
 *   character aligned.  This is because three bits of a scm_t_bits variable
 *   that is holding a pointer to a cell on the heap must be available for
 *   storing type data.
 *
 * - sizeof (scm_t_bits) <= sizeof (void*) and sizeof (SCM) <= sizeof (void*)
 *   In some parts of guile, scm_t_bits and SCM variables are passed to
 *   functions as void* arguments.  Together with the requirement above, this
 *   requires a one-to-one correspondence between the size of a void* and the
 *   sizes of scm_t_bits and SCM variables.
 *
 * - numbers are encoded using two's complement.
 *   The implementation of the bitwise scheme level operations is based on
 *   this assumption.
 *
 * - ... add more
 */

#ifdef CHAR_BIT
# define SCM_CHAR_BIT CHAR_BIT
#else
# define SCM_CHAR_BIT 8
#endif

#ifdef LONG_BIT
# define SCM_LONG_BIT LONG_BIT
#else
# define SCM_LONG_BIT (SCM_CHAR_BIT * sizeof (long) / sizeof (char))
#endif

#ifdef UCHAR_MAX
# define SCM_CHAR_CODE_LIMIT (UCHAR_MAX + 1L)
#else
# define SCM_CHAR_CODE_LIMIT 256L
#endif

#define SCM_I_UTYPE_MAX(type)      ((type)-1)
#define SCM_I_TYPE_MAX(type,umax)  ((type)((umax)/2))
#define SCM_I_TYPE_MIN(type,umax)  (-((type)((umax)/2))-1)

#define SCM_T_UINT8_MAX   SCM_I_UTYPE_MAX(scm_t_uint8)
#define SCM_T_INT8_MIN    SCM_I_TYPE_MIN(scm_t_int8,SCM_T_UINT8_MAX)
#define SCM_T_INT8_MAX    SCM_I_TYPE_MAX(scm_t_int8,SCM_T_UINT8_MAX)

#define SCM_T_UINT16_MAX  SCM_I_UTYPE_MAX(scm_t_uint16)
#define SCM_T_INT16_MIN   SCM_I_TYPE_MIN(scm_t_int16,SCM_T_UINT16_MAX)
#define SCM_T_INT16_MAX   SCM_I_TYPE_MAX(scm_t_int16,SCM_T_UINT16_MAX)

#define SCM_T_UINT32_MAX  SCM_I_UTYPE_MAX(scm_t_uint32)
#define SCM_T_INT32_MIN   SCM_I_TYPE_MIN(scm_t_int32,SCM_T_UINT32_MAX)
#define SCM_T_INT32_MAX   SCM_I_TYPE_MAX(scm_t_int32,SCM_T_UINT32_MAX)

#define SCM_T_UINT64_MAX  SCM_I_UTYPE_MAX(scm_t_uint64)
#define SCM_T_INT64_MIN   SCM_I_TYPE_MIN(scm_t_int64,SCM_T_UINT64_MAX)
#define SCM_T_INT64_MAX   SCM_I_TYPE_MAX(scm_t_int64,SCM_T_UINT64_MAX)

#if SCM_SIZEOF_LONG_LONG
#define SCM_I_ULLONG_MAX  SCM_I_UTYPE_MAX(unsigned long long)
#define SCM_I_LLONG_MIN   SCM_I_TYPE_MIN(long long,SCM_I_ULLONG_MAX)
#define SCM_I_LLONG_MAX   SCM_I_TYPE_MAX(long long,SCM_I_ULLONG_MAX)
#endif

#define SCM_T_UINTMAX_MAX SCM_I_UTYPE_MAX(scm_t_uintmax)
#define SCM_T_INTMAX_MIN  SCM_I_TYPE_MIN(scm_t_intmax,SCM_T_UINTMAX_MAX)
#define SCM_T_INTMAX_MAX  SCM_I_TYPE_MAX(scm_t_intmax,SCM_T_UINTMAX_MAX)

#define SCM_I_SIZE_MAX    SCM_I_UTYPE_MAX(size_t)
#define SCM_I_SSIZE_MIN   SCM_I_TYPE_MIN(ssize_t,SCM_I_SIZE_MAX)
#define SCM_I_SSIZE_MAX   SCM_I_TYPE_MAX(ssize_t,SCM_I_SIZE_MAX)



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
#  if defined (__ia64__)
/* For IA64, emulate the setjmp API using getcontext. */
#   include <signal.h>
#   include <ucontext.h>
    typedef struct {
      ucontext_t ctx;
      int fresh;
    } scm_i_jmp_buf;
#   define SCM_I_SETJMP(JB)			        \
      ( (JB).fresh = 1,				        \
        getcontext (&((JB).ctx)),			\
        ((JB).fresh ? ((JB).fresh = 0, 0) : 1) )
#   define SCM_I_LONGJMP(JB,VAL) scm_ia64_longjmp (&(JB), VAL)
    void scm_ia64_longjmp (scm_i_jmp_buf *, int);
#  else                 	/* ndef __ia64__ */
#   include <setjmp.h>
#  endif			/* ndef __ia64__ */
# endif				/* ndef _CRAY1 */
#endif				/* ndef vms */

/* For any platform where SCM_I_SETJMP hasn't been defined in some
   special way above, map SCM_I_SETJMP, SCM_I_LONGJMP and
   scm_i_jmp_buf to setjmp, longjmp and jmp_buf. */
#ifndef SCM_I_SETJMP
#define scm_i_jmp_buf jmp_buf
#define SCM_I_SETJMP setjmp
#define SCM_I_LONGJMP longjmp
#endif

/* James Clark came up with this neat one instruction fix for
 * continuations on the SPARC.  It flushes the register windows so
 * that all the state of the process is contained in the stack.
 */

#if defined (sparc) || defined (__sparc__) || defined (__sparc)
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

/* Cast pointer through (void *) in order to avoid compiler warnings
   when strict aliasing is enabled */
#define SCM_STACK_PTR(ptr) ((SCM_STACKITEM *) (void *) (ptr))


#define SCM_ASYNC_TICK /*fixme* should change names */ \
do { \
  if (SCM_I_CURRENT_THREAD->pending_asyncs) \
    scm_async_click (); \
} while (0)


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
#elif defined (__INTEL_COMPILER) && defined (__ia64)
#define SCM_FENCE __memory_barrier()
#else
#define SCM_FENCE
#endif

#define SCM_TICK \
do { \
  SCM_ASYNC_TICK; \
  SCM_THREAD_SWITCHING_CODE; \
} while (0)



/** SCM_ASSERT
 **
 **/


#ifdef SCM_RECKLESS
#define SCM_ASSERT(_cond, _arg, _pos, _subr)
#define SCM_ASSERT_TYPE(_cond, _arg, _pos, _subr, _msg)
#define SCM_ASRTGO(_cond, _label)
#else
#define SCM_ASSERT(_cond, _arg, _pos, _subr)			\
        do { if (SCM_UNLIKELY (!(_cond)))			\
          scm_wrong_type_arg (_subr, _pos, _arg); } while (0)
#define SCM_ASSERT_TYPE(_cond, _arg, _pos, _subr, _msg)			\
        do { if (SCM_UNLIKELY (!(_cond)))				\
          scm_wrong_type_arg_msg(_subr, _pos, _arg, _msg);  } while (0)
#define SCM_ASRTGO(_cond, _label)		\
        do {  if (SCM_UNLIKELY (!(_cond)))	\
          goto _label; } while (0)
#endif

/*
 * SCM_WTA_DISPATCH
 */

/* Dirk:FIXME:: In all of the SCM_WTA_DISPATCH_* macros it is assumed that
 * 'gf' is zero if uninitialized.  It would be cleaner if some valid SCM value
 * like SCM_BOOL_F or SCM_UNDEFINED was chosen.
 */

SCM_API SCM scm_call_generic_0 (SCM gf);

#define SCM_WTA_DISPATCH_0(gf, subr)			        \
  return (SCM_UNPACK (gf)					\
	  ? scm_call_generic_0 ((gf))				\
	  : (scm_error_num_args_subr ((subr)), SCM_UNSPECIFIED))
#define SCM_GASSERT0(cond, gf, subr)		\
  if (SCM_UNLIKELY(!(cond)))		\
    SCM_WTA_DISPATCH_0((gf), (subr))

SCM_API SCM scm_call_generic_1 (SCM gf, SCM a1);

#define SCM_WTA_DISPATCH_1(gf, a1, pos, subr)			\
  return (SCM_UNPACK (gf)					\
	  ? scm_call_generic_1 ((gf), (a1))			\
	  : (scm_wrong_type_arg ((subr), (pos), (a1)), SCM_UNSPECIFIED))
#define SCM_GASSERT1(cond, gf, a1, pos, subr)		\
  if (SCM_UNLIKELY (!(cond)))			\
    SCM_WTA_DISPATCH_1((gf), (a1), (pos), (subr))

SCM_API SCM scm_call_generic_2 (SCM gf, SCM a1, SCM a2);

#define SCM_WTA_DISPATCH_2(gf, a1, a2, pos, subr)			\
  return (SCM_UNPACK (gf)						\
	  ? scm_call_generic_2 ((gf), (a1), (a2))			\
	  : (scm_wrong_type_arg ((subr), (pos),				\
				 (pos) == SCM_ARG1 ? (a1) : (a2)),	\
	     SCM_UNSPECIFIED))
#define SCM_GASSERT2(cond, gf, a1, a2, pos, subr)	\
  if (SCM_UNLIKELY (!(cond)))			\
    SCM_WTA_DISPATCH_2((gf), (a1), (a2), (pos), (subr))

SCM_API SCM scm_apply_generic (SCM gf, SCM args);

#define SCM_WTA_DISPATCH_n(gf, args, pos, subr)				  \
  return (SCM_UNPACK (gf)						  \
	  ? scm_apply_generic ((gf), (args))				  \
	  : (scm_wrong_type_arg ((subr), (pos),				  \
				 scm_list_ref ((args),			  \
					       scm_from_int ((pos) - 1))), \
	     SCM_UNSPECIFIED))
#define SCM_GASSERTn(cond, gf, args, pos, subr)		\
  if (SCM_UNLIKELY (!(cond)))			\
    SCM_WTA_DISPATCH_n((gf), (args), (pos), (subr))

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

#endif /* SCM_MAGIC_SNARFER */



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

/* Define SCM_C_INLINE_KEYWORD so that it can be used as a replacement
   for the "inline" keyword, expanding to nothing when "inline" is not
   available.
*/

#ifdef SCM_C_INLINE
#define SCM_C_INLINE_KEYWORD SCM_C_INLINE
#else
#define SCM_C_INLINE_KEYWORD
#endif

#endif  /* SCM___SCM_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
