/* $Id$ */
/*	Copyright (C) 1999, 2000 Free Software Foundation, Inc.
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

/* Written by Greg J. Badros <gjb@cs.washington.edu>, Dec-1999 */

#ifndef SCM_VALIDATE_H__
#define SCM_VALIDATE_H__

#define SCM_FUNC_NAME (scm_makfrom0str (FUNC_NAME))

#define SCM_SYSERROR do { scm_syserror (FUNC_NAME); } while (0)

#define SCM_MEMORY_ERROR do { scm_memory_error (FUNC_NAME); } while (0)

#define SCM_SYSERROR_MSG(str, args, val) \
  do { scm_syserror_msg (FUNC_NAME, (str), (args), (val)); } while (0)

#define SCM_COERCE_ROSTRING(pos, scm) \
  do { scm = scm_coerce_rostring (scm, FUNC_NAME, pos); } while (0)

#define SCM_WTA(pos, scm) \
  do { scm_wta (scm, (char *) pos, FUNC_NAME); } while (0)

#define RETURN_SCM_WTA(pos, scm) \
  do { return scm_wta (scm, (char *) pos, FUNC_NAME); } while (0)

#define SCM_MISC_ERROR(str, args) \
  do { scm_misc_error (FUNC_NAME, str, args); } while (0)

#define SCM_WRONG_TYPE_ARG(pos, obj) \
  do { scm_wrong_type_arg (FUNC_NAME, pos, obj); } while (0)

#define SCM_NUM2ULONG(pos, arg) (scm_num2ulong (arg, (char *) pos, FUNC_NAME))

#define SCM_NUM2LONG(pos, arg) (scm_num2long (arg, (char *) pos, FUNC_NAME))

#define SCM_NUM2LONG_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_num2long (arg, (char *) pos, FUNC_NAME))

#define SCM_NUM2LONG_LONG(pos, arg) \
  (scm_num2long_long (arg, (char *) pos, FUNC_NAME))

#define SCM_OUT_OF_RANGE(pos, arg) \
  do { scm_out_of_range_pos (FUNC_NAME, arg, SCM_MAKINUM (pos)); } while (0)

#define SCM_ASSERT_RANGE(pos, arg, f) \
  do { SCM_ASSERT (f, arg, SCM_OUTOFRANGE, FUNC_NAME); } while (0)

#define SCM_MUST_MALLOC_TYPE(type) \
  ((type *) scm_must_malloc (sizeof (type), FUNC_NAME))

#define SCM_MUST_MALLOC_TYPE_NUM(type, num) \
  ((type *) scm_must_malloc (sizeof (type) * (num), FUNC_NAME))

#define SCM_MUST_MALLOC(size) (scm_must_malloc ((size), FUNC_NAME))

#define SCM_MAKE_VALIDATE(pos, var, pred) \
  do { \
    SCM_ASSERT_TYPE (SCM_ ## pred (var), var, pos, FUNC_NAME, #pred); \
  } while (0)



#define SCM_VALIDATE_REST_ARGUMENT(x) \
  do { \
    if (SCM_DEBUG_REST_ARGUMENT) { \
      if (scm_ilength (x) < 0) { \
        SCM_MISC_ERROR ("Rest arguments do not form a proper list.", SCM_EOL); \
      } \
    } \
  } while (0)

#define SCM_VALIDATE_NIM(pos, scm) SCM_MAKE_VALIDATE (pos, scm, NIMP)

#define SCM_VALIDATE_BOOL(pos, flag) SCM_MAKE_VALIDATE(pos, flag, BOOLP)

#define SCM_VALIDATE_BOOL_COPY(pos, flag, cvar) \
  do { \
    SCM_ASSERT (SCM_BOOLP (flag), flag, pos, FUNC_NAME); \
    cvar = SCM_EQ_P (flag, SCM_BOOL_T) ? 1 : 0; \
  } while (0)

#define SCM_VALIDATE_CHAR(pos, scm) SCM_MAKE_VALIDATE (pos, scm, CHARP)

#define SCM_VALIDATE_CHAR_COPY(pos, scm, cvar) \
  do { \
    SCM_ASSERT (SCM_CHARP (scm), scm, pos, FUNC_NAME); \
    cvar = SCM_CHAR (scm); \
  } while (0)

#define SCM_VALIDATE_ROSTRING(pos, str) SCM_MAKE_VALIDATE (pos, str, ROSTRINGP)

#define SCM_VALIDATE_ROSTRING_COPY(pos, str, cvar) \
  do { \
    SCM_ASSERT (SCM_ROSTRINGP (str), str, pos, FUNC_NAME); \
    cvar = SCM_ROCHARS (str); \
  } while (0)

#define SCM_VALIDATE_NULLORROSTRING_COPY(pos, str, cvar) \
  do { \
    SCM_ASSERT (SCM_FALSEP (str) || SCM_ROSTRINGP (str), \
                str, pos, FUNC_NAME); \
    if (SCM_FALSEP(str)) \
      cvar = NULL; \
    else \
      cvar = SCM_ROCHARS(str); \
  } while (0)

#define SCM_VALIDATE_STRING(pos, str) SCM_MAKE_VALIDATE (pos, str, STRINGP)

#define SCM_VALIDATE_STRINGORSUBSTR(pos, str) \
  do { \
    SCM_ASSERT (SCM_STRINGP (str) || SCM_SUBSTRP (str), \
                str, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_STRING_COPY(pos, str, cvar) \
  do { \
    SCM_ASSERT (SCM_STRINGP (str), str, pos, FUNC_NAME); \
    cvar = SCM_CHARS(str); \
  } while (0)

#define SCM_VALIDATE_RWSTRING(pos, str) \
  do { \
    SCM_ASSERT (SCM_STRINGP (str), str, pos, FUNC_NAME); \
    if (!SCM_RWSTRINGP (str)) \
      scm_misc_error (FUNC_NAME, "argument is a read-only string", str); \
  } while (0)

#define SCM_VALIDATE_REAL(pos, z) SCM_MAKE_VALIDATE (pos, z, REALP)

#define SCM_VALIDATE_NUMBER_COPY(pos, z, cvar)	\
  do {						\
    if (SCM_INUMP (z))				\
      cvar = (double) SCM_INUM (z);		\
    else if (SCM_REALP (z))			\
      cvar = SCM_REAL_VALUE (z);		\
    else if (SCM_BIGP (z))			\
      cvar = scm_big2dbl (z);			\
    else					\
      {						\
	cvar = 0.0;				\
        SCM_WTA (pos, z);			\
      }						\
  } while (0)

#define SCM_VALIDATE_INUM(pos, k) SCM_MAKE_VALIDATE (pos, k, INUMP)

#define SCM_VALIDATE_INUM_COPY(pos, k, cvar) \
  do { \
    SCM_ASSERT (SCM_INUMP (k), k, pos, FUNC_NAME); \
    cvar = SCM_INUM (k); \
  } while (0)

#define SCM_VALIDATE_ULONG_COPY(pos, k, cvar) \
  do { \
    cvar = SCM_NUM2ULONG (pos, k); \
  } while (0)

#define SCM_VALIDATE_LONG_COPY(pos, k, cvar) \
  do { \
    cvar = SCM_NUM2LONG(pos, k); \
  } while (0)

#define SCM_VALIDATE_BIGINT(pos, k) SCM_MAKE_VALIDATE (pos, k, BIGP)

#define SCM_VALIDATE_INUM_MIN(pos, k, min) \
  do { \
    SCM_ASSERT (SCM_INUMP(k), k, pos, FUNC_NAME); \
    SCM_ASSERT_RANGE (pos, k, (SCM_INUM (k) >= min)); \
  } while (0)

#define SCM_VALIDATE_INUM_MIN_COPY(pos, k, min, cvar) \
  do { \
    SCM_ASSERT (SCM_INUMP (k), k, pos, FUNC_NAME); \
    cvar = SCM_INUM (k); \
    SCM_ASSERT_RANGE (pos, k, (cvar >= min)); \
  } while (0)

#define SCM_VALIDATE_INUM_MIN_DEF_COPY(pos, k, min, default, cvar) \
  do { \
    if (SCM_UNBNDP (k)) \
      k = SCM_MAKINUM (default); \
    SCM_ASSERT (SCM_INUMP (k), k, pos, FUNC_NAME); \
    cvar = SCM_INUM (k); \
    SCM_ASSERT_RANGE (pos, k, (cvar >= min)); \
  } while (0)

#define SCM_VALIDATE_INUM_DEF(pos, k, default) \
  do { \
    if (SCM_UNBNDP (k)) \
      k = SCM_MAKINUM (default); \
    else SCM_ASSERT (SCM_INUMP (k), k, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_INUM_DEF_COPY(pos, k, default, cvar) \
  do { \
    if (SCM_UNBNDP (k)) \
      { \
        k = SCM_MAKINUM (default); \
        cvar = default; \
      } \
    else \
      { \
        SCM_ASSERT (SCM_INUMP (k), k, pos, FUNC_NAME); \
        cvar = SCM_INUM (k); \
      } \
  } while (0)

/* [low,high) */
#define SCM_VALIDATE_INUM_RANGE(pos,k,low,high) \
  do { SCM_ASSERT(SCM_INUMP(k), k, pos, FUNC_NAME); \
       SCM_ASSERT_RANGE(pos,k, \
                        (SCM_INUM (k) >= low && \
                         SCM_INUM (k) < high)); \
     } while (0)

#define SCM_VALIDATE_INUM_RANGE_COPY(pos, k, low, high, cvar) \
  do { \
    SCM_ASSERT (SCM_INUMP (k), k, pos, FUNC_NAME); \
    cvar = SCM_INUM (k); \
    SCM_ASSERT_RANGE (pos, k, cvar >= low && cvar < high); \
  } while (0)

#define SCM_VALIDATE_NULL(pos, scm) SCM_MAKE_VALIDATE (pos, scm, NULLP)

#define SCM_VALIDATE_CONS(pos, scm) SCM_MAKE_VALIDATE (pos, scm, CONSP)

#define SCM_VALIDATE_LIST(pos, lst) \
  do { \
    SCM_ASSERT (scm_ilength (lst) >= 0, lst, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_NONEMPTYLIST(pos, lst) \
  do { \
    SCM_ASSERT (scm_ilength (lst) > 0, lst, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_LIST_COPYLEN(pos, lst, cvar) \
  do { \
    cvar = scm_ilength (lst); \
    SCM_ASSERT (cvar >= 0, lst, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_NONEMPTYLIST_COPYLEN(pos, lst, cvar) \
  do { \
    cvar = scm_ilength (lst); \
    SCM_ASSERT (cvar >= 1, lst, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_ALISTCELL(pos, alist) \
  do { \
    SCM_ASSERT (SCM_CONSP (alist) && SCM_CONSP (SCM_CAR (alist)), \
                alist, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_ALISTCELL_COPYSCM(pos, alist, cvar) \
  do { \
    SCM_ASSERT (SCM_CONSP (alist), alist, pos, FUNC_NAME); \
    cvar = SCM_CAR (alist); \
    SCM_ASSERT (SCM_CONSP (cvar), alist, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_OPORT_VALUE(pos, port) \
  do { \
    SCM_ASSERT (scm_valid_oport_value_p (port), port, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_PRINTSTATE(pos, a) SCM_MAKE_VALIDATE(pos,a,PRINT_STATE_P)

#define SCM_VALIDATE_SMOB(pos, obj, type) \
  do { \
    SCM_ASSERT ((SCM_NIMP (obj) && SCM_TYP16 (obj) == scm_tc16_ ## type), \
                obj, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_THREAD(pos, a) SCM_MAKE_VALIDATE (pos, a, THREADP)

#define SCM_VALIDATE_THUNK(pos, thunk) \
  do { \
    SCM_ASSERT (SCM_NFALSEP (scm_thunk_p (thunk)), thunk, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_SYMBOL(pos, sym) SCM_MAKE_VALIDATE (pos, sym, SYMBOLP)

#define SCM_VALIDATE_VARIABLE(pos, var) SCM_MAKE_VALIDATE (pos, var, VARIABLEP)

#define SCM_VALIDATE_MEMOIZED(pos, obj) SCM_MAKE_VALIDATE (pos, obj, MEMOIZEDP)

#define SCM_VALIDATE_CLOSURE(pos, obj) SCM_MAKE_VALIDATE (pos, obj, CLOSUREP)

#define SCM_VALIDATE_PROC(pos, proc) \
  do { \
    SCM_ASSERT (SCM_EQ_P (scm_procedure_p (proc), SCM_BOOL_T), proc, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_NULLORCONS(pos, env) \
  do { \
    SCM_ASSERT (SCM_NULLP (env) || SCM_CONSP (env), env, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_HOOK(pos, a) SCM_MAKE_VALIDATE (pos, a, HOOKP)

#define SCM_VALIDATE_RGXP(pos, a) SCM_MAKE_VALIDATE (pos, a, RGXP)

#define SCM_VALIDATE_OPDIR(pos, port) SCM_MAKE_VALIDATE (pos, port, OPDIRP)

#define SCM_VALIDATE_DIR(pos, port) SCM_MAKE_VALIDATE (pos, port, DIRP)

#define SCM_VALIDATE_PORT(pos, port) SCM_MAKE_VALIDATE (pos, port, PORTP)

#define SCM_VALIDATE_INPUT_PORT(pos, port) \
  SCM_MAKE_VALIDATE (pos, port, INPUT_PORT_P)

#define SCM_VALIDATE_OUTPUT_PORT(pos, port) \
  SCM_MAKE_VALIDATE (pos, port, OUTPUT_PORT_P)

#define SCM_VALIDATE_FPORT(pos, port) SCM_MAKE_VALIDATE (pos, port, FPORTP)

#define SCM_VALIDATE_OPFPORT(pos, port) SCM_MAKE_VALIDATE (pos, port, OPFPORTP)

#define SCM_VALIDATE_OPINPORT(pos, port) \
  SCM_MAKE_VALIDATE (pos, port, OPINPORTP)

#define SCM_VALIDATE_OPENPORT(pos,port) \
  do { \
    SCM_ASSERT (SCM_PORTP (port) && SCM_OPENP (port), \
                port, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_OPPORT(pos, port) SCM_MAKE_VALIDATE (pos, port, OPPORTP)

#define SCM_VALIDATE_OPOUTPORT(pos, port) \
  SCM_MAKE_VALIDATE (pos, port, OPOUTPORTP)

#define SCM_VALIDATE_FLUID(pos, fluid) SCM_MAKE_VALIDATE (pos, fluid, FLUIDP)

#define SCM_VALIDATE_KEYWORD(pos, v) SCM_MAKE_VALIDATE (pos, v, KEYWORDP)

#define SCM_VALIDATE_STACK(pos, v) SCM_MAKE_VALIDATE (pos, v, STACKP)

#define SCM_VALIDATE_FRAME(pos, v) SCM_MAKE_VALIDATE (pos, v, FRAMEP)

#define SCM_VALIDATE_RSTATE(pos, v) SCM_MAKE_VALIDATE (pos, v, RSTATEP)

#define SCM_VALIDATE_ARRAY(pos,v) \
  do { \
    SCM_ASSERT (SCM_NIMP (v) \
                && !SCM_FALSEP (scm_array_p (v, SCM_UNDEFINED)), \
                v, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_VECTOR(pos, v) SCM_MAKE_VALIDATE (pos, v, VECTORP)

#define SCM_VALIDATE_VECTOR_OR_DVECTOR(pos, v) \
  do { \
    SCM_ASSERT ((SCM_VECTORP (v) \
                || (SCM_NIMP (v) && SCM_TYP7 (v) == scm_tc7_dvect)), \
                v, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_STRUCT(pos, v) SCM_MAKE_VALIDATE (pos, v, STRUCTP)

#define SCM_VALIDATE_VTABLE(pos, v) \
  do { \
    SCM_ASSERT (SCM_NIMP (v) && SCM_NFALSEP (scm_struct_vtable_p (v)), \
                v, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_VECTOR_LEN(pos, v, len) \
  do { \
    SCM_ASSERT (SCM_VECTORP (v) && len == SCM_LENGTH (v), v, pos, FUNC_NAME); \
  } while (0)

#endif

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
