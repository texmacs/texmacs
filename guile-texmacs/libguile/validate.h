/* classes: h_files */

#ifndef SCM_VALIDATE_H
#define SCM_VALIDATE_H

/* Copyright (C) 1999,2000,2001, 2002, 2004, 2006, 2007 Free Software Foundation, Inc.
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

/* Written by Greg J. Badros <gjb@cs.washington.edu>, Dec-1999 */



#define SCM_SYSERROR do { scm_syserror (FUNC_NAME); } while (0)

#define SCM_MEMORY_ERROR do { scm_memory_error (FUNC_NAME); } while (0)

#define SCM_SYSERROR_MSG(str, args, val) \
  do { scm_syserror_msg (FUNC_NAME, (str), (args), (val)); } while (0)

#define SCM_MISC_ERROR(str, args) \
  do { scm_misc_error (FUNC_NAME, str, args); } while (0)

#define SCM_WRONG_NUM_ARGS() \
  do { scm_error_num_args_subr (FUNC_NAME); } while (0)

#define SCM_WRONG_TYPE_ARG(pos, obj) \
  do { scm_wrong_type_arg (FUNC_NAME, pos, obj); } while (0)

#define SCM_NUM2SIZE(pos, arg) (scm_to_size_t (arg))

#define SCM_NUM2SIZE_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_to_size_t (arg))

#define SCM_NUM2PTRDIFF(pos, arg) (scm_to_ssize_t (arg))

#define SCM_NUM2PTRDIFF_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_to_ssize_t (arg))

#define SCM_NUM2SHORT(pos, arg) (scm_to_short (arg))

#define SCM_NUM2SHORT_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_to_short (arg))

#define SCM_NUM2USHORT(pos, arg) (scm_to_ushort (arg))

#define SCM_NUM2USHORT_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_to_ushort (arg))

#define SCM_NUM2INT(pos, arg) (scm_to_int (arg))

#define SCM_NUM2INT_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_to_int (arg))

#define SCM_NUM2UINT(pos, arg) (scm_to_uint (arg))

#define SCM_NUM2UINT_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_to_uint (arg))

#define SCM_NUM2ULONG(pos, arg) (scm_to_ulong (arg))

#define SCM_NUM2ULONG_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_to_ulong (arg))

#define SCM_NUM2LONG(pos, arg) (scm_to_long (arg))

#define SCM_NUM2LONG_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_to_long (arg))

#define SCM_NUM2LONG_LONG(pos, arg) (scm_to_long_long (arg))

#define SCM_NUM2LONG_LONG_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_to_long_long (arg))

#define SCM_NUM2ULONG_LONG(pos, arg) (scm_to_ulong_long (arg))

#define SCM_NUM2ULONG_LONG_DEF(pos, arg, def) \
  (SCM_UNBNDP (arg) ? def : scm_to_ulong_long (arg))

#define SCM_NUM2FLOAT(pos, arg) \
  (scm_num2float (arg, pos, FUNC_NAME))

#define SCM_NUM2DOUBLE(pos, arg) \
  (scm_num2double (arg, pos, FUNC_NAME))

#define SCM_OUT_OF_RANGE(pos, arg) \
  do { scm_out_of_range_pos (FUNC_NAME, arg, scm_from_int (pos)); } while (0)

#define SCM_ASSERT_RANGE(pos, arg, f)					\
  do { if (SCM_UNLIKELY (!(f)))					\
         scm_out_of_range_pos (FUNC_NAME, arg, scm_from_int (pos)); }	\
  while (0)

#define SCM_MUST_MALLOC_TYPE(type) \
  ((type *) scm_must_malloc (sizeof (type), FUNC_NAME))

#define SCM_MUST_MALLOC_TYPE_NUM(type, num) \
  ((type *) scm_must_malloc (sizeof (type) * (num), FUNC_NAME))

#define SCM_MUST_MALLOC(size) (scm_must_malloc ((size), FUNC_NAME))

#define SCM_MAKE_VALIDATE(pos, var, pred) \
  do { \
    SCM_ASSERT_TYPE (SCM_ ## pred (var), var, pos, FUNC_NAME, #pred); \
  } while (0)

#define SCM_I_MAKE_VALIDATE_MSG2(pos, var, pred, msg) \
  do { \
    SCM_ASSERT_TYPE (pred (var), var, pos, FUNC_NAME, msg); \
  } while (0)

#define SCM_MAKE_VALIDATE_MSG(pos, var, pred, msg) \
  SCM_I_MAKE_VALIDATE_MSG2 (pos, var, SCM_ ## pred, msg)




#define SCM_VALIDATE_REST_ARGUMENT(x) \
  do { \
    if (SCM_DEBUG_REST_ARGUMENT) { \
      if (scm_ilength (x) < 0) { \
        SCM_MISC_ERROR ("Rest arguments do not form a proper list.", SCM_EOL); \
      } \
    } \
  } while (0)

#define SCM_VALIDATE_NIM(pos, scm) SCM_MAKE_VALIDATE_MSG (pos, scm, NIMP, "non-immediate")

#define SCM_VALIDATE_BOOL(pos, flag) \
  do { \
    SCM_ASSERT_TYPE (scm_is_bool (flag), flag, pos, FUNC_NAME, "boolean"); \
  } while (0)

#define SCM_VALIDATE_BOOL_COPY(pos, flag, cvar) \
  do { \
    SCM_ASSERT (scm_is_bool (flag), flag, pos, FUNC_NAME); \
    cvar = scm_to_bool (flag); \
  } while (0)

#define SCM_VALIDATE_CHAR(pos, scm) SCM_MAKE_VALIDATE_MSG (pos, scm, CHARP, "character")

#define SCM_VALIDATE_CHAR_COPY(pos, scm, cvar) \
  do { \
    SCM_ASSERT (SCM_CHARP (scm), scm, pos, FUNC_NAME); \
    cvar = SCM_CHAR (scm); \
  } while (0)

#define SCM_VALIDATE_STRING(pos, str) \
  do { \
    SCM_ASSERT_TYPE (scm_is_string (str), str, pos, FUNC_NAME, "string"); \
  } while (0)

#define SCM_VALIDATE_REAL(pos, z) SCM_MAKE_VALIDATE_MSG (pos, z, REALP, "real")

#define SCM_VALIDATE_NUMBER(pos, z) SCM_MAKE_VALIDATE_MSG (pos, z, NUMBERP, "number")

#define SCM_VALIDATE_USHORT_COPY(pos, k, cvar) \
  do { \
    cvar = SCM_NUM2USHORT (pos, k); \
  } while (0)

#define SCM_VALIDATE_SHORT_COPY(pos, k, cvar) \
  do { \
    cvar = SCM_NUM2SHORT (pos, k); \
  } while (0)

#define SCM_VALIDATE_UINT_COPY(pos, k, cvar) \
  do { \
    cvar = SCM_NUM2UINT (pos, k); \
  } while (0)

#define SCM_VALIDATE_INT_COPY(pos, k, cvar) \
  do { \
    cvar = SCM_NUM2INT (pos, k); \
  } while (0)

#define SCM_VALIDATE_ULONG_COPY(pos, k, cvar) \
  do { \
    cvar = SCM_NUM2ULONG (pos, k); \
  } while (0)

#define SCM_VALIDATE_LONG_COPY(pos, k, cvar) \
  do { \
    cvar = SCM_NUM2LONG (pos, k); \
  } while (0)

#define SCM_VALIDATE_FLOAT_COPY(pos, k, cvar) \
  do { \
    cvar = SCM_NUM2FLOAT (pos, k); \
  } while (0)

#define SCM_VALIDATE_DOUBLE_COPY(pos, k, cvar) \
  do { \
    cvar = SCM_NUM2DOUBLE (pos, k); \
  } while (0)

#define SCM_VALIDATE_DOUBLE_DEF_COPY(pos, k, default, cvar) \
  do { \
    if (SCM_UNBNDP (k)) \
      { \
        k = scm_make_real (default); \
        cvar = default; \
      } \
    else \
      { \
        cvar = SCM_NUM2DOUBLE (pos, k); \
      } \
  } while (0)

#define SCM_VALIDATE_NULL(pos, scm) \
  SCM_I_MAKE_VALIDATE_MSG2 (pos, scm, scm_is_null, "empty list")

#define SCM_VALIDATE_NULL_OR_NIL(pos, scm) \
  SCM_MAKE_VALIDATE_MSG (pos, scm, NULL_OR_NIL_P, "empty list")

#define SCM_VALIDATE_CONS(pos, scm) \
  SCM_I_MAKE_VALIDATE_MSG2 (pos, scm, scm_is_pair, "pair")

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
    SCM_ASSERT (scm_is_pair (alist) && scm_is_pair (SCM_CAR (alist)), \
                alist, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_ALISTCELL_COPYSCM(pos, alist, cvar) \
  do { \
    SCM_ASSERT (scm_is_pair (alist), alist, pos, FUNC_NAME); \
    cvar = SCM_CAR (alist); \
    SCM_ASSERT (scm_is_pair (cvar), alist, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_OPORT_VALUE(pos, port) \
  do { \
    SCM_ASSERT (scm_valid_oport_value_p (port), port, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_PRINTSTATE(pos, a) SCM_MAKE_VALIDATE_MSG(pos, a, PRINT_STATE_P, "print-state")

#define SCM_VALIDATE_SMOB(pos, obj, type) \
  do { \
    SCM_ASSERT (SCM_TYP16_PREDICATE (scm_tc16_ ## type, obj), \
                obj, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_THUNK(pos, thunk) \
  do { \
    SCM_ASSERT (scm_is_true (scm_thunk_p (thunk)), thunk, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_SYMBOL(pos, str) \
  do { \
    SCM_ASSERT_TYPE (scm_is_symbol (str), str, pos, FUNC_NAME, "symbol"); \
  } while (0)

#define SCM_VALIDATE_VARIABLE(pos, var) SCM_MAKE_VALIDATE_MSG (pos, var, VARIABLEP, "variable")

#define SCM_VALIDATE_MEMOIZED(pos, obj) SCM_MAKE_VALIDATE_MSG (pos, obj, MEMOIZEDP, "memoized code")

#define SCM_VALIDATE_CLOSURE(pos, obj) SCM_MAKE_VALIDATE_MSG (pos, obj, CLOSUREP, "closure")

#define SCM_VALIDATE_PROC(pos, proc) \
  do { \
    SCM_ASSERT (scm_is_true (scm_procedure_p (proc)), proc, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_NULLORCONS(pos, env) \
  do { \
    SCM_ASSERT (scm_is_null (env) || scm_is_pair (env), env, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_HOOK(pos, a) SCM_MAKE_VALIDATE_MSG (pos, a, HOOKP, "hook")

#define SCM_VALIDATE_RGXP(pos, a) SCM_MAKE_VALIDATE_MSG (pos, a, RGXP, "regexp")

#define SCM_VALIDATE_DIR(pos, port) SCM_MAKE_VALIDATE_MSG (pos, port, DIRP, "directory port")

#define SCM_VALIDATE_PORT(pos, port) SCM_MAKE_VALIDATE_MSG (pos, port, PORTP, "port")

#define SCM_VALIDATE_INPUT_PORT(pos, port) \
  SCM_MAKE_VALIDATE_MSG (pos, port, INPUT_PORT_P, "input port")

#define SCM_VALIDATE_OUTPUT_PORT(pos, port) \
  SCM_MAKE_VALIDATE_MSG (pos, port, OUTPUT_PORT_P, "output port")

#define SCM_VALIDATE_FPORT(pos, port) SCM_MAKE_VALIDATE_MSG (pos, port, FPORTP, "file port")

#define SCM_VALIDATE_OPFPORT(pos, port) SCM_MAKE_VALIDATE_MSG (pos, port, OPFPORTP, "open file port")

#define SCM_VALIDATE_OPINPORT(pos, port) \
  SCM_MAKE_VALIDATE_MSG (pos, port, OPINPORTP, "open input port")

#define SCM_VALIDATE_OPENPORT(pos, port) \
  do { \
    SCM_ASSERT (SCM_PORTP (port) && SCM_OPENP (port), \
                port, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_OPPORT(pos, port) SCM_MAKE_VALIDATE_MSG (pos, port, OPPORTP, "open port")

#define SCM_VALIDATE_OPOUTPORT(pos, port) \
  SCM_MAKE_VALIDATE_MSG (pos, port, OPOUTPORTP, "open output port")

#define SCM_VALIDATE_OPOUTSTRPORT(pos, port) \
  SCM_MAKE_VALIDATE_MSG (pos, port, OPOUTSTRPORTP, "open output string port")

#define SCM_VALIDATE_FLUID(pos, fluid) \
  SCM_I_MAKE_VALIDATE_MSG2 (pos, fluid, scm_is_fluid, "fluid")

#define SCM_VALIDATE_KEYWORD(pos, v) SCM_MAKE_VALIDATE_MSG (pos, v, KEYWORDP, "keyword")

#define SCM_VALIDATE_STACK(pos, v) SCM_MAKE_VALIDATE_MSG (pos, v, STACKP, "stack")

#define SCM_VALIDATE_FRAME(pos, v) SCM_MAKE_VALIDATE_MSG (pos, v, FRAMEP, "frame")

#define SCM_VALIDATE_RSTATE(pos, v) SCM_MAKE_VALIDATE_MSG (pos, v, RSTATEP, "random-generator-state")

#define SCM_VALIDATE_ARRAY(pos, v) \
  do { \
    SCM_ASSERT (!SCM_IMP (v) \
                && scm_is_true (scm_array_p (v, SCM_UNDEFINED)), \
                v, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_VECTOR(pos, v) \
  do { \
    SCM_ASSERT (scm_is_simple_vector (v), v, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_VECTOR_OR_DVECTOR(pos, v) \
  do { \
    SCM_ASSERT ((scm_is_simple_vector (v) \
                || (scm_is_true (scm_f64vector_p (v)))), \
                v, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_STRUCT(pos, v) SCM_MAKE_VALIDATE_MSG (pos, v, STRUCTP, "struct")

#define SCM_VALIDATE_VTABLE(pos, v) \
  do { \
    SCM_ASSERT (!SCM_IMP (v) && scm_is_true (scm_struct_vtable_p (v)), \
                v, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_VECTOR_LEN(pos, v, len) \
  do { \
    SCM_ASSERT (SCM_VECTORP (v) && len == SCM_VECTOR_LENGTH (v), v, pos, FUNC_NAME); \
  } while (0)


#endif  /* SCM_VALIDATE_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
