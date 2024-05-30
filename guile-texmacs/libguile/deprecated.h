/* This file contains definitions for deprecated features.  When you
   deprecate something, move it here when that is feasible.
*/

#ifndef SCM_DEPRECATED_H
#define SCM_DEPRECATED_H

/* Copyright (C) 2003,2004, 2005, 2006, 2007 Free Software Foundation, Inc.
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

#include "libguile/__scm.h"
#include "libguile/strings.h"

#if (SCM_ENABLE_DEPRECATED == 1)

/* From eval.h: Macros for handling ilocs.  These were deprecated in guile
 * 1.7.0 on 2004-04-22.  */
#define SCM_IFRINC             (0x00000100L)
#define SCM_ICDR               (0x00080000L)
#define SCM_IFRAME(n)          ((long)((SCM_ICDR-SCM_IFRINC)>>8) \
                                & (SCM_UNPACK (n) >> 8))
#define SCM_IDIST(n)           (SCM_UNPACK (n) >> 20)
#define SCM_ICDRP(n)           (SCM_ICDR & SCM_UNPACK (n))


/* From tags.h: Macros to access internal symbol names of isyms.  Deprecated
 * in guile 1.7.0 on 2004-04-22.  */
SCM_API char *scm_isymnames[];
#define SCM_ISYMNUM(n) 	       0
#define SCM_ISYMCHARS(n)       "#@<deprecated>"


/* From tags.h: Macro checking for two tc16 types that are allocated to differ
 * only in the 's'-bit.  Deprecated in guile 1.7.0 on 2003-09-21.  */
#define SCM_TYP16S(x) 		(0xfeff & SCM_CELL_TYPE (x))


/* From numbers.h: Macros checking for types, but avoiding a redundant check
 * for !SCM_IMP.  These were deprecated in guile 1.7.0 on 2003-09-06.  */
#define SCM_SLOPPY_INEXACTP(x) (SCM_TYP16S (x) == scm_tc16_real)
#define SCM_SLOPPY_REALP(x) (SCM_TYP16 (x) == scm_tc16_real)
#define SCM_SLOPPY_COMPLEXP(x) (SCM_TYP16 (x) == scm_tc16_complex)


/* From eval.h: Macros for handling ilocs.  These were deprecated in guile
 * 1.7.0 on 2003-06-04.  */
#define SCM_ILOC00		SCM_MAKE_ITAG8(0L, scm_tc8_iloc)
#define SCM_IDINC		(0x00100000L)
#define SCM_IDSTMSK		(-SCM_IDINC)


/* From eval.h: Error messages of the evaluator.  These were deprecated in
 * guile 1.7.0 on 2003-06-02.  */
SCM_API const char scm_s_expression[];
SCM_API const char scm_s_test[];
SCM_API const char scm_s_body[];
SCM_API const char scm_s_bindings[];
SCM_API const char scm_s_variable[];
SCM_API const char scm_s_clauses[];
SCM_API const char scm_s_formals[];


/* From eval.h: Helper macros for evaluation and application.  These were
 * deprecated in guile 1.7.0 on 2003-06-02.  */
#define SCM_EVALIM2(x) \
  ((scm_is_eq ((x), SCM_EOL) \
    ? scm_misc_error (NULL, scm_s_expression, SCM_EOL), 0 \
    : 0), \
   (x))
#define SCM_EVALIM(x, env) (SCM_ILOCP (x) \
                            ? *scm_ilookup ((x), env) \
			    : SCM_EVALIM2(x))
#define SCM_XEVAL(x, env) (scm_i_eval_x ((x), (env)))
#define SCM_XEVALCAR(x, env) (SCM_SYMBOLP (SCM_CAR (x)) \
			      ? *scm_lookupcar (x, env, 1) \
			      : scm_i_eval_x (SCM_CAR (x), (env)))


#define scm_substring_move_left_x scm_substring_move_x
#define scm_substring_move_right_x scm_substring_move_x

#define scm_sizet size_t

SCM_API SCM scm_wta (SCM arg, const char *pos, const char *s_subr);

#define SCM_WNA 		8
#define SCM_OUTOFRANGE         10
#define SCM_NALLOC             11

SCM_API void scm_register_module_xxx (char *module_name, void *init_func);
SCM_API SCM scm_registered_modules (void);
SCM_API SCM scm_clear_registered_modules (void);

SCM_API SCM scm_protect_object (SCM obj);
SCM_API SCM scm_unprotect_object (SCM obj);

#define SCM_SETAND_CAR(x, y) \
  (SCM_SETCAR ((x), SCM_PACK (SCM_UNPACK (SCM_CAR (x)) & (y))))
#define SCM_SETOR_CAR(x, y)\
  (SCM_SETCAR ((x), SCM_PACK (SCM_UNPACK (SCM_CAR (x)) | (y))))
#define SCM_SETAND_CDR(x, y)\
  (SCM_SETCDR ((x), SCM_PACK (SCM_UNPACK (SCM_CDR (x)) & (y))))
#define SCM_SETOR_CDR(x, y)\
  (SCM_SETCDR ((x), SCM_PACK (SCM_UNPACK (SCM_CDR (x)) | (y))))
#define SCM_FREEP(x) (SCM_FREE_CELL_P (x))
#define SCM_NFREEP(x) (!SCM_FREE_CELL_P (x))
#define SCM_GC8MARKP(x) SCM_GC_MARK_P (x)
#define SCM_SETGC8MARK(x) SCM_SET_GC_MARK (x)
#define SCM_CLRGC8MARK(x) SCM_CLEAR_GC_MARK (x)
#define SCM_GCTYP16(x) SCM_TYP16 (x)
#define SCM_GCCDR(x) SCM_CDR (x)
SCM_API void scm_remember (SCM * ptr);

SCM_API SCM scm_the_root_module (void);
SCM_API SCM scm_make_module (SCM name);
SCM_API SCM scm_ensure_user_module (SCM name);
SCM_API SCM scm_load_scheme_module (SCM name);

#define scm_port scm_t_port
#define scm_ptob_descriptor scm_t_ptob_descriptor
#define scm_port_rw_active scm_t_port_rw_active

SCM_API SCM scm_close_all_ports_except (SCM ports);

#define scm_rstate scm_t_rstate
#define scm_rng scm_t_rng

#define SCM_SLOPPY_CONSP(x)  ((1 & SCM_CELL_TYPE (x)) == 0)
#define SCM_SLOPPY_NCONSP(x) (!SCM_SLOPPY_CONSP(x))

#define scm_tc7_ssymbol		scm_tc7_symbol
#define scm_tc7_msymbol		scm_tc7_symbol
#define scm_tcs_symbols         scm_tc7_symbol

SCM_API SCM scm_makstr (size_t len, int);
SCM_API SCM scm_makfromstr (const char *src, size_t len, int);

SCM_API SCM scm_variable_set_name_hint (SCM var, SCM hint);
SCM_API SCM scm_builtin_variable (SCM name);

SCM_API SCM scm_internal_with_fluids (SCM fluids, SCM vals,
				      SCM (*cproc)(void *), void *cdata);

SCM_API SCM scm_make_gsubr (const char *name, int req, int opt, int rst,
			    SCM (*fcn)());
SCM_API SCM scm_make_gsubr_with_generic (const char *name,
					 int req,
					 int opt,
					 int rst,
					 SCM (*fcn)(),
					 SCM *gf);

SCM_API SCM scm_create_hook (const char* name, int n_args);

#define SCM_LIST0 SCM_EOL
#define SCM_LIST1(e0) scm_cons ((e0), SCM_EOL)
#define SCM_LIST2(e0, e1) scm_cons2 ((e0), (e1), SCM_EOL)
#define SCM_LIST3(e0, e1, e2) scm_cons ((e0), SCM_LIST2 ((e1), (e2)))
#define SCM_LIST4(e0, e1, e2, e3)\
     scm_cons2 ((e0), (e1), SCM_LIST2 ((e2), (e3)))
#define SCM_LIST5(e0, e1, e2, e3, e4)\
     scm_cons ((e0), SCM_LIST4 ((e1), (e2), (e3), (e4)))
#define SCM_LIST6(e0, e1, e2, e3, e4, e5)\
     scm_cons2 ((e0), (e1), SCM_LIST4 ((e2), (e3), (e4), (e5)))
#define SCM_LIST7(e0, e1, e2, e3, e4, e5, e6)\
     scm_cons ((e0), SCM_LIST6 ((e1), (e2), (e3), (e4), (e5), (e6)))
#define SCM_LIST8(e0, e1, e2, e3, e4, e5, e6, e7)\
     scm_cons2 ((e0), (e1), SCM_LIST6 ((e2), (e3), (e4), (e5), (e6), (e7)))
#define SCM_LIST9(e0, e1, e2, e3, e4, e5, e6, e7, e8)\
     scm_cons ((e0),\
	       SCM_LIST8 ((e1), (e2), (e3), (e4), (e5), (e6), (e7), (e8)))

#define scm_listify scm_list_n

SCM_API SCM scm_sloppy_memq (SCM x, SCM lst);
SCM_API SCM scm_sloppy_memv (SCM x, SCM lst);
SCM_API SCM scm_sloppy_member (SCM x, SCM lst);

SCM_API SCM scm_read_and_eval_x (SCM port);

#define scm_subr_entry scm_t_subr_entry

#define SCM_SUBR_DOC(x) SCM_BOOL_F

SCM_API SCM scm_make_subr (const char *name, int type, SCM (*fcn) ());
SCM_API SCM scm_make_subr_with_generic (const char *name,
					int type,
					SCM (*fcn) (),
					SCM *gf);
SCM_API SCM scm_make_subr_opt (const char *name, 
			       int type, 
			       SCM (*fcn) (),
			       int set);

SCM_API SCM scm_call_catching_errors (SCM (*thunk)(), SCM (*err_filter)(),
				      void * closure);

SCM_API long scm_make_smob_type_mfpe (char *name, size_t size,
				      SCM (*mark) (SCM),
				      size_t (*free) (SCM),
				      int (*print) (SCM, SCM,
						    scm_print_state*),
				      SCM (*equalp) (SCM, SCM));

SCM_API void scm_set_smob_mfpe (long tc, 
				SCM (*mark) (SCM),
				size_t (*free) (SCM),
				int (*print) (SCM, SCM, scm_print_state*),
				SCM (*equalp) (SCM, SCM));

SCM_API SCM scm_strprint_obj (SCM obj);
SCM_API SCM scm_read_0str (char *expr);
SCM_API SCM scm_eval_0str (const char *expr);

SCM_API char *scm_i_object_chars (SCM);

#define SCM_CHARS(x)   scm_i_object_chars(x)
#define SCM_UCHARS(x)  ((unsigned char *)SCM_CHARS(x))

SCM_API long scm_i_object_length (SCM);

#define SCM_LENGTH(x) scm_i_object_length(x)

#define scm_strhash(str, len, n) (scm_string_hash ((str), (len)) % (n))

SCM_API SCM scm_sym2ovcell_soft (SCM sym, SCM obarray);
SCM_API SCM scm_sym2ovcell (SCM sym, SCM obarray);
SCM_API SCM scm_intern_obarray_soft (const char *name, size_t len,
				     SCM obarray, unsigned int softness);
SCM_API SCM scm_intern_obarray (const char *name, size_t len, SCM obarray);
SCM_API SCM scm_symbol_value0 (const char *name);

SCM_API SCM scm_string_to_obarray_symbol (SCM o, SCM s, SCM softp);
SCM_API SCM scm_intern_symbol (SCM o, SCM s);
SCM_API SCM scm_unintern_symbol (SCM o, SCM s);
SCM_API SCM scm_symbol_binding (SCM o, SCM s);
#if 0
/* This name has been reused for real uninterned symbols. */
SCM_API SCM scm_symbol_interned_p (SCM o, SCM s);
#endif
SCM_API SCM scm_symbol_bound_p (SCM o, SCM s);
SCM_API SCM scm_symbol_set_x (SCM o, SCM s, SCM v);

SCM_API SCM scm_gentemp (SCM prefix, SCM obarray);

#define SCM_OPDIRP(x) (SCM_DIRP (x) && (SCM_DIR_OPEN_P (x)))
#define scm_fport scm_t_fport
#define scm_option scm_t_option
#define scm_srcprops scm_t_srcprops
#define scm_srcprops_chunk scm_t_srcprops_chunk
#define scm_info_frame scm_t_info_frame
#define scm_stack scm_t_stack
#define scm_array scm_t_array
#define scm_array_dim scm_t_array_dim
#define SCM_ARRAY_CONTIGUOUS SCM_ARRAY_FLAG_CONTIGUOUS
#define SCM_FUNC_NAME (scm_makfrom0str (FUNC_NAME))

#define SCM_WTA(pos, scm) \
  do { scm_wta (scm, (char *) pos, FUNC_NAME); } while (0)

#define RETURN_SCM_WTA(pos, scm) \
  do { return scm_wta (scm, (char *) pos, FUNC_NAME); } while (0)

#define SCM_VALIDATE_NUMBER_COPY(pos, z, cvar)	\
  do {						\
    if (SCM_I_INUMP (z))				\
      cvar = (double) SCM_I_INUM (z);		\
    else if (SCM_REALP (z))			\
      cvar = SCM_REAL_VALUE (z);		\
    else if (SCM_BIGP (z))			\
      cvar = scm_i_big2dbl (z);			\
    else					\
      {						\
	cvar = 0.0;				\
        SCM_WRONG_TYPE_ARG (pos, z);		\
      }						\
  } while (0)

#define SCM_VALIDATE_NUMBER_DEF_COPY(pos, number, def, cvar)	\
  do {								\
    if (SCM_UNBNDP (number))					\
      cvar = def;						\
    else							\
      SCM_VALIDATE_NUMBER_COPY(pos, number, cvar);		\
  } while (0)

#define SCM_VALIDATE_OPDIR(pos, port) SCM_MAKE_VALIDATE (pos, port, OPDIRP)

/* Deprecated because we can not safely cast a SCM* to a scm_t_bits*
 */

#define SCM_CELL_WORD_LOC(x, n)   ((scm_t_bits*)SCM_CELL_OBJECT_LOC((x),(n)))

/* Users shouldn't know about INUMs.
 */

SCM_API SCM scm_i_makinum (scm_t_signed_bits val);
SCM_API int scm_i_inump (SCM obj);
SCM_API scm_t_signed_bits scm_i_inum (SCM obj);

#define SCM_MAKINUM(x)   scm_i_makinum(x)
#define SCM_INUM(x)      scm_i_inum(x)
#define SCM_INUMP(x)     scm_i_inump(x)
#define SCM_NINUMP(x)    (!SCM_INUMP(x))

#define SCM_VALIDATE_INUM(pos, k) SCM_MAKE_VALIDATE_MSG (pos, k, INUMP, "exact integer")

#define SCM_VALIDATE_INUM_COPY(pos, k, cvar) \
  do { \
    SCM_ASSERT (SCM_I_INUMP (k), k, pos, FUNC_NAME); \
    cvar = SCM_I_INUM (k); \
  } while (0)

#define SCM_VALIDATE_BIGINT(pos, k) SCM_MAKE_VALIDATE_MSG (pos, k, BIGP, "bignum")

#define SCM_VALIDATE_INUM_MIN(pos, k, min) \
  do { \
    SCM_ASSERT (SCM_I_INUMP(k), k, pos, FUNC_NAME); \
    SCM_ASSERT_RANGE (pos, k, (SCM_I_INUM (k) >= min)); \
  } while (0)

#define SCM_VALIDATE_INUM_MIN_COPY(pos, k, min, cvar) \
  do { \
    SCM_ASSERT (SCM_I_INUMP (k), k, pos, FUNC_NAME); \
    SCM_ASSERT_RANGE (pos, k, (SCM_I_INUM (k) >= min)); \
    cvar = SCM_INUM (k); \
  } while (0)

#define SCM_VALIDATE_INUM_MIN_DEF_COPY(pos, k, min, default, cvar) \
  do { \
    if (SCM_UNBNDP (k)) \
      k = SCM_I_MAKINUM (default); \
    SCM_ASSERT (SCM_I_INUMP (k), k, pos, FUNC_NAME); \
    SCM_ASSERT_RANGE (pos, k, (SCM_I_INUM (k) >= min)); \
    cvar = SCM_INUM (k); \
  } while (0)

#define SCM_VALIDATE_INUM_DEF(pos, k, default) \
  do { \
    if (SCM_UNBNDP (k)) \
      k = SCM_I_MAKINUM (default); \
    else SCM_ASSERT (SCM_I_INUMP (k), k, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_INUM_DEF_COPY(pos, k, default, cvar) \
  do { \
    if (SCM_UNBNDP (k)) \
      { \
        k = SCM_I_MAKINUM (default); \
        cvar = default; \
      } \
    else \
      { \
        SCM_ASSERT (SCM_I_INUMP (k), k, pos, FUNC_NAME); \
        cvar = SCM_INUM (k); \
      } \
  } while (0)

/* [low, high) */
#define SCM_VALIDATE_INUM_RANGE(pos, k, low, high) \
  do { SCM_ASSERT(SCM_I_INUMP(k), k, pos, FUNC_NAME); \
       SCM_ASSERT_RANGE(pos, k, \
                        (SCM_I_INUM (k) >= low && \
                         SCM_I_INUM (k) < high)); \
     } while (0)

#define SCM_VALIDATE_INUM_RANGE_COPY(pos, k, low, high, cvar) \
  do { \
    SCM_ASSERT (SCM_INUMP (k), k, pos, FUNC_NAME); \
    SCM_ASSERT_RANGE (pos, k, low <= SCM_INUM (k) && SCM_INUM (k) < high); \
    cvar = SCM_INUM (k); \
  } while (0)

#define SCM_STRING_COERCE_0TERMINATION_X(x) (x)

/* XXX - buggy interface, STR might not be large enough.

   Converts the given Scheme string OBJ into a C string, containing a copy
   of OBJ's content with a trailing null byte.  If LENP is non-NULL, set
   *LENP to the string's length.

   When STR is non-NULL it receives the copy and is returned by the function,
   otherwise new memory is allocated and the caller is responsible for 
   freeing it via free().  If out of memory, NULL is returned.

   Note that Scheme strings may contain arbitrary data, including null
   characters.  This means that null termination is not a reliable way to 
   determine the length of the returned value.  However, the function always 
   copies the complete contents of OBJ, and sets *LENP to the length of the
   scheme string (if LENP is non-null).  
*/
SCM_API char *scm_c_string2str (SCM obj, char *str, size_t *lenp);

/* XXX - buggy interface, you don't know how many bytes have been copied.

   Copy LEN characters at START from the Scheme string OBJ to memory
   at STR.  START is an index into OBJ; zero means the beginning of
   the string.  STR has already been allocated by the caller.

   If START + LEN is off the end of OBJ, silently truncate the source
   region to fit the string.  If truncation occurs, the corresponding
   area of STR is left unchanged.  
*/
SCM_API char *scm_c_substring2str (SCM obj, char *str, size_t start, size_t len);

SCM_API char *scm_c_symbol2str (SCM obj, char *str, size_t *lenp);

/* Deprecated because the names belong to what is now
   scm_truncate_number and scm_round_number.
*/
SCM_API double scm_truncate (double x);
SCM_API double scm_round (double x);

/* Deprecated because we don't want people to access the internal
   representation of strings directly.
*/

#define SCM_VALIDATE_STRING_COPY(pos, str, cvar) \
  do { \
    SCM_ASSERT (SCM_STRINGP (str), str, pos, FUNC_NAME); \
    cvar = SCM_STRING_CHARS(str); \
  } while (0)

/* validate a string and optional start/end arguments which default to
   0/string-len.  this is unrelated to the old shared substring
   support, so please do not deprecate it :) */
#define SCM_VALIDATE_SUBSTRING_SPEC_COPY(pos_str, str, c_str, \
                                         pos_start, start, c_start,\
                                         pos_end, end, c_end) \
  do {\
    SCM_VALIDATE_STRING_COPY (pos_str, str, c_str);\
    c_start = SCM_UNBNDP(start)? 0 : scm_to_size_t (start);\
    c_end = SCM_UNBNDP(end)? SCM_STRING_LENGTH(str) : scm_to_size_t (end);\
    SCM_ASSERT_RANGE (pos_start, start,\
                      0 <= c_start \
                      && (size_t) c_start <= SCM_STRING_LENGTH (str));\
    SCM_ASSERT_RANGE (pos_end, end,\
		      c_start <= c_end \
                      && (size_t) c_end <= SCM_STRING_LENGTH (str));\
  } while (0)

/* Deprecated because we don't want people to access the internals of
   symbols directly.
*/

SCM_API char *scm_i_deprecated_symbol_chars (SCM sym);
SCM_API size_t scm_i_deprecated_symbol_length (SCM sym);

#define SCM_SYMBOL_CHARS(x)  scm_i_deprecated_symbol_chars(x)
#define SCM_SYMBOL_LENGTH(x) scm_i_deprecated_symbol_length(x)

/* Deprecated because the macros used to evaluate the arguments more
   than once and because the symbol of a keyword now has no dash.
*/

SCM_API int scm_i_keywordp (SCM obj);
SCM_API SCM scm_i_keywordsym (SCM keyword);

#define SCM_KEYWORDP(x)   scm_i_keywordp(x)
#define SCM_KEYWORDSYM(x) scm_i_keywordsym(x)

/* Deprecated because we don't want to hand out unprotected pointers
   to arrays, vectors, etc. */

#define SCM_VECTOR_MAX_LENGTH ((1L << 24) - 1)

SCM_API int scm_i_vectorp (SCM x);
SCM_API unsigned long scm_i_vector_length (SCM x);
SCM_API const SCM *scm_i_velts (SCM x);
SCM_API SCM *scm_i_writable_velts (SCM x);
SCM_API SCM scm_i_vector_ref (SCM x, size_t idx);
SCM_API void scm_i_vector_set (SCM x, size_t idx, SCM val);
SCM_API SCM scm_vector_equal_p (SCM x, SCM y);

#define SCM_VECTORP(x)         scm_i_vectorp(x)
#define SCM_VECTOR_LENGTH(x)   scm_i_vector_length(x)
#define SCM_VELTS(x)           scm_i_velts(x)
#define SCM_WRITABLE_VELTS(x)  scm_i_writable_velts(x)
#define SCM_VECTOR_REF(x,y)    scm_i_vector_ref(x,y)
#define SCM_VECTOR_SET(x,y,z)  scm_i_vector_set(x,y,z)

typedef scm_i_t_array scm_t_array;

SCM_API int scm_i_arrayp (SCM a);
SCM_API size_t scm_i_array_ndim (SCM a);
SCM_API int scm_i_array_contp (SCM a);
SCM_API scm_t_array *scm_i_array_mem (SCM a);
SCM_API SCM scm_i_array_v (SCM a);
SCM_API size_t scm_i_array_base (SCM a);
SCM_API scm_t_array_dim *scm_i_array_dims (SCM a);

#define SCM_ARRAYP(a)      scm_i_arrayp(a)
#define SCM_ARRAY_NDIM(a)  scm_i_array_ndim(a)
#define SCM_ARRAY_CONTP(a) scm_i_array_contp(a)
#define SCM_ARRAY_MEM(a)   scm_i_array_mem(a)
#define SCM_ARRAY_V(a)     scm_i_array_v(a)
#define SCM_ARRAY_BASE(a)  scm_i_array_base(a)
#define SCM_ARRAY_DIMS(a)  scm_i_array_dims(a)

/* Deprecated because they should not be lvalues and we want people to
   use the official interfaces.
 */

#define scm_cur_inp           scm_i_cur_inp ()
#define scm_cur_outp          scm_i_cur_outp ()
#define scm_cur_errp          scm_i_cur_errp ()
#define scm_cur_loadp         scm_i_cur_loadp ()
#define scm_progargs          scm_i_progargs ()
#define scm_dynwinds          scm_i_deprecated_dynwinds ()
#define scm_last_debug_frame  scm_i_deprecated_last_debug_frame ()
#define scm_stack_base        scm_i_stack_base ()

SCM_API SCM scm_i_cur_inp (void);
SCM_API SCM scm_i_cur_outp (void);
SCM_API SCM scm_i_cur_errp (void);
SCM_API SCM scm_i_cur_loadp (void);
SCM_API SCM scm_i_progargs (void);
SCM_API SCM scm_i_deprecated_dynwinds (void);
SCM_API scm_t_debug_frame *scm_i_deprecated_last_debug_frame (void);
SCM_API SCM_STACKITEM *scm_i_stack_base (void);

/* Deprecated because it evaluates its argument twice.
 */
#define SCM_FLUIDP(x) scm_i_fluidp (x)
SCM_API int scm_i_fluidp (SCM x);

/* In the old days, SCM_CRITICAL_SECTION_START stopped signal handlers
   from running, since in those days the handler directly ran scheme
   code, and that had to be avoided when the heap was not in a
   consistent state etc.  And since the scheme code could do a stack
   swapping new continuation etc, signals had to be deferred around
   various C library functions which were not safe or not known to be
   safe to swap away, which was a lot of stuff.

   These days signals are implemented with asyncs and don't directly
   run scheme code in the handler, but hold it until an SCM_TICK etc
   where it will be safe.  This means interrupt protection is not
   needed and SCM_CRITICAL_SECTION_START / SCM_CRITICAL_SECTION_END is
   something of an anachronism.

   What past SCM_CRITICAL_SECTION_START usage also did though was
   indicate code that was not reentrant, ie. could not be reentered by
   signal handler code.  The present definitions are a mutex lock,
   affording that reentrancy protection against the new guile 1.8
   free-running posix threads.

   One big problem with the present defintions though is that code which
   throws an error from within a DEFER/ALLOW region will leave the
   defer_mutex locked and hence hang other threads that attempt to enter a
   similar DEFER/ALLOW region.
*/

SCM_API void scm_i_defer_ints_etc (void);
#define SCM_DEFER_INTS scm_i_defer_ints_etc ()
#define SCM_ALLOW_INTS scm_i_defer_ints_etc ()
#define SCM_REDEFER_INTS scm_i_defer_ints_etc ()
#define SCM_REALLOW_INTS scm_i_defer_ints_etc ()

/* Deprecated since they are unnecessary and had not been documented.
 */
SCM_API SCM scm_guard (SCM guardian, SCM obj, int throw_p);
SCM_API SCM scm_get_one_zombie (SCM guardian);

/* Deprecated since guardians no longer have these special features.
 */
SCM_API SCM scm_destroy_guardian_x (SCM guardian);
SCM_API SCM scm_guardian_greedy_p (SCM guardian);
SCM_API SCM scm_guardian_destroyed_p (SCM guardian);

void scm_i_init_deprecated (void);

#endif

#endif /* SCM_DEPRECATED_H */
