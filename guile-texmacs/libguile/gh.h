/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2005, 2006 Free Software Foundation, Inc.
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


#ifndef __GH_H
#define __GH_H

/* This needs to be included outside of the extern "C" block.
 */
#include <libguile.h>

#if SCM_ENABLE_DEPRECATED

#ifdef __cplusplus
extern "C" {
#endif

/* gcc has extern inline functions that are basically as fast as macros */
#ifdef __GNUC__
# define INL inline
# define EXTINL extern inline
#else
# define INL
#define EXTINL
#endif /* __GNUC__ */

SCM_API void gh_enter(int argc, char *argv[], 
		      void (*c_main_prog)(int, char **));
#define gh_init () scm_init_guile ()
SCM_API void gh_repl(int argc, char *argv[]);
SCM_API SCM gh_catch(SCM tag, scm_t_catch_body body, void *body_data,
		     scm_t_catch_handler handler, void *handler_data);

SCM_API SCM gh_standard_handler(void *data, SCM tag, SCM throw_args);

SCM_API SCM gh_eval_str(const char *scheme_code);
SCM_API SCM gh_eval_str_with_catch(const char *scheme_code, scm_t_catch_handler handler);
SCM_API SCM gh_eval_str_with_standard_handler(const char *scheme_code);
SCM_API SCM gh_eval_str_with_stack_saving_handler(const char *scheme_code);

SCM_API SCM gh_eval_file(const char *fname);
#define gh_load(fname) gh_eval_file(fname)
SCM_API SCM gh_eval_file_with_catch(const char *scheme_code, scm_t_catch_handler handler);
SCM_API SCM gh_eval_file_with_standard_handler(const char *scheme_code);

#define gh_defer_ints() SCM_CRITICAL_SECTION_START
#define gh_allow_ints() SCM_CRITICAL_SECTION_END

SCM_API SCM gh_new_procedure(const char *proc_name, SCM (*fn)(),
			     int n_required_args, int n_optional_args, 
			     int varp);
SCM_API SCM gh_new_procedure0_0(const char *proc_name, SCM (*fn)(void));
SCM_API SCM gh_new_procedure0_1(const char *proc_name, SCM (*fn)(SCM));
SCM_API SCM gh_new_procedure0_2(const char *proc_name, SCM (*fn)(SCM, SCM));
SCM_API SCM gh_new_procedure1_0(const char *proc_name, SCM (*fn)(SCM));
SCM_API SCM gh_new_procedure1_1(const char *proc_name, SCM (*fn)(SCM, SCM));
SCM_API SCM gh_new_procedure1_2(const char *proc_name, SCM (*fn)(SCM, SCM, SCM));
SCM_API SCM gh_new_procedure2_0(const char *proc_name, SCM (*fn)(SCM, SCM));
SCM_API SCM gh_new_procedure2_1(const char *proc_name, SCM (*fn)(SCM, SCM, SCM));
SCM_API SCM gh_new_procedure2_2(const char *proc_name, SCM (*fn)(SCM, SCM, SCM, SCM));
SCM_API SCM gh_new_procedure3_0(const char *proc_name, SCM (*fn)(SCM, SCM, SCM));
SCM_API SCM gh_new_procedure4_0(const char *proc_name, SCM (*fn)(SCM, SCM, SCM, SCM));
SCM_API SCM gh_new_procedure5_0(const char *proc_name, SCM (*fn)(SCM, SCM, SCM, SCM, SCM));

/* C to Scheme conversion */
SCM_API SCM gh_bool2scm(int x);
SCM_API SCM gh_int2scm(int x);
SCM_API SCM gh_ulong2scm(unsigned long x);
SCM_API SCM gh_long2scm(long x);
SCM_API SCM gh_double2scm(double x);
SCM_API SCM gh_char2scm(char c);
SCM_API SCM gh_str2scm(const char *s, size_t len);
SCM_API SCM gh_str02scm(const char *s);
SCM_API void gh_set_substr(const char *src, SCM dst, long start, size_t len);
SCM_API SCM gh_symbol2scm(const char *symbol_str);
SCM_API SCM gh_ints2scm(const int *d, long n);

SCM_API SCM gh_chars2byvect(const char *d, long n);
SCM_API SCM gh_shorts2svect(const short *d, long n);
SCM_API SCM gh_longs2ivect(const long *d, long n);
SCM_API SCM gh_ulongs2uvect(const unsigned long *d, long n);
SCM_API SCM gh_floats2fvect(const float *d, long n);
SCM_API SCM gh_doubles2dvect(const double *d, long n);

SCM_API SCM gh_doubles2scm(const double *d, long n);

/* Scheme to C conversion */
SCM_API int gh_scm2bool(SCM obj);
SCM_API int gh_scm2int(SCM obj);
SCM_API unsigned long gh_scm2ulong(SCM obj);
SCM_API long gh_scm2long(SCM obj);
SCM_API char gh_scm2char(SCM obj);
SCM_API double gh_scm2double(SCM obj);
SCM_API char *gh_scm2newstr(SCM str, size_t *lenp);
SCM_API void gh_get_substr(SCM src, char *dst, long start, size_t len);
SCM_API char *gh_symbol2newstr(SCM sym, size_t *lenp);
SCM_API char *gh_scm2chars(SCM vector, char *result);
SCM_API short *gh_scm2shorts(SCM vector, short *result);
SCM_API long *gh_scm2longs(SCM vector, long *result);
SCM_API float *gh_scm2floats(SCM vector, float *result);
SCM_API double *gh_scm2doubles(SCM vector, double *result);

/* type predicates: tell you if an SCM object has a given type */
SCM_API int gh_boolean_p(SCM val);
SCM_API int gh_symbol_p(SCM val);
SCM_API int gh_char_p(SCM val);
SCM_API int gh_vector_p(SCM val);
SCM_API int gh_pair_p(SCM val);
SCM_API int gh_number_p(SCM val);
SCM_API int gh_string_p(SCM val);
SCM_API int gh_procedure_p(SCM val);
SCM_API int gh_list_p(SCM val);
SCM_API int gh_inexact_p(SCM val);
SCM_API int gh_exact_p(SCM val);

/* more predicates */
SCM_API int gh_eq_p(SCM x, SCM y);
SCM_API int gh_eqv_p(SCM x, SCM y);
SCM_API int gh_equal_p(SCM x, SCM y);
SCM_API int gh_string_equal_p(SCM s1, SCM s2);
SCM_API int gh_null_p(SCM l);

/* standard Scheme procedures available from C */

#define gh_not(x) scm_not(x)

SCM_API SCM gh_define(const char *name, SCM val);

/* string manipulation routines */
#define gh_make_string(k, chr)       scm_make_string(k, chr)
#define gh_string_length(str)        scm_string_length(str)
#define gh_string_ref(str, k)        scm_string_ref(str, k)
#define gh_string_set_x(str, k, chr) scm_string_set_x(str, k, chr)
#define gh_substring(str, start, end) scm_substring(str, start, end)
#define gh_string_append(args)       scm_string_append(args)


/* vector manipulation routines */
/* note that gh_vector() does not behave quite like the Scheme (vector
   obj1 obj2 ...), because the interpreter engine does not pass the
   data element by element, but rather as a list.  thus, gh_vector()
   ends up being identical to gh_list_to_vector() */
#define gh_vector(ls) scm_vector(ls)
SCM_API SCM gh_make_vector(SCM length, SCM val);
SCM_API SCM gh_vector_set_x(SCM vec, SCM pos, SCM val);
SCM_API SCM gh_vector_ref(SCM vec, SCM pos);
SCM_API unsigned long gh_vector_length (SCM v);
SCM_API unsigned long gh_uniform_vector_length (SCM v);
SCM_API SCM gh_uniform_vector_ref (SCM v, SCM ilist);
#define gh_list_to_vector(ls) scm_vector(ls)
#define gh_vector_to_list(v) scm_vector_to_list(v)

SCM_API SCM gh_lookup (const char *sname);
SCM_API SCM gh_module_lookup (SCM module, const char *sname);

SCM_API SCM gh_cons(SCM x, SCM y);
#define gh_list scm_list_n
SCM_API unsigned long gh_length(SCM l);
SCM_API SCM gh_append(SCM args);
SCM_API SCM gh_append2(SCM l1, SCM l2);
SCM_API SCM gh_append3(SCM l1, SCM l2, SCM l3);
SCM_API SCM gh_append4(SCM l1, SCM l2, SCM l3, SCM l4);
#define gh_reverse(ls) scm_reverse(ls)
#define gh_list_tail(ls, k) scm_list_tail(ls, k)
#define gh_list_ref(ls, k) scm_list_ref(ls, k)
#define gh_memq(x, ls) scm_memq(x, ls)
#define gh_memv(x, ls) scm_memv(x, ls)
#define gh_member(x, ls) scm_member(x, ls)
#define gh_assq(x, alist) scm_assq(x, alist)
#define gh_assv(x, alist) scm_assv(x, alist)
#define gh_assoc(x, alist) scm_assoc(x, alist)

SCM_API SCM gh_car(SCM x);
SCM_API SCM gh_cdr(SCM x);

SCM_API SCM gh_caar(SCM x);
SCM_API SCM gh_cadr(SCM x);
SCM_API SCM gh_cdar(SCM x);
SCM_API SCM gh_cddr(SCM x);

SCM_API SCM gh_caaar(SCM x);
SCM_API SCM gh_caadr(SCM x);
SCM_API SCM gh_cadar(SCM x);
SCM_API SCM gh_caddr(SCM x);
SCM_API SCM gh_cdaar(SCM x);
SCM_API SCM gh_cdadr(SCM x);
SCM_API SCM gh_cddar(SCM x);
SCM_API SCM gh_cdddr(SCM x);

SCM_API SCM gh_set_car_x(SCM pair, SCM value);
SCM_API SCM gh_set_cdr_x(SCM pair, SCM value);


/* Calling Scheme functions from C.  */
SCM_API SCM gh_apply (SCM proc, SCM ls);
SCM_API SCM gh_call0 (SCM proc);
SCM_API SCM gh_call1 (SCM proc, SCM arg);
SCM_API SCM gh_call2 (SCM proc, SCM arg1, SCM arg2);
SCM_API SCM gh_call3 (SCM proc, SCM arg1, SCM arg2, SCM arg3);

/* reading and writing Scheme objects.  */
SCM_API void gh_display (SCM x);
SCM_API void gh_write (SCM x);
SCM_API void gh_newline (void);

/* void  gh_gc_mark(SCM)              : mark an SCM as in use. */
/* void  gh_defer_ints()              : don't interrupt code section. */
/* void  gh_allow_ints()              : see gh_defer_ints(). */
/* void  gh_new_cell(SCM, int tag)    : initialize SCM to be of type 'tag' */
/* int   gh_type_p(SCM, tag)          : test if SCM is of type 'tag' */
/* SCM   gh_intern(char*)             : get symbol corresponding to c-string.*/
/* void  gh_set_ext_data(SCM, void*)  : set extension data on SCM */
/* void *gh_get_ext_data(SCM)         : return extension data from SCM. */

/* void  gh_assert(int cond, char *msg, SCM obj); */

#ifdef __cplusplus
}
#endif

#endif

#endif /* __GH_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
