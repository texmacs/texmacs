/******************************************************************************
 * MODULE     : guile_tm.hpp
 * DESCRIPTION: Everything which depends on the version of Guile
 *              should be move to this file
 * COPYRIGHT  : (C) 1999-2011  Joris van der Hoeven and Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef GUILE_TM_H
#define GUILE_TM_H

#include "tm_configure.hpp"
#include "blackbox.hpp"
#include "array.hpp"


#ifdef OS_MINGW
// we redefine some symbols to avoid name clashes with Windows headers (included by Guile)
#define PATTERN WIN_PATTERN
#define STRING WIN_STRING
#define GROUP WIN_GROUP
#ifdef IN
#define MY_IN IN
#undef IN
#endif
#ifdef OUT
#define MY_OUT OUT
#undef OUT
#endif
#ifdef MENU_EVENT
#define MY_MENU_EVENT MENU_EVENT
#undef MENU_EVENT
#endif
#endif // OS_MINGW

#if defined(GUILE_D) || defined(GUILE_C) 
#ifdef GUILE_HEADER_18
#include <libguile18.h>
#else
#include <libguile.h>
#endif
#else
#ifdef GUILE_HEADER_16
#include <guile16/gh.h>
#else
#include <guile/gh.h>
#endif
#endif

#ifdef OS_MINGW
// put things back
#undef STRING
#undef ERROR
#undef PATTERN
#undef GROUP
#undef IN
#undef OUT
#undef MENU_EVENT
#ifdef MY_MENU_EVENT
#define MENU_EVENT MY_MENU_EVENT
#undef MY_MENU_EVENT
#endif
#ifdef MY_IN
#define IN MY_IN
#undef MY_IN
#endif
#ifdef MY_OUT
#define OUT MY_OUT
#undef MY_OUT
#endif
#endif // OS_MINGW

#ifdef GUILE_D

#define SCM_NULL scm_list_n (SCM_UNDEFINED)
#define scm_bool2scm scm_from_bool
#define scm_is_list(x) scm_is_true(scm_list_p(x))
#define scm_scm2bool scm_is_true
#define scm_is_int scm_is_integer
#define scm_is_double scm_is_real
#define scm_new_procedure(name,r,a,b,c) scm_c_define_gsubr(name,a,b,c,(scm_t_subr)r)
#define scm_lookup_string(name) scm_variable_ref(scm_c_lookup(name))
#define scm_long2scm scm_long2num
#define scm_scm2long(x) scm_num2long(x,SCM_ARG1,"scm2long")
#define scm_double2scm scm_from_double
#define scm_scm2double scm_to_double
#define scm_str2scm scm_from_locale_stringn
#define scm_scm2str scm_to_locale_stringn
#define scm_symbol2scm scm_from_locale_symbol
#define scm_scm2symbol(x,y) scm_to_locale_stringn(scm_symbol_to_string(x),y)

#else
#ifdef GUILE_C

#define SCM_NULL scm_list_n (SCM_UNDEFINED)
#define scm_bool2scm scm_from_bool
#define scm_is_list(x) scm_is_true(scm_list_p(x))
#define scm_scm2bool scm_is_true
#define scm_is_int scm_is_integer
#define scm_is_double scm_is_real
#define scm_new_procedure(name,r,a,b,c) scm_c_define_gsubr(name,a,b,c,r)
#define scm_lookup_string(name) scm_variable_ref(scm_c_lookup(name))
#define scm_long2scm scm_long2num
#define scm_scm2long(x) scm_num2long(x,SCM_ARG1,"scm2long")
#define scm_double2scm scm_from_double
#define scm_scm2double scm_to_double
#define scm_str2scm scm_from_locale_stringn
#define scm_scm2str scm_to_locale_stringn
#define scm_symbol2scm scm_from_locale_symbol
#define scm_scm2symbol(x,y) scm_to_locale_stringn(scm_symbol_to_string(x),y)

#else
#ifdef GUILE_B

#define SCM_NULL gh_list (SCM_UNDEFINED)
#define scm_is_list(x) SCM_NFALSEP(scm_list_p(x))
#define scm_new_procedure(name,r,a,b,c) scm_c_define_gsubr(name,a,b,c,r)
#define scm_lookup_string(name) scm_variable_ref(scm_c_lookup(name))
#define scm_long2scm scm_long2num
#define scm_scm2long(x) scm_num2long(x,SCM_ARG1,"scm2long")

#define scm_is_null(x) SCM_NFALSEP(scm_null_p(x))
#define scm_is_pair(x) SCM_NFALSEP(scm_pair_p(x))
#define scm_is_bool(x) SCM_NFALSEP(scm_boolean_p(x))
#define scm_is_int SCM_INUMP
#define scm_is_double SCM_REALP
#define scm_is_string(obj) (SCM_NIMP(obj) && SCM_STRINGP(obj))
#define scm_is_symbol(x) SCM_NFALSEP(scm_symbol_p(x))

#define scm_bool2scm SCM_BOOL
#define scm_scm2bool SCM_NFALSEP
#define scm_long2scm scm_long2num
#define scm_scm2long(x) scm_num2long(x,SCM_ARG1,"scm2long")
#define scm_double2scm scm_make_real
#define scm_scm2double(x) scm_num2dbl(x,"scm2double")
#define scm_symbol2scm scm_str2symbol
#define scm_scm2symbol gh_symbol2newstr

#define scm_str2scm scm_mem2string
#define scm_scm2str gh_scm2newstr

#else
#ifdef GUILE_A

#define SCM_NULL gh_list (SCM_UNDEFINED)
#define scm_is_bool gh_boolean_p
#define scm_is_int SCM_INUMP
#define scm_is_double SCM_REALP
#define scm_is_string(obj) (SCM_NIMP(obj) && SCM_STRINGP(obj))
#define scm_is_symbol gh_symbol_p
#define scm_is_null gh_null_p
#define scm_is_pair gh_pair_p
#define scm_is_list gh_list_p

#define scm_bool2scm gh_bool2scm
#define scm_scm2bool gh_scm2bool
#define scm_long2scm gh_long2scm
#define scm_scm2long gh_scm2long
#define scm_double2scm gh_double2scm
#define scm_scm2double gh_scm2double
#define scm_str2scm gh_str2scm
#define scm_scm2str gh_scm2newstr
#define scm_symbol2scm gh_symbol2scm
#define scm_scm2symbol gh_symbol2newstr

#define scm_c_primitive_load gh_eval_file
#define scm_c_eval_string gh_eval_str
#define scm_apply_0 gh_apply
#define scm_call_0 gh_call0
#define scm_call_1 gh_call1
#define scm_call_2 gh_call2
#define scm_call_3 gh_call3
#define scm_new_procedure gh_new_procedure
#define scm_lookup_string gh_lookup

typedef SCM (*scm_t_catch_body) (void *data);
typedef SCM (*scm_t_catch_handler) (void *data, SCM tag, SCM throw_args);

#else

#error "At least one of the macros GUILE_{A,B,C,D} should be defined" 

#endif // defined(GUILE_A)
#endif // defined(GUILE_B)
#endif // defined(GUILE_C)
#endif // defined(GUILE_D)

#define SCM_ARG8 8
#define SCM_ARG9 9
#define SCM_ARG10 10
#define SCM_ARG11 11

#ifdef DOTS_OK
typedef SCM (*FN)(...);
#else
typedef SCM (*FN)();
#endif

#if defined(GUILE_A) || defined(GUILE_B)
int scm_to_bool (SCM obj);
int scm_to_int (SCM obj);
long scm_to_long (SCM obj);
double scm_to_double (SCM obj);
#endif


typedef SCM tmscm;

bool tmscm_is_blackbox (tmscm obj);
tmscm blackbox_to_tmscm (blackbox b);
blackbox tmscm_to_blackbox (tmscm obj);

inline tmscm tmscm_null () { return SCM_NULL; }
inline tmscm tmscm_true () { return SCM_BOOL_T; }
inline tmscm tmscm_false () { return SCM_BOOL_F; }
inline void tmscm_set_car (tmscm a, tmscm b) { SCM_SETCAR(a,b); }
inline void tmscm_set_cdr (tmscm a, tmscm b) { SCM_SETCDR(a,b); }
	
	
inline bool tmscm_is_equal (tmscm o1, tmscm o2) { return SCM_NFALSEP ( scm_equal_p(o1, o2)); }



inline bool tmscm_is_null (tmscm obj) { return scm_is_null (obj); }
inline bool tmscm_is_pair (tmscm obj) { return scm_is_pair (obj); }
inline bool tmscm_is_list (tmscm obj) { return scm_is_list (obj); }
inline bool tmscm_is_bool (tmscm obj) { return scm_is_bool (obj); }
inline bool tmscm_is_int (tmscm obj) { return scm_is_int (obj); }
inline bool tmscm_is_double (tmscm obj) { return scm_is_double (obj); }
inline bool tmscm_is_string (tmscm obj) { return scm_is_string (obj); }
inline bool tmscm_is_symbol (tmscm obj) { return scm_is_symbol (obj); }

inline tmscm tmscm_cons (tmscm obj1, tmscm obj2) { return scm_cons (obj1, obj2); }
inline tmscm tmscm_car (tmscm obj) { return SCM_CAR (obj); }
inline tmscm tmscm_cdr (tmscm obj) { return SCM_CDR (obj); }
inline tmscm tmscm_caar (tmscm obj) { return SCM_CAAR (obj); }
inline tmscm tmscm_cadr (tmscm obj) { return SCM_CADR (obj); }
inline tmscm tmscm_cdar (tmscm obj) { return SCM_CDAR (obj); }
inline tmscm tmscm_cddr (tmscm obj) { return SCM_CDDR (obj); }
inline tmscm tmscm_caddr (tmscm obj) { return SCM_CADDR (obj); }
inline tmscm tmscm_cadddr (tmscm obj) { return SCM_CADDDR (obj); }



SCM bool_to_scm (bool b);
SCM int_to_scm (int i);
SCM long_to_scm (long l);
SCM double_to_scm (double i);



inline tmscm bool_to_tmscm (bool b) { return bool_to_scm (b); }
inline tmscm int_to_tmscm (int i) { return int_to_scm (i); }
inline tmscm long_to_tmscm (long l) { return long_to_scm (l); }
inline tmscm double_to_tmscm (double i) { return double_to_scm (i); }
tmscm string_to_tmscm (string s);
tmscm symbol_to_tmscm (string s);

inline bool tmscm_to_bool (tmscm obj) { return scm_to_bool (obj); }
inline int tmscm_to_int (tmscm obj) { return scm_to_int (obj); }
inline double tmscm_to_double (tmscm obj) { return scm_to_double (obj); }
string tmscm_to_string (tmscm obj);
string tmscm_to_symbol (tmscm obj);




tmscm eval_scheme_file (string name);
tmscm eval_scheme (string s);
tmscm call_scheme (tmscm fun);
tmscm call_scheme (tmscm fun, tmscm a1);
tmscm call_scheme (tmscm fun, tmscm a1, tmscm a2);
tmscm call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3);
tmscm call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3, tmscm a4);
tmscm call_scheme (tmscm fun, array<tmscm> a);


#define tmscm_install_procedure(name, func, args, p0, p1) \
  scm_new_procedure (name, ( FN )( func ), args, p0, p1)

#define TMSCM_ASSERT(_cond, _arg, _pos, _subr) \
 SCM_ASSERT(_cond, _arg, _pos, _subr)

#define TMSCM_ARG1 SCM_ARG1
#define TMSCM_ARG2 SCM_ARG2
#define TMSCM_ARG3 SCM_ARG3
#define TMSCM_ARG4 SCM_ARG4
#define TMSCM_ARG5 SCM_ARG5
#define TMSCM_ARG6 SCM_ARG6
#define TMSCM_ARG7 SCM_ARG7
#define TMSCM_ARG8 SCM_ARG8
#define TMSCM_ARG9 SCM_ARG9
#define TMSCM_ARG10 SCM_ARG10

#define TMSCM_UNSPECIFIED SCM_UNSPECIFIED


string scheme_dialect ();


#endif // defined GUILE_TM_H


