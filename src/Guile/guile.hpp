
/******************************************************************************
* MODULE     : guile.hpp
* DESCRIPTION: Everything which depends on the version of Guile
*              should be move to this file
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef GUILE_HH
#define GUILE_HH
#include "tm_configure.hpp"
#ifdef __MINGW32__
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
#include <guile/gh.h>
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
#else
#include <guile/gh.h>
#endif


#define SCM_NULL gh_list (SCM_UNDEFINED)

#ifdef GUILE_A
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
#endif

#ifndef GUILE_A
#define scm_is_bool(x) SCM_NFALSEP(scm_boolean_p(x))
#ifdef GUILE_B
#define scm_is_int SCM_INUMP
#define scm_is_double SCM_REALP
#define scm_is_string(obj) (SCM_NIMP(obj) && SCM_STRINGP(obj))
#else
#define scm_is_int scm_is_integer
#define scm_is_double scm_is_real
#endif
#ifndef scm_is_symbol
#define scm_is_symbol(x) SCM_NFALSEP(scm_symbol_p(x))
#endif
#ifndef scm_is_null
#define scm_is_null(x) SCM_NFALSEP(scm_null_p(x))
#endif
#define scm_is_pair(x) SCM_NFALSEP(scm_pair_p(x))
#define scm_is_list(x) SCM_NFALSEP(scm_list_p(x))

#define scm_bool2scm SCM_BOOL
#define scm_scm2bool SCM_NFALSEP
#define scm_long2scm scm_long2num
#define scm_scm2long(x) scm_num2long(x,SCM_ARG1,"scm2long")
#define scm_double2scm scm_make_real
#define scm_scm2double(x) scm_num2dbl(x,"scm2double")
#define scm_str2scm scm_mem2string
#define scm_scm2str gh_scm2newstr
#define scm_symbol2scm scm_str2symbol
#define scm_scm2symbol gh_symbol2newstr

#ifdef GUILE_C
#define scm_new_procedure(name,r,a,b,c) scm_c_define_gsubr(name,a,b,c,r)
#define scm_lookup_string(name) scm_variable_ref(scm_c_lookup(name))
#else
#define scm_new_procedure gh_new_procedure
#define scm_lookup_string gh_lookup
#endif
#endif

#endif // defined GUILE_HH
