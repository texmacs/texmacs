
/******************************************************************************
* MODULE     : guile.hpp
* DESCRIPTION: Everything which depends on the version of Guile
*              should be move to this file
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef GUILE_HH
#define GUILE_HH
#include "tm_configure.hpp"
#include <guile/gh.h>

#define SCM_NULL gh_list (SCM_UNDEFINED)

#ifdef GUILE_A
#define scm_is_bool gh_boolean_p
#define scm_is_int SCM_INUMP
#define scm_is_string gh_string_p
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

typedef SCM (*scm_t_catch_body) (void *data);
typedef SCM (*scm_t_catch_handler) (void *data, SCM tag, SCM throw_args);
#endif

#ifdef GUILE_B
#define scm_is_bool(x) SCM_NFALSEP(scm_boolean_p(x))
#define scm_is_int SCM_INUMP
#define scm_is_string(x) SCM_NFALSEP(scm_string_p(x))
#define scm_is_symbol(x) SCM_NFALSEP(scm_symbol_p(x))
#define scm_is_null(x) SCM_NFALSEP(scm_null_p(x))
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
#endif

#endif // defined GUILE_HH
