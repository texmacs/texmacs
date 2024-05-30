/* This file contains definitions for discouraged features.  When you
   discourage something, move it here when that is feasible.

   A discouraged feature is one that shouldn't be used in new code
   since we have a better alternative now.  However, there is nothing
   wrong with using the old feature, so it is OK to continue to use
   it.

   Eventually, discouraged features can be deprecated since removing
   them will make Guile simpler.
*/

#ifndef SCM_DISCOURAGED_H
#define SCM_DISCOURAGED_H

/* Copyright (C) 2004, 2006 Free Software Foundation, Inc.
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

#if SCM_ENABLE_DISCOURAGED == 1

/* Discouraged because they do not follow the naming convention.  That
   is, they end in "P" but return a C boolean.  Also, SCM_BOOLP
   evaluates its argument twice.
*/

#define SCM_FALSEP		scm_is_false
#define SCM_NFALSEP		scm_is_true
#define SCM_BOOLP               scm_is_bool
#define SCM_EQ_P                scm_is_eq


/* Convert from a C boolean to a SCM boolean value */
#define SCM_BOOL		scm_from_bool

/* Convert from a C boolean to a SCM boolean value and negate it */
#define SCM_NEGATE_BOOL(f)	scm_from_bool(!(f))

/* SCM_BOOL_NOT returns the other boolean.  
 * The order of ^s here is important for Borland C++ (!?!?!)
 */
#define SCM_BOOL_NOT(x)		(SCM_PACK (SCM_UNPACK (x) \
					   ^ (SCM_UNPACK (SCM_BOOL_T) \
					      ^ SCM_UNPACK (SCM_BOOL_F))))

/* scm_to_int, scm_from_int are the official functions to do the job,
   but there is nothing wrong with using scm_num2int, etc.

   These could be trivially defined via macros, but we leave them as
   functions since existing code may take their addresses.
*/

SCM_API SCM scm_short2num (short n);
SCM_API SCM scm_ushort2num (unsigned short n);
SCM_API SCM scm_int2num (int n);
SCM_API SCM scm_uint2num (unsigned int n);
SCM_API SCM scm_long2num (long n);
SCM_API SCM scm_ulong2num (unsigned long n);
SCM_API SCM scm_size2num (size_t n);
SCM_API SCM scm_ptrdiff2num (scm_t_ptrdiff n);
SCM_API short scm_num2short (SCM num, unsigned long int pos,
			     const char *s_caller);
SCM_API unsigned short scm_num2ushort (SCM num, unsigned long int pos,
				       const char *s_caller);
SCM_API int scm_num2int (SCM num, unsigned long int pos,
			 const char *s_caller);
SCM_API unsigned int scm_num2uint (SCM num, unsigned long int pos,
				   const char *s_caller);
SCM_API long scm_num2long (SCM num, unsigned long int pos,
			   const char *s_caller);
SCM_API unsigned long scm_num2ulong (SCM num, unsigned long int pos,
				     const char *s_caller);
SCM_API scm_t_ptrdiff scm_num2ptrdiff (SCM num, unsigned long int pos,
                                       const char *s_caller);
SCM_API size_t scm_num2size (SCM num, unsigned long int pos,
			     const char *s_caller);
#if SCM_SIZEOF_LONG_LONG != 0
SCM_API SCM scm_long_long2num (long long sl);
SCM_API SCM scm_ulong_long2num (unsigned long long sl);
SCM_API long long scm_num2long_long (SCM num, unsigned long int pos,
				     const char *s_caller);
SCM_API unsigned long long scm_num2ulong_long (SCM num, unsigned long int pos,
					       const char *s_caller);
#endif

SCM_API SCM scm_make_real (double x);
SCM_API double scm_num2dbl (SCM a, const char * why);
SCM_API SCM scm_float2num (float n);
SCM_API SCM scm_double2num (double n);

/* The next two are implemented in numbers.c since they use features
   only available there.
*/
SCM_API float scm_num2float (SCM num, unsigned long int pos,
			     const char *s_caller);
SCM_API double scm_num2double (SCM num, unsigned long int pos,
			       const char *s_caller);

SCM_API SCM scm_make_complex (double x, double y);

/* Discouraged because they don't make the encoding explicit.
 */

SCM_API SCM scm_mem2symbol (const char *mem, size_t len);
SCM_API SCM scm_mem2uninterned_symbol (const char *mem, size_t len);
SCM_API SCM scm_str2symbol (const char *str);

SCM_API SCM scm_take_str (char *s, size_t len);
SCM_API SCM scm_take0str (char *s);
SCM_API SCM scm_mem2string (const char *src, size_t len);
SCM_API SCM scm_str2string (const char *src);
SCM_API SCM scm_makfrom0str (const char *src);
SCM_API SCM scm_makfrom0str_opt (const char *src);

/* Discouraged because scm_c_make_string has a better name and is more
   consistent with make-string.
 */
SCM_API SCM scm_allocate_string (size_t len);

/* Discouraged because scm_is_symbol has a better name,
 */
#define SCM_SYMBOLP scm_is_symbol

/* Discouraged because the alternatives have the better names.
 */
#define SCM_SYMBOL_FUNC	            scm_symbol_fref
#define SCM_SET_SYMBOL_FUNC         scm_symbol_fset_x
#define SCM_SYMBOL_PROPS	    scm_symbol_pref
#define SCM_SET_SYMBOL_PROPS        scm_symbol_pset_x

/* Discouraged because there are better ways.
 */
#define SCM_SYMBOL_HASH             scm_i_symbol_hash
#define SCM_SYMBOL_INTERNED_P(X)    scm_i_symbol_is_interned

/* Discouraged because they evaluated their arguments twice and/or
   don't fit the naming scheme.
*/

#define SCM_CONSP(x)            (scm_is_pair (x))
#define SCM_NCONSP(x)           (!SCM_CONSP (x))
#define SCM_NULLP(x)		(scm_is_null (x))
#define SCM_NNULLP(x)		(!scm_is_null (x))

/* Discouraged because they are just strange.
 */

SCM_API SCM scm_make_keyword_from_dash_symbol (SCM symbol);
SCM_API SCM scm_keyword_dash_symbol (SCM keyword);

/* Discouraged because it does not state what encoding S is in.
 */

SCM_API SCM scm_c_make_keyword (const char *s);

/* Discouraged because the 'internal' and 'thread' moniker is
   confusing.
 */

#define scm_internal_select scm_std_select
#define scm_thread_sleep    scm_std_sleep
#define scm_thread_usleep   scm_std_usleep

void scm_i_init_discouraged (void);

#endif /* SCM_ENABLE_DISCOURAGED == 1 */

#endif /* SCM_DISCOURAGED_H */
