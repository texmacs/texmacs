/* Copyright (C) 2002, 2006 Free Software Foundation, Inc.
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


#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/validate.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"
#include "libguile/pairs.h"
#include "libguile/unif.h"
#include "libguile/srfi-4.h"

#include "libguile/convert.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

/* char *scm_c_scm2chars (SCM obj, char *dst);
   SCM   scm_c_chars2scm (const char *src, long n);
   SCM   scm_c_chars2byvect (const char *src, long n);
*/

#define CTYPE            char
#define FROM_CTYPE       scm_from_char
#define SCM2CTYPES       scm_c_scm2chars
#define CTYPES2SCM       scm_c_chars2scm
#define CTYPES2UVECT     scm_c_chars2byvect
#if CHAR_MIN == 0
/* 'char' is unsigned. */
#define UVEC_TAG         u8
#define UVEC_CTYPE       scm_t_uint8
#else
/* 'char' is signed. */
#define UVEC_TAG         s8
#define UVEC_CTYPE       scm_t_int8
#endif
#include "libguile/convert.i.c"

/* short *scm_c_scm2shorts (SCM obj, short *dst);
   SCM scm_c_shorts2scm (const short *src, long n);
   SCM scm_c_shorts2svect (const short *src, long n);
*/

#define CTYPE            short
#define FROM_CTYPE       scm_from_short
#define SCM2CTYPES       scm_c_scm2shorts
#define CTYPES2SCM       scm_c_shorts2scm
#define CTYPES2UVECT     scm_c_shorts2svect
#define UVEC_TAG         s16
#define UVEC_CTYPE       scm_t_int16
#include "libguile/convert.i.c"

/* int *scm_c_scm2ints (SCM obj, int *dst);
   SCM scm_c_ints2scm (const int *src, long n);
   SCM scm_c_ints2ivect (const int *src, long n);
   SCM scm_c_uints2uvect (const unsigned int *src, long n);
*/

#define CTYPE            int
#define FROM_CTYPE       scm_from_int
#define SCM2CTYPES       scm_c_scm2ints
#define CTYPES2SCM       scm_c_ints2scm
#define CTYPES2UVECT     scm_c_ints2ivect
#define UVEC_TAG         s32
#define UVEC_CTYPE       scm_t_int32

#define CTYPES2UVECT_2   scm_c_uints2uvect
#define CTYPE_2          unsigned int
#define UVEC_TAG_2       u32
#define UVEC_CTYPE_2     scm_t_uint32

#include "libguile/convert.i.c"

/* long *scm_c_scm2longs (SCM obj, long *dst);
   SCM scm_c_longs2scm (const long *src, long n);
   SCM scm_c_longs2ivect (const long *src, long n);
   SCM scm_c_ulongs2uvect (const unsigned long *src, long n);
*/

#define CTYPE            long
#define FROM_CTYPE       scm_from_long
#define SCM2CTYPES       scm_c_scm2longs
#define CTYPES2SCM       scm_c_longs2scm
#define CTYPES2UVECT     scm_c_longs2ivect
#define UVEC_TAG         s32
#define UVEC_CTYPE       scm_t_int32

#define CTYPES2UVECT_2   scm_c_ulongs2uvect
#define CTYPE_2          unsigned int
#define UVEC_TAG_2       u32
#define UVEC_CTYPE_2     scm_t_uint32

#include "libguile/convert.i.c"

/* float *scm_c_scm2floats (SCM obj, float *dst);
   SCM scm_c_floats2scm (const float *src, long n);
   SCM scm_c_floats2fvect (const float *src, long n);
*/

#define CTYPE            float
#define FROM_CTYPE       scm_from_double
#define SCM2CTYPES       scm_c_scm2floats
#define CTYPES2SCM       scm_c_floats2scm
#define CTYPES2UVECT     scm_c_floats2fvect
#define UVEC_TAG         f32
#define UVEC_CTYPE       float
#include "libguile/convert.i.c"

/* double *scm_c_scm2doubles (SCM obj, double *dst);
   SCM scm_c_doubles2scm (const double *src, long n);
   SCM scm_c_doubles2dvect (const double *src, long n);
*/

#define CTYPE            double
#define FROM_CTYPE       scm_from_double
#define SCM2CTYPES       scm_c_scm2doubles
#define CTYPES2SCM       scm_c_doubles2scm
#define CTYPES2UVECT     scm_c_doubles2dvect
#define UVEC_TAG         f64
#define UVEC_CTYPE       double
#include "libguile/convert.i.c"

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
