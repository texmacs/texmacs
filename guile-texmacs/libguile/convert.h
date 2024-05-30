/* classes: h_files */

#ifndef SCM_CONVERT_H
#define SCM_CONVERT_H

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



#include "libguile/__scm.h"

SCM_API char *scm_c_scm2chars (SCM obj, char *dst);
SCM_API short *scm_c_scm2shorts (SCM obj, short *dst);
SCM_API int *scm_c_scm2ints (SCM obj, int *dst);
SCM_API long *scm_c_scm2longs (SCM obj, long *dst);
SCM_API float *scm_c_scm2floats (SCM obj, float *dst);
SCM_API double *scm_c_scm2doubles (SCM obj, double *dst);

SCM_API SCM scm_c_chars2scm (const char *src, long n);
SCM_API SCM scm_c_shorts2scm (const short *src, long n);
SCM_API SCM scm_c_ints2scm (const int *src, long n);
SCM_API SCM scm_c_longs2scm (const long *src, long n);
SCM_API SCM scm_c_floats2scm (const float *src, long n);
SCM_API SCM scm_c_doubles2scm (const double *src, long n);

SCM_API SCM scm_c_chars2byvect (const char *src, long n);
SCM_API SCM scm_c_shorts2svect (const short *src, long n);
SCM_API SCM scm_c_ints2ivect (const int *src, long n);
SCM_API SCM scm_c_uints2uvect (const unsigned int *src, long n);
SCM_API SCM scm_c_longs2ivect (const long *src, long n);
SCM_API SCM scm_c_ulongs2uvect (const unsigned long *src, long n);
SCM_API SCM scm_c_floats2fvect (const float *src, long n);
SCM_API SCM scm_c_doubles2dvect (const double *src, long n);

#endif /* SCM_CONVERT_H */
