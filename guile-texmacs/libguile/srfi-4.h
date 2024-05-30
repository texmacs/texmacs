#ifndef SCM_SRFI_4_H
#define SCM_SRFI_4_H
/* srfi-4.c --- Homogeneous numeric vector datatypes.
 *
 * 	Copyright (C) 2001, 2004, 2006, 2010 Free Software Foundation, Inc.
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
#include "libguile/unif.h"

/* Generic procedures.
 */

SCM_API SCM scm_uniform_vector_p (SCM v);
SCM_API SCM scm_uniform_vector_length (SCM v);
SCM_API SCM scm_uniform_vector_ref (SCM v, SCM idx);
SCM_API SCM scm_uniform_vector_set_x (SCM v, SCM idx, SCM val);
SCM_API SCM scm_uniform_vector_to_list (SCM v);
SCM_API SCM scm_uniform_vector_read_x (SCM v, SCM port_or_fd,
				       SCM start, SCM end);
SCM_API SCM scm_uniform_vector_write (SCM v, SCM port_or_fd,
				      SCM start, SCM end);

SCM_API int scm_is_uniform_vector (SCM obj);
SCM_API size_t scm_c_uniform_vector_length (SCM v);
SCM_API SCM scm_c_uniform_vector_ref (SCM v, size_t idx);
SCM_API void scm_c_uniform_vector_set_x (SCM v, size_t idx, SCM val);
SCM_API size_t scm_array_handle_uniform_element_size (scm_t_array_handle *h);
SCM_API const void *scm_array_handle_uniform_elements (scm_t_array_handle *h);
SCM_API void *scm_array_handle_uniform_writable_elements (scm_t_array_handle *h);
SCM_API const void *scm_uniform_vector_elements (SCM uvec, 
						 scm_t_array_handle *h,
						 size_t *lenp, ssize_t *incp);
SCM_API void *scm_uniform_vector_writable_elements (SCM uvec, 
						    scm_t_array_handle *h,
						    size_t *lenp,
						    ssize_t *incp);

/* Specific procedures.
 */

SCM_API SCM scm_u8vector_p (SCM obj);
SCM_API SCM scm_make_u8vector (SCM n, SCM fill);
SCM_API SCM scm_take_u8vector (scm_t_uint8 *data, size_t n);
SCM_API SCM scm_u8vector (SCM l);
SCM_API SCM scm_u8vector_length (SCM uvec);
SCM_API SCM scm_u8vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_u8vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_u8vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_u8vector (SCM l);
SCM_API SCM scm_any_to_u8vector (SCM obj);
SCM_API const scm_t_uint8 *scm_array_handle_u8_elements (scm_t_array_handle *h);
SCM_API scm_t_uint8 *scm_array_handle_u8_writable_elements (scm_t_array_handle *h);
SCM_API const scm_t_uint8 *scm_u8vector_elements (SCM uvec, 
						  scm_t_array_handle *h,
						  size_t *lenp, ssize_t *incp);
SCM_API scm_t_uint8 *scm_u8vector_writable_elements (SCM uvec, 
						     scm_t_array_handle *h,
						     size_t *lenp,
						     ssize_t *incp);

SCM_API SCM scm_s8vector_p (SCM obj);
SCM_API SCM scm_make_s8vector (SCM n, SCM fill);
SCM_API SCM scm_take_s8vector (scm_t_int8 *data, size_t n);
SCM_API SCM scm_s8vector (SCM l);
SCM_API SCM scm_s8vector_length (SCM uvec);
SCM_API SCM scm_s8vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_s8vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_s8vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_s8vector (SCM l);
SCM_API SCM scm_any_to_s8vector (SCM obj);
SCM_API const scm_t_int8 *scm_array_handle_s8_elements (scm_t_array_handle *h);
SCM_API scm_t_int8 *scm_array_handle_s8_writable_elements (scm_t_array_handle *h);
SCM_API const scm_t_int8 *scm_s8vector_elements (SCM uvec, 
						 scm_t_array_handle *h,
						 size_t *lenp, ssize_t *incp);
SCM_API scm_t_int8 *scm_s8vector_writable_elements (SCM uvec, 
						    scm_t_array_handle *h,
						    size_t *lenp,
						    ssize_t *incp);

SCM_API SCM scm_u16vector_p (SCM obj);
SCM_API SCM scm_make_u16vector (SCM n, SCM fill);
SCM_API SCM scm_take_u16vector (scm_t_uint16 *data, size_t n);
SCM_API SCM scm_u16vector (SCM l);
SCM_API SCM scm_u16vector_length (SCM uvec);
SCM_API SCM scm_u16vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_u16vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_u16vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_u16vector (SCM l);
SCM_API SCM scm_any_to_u16vector (SCM obj);
SCM_API const scm_t_uint16 *scm_array_handle_u16_elements (scm_t_array_handle *h);
SCM_API scm_t_uint16 *scm_array_handle_u16_writable_elements (scm_t_array_handle *h);
SCM_API const scm_t_uint16 *scm_u16vector_elements (SCM uvec, 
						    scm_t_array_handle *h,
						    size_t *lenp,
						    ssize_t *incp);
SCM_API scm_t_uint16 *scm_u16vector_writable_elements (SCM uvec, 
						       scm_t_array_handle *h,
						       size_t *lenp,
						       ssize_t *incp);

SCM_API SCM scm_s16vector_p (SCM obj);
SCM_API SCM scm_make_s16vector (SCM n, SCM fill);
SCM_API SCM scm_take_s16vector (scm_t_int16 *data, size_t n);
SCM_API SCM scm_s16vector (SCM l);
SCM_API SCM scm_s16vector_length (SCM uvec);
SCM_API SCM scm_s16vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_s16vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_s16vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_s16vector (SCM l);
SCM_API SCM scm_any_to_s16vector (SCM obj);
SCM_API const scm_t_int16 *scm_array_handle_s16_elements (scm_t_array_handle *h);
SCM_API scm_t_int16 *scm_array_handle_s16_writable_elements (scm_t_array_handle *h);
SCM_API const scm_t_int16 *scm_s16vector_elements (SCM uvec, 
						   scm_t_array_handle *h,
						   size_t *lenp, ssize_t *incp);
SCM_API scm_t_int16 *scm_s16vector_writable_elements (SCM uvec, 
						      scm_t_array_handle *h,
						      size_t *lenp,
						      ssize_t *incp);

SCM_API SCM scm_u32vector_p (SCM obj);
SCM_API SCM scm_make_u32vector (SCM n, SCM fill);
SCM_API SCM scm_take_u32vector (scm_t_uint32 *data, size_t n);
SCM_API SCM scm_u32vector (SCM l);
SCM_API SCM scm_u32vector_length (SCM uvec);
SCM_API SCM scm_u32vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_u32vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_u32vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_u32vector (SCM l);
SCM_API SCM scm_any_to_u32vector (SCM obj);
SCM_API const scm_t_uint32 *scm_array_handle_u32_elements (scm_t_array_handle *h);
SCM_API scm_t_uint32 *scm_array_handle_u32_writable_elements (scm_t_array_handle *h);
SCM_API const scm_t_uint32 *scm_u32vector_elements (SCM uvec, 
						    scm_t_array_handle *h,
						    size_t *lenp,
						    ssize_t *incp);
SCM_API scm_t_uint32 *scm_u32vector_writable_elements (SCM uvec, 
						       scm_t_array_handle *h,
						       size_t *lenp,
						       ssize_t *incp);

SCM_API SCM scm_s32vector_p (SCM obj);
SCM_API SCM scm_make_s32vector (SCM n, SCM fill);
SCM_API SCM scm_take_s32vector (scm_t_int32 *data, size_t n);
SCM_API SCM scm_s32vector (SCM l);
SCM_API SCM scm_s32vector_length (SCM uvec);
SCM_API SCM scm_s32vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_s32vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_s32vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_s32vector (SCM l);
SCM_API SCM scm_any_to_s32vector (SCM obj);
SCM_API const scm_t_int32 *scm_array_handle_s32_elements (scm_t_array_handle *h);
SCM_API scm_t_int32 *scm_array_handle_s32_writable_elements (scm_t_array_handle *h);
SCM_API const scm_t_int32 *scm_s32vector_elements (SCM uvec, 
						   scm_t_array_handle *h,
						   size_t *lenp, ssize_t *incp);
SCM_API scm_t_int32 *scm_s32vector_writable_elements (SCM uvec, 
						      scm_t_array_handle *h,
						      size_t *lenp,
						      ssize_t *incp);

SCM_API SCM scm_u64vector_p (SCM obj);
SCM_API SCM scm_make_u64vector (SCM n, SCM fill);
SCM_API SCM scm_u64vector (SCM l);
SCM_API SCM scm_u64vector_length (SCM uvec);
SCM_API SCM scm_u64vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_u64vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_u64vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_u64vector (SCM l);
SCM_API SCM scm_any_to_u64vector (SCM obj);

SCM_API SCM scm_take_u64vector (scm_t_uint64 *data, size_t n);
SCM_API const scm_t_uint64 *scm_array_handle_u64_elements (scm_t_array_handle *h);
SCM_API scm_t_uint64 *scm_array_handle_u64_writable_elements (scm_t_array_handle *h);
SCM_API const scm_t_uint64 *scm_u64vector_elements (SCM uvec, 
						    scm_t_array_handle *h,
						    size_t *lenp,
						    ssize_t *incp);
SCM_API scm_t_uint64 *scm_u64vector_writable_elements (SCM uvec, 
						       scm_t_array_handle *h,
						       size_t *lenp,
						       ssize_t *incp);

SCM_API SCM scm_s64vector_p (SCM obj);
SCM_API SCM scm_make_s64vector (SCM n, SCM fill);
SCM_API SCM scm_s64vector (SCM l);
SCM_API SCM scm_s64vector_length (SCM uvec);
SCM_API SCM scm_s64vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_s64vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_s64vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_s64vector (SCM l);
SCM_API SCM scm_any_to_s64vector (SCM obj);

SCM_API SCM scm_take_s64vector (scm_t_int64 *data, size_t n);
SCM_API const scm_t_int64 *scm_array_handle_s64_elements (scm_t_array_handle *h);
SCM_API scm_t_int64 *scm_array_handle_s64_writable_elements (scm_t_array_handle *h);
SCM_API const scm_t_int64 *scm_s64vector_elements (SCM uvec, 
						   scm_t_array_handle *h,
						   size_t *lenp, ssize_t *incp);
SCM_API scm_t_int64 *scm_s64vector_writable_elements (SCM uvec, 
						      scm_t_array_handle *h,
						      size_t *lenp,
						      ssize_t *incp);

SCM_API SCM scm_f32vector_p (SCM obj);
SCM_API SCM scm_make_f32vector (SCM n, SCM fill);
SCM_API SCM scm_take_f32vector (float *data, size_t n);
SCM_API SCM scm_f32vector (SCM l);
SCM_API SCM scm_f32vector_length (SCM uvec);
SCM_API SCM scm_f32vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_f32vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_f32vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_f32vector (SCM l);
SCM_API SCM scm_any_to_f32vector (SCM obj);
SCM_API const float *scm_array_handle_f32_elements (scm_t_array_handle *h);
SCM_API float *scm_array_handle_f32_writable_elements (scm_t_array_handle *h);
SCM_API const float *scm_f32vector_elements (SCM uvec, 
					    scm_t_array_handle *h,
					    size_t *lenp, ssize_t *incp);
SCM_API float *scm_f32vector_writable_elements (SCM uvec, 
						scm_t_array_handle *h,
						size_t *lenp,
						ssize_t *incp);

SCM_API SCM scm_f64vector_p (SCM obj);
SCM_API SCM scm_make_f64vector (SCM n, SCM fill);
SCM_API SCM scm_take_f64vector (double *data, size_t n);
SCM_API SCM scm_f64vector (SCM l);
SCM_API SCM scm_f64vector_length (SCM uvec);
SCM_API SCM scm_f64vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_f64vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_f64vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_f64vector (SCM l);
SCM_API SCM scm_any_to_f64vector (SCM obj);
SCM_API const double *scm_array_handle_f64_elements (scm_t_array_handle *h);
SCM_API double *scm_array_handle_f64_writable_elements (scm_t_array_handle *h);
SCM_API const double *scm_f64vector_elements (SCM uvec, 
					      scm_t_array_handle *h,
					      size_t *lenp, ssize_t *incp);
SCM_API double *scm_f64vector_writable_elements (SCM uvec, 
						 scm_t_array_handle *h,
						 size_t *lenp,
						 ssize_t *incp);

SCM_API SCM scm_c32vector_p (SCM obj);
SCM_API SCM scm_make_c32vector (SCM n, SCM fill);
SCM_API SCM scm_take_c32vector (float *data, size_t n);
SCM_API SCM scm_c32vector (SCM l);
SCM_API SCM scm_c32vector_length (SCM uvec);
SCM_API SCM scm_c32vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_c32vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_c32vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_c32vector (SCM l);
SCM_API SCM scm_any_to_c32vector (SCM obj);
SCM_API const float *scm_array_handle_c32_elements (scm_t_array_handle *h);
SCM_API float *scm_array_handle_c32_writable_elements (scm_t_array_handle *h);
SCM_API const float *scm_c32vector_elements (SCM uvec, 
					     scm_t_array_handle *h,
					     size_t *lenp, ssize_t *incp);
SCM_API float *scm_c32vector_writable_elements (SCM uvec, 
						scm_t_array_handle *h,
						size_t *lenp,
						ssize_t *incp);

SCM_API SCM scm_c64vector_p (SCM obj);
SCM_API SCM scm_make_c64vector (SCM n, SCM fill);
SCM_API SCM scm_take_c64vector (double *data, size_t n);
SCM_API SCM scm_c64vector (SCM l);
SCM_API SCM scm_c64vector_length (SCM uvec);
SCM_API SCM scm_c64vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_c64vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_c64vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_c64vector (SCM l);
SCM_API SCM scm_any_to_c64vector (SCM obj);
SCM_API const double *scm_array_handle_c64_elements (scm_t_array_handle *h);
SCM_API double *scm_array_handle_c64_writable_elements (scm_t_array_handle *h);
SCM_API const double *scm_c64vector_elements (SCM uvec, 
					      scm_t_array_handle *h,
					      size_t *lenp, ssize_t *incp);
SCM_API double *scm_c64vector_writable_elements (SCM uvec, 
						 scm_t_array_handle *h,
						 size_t *lenp,
						 ssize_t *incp);

SCM_API SCM scm_i_generalized_vector_type (SCM vec);
SCM_API const char *scm_i_uniform_vector_tag (SCM uvec);
SCM_API scm_i_t_array_ref scm_i_uniform_vector_ref_proc (SCM uvec);
SCM_API scm_i_t_array_set scm_i_uniform_vector_set_proc (SCM uvec);

#if SCM_ENABLE_DEPRECATED

/* Deprecated because we want people to use the scm_t_array_handle
   interface.
*/

SCM_API size_t scm_uniform_element_size (SCM obj);

#endif

SCM_API void scm_init_srfi_4 (void);

#endif /* SCM_SRFI_4_H */
