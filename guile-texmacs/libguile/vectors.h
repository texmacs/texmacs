/* classes: h_files */

#ifndef SCM_VECTORS_H
#define SCM_VECTORS_H

/* Copyright (C) 1995,1996,1998,2000,2001,2002,2004,2005, 2006 Free Software Foundation, Inc.
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



SCM_API SCM scm_vector_p (SCM x);
SCM_API SCM scm_vector_length (SCM v);
SCM_API SCM scm_vector (SCM l);
SCM_API SCM scm_vector_ref (SCM v, SCM k);
SCM_API SCM scm_vector_set_x (SCM v, SCM k, SCM obj);
SCM_API SCM scm_make_vector (SCM k, SCM fill);
SCM_API SCM scm_vector_to_list (SCM v);
SCM_API SCM scm_vector_fill_x (SCM v, SCM fill_x);
SCM_API SCM scm_vector_move_left_x (SCM vec1, SCM start1, SCM end1,
				    SCM vec2, SCM start2);
SCM_API SCM scm_vector_move_right_x (SCM vec1, SCM start1, SCM end1, 
				     SCM vec2, SCM start2);
SCM_API SCM scm_vector_copy (SCM vec);

SCM_API int scm_is_vector (SCM obj);
SCM_API int scm_is_simple_vector (SCM obj);
SCM_API SCM scm_c_make_vector (size_t len, SCM fill);
SCM_API size_t scm_c_vector_length (SCM vec);
SCM_API SCM scm_c_vector_ref (SCM vec, size_t k);
SCM_API void scm_c_vector_set_x (SCM vec, size_t k, SCM obj);
SCM_API const SCM *scm_vector_elements (SCM vec,
					scm_t_array_handle *h,
					size_t *lenp, ssize_t *incp);
SCM_API SCM *scm_vector_writable_elements (SCM vec,
					   scm_t_array_handle *h,
					   size_t *lenp, ssize_t *incp);

/* Fast, non-checking accessors for simple vectors.
 */
#define SCM_SIMPLE_VECTOR_LENGTH(x)      SCM_I_VECTOR_LENGTH(x)
#define SCM_SIMPLE_VECTOR_REF(x,idx)     ((SCM_I_VECTOR_ELTS(x))[idx])
#define SCM_SIMPLE_VECTOR_SET(x,idx,val) ((SCM_I_VECTOR_WELTS(x))[idx]=(val))

/* Generalized vectors */

SCM_API SCM scm_generalized_vector_p (SCM v);
SCM_API SCM scm_generalized_vector_length (SCM v);
SCM_API SCM scm_generalized_vector_ref (SCM v, SCM idx);
SCM_API SCM scm_generalized_vector_set_x (SCM v, SCM idx, SCM val);
SCM_API SCM scm_generalized_vector_to_list (SCM v);

SCM_API int scm_is_generalized_vector (SCM obj);
SCM_API size_t scm_c_generalized_vector_length (SCM v);
SCM_API SCM scm_c_generalized_vector_ref (SCM v, size_t idx);
SCM_API void scm_c_generalized_vector_set_x (SCM v, size_t idx, SCM val);
SCM_API void scm_generalized_vector_get_handle (SCM vec,
						scm_t_array_handle *h);

/* Internals */

#define SCM_I_IS_VECTOR(x)     (!SCM_IMP(x) && (SCM_TYP7S(x)==scm_tc7_vector))
#define SCM_I_VECTOR_ELTS(x)   ((const SCM *) SCM_CELL_WORD_1 (x))
#define SCM_I_VECTOR_WELTS(x)  ((SCM *) SCM_CELL_WORD_1 (x))
#define SCM_I_VECTOR_LENGTH(x) (((size_t) SCM_CELL_WORD_0 (x)) >> 8)

SCM_API void scm_i_vector_free (SCM vec);
SCM_API SCM  scm_i_vector_equal_p (SCM x, SCM y);

/* Weak vectors share implementation details with ordinary vectors,
   but no one else should.
 */

#define SCM_I_WVECTP(x)                 (!SCM_IMP (x) && \
                                         SCM_TYP7 (x) == scm_tc7_wvect)
#define SCM_I_WVECT_LENGTH              SCM_I_VECTOR_LENGTH
#define SCM_I_WVECT_VELTS               SCM_I_VECTOR_ELTS
#define SCM_I_WVECT_GC_WVELTS           SCM_I_VECTOR_WELTS
#define SCM_I_WVECT_EXTRA(x)            (SCM_CELL_WORD_2 (x))
#define SCM_I_SET_WVECT_EXTRA(x, t)     (SCM_SET_CELL_WORD_2 ((x),(t)))
#define SCM_I_WVECT_GC_CHAIN(x)         (SCM_CELL_OBJECT_3 (x))
#define SCM_I_SET_WVECT_GC_CHAIN(x, o)  (SCM_SET_CELL_OBJECT_3 ((x), (o)))

SCM_API SCM scm_i_allocate_weak_vector (scm_t_bits type, SCM size, SCM fill);

SCM_API void scm_init_vectors (void);

#endif  /* SCM_VECTORS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
