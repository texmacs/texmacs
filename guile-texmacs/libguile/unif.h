/* classes: h_files */

#ifndef SCM_UNIF_H
#define SCM_UNIF_H

/* Copyright (C) 1995,1996,1997,1999,2000,2001, 2004, 2006 Free Software Foundation, Inc.
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
#include "libguile/print.h"



/* This file contains the definitions for arrays and bit vectors.
   Uniform numeric vectors are now in srfi-4.c.
*/


/** Arrays */

typedef struct scm_t_array_dim
{
  ssize_t lbnd;
  ssize_t ubnd;
  ssize_t inc;
} scm_t_array_dim;

SCM_API SCM scm_array_p (SCM v, SCM prot);
SCM_API SCM scm_typed_array_p (SCM v, SCM type);
SCM_API SCM scm_make_array (SCM fill, SCM bounds);
SCM_API SCM scm_make_typed_array (SCM type, SCM fill, SCM bounds);
SCM_API SCM scm_array_rank (SCM ra);
SCM_API size_t scm_c_array_rank (SCM ra);
SCM_API SCM scm_array_dimensions (SCM ra);
SCM_API SCM scm_shared_array_root (SCM ra);
SCM_API SCM scm_shared_array_offset (SCM ra);
SCM_API SCM scm_shared_array_increments (SCM ra);
SCM_API SCM scm_make_shared_array (SCM oldra, SCM mapfunc, SCM dims);
SCM_API SCM scm_transpose_array (SCM ra, SCM args);
SCM_API SCM scm_enclose_array (SCM ra, SCM axes);
SCM_API SCM scm_array_in_bounds_p (SCM v, SCM args);
SCM_API SCM scm_array_ref (SCM v, SCM args);
SCM_API SCM scm_array_set_x (SCM v, SCM obj, SCM args);
SCM_API SCM scm_array_contents (SCM ra, SCM strict);
SCM_API SCM scm_uniform_array_read_x (SCM ra, SCM port_or_fd,
				      SCM start, SCM end);
SCM_API SCM scm_uniform_array_write (SCM v, SCM port_or_fd,
				     SCM start, SCM end);
SCM_API SCM scm_array_to_list (SCM v);
SCM_API SCM scm_list_to_array (SCM ndim, SCM lst);
SCM_API SCM scm_list_to_typed_array (SCM type, SCM ndim, SCM lst);
SCM_API SCM scm_array_type (SCM ra);

SCM_API int scm_is_array (SCM obj);
SCM_API int scm_is_typed_array (SCM obj, SCM type);

SCM_API SCM scm_ra2contig (SCM ra, int copy);

struct scm_t_array_handle;

typedef SCM (*scm_i_t_array_ref) (struct scm_t_array_handle *, ssize_t);
typedef void (*scm_i_t_array_set) (struct scm_t_array_handle *, ssize_t, SCM);

typedef struct scm_t_array_handle {
  SCM array;
  size_t base;
  scm_t_array_dim *dims;
  scm_t_array_dim dim0;
  scm_i_t_array_ref ref;
  scm_i_t_array_set set;
  const void *elements;
  void *writable_elements;
} scm_t_array_handle;

SCM_API void scm_array_get_handle (SCM array, scm_t_array_handle *h);
SCM_API size_t scm_array_handle_rank (scm_t_array_handle *h);
SCM_API scm_t_array_dim *scm_array_handle_dims (scm_t_array_handle *h);
SCM_API ssize_t scm_array_handle_pos (scm_t_array_handle *h, SCM indices);
SCM_API const SCM *scm_array_handle_elements (scm_t_array_handle *h);
SCM_API SCM *scm_array_handle_writable_elements (scm_t_array_handle *h);
SCM_API void scm_array_handle_release (scm_t_array_handle *h);

/* See inline.h for scm_array_handle_ref and scm_array_handle_set */


/** Bit vectors */

SCM_API SCM scm_bitvector_p (SCM vec);
SCM_API SCM scm_bitvector (SCM bits);
SCM_API SCM scm_make_bitvector (SCM len, SCM fill);
SCM_API SCM scm_bitvector_length (SCM vec);
SCM_API SCM scm_bitvector_ref (SCM vec, SCM idx);
SCM_API SCM scm_bitvector_set_x (SCM vec, SCM idx, SCM val);
SCM_API SCM scm_list_to_bitvector (SCM list);
SCM_API SCM scm_bitvector_to_list (SCM vec);
SCM_API SCM scm_bitvector_fill_x (SCM vec, SCM val);

SCM_API SCM scm_bit_count (SCM item, SCM seq);
SCM_API SCM scm_bit_position (SCM item, SCM v, SCM k);
SCM_API SCM scm_bit_set_star_x (SCM v, SCM kv, SCM obj);
SCM_API SCM scm_bit_count_star (SCM v, SCM kv, SCM obj);
SCM_API SCM scm_bit_invert_x (SCM v);
SCM_API SCM scm_istr2bve (SCM str);

SCM_API int scm_is_bitvector (SCM obj);
SCM_API SCM scm_c_make_bitvector (size_t len, SCM fill);
SCM_API size_t scm_c_bitvector_length (SCM vec);
SCM_API SCM scm_c_bitvector_ref (SCM vec, size_t idx);
SCM_API void scm_c_bitvector_set_x (SCM vec, size_t idx, SCM val);
SCM_API const scm_t_uint32 *scm_array_handle_bit_elements (scm_t_array_handle *h);
SCM_API scm_t_uint32 *scm_array_handle_bit_writable_elements (scm_t_array_handle *h);
SCM_API size_t scm_array_handle_bit_elements_offset (scm_t_array_handle *h);
SCM_API const scm_t_uint32 *scm_bitvector_elements (SCM vec,
						    scm_t_array_handle *h,
						    size_t *offp,
						    size_t *lenp,
						    ssize_t *incp);
SCM_API scm_t_uint32 *scm_bitvector_writable_elements (SCM vec, 
						       scm_t_array_handle *h,
						       size_t *offp,
						       size_t *lenp,
						       ssize_t *incp);

/* internal. */

typedef struct scm_i_t_array
{
  SCM v;  /* the contents of the array, e.g., a vector or uniform vector.  */
  unsigned long base;
} scm_i_t_array;

SCM_API scm_t_bits scm_i_tc16_array;
SCM_API scm_t_bits scm_i_tc16_enclosed_array;

#define SCM_I_ARRAY_FLAG_CONTIGUOUS (1 << 16)

#define SCM_I_ARRAYP(a)	    SCM_TYP16_PREDICATE (scm_i_tc16_array, a)
#define SCM_I_ENCLOSED_ARRAYP(a) \
                            SCM_TYP16_PREDICATE (scm_i_tc16_enclosed_array, a)
#define SCM_I_ARRAY_NDIM(x)  ((size_t) (SCM_CELL_WORD_0 (x) >> 17))
#define SCM_I_ARRAY_CONTP(x) (SCM_CELL_WORD_0(x) & SCM_I_ARRAY_FLAG_CONTIGUOUS)

#define SCM_I_ARRAY_MEM(a)  ((scm_i_t_array *) SCM_CELL_WORD_1 (a))
#define SCM_I_ARRAY_V(a)    (SCM_I_ARRAY_MEM (a)->v)
#define SCM_I_ARRAY_BASE(a) (SCM_I_ARRAY_MEM (a)->base)
#define SCM_I_ARRAY_DIMS(a) \
  ((scm_t_array_dim *)((char *) SCM_I_ARRAY_MEM (a) + sizeof (scm_i_t_array)))

SCM_API SCM scm_i_make_ra (int ndim, int enclosed);
SCM_API SCM scm_i_cvref (SCM v, size_t p, int enclosed);
SCM_API SCM scm_i_read_array (SCM port, int c);

/* deprecated. */

#if SCM_ENABLE_DEPRECATED

SCM_API SCM scm_make_uve (long k, SCM prot);
SCM_API SCM scm_array_prototype (SCM ra);
SCM_API SCM scm_list_to_uniform_array (SCM ndim, SCM prot, SCM lst);
SCM_API SCM scm_dimensions_to_uniform_array (SCM dims, SCM prot, SCM fill);
SCM_API SCM scm_make_ra (int ndim);
SCM_API SCM scm_shap2ra (SCM args, const char *what);
SCM_API SCM scm_cvref (SCM v, unsigned long pos, SCM last);
SCM_API void scm_ra_set_contp (SCM ra);
SCM_API long scm_aind (SCM ra, SCM args, const char *what);
SCM_API int scm_raprin1 (SCM exp, SCM port, scm_print_state *pstate);

#endif

SCM_API void scm_init_unif (void);

#endif  /* SCM_UNIF_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
