/* classes: h_files */

#ifndef UNIFH
#define UNIFH
/*	Copyright (C) 1995,1996,1997,1999, 2000 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */


#include "libguile/__scm.h"



/*
  an array SCM is a non-immediate pointing to a  heap cell with:

   CAR: bits 0-14 hold the dimension (0 -- 32767)
        bit  15 is the SCM_ARRAY_CONTIGUOUS flag
        bits 16-31 hold the smob type id: scm_tc16_array
   CDR: pointer to a malloced block containing an scm_array structure
        followed by an scm_array_dim structure for each dimension.
*/

typedef struct scm_array
{
  SCM v;  /* the contents of the array, e.g., a vector or uniform vector.  */
  scm_sizet base;
} scm_array;

typedef struct scm_array_dim
{
  long lbnd;
  long ubnd;
  long inc;
} scm_array_dim;


GUILE_API extern long scm_tc16_array;
#define SCM_ARRAYP(a) 		(SCM_NIMP(a) && (scm_tc16_array == SCM_TYP16(a)))
#define SCM_ARRAY_NDIM(x) 	((scm_sizet) (SCM_CELL_WORD_0 (x) >> 17))
#define SCM_ARRAY_CONTIGUOUS 	0x10000
#define SCM_ARRAY_CONTP(x) 	(SCM_ARRAY_CONTIGUOUS & (SCM_CELL_WORD_0 (x)))

#define SCM_ARRAY_V(a) 	  (((scm_array *) SCM_CELL_WORD_1 (a))->v)
#define SCM_ARRAY_BASE(a) (((scm_array *) SCM_CELL_WORD_1 (a))->base)
#define SCM_ARRAY_DIMS(a) ((scm_array_dim *)(SCM_CHARS(a)+sizeof(scm_array))) 

/* apparently it's possible to have more than SCM_LENGTH_MAX elements
   in an array: if the length is SCM_LENGTH_MAX then the SCM_VELTS
   block begins with the true length (a long int).  I wonder if it
   works.  */

#define SCM_HUGE_LENGTH(x)\
  (SCM_LENGTH_MAX==SCM_LENGTH(x) ? *((long *)SCM_VELTS(x)) : SCM_LENGTH(x))



GUILE_API extern scm_sizet scm_uniform_element_size (SCM obj);
GUILE_API extern SCM scm_make_uve (long k, SCM prot);
GUILE_API extern SCM scm_uniform_vector_length (SCM v);
GUILE_API extern SCM scm_array_p (SCM v, SCM prot);
GUILE_API extern SCM scm_array_rank (SCM ra);
GUILE_API extern SCM scm_array_dimensions (SCM ra);
GUILE_API extern SCM scm_shared_array_root (SCM ra);
GUILE_API extern SCM scm_shared_array_offset (SCM ra);
GUILE_API extern SCM scm_shared_array_increments (SCM ra);
GUILE_API extern long scm_aind (SCM ra, SCM args, const char *what);
GUILE_API extern SCM scm_make_ra (int ndim);
GUILE_API extern SCM scm_shap2ra (SCM args, const char *what);
GUILE_API extern SCM scm_dimensions_to_uniform_array (SCM dims, SCM prot, SCM fill);
GUILE_API extern void scm_ra_set_contp (SCM ra);
GUILE_API extern SCM scm_make_shared_array (SCM oldra, SCM mapfunc, SCM dims);
GUILE_API extern SCM scm_transpose_array (SCM ra, SCM args);
GUILE_API extern SCM scm_enclose_array (SCM ra, SCM axes);
GUILE_API extern SCM scm_array_in_bounds_p (SCM v, SCM args);
GUILE_API extern SCM scm_uniform_vector_ref (SCM v, SCM args);
GUILE_API extern SCM scm_cvref (SCM v, scm_sizet pos, SCM last);
GUILE_API extern SCM scm_array_set_x (SCM v, SCM obj, SCM args);
GUILE_API extern SCM scm_array_contents (SCM ra, SCM strict);
GUILE_API extern SCM scm_ra2contig (SCM ra, int copy);
GUILE_API extern SCM scm_uniform_array_read_x (SCM ra, SCM port_or_fd, SCM start, SCM end);
GUILE_API extern SCM scm_uniform_array_write (SCM v, SCM port_or_fd, SCM start, SCM end);
GUILE_API extern SCM scm_bit_count (SCM item, SCM seq);
GUILE_API extern SCM scm_bit_position (SCM item, SCM v, SCM k);
GUILE_API extern SCM scm_bit_set_star_x (SCM v, SCM kv, SCM obj);
GUILE_API extern SCM scm_bit_count_star (SCM v, SCM kv, SCM obj);
GUILE_API extern SCM scm_bit_invert_x (SCM v);
GUILE_API extern SCM scm_istr2bve (char *str, long len);
GUILE_API extern SCM scm_array_to_list (SCM v);
GUILE_API extern SCM scm_list_to_uniform_array (SCM ndim, SCM prot, SCM lst);
GUILE_API extern int scm_raprin1 (SCM exp, SCM port, scm_print_state *pstate);
GUILE_API extern SCM scm_array_prototype (SCM ra);
GUILE_API extern void scm_init_unif (void);

#endif  /* UNIFH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
