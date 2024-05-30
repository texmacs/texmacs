/* classes: h_files */

#ifndef SCM_PAIRS_H
#define SCM_PAIRS_H

/* Copyright (C) 1995,1996,2000,2001, 2004, 2006, 2008 Free Software Foundation, Inc.
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



#if (SCM_DEBUG_PAIR_ACCESSES == 1)
# define SCM_VALIDATE_PAIR(cell, expr) \
    ((!scm_is_pair (cell) ? scm_error_pair_access (cell), 0 : 0), (expr))
#else
# define SCM_VALIDATE_PAIR(cell, expr) (expr)
#endif

#define scm_is_null(x)          (scm_is_eq ((x), SCM_EOL))

#define SCM_CAR(x)		(SCM_VALIDATE_PAIR (x, SCM_CELL_OBJECT_0 (x)))
#define SCM_CDR(x)		(SCM_VALIDATE_PAIR (x, SCM_CELL_OBJECT_1 (x)))

#define SCM_SETCAR(x, v)	(SCM_VALIDATE_PAIR (x, SCM_SET_CELL_OBJECT_0 ((x), (v))))
#define SCM_SETCDR(x, v)	(SCM_VALIDATE_PAIR (x, SCM_SET_CELL_OBJECT_1 ((x), (v))))

#define SCM_CAAR(OBJ)		SCM_CAR (SCM_CAR (OBJ))
#define SCM_CDAR(OBJ)		SCM_CDR (SCM_CAR (OBJ))
#define SCM_CADR(OBJ)		SCM_CAR (SCM_CDR (OBJ))
#define SCM_CDDR(OBJ)		SCM_CDR (SCM_CDR (OBJ))

#define SCM_CAAAR(OBJ)		SCM_CAR (SCM_CAR (SCM_CAR (OBJ)))
#define SCM_CDAAR(OBJ)		SCM_CDR (SCM_CAR (SCM_CAR (OBJ)))
#define SCM_CADAR(OBJ)		SCM_CAR (SCM_CDR (SCM_CAR (OBJ)))
#define SCM_CDDAR(OBJ)		SCM_CDR (SCM_CDR (SCM_CAR (OBJ)))
#define SCM_CAADR(OBJ)		SCM_CAR (SCM_CAR (SCM_CDR (OBJ)))
#define SCM_CDADR(OBJ)		SCM_CDR (SCM_CAR (SCM_CDR (OBJ)))
#define SCM_CADDR(OBJ)		SCM_CAR (SCM_CDR (SCM_CDR (OBJ)))
#define SCM_CDDDR(OBJ)		SCM_CDR (SCM_CDR (SCM_CDR (OBJ)))

#define SCM_CAAAAR(OBJ)		SCM_CAR (SCM_CAR (SCM_CAR (SCM_CAR (OBJ))))
#define SCM_CDAAAR(OBJ)		SCM_CDR (SCM_CAR (SCM_CAR (SCM_CAR (OBJ))))
#define SCM_CADAAR(OBJ)		SCM_CAR (SCM_CDR (SCM_CAR (SCM_CAR (OBJ))))
#define SCM_CDDAAR(OBJ)		SCM_CDR (SCM_CDR (SCM_CAR (SCM_CAR (OBJ))))
#define SCM_CAADAR(OBJ)		SCM_CAR (SCM_CAR (SCM_CDR (SCM_CAR (OBJ))))
#define SCM_CDADAR(OBJ)		SCM_CDR (SCM_CAR (SCM_CDR (SCM_CAR (OBJ))))
#define SCM_CADDAR(OBJ)		SCM_CAR (SCM_CDR (SCM_CDR (SCM_CAR (OBJ))))
#define SCM_CDDDAR(OBJ)		SCM_CDR (SCM_CDR (SCM_CDR (SCM_CAR (OBJ))))
#define SCM_CAAADR(OBJ)		SCM_CAR (SCM_CAR (SCM_CAR (SCM_CDR (OBJ))))
#define SCM_CDAADR(OBJ)		SCM_CDR (SCM_CAR (SCM_CAR (SCM_CDR (OBJ))))
#define SCM_CADADR(OBJ)		SCM_CAR (SCM_CDR (SCM_CAR (SCM_CDR (OBJ))))
#define SCM_CDDADR(OBJ)		SCM_CDR (SCM_CDR (SCM_CAR (SCM_CDR (OBJ))))
#define SCM_CAADDR(OBJ)		SCM_CAR (SCM_CAR (SCM_CDR (SCM_CDR (OBJ))))
#define SCM_CDADDR(OBJ)		SCM_CDR (SCM_CAR (SCM_CDR (SCM_CDR (OBJ))))
#define SCM_CADDDR(OBJ)		SCM_CAR (SCM_CDR (SCM_CDR (SCM_CDR (OBJ))))
#define SCM_CDDDDR(OBJ)		SCM_CDR (SCM_CDR (SCM_CDR (SCM_CDR (OBJ))))



#if (SCM_DEBUG_PAIR_ACCESSES == 1)
SCM_API void scm_error_pair_access (SCM);
#endif

SCM_API SCM scm_cons (SCM x, SCM y);
SCM_API SCM scm_cons2 (SCM w, SCM x, SCM y);
SCM_API SCM scm_pair_p (SCM x);
SCM_API SCM scm_car (SCM x);
SCM_API SCM scm_cdr (SCM x);
SCM_API SCM scm_set_car_x (SCM pair, SCM value);
SCM_API SCM scm_set_cdr_x (SCM pair, SCM value);

#define SCM_I_D_PAT    0x02 /* 00000010 */
#define SCM_I_A_PAT    0x03 /* 00000011 */
#define SCM_I_DD_PAT   0x0a /* 00001010 */
#define SCM_I_DA_PAT   0x0b /* 00001011 */
#define SCM_I_AD_PAT   0x0e /* 00001110 */
#define SCM_I_AA_PAT   0x0f /* 00001111 */
#define SCM_I_DDD_PAT  0x2a /* 00101010 */
#define SCM_I_DDA_PAT  0x2b /* 00101011 */
#define SCM_I_DAD_PAT  0x2e /* 00101110 */
#define SCM_I_DAA_PAT  0x2f /* 00101111 */
#define SCM_I_ADD_PAT  0x3a /* 00111010 */
#define SCM_I_ADA_PAT  0x3b /* 00111011 */
#define SCM_I_AAD_PAT  0x3e /* 00111110 */
#define SCM_I_AAA_PAT  0x3f /* 00111111 */
#define SCM_I_DDDD_PAT 0xaa /* 10101010 */
#define SCM_I_DDDA_PAT 0xab /* 10101011 */
#define SCM_I_DDAD_PAT 0xae /* 10101110 */
#define SCM_I_DDAA_PAT 0xaf /* 10101111 */
#define SCM_I_DADD_PAT 0xba /* 10111010 */
#define SCM_I_DADA_PAT 0xbb /* 10111011 */
#define SCM_I_DAAD_PAT 0xbe /* 10111110 */
#define SCM_I_DAAA_PAT 0xbf /* 10111111 */
#define SCM_I_ADDD_PAT 0xea /* 11101010 */
#define SCM_I_ADDA_PAT 0xeb /* 11101011 */
#define SCM_I_ADAD_PAT 0xee /* 11101110 */
#define SCM_I_ADAA_PAT 0xef /* 11101111 */
#define SCM_I_AADD_PAT 0xfa /* 11111010 */
#define SCM_I_AADA_PAT 0xfb /* 11111011 */
#define SCM_I_AAAD_PAT 0xfe /* 11111110 */
#define SCM_I_AAAA_PAT 0xff /* 11111111 */

SCM_API SCM scm_i_chase_pairs (SCM x, scm_t_uint32 pattern);

#define scm_cddr(x)   scm_i_chase_pairs ((x), SCM_I_DD_PAT)
#define scm_cdar(x)   scm_i_chase_pairs ((x), SCM_I_DA_PAT)
#define scm_cadr(x)   scm_i_chase_pairs ((x), SCM_I_AD_PAT)
#define scm_caar(x)   scm_i_chase_pairs ((x), SCM_I_AA_PAT)
#define scm_cdddr(x)  scm_i_chase_pairs ((x), SCM_I_DDD_PAT)
#define scm_cddar(x)  scm_i_chase_pairs ((x), SCM_I_DDA_PAT)
#define scm_cdadr(x)  scm_i_chase_pairs ((x), SCM_I_DAD_PAT)
#define scm_cdaar(x)  scm_i_chase_pairs ((x), SCM_I_DAA_PAT)
#define scm_caddr(x)  scm_i_chase_pairs ((x), SCM_I_ADD_PAT)
#define scm_cadar(x)  scm_i_chase_pairs ((x), SCM_I_ADA_PAT)
#define scm_caadr(x)  scm_i_chase_pairs ((x), SCM_I_AAD_PAT)
#define scm_caaar(x)  scm_i_chase_pairs ((x), SCM_I_AAA_PAT)
#define scm_cddddr(x) scm_i_chase_pairs ((x), SCM_I_DDDD_PAT)
#define scm_cdddar(x) scm_i_chase_pairs ((x), SCM_I_DDDA_PAT)
#define scm_cddadr(x) scm_i_chase_pairs ((x), SCM_I_DDAD_PAT)
#define scm_cddaar(x) scm_i_chase_pairs ((x), SCM_I_DDAA_PAT)
#define scm_cdaddr(x) scm_i_chase_pairs ((x), SCM_I_DADD_PAT)
#define scm_cdadar(x) scm_i_chase_pairs ((x), SCM_I_DADA_PAT)
#define scm_cdaadr(x) scm_i_chase_pairs ((x), SCM_I_DAAD_PAT)
#define scm_cdaaar(x) scm_i_chase_pairs ((x), SCM_I_DAAA_PAT)
#define scm_cadddr(x) scm_i_chase_pairs ((x), SCM_I_ADDD_PAT)
#define scm_caddar(x) scm_i_chase_pairs ((x), SCM_I_ADDA_PAT)
#define scm_cadadr(x) scm_i_chase_pairs ((x), SCM_I_ADAD_PAT)
#define scm_cadaar(x) scm_i_chase_pairs ((x), SCM_I_ADAA_PAT)
#define scm_caaddr(x) scm_i_chase_pairs ((x), SCM_I_AADD_PAT)
#define scm_caadar(x) scm_i_chase_pairs ((x), SCM_I_AADA_PAT)
#define scm_caaadr(x) scm_i_chase_pairs ((x), SCM_I_AAAD_PAT)
#define scm_caaaar(x) scm_i_chase_pairs ((x), SCM_I_AAAA_PAT)

SCM_API void scm_init_pairs (void);

#endif  /* SCM_PAIRS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
