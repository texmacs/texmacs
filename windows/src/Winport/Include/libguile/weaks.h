/* classes: h_files */

#ifndef WEAKSH
#define WEAKSH
/*	Copyright (C) 1995,1996, 2000 Free Software Foundation, Inc.   
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




#define SCM_WVECTP(x) (SCM_NIMP(x) && (SCM_TYP7(x)==scm_tc7_wvect))
#define SCM_IS_WHVEC(X) (SCM_UNPACK (SCM_VELTS(X)[-1]) == 1)
#define SCM_IS_WHVEC_V(X) (SCM_UNPACK (SCM_VELTS(X)[-1]) == 2)
#define SCM_IS_WHVEC_B(X) (SCM_UNPACK (SCM_VELTS(X)[-1]) == 3)
#define SCM_IS_WHVEC_ANY(X) (SCM_UNPACK (SCM_VELTS(X)[-1]) != 0)
#define SCM_WVECT_GC_CHAIN(X) (SCM_VELTS(X)[-2])

GUILE_API extern SCM scm_weak_vectors;



GUILE_API extern SCM scm_make_weak_vector (SCM k, SCM fill);
GUILE_API extern SCM scm_weak_vector (SCM l);
GUILE_API extern SCM scm_weak_vector_p (SCM x);
GUILE_API extern SCM scm_make_weak_key_hash_table (SCM k);
GUILE_API extern SCM scm_make_weak_value_hash_table (SCM k);
GUILE_API extern SCM scm_make_doubly_weak_hash_table (SCM k);
GUILE_API extern SCM scm_weak_key_hash_table_p (SCM x);
GUILE_API extern SCM scm_weak_value_hash_table_p (SCM x);
GUILE_API extern SCM scm_doubly_weak_hash_table_p (SCM x);
GUILE_API extern void scm_weaks_prehistory (void);
GUILE_API extern void scm_init_weaks (void);

#endif  /* WEAKSH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
