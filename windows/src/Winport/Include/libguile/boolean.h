/* classes: h_files */

#ifndef BOOLEANH
#define BOOLEANH
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

#include "libguile/__scm.h"



/* Boolean Values 
 *
 */ 
#define SCM_FALSEP(x)		(SCM_EQ_P ((x), SCM_BOOL_F))
#define SCM_NFALSEP(x)		(!SCM_FALSEP (x))

#define SCM_BOOLP(x)		(SCM_EQ_P ((x), SCM_BOOL_F) || SCM_EQ_P ((x), SCM_BOOL_T))

/* Convert from a C boolean to a SCM boolean value */
#define SCM_BOOL(f)		((f) ? SCM_BOOL_T : SCM_BOOL_F)

/* Convert from a C boolean to a SCM boolean value and negate it */
#define SCM_NEGATE_BOOL(f)	((f) ? SCM_BOOL_F : SCM_BOOL_T)

/* SCM_BOOL_NOT returns the other boolean.  
 * The order of ^s here is important for Borland C++ (!?!?!)
 */
#define SCM_BOOL_NOT(x)		(SCM_PACK (SCM_UNPACK (x) \
					   ^ (SCM_UNPACK (SCM_BOOL_T) \
					      ^ SCM_UNPACK (SCM_BOOL_F))))



GUILE_API extern SCM scm_not (SCM x);
GUILE_API extern SCM scm_boolean_p (SCM obj);
GUILE_API extern void scm_init_boolean (void);

#endif  /* BOOLEANH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
