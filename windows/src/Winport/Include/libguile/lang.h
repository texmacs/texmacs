/* classes: h_files */

#ifndef LANGH
#define LANGH
/*	Copyright (C) 1998 Free Software Foundation, Inc.
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



GUILE_API extern SCM scm_lisp_nil;
GUILE_API extern SCM scm_lisp_t;

#define SCM_NILP(x) (SCM_EQ_P ((x), scm_lisp_nil))
#define SCM_NILNULLP(x) (SCM_NILP (x) || SCM_NULLP (x))
#define SCM_NIL2EOL(x, tmp) (SCM_EQ_P ((tmp = (x)), scm_lisp_nil) ? SCM_EOL : tmp)
#define SCM_EOL2NIL(x, tmp) (SCM_NULLP (tmp = (x)) ? scm_lisp_nil : tmp)
#define SCM_EOL_IFY(x, tmp) (tmp = (x), SCM_NILP (tmp) ? SCM_EOL : tmp)
#define SCM_NIL_IFY(x, tmp) (tmp = (x), SCM_NILP (tmp) ? scm_lisp_nil : tmp)



GUILE_API extern SCM scm_nil_cons (SCM x, SCM y);
GUILE_API extern SCM scm_nil_car (SCM x);
GUILE_API extern SCM scm_nil_cdr (SCM x);
GUILE_API extern SCM scm_null (SCM x);
GUILE_API extern SCM scm_m_while (SCM exp, SCM env);
GUILE_API extern SCM scm_nil_eq (SCM x, SCM y);
GUILE_API extern void scm_init_lang (void);

#endif  /* PAIRSH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
