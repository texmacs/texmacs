/* classes: h_files */

#ifndef LISTH
#define LISTH
/*	Copyright (C) 1995,1996,1997, 2000 Free Software Foundation, Inc.
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



#define SCM_LIST0 SCM_EOL
#define SCM_LIST1(e0) scm_cons ((e0), SCM_EOL)
#define SCM_LIST2(e0, e1) scm_cons2 ((e0), (e1), SCM_EOL)
#define SCM_LIST3(e0, e1, e2) scm_cons ((e0), SCM_LIST2 ((e1), (e2)))
#define SCM_LIST4(e0, e1, e2, e3)\
     scm_cons2 ((e0), (e1), SCM_LIST2 ((e2), (e3)))
#define SCM_LIST5(e0, e1, e2, e3, e4)\
     scm_cons ((e0), SCM_LIST4 ((e1), (e2), (e3), (e4)))
#define SCM_LIST6(e0, e1, e2, e3, e4, e5)\
     scm_cons2 ((e0), (e1), SCM_LIST4 ((e2), (e3), (e4), (e5)))
#define SCM_LIST7(e0, e1, e2, e3, e4, e5, e6)\
     scm_cons ((e0), SCM_LIST6 ((e1), (e2), (e3), (e4), (e5), (e6)))
#define SCM_LIST8(e0, e1, e2, e3, e4, e5, e6, e7)\
     scm_cons2 ((e0), (e1), SCM_LIST6 ((e2), (e3), (e4), (e5), (e6), (e7)))
#define SCM_LIST9(e0, e1, e2, e3, e4, e5, e6, e7, e8)\
     scm_cons ((e0),\
	       SCM_LIST8 ((e1), (e2), (e3), (e4), (e5), (e6), (e7), (e8)))

GUILE_API extern SCM scm_list_head (SCM lst, SCM k);
GUILE_API extern SCM scm_listify (SCM elt, ...);
GUILE_API extern SCM scm_list (SCM objs);
GUILE_API extern SCM scm_cons_star (SCM arg, SCM objs);
GUILE_API extern SCM scm_null_p (SCM x);
GUILE_API extern SCM scm_list_p (SCM x);
GUILE_API extern long scm_ilength (SCM sx);
GUILE_API extern SCM scm_length (SCM x);
GUILE_API extern SCM scm_append (SCM args);
GUILE_API extern SCM scm_append_x (SCM args);
GUILE_API extern SCM scm_reverse (SCM lst);
GUILE_API extern SCM scm_reverse_x (SCM lst, SCM newtail);
GUILE_API extern SCM scm_list_ref (SCM lst, SCM k);
GUILE_API extern SCM scm_list_set_x (SCM lst, SCM k, SCM val);
GUILE_API extern SCM scm_list_cdr_set_x (SCM lst, SCM k, SCM val);
GUILE_API extern SCM scm_last_pair (SCM sx);
GUILE_API extern SCM scm_list_tail (SCM lst, SCM k);
GUILE_API extern SCM scm_sloppy_memq (SCM x, SCM lst);
GUILE_API extern SCM scm_sloppy_memv (SCM x, SCM lst);
GUILE_API extern SCM scm_sloppy_member (SCM x, SCM lst);
GUILE_API extern SCM scm_memq (SCM x, SCM lst);
GUILE_API extern SCM scm_memv (SCM x, SCM lst);
GUILE_API extern SCM scm_member (SCM x, SCM lst);
GUILE_API extern SCM scm_delq_x (SCM item, SCM lst);
GUILE_API extern SCM scm_delv_x (SCM item, SCM lst);
GUILE_API extern SCM scm_delete_x (SCM item, SCM lst);
GUILE_API extern SCM scm_list_copy (SCM lst);
GUILE_API extern SCM scm_delq (SCM item, SCM lst);
GUILE_API extern SCM scm_delv (SCM item, SCM lst);
GUILE_API extern SCM scm_delete (SCM item, SCM lst);
GUILE_API extern SCM scm_delq1_x (SCM item, SCM lst);
GUILE_API extern SCM scm_delv1_x (SCM item, SCM lst);
GUILE_API extern SCM scm_delete1_x (SCM item, SCM lst);
GUILE_API extern void scm_init_list (void);



#if (SCM_DEBUG_DEPRECATED == 0)

#define scm_list_star scm_cons_star

#endif  /* SCM_DEBUG_DEPRECATED == 0 */

#endif  /* LISTH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
