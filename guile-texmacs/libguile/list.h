/* classes: h_files */

#ifndef SCM_LIST_H
#define SCM_LIST_H

/* Copyright (C) 1995,1996,1997,2000,2001,2003,2004,2005,2006
 * Free Software Foundation, Inc.
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



SCM_API SCM scm_list_1 (SCM e1);
SCM_API SCM scm_list_2 (SCM e1, SCM e2);
SCM_API SCM scm_list_3 (SCM e1, SCM e2, SCM e3);
SCM_API SCM scm_list_4 (SCM e1, SCM e2, SCM e3, SCM e4);
SCM_API SCM scm_list_5 (SCM e1, SCM e2, SCM e3, SCM e4, SCM e5);
SCM_API SCM scm_list_n (SCM elt, ...);
SCM_API SCM scm_list (SCM objs);
SCM_API SCM scm_list_head (SCM lst, SCM k);
SCM_API SCM scm_make_list (SCM n, SCM init);
SCM_API SCM scm_cons_star (SCM arg, SCM objs);
SCM_API SCM scm_null_p (SCM x);
SCM_API SCM scm_list_p (SCM x);
SCM_API long scm_ilength (SCM sx);
SCM_API SCM scm_length (SCM x);
SCM_API SCM scm_append (SCM args);
SCM_API SCM scm_append_x (SCM args);
SCM_API SCM scm_reverse (SCM lst);
SCM_API SCM scm_reverse_x (SCM lst, SCM newtail);
SCM_API SCM scm_list_ref (SCM lst, SCM k);
SCM_API SCM scm_list_set_x (SCM lst, SCM k, SCM val);
SCM_API SCM scm_list_cdr_set_x (SCM lst, SCM k, SCM val);
SCM_API SCM scm_last_pair (SCM sx);
SCM_API SCM scm_list_tail (SCM lst, SCM k);
SCM_API SCM scm_c_memq (SCM x, SCM lst);
SCM_API SCM scm_memq (SCM x, SCM lst);
SCM_API SCM scm_memv (SCM x, SCM lst);
SCM_API SCM scm_member (SCM x, SCM lst);
SCM_API SCM scm_delq_x (SCM item, SCM lst);
SCM_API SCM scm_delv_x (SCM item, SCM lst);
SCM_API SCM scm_delete_x (SCM item, SCM lst);
SCM_API SCM scm_list_copy (SCM lst);
SCM_API SCM scm_delq (SCM item, SCM lst);
SCM_API SCM scm_delv (SCM item, SCM lst);
SCM_API SCM scm_delete (SCM item, SCM lst);
SCM_API SCM scm_delq1_x (SCM item, SCM lst);
SCM_API SCM scm_delv1_x (SCM item, SCM lst);
SCM_API SCM scm_delete1_x (SCM item, SCM lst);
SCM_API SCM scm_filter (SCM pred, SCM list);
SCM_API SCM scm_filter_x (SCM pred, SCM list);



/* Guile internal functions */

SCM_API SCM scm_i_finite_list_copy (SCM /* a list known to be finite */);
SCM_API void scm_init_list (void);

#endif  /* SCM_LIST_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
