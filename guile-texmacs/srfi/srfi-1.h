#ifndef SCM_SRFI_1_H
#define SCM_SRFI_1_H
/* srfi-1.h --- SRFI-1 procedures for Guile
 *
 * 	Copyright (C) 2002, 2003, 2005, 2006 Free Software Foundation, Inc.
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


/* SCM_SRFI1_API is a macro prepended to all function and data definitions
   which should be exported or imported in the resulting dynamic link
   library in the Win32 port. */

#if defined (SCM_SRFI1_IMPORT)
# define SCM_SRFI1_API __declspec (dllimport) extern
#elif defined (SCM_SRFI1_EXPORT) || defined (DLL_EXPORT)
# define SCM_SRFI1_API __declspec (dllexport) extern
#else
# define SCM_SRFI1_API extern
#endif

SCM_SRFI1_API SCM scm_srfi1_alist_copy (SCM alist);
SCM_SRFI1_API SCM scm_srfi1_append_reverse (SCM revhead, SCM tail);
SCM_SRFI1_API SCM scm_srfi1_append_reverse_x (SCM revhead, SCM tail);
SCM_SRFI1_API SCM scm_srfi1_break (SCM pred, SCM lst);
SCM_SRFI1_API SCM scm_srfi1_break_x (SCM pred, SCM lst);
SCM_SRFI1_API SCM scm_srfi1_car_plus_cdr (SCM pair);
SCM_SRFI1_API SCM scm_srfi1_concatenate (SCM lstlst);
SCM_SRFI1_API SCM scm_srfi1_concatenate_x (SCM lstlst);
SCM_SRFI1_API SCM scm_srfi1_count (SCM pred, SCM list1, SCM rest);
SCM_SRFI1_API SCM scm_srfi1_delete (SCM x, SCM lst, SCM pred);
SCM_SRFI1_API SCM scm_srfi1_delete_x (SCM x, SCM lst, SCM pred);
SCM_SRFI1_API SCM scm_srfi1_delete_duplicates (SCM lst, SCM pred);
SCM_SRFI1_API SCM scm_srfi1_delete_duplicates_x (SCM lst, SCM pred);
SCM_SRFI1_API SCM scm_srfi1_drop_right (SCM lst, SCM n);
SCM_SRFI1_API SCM scm_srfi1_drop_right_x (SCM lst, SCM n);
SCM_SRFI1_API SCM scm_srfi1_drop_while (SCM pred, SCM lst);
SCM_SRFI1_API SCM scm_srfi1_eighth (SCM lst);
SCM_SRFI1_API SCM scm_srfi1_fifth (SCM lst);
SCM_SRFI1_API SCM scm_srfi1_filter_map (SCM proc, SCM list1, SCM rest);
SCM_SRFI1_API SCM scm_srfi1_find (SCM pred, SCM lst);
SCM_SRFI1_API SCM scm_srfi1_find_tail (SCM pred, SCM lst);
SCM_SRFI1_API SCM scm_srfi1_fold (SCM proc, SCM init, SCM list1, SCM rest);
SCM_SRFI1_API SCM scm_srfi1_last (SCM lst);
SCM_SRFI1_API SCM scm_srfi1_length_plus (SCM lst);
SCM_SRFI1_API SCM scm_srfi1_lset_adjoin (SCM equal, SCM lst, SCM rest);
SCM_SRFI1_API SCM scm_srfi1_lset_difference_x (SCM equal, SCM lst, SCM rest);
SCM_SRFI1_API SCM scm_srfi1_list_copy (SCM lst);
SCM_SRFI1_API SCM scm_srfi1_list_index (SCM pred, SCM list1, SCM rest);
SCM_SRFI1_API SCM scm_srfi1_list_tabulate (SCM n, SCM proc);
SCM_SRFI1_API SCM scm_srfi1_map (SCM proc, SCM arg1, SCM args);
SCM_SRFI1_API SCM scm_srfi1_for_each (SCM proc, SCM arg1, SCM args);
SCM_SRFI1_API SCM scm_srfi1_member (SCM obj, SCM ls, SCM pred);
SCM_SRFI1_API SCM scm_srfi1_ninth (SCM lst);
SCM_SRFI1_API SCM scm_srfi1_assoc (SCM key, SCM alist, SCM pred);
SCM_SRFI1_API SCM scm_srfi1_not_pair_p (SCM obj);
SCM_SRFI1_API SCM scm_srfi1_partition (SCM pred, SCM list);
SCM_SRFI1_API SCM scm_srfi1_partition_x (SCM pred, SCM list);
SCM_SRFI1_API SCM scm_srfi1_reduce (SCM proc, SCM def, SCM lst);
SCM_SRFI1_API SCM scm_srfi1_reduce_right (SCM proc, SCM def, SCM lst);
SCM_SRFI1_API SCM scm_srfi1_remove (SCM pred, SCM list);
SCM_SRFI1_API SCM scm_srfi1_remove_x (SCM pred, SCM list);
SCM_SRFI1_API SCM scm_srfi1_seventh (SCM lst);
SCM_SRFI1_API SCM scm_srfi1_sixth (SCM lst);
SCM_SRFI1_API SCM scm_srfi1_span (SCM pred, SCM lst);
SCM_SRFI1_API SCM scm_srfi1_span_x (SCM pred, SCM lst);
SCM_SRFI1_API SCM scm_srfi1_split_at (SCM lst, SCM n);
SCM_SRFI1_API SCM scm_srfi1_split_at_x (SCM lst, SCM n);
SCM_SRFI1_API SCM scm_srfi1_take_x (SCM lst, SCM n);
SCM_SRFI1_API SCM scm_srfi1_take_right (SCM lst, SCM n);
SCM_SRFI1_API SCM scm_srfi1_take_while (SCM pred, SCM lst);
SCM_SRFI1_API SCM scm_srfi1_take_while_x (SCM pred, SCM lst);
SCM_SRFI1_API SCM scm_srfi1_tenth (SCM lst);
SCM_SRFI1_API SCM scm_srfi1_xcons (SCM d, SCM a);

SCM_SRFI1_API void scm_init_srfi_1 (void);

#endif /* SCM_SRFI_1_H */
