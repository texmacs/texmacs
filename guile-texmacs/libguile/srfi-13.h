#ifndef SCM_SRFI_13_H
#define SCM_SRFI_13_H

/* srfi-13.c --- SRFI-13 procedures for Guile
 *
 * 	Copyright (C) 2001, 2004, 2006 Free Software Foundation, Inc.
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

SCM_API SCM scm_string_null_p (SCM s);
SCM_API SCM scm_string_any (SCM pred, SCM s, SCM start, SCM end);
SCM_API SCM scm_string_every (SCM pred, SCM s, SCM start, SCM end);
SCM_API SCM scm_string_tabulate (SCM proc, SCM len);
SCM_API SCM scm_string_to_list (SCM str);
SCM_API SCM scm_substring_to_list (SCM str, SCM start, SCM end);
SCM_API SCM scm_reverse_list_to_string (SCM chrs);
SCM_API SCM scm_string_join (SCM ls, SCM delimiter, SCM grammar);
SCM_API SCM scm_string_copy (SCM str);
SCM_API SCM scm_string_copy_x (SCM target, SCM tstart, SCM s, SCM start, SCM end);
SCM_API SCM scm_substring_move_x (SCM str1, SCM start1, SCM end1, 
				  SCM str2, SCM start2);
SCM_API SCM scm_string_take (SCM s, SCM n);
SCM_API SCM scm_string_drop (SCM s, SCM n);
SCM_API SCM scm_string_take_right (SCM s, SCM n);
SCM_API SCM scm_string_drop_right (SCM s, SCM n);
SCM_API SCM scm_string_pad (SCM s, SCM len, SCM chr, SCM start, SCM end);
SCM_API SCM scm_string_pad_right (SCM s, SCM len, SCM chr, SCM start, SCM end);
SCM_API SCM scm_string_trim (SCM s, SCM char_pred, SCM start, SCM end);
SCM_API SCM scm_string_trim_right (SCM s, SCM char_pred, SCM start, SCM end);
SCM_API SCM scm_string_trim_both (SCM s, SCM char_pred, SCM start, SCM end);
SCM_API SCM scm_substring_fill_x (SCM str, SCM chr, SCM start, SCM end);
SCM_API SCM scm_string_fill_x (SCM str, SCM chr);
SCM_API SCM scm_string_compare (SCM s1, SCM s2, SCM proc_lt, SCM proc_eq, SCM proc_gt, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_compare_ci (SCM s1, SCM s2, SCM proc_lt, SCM proc_eq, SCM proc_gt, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_eq (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_neq (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_lt (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_gt (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_le (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_ge (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_ci_eq (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_ci_neq (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_ci_lt (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_ci_gt (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_ci_le (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_ci_ge (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_substring_hash (SCM s, SCM bound, SCM start, SCM end);
SCM_API SCM scm_substring_hash_ci (SCM s, SCM bound, SCM start, SCM end);
SCM_API SCM scm_string_prefix_length (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_prefix_length_ci (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_suffix_length (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_suffix_length_ci (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_prefix_p (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_prefix_ci_p (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_suffix_p (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_suffix_ci_p (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_index (SCM s, SCM char_pred, SCM start, SCM end);
SCM_API SCM scm_string_index_right (SCM s, SCM char_pred, SCM start, SCM end);
SCM_API SCM scm_string_rindex (SCM s, SCM char_pred, SCM start, SCM end);
SCM_API SCM scm_string_skip (SCM s, SCM char_pred, SCM start, SCM end);
SCM_API SCM scm_string_skip_right (SCM s, SCM char_pred, SCM start, SCM end);
SCM_API SCM scm_string_count (SCM s, SCM char_pred, SCM start, SCM end);
SCM_API SCM scm_string_contains (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_contains_ci (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_substring_upcase_x (SCM str, SCM start, SCM end);
SCM_API SCM scm_substring_upcase (SCM str, SCM start, SCM end);
SCM_API SCM scm_string_upcase_x (SCM str);
SCM_API SCM scm_string_upcase (SCM str);
SCM_API SCM scm_substring_downcase_x (SCM str, SCM start, SCM end);
SCM_API SCM scm_substring_downcase (SCM str, SCM start, SCM end);
SCM_API SCM scm_string_downcase_x (SCM str);
SCM_API SCM scm_string_downcase (SCM str);
SCM_API SCM scm_string_titlecase_x (SCM str, SCM start, SCM end);
SCM_API SCM scm_string_titlecase (SCM str, SCM start, SCM end);
SCM_API SCM scm_string_capitalize_x (SCM str);
SCM_API SCM scm_string_capitalize (SCM str);
SCM_API SCM scm_string_reverse (SCM str, SCM start, SCM end);
SCM_API SCM scm_string_reverse_x (SCM str, SCM start, SCM end);
SCM_API SCM scm_string_append_shared (SCM ls);
SCM_API SCM scm_string_concatenate (SCM ls);
SCM_API SCM scm_string_concatenate_shared (SCM ls);
SCM_API SCM scm_string_concatenate_reverse (SCM ls, SCM final_string, SCM end);
SCM_API SCM scm_string_concatenate_reverse_shared (SCM ls, SCM final_string, SCM end);
SCM_API SCM scm_string_map (SCM proc, SCM s, SCM start, SCM end);
SCM_API SCM scm_string_map_x (SCM proc, SCM s, SCM start, SCM end);
SCM_API SCM scm_string_fold (SCM kons, SCM knil, SCM s, SCM start, SCM end);
SCM_API SCM scm_string_fold_right (SCM kons, SCM knil, SCM s, SCM start, SCM end);
SCM_API SCM scm_string_unfold (SCM p, SCM f, SCM g, SCM seed, SCM base, SCM make_final);
SCM_API SCM scm_string_unfold_right (SCM p, SCM f, SCM g, SCM seed, SCM base, SCM make_final);
SCM_API SCM scm_string_for_each (SCM proc, SCM s, SCM start, SCM end);
SCM_API SCM scm_string_for_each_index (SCM proc, SCM s, SCM start, SCM end);
SCM_API SCM scm_xsubstring (SCM s, SCM from, SCM to, SCM start, SCM end);
SCM_API SCM scm_string_xcopy_x (SCM target, SCM tstart, SCM s, SCM sfrom, SCM sto, SCM start, SCM end);
SCM_API SCM scm_string_replace (SCM s1, SCM s2, SCM start1, SCM end1, SCM start2, SCM end2);
SCM_API SCM scm_string_tokenize (SCM s, SCM token_char, SCM start, SCM end);
SCM_API SCM scm_string_split (SCM s, SCM chr);
SCM_API SCM scm_string_filter (SCM s, SCM char_pred, SCM start, SCM end);
SCM_API SCM scm_string_delete (SCM s, SCM char_pred, SCM start, SCM end);

SCM_API void scm_init_srfi_13 (void);
SCM_API void scm_init_srfi_13_14 (void);

#endif /* SCM_SRFI_13_H */
