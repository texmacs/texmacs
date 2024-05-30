#ifndef SCM_SRFI_14_H
#define SCM_SRFI_14_H

/* srfi-14.c --- SRFI-14 procedures for Guile
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

#define SCM_CHARSET_SIZE 256

/* We expect 8-bit bytes here.  Should be no problem in the year
   2001.  */
#ifndef SCM_BITS_PER_LONG
# define SCM_BITS_PER_LONG (sizeof (long) * 8)
#endif

#define SCM_CHARSET_GET(cs, idx) (((long *) SCM_SMOB_DATA (cs))\
			           [((unsigned char) (idx)) / SCM_BITS_PER_LONG] &\
			 	   (1L << (((unsigned char) (idx)) % SCM_BITS_PER_LONG)))

#define SCM_CHARSETP(x) (!SCM_IMP (x) && (SCM_TYP16 (x) == scm_tc16_charset))

/* Smob type code for character sets.  */
SCM_API int scm_tc16_charset;

SCM_API SCM scm_char_set_p (SCM obj);
SCM_API SCM scm_char_set_eq (SCM char_sets);
SCM_API SCM scm_char_set_leq (SCM char_sets);
SCM_API SCM scm_char_set_hash (SCM cs, SCM bound);
SCM_API SCM scm_char_set_cursor (SCM cs);
SCM_API SCM scm_char_set_ref (SCM cs, SCM cursor);
SCM_API SCM scm_char_set_cursor_next (SCM cs, SCM cursor);
SCM_API SCM scm_end_of_char_set_p (SCM cursor);
SCM_API SCM scm_char_set_fold (SCM kons, SCM knil, SCM cs);
SCM_API SCM scm_char_set_unfold (SCM p, SCM f, SCM g, SCM seed, SCM base_cs);
SCM_API SCM scm_char_set_unfold_x (SCM p, SCM f, SCM g, SCM seed, SCM base_cs);
SCM_API SCM scm_char_set_for_each (SCM proc, SCM cs);
SCM_API SCM scm_char_set_map (SCM proc, SCM cs);
SCM_API SCM scm_char_set_copy (SCM cs);
SCM_API SCM scm_char_set (SCM rest);
SCM_API SCM scm_list_to_char_set (SCM list, SCM base_cs);
SCM_API SCM scm_list_to_char_set_x (SCM list, SCM base_cs);
SCM_API SCM scm_string_to_char_set (SCM str, SCM base_cs);
SCM_API SCM scm_string_to_char_set_x (SCM str, SCM base_cs);
SCM_API SCM scm_char_set_filter (SCM pred, SCM cs, SCM base_cs);
SCM_API SCM scm_char_set_filter_x (SCM pred, SCM cs, SCM base_cs);
SCM_API SCM scm_ucs_range_to_char_set (SCM lower, SCM upper, SCM error, SCM base_cs);
SCM_API SCM scm_ucs_range_to_char_set_x (SCM lower, SCM upper, SCM error, SCM base_cs);
SCM_API SCM scm_to_char_set (SCM x);
SCM_API SCM scm_char_set_size (SCM cs);
SCM_API SCM scm_char_set_count (SCM pred, SCM cs);
SCM_API SCM scm_char_set_to_list (SCM cs);
SCM_API SCM scm_char_set_to_string (SCM cs);
SCM_API SCM scm_char_set_contains_p (SCM cs, SCM ch);
SCM_API SCM scm_char_set_every (SCM pred, SCM cs);
SCM_API SCM scm_char_set_any (SCM pred, SCM cs);
SCM_API SCM scm_char_set_adjoin (SCM cs, SCM rest);
SCM_API SCM scm_char_set_delete (SCM cs, SCM rest);
SCM_API SCM scm_char_set_adjoin_x (SCM cs, SCM rest);
SCM_API SCM scm_char_set_delete_x (SCM cs, SCM rest);
SCM_API SCM scm_char_set_complement (SCM cs);
SCM_API SCM scm_char_set_union (SCM rest);
SCM_API SCM scm_char_set_intersection (SCM rest);
SCM_API SCM scm_char_set_difference (SCM cs1, SCM rest);
SCM_API SCM scm_char_set_xor (SCM rest);
SCM_API SCM scm_char_set_diff_plus_intersection (SCM cs1, SCM rest);
SCM_API SCM scm_char_set_complement_x (SCM cs);
SCM_API SCM scm_char_set_union_x (SCM cs1, SCM rest);
SCM_API SCM scm_char_set_intersection_x (SCM cs1, SCM rest);
SCM_API SCM scm_char_set_difference_x (SCM cs1, SCM rest);
SCM_API SCM scm_char_set_xor_x (SCM cs1, SCM rest);
SCM_API SCM scm_char_set_diff_plus_intersection_x (SCM cs1, SCM cs2, SCM rest);

SCM_API SCM scm_char_set_lower_case;
SCM_API SCM scm_char_set_upper_case;
SCM_API SCM scm_char_set_title_case;
SCM_API SCM scm_char_set_letter;
SCM_API SCM scm_char_set_digit;
SCM_API SCM scm_char_set_letter_and_digit;
SCM_API SCM scm_char_set_graphic;
SCM_API SCM scm_char_set_printing;
SCM_API SCM scm_char_set_whitespace;
SCM_API SCM scm_char_set_iso_control;
SCM_API SCM scm_char_set_punctuation;
SCM_API SCM scm_char_set_symbol;
SCM_API SCM scm_char_set_hex_digit;
SCM_API SCM scm_char_set_blank;
SCM_API SCM scm_char_set_ascii;
SCM_API SCM scm_char_set_empty;
SCM_API SCM scm_char_set_full;

SCM_API void scm_srfi_14_compute_char_sets (void);
SCM_API void scm_init_srfi_14 (void);

#endif /* SCM_SRFI_14_H */
