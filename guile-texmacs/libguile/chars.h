/* classes: h_files */

#ifndef SCM_CHARS_H
#define SCM_CHARS_H

/* Copyright (C) 1995,1996,2000,2001,2004, 2006 Free Software Foundation, Inc.
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


/* Immediate Characters
 */
#define SCM_CHARP(x) (SCM_ITAG8(x) == scm_tc8_char)
#define SCM_CHAR(x) ((unsigned int)SCM_ITAG8_DATA(x))
#define SCM_MAKE_CHAR(x) SCM_MAKE_ITAG8((scm_t_bits) (unsigned char) (x), scm_tc8_char)



SCM_API char *const scm_charnames[];
SCM_API int scm_n_charnames;
SCM_API const char scm_charnums[];



SCM_API SCM scm_char_p (SCM x);
SCM_API SCM scm_char_eq_p (SCM x, SCM y);
SCM_API SCM scm_char_less_p (SCM x, SCM y);
SCM_API SCM scm_char_leq_p (SCM x, SCM y);
SCM_API SCM scm_char_gr_p (SCM x, SCM y);
SCM_API SCM scm_char_geq_p (SCM x, SCM y);
SCM_API SCM scm_char_ci_eq_p (SCM x, SCM y);
SCM_API SCM scm_char_ci_less_p (SCM x, SCM y);
SCM_API SCM scm_char_ci_leq_p (SCM x, SCM y);
SCM_API SCM scm_char_ci_gr_p (SCM x, SCM y);
SCM_API SCM scm_char_ci_geq_p (SCM x, SCM y);
SCM_API SCM scm_char_alphabetic_p (SCM chr);
SCM_API SCM scm_char_numeric_p (SCM chr);
SCM_API SCM scm_char_whitespace_p (SCM chr);
SCM_API SCM scm_char_upper_case_p (SCM chr);
SCM_API SCM scm_char_lower_case_p (SCM chr);
SCM_API SCM scm_char_is_both_p (SCM chr);
SCM_API SCM scm_char_to_integer (SCM chr);
SCM_API SCM scm_integer_to_char (SCM n);
SCM_API SCM scm_char_upcase (SCM chr);
SCM_API SCM scm_char_downcase (SCM chr);
SCM_API int scm_c_upcase (unsigned int c);
SCM_API int scm_c_downcase (unsigned int c);
SCM_API void scm_init_chars (void);

#endif  /* SCM_CHARS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
