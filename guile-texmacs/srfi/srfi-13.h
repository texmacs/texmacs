#ifndef SCM_SRFI_13_H
#define SCM_SRFI_13_H

/* SRFI-13 procedures for Guile
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


/* All SRFI-13 procedures are in in the core now. */

#include <libguile.h>

/* SCM_SRFI1314_API is a macro prepended to all function and data definitions
   which should be exported or imported in the resulting dynamic link
   library in the Win32 port. */

#if defined (SCM_SRFI1314_IMPORT)
# define SCM_SRFI1314_API __declspec (dllimport) extern
#elif defined (SCM_SRFI1314_EXPORT) || defined (DLL_EXPORT)
# define SCM_SRFI1314_API __declspec (dllexport) extern
#else
# define SCM_SRFI1314_API extern
#endif

SCM_SRFI1314_API void scm_init_srfi_13 (void);
SCM_SRFI1314_API void scm_init_srfi_13_14 (void);

/* The following functions have new names in the core.
 */

#define scm_string_to_listS    scm_substring_to_list
#define scm_string_copyS       scm_substring_copy
#define scm_substring_sharedS  scm_substring_shared
#define scm_string_fill_xS     scm_substring_fill_x
#define scm_string_indexS      scm_string_index
#define scm_string_upcase_xS   scm_substring_upcase_x
#define scm_string_upcaseS     scm_substring_upcase
#define scm_string_downcase_xS scm_substring_downcase_x
#define scm_string_downcaseS   scm_substring_downcase

#endif /* SCM_SRFI_13_H */
