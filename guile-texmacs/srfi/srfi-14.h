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

SCM_SRFI1314_API void scm_c_init_srfi_14 (void);
SCM_SRFI1314_API void scm_init_srfi_14 (void);

#endif /* SCM_SRFI_14_H */
