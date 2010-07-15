/*  $Header: /home/cvsroot/dvipdfmx/src/mem.h,v 1.6 2009/11/29 01:18:20 matthias Exp $

    This is dvipdfmx, an eXtended version of dvipdfm by Mark A. Wicks.

    Copyright (C) 2002 by Jin-Hwan Cho and Shunsaku Hirata,
    the dvipdfmx project team <dvipdfmx@project.ktug.or.kr>
    
    Copyright (C) 1998, 1999 by Mark A. Wicks <mwicks@kettering.edu>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
*/

#ifndef _MEM_H_
#define _MEM_H_

#include <stdlib.h>

extern void *new (size_t size);
extern void *renew (void *p, size_t size);

extern void mem_debug_init(void);
extern void mem_debug_check(void);

#ifdef MEM_DEBUG

extern void *mem_add    (void *ptr,
			 const char *file, const char *function, int line);
extern void *mem_remove (void *ptr,
			 const char *file, const char *function, int line);
#define MEM_ADD(p)     mem_add(p, __FILE__, __FUNCTION__, __LINE__)
#define MEM_REMOVE(p)  mem_remove(p, __FILE__, __FUNCTION__, __LINE__)

#else /* ! MEM_DEBUG */

extern void *mem_add    (void *ptr);
extern void *mem_remove (void *ptr);
#define MEM_ADD(p)     mem_add(p)
#define MEM_REMOVE(p)  mem_remove(p)

#endif /* MEM_DEBUG */


#define NEW(n,type)     (type *) MEM_ADD(new(((size_t)(n))*sizeof(type)))
#define RENEW(p,n,type) (type *) MEM_ADD(renew(MEM_REMOVE(p),(n)*sizeof(type)))
#define RELEASE(p)      free(MEM_REMOVE(p))

/* wrappers for functions from kpathsea */
#define kpse_path_search(x,y,z)   (char *) MEM_ADD(kpse_path_search(x,y,z))
#define kpse_find_file(x,y,z)     (char *) MEM_ADD(kpse_find_file(x,y,z))
#define kpse_find_glyph(x,y,z,w)  (char *) MEM_ADD(kpse_find_glyph(x,y,z,w))

#endif /* _MEM_H_ */
