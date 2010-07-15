/*  $Header: /home/cvsroot/dvipdfmx/src/unicode.h,v 1.10 2004/09/11 14:50:29 hirata Exp $

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

#ifndef _UNICODE_H_
#define _UNICODE_H_

#include "pdfobj.h"
#include "agl.h"

extern void UC_set_verbose (void);

extern int UC_sput_UTF16BE  (long ucv, unsigned char **dstpp, unsigned char *endptr);
extern int UC_is_valid      (long ucv);

#endif /* _UNICODE_H_ */
