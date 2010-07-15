/*  $Header: /home/cvsroot/dvipdfmx/src/pst_obj.h,v 1.4 2008/11/30 21:12:27 matthias Exp $

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

#ifndef _PST_OBJ_H_
#define _PST_OBJ_H_

#include "pst.h"

extern pst_obj *pst_parse_null   (unsigned char **inbuf, unsigned char *inbufend);
extern pst_obj *pst_parse_boolean(unsigned char **inbuf, unsigned char *inbufend);
extern pst_obj *pst_parse_name   (unsigned char **inbuf, unsigned char *inbufend);
extern pst_obj *pst_parse_number (unsigned char **inbuf, unsigned char *inbufend);
extern pst_obj *pst_parse_string (unsigned char **inbuf, unsigned char *inbufend);

#if 0
extern int   pst_name_is_valid (const char *name);
extern char *pst_name_encode   (const char *name);
#endif

#endif /* _PST_OBJ_H_ */
