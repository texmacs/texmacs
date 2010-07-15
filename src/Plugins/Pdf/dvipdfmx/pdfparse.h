/*  $Header: /home/cvsroot/dvipdfmx/src/pdfparse.h,v 1.15 2008/05/13 12:23:45 matthias Exp $

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

#ifndef _PDFPARSE_H_
#define _PDFPARSE_H_

#include "numbers.h"
#include "pdfobj.h"

/* Please remove this */
extern void dump (const char *start, const char *end);

extern void skip_line  (char **start, char *end);
extern void skip_white (char **start, char *end);

extern char *parse_number   (char **start, char *end);
extern char *parse_unsigned (char **start, char *end);

extern char *parse_ident     (char **start, char *end);
extern char *parse_val_ident (char **start, char *end);
extern char *parse_opt_ident (char **start, char *end);

extern pdf_obj *parse_pdf_name    (char **pp, char *endptr);
extern pdf_obj *parse_pdf_boolean (char **pp, char *endptr);
extern pdf_obj *parse_pdf_number  (char **pp, char *endptr);
extern pdf_obj *parse_pdf_null    (char **pp, char *endptr);
extern pdf_obj *parse_pdf_string  (char **pp, char *endptr);
extern pdf_obj *parse_pdf_dict    (char **pp, char *endptr, pdf_file *pf);
extern pdf_obj *parse_pdf_array   (char **pp, char *endptr, pdf_file *pf);
extern pdf_obj *parse_pdf_object  (char **pp, char *endptr, pdf_file *pf);

extern pdf_obj *parse_pdf_tainted_dict (char **pp, char *endptr);

#endif /* _PDFPARSE_H_ */
