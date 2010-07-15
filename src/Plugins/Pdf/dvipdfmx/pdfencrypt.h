/*  $Header: /home/cvsroot/dvipdfmx/src/pdfencrypt.h,v 1.4 2010/02/07 12:53:44 chofchof Exp $

    This is dvipdfmx, an eXtended version of dvipdfm by Mark A. Wicks.

    Copyright (C) 2007 by Jin-Hwan Cho and Shunsaku Hirata,
    the dvipdfmx project team <dvipdfmx@project.ktug.or.kr>
    
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

#ifndef _PDFENCRYPT_H_
#define _PDFENCRYPT_H_

#include "pdfobj.h"

extern void pdf_enc_set_verbose (void);
extern pdf_obj *pdf_enc_id_array (void);
extern void pdf_enc_compute_id_string (char *dviname, char *pdfname);
extern void pdf_enc_set_label (unsigned long label);
extern void pdf_enc_set_generation (unsigned generation);
extern void pdf_enc_set_passwd (unsigned size, unsigned perm, char *dviname, char *pdfname);
extern void pdf_encrypt_data (unsigned char *data, unsigned long len);
extern pdf_obj *pdf_encrypt_obj (void);

#endif /* _PDFENCRYPT_H_ */
