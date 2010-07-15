/*  $Header: /home/cvsroot/dvipdfmx/src/dpxfile.h,v 1.10 2010/05/29 20:56:42 matthias Exp $
    
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

#ifndef _DPXFILE_H_
#define _DPXFILE_H_

#define DPX_CONFIG_FILE "dvipdfmx.cfg"

#define DPX_RES_TYPE_FONTMAP  0x00

#define DPX_RES_TYPE_T1FONT   0x10
#define DPX_RES_TYPE_TTFONT   0x11
#define DPX_RES_TYPE_OTFONT   0x12
#define DPX_RES_TYPE_PKFONT   0x13
#define DPX_RES_TYPE_DFONT    0x14

#define DPX_RES_TYPE_ENC      0x20
#define DPX_RES_TYPE_CMAP     0x21
#define DPX_RES_TYPE_SFD      0x22
#define DPX_RES_TYPE_AGL      0x23

#define DPX_RES_TYPE_ICCPROFILE 0x30

#define DPX_RES_TYPE_BINARY   0x40
#define DPX_RES_TYPE_TEXT     0x41

#include "mfileio.h"
extern FILE *dpx_open_file (const char *filename, int type);

#define DPXFOPEN(n,t)  dpx_open_file((const char *)(n),(t))
#define DPXFCLOSE(f)   MFCLOSE((f))

extern void  dpx_file_set_verbose  (void);

extern int   dpx_file_apply_filter (const char *cmdtmpl,
                                   const char *input, const char *output,
                                   unsigned short version);
extern char *dpx_create_temp_file  (void);
extern void  dpx_delete_temp_file  (char *tmp); /* tmp freed here */

#endif /* _DPXFILE_H_ */
