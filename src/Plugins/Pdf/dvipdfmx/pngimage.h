/*  $Header: /home/cvsroot/dvipdfmx/src/pngimage.h,v 1.5 2009/03/24 02:07:48 matthias Exp $

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

	
#ifndef _PNGIMAGE_H_
#define _PNGIMAGE_H_

#if HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_LIBPNG

#include "mfileio.h"
#include "pdfximage.h"

extern int png_include_image (pdf_ximage *ximage, FILE *file);
extern int check_for_png     (FILE *file);
extern int png_get_bbox (FILE *fp, long *width, long *height,
			 double *xdensity, double *ydensity);

#endif

#endif /* _PNGIMAGE_H_ */
