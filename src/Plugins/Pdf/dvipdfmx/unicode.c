/*  $Header: /home/cvsroot/dvipdfmx/src/unicode.c,v 1.21 2004/09/11 14:50:29 hirata Exp $

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

/*
 * Unicode related:
 *  Conversion between UTF-* and UCS-*.
 *  ToUnicode CMap
 *
 * Normalization?
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <string.h>

#include "system.h"
#include "mem.h"
#include "error.h"
#include "dpxutil.h"

#include "pdfobj.h"
#include "pdfresource.h"

#include "pdfencoding.h"
#include "agl.h"
#include "unicode.h"

static int __verbose = 0;

#define UC_DEBUG     3
#define UC_DEBUG_STR "UC"

void
UC_set_verbose (void)
{
  __verbose++;
}

#define UC_REPLACEMENT_CHAR 0x0000FFFDL

#define UC_SUR_SHIFT      10
#define UC_SUR_MASK       0x3FFUL
#define UC_SUR_LOW_START  0xDC00UL
#define UC_SUR_HIGH_START 0xD800UL

int
UC_sput_UTF16BE (long ucv, unsigned char **pp, unsigned char *limptr)
{
  int count = 0;
  unsigned char *p = *pp;

  if (ucv >= 0 && ucv <= 0xFFFF) {
    if (p + 2 >= limptr)
      ERROR("Not enough space available...");
    p[0] = (ucv >> 8) & 0xff;
    p[1] = ucv & 0xff;
    count = 2;
  } else if (ucv >= 0x010000L && ucv <= 0x10FFFFL) {
    unsigned short high, low;

    if (p + 4 >= limptr)
      ERROR("Not enough space available...");
    ucv  -= 0x00010000L;
    high = (ucv >> UC_SUR_SHIFT) + UC_SUR_HIGH_START;
    low  = (ucv &  UC_SUR_MASK)  + UC_SUR_LOW_START;
    p[0] = (high >> 8) & 0xff;
    p[1] = (high & 0xff);
    p[2] = (low >> 8) & 0xff;
    p[3] = (low & 0xff);
    count = 4;
  } else {
    if (p + 2 >= limptr)
      ERROR("Not enough space available...");
    p[0] = (UC_REPLACEMENT_CHAR >> 8) & 0xff;
    p[1] = (UC_REPLACEMENT_CHAR & 0xff);
    count = 2;
  }

  *pp += count;
  return count;
}

int
UC_is_valid (long ucv)
{
  if (ucv < 0 || (ucv >= 0x0000D800L && ucv <= 0x0000DFFFL))
    return 0;
  return 1;
}

