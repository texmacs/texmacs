/*  $Header: /home/cvsroot/dvipdfmx/src/dpxcrypt.h,v 1.1 2003/08/05 07:09:09 chofchof Exp $
 
    This is DVIPDFMx, an eXtended version of DVIPDFM by Mark A. Wicks.

    Copyright (C) 2003 by Jin-Hwan Cho and Shunsaku Hirata,
    the DVIPDFMx project team <dvipdfmx@project.ktug.or.kr>
    
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

#ifndef _DPXCRYPT_H_
#define _DPXCRYPT_H_

#include <stdio.h>

/* libgcrypt md5 */
typedef struct {
  unsigned long A,B,C,D; /* chaining variables */
  unsigned long nblocks;
  unsigned char buf[64];
  int count;
} MD5_CONTEXT;

void MD5_init (MD5_CONTEXT *ctx);
void MD5_write (MD5_CONTEXT *ctx, unsigned char *inbuf, unsigned long inlen);
void MD5_final (unsigned char *outbuf, MD5_CONTEXT *ctx);

/* libgcrypt arcfour */
typedef struct {
  int idx_i, idx_j;
  unsigned char sbox[256];
} ARC4_KEY;

void ARC4 (ARC4_KEY *ctx, unsigned long len, const unsigned char *inbuf, unsigned char *outbuf);
void ARC4_set_key (ARC4_KEY *ctx, unsigned int keylen, const unsigned char *key);

#endif /* _DPXCRYPT_H_ */
