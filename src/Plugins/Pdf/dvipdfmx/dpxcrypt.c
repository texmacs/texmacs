/*  $Header: /home/cvsroot/dvipdfmx/src/dpxcrypt.c,v 1.2 2004/07/27 12:08:46 hirata Exp $
 
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

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <string.h>

#include "dpxcrypt.h"

static void _gcry_burn_stack (int bytes)
{
  char buf[64];
    
  memset(buf, 0, sizeof buf);
  bytes -= sizeof buf;
  if (bytes > 0) _gcry_burn_stack(bytes);
}

/* Rotate a 32 bit integer by n bytes */
#define rol(x,n) ( ((x) << (n)) | ((x) >> (32-(n))) )

/*
 * The following codes for MD5 Message-Digest Algorithm were modified
 * by Jin-Hwan Cho on August 5, 2003 based on libgrypt-1.1.42.
 *
 * Copyright (C) 1995,1996,1998,1999,2001,2002,2003 Free Software Foundation, Inc.
 *
 * Libgcrypt is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * Libgcrypt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
 *
 * According to the definition of MD5 in RFC 1321 from April 1992.
 * NOTE: This is *not* the same file as the one from glibc.
 * Written by Ulrich Drepper <drepper@gnu.ai.mit.edu>, 1995. 
 * heavily modified for GnuPG by Werner Koch <wk@gnupg.org> 
 */

void MD5_init (MD5_CONTEXT *ctx)
{
  ctx->A = 0x67452301;
  ctx->B = 0xefcdab89;
  ctx->C = 0x98badcfe;
  ctx->D = 0x10325476;

  ctx->nblocks = 0;
  ctx->count = 0;
}

/* These are the four functions used in the four steps of the MD5 algorithm
 * and defined in the RFC 1321. The first function is a little bit optimized
 * (as found in Colin Plumbs public domain implementation). */
/* #define FF(b, c, d) ((b & c) | (~b & d)) */
#define FF(b, c, d) (d ^ (b & (c ^ d)))
#define FG(b, c, d) FF(d, b, c)
#define FH(b, c, d) (b ^ c ^ d)
#define FI(b, c, d) (c ^ (b | ~d))

/* transform n*64 bytes */
static void transform (MD5_CONTEXT *ctx, unsigned char *data)
{
  unsigned long correct_words[16];
  register unsigned long A = ctx->A;
  register unsigned long B = ctx->B;
  register unsigned long C = ctx->C;
  register unsigned long D = ctx->D;
  unsigned long *cwp = correct_words;

#ifdef WORDS_BIGENDIAN
  { int i; unsigned char *p1, *p2;
    for (i = 0, p1 = data, p2 = (unsigned char *)correct_words; i < 16; i++, p2 += 4 ) {
      p2[3] = *p1++; p2[2] = *p1++; p2[1] = *p1++; p2[0] = *p1++;
    }
  }
#else
  memcpy(correct_words, data, 64);
#endif

#define OP(a, b, c, d, s, T) \
  do { a += FF(b, c, d) + (*cwp++) + T; a = rol(a, s); a += b; } while (0)

  /* Before we start, one word about the strange constants.
   * They are defined in RFC 1321 as
   *
   *   T[i] = (int) (4294967296.0 * fabs (sin (i))), i=1..64
   */

  /* Round 1. */
  OP(A, B, C, D,  7, 0xd76aa478);
  OP(D, A, B, C, 12, 0xe8c7b756);
  OP(C, D, A, B, 17, 0x242070db);
  OP(B, C, D, A, 22, 0xc1bdceee);
  OP(A, B, C, D,  7, 0xf57c0faf);
  OP(D, A, B, C, 12, 0x4787c62a);
  OP(C, D, A, B, 17, 0xa8304613);
  OP(B, C, D, A, 22, 0xfd469501);
  OP(A, B, C, D,  7, 0x698098d8);
  OP(D, A, B, C, 12, 0x8b44f7af);
  OP(C, D, A, B, 17, 0xffff5bb1);
  OP(B, C, D, A, 22, 0x895cd7be);
  OP(A, B, C, D,  7, 0x6b901122);
  OP(D, A, B, C, 12, 0xfd987193);
  OP(C, D, A, B, 17, 0xa679438e);
  OP(B, C, D, A, 22, 0x49b40821);

#undef OP
#define OP(f, a, b, c, d, k, s, T) \
  do { a += f(b, c, d) + correct_words[k] + T; a = rol(a, s); a += b; } while (0)

  /* Round 2. */
  OP(FG, A, B, C, D,  1,  5, 0xf61e2562);
  OP(FG, D, A, B, C,  6,  9, 0xc040b340);
  OP(FG, C, D, A, B, 11, 14, 0x265e5a51);
  OP(FG, B, C, D, A,  0, 20, 0xe9b6c7aa);
  OP(FG, A, B, C, D,  5,  5, 0xd62f105d);
  OP(FG, D, A, B, C, 10,  9, 0x02441453);
  OP(FG, C, D, A, B, 15, 14, 0xd8a1e681);
  OP(FG, B, C, D, A,  4, 20, 0xe7d3fbc8);
  OP(FG, A, B, C, D,  9,  5, 0x21e1cde6);
  OP(FG, D, A, B, C, 14,  9, 0xc33707d6);
  OP(FG, C, D, A, B,  3, 14, 0xf4d50d87);
  OP(FG, B, C, D, A,  8, 20, 0x455a14ed);
  OP(FG, A, B, C, D, 13,  5, 0xa9e3e905);
  OP(FG, D, A, B, C,  2,  9, 0xfcefa3f8);
  OP(FG, C, D, A, B,  7, 14, 0x676f02d9);
  OP(FG, B, C, D, A, 12, 20, 0x8d2a4c8a);

  /* Round 3. */
  OP(FH, A, B, C, D,  5,  4, 0xfffa3942);
  OP(FH, D, A, B, C,  8, 11, 0x8771f681);
  OP(FH, C, D, A, B, 11, 16, 0x6d9d6122);
  OP(FH, B, C, D, A, 14, 23, 0xfde5380c);
  OP(FH, A, B, C, D,  1,  4, 0xa4beea44);
  OP(FH, D, A, B, C,  4, 11, 0x4bdecfa9);
  OP(FH, C, D, A, B,  7, 16, 0xf6bb4b60);
  OP(FH, B, C, D, A, 10, 23, 0xbebfbc70);
  OP(FH, A, B, C, D, 13,  4, 0x289b7ec6);
  OP(FH, D, A, B, C,  0, 11, 0xeaa127fa);
  OP(FH, C, D, A, B,  3, 16, 0xd4ef3085);
  OP(FH, B, C, D, A,  6, 23, 0x04881d05);
  OP(FH, A, B, C, D,  9,  4, 0xd9d4d039);
  OP(FH, D, A, B, C, 12, 11, 0xe6db99e5);
  OP(FH, C, D, A, B, 15, 16, 0x1fa27cf8);
  OP(FH, B, C, D, A,  2, 23, 0xc4ac5665);

  /* Round 4.  */
  OP(FI, A, B, C, D,  0,  6, 0xf4292244);
  OP(FI, D, A, B, C,  7, 10, 0x432aff97);
  OP(FI, C, D, A, B, 14, 15, 0xab9423a7);
  OP(FI, B, C, D, A,  5, 21, 0xfc93a039);
  OP(FI, A, B, C, D, 12,  6, 0x655b59c3);
  OP(FI, D, A, B, C,  3, 10, 0x8f0ccc92);
  OP(FI, C, D, A, B, 10, 15, 0xffeff47d);
  OP(FI, B, C, D, A,  1, 21, 0x85845dd1);
  OP(FI, A, B, C, D,  8,  6, 0x6fa87e4f);
  OP(FI, D, A, B, C, 15, 10, 0xfe2ce6e0);
  OP(FI, C, D, A, B,  6, 15, 0xa3014314);
  OP(FI, B, C, D, A, 13, 21, 0x4e0811a1);
  OP(FI, A, B, C, D,  4,  6, 0xf7537e82);
  OP(FI, D, A, B, C, 11, 10, 0xbd3af235);
  OP(FI, C, D, A, B,  2, 15, 0x2ad7d2bb);
  OP(FI, B, C, D, A,  9, 21, 0xeb86d391);

  /* Put checksum in context given as argument. */
  ctx->A += A;
  ctx->B += B;
  ctx->C += C;
  ctx->D += D;
}

/* The routine updates the message-digest context to
 * account for the presence of each of the characters inBuf[0..inLen-1]
 * in the message whose digest is being computed. */
void MD5_write (MD5_CONTEXT *hd, unsigned char *inbuf, unsigned long inlen)
{
  if (hd->count == 64) { /* flush the buffer */
    transform(hd, hd->buf);
    _gcry_burn_stack(80+6*sizeof(void*));
    hd->count = 0;
    hd->nblocks++;
  }
  if (!inbuf) return;
  if (hd->count) {
    for (; inlen && hd->count < 64; inlen--)
      hd->buf[hd->count++] = *inbuf++;
    MD5_write(hd, NULL, 0);
    if (!inlen) return;
  }
  _gcry_burn_stack(80+6*sizeof(void*));

  while (inlen >= 64) {
    transform(hd, inbuf);
    hd->count = 0;
    hd->nblocks++;
    inlen -= 64;
    inbuf += 64;
  }
  for (; inlen && hd->count < 64; inlen--)
    hd->buf[hd->count++] = *inbuf++;
}

/* The routine final terminates the message-digest computation and
 * ends with the desired message digest in mdContext->digest[0...15].
 * The handle is prepared for a new MD5 cycle.
 * Returns 16 bytes representing the digest. */

void MD5_final (unsigned char *outbuf, MD5_CONTEXT *hd)
{
  unsigned long t, msb, lsb;
  unsigned char *p;

  MD5_write(hd, NULL, 0); /* flush */

  t = hd->nblocks;
  /* multiply by 64 to make a byte count */
  lsb = t << 6;
  msb = t >> 26;
  /* add the count */
  t = lsb;
  if ((lsb += hd->count) < t) msb++;
  /* multiply by 8 to make a bit count */
  t = lsb;
  lsb <<= 3;
  msb <<= 3;
  msb |= t >> 29;

  if (hd->count < 56) { /* enough room */
    hd->buf[hd->count++] = 0x80; /* pad */
    while (hd->count < 56) hd->buf[hd->count++] = 0; /* pad */
  } else { /* need one extra block */
    hd->buf[hd->count++] = 0x80; /* pad character */
    while (hd->count < 64) hd->buf[hd->count++] = 0;
    MD5_write(hd, NULL, 0); /* flush */
    memset(hd->buf, 0, 56); /* fill next block with zeroes */
  }
  /* append the 64 bit count */
  hd->buf[56] = lsb;
  hd->buf[57] = lsb >> 8;
  hd->buf[58] = lsb >> 16;
  hd->buf[59] = lsb >> 24;
  hd->buf[60] = msb;
  hd->buf[61] = msb >> 8;
  hd->buf[62] = msb >> 16;
  hd->buf[63] = msb >> 24;
  transform(hd, hd->buf);
  _gcry_burn_stack(80+6*sizeof(void*));

  p = outbuf; /* p = hd->buf; */
#ifdef WORDS_BIGENDIAN
#define X(a) do { *p++ = hd->a; *p++ = hd->a >> 8; \
	          *p++ = hd->a >> 16; *p++ = hd->a >> 24; } while (0)
#else /* little endian */
#define X(a) do { *(unsigned long *)p = (*hd).a ; p += 4; } while (0)
#endif
  X(A);
  X(B);
  X(C);
  X(D);
#undef X
}

/*
 * The following codes for the arcfour stream cipher were modified
 * by Jin-Hwan Cho on August 5, 2003 based on libgrypt-1.1.42.
 *
 * Copyright (C) 2000,2001,2002,2003 Free Software Foundation, Inc.
 *
 * Libgcrypt is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * Libgcrypt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
 *
 * According to the definition of MD5 in RFC 1321 from April 1992.
 * NOTE: This is *not* the same file as the one from glibc.
 * Written by Ulrich Drepper <drepper@gnu.ai.mit.edu>, 1995. 
 * heavily modified for GnuPG by Werner Koch <wk@gnupg.org> 
 */

static void do_encrypt_stream (ARC4_KEY *ctx, unsigned char *outbuf, const unsigned char *inbuf, unsigned long len)
{
  register int i = ctx->idx_i;
  register int j = ctx->idx_j;
  register unsigned char *sbox = ctx->sbox;
  register int t;  

  while (len--) {
    i++;
    i = i & 255; /* and seems to be faster than mod */
    j += sbox[i];
    j &= 255;
    t = sbox[i]; sbox[i] = sbox[j]; sbox[j] = t;
    *outbuf++ = *inbuf++ ^ sbox[(sbox[i] + sbox[j]) & 255];
  }
  
  ctx->idx_i = i;
  ctx->idx_j = j;
}

void ARC4 (ARC4_KEY *ctx, unsigned long len, const unsigned char *inbuf, unsigned char *outbuf)
{
  do_encrypt_stream(ctx, outbuf, inbuf, len);
  _gcry_burn_stack(64);
}

static void do_arcfour_setkey (ARC4_KEY *ctx, const unsigned char *key, unsigned int keylen)
{
  int i, j;
  unsigned char karr[256];

  ctx->idx_i = ctx->idx_j = 0;
  for (i = 0; i < 256; i++) ctx->sbox[i] = i;
  for (i = 0; i < 256; i++) karr[i] = key[i%keylen];
  for (i = j = 0; i < 256; i++) {
    int t;
    j = (j + ctx->sbox[i] + karr[i]) % 256;
    t = ctx->sbox[i];
    ctx->sbox[i] = ctx->sbox[j];
    ctx->sbox[j] = t;
  } 
  memset(karr, 0, 256);
}

void ARC4_set_key (ARC4_KEY *ctx, unsigned int keylen, const unsigned char *key)
{
  do_arcfour_setkey(ctx, key, keylen);
  _gcry_burn_stack(300);
}
