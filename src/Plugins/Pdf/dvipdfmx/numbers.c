/*  $Header: /home/cvsroot/dvipdfmx/src/numbers.c,v 1.9 2008/11/30 21:12:27 matthias Exp $

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

#include "system.h"	
#include "error.h"
#include "mfileio.h"
#include "numbers.h"

UNSIGNED_BYTE get_unsigned_byte (FILE *file)
{
  int ch;
  if ((ch = fgetc (file)) < 0)
    ERROR ("File ended prematurely\n");
  return (UNSIGNED_BYTE) ch;
}

#if 0
UNSIGNED_BYTE sget_unsigned_byte (char *s)
{
  return *((unsigned char *) s);
}
#endif

SIGNED_BYTE get_signed_byte (FILE *file)
{
  int byte;
  byte = get_unsigned_byte(file);
  if (byte >= 0x80) 
    byte -= 0x100;
  return (SIGNED_BYTE) byte;
}

UNSIGNED_PAIR get_unsigned_pair (FILE *file)
{
  int i;
  UNSIGNED_BYTE byte;
  UNSIGNED_PAIR pair = 0;
  for (i=0; i<2; i++) {
    byte = get_unsigned_byte(file);
    pair = pair*0x100u + byte;
  }
  return pair;
}

UNSIGNED_PAIR sget_unsigned_pair (unsigned char *s)
{
  int i;
  UNSIGNED_BYTE byte;
  UNSIGNED_PAIR pair = 0;
  for (i=0; i<2; i++) {
    byte = *(s++);
    pair = pair*0x100u + byte;
  }
  return pair;
}

SIGNED_PAIR get_signed_pair (FILE *file)
{
  int i;
  long pair = 0;
  for (i=0; i<2; i++) {
    pair = pair*0x100 + get_unsigned_byte(file);
  }
  if (pair >= 0x8000) {
    pair -= 0x10000l;
  }
  return (SIGNED_PAIR) pair;
}


UNSIGNED_TRIPLE get_unsigned_triple(FILE *file)
{
  int i;
  long triple = 0;
  for (i=0; i<3; i++) {
    triple = triple*0x100u + get_unsigned_byte(file);
  }
  return (UNSIGNED_TRIPLE) triple;
}

SIGNED_TRIPLE get_signed_triple(FILE *file)
{
  int i;
  long triple = 0;
  for (i=0; i<3; i++) {
    triple = triple*0x100 + get_unsigned_byte(file);
  }
  if (triple >= 0x800000l) 
    triple -= 0x1000000l;
  return (SIGNED_TRIPLE) triple;
}

SIGNED_QUAD get_signed_quad(FILE *file)
{
  int byte, i;
  long quad = 0;

  /* Check sign on first byte before reading others */
  byte = get_unsigned_byte(file);
  quad = byte;
  if (quad >= 0x80) 
    quad = byte - 0x100;
  for (i=0; i<3; i++) {
    quad = quad*0x100 + get_unsigned_byte(file);
  }
  return (SIGNED_QUAD) quad;
}

UNSIGNED_QUAD get_unsigned_quad(FILE *file)
{
  int i;
  unsigned long quad = 0;
  for (i=0; i<4; i++) {
    quad = quad*0x100u + get_unsigned_byte(file);
  }
  return (UNSIGNED_QUAD) quad;
}

SIGNED_QUAD sqxfw (SIGNED_QUAD sq, fixword fw)
{
  int sign = 1;
  unsigned long a, b, c, d, ad, bd, bc, ac;
  unsigned long e, f, g, h, i, j, k;
  unsigned long result;
  /* Make positive. */
  if (sq < 0) {
    sign = -sign;
    sq = -sq;
  }
  if (fw < 0) {
    sign = -sign;
    fw = -fw;
  }
  a = ((unsigned long) sq) >> 16u;
  b = ((unsigned long) sq) & 0xffffu;
  c = ((unsigned long) fw) >> 16u;
  d = ((unsigned long) fw) & 0xffffu;
  ad = a*d; bd = b*d; bc = b*c; ac = a*c;
  e = bd >> 16u;
  f = ad >> 16u;
  g = ad & 0xffffu;
  h = bc >> 16u;
  i = bc & 0xffffu;
  j = ac >> 16u;
  k = ac & 0xffffu;
  result = (e+g+i + (1<<3)) >> 4u;  /* 1<<3 is for rounding */
  result += (f+h+k) << 12u;
  result += j << 28u;
  return (sign > 0) ? result : result * -1L;
}

#if 0
SIGNED_QUAD axboverc (SIGNED_QUAD n1, SIGNED_QUAD n2, SIGNED_QUAD divide)
{
  int sign = 1;
  unsigned long a, b, c, d, ad, bd, bc, ac, e, f, g, h, i, j, o;
  unsigned long high, low;
  SIGNED_QUAD result = 0;
  /*  Make positive. */
  if (n1 < 0) {
    sign = -sign;
    n1 = -n1;
  }
  if (n2 < 0) {
    sign = -sign;
    n2 = -n2;
  }
  if (divide < 0) {
    sign = -sign;
    divide = -divide;
  }
  a = ((unsigned long) n1) >> 16u;
  b = ((unsigned long) n1) & 0xffffu;
  c = ((unsigned long) n2) >> 16u;
  d = ((unsigned long) n2) & 0xffffu;
  ad = a*d; bd = b*d; bc = b*c; ac = a*c;
  e = bd >> 16u; f = bd & 0xffffu;
  g = ad >> 16u; h = ad & 0xffffu;
  i = bc >> 16u; j = bc & 0xffffu;
  o = e+h+j;
  high = g+i+(o>>16u)+ac; o &= 0xffffu;
  low = (o << 16) + f;
  if (high >= divide)
    ERROR ("Overflow in axboc");
  {
    int k;
    for (k=0; k<32; k++) {
      high *= 2;
      result *= 2;
      if (low >= 0x80000000) {
	low -= 0x80000000;
	high += 1;
      }
      low *= 2;
      if (high > divide) {
	high -= divide;
	result += 1;
      }
    }
  }
  high *= 2;
  if (high >= divide)
    result += 1;
  return (sign>0)?result:-result;
}
#endif

