/*  $Header: /home/cvsroot/dvipdfmx/src/numbers.h,v 1.10 2008/11/30 21:12:27 matthias Exp $

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

#ifndef _NUMBERS_H_
#define _NUMBERS_H_

#include <stdio.h>
#include <math.h>

typedef unsigned char Ubyte;
typedef int UNSIGNED_BYTE, SIGNED_BYTE, SIGNED_PAIR;
typedef unsigned UNSIGNED_PAIR;
typedef long UNSIGNED_TRIPLE, SIGNED_TRIPLE, SIGNED_QUAD;
typedef unsigned long UNSIGNED_QUAD;

extern UNSIGNED_BYTE get_unsigned_byte (FILE *);
#if 0
extern UNSIGNED_BYTE sget_unsigned_byte (char *);
#endif
extern SIGNED_BYTE get_signed_byte (FILE *);
extern UNSIGNED_PAIR get_unsigned_pair (FILE *);
extern UNSIGNED_PAIR sget_unsigned_pair (unsigned char *);
extern SIGNED_PAIR get_signed_pair (FILE *);
extern UNSIGNED_TRIPLE get_unsigned_triple (FILE *);
extern SIGNED_TRIPLE get_signed_triple (FILE *);
extern SIGNED_QUAD get_signed_quad (FILE *);
extern UNSIGNED_QUAD get_unsigned_quad (FILE *);

typedef signed long fixword;

extern SIGNED_QUAD sqxfw (SIGNED_QUAD sq, fixword fw);
#if 0
extern SIGNED_QUAD axboverc (SIGNED_QUAD n1, SIGNED_QUAD n2, SIGNED_QUAD divide);
#endif

#ifndef MAX
#  define MAX(a,b) ((a)>(b)?(a):(b))
#endif
#ifndef MIN
#  define MIN(a,b) ((a)<(b)?(a):(b))
#endif
#define ISODD(n) (((n)/2)*2!=(n))
#define ISEVEN(n) (((n)/2)*2==(n))

#ifndef M_PI
#  define M_PI (4.0*atan(1.0))
#endif

#define ROUND(n,acc) (floor(((double)n)/(acc)+0.5)*(acc)) 

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
#  define __C99__
#endif

#ifndef __C99__
#  ifndef round
#  define round(v) (floor((v) + 0.5))
#  endif
#  ifndef trunc
#  define trunc(v) ((v) > 0.0 ? floor((v)) : ceil((v)))
#  endif
#endif
#define round_at(v,acc) (round(((double)(v))/(acc))*(acc))

#endif /* _NUMBERS_H_ */
