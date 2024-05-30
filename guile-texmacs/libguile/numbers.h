/* classes: h_files */

#ifndef SCM_NUMBERS_H
#define SCM_NUMBERS_H

/* Copyright (C) 1995,1996,1998,2000,2001,2002,2003,2004,2005, 2006, 2010 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */



#include <gmp.h>

#include "libguile/__scm.h"
#include "libguile/print.h"

#if SCM_HAVE_FLOATINGPOINT_H
# include <floatingpoint.h>
#endif

#if SCM_HAVE_IEEEFP_H
# include <ieeefp.h>
#endif

#if SCM_HAVE_NAN_H
# if defined (SCO)
#   define _IEEE 1
# endif
# include <nan.h>
# if defined (SCO)
#   undef _IEEE
# endif
#endif /* SCM_HAVE_NAN_H */



/* Immediate Numbers, also known as fixnums
 *
 * Inums are exact integer data that fits within an SCM word.  */

/* SCM_T_SIGNED_MAX is                 (- (expt 2 n) 1),
 * SCM_MOST_POSITIVE_FIXNUM should be  (- (expt 2 (- n 2)) 1)
 * which is the same as                (/ (- (expt 2 n) 4) 4)
 */

#define SCM_I_FIXNUM_BIT         (SCM_LONG_BIT - 2)
#define SCM_MOST_POSITIVE_FIXNUM ((SCM_T_SIGNED_BITS_MAX-3)/4)
#define SCM_MOST_NEGATIVE_FIXNUM (-SCM_MOST_POSITIVE_FIXNUM-1)

/* SCM_SRS is signed right shift */
#if (-1 == (((-1) << 2) + 2) >> 2)
# define SCM_SRS(x, y) ((x) >> (y))
#else
# define SCM_SRS(x, y) ((x) < 0 ? ~((~(x)) >> (y)) : ((x) >> (y)))
#endif /* (-1 == (((-1) << 2) + 2) >> 2) */


#define SCM_I_INUMP(x)	(2 & SCM_UNPACK (x))
#define SCM_I_NINUMP(x) (!SCM_I_INUMP (x))
#define SCM_I_MAKINUM(x) \
  (SCM_PACK ((((scm_t_signed_bits) (x)) << 2) + scm_tc2_int))
#define SCM_I_INUM(x)   (SCM_SRS ((scm_t_signed_bits) SCM_UNPACK (x), 2))

/* SCM_FIXABLE is true if its long argument can be encoded in an SCM_INUM. */
#define SCM_POSFIXABLE(n) ((n) <= SCM_MOST_POSITIVE_FIXNUM)
#define SCM_NEGFIXABLE(n) ((n) >= SCM_MOST_NEGATIVE_FIXNUM)
#define SCM_FIXABLE(n) (SCM_POSFIXABLE (n) && SCM_NEGFIXABLE (n))


/* A name for 0. */
#define SCM_INUM0 (SCM_I_MAKINUM (0))

/* SCM_MAXEXP is the maximum double precision exponent
 * SCM_FLTMAX is less than or scm_equal the largest single precision float
 */

#if SCM_HAVE_STDC_HEADERS
# ifndef GO32
#  include <float.h>
#  ifdef __MINGW32__
#   define copysign _copysign
#   define finite _finite
#  endif /* __MINGW32__ */
# endif /* ndef GO32 */
#endif /* def STDC_HEADERS */

#ifdef DBL_MAX_10_EXP
# define SCM_MAXEXP DBL_MAX_10_EXP
#else
# define SCM_MAXEXP 308		/* IEEE doubles */
#endif /* def DBL_MAX_10_EXP */

#ifdef FLT_MAX
# define SCM_FLTMAX FLT_MAX
#else
# define SCM_FLTMAX 1e+23
#endif /* def FLT_MAX */


/* SCM_INTBUFLEN is the maximum number of characters neccessary for
 * the printed or scm_string representation of an scm_t_intmax in
 * radix 2.  The buffer passed to scm_iint2str and scm_iuint2str must
 * be of this size, for example.
 */
#define SCM_INTBUFLEN (5 + SCM_CHAR_BIT*sizeof(scm_t_intmax))



/* Numbers 
 */


/* Note that scm_tc16_real and scm_tc16_complex are given tc16-codes that only
 * differ in one bit: This way, checking if an object is an inexact number can
 * be done quickly (using the TYP16S macro).  */

/* Number subtype 1 to 3 (note the dependency on the predicates SCM_INEXACTP
 * and SCM_NUMP)  */
#define scm_tc16_big		(scm_tc7_number + 1 * 256L)
#define scm_tc16_real           (scm_tc7_number + 2 * 256L)
#define scm_tc16_complex        (scm_tc7_number + 3 * 256L)
#define scm_tc16_fraction       (scm_tc7_number + 4 * 256L)

#define SCM_INEXACTP(x) \
  (!SCM_IMP (x) && (0xfeff & SCM_CELL_TYPE (x)) == scm_tc16_real)
#define SCM_REALP(x) (!SCM_IMP (x) && SCM_TYP16 (x) == scm_tc16_real)
#define SCM_COMPLEXP(x) (!SCM_IMP (x) && SCM_TYP16 (x) == scm_tc16_complex)

#define SCM_REAL_VALUE(x) (((scm_t_double *) SCM2PTR (x))->real)
#define SCM_COMPLEX_MEM(x) ((scm_t_complex *) SCM_CELL_WORD_1 (x))
#define SCM_COMPLEX_REAL(x) (SCM_COMPLEX_MEM (x)->real)
#define SCM_COMPLEX_IMAG(x) (SCM_COMPLEX_MEM (x)->imag)

/* Each bignum is just an mpz_t stored in a double cell starting at word 1. */
#define SCM_I_BIG_MPZ(x) (*((mpz_t *) (SCM_CELL_OBJECT_LOC((x),1))))
#define SCM_BIGP(x) (!SCM_IMP (x) && SCM_TYP16 (x) == scm_tc16_big)

#define SCM_NUMBERP(x) (SCM_I_INUMP(x) || SCM_NUMP(x))
#define SCM_NUMP(x) (!SCM_IMP(x) \
  && (((0xfcff & SCM_CELL_TYPE (x)) == scm_tc7_number) \
      || ((0xfbff & SCM_CELL_TYPE (x)) == scm_tc7_number)))
/* 0xfcff (#b1100) for 0 free, 1 big, 2 real, 3 complex, then 0xfbff (#b1011) for 4 fraction */

#define SCM_FRACTIONP(x) (!SCM_IMP (x) && SCM_TYP16 (x) == scm_tc16_fraction)
#define SCM_FRACTION_NUMERATOR(x) (SCM_CELL_OBJECT_1 (x))
#define SCM_FRACTION_DENOMINATOR(x) (SCM_CELL_OBJECT_2 (x))



typedef struct scm_t_double
{
  SCM type;
  SCM pad;
  double real;
} scm_t_double;

typedef struct scm_t_complex
{
  double real;
  double imag;
} scm_t_complex;



SCM_API SCM scm_exact_p (SCM x);
SCM_API SCM scm_odd_p (SCM n);
SCM_API SCM scm_even_p (SCM n);
SCM_API SCM scm_inf_p (SCM n);
SCM_API SCM scm_nan_p (SCM n);
SCM_API SCM scm_inf (void);
SCM_API SCM scm_nan (void);
SCM_API SCM scm_abs (SCM x);
SCM_API SCM scm_quotient (SCM x, SCM y);
SCM_API SCM scm_remainder (SCM x, SCM y);
SCM_API SCM scm_modulo (SCM x, SCM y);
SCM_API SCM scm_gcd (SCM x, SCM y);
SCM_API SCM scm_lcm (SCM n1, SCM n2);
SCM_API SCM scm_logand (SCM n1, SCM n2);
SCM_API SCM scm_logior (SCM n1, SCM n2);
SCM_API SCM scm_logxor (SCM n1, SCM n2);
SCM_API SCM scm_logtest (SCM n1, SCM n2);
SCM_API SCM scm_logbit_p (SCM n1, SCM n2);
SCM_API SCM scm_lognot (SCM n);
SCM_API SCM scm_modulo_expt (SCM n, SCM k, SCM m);
SCM_API SCM scm_integer_expt (SCM z1, SCM z2);
SCM_API SCM scm_ash (SCM n, SCM cnt);
SCM_API SCM scm_bit_extract (SCM n, SCM start, SCM end);
SCM_API SCM scm_logcount (SCM n);
SCM_API SCM scm_integer_length (SCM n);

SCM_API size_t scm_iint2str (scm_t_intmax num, int rad, char *p);
SCM_API size_t scm_iuint2str (scm_t_uintmax num, int rad, char *p);
SCM_API SCM scm_number_to_string (SCM x, SCM radix);
SCM_API int scm_print_real (SCM sexp, SCM port, scm_print_state *pstate);
SCM_API int scm_print_complex (SCM sexp, SCM port, scm_print_state *pstate);
SCM_API int scm_bigprint (SCM exp, SCM port, scm_print_state *pstate);
SCM_API SCM scm_c_locale_stringn_to_number (const char *mem, size_t len,
					    unsigned int radix);
SCM_API SCM scm_string_to_number (SCM str, SCM radix);
SCM_API SCM scm_bigequal (SCM x, SCM y);
SCM_API SCM scm_real_equalp (SCM x, SCM y);
SCM_API SCM scm_complex_equalp (SCM x, SCM y);
SCM_API SCM scm_number_p (SCM x);
SCM_API SCM scm_complex_p (SCM x);
SCM_API SCM scm_real_p (SCM x);
SCM_API SCM scm_rational_p (SCM z);
SCM_API SCM scm_integer_p (SCM x);
SCM_API SCM scm_inexact_p (SCM x);
SCM_API SCM scm_num_eq_p (SCM x, SCM y);
SCM_API SCM scm_less_p (SCM x, SCM y);
SCM_API SCM scm_gr_p (SCM x, SCM y);
SCM_API SCM scm_leq_p (SCM x, SCM y);
SCM_API SCM scm_geq_p (SCM x, SCM y);
SCM_API SCM scm_zero_p (SCM z);
SCM_API SCM scm_positive_p (SCM x);
SCM_API SCM scm_negative_p (SCM x);
SCM_API SCM scm_max (SCM x, SCM y);
SCM_API SCM scm_min (SCM x, SCM y);
SCM_API SCM scm_sum (SCM x, SCM y);
SCM_API SCM scm_oneplus (SCM x);
SCM_API SCM scm_difference (SCM x, SCM y);
SCM_API SCM scm_oneminus (SCM x);
SCM_API SCM scm_product (SCM x, SCM y);
SCM_API SCM scm_divide (SCM x, SCM y);
SCM_API SCM scm_floor (SCM x);
SCM_API SCM scm_ceiling (SCM x);
SCM_API double scm_asinh (double x);
SCM_API double scm_acosh (double x);
SCM_API double scm_atanh (double x);
SCM_API double scm_c_truncate (double x);
SCM_API double scm_c_round (double x);
SCM_API SCM scm_truncate_number (SCM x);
SCM_API SCM scm_round_number (SCM x);
SCM_API SCM scm_sys_expt (SCM z1, SCM z2);
SCM_API SCM scm_sys_atan2 (SCM z1, SCM z2);
SCM_API SCM scm_make_rectangular (SCM z1, SCM z2);
SCM_API SCM scm_make_polar (SCM z1, SCM z2);
SCM_API SCM scm_real_part (SCM z);
SCM_API SCM scm_imag_part (SCM z);
SCM_API SCM scm_magnitude (SCM z);
SCM_API SCM scm_angle (SCM z);
SCM_API SCM scm_exact_to_inexact (SCM z);
SCM_API SCM scm_inexact_to_exact (SCM z);
SCM_API SCM scm_trunc (SCM x);
SCM_API SCM scm_log (SCM z);
SCM_API SCM scm_log10 (SCM z);
SCM_API SCM scm_exp (SCM z);
SCM_API SCM scm_sqrt (SCM z);

/* bignum internal functions */
SCM_API SCM scm_i_mkbig (void);
SCM_API SCM scm_i_normbig (SCM x);
SCM_API int scm_i_bigcmp (SCM a, SCM b);
SCM_API SCM scm_i_dbl2big (double d);
SCM_API SCM scm_i_dbl2num (double d);
SCM_API double scm_i_big2dbl (SCM b);
SCM_API SCM scm_i_long2big (long n);
SCM_API SCM scm_i_ulong2big (unsigned long n);
SCM_API SCM scm_i_clonebig (SCM src_big, int same_sign_p);

/* ratio functions */
SCM_API SCM scm_rationalize (SCM x, SCM err);
SCM_API SCM scm_numerator (SCM z);
SCM_API SCM scm_denominator (SCM z);

/* fraction internal functions */
SCM_API double scm_i_fraction2double (SCM z);
SCM_API SCM scm_i_fraction_equalp (SCM x, SCM y);
SCM_API int scm_i_print_fraction (SCM sexp, SCM port, scm_print_state *pstate);

/* general internal functions */
SCM_API void scm_i_print_double (double val, SCM port);
SCM_API void scm_i_print_complex (double real, double imag, SCM port);

/* conversion functions for integers */

SCM_API int scm_is_integer (SCM val);
SCM_API int scm_is_signed_integer (SCM val,
				   scm_t_intmax min, scm_t_intmax max);
SCM_API int scm_is_unsigned_integer (SCM val,
				     scm_t_uintmax min, scm_t_uintmax max);

SCM_API SCM scm_from_signed_integer (scm_t_intmax val);
SCM_API SCM scm_from_unsigned_integer (scm_t_uintmax val);

SCM_API scm_t_intmax scm_to_signed_integer (SCM val,
					    scm_t_intmax min,
					    scm_t_intmax max);
SCM_API scm_t_uintmax scm_to_unsigned_integer (SCM val,
					       scm_t_uintmax min,
					       scm_t_uintmax max);

SCM_API scm_t_int8   scm_to_int8     (SCM x);
SCM_API SCM          scm_from_int8   (scm_t_int8 x);

SCM_API scm_t_uint8  scm_to_uint8    (SCM x);
SCM_API SCM          scm_from_uint8  (scm_t_uint8 x);

SCM_API scm_t_int16  scm_to_int16    (SCM x);
SCM_API SCM          scm_from_int16  (scm_t_int16 x);

SCM_API scm_t_uint16 scm_to_uint16   (SCM x);
SCM_API SCM          scm_from_uint16 (scm_t_uint16 x);

SCM_API scm_t_int32  scm_to_int32    (SCM x);
SCM_API SCM          scm_from_int32  (scm_t_int32 x);

SCM_API scm_t_uint32 scm_to_uint32   (SCM x);
SCM_API SCM          scm_from_uint32 (scm_t_uint32 x);

SCM_API scm_t_int64  scm_to_int64    (SCM x);
SCM_API SCM          scm_from_int64  (scm_t_int64 x);

SCM_API scm_t_uint64 scm_to_uint64   (SCM x);
SCM_API SCM          scm_from_uint64 (scm_t_uint64 x);

SCM_API void scm_to_mpz (SCM x, mpz_t rop);
SCM_API SCM  scm_from_mpz (mpz_t rop);


/* The conversion functions for other types are aliased to the
   appropriate ones from above.  We pick the right one based on the
   size of the type.

   Not each and every possibility is covered by the code below, and
   while it is trivial to complete the tests, it might be better to
   just test for the 'sane' possibilities.  When one of the tests
   below fails, chances are good that some silent assumption somewhere
   else will also fail.
*/

#if SCM_SIZEOF_CHAR == 1
#define scm_to_schar   scm_to_int8
#define scm_from_schar scm_from_int8
#define scm_to_uchar   scm_to_uint8
#define scm_from_uchar scm_from_uint8
#if CHAR_MIN == 0
#define scm_to_char    scm_to_uint8
#define scm_from_char  scm_from_uint8
#else
#define scm_to_char    scm_to_int8
#define scm_from_char  scm_from_int8
#endif
#else
#error sizeof(char) is not 1.
#endif

#if SCM_SIZEOF_SHORT == 1
#define scm_to_short    scm_to_int8
#define scm_from_short  scm_from_int8
#define scm_to_ushort   scm_to_uint8
#define scm_from_ushort scm_from_uint8
#else
#if SCM_SIZEOF_SHORT == 2
#define scm_to_short    scm_to_int16
#define scm_from_short  scm_from_int16
#define scm_to_ushort   scm_to_uint16
#define scm_from_ushort scm_from_uint16
#else
#if SCM_SIZEOF_SHORT == 4
#define scm_to_short    scm_to_int32
#define scm_from_short  scm_from_int32
#define scm_to_ushort   scm_to_uint32
#define scm_from_ushort scm_from_uint32
#else
#error sizeof(short) is not 1, 2, or 4.
#endif
#endif
#endif

#if SCM_SIZEOF_INT == 4
#define scm_to_int    scm_to_int32
#define scm_from_int  scm_from_int32
#define scm_to_uint   scm_to_uint32
#define scm_from_uint scm_from_uint32
#else
#if SCM_SIZEOF_INT == 8
#define scm_to_int    scm_to_int64
#define scm_from_int  scm_from_int64
#define scm_to_uint   scm_to_uint64
#define scm_from_uint scm_from_uint64
#else
#error sizeof(int) is not 4 or 8.
#endif
#endif

#if SCM_SIZEOF_LONG == 4
#define scm_to_long    scm_to_int32
#define scm_from_long  scm_from_int32
#define scm_to_ulong   scm_to_uint32
#define scm_from_ulong scm_from_uint32
#else
#if SCM_SIZEOF_LONG == 8
#define scm_to_long    scm_to_int64
#define scm_from_long  scm_from_int64
#define scm_to_ulong   scm_to_uint64
#define scm_from_ulong scm_from_uint64
#else
#error sizeof(long) is not 4 or 8.
#endif
#endif

#if SCM_SIZEOF_INTMAX == 4
#define scm_to_intmax    scm_to_int32
#define scm_from_intmax  scm_from_int32
#define scm_to_uintmax   scm_to_uint32
#define scm_from_uintmax scm_from_uint32
#else
#if SCM_SIZEOF_INTMAX == 8
#define scm_to_intmax    scm_to_int64
#define scm_from_intmax  scm_from_int64
#define scm_to_uintmax   scm_to_uint64
#define scm_from_uintmax scm_from_uint64
#else
#error sizeof(scm_t_intmax) is not 4 or 8.
#endif
#endif

#if SCM_SIZEOF_LONG_LONG == 0
#else
#if SCM_SIZEOF_LONG_LONG == 8
#define scm_to_long_long    scm_to_int64
#define scm_from_long_long  scm_from_int64
#define scm_to_ulong_long   scm_to_uint64
#define scm_from_ulong_long scm_from_uint64
#else
#error sizeof(long long) is not 8.
#endif
#endif

#if SCM_SIZEOF_SIZE_T == 4
#define scm_to_ssize_t    scm_to_int32
#define scm_from_ssize_t  scm_from_int32
#define scm_to_size_t     scm_to_uint32
#define scm_from_size_t   scm_from_uint32
#else
#if SCM_SIZEOF_SIZE_T == 8
#define scm_to_ssize_t    scm_to_int64
#define scm_from_ssize_t  scm_from_int64
#define scm_to_size_t     scm_to_uint64
#define scm_from_size_t   scm_from_uint64
#else
#error sizeof(size_t) is not 4 or 8.
#endif
#endif

/* conversion functions for double */

SCM_API int scm_is_real (SCM val);
SCM_API int scm_is_rational (SCM val);
SCM_API double scm_to_double (SCM val);
SCM_API SCM scm_from_double (double val);

/* conversion functions for complex */

SCM_API int scm_is_complex (SCM val);
SCM_API SCM scm_c_make_rectangular (double re, double im);
SCM_API SCM scm_c_make_polar (double mag, double ang);
SCM_API double scm_c_real_part (SCM z);
SCM_API double scm_c_imag_part (SCM z);
SCM_API double scm_c_magnitude (SCM z);
SCM_API double scm_c_angle (SCM z);

SCM_API int scm_is_number (SCM val);

SCM_API void scm_init_numbers (void);

#endif  /* SCM_NUMBERS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
