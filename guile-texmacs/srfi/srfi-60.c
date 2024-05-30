/* srfi-60.c --- Integers as Bits
 *
 * Copyright (C) 2005, 2006, 2008 Free Software Foundation, Inc.
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libguile.h>
#include <srfi/srfi-60.h>

#define SCM_MIN(A, B) ((A) < (B) ? (A) : (B))

SCM_DEFINE (scm_srfi60_log2_binary_factors, "log2-binary-factors", 1, 0, 0,
            (SCM n),
	    "Return a count of how many factors of 2 are present in @var{n}.\n"
	    "This is also the bit index of the lowest 1 bit in @var{n}.  If\n"
	    "@var{n} is 0, the return is @math{-1}.\n"
	    "\n"
	    "@example\n"
	    "(log2-binary-factors 6) @result{} 1\n"
	    "(log2-binary-factors -8) @result{} 3\n"
	    "@end example")
#define FUNC_NAME s_scm_srfi60_log2_binary_factors
{
  SCM ret = SCM_EOL;

  if (SCM_I_INUMP (n))
    {
      long nn = SCM_I_INUM (n);
      if (nn == 0)
        return SCM_I_MAKINUM (-1);
      nn = nn ^ (nn-1);  /* 1 bits for each low 0 and lowest 1 */
      return scm_logcount (SCM_I_MAKINUM (nn >> 1));
    }
  else if (SCM_BIGP (n))
    {
      /* no need for scm_remember_upto_here_1 here, mpz_scan1 doesn't do
         anything that could result in a gc */
      return SCM_I_MAKINUM (mpz_scan1 (SCM_I_BIG_MPZ (n), 0L));
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);

  return ret;
}
#undef FUNC_NAME


SCM_DEFINE (scm_srfi60_copy_bit, "copy-bit", 3, 0, 0,
            (SCM index, SCM n, SCM bit),
	    "Return @var{n} with the bit at @var{index} set according to\n"
	    "@var{newbit}.  @var{newbit} should be @code{#t} to set the bit\n"
	    "to 1, or @code{#f} to set it to 0.  Bits other than at\n"
	    "@var{index} are unchanged in the return.\n"
	    "\n"
	    "@example\n"
	    "(copy-bit 1 #b0101 #t) @result{} 7\n"
	    "@end example")
#define FUNC_NAME s_scm_srfi60_copy_bit
{
  SCM r;
  unsigned long ii;
  int bb;

  ii = scm_to_ulong (index);
  bb = scm_to_bool (bit);

  if (SCM_I_INUMP (n))
    {
      long nn = SCM_I_INUM (n);

      /* can't set high bit ii==SCM_LONG_BIT-1, that would change the sign,
         which is not what's wanted */
      if (ii < SCM_LONG_BIT-1)
        {
          nn &= ~(1L << ii);  /* zap bit at index */
          nn |= ((long) bb << ii);   /* insert desired bit */
          return scm_from_long (nn);
        }
      else
        {
          /* bits at ii==SCM_LONG_BIT-1 and above are all copies of the sign
             bit, if this is already the desired "bit" value then no need to
             make a new bignum value */
          if (bb == (nn < 0))
            return n;

          r = scm_i_long2big (nn);
          goto big;
        }
    }
  else if (SCM_BIGP (n))
    {
      /* if the bit is already what's wanted then no need to make a new
         bignum */
      if (bb == mpz_tstbit (SCM_I_BIG_MPZ (n), ii))
        return n;

      r = scm_i_clonebig (n, 1);
    big:
      if (bb)
        mpz_setbit (SCM_I_BIG_MPZ (r), ii);
      else
        mpz_clrbit (SCM_I_BIG_MPZ (r), ii);

      /* changing a high bit might put the result into range of a fixnum */
      return scm_i_normbig (r);
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);
}
#undef FUNC_NAME


SCM_DEFINE (scm_srfi60_rotate_bit_field, "rotate-bit-field", 4, 0, 0,
            (SCM n, SCM count, SCM start, SCM end),
	    "Return @var{n} with the bit field from @var{start} (inclusive)\n"
	    "to @var{end} (exclusive) rotated upwards by @var{count} bits.\n"
	    "\n"
	    "@var{count} can be positive or negative, and it can be more\n"
	    "than the field width (it'll be reduced modulo the width).\n"
	    "\n"
	    "@example\n"
	    "(rotate-bit-field #b0110 2 1 4) @result{} #b1010\n"
	    "@end example")
#define FUNC_NAME s_scm_srfi60_rotate_bit_field
{
  unsigned long ss = scm_to_ulong (start);
  unsigned long ee = scm_to_ulong (end);
  unsigned long ww, cc;

  SCM_ASSERT_RANGE (3, end, (ee >= ss));
  ww = ee - ss;

  cc = scm_to_ulong (scm_modulo (count, scm_difference (end, start)));

  if (SCM_I_INUMP (n))
    {
      long nn = SCM_I_INUM (n);

      if (ee <= SCM_LONG_BIT-1)
        {
          /* all within a long */
          long below = nn & ((1L << ss) - 1);  /* before start */
          long above = nn & (-1L << ee);       /* above end */
          long fmask = (-1L << ss) & ((1L << ee) - 1);  /* field mask */
          long ff = nn & fmask;                /* field */

          return scm_from_long (above
                                | ((ff << cc) & fmask)
                                | ((ff >> (ww-cc)) & fmask)
                                | below);
        }
      else
        {
          /* either no movement, or a field of only 0 or 1 bits, result
             unchanged, avoid creating a bignum */
          if (cc == 0 || ww <= 1)
            return n;

          n = scm_i_long2big (nn);
          goto big;
        }
    }
  else if (SCM_BIGP (n))
    {
      mpz_t tmp;
      SCM r;

      /* either no movement, or in a field of only 0 or 1 bits, result
         unchanged, avoid creating a new bignum */
      if (cc == 0 || ww <= 1)
        return n;

    big:
      r = scm_i_ulong2big (0);
      mpz_init (tmp);

      /* portion above end */
      mpz_fdiv_q_2exp (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (n), ee);
      mpz_mul_2exp (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (r), ee);

      /* field high part, width-count bits from start go to start+count */
      mpz_fdiv_q_2exp (tmp, SCM_I_BIG_MPZ (n), ss);
      mpz_fdiv_r_2exp (tmp, tmp, ww - cc);
      mpz_mul_2exp (tmp, tmp, ss + cc);
      mpz_ior (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (r), tmp);

      /* field high part, count bits from end-count go to start */
      mpz_fdiv_q_2exp (tmp, SCM_I_BIG_MPZ (n), ee - cc);
      mpz_fdiv_r_2exp (tmp, tmp, cc);
      mpz_mul_2exp (tmp, tmp, ss);
      mpz_ior (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (r), tmp);

      /* portion below start */
      mpz_fdiv_r_2exp (tmp, SCM_I_BIG_MPZ (n), ss);
      mpz_ior (SCM_I_BIG_MPZ (r), SCM_I_BIG_MPZ (r), tmp);

      mpz_clear (tmp);

      /* bits moved around might leave us in range of an inum */
      return scm_i_normbig (r);
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);
}
#undef FUNC_NAME


SCM_DEFINE (scm_srfi60_reverse_bit_field, "reverse-bit-field", 3, 0, 0,
            (SCM n, SCM start, SCM end),
	    "Return @var{n} with the bits between @var{start} (inclusive) to\n"
	    "@var{end} (exclusive) reversed.\n"
	    "\n"
	    "@example\n"
	    "(reverse-bit-field #b101001 2 4) @result{} #b100101\n"
	    "@end example")
#define FUNC_NAME s_scm_srfi60_reverse_bit_field
{
  long ss = scm_to_long (start);
  long ee = scm_to_long (end);
  long swaps = (ee - ss) / 2;  /* number of swaps */
  SCM b;

  if (SCM_I_INUMP (n))
    {
      long nn = SCM_I_INUM (n);

      if (ee <= SCM_LONG_BIT-1)
        {
          /* all within a long */
          long smask = 1L << ss;
          long emask = 1L << (ee-1);
          for ( ; swaps > 0; swaps--)
            {
              long sbit = nn & smask;
              long ebit = nn & emask;
              nn ^= sbit ^ (ebit ? smask : 0)  /* zap sbit, put ebit value */
                ^   ebit ^ (sbit ? emask : 0); /* zap ebit, put sbit value */

              smask <<= 1;
              emask >>= 1;
            }
          return scm_from_long (nn);
        }
      else
        {
          /* avoid creating a new bignum if reversing only 0 or 1 bits */
          if (ee - ss <= 1)
            return n;

          b = scm_i_long2big (nn);
          goto big;
        }
    }
  else if (SCM_BIGP (n))
    {
      /* avoid creating a new bignum if reversing only 0 or 1 bits */
      if (ee - ss <= 1)
        return n;

      b = scm_i_clonebig (n, 1);
    big:

      ee--;
      for ( ; swaps > 0; swaps--)
        {
          int sbit = mpz_tstbit (SCM_I_BIG_MPZ (b), ss);
          int ebit = mpz_tstbit (SCM_I_BIG_MPZ (b), ee);
          if (sbit ^ ebit)
            {
              /* the two bits are different, flip them */
              if (sbit)
                {
                  mpz_clrbit (SCM_I_BIG_MPZ (b), ss);
                  mpz_setbit (SCM_I_BIG_MPZ (b), ee);
                }
              else
                {
                  mpz_setbit (SCM_I_BIG_MPZ (b), ss);
                  mpz_clrbit (SCM_I_BIG_MPZ (b), ee);
                }
            }
          ss++;
          ee--;
        }
      /* swapping zero bits into the high might make us fit a fixnum */
      return scm_i_normbig (b);
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);
}
#undef FUNC_NAME


SCM_DEFINE (scm_srfi60_integer_to_list, "integer->list", 1, 1, 0,
            (SCM n, SCM len),
	    "Return bits from @var{n} in the form of a list of @code{#t} for\n"
	    "1 and @code{#f} for 0.  The least significant @var{len} bits\n"
	    "are returned, and the first list element is the most\n"
	    "significant of those bits.  If @var{len} is not given, the\n"
	    "default is @code{(integer-length @var{n})} (@pxref{Bitwise\n"
	    "Operations}).\n"
	    "\n"
	    "@example\n"
	    "(integer->list 6)   @result{} (#t #t #f)\n"
	    "(integer->list 1 4) @result{} (#f #f #f #t)\n"
	    "@end example")
#define FUNC_NAME s_scm_srfi60_integer_to_list
{
  SCM ret = SCM_EOL;
  unsigned long ll, i;

  if (SCM_UNBNDP (len))
    len = scm_integer_length (n);
  ll = scm_to_ulong (len);

  if (SCM_I_INUMP (n))
    {
      long nn = SCM_I_INUM (n);
      for (i = 0; i < ll; i++)
        {
          unsigned long shift = SCM_MIN (i, (unsigned long) SCM_LONG_BIT-1);
          int bit = (nn >> shift) & 1;
          ret = scm_cons (scm_from_bool (bit), ret);
        }
    }
  else if (SCM_BIGP (n))
    {
      for (i = 0; i < ll; i++)
        ret = scm_cons (scm_from_bool (mpz_tstbit (SCM_I_BIG_MPZ (n), i)),
                        ret);
      scm_remember_upto_here_1 (n);
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);

  return ret;
}
#undef FUNC_NAME


SCM_DEFINE (scm_srfi60_list_to_integer, "list->integer", 1, 0, 0,
            (SCM lst),
	    "Return an integer formed bitwise from the given @var{lst} list\n"
	    "of booleans.  Each boolean is @code{#t} for a 1 and @code{#f}\n"
	    "for a 0.  The first element becomes the most significant bit in\n"
	    "the return.\n"
	    "\n"
	    "@example\n"
	    "(list->integer '(#t #f #t #f)) @result{} 10\n"
	    "@end example")
#define FUNC_NAME s_scm_srfi60_list_to_integer
{
  long len;

  /* strip high zero bits from lst; after this the length tells us whether
     an inum or bignum is required */
  while (scm_is_pair (lst) && scm_is_false (SCM_CAR (lst)))
    lst = SCM_CDR (lst);

  SCM_VALIDATE_LIST_COPYLEN (SCM_ARG1, lst, len);

  if (len <= SCM_I_FIXNUM_BIT - 1)
    {
      /* fits an inum (a positive inum) */
      long n = 0;
      while (scm_is_pair (lst))
        {
          n <<= 1;
          if (! scm_is_false (SCM_CAR (lst)))
            n++;
          lst = SCM_CDR (lst);
        }
      return SCM_I_MAKINUM (n);
    }
  else
    {
      /* need a bignum */
      SCM n = scm_i_ulong2big (0);
      while (scm_is_pair (lst))
        {
          len--;
          if (! scm_is_false (SCM_CAR (lst)))
            mpz_setbit (SCM_I_BIG_MPZ (n), len);
          lst = SCM_CDR (lst);
        }
      return n;
    }
}
#undef FUNC_NAME


/* note: don't put "scm_srfi60_list_to_integer" arg on its own line, a
   newline breaks the snarfer */
SCM_REGISTER_PROC (s_srfi60_booleans_to_integer, "booleans->integer", 0, 0, 1, scm_srfi60_list_to_integer);


void
scm_init_srfi_60 (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "srfi/srfi-60.x"
#endif
}
