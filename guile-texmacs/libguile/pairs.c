/* Copyright (C) 1995,1996,2000,2001, 2004, 2005, 2006, 2008 Free Software Foundation, Inc.
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

#include "libguile/_scm.h"
#include "libguile/validate.h"

#include "libguile/pairs.h"



/* {Pairs}
 */

#if (SCM_DEBUG_PAIR_ACCESSES == 1)

#include "libguile/ports.h"
#include "libguile/strings.h"

void scm_error_pair_access (SCM non_pair)
{
  static unsigned int running = 0;
  SCM message = scm_from_locale_string ("Non-pair accessed with SCM_C[AD]R: `~S'\n");

  if (!running)
    {
      running = 1;
      scm_simple_format (scm_current_error_port (),
			 message, scm_list_1 (non_pair));
      abort ();
    }
}

#endif

SCM_DEFINE (scm_cons, "cons", 2, 0, 0,
	    (SCM x, SCM y),
	    "Return a newly allocated pair whose car is @var{x} and whose\n"
	    "cdr is @var{y}.  The pair is guaranteed to be different (in the\n"
	    "sense of @code{eq?}) from every previously existing object.")
#define FUNC_NAME s_scm_cons
{
  return scm_cell (SCM_UNPACK (x), SCM_UNPACK (y));
}
#undef FUNC_NAME


SCM 
scm_cons2 (SCM w, SCM x, SCM y)
{
  return scm_cons (w, scm_cons (x, y));
}


SCM_DEFINE (scm_pair_p, "pair?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} if @var{x} is a pair; otherwise return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_pair_p
{
  return scm_from_bool (scm_is_pair (x));
}
#undef FUNC_NAME

SCM
scm_car (SCM pair)
{
  if (!scm_is_pair (pair))
    scm_wrong_type_arg_msg (NULL, 0, pair, "pair");
  return SCM_CAR (pair);
}

SCM
scm_cdr (SCM pair)
{
  if (!scm_is_pair (pair))
    scm_wrong_type_arg_msg (NULL, 0, pair, "pair");
  return SCM_CDR (pair);
}

SCM
scm_i_chase_pairs (SCM tree, scm_t_uint32 pattern)
{
  do
    {
      if (!scm_is_pair (tree))
	scm_wrong_type_arg_msg (NULL, 0, tree, "pair");
      tree = (pattern & 1) ? SCM_CAR (tree) : SCM_CDR (tree);
      pattern >>= 2;
    }
  while (pattern);
  return tree;
}

SCM_DEFINE (scm_set_car_x, "set-car!", 2, 0, 0,
            (SCM pair, SCM value),
            "Stores @var{value} in the car field of @var{pair}.  The value returned\n"
            "by @code{set-car!} is unspecified.")
#define FUNC_NAME s_scm_set_car_x
{
  SCM_VALIDATE_CONS (1, pair);
  SCM_SETCAR (pair, value);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_set_cdr_x, "set-cdr!", 2, 0, 0,
            (SCM pair, SCM value),
            "Stores @var{value} in the cdr field of @var{pair}.  The value returned\n"
            "by @code{set-cdr!} is unspecified.")
#define FUNC_NAME s_scm_set_cdr_x
{
  SCM_VALIDATE_CONS (1, pair);
  SCM_SETCDR (pair, value);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* Every cxr-pattern is made up of pairs of bits, starting with the two least
 * significant bits.  If in a pair of bits the least significant of the two
 * bits is 0, this means CDR, otherwise CAR.  The most significant bits of the
 * two bits is only needed to indicate when cxr-ing is ready.  This is the
 * case, when all remaining pairs of bits equal 00.  */

typedef struct {
  const char *name;
  unsigned char pattern;
} t_cxr;

static const t_cxr cxrs[] = 
{
  {"cdr",    0x02}, /* 00000010 */
  {"car",    0x03}, /* 00000011 */
  {"cddr",   0x0a}, /* 00001010 */
  {"cdar",   0x0b}, /* 00001011 */
  {"cadr",   0x0e}, /* 00001110 */
  {"caar",   0x0f}, /* 00001111 */
  {"cdddr",  0x2a}, /* 00101010 */
  {"cddar",  0x2b}, /* 00101011 */
  {"cdadr",  0x2e}, /* 00101110 */
  {"cdaar",  0x2f}, /* 00101111 */
  {"caddr",  0x3a}, /* 00111010 */
  {"cadar",  0x3b}, /* 00111011 */
  {"caadr",  0x3e}, /* 00111110 */
  {"caaar",  0x3f}, /* 00111111 */
  {"cddddr", 0xaa}, /* 10101010 */
  {"cdddar", 0xab}, /* 10101011 */
  {"cddadr", 0xae}, /* 10101110 */
  {"cddaar", 0xaf}, /* 10101111 */
  {"cdaddr", 0xba}, /* 10111010 */
  {"cdadar", 0xbb}, /* 10111011 */
  {"cdaadr", 0xbe}, /* 10111110 */
  {"cdaaar", 0xbf}, /* 10111111 */
  {"cadddr", 0xea}, /* 11101010 */
  {"caddar", 0xeb}, /* 11101011 */
  {"cadadr", 0xee}, /* 11101110 */
  {"cadaar", 0xef}, /* 11101111 */
  {"caaddr", 0xfa}, /* 11111010 */
  {"caadar", 0xfb}, /* 11111011 */
  {"caaadr", 0xfe}, /* 11111110 */
  {"caaaar", 0xff}, /* 11111111 */
  {0, 0}
};



void
scm_init_pairs ()
{
  unsigned int subnr = 0;

  for (subnr = 0; cxrs[subnr].name; subnr++)
    {
      SCM (*pattern) () = (SCM (*) ()) (scm_t_bits) cxrs[subnr].pattern;
      scm_c_define_subr (cxrs[subnr].name, scm_tc7_cxr, pattern);
    }

#include "libguile/pairs.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
