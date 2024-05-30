/* Copyright (C) 2004, 2006, 2008, 2009 Free Software Foundation, Inc.
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

#if HAVE_CONFIG_H
# include <config.h>
#endif

#include <assert.h>
#include <math.h>
#include <stdio.h>

#if HAVE_FENV_H
#include <fenv.h>
#elif defined HAVE_MACHINE_FPU_H
/* On Tru64 5.1b, the declaration of fesetround(3) is in <machine/fpu.h>.
   On NetBSD, this header has to be included along with <sys/types.h>.  */
# ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
# endif
# include <machine/fpu.h>
#endif

#include <libguile.h>


#define numberof(x)  (sizeof (x) / sizeof ((x)[0]))

static void
test_scm_c_round ()
{
  /* FE constants are defined only where supported, in particular for
     instance some ARM systems have been seen with only a couple of modes */
  static const int modes[] = {
    0,
#ifdef FE_TONEAREST
    FE_TONEAREST,
#endif
#ifdef FE_UPWARD
    FE_UPWARD,
#endif
#ifdef FE_DOWNWARD
    FE_DOWNWARD,
#endif
#ifdef FE_TOWARDZERO
    FE_TOWARDZERO,
#endif
  };

  double  x, want;
  int  i;

  for (i = 0; i < numberof (modes); i++)
    {
      /* First iteration is the default rounding mode, ie. no call to
         fesetround.  Subsequent iterations are the FE modes from the
         table.  */
      if (i != 0)
        {
#if HAVE_FESETROUND
          fesetround (modes[i]);
#endif
        }

      assert (scm_c_round (0.0) == 0.0);
      assert (scm_c_round (1.0) == 1.0);
      assert (scm_c_round (-1.0) == -1.0);

      assert (scm_c_round (0.5) == 0.0);
      assert (scm_c_round (1.5) == 2.0);
      assert (scm_c_round (-1.5) == -2.0);
      assert (scm_c_round (2.5) == 2.0);
      assert (scm_c_round (-2.5) == -2.0);
      assert (scm_c_round (3.5) == 4.0);
      assert (scm_c_round (-3.5) == -4.0);

      /* 2^(DBL_MANT_DIG-1)-1+0.5 */
      x = ldexp (1.0, DBL_MANT_DIG - 1) - 1.0 + 0.5;
      want = ldexp (1.0, DBL_MANT_DIG - 1);
      assert (scm_c_round (x) == want);

      /* -(2^(DBL_MANT_DIG-1)-1+0.5) */
      x = - (ldexp (1.0, DBL_MANT_DIG - 1) - 1.0 + 0.5);
      want = - ldexp (1.0, DBL_MANT_DIG - 1);
      assert (scm_c_round (x) == want);

      /* 2^DBL_MANT_DIG-1
         In the past scm_c_round had incorrectly incremented this value, due
         to the way that x+0.5 would round upwards (in the usual default
         nearest-even mode on most systems).  */
      x = ldexp (1.0, DBL_MANT_DIG) - 1.0;
      assert (x == floor (x));      /* should be an integer already */
      assert (scm_c_round (x) == x);  /* scm_c_round should return it unchanged */

      /* -(2^DBL_MANT_DIG-1) */
      x = - (ldexp (1.0, DBL_MANT_DIG) - 1.0);
      assert (x == floor (x));      /* should be an integer already */
      assert (scm_c_round (x) == x);  /* scm_c_round should return it unchanged */

      /* 2^64 */
      x = ldexp (1.0, 64);
      assert (scm_c_round (x) == x);

      /* -2^64
         In the past scm_c_round had incorrectely gone to the next highest
         representable value in FE_UPWARD, due to x+0.5 rounding.  */
      x = - ldexp (1.0, 64);
      assert (scm_c_round (x) == x);
    }
}

static void
tests (void *data, int argc, char **argv)
{
  test_scm_c_round ();
}

int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, tests, NULL);
  return 0;
}
