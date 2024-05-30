/* Copyright (C) 1999,2000,2001, 2003, 2005, 2006, 2010 Free Software Foundation, Inc.
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



/* Author: Mikael Djurfeldt <djurfeldt@nada.kth.se> */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "libguile/_scm.h"

#include <gmp.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "libguile/smob.h"
#include "libguile/numbers.h"
#include "libguile/feature.h"
#include "libguile/strings.h"
#include "libguile/unif.h"
#include "libguile/srfi-4.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/random.h"


/*
 * A plugin interface for RNGs
 *
 * Using this interface, it is possible for the application to tell
 * libguile to use a different RNG.  This is desirable if it is
 * necessary to use the same RNG everywhere in the application in
 * order to prevent interference, if the application uses RNG
 * hardware, or if the application has special demands on the RNG.
 *
 * Look in random.h and how the default generator is "plugged in" in
 * scm_init_random().
 */

scm_t_rng scm_the_rng;


/*
 * The prepackaged RNG
 *
 * This is the MWC (Multiply With Carry) random number generator
 * described by George Marsaglia at the Department of Statistics and
 * Supercomputer Computations Research Institute, The Florida State
 * University (http://stat.fsu.edu/~geo).
 *
 * It uses 64 bits, has a period of 4578426017172946943 (4.6e18), and
 * passes all tests in the DIEHARD test suite
 * (http://stat.fsu.edu/~geo/diehard.html)
 */

#define A 2131995753UL

#ifndef M_PI
#define M_PI 3.14159265359
#endif

unsigned long
scm_i_uniform32 (scm_t_i_rstate *state)
{
  scm_t_uint64 x = (scm_t_uint64) A * state->w + state->c;
  scm_t_uint32 w = x & 0xffffffffUL;
  state->w = w;
  state->c = x >> 32L;
  return w;
}

void
scm_i_init_rstate (scm_t_i_rstate *state, const char *seed, int n)
{
  scm_t_uint32 w = 0L;
  scm_t_uint32 c = 0L;
  int i, m;
  for (i = 0; i < n; ++i)
    {
      m = i % 8;
      if (m < 4)
	w += seed[i] << (8 * m);
      else
        c += seed[i] << (8 * (m - 4));
    }
  if ((w == 0 && c == 0) || (w == -1 && c == A - 1))
    ++c;
  state->w = w;
  state->c = c;
}

scm_t_i_rstate *
scm_i_copy_rstate (scm_t_i_rstate *state)
{
  scm_t_rstate *new_state = scm_malloc (scm_the_rng.rstate_size);
  return memcpy (new_state, state, scm_the_rng.rstate_size);
}


/*
 * Random number library functions
 */

scm_t_rstate *
scm_c_make_rstate (const char *seed, int n)
{
  scm_t_rstate *state = scm_malloc (scm_the_rng.rstate_size);
  state->reserved0 = 0;
  scm_the_rng.init_rstate (state, seed, n);
  return state;
}


scm_t_rstate *
scm_c_default_rstate ()
#define FUNC_NAME "scm_c_default_rstate"
{
  SCM state = SCM_VARIABLE_REF (scm_var_random_state);
  if (!SCM_RSTATEP (state))
    SCM_MISC_ERROR ("*random-state* contains bogus random state", SCM_EOL);
  return SCM_RSTATE (state);
}
#undef FUNC_NAME


inline double
scm_c_uniform01 (scm_t_rstate *state)
{
  double x = (double) scm_the_rng.random_bits (state) / (double) 0xffffffffUL;
  return ((x + (double) scm_the_rng.random_bits (state))
	  / (double) 0xffffffffUL);
}

double
scm_c_normal01 (scm_t_rstate *state)
{
  if (state->reserved0)
    {
      state->reserved0 = 0;
      return state->reserved1;
    }
  else
    {
      double r, a, n;
      
      r = sqrt (-2.0 * log (scm_c_uniform01 (state)));
      a = 2.0 * M_PI * scm_c_uniform01 (state);
      
      n = r * sin (a);
      state->reserved1 = r * cos (a);
      state->reserved0 = 1;
      
      return n;
    }
}

double
scm_c_exp1 (scm_t_rstate *state)
{
  return - log (scm_c_uniform01 (state));
}

unsigned char scm_masktab[256];

static inline scm_t_uint32
scm_i_mask32 (scm_t_uint32 m)
{
  return (m < 0x100
	  ? scm_masktab[m]
	  : (m < 0x10000
	     ? scm_masktab[m >> 8] << 8 | 0xff
	     : (m < 0x1000000
		? scm_masktab[m >> 16] << 16 | 0xffff
		: scm_masktab[m >> 24] << 24 | 0xffffff)));
}

static scm_t_uint32
scm_c_random32 (scm_t_rstate *state, scm_t_uint32 m)
{
  scm_t_uint32 r, mask = scm_i_mask32 (m);
  while ((r = scm_the_rng.random_bits (state) & mask) >= m);
  return r;
}

/* Returns 32 random bits. */
unsigned long
scm_c_random (scm_t_rstate *state, unsigned long m)
{
  return scm_c_random32 (state, (scm_t_uint32)m);
}

scm_t_uint64
scm_c_random64 (scm_t_rstate *state, scm_t_uint64 m)
{
  scm_t_uint64 r;
  scm_t_uint32 mask;

  if (m <= SCM_T_UINT32_MAX)
    return scm_c_random32 (state, (scm_t_uint32) m);
  
  mask = scm_i_mask32 (m >> 32);
  while ((r = ((scm_t_uint64) (scm_the_rng.random_bits (state) & mask) << 32)
          | scm_the_rng.random_bits (state)) >= m)
    ;
  return r;
}

/*
  SCM scm_c_random_bignum (scm_t_rstate *state, SCM m)

  Takes a random state (source of random bits) and a bignum m.
  Returns a bignum b, 0 <= b < m.

  It does this by allocating a bignum b with as many base 65536 digits
  as m, filling b with random bits (in 32 bit chunks) up to the most
  significant 1 in m, and, finally checking if the resultant b is too
  large (>= m).  If too large, we simply repeat the process again.  (It
  is important to throw away all generated random bits if b >= m,
  otherwise we'll end up with a distorted distribution.)

*/

SCM
scm_c_random_bignum (scm_t_rstate *state, SCM m)
{
  SCM result = scm_i_mkbig ();
  const size_t m_bits = mpz_sizeinbase (SCM_I_BIG_MPZ (m), 2);
  /* how many bits would only partially fill the last u32? */
  const size_t end_bits = m_bits % (sizeof (scm_t_uint32) * SCM_CHAR_BIT);
  scm_t_uint32 *random_chunks = NULL;
  const scm_t_uint32 num_full_chunks =
    m_bits / (sizeof (scm_t_uint32) * SCM_CHAR_BIT);
  const scm_t_uint32 num_chunks = num_full_chunks + ((end_bits) ? 1 : 0);

  /* we know the result will be this big */
  mpz_realloc2 (SCM_I_BIG_MPZ (result), m_bits);

  random_chunks =
    (scm_t_uint32 *) scm_gc_calloc (num_chunks * sizeof (scm_t_uint32),
                                     "random bignum chunks");

  do
    {
      scm_t_uint32 *current_chunk = random_chunks + (num_chunks - 1);
      scm_t_uint32 chunks_left = num_chunks;

      mpz_set_ui (SCM_I_BIG_MPZ (result), 0);
      
      if (end_bits)
        {
          /* generate a mask with ones in the end_bits position, i.e. if
             end_bits is 3, then we'd have a mask of ...0000000111 */
          const unsigned long rndbits = scm_the_rng.random_bits (state);
          int rshift = (sizeof (scm_t_uint32) * SCM_CHAR_BIT) - end_bits;
          scm_t_uint32 mask = 0xffffffff >> rshift;
          scm_t_uint32 highest_bits = ((scm_t_uint32) rndbits) & mask;
          *current_chunk-- = highest_bits;
          chunks_left--;
        }
      
      while (chunks_left)
        {
          /* now fill in the remaining scm_t_uint32 sized chunks */
          *current_chunk-- = scm_the_rng.random_bits (state);
          chunks_left--;
        }
      mpz_import (SCM_I_BIG_MPZ (result),
                  num_chunks,
                  -1,
                  sizeof (scm_t_uint32),
                  0,
                  0,
                  random_chunks);
      /* if result >= m, regenerate it (it is important to regenerate
	 all bits in order not to get a distorted distribution) */
    } while (mpz_cmp (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (m)) >= 0);
  scm_gc_free (random_chunks,
               num_chunks * sizeof (scm_t_uint32),
               "random bignum chunks");
  return scm_i_normbig (result);
}

/*
 * Scheme level representation of random states.
 */
 
scm_t_bits scm_tc16_rstate;

static SCM
make_rstate (scm_t_rstate *state)
{
  SCM_RETURN_NEWSMOB (scm_tc16_rstate, state);
}

static size_t
rstate_free (SCM rstate)
{
  free (SCM_RSTATE (rstate));
  return 0;
}

/*
 * Scheme level interface.
 */

SCM_GLOBAL_VARIABLE_INIT (scm_var_random_state, "*random-state*", scm_seed_to_random_state (scm_from_locale_string ("URL:http://stat.fsu.edu/~geo/diehard.html")));

SCM_DEFINE (scm_random, "random", 1, 1, 0, 
            (SCM n, SCM state),
            "Return a number in [0, N).\n"
            "\n"
            "Accepts a positive integer or real n and returns a\n"
            "number of the same type between zero (inclusive) and\n"
            "N (exclusive). The values returned have a uniform\n"
            "distribution.\n"
            "\n"
            "The optional argument @var{state} must be of the type produced\n"
	    "by @code{seed->random-state}. It defaults to the value of the\n"
	    "variable @var{*random-state*}. This object is used to maintain\n"
	    "the state of the pseudo-random-number generator and is altered\n"
	    "as a side effect of the random operation.")
#define FUNC_NAME s_scm_random
{
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (2, state);
  if (SCM_I_INUMP (n))
    {
      unsigned long m = (unsigned long) SCM_I_INUM (n);
      SCM_ASSERT_RANGE (1, n, SCM_I_INUM (n) > 0);
#if SCM_SIZEOF_UNSIGNED_LONG <= 4
      return scm_from_uint32 (scm_c_random (SCM_RSTATE (state),
                                            (scm_t_uint32) m));
#elif SCM_SIZEOF_UNSIGNED_LONG <= 8
      return scm_from_uint64 (scm_c_random64 (SCM_RSTATE (state),
                                              (scm_t_uint64) m));
#else
#error "Cannot deal with this platform's unsigned long size"
#endif
    }
  SCM_VALIDATE_NIM (1, n);
  if (SCM_REALP (n))
    return scm_from_double (SCM_REAL_VALUE (n)
			    * scm_c_uniform01 (SCM_RSTATE (state)));

  if (!SCM_BIGP (n))
    SCM_WRONG_TYPE_ARG (1, n);
  return scm_c_random_bignum (SCM_RSTATE (state), n);
}
#undef FUNC_NAME

SCM_DEFINE (scm_copy_random_state, "copy-random-state", 0, 1, 0, 
            (SCM state),
            "Return a copy of the random state @var{state}.")
#define FUNC_NAME s_scm_copy_random_state
{
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (1, state);
  return make_rstate (scm_the_rng.copy_rstate (SCM_RSTATE (state)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_seed_to_random_state, "seed->random-state", 1, 0, 0, 
            (SCM seed),
            "Return a new random state using @var{seed}.")
#define FUNC_NAME s_scm_seed_to_random_state
{
  SCM res;
  if (SCM_NUMBERP (seed))
    seed = scm_number_to_string (seed, SCM_UNDEFINED);
  SCM_VALIDATE_STRING (1, seed);
  res = make_rstate (scm_c_make_rstate (scm_i_string_chars (seed),
					scm_i_string_length (seed)));
  scm_remember_upto_here_1 (seed);
  return res;
  
}
#undef FUNC_NAME

SCM_DEFINE (scm_random_uniform, "random:uniform", 0, 1, 0, 
            (SCM state),
	    "Return a uniformly distributed inexact real random number in\n"
	    "[0,1).")
#define FUNC_NAME s_scm_random_uniform
{
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (1, state);
  return scm_from_double (scm_c_uniform01 (SCM_RSTATE (state)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_random_normal, "random:normal", 0, 1, 0, 
            (SCM state),
	    "Return an inexact real in a normal distribution.  The\n"
	    "distribution used has mean 0 and standard deviation 1.  For a\n"
	    "normal distribution with mean m and standard deviation d use\n"
	    "@code{(+ m (* d (random:normal)))}.")
#define FUNC_NAME s_scm_random_normal
{
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (1, state);
  return scm_from_double (scm_c_normal01 (SCM_RSTATE (state)));
}
#undef FUNC_NAME

static void
vector_scale_x (SCM v, double c)
{
  size_t n;
  if (scm_is_simple_vector (v))
    {
      n = SCM_SIMPLE_VECTOR_LENGTH (v);
      while (n-- > 0)
	SCM_REAL_VALUE (SCM_SIMPLE_VECTOR_REF (v, n)) *= c;
    }
  else
    {
      /* must be a f64vector. */
      scm_t_array_handle handle;
      size_t i, len;
      ssize_t inc;
      double *elts;

      elts = scm_f64vector_writable_elements (v, &handle, &len, &inc);

      for (i = 0; i < len; i++, elts += inc)
	*elts *= c;
      
      scm_array_handle_release (&handle);
    }
}

static double
vector_sum_squares (SCM v)
{
  double x, sum = 0.0;
  size_t n;
  if (scm_is_simple_vector (v))
    {
      n = SCM_SIMPLE_VECTOR_LENGTH (v);
      while (n-- > 0)
	{
	  x = SCM_REAL_VALUE (SCM_SIMPLE_VECTOR_REF (v, n));
	  sum += x * x;
	}
    }
  else
    {
      /* must be a f64vector. */
      scm_t_array_handle handle;
      size_t i, len;
      ssize_t inc;
      const double *elts;

      elts = scm_f64vector_elements (v, &handle, &len, &inc);

      for (i = 0; i < len; i++, elts += inc)
	{
	  x = *elts;
	  sum += x * x;
	}

      scm_array_handle_release (&handle);
    }
  return sum;
}

/* For the uniform distribution on the solid sphere, note that in
 * this distribution the length r of the vector has cumulative
 * distribution r^n; i.e., u=r^n is uniform [0,1], so r can be
 * generated as r=u^(1/n).
 */
SCM_DEFINE (scm_random_solid_sphere_x, "random:solid-sphere!", 1, 1, 0, 
            (SCM v, SCM state),
	    "Fills @var{vect} with inexact real random numbers the sum of\n"
	    "whose squares is less than 1.0.  Thinking of @var{vect} as\n"
	    "coordinates in space of dimension @var{n} @math{=}\n"
	    "@code{(vector-length @var{vect})}, the coordinates are\n"
	    "uniformly distributed within the unit @var{n}-sphere.")
#define FUNC_NAME s_scm_random_solid_sphere_x
{
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (2, state);
  scm_random_normal_vector_x (v, state);
  vector_scale_x (v,
		  pow (scm_c_uniform01 (SCM_RSTATE (state)),
		       1.0 / scm_c_generalized_vector_length (v))
		  / sqrt (vector_sum_squares (v)));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_random_hollow_sphere_x, "random:hollow-sphere!", 1, 1, 0, 
            (SCM v, SCM state),
            "Fills vect with inexact real random numbers\n"
            "the sum of whose squares is equal to 1.0.\n"
            "Thinking of vect as coordinates in space of\n"
            "dimension n = (vector-length vect), the coordinates\n"
            "are uniformly distributed over the surface of the\n"
            "unit n-sphere.")
#define FUNC_NAME s_scm_random_hollow_sphere_x
{
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (2, state);
  scm_random_normal_vector_x (v, state);
  vector_scale_x (v, 1 / sqrt (vector_sum_squares (v)));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_random_normal_vector_x, "random:normal-vector!", 1, 1, 0, 
            (SCM v, SCM state),
            "Fills vect with inexact real random numbers that are\n"
            "independent and standard normally distributed\n"
            "(i.e., with mean 0 and variance 1).")
#define FUNC_NAME s_scm_random_normal_vector_x
{
  long i;
  scm_t_array_handle handle;
  scm_t_array_dim *dim;

  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (2, state);

  scm_generalized_vector_get_handle (v, &handle);
  dim = scm_array_handle_dims (&handle);

  if (scm_is_vector (v))
    {
      SCM *elts = scm_array_handle_writable_elements (&handle);
      for (i = dim->lbnd; i <= dim->ubnd; i++, elts += dim->inc)
	*elts = scm_from_double (scm_c_normal01 (SCM_RSTATE (state)));
    }
  else
    {
      /* must be a f64vector. */
      double *elts = scm_array_handle_f64_writable_elements (&handle);
      for (i = dim->lbnd; i <= dim->ubnd; i++, elts += dim->inc)
	*elts = scm_c_normal01 (SCM_RSTATE (state));
    }

  scm_array_handle_release (&handle);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_random_exp, "random:exp", 0, 1, 0, 
            (SCM state),
	    "Return an inexact real in an exponential distribution with mean\n"
	    "1.  For an exponential distribution with mean u use (* u\n"
	    "(random:exp)).")
#define FUNC_NAME s_scm_random_exp
{
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (1, state);
  return scm_from_double (scm_c_exp1 (SCM_RSTATE (state)));
}
#undef FUNC_NAME

void
scm_init_random ()
{
  int i, m;
  /* plug in default RNG */
  scm_t_rng rng =
  {
    sizeof (scm_t_i_rstate),
    (unsigned long (*)()) scm_i_uniform32,
    (void (*)())          scm_i_init_rstate,
    (scm_t_rstate *(*)())    scm_i_copy_rstate
  };
  scm_the_rng = rng;
  
  scm_tc16_rstate = scm_make_smob_type ("random-state", 0);
  scm_set_smob_free (scm_tc16_rstate, rstate_free);

  for (m = 1; m <= 0x100; m <<= 1)
    for (i = m >> 1; i < m; ++i)
      scm_masktab[i] = m - 1;
  
#include "libguile/random.x"

  scm_add_feature ("random");
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
