/* classes: h_files */

#ifndef SCM_RANDOM_H
#define SCM_RANDOM_H

/* Copyright (C) 1999,2000,2001, 2006, 2010 Free Software Foundation, Inc.
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



#include "libguile/__scm.h"


/*
 * A plugin interface for RNGs
 *
 * Using this interface, it is possible for the application to tell
 * libguile to use a different RNG.  This is desirable if it is
 * necessary to use the same RNG everywhere in the application in
 * order to prevent interference, if the application uses RNG
 * hardware, or if the application has special demands on the RNG.
 *
 * Look how the default generator is "plugged in" in scm_init_random().
 */

typedef struct scm_t_rstate {
  int reserved0;
  double reserved1;
  /* Custom fields follow here */
} scm_t_rstate;

typedef struct scm_t_rng {
  size_t rstate_size;				    /* size of random state */
  /* Though this returns an unsigned long, it's only 32 bits of randomness. */
  unsigned long (*random_bits) (scm_t_rstate *state); /* gives 32 random bits */
  void (*init_rstate) (scm_t_rstate *state, const char *seed, int n);
  scm_t_rstate *(*copy_rstate) (scm_t_rstate *state);
} scm_t_rng;

SCM_API scm_t_rng scm_the_rng;


/*
 * Default RNG
 */
typedef struct scm_t_i_rstate {
  scm_t_rstate rstate;
  unsigned long w;
  unsigned long c;
} scm_t_i_rstate;

/* Though this returns an unsigned long, it's only 32 bits of randomness. */
SCM_API unsigned long scm_i_uniform32 (scm_t_i_rstate *);
SCM_API void scm_i_init_rstate (scm_t_i_rstate *, const char *seed, int n);
SCM_API scm_t_i_rstate *scm_i_copy_rstate (scm_t_i_rstate *);


/*
 * Random number library functions
 */
SCM_API scm_t_rstate *scm_c_make_rstate (const char *, int);
SCM_API scm_t_rstate *scm_c_default_rstate (void);
#define scm_c_uniform32(RSTATE) scm_the_rng.random_bits (RSTATE)
SCM_API double scm_c_uniform01 (scm_t_rstate *);
SCM_API double scm_c_normal01 (scm_t_rstate *);
SCM_API double scm_c_exp1 (scm_t_rstate *);
/* Though this returns an unsigned long, it's only 32 bits of randomness. */
SCM_API unsigned long scm_c_random (scm_t_rstate *, unsigned long m);
/* This one returns 64 bits of randomness. */
SCM_API scm_t_uint64 scm_c_random64 (scm_t_rstate *state, scm_t_uint64 m);
SCM_API SCM scm_c_random_bignum (scm_t_rstate *, SCM m);


/*
 * Scheme level interface
 */
SCM_API scm_t_bits scm_tc16_rstate;
#define SCM_RSTATEP(obj) SCM_SMOB_PREDICATE (scm_tc16_rstate, obj)
#define SCM_RSTATE(obj)  ((scm_t_rstate *) SCM_SMOB_DATA (obj))

SCM_API unsigned char scm_masktab[256];

SCM_API SCM scm_var_random_state;
SCM_API SCM scm_random (SCM n, SCM state);
SCM_API SCM scm_copy_random_state (SCM state);
SCM_API SCM scm_seed_to_random_state (SCM seed);
SCM_API SCM scm_random_uniform (SCM state);
SCM_API SCM scm_random_solid_sphere_x (SCM v, SCM state);
SCM_API SCM scm_random_hollow_sphere_x (SCM v, SCM state);
SCM_API SCM scm_random_normal (SCM state);
SCM_API SCM scm_random_normal_vector_x (SCM v, SCM state);
SCM_API SCM scm_random_exp (SCM state);
SCM_API void scm_init_random (void);

#endif  /* SCM_RANDOM_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
