/* Copyright (C) 1996,1998,2000,2001,2004,2005, 2006, 2008 Free Software Foundation, Inc.
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


/*
  HWN:FIXME::
  Someone should rename this to arraymap.c; that would reflect the
  contents better.  */




#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/strings.h"
#include "libguile/unif.h"
#include "libguile/smob.h"
#include "libguile/chars.h"
#include "libguile/eq.h"
#include "libguile/eval.h"
#include "libguile/feature.h"
#include "libguile/root.h"
#include "libguile/vectors.h"
#include "libguile/srfi-4.h"
#include "libguile/dynwind.h"

#include "libguile/validate.h"
#include "libguile/ramap.h"


typedef struct
{
  char *name;
  SCM sproc;
  int (*vproc) ();
} ra_iproc;


/* These tables are a kluge that will not scale well when more
 * vectorized subrs are added.  It is tempting to steal some bits from
 * the SCM_CAR of all subrs (like those selected by SCM_SMOBNUM) to hold an
 * offset into a table of vectorized subrs.  
 */

static ra_iproc ra_rpsubrs[] =
{
  {"=", SCM_UNDEFINED, scm_ra_eqp},
  {"<", SCM_UNDEFINED, scm_ra_lessp},
  {"<=", SCM_UNDEFINED, scm_ra_leqp},
  {">", SCM_UNDEFINED, scm_ra_grp},
  {">=", SCM_UNDEFINED, scm_ra_greqp},
  {0, 0, 0}
};

static ra_iproc ra_asubrs[] =
{
  {"+", SCM_UNDEFINED, scm_ra_sum},
  {"-", SCM_UNDEFINED, scm_ra_difference},
  {"*", SCM_UNDEFINED, scm_ra_product},
  {"/", SCM_UNDEFINED, scm_ra_divide},
  {0, 0, 0}
};


#define GVREF scm_c_generalized_vector_ref
#define GVSET scm_c_generalized_vector_set_x

static unsigned long
cind (SCM ra, long *ve)
{
  unsigned long i;
  int k;
  if (!SCM_I_ARRAYP (ra))
    return *ve;
  i = SCM_I_ARRAY_BASE (ra);
  for (k = 0; k < SCM_I_ARRAY_NDIM (ra); k++)
    i += (ve[k] - SCM_I_ARRAY_DIMS (ra)[k].lbnd) * SCM_I_ARRAY_DIMS (ra)[k].inc;
  return i;
}


/* Checker for scm_array mapping functions:
   return values: 4 --> shapes, increments, and bases are the same;
   3 --> shapes and increments are the same;
   2 --> shapes are the same;
   1 --> ras are at least as big as ra0;
   0 --> no match.
   */

int 
scm_ra_matchp (SCM ra0, SCM ras)
{
  SCM ra1;
  scm_t_array_dim dims;
  scm_t_array_dim *s0 = &dims;
  scm_t_array_dim *s1;
  unsigned long bas0 = 0;
  int i, ndim = 1;
  int exact = 2	  /* 4 */ ;  /* Don't care about values >2 (yet?) */

  if (scm_is_generalized_vector (ra0))
    {
      s0->lbnd = 0;
      s0->inc = 1;
      s0->ubnd = scm_c_generalized_vector_length (ra0) - 1;
    }
  else if (SCM_I_ARRAYP (ra0))
    {
      ndim = SCM_I_ARRAY_NDIM (ra0);
      s0 = SCM_I_ARRAY_DIMS (ra0);
      bas0 = SCM_I_ARRAY_BASE (ra0);
    }
  else
    return 0;

  while (SCM_NIMP (ras))
    {
      ra1 = SCM_CAR (ras);
      
      if (scm_is_generalized_vector (ra1))
	{
	  size_t length;
	  
	  if (1 != ndim)
	    return 0;
	  
	  length = scm_c_generalized_vector_length (ra1);
	  
	  switch (exact)
	    {
	    case 4:
	      if (0 != bas0)
		exact = 3;
	    case 3:
	      if (1 != s0->inc)
		exact = 2;
	    case 2:
	      if ((0 == s0->lbnd) && (s0->ubnd == length - 1))
		break;
	      exact = 1;
	    case 1:
	      if (s0->lbnd < 0 || s0->ubnd >= length)
		return 0;
	    }
	}
      else if (SCM_I_ARRAYP (ra1) && ndim == SCM_I_ARRAY_NDIM (ra1))
	{
	  s1 = SCM_I_ARRAY_DIMS (ra1);
	  if (bas0 != SCM_I_ARRAY_BASE (ra1))
	    exact = 3;
	  for (i = 0; i < ndim; i++)
	    switch (exact)
	      {
	      case 4:
	      case 3:
		if (s0[i].inc != s1[i].inc)
		  exact = 2;
	      case 2:
		if (s0[i].lbnd == s1[i].lbnd && s0[i].ubnd == s1[i].ubnd)
		  break;
		exact = 1;
	      default:
		if (s0[i].lbnd < s1[i].lbnd || s0[i].ubnd > s1[i].ubnd)
		  return (s0[i].lbnd <= s0[i].ubnd ? 0 : 1);
	      }
	}
      else
	return 0;

      ras = SCM_CDR (ras);
    }

  return exact;
}

/* array mapper: apply cproc to each dimension of the given arrays?. 
     int (*cproc) ();   procedure to call on unrolled arrays?
			   cproc (dest, source list) or
			   cproc (dest, data, source list).  
     SCM data;          data to give to cproc or unbound. 
     SCM ra0;           destination array.
     SCM lra;           list of source arrays.
     const char *what;  caller, for error reporting. */
int 
scm_ramapc (int (*cproc)(), SCM data, SCM ra0, SCM lra, const char *what)
{
  SCM z;
  SCM vra0, ra1, vra1;
  SCM lvra, *plvra;
  long *vinds;
  int k, kmax;
  switch (scm_ra_matchp (ra0, lra))
    {
    default:
    case 0:
      scm_misc_error (what, "array shape mismatch: ~S", scm_list_1 (ra0));
    case 2:
    case 3:
    case 4:			/* Try unrolling arrays */
      kmax = (SCM_I_ARRAYP (ra0) ? SCM_I_ARRAY_NDIM (ra0) - 1 : 0);
      if (kmax < 0)
	goto gencase;
      vra0 = scm_array_contents (ra0, SCM_UNDEFINED);
      if (SCM_IMP (vra0)) goto gencase;
      if (!SCM_I_ARRAYP (vra0))
	{
	  size_t length = scm_c_generalized_vector_length (vra0);
	  vra1 = scm_i_make_ra (1, 0);
	  SCM_I_ARRAY_BASE (vra1) = 0;
	  SCM_I_ARRAY_DIMS (vra1)->lbnd = 0;
	  SCM_I_ARRAY_DIMS (vra1)->ubnd = length - 1;
	  SCM_I_ARRAY_DIMS (vra1)->inc = 1;
	  SCM_I_ARRAY_V (vra1) = vra0;
	  vra0 = vra1;
	}
      lvra = SCM_EOL;
      plvra = &lvra;
      for (z = lra; SCM_NIMP (z); z = SCM_CDR (z))
	{
	  ra1 = SCM_CAR (z);
	  vra1 = scm_i_make_ra (1, 0);
	  SCM_I_ARRAY_DIMS (vra1)->lbnd = SCM_I_ARRAY_DIMS (vra0)->lbnd;
	  SCM_I_ARRAY_DIMS (vra1)->ubnd = SCM_I_ARRAY_DIMS (vra0)->ubnd;
	  if (!SCM_I_ARRAYP (ra1))
	    {
	      SCM_I_ARRAY_BASE (vra1) = 0;
	      SCM_I_ARRAY_DIMS (vra1)->inc = 1;
	      SCM_I_ARRAY_V (vra1) = ra1;
	    }
	  else if (!SCM_I_ARRAY_CONTP (ra1))
	    goto gencase;
	  else
	    {
	      SCM_I_ARRAY_BASE (vra1) = SCM_I_ARRAY_BASE (ra1);
	      SCM_I_ARRAY_DIMS (vra1)->inc = SCM_I_ARRAY_DIMS (ra1)[kmax].inc;
	      SCM_I_ARRAY_V (vra1) = SCM_I_ARRAY_V (ra1);
	    }
	  *plvra = scm_cons (vra1, SCM_EOL);
	  plvra = SCM_CDRLOC (*plvra);
	}
      return (SCM_UNBNDP (data) ? cproc(vra0, lvra) : cproc(vra0, data, lvra));
    case 1:
    gencase:			/* Have to loop over all dimensions. */
    vra0 = scm_i_make_ra (1, 0);
    if (SCM_I_ARRAYP (ra0))
      {
	kmax = SCM_I_ARRAY_NDIM (ra0) - 1;
	if (kmax < 0)
	  {
	    SCM_I_ARRAY_DIMS (vra0)->lbnd = 0;
	    SCM_I_ARRAY_DIMS (vra0)->ubnd = 0;
	    SCM_I_ARRAY_DIMS (vra0)->inc = 1;
	  }
	else
	  {
	    SCM_I_ARRAY_DIMS (vra0)->lbnd = SCM_I_ARRAY_DIMS (ra0)[kmax].lbnd;
	    SCM_I_ARRAY_DIMS (vra0)->ubnd = SCM_I_ARRAY_DIMS (ra0)[kmax].ubnd;
	    SCM_I_ARRAY_DIMS (vra0)->inc = SCM_I_ARRAY_DIMS (ra0)[kmax].inc;
	  }
	SCM_I_ARRAY_BASE (vra0) = SCM_I_ARRAY_BASE (ra0);
	SCM_I_ARRAY_V (vra0) = SCM_I_ARRAY_V (ra0);
      }
    else
      {
	size_t length = scm_c_generalized_vector_length (ra0);
	kmax = 0;
	SCM_I_ARRAY_DIMS (vra0)->lbnd = 0;
	SCM_I_ARRAY_DIMS (vra0)->ubnd = length - 1;
	SCM_I_ARRAY_DIMS (vra0)->inc = 1;
	SCM_I_ARRAY_BASE (vra0) = 0;
	SCM_I_ARRAY_V (vra0) = ra0;
	ra0 = vra0;
      }
    lvra = SCM_EOL;
    plvra = &lvra;
    for (z = lra; SCM_NIMP (z); z = SCM_CDR (z))
      {
	ra1 = SCM_CAR (z);
	vra1 = scm_i_make_ra (1, 0);
	SCM_I_ARRAY_DIMS (vra1)->lbnd = SCM_I_ARRAY_DIMS (vra0)->lbnd;
	SCM_I_ARRAY_DIMS (vra1)->ubnd = SCM_I_ARRAY_DIMS (vra0)->ubnd;
	if (SCM_I_ARRAYP (ra1))
	  {
	    if (kmax >= 0)
	      SCM_I_ARRAY_DIMS (vra1)->inc = SCM_I_ARRAY_DIMS (ra1)[kmax].inc;
	    SCM_I_ARRAY_V (vra1) = SCM_I_ARRAY_V (ra1);
	  }
	else
	  {
	    SCM_I_ARRAY_DIMS (vra1)->inc = 1;
	    SCM_I_ARRAY_V (vra1) = ra1;
	  }
	*plvra = scm_cons (vra1, SCM_EOL);
	plvra = SCM_CDRLOC (*plvra);
      }

    scm_dynwind_begin (0);

    vinds = scm_malloc (sizeof(long) * SCM_I_ARRAY_NDIM (ra0));
    scm_dynwind_free (vinds);

    for (k = 0; k <= kmax; k++)
      vinds[k] = SCM_I_ARRAY_DIMS (ra0)[k].lbnd;
    k = kmax;
    do
      {
	if (k == kmax)
	  {
	    SCM y = lra;
	    SCM_I_ARRAY_BASE (vra0) = cind (ra0, vinds);
	    for (z = lvra; SCM_NIMP (z); z = SCM_CDR (z), y = SCM_CDR (y))
	      SCM_I_ARRAY_BASE (SCM_CAR (z)) = cind (SCM_CAR (y), vinds);
	    if (0 == (SCM_UNBNDP (data) ? cproc(vra0, lvra) : cproc(vra0, data, lvra)))
	      return 0;
	    k--;
	    continue;
	  }
	if (vinds[k] < SCM_I_ARRAY_DIMS (ra0)[k].ubnd)
	  {
	    vinds[k]++;
	    k++;
	    continue;
	  }
	vinds[k] = SCM_I_ARRAY_DIMS (ra0)[k].lbnd - 1;
	k--;
      }
    while (k >= 0);

    scm_dynwind_end ();
    return 1;
    }
}


SCM_DEFINE (scm_array_fill_x, "array-fill!", 2, 0, 0,
	    (SCM ra, SCM fill),
	    "Store @var{fill} in every element of @var{array}.  The value returned\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_array_fill_x
{
  scm_ramapc (scm_array_fill_int, fill, ra, SCM_EOL, FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* to be used as cproc in scm_ramapc to fill an array dimension with
   "fill". */
int 
scm_array_fill_int (SCM ra, SCM fill, SCM ignore SCM_UNUSED)
#define FUNC_NAME s_scm_array_fill_x
{
  unsigned long i;
  unsigned long n = SCM_I_ARRAY_DIMS (ra)->ubnd - SCM_I_ARRAY_DIMS (ra)->lbnd + 1;
  long inc = SCM_I_ARRAY_DIMS (ra)->inc;
  unsigned long base = SCM_I_ARRAY_BASE (ra);

  ra = SCM_I_ARRAY_V (ra);

  for (i = base; n--; i += inc)
    GVSET (ra, i, fill);

  return 1;
}
#undef FUNC_NAME



static int 
racp (SCM src, SCM dst)
{
  long n = (SCM_I_ARRAY_DIMS (src)->ubnd - SCM_I_ARRAY_DIMS (src)->lbnd + 1);
  long inc_d, inc_s = SCM_I_ARRAY_DIMS (src)->inc;
  unsigned long i_d, i_s = SCM_I_ARRAY_BASE (src);
  dst = SCM_CAR (dst);
  inc_d = SCM_I_ARRAY_DIMS (dst)->inc;
  i_d = SCM_I_ARRAY_BASE (dst);
  src = SCM_I_ARRAY_V (src);
  dst = SCM_I_ARRAY_V (dst);

  for (; n-- > 0; i_s += inc_s, i_d += inc_d)
    GVSET (dst, i_d, GVREF (src, i_s));
  return 1;
}

SCM_REGISTER_PROC(s_array_copy_in_order_x, "array-copy-in-order!", 2, 0, 0, scm_array_copy_x);


SCM_DEFINE (scm_array_copy_x, "array-copy!", 2, 0, 0,
	    (SCM src, SCM dst),
	    "@deffnx {Scheme Procedure} array-copy-in-order! src dst\n"
	    "Copy every element from vector or array @var{source} to the\n"
	    "corresponding element of @var{destination}.  @var{destination} must have\n"
	    "the same rank as @var{source}, and be at least as large in each\n"
	    "dimension.  The order is unspecified.")
#define FUNC_NAME s_scm_array_copy_x
{
  scm_ramapc (racp, SCM_UNDEFINED, src, scm_cons (dst, SCM_EOL), FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* Functions callable by ARRAY-MAP! */


int
scm_ra_eqp (SCM ra0, SCM ras)
{
  SCM ra1 = SCM_CAR (ras), ra2 = SCM_CAR (SCM_CDR (ras));
  scm_t_array_handle ra0_handle;
  scm_t_array_dim *ra0_dims;
  size_t n;
  ssize_t inc0;
  size_t i0 = 0;
  unsigned long i1 = SCM_I_ARRAY_BASE (ra1), i2 = SCM_I_ARRAY_BASE (ra2);
  long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
  long inc2 = SCM_I_ARRAY_DIMS (ra1)->inc;
  ra1 = SCM_I_ARRAY_V (ra1);
  ra2 = SCM_I_ARRAY_V (ra2);

  scm_array_get_handle (ra0, &ra0_handle);
  ra0_dims = scm_array_handle_dims (&ra0_handle);
  n = ra0_dims[0].ubnd - ra0_dims[0].lbnd + 1;
  inc0 = ra0_dims[0].inc;

  {
    for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
      if (scm_is_true (scm_array_handle_ref (&ra0_handle, i0)))
	if (!scm_is_eq (GVREF (ra1, i1), GVREF (ra2, i2)))
	  scm_array_handle_set (&ra0_handle, i0, SCM_BOOL_F);
  }

  scm_array_handle_release (&ra0_handle);
  return 1;
}

/* opt 0 means <, nonzero means >= */

static int
ra_compare (SCM ra0, SCM ra1, SCM ra2, int opt)
{
  scm_t_array_handle ra0_handle;
  scm_t_array_dim *ra0_dims;
  size_t n;
  ssize_t inc0;
  size_t i0 = 0;
  unsigned long i1 = SCM_I_ARRAY_BASE (ra1), i2 = SCM_I_ARRAY_BASE (ra2);
  long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
  long inc2 = SCM_I_ARRAY_DIMS (ra1)->inc;
  ra1 = SCM_I_ARRAY_V (ra1);
  ra2 = SCM_I_ARRAY_V (ra2);

  scm_array_get_handle (ra0, &ra0_handle);
  ra0_dims = scm_array_handle_dims (&ra0_handle);
  n = ra0_dims[0].ubnd - ra0_dims[0].lbnd + 1;
  inc0 = ra0_dims[0].inc;

  {
    for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
      if (scm_is_true (scm_array_handle_ref (&ra0_handle, i0)))
	if (opt ?
	    scm_is_true (scm_less_p (GVREF (ra1, i1), GVREF (ra2, i2))) :
	    scm_is_false (scm_less_p (GVREF (ra1, i1), GVREF (ra2, i2))))
	  scm_array_handle_set (&ra0_handle, i0, SCM_BOOL_F);
  }

  scm_array_handle_release (&ra0_handle);
  return 1;
}



int
scm_ra_lessp (SCM ra0, SCM ras)
{
  return ra_compare (ra0, SCM_CAR (ras), SCM_CAR (SCM_CDR (ras)), 0);
}


int
scm_ra_leqp (SCM ra0, SCM ras)
{
  return ra_compare (ra0, SCM_CAR (SCM_CDR (ras)), SCM_CAR (ras), 1);
}


int
scm_ra_grp (SCM ra0, SCM ras)
{
  return ra_compare (ra0, SCM_CAR (SCM_CDR (ras)), SCM_CAR (ras), 0);
}


int
scm_ra_greqp (SCM ra0, SCM ras)
{
  return ra_compare (ra0, SCM_CAR (ras), SCM_CAR (SCM_CDR (ras)), 1);
}


int
scm_ra_sum (SCM ra0, SCM ras)
{
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd - SCM_I_ARRAY_DIMS (ra0)->lbnd + 1;
  unsigned long i0 = SCM_I_ARRAY_BASE (ra0);
  long inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  ra0 = SCM_I_ARRAY_V (ra0);
  if (!scm_is_null(ras))
    {
      SCM ra1 = SCM_CAR (ras);
      unsigned long i1 = SCM_I_ARRAY_BASE (ra1);
      long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_I_ARRAY_V (ra1);
      switch (SCM_TYP7 (ra0) == SCM_TYP7 (ra1) ? SCM_TYP7 (ra0) : 0)
	{
	default:
	  {
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      GVSET (ra0, i0, scm_sum (GVREF(ra0, i0), GVREF(ra1, i1)));
	    break;
	  }
	}
    }
  return 1;
}



int
scm_ra_difference (SCM ra0, SCM ras)
{
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd - SCM_I_ARRAY_DIMS (ra0)->lbnd + 1;
  unsigned long i0 = SCM_I_ARRAY_BASE (ra0);
  long inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  ra0 = SCM_I_ARRAY_V (ra0);
  if (scm_is_null (ras))
    {
      switch (SCM_TYP7 (ra0))
	{
	default:
	  {
	    for (; n-- > 0; i0 += inc0)
	      GVSET (ra0, i0, scm_difference (GVREF(ra0, i0), SCM_UNDEFINED));
	    break;
	  }
	}
    }
  else
    {
      SCM ra1 = SCM_CAR (ras);
      unsigned long i1 = SCM_I_ARRAY_BASE (ra1);
      long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_I_ARRAY_V (ra1);
      switch (SCM_TYP7 (ra0) == SCM_TYP7 (ra1) ? SCM_TYP7 (ra0) : 0)
	{
	default:
	  {
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      GVSET (ra0, i0, scm_difference (GVREF (ra0, i0),
					      GVREF (ra1, i1)));
	    break;
	  }
	}
    }
  return 1;
}



int
scm_ra_product (SCM ra0, SCM ras)
{
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd - SCM_I_ARRAY_DIMS (ra0)->lbnd + 1;
  unsigned long i0 = SCM_I_ARRAY_BASE (ra0);
  long inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  ra0 = SCM_I_ARRAY_V (ra0);
  if (!scm_is_null (ras))
    {
      SCM ra1 = SCM_CAR (ras);
      unsigned long i1 = SCM_I_ARRAY_BASE (ra1);
      long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_I_ARRAY_V (ra1);
      switch (SCM_TYP7 (ra0) == SCM_TYP7 (ra1) ? SCM_TYP7 (ra0) : 0)
	{
	default:
	  {
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      GVSET (ra0, i0, scm_product (GVREF (ra0, i0),
					   GVREF (ra1, i1)));
	  }
	}
    }
  return 1;
}


int
scm_ra_divide (SCM ra0, SCM ras)
{
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd - SCM_I_ARRAY_DIMS (ra0)->lbnd + 1;
  unsigned long i0 = SCM_I_ARRAY_BASE (ra0);
  long inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  ra0 = SCM_I_ARRAY_V (ra0);
  if (scm_is_null (ras))
    {
      switch (SCM_TYP7 (ra0))
	{
	default:
	  {
	    for (; n-- > 0; i0 += inc0)
	      GVSET (ra0, i0, scm_divide (GVREF (ra0, i0), SCM_UNDEFINED));
	    break;
	  }
	}
    }
  else
    {
      SCM ra1 = SCM_CAR (ras);
      unsigned long i1 = SCM_I_ARRAY_BASE (ra1);
      long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_I_ARRAY_V (ra1);
      switch (SCM_TYP7 (ra0) == SCM_TYP7 (ra1) ? SCM_TYP7 (ra0) : 0)
	{
	default:
	  {
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      {
		SCM res =  scm_divide (GVREF (ra0, i0),
				       GVREF (ra1, i1));
		GVSET (ra0, i0, res);
	      }
	    break;
	  }
	}
    }
  return 1;
}


int
scm_array_identity (SCM dst, SCM src)
{
  return racp (SCM_CAR (src), scm_cons (dst, SCM_EOL));
}



static int 
ramap (SCM ra0, SCM proc, SCM ras)
{
  long i = SCM_I_ARRAY_DIMS (ra0)->lbnd;
  long inc = SCM_I_ARRAY_DIMS (ra0)->inc;
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd;
  long base = SCM_I_ARRAY_BASE (ra0) - i * inc;
  ra0 = SCM_I_ARRAY_V (ra0);
  if (scm_is_null (ras))
    for (; i <= n; i++)
      GVSET (ra0, i*inc+base, scm_call_0 (proc));
  else
    {
      SCM ra1 = SCM_CAR (ras);
      SCM args;
      unsigned long k, i1 = SCM_I_ARRAY_BASE (ra1);
      long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_I_ARRAY_V (ra1);
      ras = SCM_CDR (ras);
      if (scm_is_null(ras))
	ras = scm_nullvect;
      else
	ras = scm_vector (ras);
      
      for (; i <= n; i++, i1 += inc1)
	{
	  args = SCM_EOL;
	  for (k = scm_c_vector_length (ras); k--;)
	    args = scm_cons (GVREF (scm_c_vector_ref (ras, k), i), args);
	  args = scm_cons (GVREF (ra1, i1), args);
	  GVSET (ra0, i*inc+base, scm_apply_0 (proc, args));
	}
    }
  return 1;
}


static int
ramap_dsubr (SCM ra0, SCM proc, SCM ras)
{
  SCM ra1 = SCM_CAR (ras);
  unsigned long i0 = SCM_I_ARRAY_BASE (ra0), i1 = SCM_I_ARRAY_BASE (ra1);
  long inc0 = SCM_I_ARRAY_DIMS (ra0)->inc, inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd - SCM_I_ARRAY_DIMS (ra1)->lbnd + 1;
  ra0 = SCM_I_ARRAY_V (ra0);
  ra1 = SCM_I_ARRAY_V (ra1);
  switch (SCM_TYP7 (ra0))
    {
    default:
      for (; n-- > 0; i0 += inc0, i1 += inc1)
	GVSET (ra0, i0, scm_call_1 (proc, GVREF (ra1, i1)));
      break;
    }
  return 1;
}



static int
ramap_rp (SCM ra0, SCM proc, SCM ras)
{
  SCM ra1 = SCM_CAR (ras), ra2 = SCM_CAR (SCM_CDR (ras));
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd - SCM_I_ARRAY_DIMS (ra0)->lbnd + 1;
  unsigned long i0 = SCM_I_ARRAY_BASE (ra0), i1 = SCM_I_ARRAY_BASE (ra1), i2 = SCM_I_ARRAY_BASE (ra2);
  long inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
  long inc2 = SCM_I_ARRAY_DIMS (ra1)->inc;
  ra0 = SCM_I_ARRAY_V (ra0);
  ra1 = SCM_I_ARRAY_V (ra1);
  ra2 = SCM_I_ARRAY_V (ra2);

  for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
    if (scm_is_true (scm_c_bitvector_ref (ra0, i0)))
      if (scm_is_false (SCM_SUBRF (proc) (GVREF (ra1, i1), GVREF (ra2, i2))))
	scm_c_bitvector_set_x (ra0, i0, SCM_BOOL_F);

  return 1;
}



static int
ramap_1 (SCM ra0, SCM proc, SCM ras)
{
  SCM ra1 = SCM_CAR (ras);
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd - SCM_I_ARRAY_DIMS (ra0)->lbnd + 1;
  unsigned long i0 = SCM_I_ARRAY_BASE (ra0), i1 = SCM_I_ARRAY_BASE (ra1);
  long inc0 = SCM_I_ARRAY_DIMS (ra0)->inc, inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
  ra0 = SCM_I_ARRAY_V (ra0);
  ra1 = SCM_I_ARRAY_V (ra1);
  if (scm_tc7_vector == SCM_TYP7 (ra0) || scm_tc7_wvect == SCM_TYP7 (ra0))
    for (; n-- > 0; i0 += inc0, i1 += inc1)
      GVSET (ra0, i0, SCM_SUBRF (proc) (GVREF (ra1, i1)));
  else
    for (; n-- > 0; i0 += inc0, i1 += inc1)
      GVSET (ra0, i0, SCM_SUBRF (proc) (GVREF (ra1, i1)));
  return 1;
}



static int
ramap_2o (SCM ra0, SCM proc, SCM ras)
{
  SCM ra1 = SCM_CAR (ras);
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd - SCM_I_ARRAY_DIMS (ra0)->lbnd + 1;
  unsigned long i0 = SCM_I_ARRAY_BASE (ra0), i1 = SCM_I_ARRAY_BASE (ra1);
  long inc0 = SCM_I_ARRAY_DIMS (ra0)->inc, inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
  ra0 = SCM_I_ARRAY_V (ra0);
  ra1 = SCM_I_ARRAY_V (ra1);
  ras = SCM_CDR (ras);
  if (scm_is_null (ras))
    {
      for (; n-- > 0; i0 += inc0, i1 += inc1)
	GVSET (ra0, i0, SCM_SUBRF (proc) (GVREF (ra1, i1), SCM_UNDEFINED));
    }
  else
    {
      SCM ra2 = SCM_CAR (ras);
      unsigned long i2 = SCM_I_ARRAY_BASE (ra2);
      long inc2 = SCM_I_ARRAY_DIMS (ra2)->inc;
      ra2 = SCM_I_ARRAY_V (ra2);
      for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
	GVSET (ra0, i0, SCM_SUBRF (proc) (GVREF (ra1, i1), GVREF (ra2, i2)));
    }
  return 1;
}



static int
ramap_a (SCM ra0, SCM proc, SCM ras)
{
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd - SCM_I_ARRAY_DIMS (ra0)->lbnd + 1;
  unsigned long i0 = SCM_I_ARRAY_BASE (ra0);
  long inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  ra0 = SCM_I_ARRAY_V (ra0);
  if (scm_is_null (ras))
    for (; n-- > 0; i0 += inc0)
      GVSET (ra0, i0, SCM_SUBRF (proc) (GVREF (ra0, i0), SCM_UNDEFINED));
  else
    {
      SCM ra1 = SCM_CAR (ras);
      unsigned long i1 = SCM_I_ARRAY_BASE (ra1);
      long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_I_ARRAY_V (ra1);
      for (; n-- > 0; i0 += inc0, i1 += inc1)
	GVSET (ra0, i0, SCM_SUBRF (proc) (GVREF (ra0, i0), GVREF (ra1, i1)));
    }
  return 1;
}


SCM_REGISTER_PROC(s_array_map_in_order_x, "array-map-in-order!", 2, 0, 1, scm_array_map_x);

SCM_SYMBOL (sym_b, "b");

SCM_DEFINE (scm_array_map_x, "array-map!", 2, 0, 1,
	    (SCM ra0, SCM proc, SCM lra),
	    "@deffnx {Scheme Procedure} array-map-in-order! ra0 proc . lra\n"
	    "@var{array1}, @dots{} must have the same number of dimensions as\n"
	    "@var{array0} and have a range for each index which includes the range\n"
	    "for the corresponding index in @var{array0}.  @var{proc} is applied to\n"
	    "each tuple of elements of @var{array1} @dots{} and the result is stored\n"
	    "as the corresponding element in @var{array0}.  The value returned is\n"
	    "unspecified.  The order of application is unspecified.")
#define FUNC_NAME s_scm_array_map_x
{
  SCM_VALIDATE_PROC (2, proc);
  SCM_VALIDATE_REST_ARGUMENT (lra);

  switch (SCM_TYP7 (proc))
    {
    default:
    gencase:
 scm_ramapc (ramap, proc, ra0, lra, FUNC_NAME);
 return SCM_UNSPECIFIED;
    case scm_tc7_subr_1:
      if (! scm_is_pair (lra))
        SCM_WRONG_NUM_ARGS ();  /* need 1 source */
      scm_ramapc (ramap_1, proc, ra0, lra, FUNC_NAME);
      return SCM_UNSPECIFIED;
    case scm_tc7_subr_2:
      if (! (scm_is_pair (lra) && scm_is_pair (SCM_CDR (lra))))
        SCM_WRONG_NUM_ARGS ();  /* need 2 sources */
      goto subr_2o;
    case scm_tc7_subr_2o:
      if (! scm_is_pair (lra))
        SCM_WRONG_NUM_ARGS ();  /* need 1 source */
    subr_2o:
      scm_ramapc (ramap_2o, proc, ra0, lra, FUNC_NAME);
      return SCM_UNSPECIFIED;
    case scm_tc7_dsubr:
      if (! scm_is_pair (lra))
        SCM_WRONG_NUM_ARGS ();  /* need 1 source */
      scm_ramapc (ramap_dsubr, proc, ra0, lra, FUNC_NAME);
      return SCM_UNSPECIFIED;
    case scm_tc7_rpsubr:
      {
	ra_iproc *p;
	if (!scm_is_typed_array (ra0, sym_b))
	  goto gencase;
	scm_array_fill_x (ra0, SCM_BOOL_T);
	for (p = ra_rpsubrs; p->name; p++)
	  if (scm_is_eq (proc, p->sproc))
	    {
	      while (!scm_is_null (lra) && !scm_is_null (SCM_CDR (lra)))
		{
		  scm_ramapc (p->vproc, SCM_UNDEFINED, ra0, lra, FUNC_NAME);
		  lra = SCM_CDR (lra);
		}
	      return SCM_UNSPECIFIED;
	    }
	while (!scm_is_null (lra) && !scm_is_null (SCM_CDR (lra)))
	  {
	    scm_ramapc (ramap_rp, proc, ra0, lra, FUNC_NAME);
	    lra = SCM_CDR (lra);
	  }
	return SCM_UNSPECIFIED;
      }
    case scm_tc7_asubr:
      if (scm_is_null (lra))
	{
	  SCM fill = SCM_SUBRF (proc) (SCM_UNDEFINED, SCM_UNDEFINED);
	  scm_array_fill_x (ra0, fill);
	}
      else
	{
	  SCM tail, ra1 = SCM_CAR (lra);
	  SCM v0 = (SCM_I_ARRAYP (ra0) ? SCM_I_ARRAY_V (ra0) : ra0);
	  ra_iproc *p;
	  /* Check to see if order might matter.
	     This might be an argument for a separate
	     SERIAL-ARRAY-MAP! */
	  if (scm_is_eq (v0, ra1) 
	      || (SCM_I_ARRAYP (ra1) && scm_is_eq (v0, SCM_I_ARRAY_V (ra1))))
	    if (!scm_is_eq (ra0, ra1) 
		|| (SCM_I_ARRAYP(ra0) && !SCM_I_ARRAY_CONTP(ra0)))
	      goto gencase;
	  for (tail = SCM_CDR (lra); !scm_is_null (tail); tail = SCM_CDR (tail))
	    {
	      ra1 = SCM_CAR (tail);
	      if (scm_is_eq (v0, ra1) 
		  || (SCM_I_ARRAYP (ra1) && scm_is_eq (v0, SCM_I_ARRAY_V (ra1))))
		goto gencase;
	    }
	  for (p = ra_asubrs; p->name; p++)
	    if (scm_is_eq (proc, p->sproc))
	      {
		if (!scm_is_eq (ra0, SCM_CAR (lra)))
		  scm_ramapc (scm_array_identity, SCM_UNDEFINED, ra0, scm_cons (SCM_CAR (lra), SCM_EOL), FUNC_NAME);
		lra = SCM_CDR (lra);
		while (1)
		  {
		    scm_ramapc (p->vproc, SCM_UNDEFINED, ra0, lra, FUNC_NAME);
		    if (SCM_IMP (lra) || SCM_IMP (SCM_CDR (lra)))
		      return SCM_UNSPECIFIED;
		    lra = SCM_CDR (lra);
		  }
	      }
	  scm_ramapc (ramap_2o, proc, ra0, lra, FUNC_NAME);
	  lra = SCM_CDR (lra);
	  if (SCM_NIMP (lra))
	    for (lra = SCM_CDR (lra); SCM_NIMP (lra); lra = SCM_CDR (lra))
	      scm_ramapc (ramap_a, proc, ra0, lra, FUNC_NAME);
	}
      return SCM_UNSPECIFIED;
    }
}
#undef FUNC_NAME


static int
rafe (SCM ra0, SCM proc, SCM ras)
{
  long i = SCM_I_ARRAY_DIMS (ra0)->lbnd;
  unsigned long i0 = SCM_I_ARRAY_BASE (ra0);
  long inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd;
  ra0 = SCM_I_ARRAY_V (ra0);
  if (scm_is_null (ras))
    for (; i <= n; i++, i0 += inc0)
      scm_call_1 (proc, GVREF (ra0, i0));
  else
    {
      SCM ra1 = SCM_CAR (ras);
      SCM args;
      unsigned long k, i1 = SCM_I_ARRAY_BASE (ra1);
      long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_I_ARRAY_V (ra1);
      ras = SCM_CDR (ras);
      if (scm_is_null(ras))
	ras = scm_nullvect;
      else
	ras = scm_vector (ras);
      for (; i <= n; i++, i0 += inc0, i1 += inc1)
	{
	  args = SCM_EOL;
	  for (k = scm_c_vector_length (ras); k--;)
	    args = scm_cons (GVREF (scm_c_vector_ref (ras, k), i), args);
	  args = scm_cons2 (GVREF (ra0, i0), GVREF (ra1, i1), args);
	  scm_apply_0 (proc, args);
	}
    }
  return 1;
}


SCM_DEFINE (scm_array_for_each, "array-for-each", 2, 0, 1,
	    (SCM proc, SCM ra0, SCM lra),
	    "Apply @var{proc} to each tuple of elements of @var{array0} @dots{}\n"
	    "in row-major order.  The value returned is unspecified.")
#define FUNC_NAME s_scm_array_for_each
{
  SCM_VALIDATE_PROC (1, proc);
  SCM_VALIDATE_REST_ARGUMENT (lra);
  scm_ramapc (rafe, proc, ra0, lra, FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_array_index_map_x, "array-index-map!", 2, 0, 0,
	    (SCM ra, SCM proc),
	    "Apply @var{proc} to the indices of each element of @var{array} in\n"
	    "turn, storing the result in the corresponding element.  The value\n"
	    "returned and the order of application are unspecified.\n\n"
	    "One can implement @var{array-indexes} as\n"
	    "@lisp\n"
	    "(define (array-indexes array)\n"
	    "    (let ((ra (apply make-array #f (array-shape array))))\n"
	    "      (array-index-map! ra (lambda x x))\n"
	    "      ra))\n"
	    "@end lisp\n"
	    "Another example:\n"
	    "@lisp\n"
	    "(define (apl:index-generator n)\n"
	    "    (let ((v (make-uniform-vector n 1)))\n"
	    "      (array-index-map! v (lambda (i) i))\n"
	    "      v))\n"
	    "@end lisp")
#define FUNC_NAME s_scm_array_index_map_x
{
  unsigned long i;
  SCM_VALIDATE_PROC (2, proc);

  if (SCM_I_ARRAYP (ra))
    {
      SCM args = SCM_EOL;
      int j, k, kmax = SCM_I_ARRAY_NDIM (ra) - 1;
      long *vinds;

      if (kmax < 0)
	return scm_array_set_x (ra, scm_call_0 (proc), SCM_EOL);

      scm_dynwind_begin (0);

      vinds = scm_malloc (sizeof(long) * SCM_I_ARRAY_NDIM (ra));
      scm_dynwind_free (vinds);

      for (k = 0; k <= kmax; k++)
	vinds[k] = SCM_I_ARRAY_DIMS (ra)[k].lbnd;
      k = kmax;
      do
	{
	  if (k == kmax)
	    {
	      vinds[k] = SCM_I_ARRAY_DIMS (ra)[k].lbnd;
	      i = cind (ra, vinds);
	      for (; vinds[k] <= SCM_I_ARRAY_DIMS (ra)[k].ubnd; vinds[k]++)
		{
		  for (j = kmax + 1, args = SCM_EOL; j--;)
		    args = scm_cons (scm_from_long (vinds[j]), args);
		  GVSET (SCM_I_ARRAY_V (ra), i, scm_apply_0 (proc, args));
		  i += SCM_I_ARRAY_DIMS (ra)[k].inc;
		}
	      k--;
	      continue;
	    }
	  if (vinds[k] < SCM_I_ARRAY_DIMS (ra)[k].ubnd)
	    {
	      vinds[k]++;
	      k++;
	      continue;
	    }
	  vinds[k] = SCM_I_ARRAY_DIMS (ra)[k].lbnd - 1;
	  k--;
	}
      while (k >= 0);

      scm_dynwind_end ();
      return SCM_UNSPECIFIED;
    }
  else if (scm_is_generalized_vector (ra))
    {
      size_t length = scm_c_generalized_vector_length (ra);
      for (i = 0; i < length; i++)
	GVSET (ra, i, scm_call_1 (proc, scm_from_ulong (i)));
      return SCM_UNSPECIFIED;
    }
  else 
    scm_wrong_type_arg_msg (NULL, 0, ra, "array");
}
#undef FUNC_NAME


static int
raeql_1 (SCM ra0, SCM as_equal, SCM ra1)
{
  unsigned long i0 = 0, i1 = 0;
  long inc0 = 1, inc1 = 1;
  unsigned long n;
  ra1 = SCM_CAR (ra1);
  if (SCM_I_ARRAYP(ra0))
    {
      n = SCM_I_ARRAY_DIMS (ra0)->ubnd - SCM_I_ARRAY_DIMS (ra0)->lbnd + 1;
      i0 = SCM_I_ARRAY_BASE (ra0);
      inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
      ra0 = SCM_I_ARRAY_V (ra0);
    }
  else
    n = scm_c_generalized_vector_length (ra0);

  if (SCM_I_ARRAYP (ra1))
    {
      i1 = SCM_I_ARRAY_BASE (ra1);
      inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_I_ARRAY_V (ra1);
    }

  if (scm_is_generalized_vector (ra0))
    {
      for (; n--; i0 += inc0, i1 += inc1)
	{
	  if (scm_is_false (as_equal))
	    {
	      if (scm_is_false (scm_array_equal_p (GVREF (ra0, i0), GVREF (ra1, i1))))
		return 0;
	    }
	  else if (scm_is_false (scm_equal_p (GVREF (ra0, i0), GVREF (ra1, i1))))
	    return 0;
	}
      return 1;
    }
  else
    return 0;
}



static int
raeql (SCM ra0, SCM as_equal, SCM ra1)
{
  SCM v0 = ra0, v1 = ra1;
  scm_t_array_dim dim0, dim1;
  scm_t_array_dim *s0 = &dim0, *s1 = &dim1;
  unsigned long bas0 = 0, bas1 = 0;
  int k, unroll = 1, vlen = 1, ndim = 1;
  if (SCM_I_ARRAYP (ra0))
    {
      ndim = SCM_I_ARRAY_NDIM (ra0);
      s0 = SCM_I_ARRAY_DIMS (ra0);
      bas0 = SCM_I_ARRAY_BASE (ra0);
      v0 = SCM_I_ARRAY_V (ra0);
    }
  else
    {
      s0->inc = 1;
      s0->lbnd = 0;
      s0->ubnd = scm_c_generalized_vector_length (v0) - 1;
      unroll = 0;
    }
  if (SCM_I_ARRAYP (ra1))
    {
      if (ndim != SCM_I_ARRAY_NDIM (ra1))
	return 0;
      s1 = SCM_I_ARRAY_DIMS (ra1);
      bas1 = SCM_I_ARRAY_BASE (ra1);
      v1 = SCM_I_ARRAY_V (ra1);
    }
  else
    {
      /*
	Huh ? Schizophrenic return type. --hwn
      */
      if (1 != ndim)
	return 0;
      s1->inc = 1;
      s1->lbnd = 0;
      s1->ubnd = scm_c_generalized_vector_length (v1) - 1;
      unroll = 0;
    }
  if (SCM_TYP7 (v0) != SCM_TYP7 (v1))
    return 0;
  for (k = ndim; k--;)
    {
      if (s0[k].lbnd != s1[k].lbnd || s0[k].ubnd != s1[k].ubnd)
	return 0;
      if (unroll)
	{
	  unroll = (s0[k].inc == s1[k].inc);
	  vlen *= s0[k].ubnd - s1[k].lbnd + 1;
	}
    }
  if (unroll && bas0 == bas1 && scm_is_eq (v0, v1))
    return 1;
  return scm_ramapc (raeql_1, as_equal, ra0, scm_cons (ra1, SCM_EOL), "");
}


SCM
scm_raequal (SCM ra0, SCM ra1)
{
  return scm_from_bool(raeql (ra0, SCM_BOOL_T, ra1));
}

#if 0
/* GJB:FIXME:: Why not use SCM_DEFINE1 for array-equal? */
SCM_DEFINE1 (scm_array_equal_p, "array-equal?", scm_tc7_rpsubr,
	     (SCM ra0, SCM ra1),
	    "Return @code{#t} iff all arguments are arrays with the same\n"
	    "shape, the same type, and have corresponding elements which are\n"
	    "either @code{equal?}  or @code{array-equal?}.  This function\n"
	    "differs from @code{equal?} in that a one dimensional shared\n"
	    "array may be @var{array-equal?} but not @var{equal?} to a\n"
	    "vector or uniform vector.")
#define FUNC_NAME s_scm_array_equal_p
{
}
#undef FUNC_NAME
#endif

static char s_array_equal_p[] = "array-equal?";


SCM
scm_array_equal_p (SCM ra0, SCM ra1)
{
  if (SCM_I_ARRAYP (ra0) || SCM_I_ARRAYP (ra1))
    return scm_from_bool(raeql (ra0, SCM_BOOL_F, ra1));
  return scm_equal_p (ra0, ra1);
}


static void
init_raprocs (ra_iproc *subra)
{
  for (; subra->name; subra++)
    {
      SCM sym = scm_from_locale_symbol (subra->name);
      SCM var =
	scm_sym2var (sym, scm_current_module_lookup_closure (), SCM_BOOL_F);
      if (var != SCM_BOOL_F)
	subra->sproc = SCM_VARIABLE_REF (var);
      else
	subra->sproc = SCM_BOOL_F;
    }
}


void
scm_init_ramap ()
{
  init_raprocs (ra_rpsubrs);
  init_raprocs (ra_asubrs);
  scm_c_define_subr (s_array_equal_p, scm_tc7_rpsubr, scm_array_equal_p);
  scm_smobs[SCM_TC2SMOBNUM (scm_i_tc16_array)].equalp = scm_raequal;
#include "libguile/ramap.x"
  scm_add_feature (s_scm_array_for_each);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
