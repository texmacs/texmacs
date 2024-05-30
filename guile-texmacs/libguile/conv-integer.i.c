/* This code in included by numbers.c to generate integer conversion
   functions like scm_to_int and scm_from_int.  It is only for signed
   types, see conv-uinteger.i.c for the unsigned variant.
*/

/* You need to define the following macros before including this
   template.  They are undefined at the end of this file to give a
   clean slate for the next inclusion.

   TYPE         - the integral type to be converted
   TYPE_MIN     - the smallest representable number of TYPE
   TYPE_MAX     - the largest representable number of TYPE
   SIZEOF_TYPE  - the size of TYPE, equal to "sizeof (TYPE)" but
                  in a form that can be computed by the preprocessor.
		  When this number is 0, the preprocessor is not used
		  to select which code to compile; the most general
		  code is always used.

   SCM_TO_TYPE_PROTO(arg), SCM_FROM_TYPE_PROTO(arg) 
                - These two macros should expand into the prototype
                  for the two defined functions, without the return
                  type.

*/

TYPE
SCM_TO_TYPE_PROTO (SCM val)
{
  if (SCM_I_INUMP (val))
    {
      scm_t_signed_bits n = SCM_I_INUM (val);
#if SIZEOF_TYPE != 0 && SIZEOF_TYPE > SIZEOF_SCM_T_BITS
      return n;
#else
      if (n >= TYPE_MIN && n <= TYPE_MAX)
	return n;
      else
	{
	  goto out_of_range;
	}
#endif
    }
  else if (SCM_BIGP (val))
    {
      if (TYPE_MIN >= SCM_MOST_NEGATIVE_FIXNUM
	  && TYPE_MAX <= SCM_MOST_POSITIVE_FIXNUM)
	goto out_of_range;
      else if (TYPE_MIN >= LONG_MIN && TYPE_MAX <= LONG_MAX)
	{
	  if (mpz_fits_slong_p (SCM_I_BIG_MPZ (val)))
	    {
	      long n = mpz_get_si (SCM_I_BIG_MPZ (val));
#if SIZEOF_TYPE != 0 && SIZEOF_TYPE > SCM_SIZEOF_LONG
	      return n;
#else
	      if (n >= TYPE_MIN && n <= TYPE_MAX)
		return n;
	      else
		goto out_of_range;
#endif
	    } 
	  else
	    goto out_of_range;
	}
      else
	{
	  scm_t_intmax n;
	  size_t count;

	  if (mpz_sizeinbase (SCM_I_BIG_MPZ (val), 2)
	      > CHAR_BIT*sizeof (scm_t_uintmax))
	    goto out_of_range;
	  
	  mpz_export (&n, &count, 1, sizeof (scm_t_uintmax), 0, 0,
		      SCM_I_BIG_MPZ (val));

	  if (mpz_sgn (SCM_I_BIG_MPZ (val)) >= 0)
	    {
	      if (n < 0)
		goto out_of_range;
	    }
	  else
	    {
	      n = -n;
	      if (n >= 0)
		goto out_of_range;
	    }

	  if (n >= TYPE_MIN && n <= TYPE_MAX)
	    return n;
	  else
	    {
	    out_of_range:
	      scm_i_range_error (val,
				 scm_from_signed_integer (TYPE_MIN),
				 scm_from_signed_integer (TYPE_MAX));
	      return 0;
	    }
	}
    }
  else
    {
      scm_wrong_type_arg_msg (NULL, 0, val, "exact integer");
      return 0;
    }
}

SCM
SCM_FROM_TYPE_PROTO (TYPE val)
{
#if SIZEOF_TYPE != 0 && SIZEOF_TYPE < SIZEOF_SCM_T_BITS
  return SCM_I_MAKINUM (val);
#else
  if (SCM_FIXABLE (val))
    return SCM_I_MAKINUM (val);
  else if (val >= LONG_MIN && val <= LONG_MAX)
    return scm_i_long2big (val);
  else
    {
      SCM z = scm_double_cell (scm_tc16_big, 0, 0, 0);
      mpz_init (SCM_I_BIG_MPZ (z));
      if (val < 0)
	{
	  val = -val;
	  mpz_import (SCM_I_BIG_MPZ (z), 1, 1, sizeof (TYPE), 0, 0,
		      &val);
	  mpz_neg (SCM_I_BIG_MPZ (z), SCM_I_BIG_MPZ (z));
	}
      else
	mpz_import (SCM_I_BIG_MPZ (z), 1, 1, sizeof (TYPE), 0, 0,
		    &val);
      return z;
    }
#endif
}

/* clean up */
#undef TYPE
#undef TYPE_MIN
#undef TYPE_MAX
#undef SIZEOF_TYPE
#undef SCM_TO_TYPE_PROTO
#undef SCM_FROM_TYPE_PROTO

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
