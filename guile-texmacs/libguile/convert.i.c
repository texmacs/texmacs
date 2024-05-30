/* this file is #include'd (x times) by convert.c */

/* You need to define the following macros before including this
   template.  They are undefined at the end of this file to give a
   clean slate for the next inclusion.

   - CTYPE

   The type of an element of the C array, for example 'char'.

   - FROM_CTYPE

   The function that converts a CTYPE to a SCM, for example
   scm_from_char.

   - UVEC_TAG

   The tag of a suitable uniform vector that can hold the CTYPE, for
   example 's8'.

   - UVEC_CTYPE

   The C type of an element of the uniform vector, for example
   scm_t_int8.

   - SCM2CTYPES

   The name of the 'SCM-to-C' function, for example scm_c_scm2chars.

   - CTYPES2SCM

   The name of the 'C-to-SCM' function, for example, scm_c_chars2scm.

   - CTYPES2UVECT

   The name of the 'C-to-uniform-vector' function, for example
   scm_c_chars2byvect.  It will create a uniform vector of kind
   UVEC_TAG.

   - CTYPES2UVECT_2

   The name of a second 'C-to-uniform-vector' function.  Leave
   undefined if you want only one such function.

   - CTYPE_2
   - UVEC_TAG_2
   - UVEC_CTYPE_2

   The tag and C type of the second kind of uniform vector, for use
   with the function described above.

*/

/* The first level does not expand macros in the arguments. */
#define paste(a1,a2,a3)   a1##a2##a3
#define stringify(a)      #a

/* But the second level does. */
#define F(pre,T,suf)   paste(pre,T,suf)
#define S(T)           stringify(T)

/* Convert a vector, list or uniform vector into a C array.  If the
   result array in argument 2 is NULL, malloc() a new one.
*/

CTYPE *
SCM2CTYPES (SCM obj, CTYPE *data)
{
  scm_t_array_handle handle;
  size_t i, len;
  ssize_t inc;
  const UVEC_CTYPE *uvec_elements;

  obj = F(scm_any_to_,UVEC_TAG,vector) (obj);
  uvec_elements = F(scm_,UVEC_TAG,vector_elements) (obj, &handle, &len, &inc);

  if (data == NULL)
    data = scm_malloc (len * sizeof (CTYPE));
  for (i = 0; i < len; i++, uvec_elements += inc)
    data[i] = uvec_elements[i];

  scm_array_handle_release (&handle);

  return data;
}

/* Converts a C array into a vector. */

SCM
CTYPES2SCM (const CTYPE *data, long n)
{
  long i;
  SCM v;
  
  v = scm_c_make_vector (n, SCM_UNSPECIFIED);

  for (i = 0; i < n; i++)
    SCM_SIMPLE_VECTOR_SET (v, i, FROM_CTYPE (data[i]));

  return v;
}

/* Converts a C array into a uniform vector. */

SCM
CTYPES2UVECT (const CTYPE *data, long n)
{
  scm_t_array_handle handle;
  long i;
  SCM uvec;
  UVEC_CTYPE *uvec_elements;
  
  uvec = F(scm_make_,UVEC_TAG,vector) (scm_from_long (n), SCM_UNDEFINED);
  uvec_elements = F(scm_,UVEC_TAG,vector_writable_elements) (uvec, &handle,
							     NULL, NULL);
  for (i = 0; i < n; i++)
    uvec_elements[i] = data[i];

  scm_array_handle_release (&handle);

  return uvec;
}

#ifdef CTYPE2UVECT_2

SCM
CTYPES2UVECT_2 (const CTYPE_2 *data, long n)
{
  scm_t_array_handle handle;
  long i;
  SCM uvec;
  UVEC_CTYPE_2 *uvec_elements;
  
  uvec = F(scm_make_,UVEC_TAG_2,vector) (scm_from_long (n), SCM_UNDEFINED);
  uvec_elements = F(scm_,UVEC_TAG_2,vector_writable_elements) (uvec, &handle,
							       NULL, NULL);

  for (i = 0; i < n; i++)
    uvec_elements[i] = data[i];

  scm_array_handle_release (&handle);

  return uvec;
}

#endif

#undef paste
#undef stringify
#undef F
#undef S

#undef CTYPE
#undef FROM_CTYPE
#undef UVEC_TAG
#undef UVEC_CTYPE
#undef SCM2CTYPES
#undef CTYPES2SCM
#undef CTYPES2UVECT
#ifdef CTYPES2UVECT_2
#undef CTYPES2UVECT_2
#undef CTYPE_2
#undef UVEC_TAG_2
#undef UVEC_CTYPE_2
#endif

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
