/* This file defines the procedures related to one type of uniform
   numeric vector.  It is included multiple time in srfi-4.c, once for
   each type.

   Before inclusion, the following macros must be defined.  They are
   undefined at the end of this file to get back to a clean slate for
   the next inclusion.

   - TYPE

   The type tag of the vector, for example SCM_UVEC_U8

   - TAG

   The tag name of the vector, for example u8.  The tag is used to
   form the function names and is included in the docstrings, for
   example.

   - CTYPE

   The C type of the elements, for example scm_t_uint8.  The code
   below will never do sizeof (CTYPE), thus you can use just 'float'
   for the c32 type, for example.

   When CTYPE is not defined, the functions using it are excluded.
*/

/* The first level does not expand macros in the arguments. */
#define paste(a1,a2,a3)   a1##a2##a3
#define s_paste(a1,a2,a3) s_##a1##a2##a3
#define stringify(a)      #a

/* But the second level does. */
#define F(pre,T,suf)   paste(pre,T,suf)
#define s_F(pre,T,suf) s_paste(pre,T,suf)
#define S(T)           stringify(T)

SCM_DEFINE (F(scm_,TAG,vector_p), S(TAG)"vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type " S(TAG) ",\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_F(scm_, TAG, vector_p)
{
  return uvec_p (TYPE, obj);
}
#undef FUNC_NAME

SCM_DEFINE (F(scm_make_,TAG,vector), "make-"S(TAG)"vector", 1, 1, 0,
            (SCM len, SCM fill),
	    "Return a newly allocated uniform numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_S(scm_make_,TAG,vector)
{
  return make_uvec (TYPE, len, fill);
}
#undef FUNC_NAME

SCM_DEFINE (F(scm_,TAG,vector), S(TAG)"vector", 0, 0, 1,
            (SCM l),
	    "Return a newly allocated uniform numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_F(scm_,TAG,vector)
{
  return list_to_uvec (TYPE, l);
}
#undef FUNC_NAME


SCM_DEFINE (F(scm_,TAG,vector_length), S(TAG)"vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the uniform numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_F(scm_,TAG,vector_length)
{
  return uvec_length (TYPE, uvec);
}
#undef FUNC_NAME


SCM_DEFINE (F(scm_,TAG,vector_ref), S(TAG)"vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the uniform numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_F(scm_,TAG,vector_ref)
{
  return uvec_ref (TYPE, uvec, index);
}
#undef FUNC_NAME


SCM_DEFINE (F(scm_,TAG,vector_set_x), S(TAG)"vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the uniform numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_F(scm_,TAG,vector_set_x)
{
  return uvec_set_x (TYPE, uvec, index, value);
}
#undef FUNC_NAME


SCM_DEFINE (F(scm_,TAG,vector_to_list), S(TAG)"vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the uniform numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_F(scm_,TAG,vector_to_list)
{
  return uvec_to_list (TYPE, uvec);
}
#undef FUNC_NAME


SCM_DEFINE (F(scm_list_to_,TAG,vector), "list->"S(TAG)"vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l} to a numeric uniform vector.")
#define FUNC_NAME s_F(scm_list_to_,TAG,vector)
{
  return list_to_uvec (TYPE, l);
}
#undef FUNC_NAME

SCM_DEFINE (F(scm_any_to_,TAG,vector), "any->"S(TAG)"vector", 1, 0, 0,
	    (SCM obj),
	    "Convert @var{obj}, which can be a list, vector, or\n"
	    "uniform vector, to a numeric uniform vector of\n"
	    "type " S(TAG)".")
#define FUNC_NAME s_F(scm_any_to_,TAG,vector)
{
  return coerce_to_uvec (TYPE, obj);
}
#undef FUNC_NAME

#ifdef CTYPE

SCM
F(scm_take_,TAG,vector) (CTYPE *data, size_t n)
{
  scm_gc_register_collectable_memory ((void *)data, n*uvec_sizes[TYPE],
				      uvec_names[TYPE]);
  return take_uvec (TYPE, data, n);
}

const CTYPE *
F(scm_array_handle_,TAG,_elements) (scm_t_array_handle *h)
{
  return F(scm_array_handle_,TAG,_writable_elements) (h);
}

CTYPE *
F(scm_array_handle_,TAG,_writable_elements) (scm_t_array_handle *h)
{
  SCM vec = h->array;
  if (SCM_I_ARRAYP (vec))
    vec = SCM_I_ARRAY_V (vec);
  uvec_assert (TYPE, vec);
  if (TYPE == SCM_UVEC_C32 || TYPE == SCM_UVEC_C64)
    return ((CTYPE *)SCM_UVEC_BASE (vec)) + 2*h->base;
  else
    return ((CTYPE *)SCM_UVEC_BASE (vec)) + h->base;
}

const CTYPE *
F(scm_,TAG,vector_elements) (SCM uvec, 
			     scm_t_array_handle *h,
			     size_t *lenp, ssize_t *incp)
{
  return F(scm_,TAG,vector_writable_elements) (uvec, h, lenp, incp);
}

CTYPE *
F(scm_,TAG,vector_writable_elements) (SCM uvec, 
				      scm_t_array_handle *h,
				      size_t *lenp, ssize_t *incp)
{
  scm_generalized_vector_get_handle (uvec, h);
  if (lenp)
    {
      scm_t_array_dim *dim = scm_array_handle_dims (h);
      *lenp = dim->ubnd - dim->lbnd + 1;
      *incp = dim->inc;
    }
  return F(scm_array_handle_,TAG,_writable_elements) (h);
}

#endif

static SCM
F(,TAG,ref) (scm_t_array_handle *handle, ssize_t pos)
{
  return uvec_fast_ref (TYPE, handle->elements, pos);
}

static void
F(,TAG,set) (scm_t_array_handle *handle, ssize_t pos, SCM val)
{
  uvec_fast_set_x (TYPE, handle->writable_elements, pos, val);
}

#undef paste
#undef s_paste
#undef stringify
#undef F
#undef s_F
#undef S

#undef TYPE
#undef TAG
#undef CTYPE
