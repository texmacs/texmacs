/* Copyright (C) 1996,1997,1998,1999,2000,2001, 2003, 2004, 2006, 2007 Free Software Foundation, Inc.
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
#  include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/async.h"
#include "libguile/chars.h"
#include "libguile/eval.h"
#include "libguile/alist.h"
#include "libguile/weaks.h"
#include "libguile/hashtab.h"
#include "libguile/ports.h"
#include "libguile/strings.h"

#include "libguile/validate.h"
#include "libguile/struct.h"

#include "libguile/eq.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif



static SCM required_vtable_fields = SCM_BOOL_F;
SCM scm_struct_table;


SCM_DEFINE (scm_make_struct_layout, "make-struct-layout", 1, 0, 0, 
            (SCM fields),
	    "Return a new structure layout object.\n\n"
	    "@var{fields} must be a string made up of pairs of characters\n"
	    "strung together.  The first character of each pair describes a field\n"
	    "type, the second a field protection.  Allowed types are 'p' for\n"
	    "GC-protected Scheme data, 'u' for unprotected binary data, and 's' for\n"
	    "a field that points to the structure itself.    Allowed protections\n"
	    "are 'w' for mutable fields, 'r' for read-only fields, and 'o' for opaque\n"
	    "fields.  The last field protection specification may be capitalized to\n"
	    "indicate that the field is a tail-array.")
#define FUNC_NAME s_scm_make_struct_layout
{
  SCM new_sym;
  SCM_VALIDATE_STRING (1, fields);

  { /* scope */
    const char * field_desc;
    size_t len;
    int x;

    len = scm_i_string_length (fields);
    if (len % 2 == 1)
      SCM_MISC_ERROR ("odd length field specification: ~S", 
		      scm_list_1 (fields));

    field_desc = scm_i_string_chars (fields);

    for (x = 0; x < len; x += 2)
      {
	switch (field_desc[x])
	  {
	  case 'u':
	  case 'p':
#if 0
	  case 'i':
	  case 'd':
#endif
	  case 's':
	    break;
	  default:
	    SCM_MISC_ERROR ("unrecognized field type: ~S", 
			    scm_list_1 (SCM_MAKE_CHAR (field_desc[x])));
	  }

	switch (field_desc[x + 1])
	  {
	  case 'w':
	    if (field_desc[x] == 's')
	      SCM_MISC_ERROR ("self fields not writable", SCM_EOL);
	  case 'r':
	  case 'o':
	    break;
	  case 'R':
	  case 'W':
	  case 'O':
	    if (field_desc[x] == 's')
	      SCM_MISC_ERROR ("self fields not allowed in tail array", 
			      SCM_EOL);
	    if (x != len - 2)
	      SCM_MISC_ERROR ("tail array field must be last field in layout",
			      SCM_EOL);
	    break;
	  default:
	    SCM_MISC_ERROR ("unrecognized ref specification: ~S",
			    scm_list_1 (SCM_MAKE_CHAR (field_desc[x + 1])));
	  }
#if 0
	if (field_desc[x] == 'd')
	  {
	    if (field_desc[x + 2] != '-')
	      SCM_MISC_ERROR ("missing dash field at position ~A",
			      scm_list_1 (scm_from_int (x / 2)));
	    x += 2;
	    goto recheck_ref;
	  }
#endif
      }
    new_sym = scm_string_to_symbol (fields);
  }
  scm_remember_upto_here_1 (fields);
  return new_sym;
}
#undef FUNC_NAME





static void
scm_struct_init (SCM handle, SCM layout, scm_t_bits * mem, int tail_elts, SCM inits)
{
  unsigned const char *fields_desc =
    (unsigned const char *) scm_i_symbol_chars (layout) - 2;
  unsigned char prot = 0;
  int n_fields = scm_i_symbol_length (layout) / 2;
  int tailp = 0;

  while (n_fields)
    {
      if (!tailp)
	{
	  fields_desc += 2;
	  prot = fields_desc[1];
	  if (SCM_LAYOUT_TAILP (prot))
	    {
	      tailp = 1;
	      prot = prot == 'R' ? 'r' : prot == 'W' ? 'w' : 'o';
	      *mem++ = tail_elts;
	      n_fields += tail_elts - 1;
	      if (n_fields == 0)
		break;
	    }
	}
      
      switch (*fields_desc)
	{
#if 0
	case 'i':
	  if ((prot != 'r' && prot != 'w') || inits == SCM_EOL)
	    *mem = 0;
	  else
	    {
	      *mem = scm_to_long (SCM_CAR (inits));
	      inits = SCM_CDR (inits);
	    }
	  break;
#endif

	case 'u':
	  if ((prot != 'r' && prot != 'w') || scm_is_null (inits))
	    *mem = 0;
	  else
	    {
	      *mem = scm_to_ulong (SCM_CAR (inits));
	      inits = SCM_CDR (inits);
	    }
	  break;

	case 'p':
	  if ((prot != 'r' && prot != 'w') || scm_is_null (inits))
	    *mem = SCM_UNPACK (SCM_BOOL_F);
	  else
	    {
	      *mem = SCM_UNPACK (SCM_CAR (inits));
	      inits = SCM_CDR (inits);
	    }
	      
	  break;

#if 0
	case 'd':
	  if ((prot != 'r' && prot != 'w') || inits == SCM_EOL)
	    *((double *)mem) = 0.0;
	  else
	    {
	      *mem = scm_num2dbl (SCM_CAR (inits), "scm_struct_init");
	      inits = SCM_CDR (inits);
	    }
	  fields_desc += 2;
	  break;
#endif

	case 's':
	  *mem = SCM_UNPACK (handle);
	  break;
	}

      n_fields--;
      mem++;
    }
}


SCM_DEFINE (scm_struct_p, "struct?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} iff @var{x} is a structure object, else\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_struct_p
{
  return scm_from_bool(SCM_STRUCTP (x));
}
#undef FUNC_NAME

SCM_DEFINE (scm_struct_vtable_p, "struct-vtable?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} iff @var{x} is a vtable structure.")
#define FUNC_NAME s_scm_struct_vtable_p
{
  SCM layout;
  scm_t_bits * mem;
  int tmp;

  if (!SCM_STRUCTP (x))
    return SCM_BOOL_F;

  layout = SCM_STRUCT_LAYOUT (x);

  if (scm_i_symbol_length (layout)
      < scm_i_string_length (required_vtable_fields))
    return SCM_BOOL_F;

  tmp = strncmp (scm_i_symbol_chars (layout),
		 scm_i_string_chars (required_vtable_fields),
		 scm_i_string_length (required_vtable_fields));
  scm_remember_upto_here_1 (required_vtable_fields);
  if (tmp)
    return SCM_BOOL_F;

  mem = SCM_STRUCT_DATA (x);

  return scm_from_bool (scm_is_symbol (SCM_PACK (mem[scm_vtable_index_layout])));
}
#undef FUNC_NAME


/* All struct data must be allocated at an address whose bottom three
   bits are zero.  This is because the tag for a struct lives in the
   bottom three bits of the struct's car, and the upper bits point to
   the data of its vtable, which is a struct itself.  Thus, if the
   address of that data doesn't end in three zeros, tagging it will
   destroy the pointer.

   This function allocates a block of memory, and returns a pointer at
   least scm_struct_n_extra_words words into the block.  Furthermore,
   it guarantees that that pointer's least three significant bits are
   all zero.

   The argument n_words should be the number of words that should
   appear after the returned address.  (That is, it shouldn't include
   scm_struct_n_extra_words.)

   This function initializes the following fields of the struct:

     scm_struct_i_ptr --- the actual start of the block of memory; the
	 address you should pass to 'free' to dispose of the block.
	 This field allows us to both guarantee that the returned
	 address is divisible by eight, and allow the GC to free the
	 block.

     scm_struct_i_n_words --- the number of words allocated to the
         block, including the extra fields.  This is used by the GC.

     Ugh.  */


scm_t_bits *
scm_alloc_struct (int n_words, int n_extra, const char *what)
{
  int size = sizeof (scm_t_bits) * (n_words + n_extra) + 7;
  void * block = scm_gc_malloc (size, what);

  /* Adjust the pointer to hide the extra words.  */
  scm_t_bits * p = (scm_t_bits *) block + n_extra;

  /* Adjust it even further so it's aligned on an eight-byte boundary.  */
  p = (scm_t_bits *) (((scm_t_bits) p + 7) & ~7);

  /* Initialize a few fields as described above.  */
  p[scm_struct_i_free] = (scm_t_bits) scm_struct_free_standard;
  p[scm_struct_i_ptr] = (scm_t_bits) block;
  p[scm_struct_i_n_words] = n_words;
  p[scm_struct_i_flags] = 0;

  return p;
}

void
scm_struct_free_0 (scm_t_bits * vtable SCM_UNUSED,
		   scm_t_bits * data SCM_UNUSED)
{
}

void
scm_struct_free_light (scm_t_bits * vtable, scm_t_bits * data)
{
  size_t n = vtable [scm_struct_i_size] & ~SCM_STRUCTF_MASK;
  scm_gc_free (data, n, "struct");
}

void
scm_struct_free_standard (scm_t_bits * vtable SCM_UNUSED, scm_t_bits * data)
{
  size_t n = (data[scm_struct_i_n_words] + scm_struct_n_extra_words)
	     * sizeof (scm_t_bits) + 7;
  scm_gc_free ((void *) data[scm_struct_i_ptr], n, "heavy struct");
}

void
scm_struct_free_entity (scm_t_bits * vtable SCM_UNUSED, scm_t_bits * data)
{
  size_t n = (data[scm_struct_i_n_words] + scm_struct_entity_n_extra_words)
	     * sizeof (scm_t_bits) + 7;
  scm_gc_free ((void *) data[scm_struct_i_ptr], n, "entity struct");
}

static void *
scm_struct_gc_init (void *dummy1 SCM_UNUSED,
		    void *dummy2 SCM_UNUSED,
		    void *dummy3 SCM_UNUSED)
{
  scm_i_structs_to_free = SCM_EOL;
  return 0;
}

static void *
scm_free_structs (void *dummy1 SCM_UNUSED,
		  void *dummy2 SCM_UNUSED,
		  void *dummy3 SCM_UNUSED)
{
  SCM newchain = scm_i_structs_to_free;
  do
    {
      /* Mark vtables in GC chain.  GC mark set means delay freeing. */
      SCM chain = newchain;
      while (!scm_is_null (chain))
	{
	  SCM vtable = SCM_STRUCT_VTABLE (chain);
	  if (SCM_STRUCT_GC_CHAIN (vtable) != 0 && vtable != chain)
	    SCM_SET_GC_MARK (vtable);
	  chain = SCM_STRUCT_GC_CHAIN (chain);
	}
      /* Free unmarked structs.  */
      chain = newchain;
      newchain = SCM_EOL;
      while (!scm_is_null (chain))
	{
	  SCM obj = chain;
	  chain = SCM_STRUCT_GC_CHAIN (chain);
	  if (SCM_GC_MARK_P (obj))
	    {
	      SCM_CLEAR_GC_MARK (obj);
	      SCM_SET_STRUCT_GC_CHAIN (obj, newchain);
	      newchain = obj;
	    }
	  else
	    {
	      scm_t_bits * vtable_data = SCM_STRUCT_VTABLE_DATA (obj);
	      scm_t_bits * data = SCM_STRUCT_DATA (obj);
	      scm_t_struct_free free_struct_data
		= ((scm_t_struct_free) vtable_data[scm_struct_i_free]);
	      SCM_SET_CELL_TYPE (obj, scm_tc_free_cell);
	      free_struct_data (vtable_data, data);
	    }
	}
    }
  while (!scm_is_null (newchain));
  return 0;
}

SCM_DEFINE (scm_make_struct, "make-struct", 2, 0, 1, 
            (SCM vtable, SCM tail_array_size, SCM init),
	    "Create a new structure.\n\n"
	    "@var{type} must be a vtable structure (@pxref{Vtables}).\n\n"
	    "@var{tail-elts} must be a non-negative integer.  If the layout\n"
	    "specification indicated by @var{type} includes a tail-array,\n"
	    "this is the number of elements allocated to that array.\n\n"
	    "The @var{init1}, @dots{} are optional arguments describing how\n"
	    "successive fields of the structure should be initialized.  Only fields\n"
	    "with protection 'r' or 'w' can be initialized, except for fields of\n"
	    "type 's', which are automatically initialized to point to the new\n"
	    "structure itself; fields with protection 'o' can not be initialized by\n"
	    "Scheme programs.\n\n"
	    "If fewer optional arguments than initializable fields are supplied,\n"
	    "fields of type 'p' get default value #f while fields of type 'u' are\n"
	    "initialized to 0.\n\n"
	    "Structs are currently the basic representation for record-like data\n"
	    "structures in Guile.  The plan is to eventually replace them with a\n"
	    "new representation which will at the same time be easier to use and\n"
	    "more powerful.\n\n"
	    "For more information, see the documentation for @code{make-vtable-vtable}.")
#define FUNC_NAME s_scm_make_struct
{
  SCM layout;
  size_t basic_size;
  size_t tail_elts;
  scm_t_bits * data;
  SCM handle;

  SCM_VALIDATE_VTABLE (1, vtable);
  SCM_VALIDATE_REST_ARGUMENT (init);

  layout = SCM_PACK (SCM_STRUCT_DATA (vtable) [scm_vtable_index_layout]);
  basic_size = scm_i_symbol_length (layout) / 2;
  tail_elts = scm_to_size_t (tail_array_size);

  /* A tail array is only allowed if the layout fields string ends in "R",
     "W" or "O". */
  if (tail_elts != 0)
    {
      SCM layout_str, last_char;
      
      if (basic_size == 0)
        {
        bad_tail: 
          SCM_MISC_ERROR ("tail array not allowed unless layout ends R, W, or O", SCM_EOL);
        }

      layout_str = scm_symbol_to_string (layout);
      last_char = scm_string_ref (layout_str,
                                  scm_from_size_t (2 * basic_size - 1));
      if (! SCM_LAYOUT_TAILP (SCM_CHAR (last_char)))
        goto bad_tail;
    }

  /* In guile 1.8.5 and earlier, everything below was covered by a
     CRITICAL_SECTION lock.  This can lead to deadlocks in garbage
     collection, since other threads might be holding the heap_mutex, while
     sleeping on the CRITICAL_SECTION lock.  There does not seem to be any
     need for a lock on the section below, as it does not access or update
     any globals, so the critical section has been removed. */

  if (SCM_STRUCT_DATA (vtable)[scm_struct_i_flags] & SCM_STRUCTF_ENTITY)
    {
      data = scm_alloc_struct (basic_size + tail_elts,
			       scm_struct_entity_n_extra_words,
			       "entity struct");
      data[scm_struct_i_procedure] = SCM_UNPACK (SCM_BOOL_F);
      data[scm_struct_i_setter] = SCM_UNPACK (SCM_BOOL_F);
    }
  else
    data = scm_alloc_struct (basic_size + tail_elts,
			     scm_struct_n_extra_words,
			     "struct");
  handle = scm_double_cell ((((scm_t_bits) SCM_STRUCT_DATA (vtable))
			     + scm_tc3_struct),
			    (scm_t_bits) data, 0, 0);

  scm_struct_init (handle, layout, data, tail_elts, init);

  return handle;
}
#undef FUNC_NAME



SCM_DEFINE (scm_make_vtable_vtable, "make-vtable-vtable", 2, 0, 1,
            (SCM user_fields, SCM tail_array_size, SCM init),
	    "Return a new, self-describing vtable structure.\n\n"
	    "@var{user-fields} is a string describing user defined fields of the\n"
	    "vtable beginning at index @code{vtable-offset-user}\n"
	    "(see @code{make-struct-layout}).\n\n"
	    "@var{tail-size} specifies the size of the tail-array (if any) of\n"
	    "this vtable.\n\n"
	    "@var{init1}, @dots{} are the optional initializers for the fields of\n"
	    "the vtable.\n\n"
	    "Vtables have one initializable system field---the struct printer.\n"
	    "This field comes before the user fields in the initializers passed\n"
	    "to @code{make-vtable-vtable} and @code{make-struct}, and thus works as\n"
	    "a third optional argument to @code{make-vtable-vtable} and a fourth to\n"
	    "@code{make-struct} when creating vtables:\n\n"
	    "If the value is a procedure, it will be called instead of the standard\n"
	    "printer whenever a struct described by this vtable is printed.\n"
	    "The procedure will be called with arguments STRUCT and PORT.\n\n"
	    "The structure of a struct is described by a vtable, so the vtable is\n"
	    "in essence the type of the struct.  The vtable is itself a struct with\n"
	    "a vtable.  This could go on forever if it weren't for the\n"
	    "vtable-vtables which are self-describing vtables, and thus terminate\n"
	    "the chain.\n\n"
	    "There are several potential ways of using structs, but the standard\n"
	    "one is to use three kinds of structs, together building up a type\n"
	    "sub-system: one vtable-vtable working as the root and one or several\n"
	    "\"types\", each with a set of \"instances\".  (The vtable-vtable should be\n"
	    "compared to the class <class> which is the class of itself.)\n\n"
	    "@lisp\n"
	    "(define ball-root (make-vtable-vtable \"pr\" 0))\n\n"
	    "(define (make-ball-type ball-color)\n"
	    "  (make-struct ball-root 0\n"
	    "	       (make-struct-layout \"pw\")\n"
	    "               (lambda (ball port)\n"
	    "                 (format port \"#<a ~A ball owned by ~A>\"\n"
	    "                         (color ball)\n"
	    "                         (owner ball)))\n"
	    "               ball-color))\n"
	    "(define (color ball) (struct-ref (struct-vtable ball) vtable-offset-user))\n"
	    "(define (owner ball) (struct-ref ball 0))\n\n"
	    "(define red (make-ball-type 'red))\n"
	    "(define green (make-ball-type 'green))\n\n"
	    "(define (make-ball type owner) (make-struct type 0 owner))\n\n"
	    "(define ball (make-ball green 'Nisse))\n"
	    "ball @result{} #<a green ball owned by Nisse>\n"
	    "@end lisp")
#define FUNC_NAME s_scm_make_vtable_vtable
{
  SCM fields;
  SCM layout;
  size_t basic_size;
  size_t tail_elts;
  scm_t_bits *data;
  SCM handle;

  SCM_VALIDATE_STRING (1, user_fields);
  SCM_VALIDATE_REST_ARGUMENT (init);

  fields = scm_string_append (scm_list_2 (required_vtable_fields,
					  user_fields));
  layout = scm_make_struct_layout (fields);
  basic_size = scm_i_symbol_length (layout) / 2;
  tail_elts = scm_to_size_t (tail_array_size);
  SCM_CRITICAL_SECTION_START;
  data = scm_alloc_struct (basic_size + tail_elts,
			   scm_struct_n_extra_words,
			   "struct");
  handle = scm_double_cell ((scm_t_bits) data + scm_tc3_struct,
			    (scm_t_bits) data, 0, 0);
  data [scm_vtable_index_layout] = SCM_UNPACK (layout);
  scm_struct_init (handle, layout, data, tail_elts, scm_cons (layout, init));
  SCM_CRITICAL_SECTION_END;
  return handle;
}
#undef FUNC_NAME


static SCM scm_i_vtable_vtable_no_extra_fields;

SCM_DEFINE (scm_make_vtable, "make-vtable", 1, 1, 0,
            (SCM fields, SCM printer),
	    "Create a vtable, for creating structures with the given\n"
	    "@var{fields}.\n"
	    "\n"
	    "The optional @var{printer} argument is a function to be called\n"
	    "@code{(@var{printer} struct port)} on the structures created.\n"
	    "It should look at @var{struct} and write to @var{port}.")
#define FUNC_NAME s_scm_make_vtable
{
  if (SCM_UNBNDP (printer))
    printer = SCM_BOOL_F;

  return scm_make_struct (scm_i_vtable_vtable_no_extra_fields, SCM_INUM0,
                          scm_list_2 (scm_make_struct_layout (fields),
                                      printer));
}
#undef FUNC_NAME


/* Return true if S1 and S2 are equal structures, i.e., if their vtable and
   contents are the same.  Field protections are honored.  Thus, it is an
   error to test the equality of structures that contain opaque fields.  */
SCM
scm_i_struct_equalp (SCM s1, SCM s2)
#define FUNC_NAME "scm_i_struct_equalp"
{
  SCM vtable1, vtable2, layout;
  size_t struct_size, field_num;

  SCM_VALIDATE_STRUCT (1, s1);
  SCM_VALIDATE_STRUCT (2, s2);

  vtable1 = SCM_STRUCT_VTABLE (s1);
  vtable2 = SCM_STRUCT_VTABLE (s2);

  if (!scm_is_eq (vtable1, vtable2))
    return SCM_BOOL_F;

  layout = SCM_STRUCT_LAYOUT (s1);
  struct_size = scm_i_symbol_length (layout) / 2;

  for (field_num = 0; field_num < struct_size; field_num++)
    {
      SCM s_field_num;
      SCM field1, field2;

      /* We have to use `scm_struct_ref ()' here so that fields are accessed
	 consistently, notably wrt. field types and access rights.  */
      s_field_num = scm_from_size_t (field_num);
      field1 = scm_struct_ref (s1, s_field_num);
      field2 = scm_struct_ref (s2, s_field_num);

      /* Self-referencing fields (type `s') must be skipped to avoid infinite
	 recursion.  */
      if (!(scm_is_eq (field1, s1) && (scm_is_eq (field2, s2))))
	if (scm_is_false (scm_equal_p (field1, field2)))
	  return SCM_BOOL_F;
    }

  /* FIXME: Tail elements should be tested for equality.  */

  return SCM_BOOL_T;
}
#undef FUNC_NAME





SCM_DEFINE (scm_struct_ref, "struct-ref", 2, 0, 0,
            (SCM handle, SCM pos),
	    "@deffnx {Scheme Procedure} struct-set! struct n value\n"
	    "Access (or modify) the @var{n}th field of @var{struct}.\n\n"
	    "If the field is of type 'p', then it can be set to an arbitrary value.\n\n"
	    "If the field is of type 'u', then it can only be set to a non-negative\n"
	    "integer value small enough to fit in one machine word.")
#define FUNC_NAME s_scm_struct_ref
{
  SCM answer = SCM_UNDEFINED;
  scm_t_bits * data;
  SCM layout;
  size_t layout_len;
  size_t p;
  scm_t_bits n_fields;
  const char *fields_desc;
  char field_type = 0;
  

  SCM_VALIDATE_STRUCT (1, handle);

  layout = SCM_STRUCT_LAYOUT (handle);
  data = SCM_STRUCT_DATA (handle);
  p = scm_to_size_t (pos);

  fields_desc = scm_i_symbol_chars (layout);
  layout_len = scm_i_symbol_length (layout);
  if (SCM_STRUCT_VTABLE_FLAGS (handle) & SCM_STRUCTF_LIGHT)
    /* no extra words */
    n_fields = layout_len / 2;
  else
    n_fields = data[scm_struct_i_n_words];
  
  SCM_ASSERT_RANGE(1, pos, p < n_fields);

  if (p * 2 < layout_len)
    {
      char ref;
      field_type = fields_desc[p * 2];
      ref = fields_desc[p * 2 + 1];
      if ((ref != 'r') && (ref != 'w'))
	{
	  if ((ref == 'R') || (ref == 'W'))
	    field_type = 'u';
	  else
	    SCM_MISC_ERROR ("ref denied for field ~A", scm_list_1 (pos));
	}
    }
  else if (fields_desc[layout_len - 1] != 'O')    
    field_type = fields_desc[layout_len - 2];
  else
    SCM_MISC_ERROR ("ref denied for field ~A", scm_list_1 (pos));
  
  switch (field_type)
    {
    case 'u':
      answer = scm_from_ulong (data[p]);
      break;

#if 0
    case 'i':
      answer = scm_from_long (data[p]);
      break;

    case 'd':
      answer = scm_make_real (*((double *)&(data[p])));
      break;
#endif

    case 's':
    case 'p':
      answer = SCM_PACK (data[p]);
      break;


    default:
      SCM_MISC_ERROR ("unrecognized field type: ~S",
		      scm_list_1 (SCM_MAKE_CHAR (field_type)));
    }

  return answer;
}
#undef FUNC_NAME


SCM_DEFINE (scm_struct_set_x, "struct-set!", 3, 0, 0,
            (SCM handle, SCM pos, SCM val),
	    "Set the slot of the structure @var{handle} with index @var{pos}\n"
	    "to @var{val}.  Signal an error if the slot can not be written\n"
	    "to.")
#define FUNC_NAME s_scm_struct_set_x
{
  scm_t_bits * data;
  SCM layout;
  size_t layout_len;
  size_t p;
  int n_fields;
  const char *fields_desc;
  char field_type = 0;

  SCM_VALIDATE_STRUCT (1, handle);

  layout = SCM_STRUCT_LAYOUT (handle);
  data = SCM_STRUCT_DATA (handle);
  p = scm_to_size_t (pos);

  fields_desc = scm_i_symbol_chars (layout);
  layout_len = scm_i_symbol_length (layout);
  if (SCM_STRUCT_VTABLE_FLAGS (handle) & SCM_STRUCTF_LIGHT)
    /* no extra words */
    n_fields = layout_len / 2;
  else
    n_fields = data[scm_struct_i_n_words];

  SCM_ASSERT_RANGE (1, pos, p < n_fields);

  if (p * 2 < layout_len)
    {
      char set_x;
      field_type = fields_desc[p * 2];
      set_x = fields_desc [p * 2 + 1];
      if (set_x != 'w')
	SCM_MISC_ERROR ("set! denied for field ~A", scm_list_1 (pos));
    }
  else if (fields_desc[layout_len - 1] == 'W')    
    field_type = fields_desc[layout_len - 2];
  else
    SCM_MISC_ERROR ("set! denied for field ~A", scm_list_1 (pos));
  
  switch (field_type)
    {
    case 'u':
      data[p] = SCM_NUM2ULONG (3, val);
      break;

#if 0
    case 'i':
      data[p] = SCM_NUM2LONG (3, val);
      break;

    case 'd':
      *((double *)&(data[p])) = scm_num2dbl (val, (char *)SCM_ARG3);
      break;
#endif

    case 'p':
      data[p] = SCM_UNPACK (val);
      break;

    case 's':
      SCM_MISC_ERROR ("self fields immutable", SCM_EOL);

    default:
      SCM_MISC_ERROR ("unrecognized field type: ~S",
		      scm_list_1 (SCM_MAKE_CHAR (field_type)));
    }

  return val;
}
#undef FUNC_NAME


SCM_DEFINE (scm_struct_vtable, "struct-vtable", 1, 0, 0, 
            (SCM handle),
	    "Return the vtable structure that describes the type of @var{struct}.")
#define FUNC_NAME s_scm_struct_vtable
{
  SCM_VALIDATE_STRUCT (1, handle);
  return SCM_STRUCT_VTABLE (handle);
}
#undef FUNC_NAME


SCM_DEFINE (scm_struct_vtable_tag, "struct-vtable-tag", 1, 0, 0, 
            (SCM handle),
	    "Return the vtable tag of the structure @var{handle}.")
#define FUNC_NAME s_scm_struct_vtable_tag
{
  SCM_VALIDATE_VTABLE (1, handle);
  return scm_from_ulong (((unsigned long)SCM_STRUCT_DATA (handle)) >> 3);
}
#undef FUNC_NAME

/* {Associating names and classes with vtables}
 *
 * The name of a vtable should probably be stored as a slot.  This is
 * a backward compatible solution until agreement has been achieved on
 * how to associate names with vtables.
 */

unsigned long
scm_struct_ihashq (SCM obj, unsigned long n)
{
  /* The length of the hash table should be a relative prime it's not
     necessary to shift down the address.  */
  return SCM_UNPACK (obj) % n;
}

SCM
scm_struct_create_handle (SCM obj)
{
  SCM handle = scm_hash_fn_create_handle_x (scm_struct_table,
					    obj,
					    SCM_BOOL_F,
					    scm_struct_ihashq,
					    scm_sloppy_assq,
					    0);
  if (scm_is_false (SCM_CDR (handle)))
    SCM_SETCDR (handle, scm_cons (SCM_BOOL_F, SCM_BOOL_F));
  return handle;
}

SCM_DEFINE (scm_struct_vtable_name, "struct-vtable-name", 1, 0, 0, 
            (SCM vtable),
	    "Return the name of the vtable @var{vtable}.")
#define FUNC_NAME s_scm_struct_vtable_name
{
  SCM_VALIDATE_VTABLE (1, vtable);
  return SCM_STRUCT_TABLE_NAME (SCM_CDR (scm_struct_create_handle (vtable)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_struct_vtable_name_x, "set-struct-vtable-name!", 2, 0, 0, 
            (SCM vtable, SCM name),
	    "Set the name of the vtable @var{vtable} to @var{name}.")
#define FUNC_NAME s_scm_set_struct_vtable_name_x
{
  SCM_VALIDATE_VTABLE (1, vtable);
  SCM_VALIDATE_SYMBOL (2, name);
  SCM_SET_STRUCT_TABLE_NAME (SCM_CDR (scm_struct_create_handle (vtable)),
			     name);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




void
scm_print_struct (SCM exp, SCM port, scm_print_state *pstate)
{
  if (scm_is_true (scm_procedure_p (SCM_STRUCT_PRINTER (exp))))
    scm_printer_apply (SCM_STRUCT_PRINTER (exp), exp, port, pstate);
  else
    {
      SCM vtable = SCM_STRUCT_VTABLE (exp);
      SCM name = scm_struct_vtable_name (vtable);
      scm_puts ("#<", port);
      if (scm_is_true (name))
	scm_display (name, port);
      else
	scm_puts ("struct", port);
      scm_putc (' ', port);
      scm_uintprint (SCM_UNPACK (vtable), 16, port);
      scm_putc (':', port);
      scm_uintprint (SCM_UNPACK (exp), 16, port);
      scm_putc ('>', port);
    }
}

void
scm_struct_prehistory ()
{
  scm_i_structs_to_free = SCM_EOL;
  scm_c_hook_add (&scm_before_sweep_c_hook, scm_struct_gc_init, 0, 0);
  /* With the new lazy sweep GC, the point at which the entire heap is
     swept is just before the mark phase. */
  scm_c_hook_add (&scm_before_mark_c_hook, scm_free_structs, 0, 0);
}

void
scm_init_struct ()
{
  scm_struct_table
    = scm_permanent_object (scm_make_weak_key_hash_table (scm_from_int (31)));
  required_vtable_fields = scm_from_locale_string ("prsrpw");
  scm_permanent_object (required_vtable_fields);

  scm_i_vtable_vtable_no_extra_fields =
    scm_permanent_object
    (scm_make_vtable_vtable (scm_nullstr, SCM_INUM0, SCM_EOL));

  scm_c_define ("vtable-index-layout", scm_from_int (scm_vtable_index_layout));
  scm_c_define ("vtable-index-vtable", scm_from_int (scm_vtable_index_vtable));
  scm_c_define ("vtable-index-printer",
		scm_from_int (scm_vtable_index_printer));
  scm_c_define ("vtable-offset-user", scm_from_int (scm_vtable_offset_user));
#include "libguile/struct.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
