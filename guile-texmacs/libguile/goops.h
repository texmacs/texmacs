/* classes: h_files */

#ifndef SCM_GOOPS_H
#define SCM_GOOPS_H

/* Copyright (C) 1998,1999,2000,2001,2002,2003, 2006 Free Software Foundation, Inc.
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



/* This software is a derivative work of other copyrighted softwares; the
 * copyright notices of these softwares are placed in the file COPYRIGHTS
 *
 * This file is based upon stklos.h from the STk distribution by
 * Erick Gallesio <eg@unice.fr>.
 */

#include "libguile/__scm.h"

#include "libguile/validate.h"

/*
 * scm_class_class
 */

#define SCM_CLASS_CLASS_LAYOUT "prsrpwpopopwururururururururpwpwpwpwpwpwpwpwpwpwpwpw"

#define scm_si_layout		  0 /* the struct layout */
#define scm_si_vtable		  1
#define scm_si_print		  2 /* the struct print closure */
#define scm_si_proc		  3
#define scm_si_setter		  4

#define scm_si_goops_fields	  5

/* Defined in libguile/objects.h:
#define scm_si_redefined	  5    The class to which class was redefined.
#define scm_si_hashsets	 	  6
*/
#define scm_si_name 		 14 /* a symbol */
#define scm_si_direct_supers 	 15 /* (class ...) */
#define scm_si_direct_slots	 16 /* ((name . options) ...) */
#define scm_si_direct_subclasses 17 /* (class ...) */
#define scm_si_direct_methods	 18 /* (methods ...) */
#define scm_si_cpl		 19 /* (class ...) */
#define scm_si_slotdef_class	 20
#define scm_si_slots		 21 /* ((name . options) ...) */
#define scm_si_name_access	 22
#define scm_si_keyword_access	 23
#define scm_si_nfields		 24 /* an integer */
#define scm_si_environment	 25 /* The environment in which class is built  */
#define SCM_N_CLASS_SLOTS	 26

typedef struct scm_t_method {
  SCM generic_function;
  SCM specializers;
  SCM procedure;
} scm_t_method;

#define SCM_METHOD(obj) ((scm_t_method *) SCM_STRUCT_DATA (obj))

#define SCM_CLASSF_SIMPLE_METHOD    (0x004 << 20)
#define SCM_CLASSF_ACCESSOR_METHOD  (0x008 << 20)

/* Defined in libguile/objects.c */
/* #define SCM_CLASSF_PURE_GENERIC  (0x010 << 20) */

#define SCM_CLASSF_FOREIGN	    (0x020 << 20)
#define SCM_CLASSF_METACLASS        (0x040 << 20)

/* Defined in libguile/objects.c */
/* #define SCM_CLASSF_GOOPS_VALID   (0x080 << 20) */
/* #define SCM_CLASSF_GOOPS         (0x100 << 20) */
#define SCM_CLASSF_GOOPS_OR_VALID (SCM_CLASSF_GOOPS | SCM_CLASSF_GOOPS_VALID)

#define SCM_CLASSF_INHERIT	 (~(SCM_CLASSF_PURE_GENERIC \
				    | SCM_CLASSF_SIMPLE_METHOD \
				    | SCM_CLASSF_ACCESSOR_METHOD \
				    | SCM_STRUCTF_LIGHT) \
				  & SCM_CLASSF_MASK)

#define SCM_INST(x)	       SCM_STRUCT_DATA (x)

/* Also defined in libguile/objects.c */
#define SCM_CLASS_OF(x)        SCM_STRUCT_VTABLE (x)
#define SCM_ACCESSORS_OF(x)    (SCM_PACK (SCM_STRUCT_VTABLE_DATA (x)[scm_si_getters_n_setters]))

#define SCM_CLASSP(x) \
  (SCM_STRUCTP (x) && SCM_STRUCT_VTABLE_FLAGS (x) & SCM_CLASSF_METACLASS)
#define SCM_VALIDATE_CLASS(pos, x) SCM_MAKE_VALIDATE_MSG (pos, x, CLASSP, "class")

#define SCM_INSTANCEP(x) \
  (SCM_STRUCTP (x) && (SCM_STRUCT_VTABLE_FLAGS (x) & SCM_CLASSF_GOOPS))
#define SCM_VALIDATE_INSTANCE(pos, x) SCM_MAKE_VALIDATE_MSG (pos, x, INSTANCEP, "instance")

#define SCM_PUREGENERICP(x) \
  (SCM_STRUCTP (x) && (SCM_STRUCT_VTABLE_FLAGS (x) & SCM_CLASSF_PURE_GENERIC))
#define SCM_VALIDATE_PUREGENERIC(pos, x) SCM_MAKE_VALIDATE_MSG (pos, x, PUREGENERICP, "pure generic function")

#define SCM_ACCESSORP(x) \
  (SCM_STRUCTP (x) && (SCM_STRUCT_VTABLE_FLAGS (x) & SCM_CLASSF_ACCESSOR_METHOD))
#define SCM_VALIDATE_ACCESSOR(pos, x) SCM_MAKE_VALIDATE_MSG (pos, x, ACCESSORP, "accessor")

#define SCM_SLOT(x, i)         (SCM_PACK (SCM_INST (x) [i]))
#define SCM_SET_SLOT(x, i, v)  (SCM_INST (x) [i] = SCM_UNPACK (v))
#define SCM_INSTANCE_HASH(c, i) (SCM_INST (c) [scm_si_hashsets + (i)])
#define SCM_SET_HASHSET(c, i, h)  (SCM_INST (c) [scm_si_hashsets + (i)] = (h))

#define SCM_SUBCLASSP(c1, c2)  (scm_is_true (scm_c_memq (c2, SCM_SLOT (c1, scm_si_cpl))))
#define SCM_IS_A_P(x, c) \
  (SCM_INSTANCEP (x) && SCM_SUBCLASSP (SCM_CLASS_OF (x), c))

#define SCM_GENERICP(x) \
  (SCM_INSTANCEP (x) && SCM_SUBCLASSP (SCM_CLASS_OF (x), scm_class_generic))
#define SCM_VALIDATE_GENERIC(pos, x) SCM_MAKE_VALIDATE_MSG (pos, x, GENERICP, "generic function")

#define SCM_METHODP(x) \
  (SCM_INSTANCEP (x) && SCM_SUBCLASSP (SCM_CLASS_OF (x), scm_class_method))
#define SCM_VALIDATE_METHOD(pos, x) SCM_MAKE_VALIDATE_MSG (pos, x, METHODP, "method")

#define SCM_MCACHE_N_SPECIALIZED(C) SCM_CADDR (C)
#define SCM_SET_MCACHE_N_SPECIALIZED(C, X) SCM_SETCAR (SCM_CDDR (C), X)

#define SCM_INITIAL_MCACHE_SIZE	  1

#define scm_si_getters_n_setters scm_si_name_access

#define scm_si_constructor	 SCM_N_CLASS_SLOTS
#define scm_si_destructor	 SCM_N_CLASS_SLOTS + 1

#define scm_si_methods		 0  /* offset of methods slot in a <generic> */
#define scm_si_n_specialized	 1
#define scm_si_used_by		 2
#define scm_si_cache_mutex	 3

#define scm_si_generic_function	 0  /* offset of gf    slot in a <method> */
#define scm_si_specializers	 1  /* offset of spec. slot in a <method> */

#define scm_si_procedure 	 2  /* offset of proc. slot in a <method> */
#define scm_si_code_table	 3  /* offset of code. slot in a <method> */

/* C interface */
SCM_API SCM scm_class_boolean;
SCM_API SCM scm_class_char;
SCM_API SCM scm_class_pair;
SCM_API SCM scm_class_procedure;
SCM_API SCM scm_class_string;
SCM_API SCM scm_class_symbol;
SCM_API SCM scm_class_procedure_with_setter;
SCM_API SCM scm_class_primitive_generic;
SCM_API SCM scm_class_vector, scm_class_null;
SCM_API SCM scm_class_real;
SCM_API SCM scm_class_complex;
SCM_API SCM scm_class_integer;
SCM_API SCM scm_class_fraction;
SCM_API SCM scm_class_unknown;
SCM_API SCM *scm_port_class;
SCM_API SCM *scm_smob_class;
SCM_API SCM scm_class_top;
SCM_API SCM scm_class_object;
SCM_API SCM scm_class_class;
SCM_API SCM scm_class_applicable;
SCM_API SCM scm_class_entity;
SCM_API SCM scm_class_entity_with_setter;
SCM_API SCM scm_class_generic;
SCM_API SCM scm_class_generic_with_setter;
SCM_API SCM scm_class_accessor;
SCM_API SCM scm_class_extended_generic;
SCM_API SCM scm_class_extended_generic_with_setter;
SCM_API SCM scm_class_extended_accessor;
SCM_API SCM scm_class_method;
SCM_API SCM scm_class_simple_method;
SCM_API SCM scm_class_accessor_method;
SCM_API SCM scm_class_procedure_class;
SCM_API SCM scm_class_operator_class;
SCM_API SCM scm_class_operator_with_setter_class;
SCM_API SCM scm_class_entity_class;
SCM_API SCM scm_class_number;
SCM_API SCM scm_class_list;
SCM_API SCM scm_class_keyword;
SCM_API SCM scm_class_port;
SCM_API SCM scm_class_input_output_port;
SCM_API SCM scm_class_input_port;
SCM_API SCM scm_class_output_port;
SCM_API SCM scm_class_foreign_class;
SCM_API SCM scm_class_foreign_object;
SCM_API SCM scm_class_foreign_slot;
SCM_API SCM scm_class_self;
SCM_API SCM scm_class_protected;
SCM_API SCM scm_class_opaque;
SCM_API SCM scm_class_read_only;
SCM_API SCM scm_class_protected_opaque;
SCM_API SCM scm_class_protected_read_only;
SCM_API SCM scm_class_scm;
SCM_API SCM scm_class_int;
SCM_API SCM scm_class_float;
SCM_API SCM scm_class_double;
SCM_API const char *scm_s_slot_set_x;

SCM_API SCM scm_no_applicable_method;

SCM_API SCM scm_module_goops;

SCM_API SCM scm_goops_version (void);
SCM_API SCM scm_oldfmt (SCM);
SCM_API char *scm_c_oldfmt0 (char *);
SCM_API char *scm_c_oldfmt (char *, int n);
SCM_API void scm_load_goops (void);
SCM_API SCM scm_make_foreign_object (SCM cls, SCM initargs);
SCM_API SCM scm_make_class (SCM meta, char *s_name, SCM supers, size_t size,
			    void * (*constructor) (SCM initargs),
			    size_t (*destructor) (void *));
SCM_API void scm_add_slot (SCM c, char *slot, SCM slot_class,
			   SCM (*getter) (SCM obj),
			   SCM (*setter) (SCM obj, SCM x),
			   char *accessor_name);
SCM_API SCM scm_wrap_object (SCM c, void *);
SCM_API SCM scm_wrap_component (SCM c, SCM obj, void *);
SCM_API SCM scm_ensure_accessor (SCM name);
SCM_API void scm_add_method (SCM gf, SCM m);
SCM_API SCM scm_class_of (SCM obj);

/* Low level functions exported */
SCM_API SCM scm_make_next_method (SCM methods, SCM args, SCM gf);
SCM_API SCM scm_basic_basic_make_class (SCM c, SCM name, SCM dsupers, SCM dslots);
SCM_API SCM scm_basic_make_class (SCM c, SCM name, SCM dsupers, SCM dslots);

/* Primitives exported */
SCM_API SCM scm_sys_allocate_instance (SCM c, SCM initargs);
SCM_API SCM scm_sys_set_object_setter_x (SCM obj, SCM setter);
SCM_API SCM scm_slot_ref (SCM obj, SCM slot_name);
SCM_API SCM scm_slot_set_x (SCM obj, SCM slot_name, SCM value);

SCM_API SCM scm_compute_applicable_methods (SCM gf, SCM args, long len, int scm_find_method);
SCM_API SCM scm_sys_compute_applicable_methods (SCM gf, SCM args);
#ifdef GUILE_DEBUG
SCM_API SCM scm_pure_generic_p (SCM obj);
#endif

SCM_API SCM scm_sys_compute_slots (SCM c);
SCM_API SCM scm_i_get_keyword (SCM key, SCM l, long len, SCM default_value, const char *subr);
SCM_API SCM scm_get_keyword (SCM key, SCM l, SCM default_value);
SCM_API SCM scm_sys_initialize_object (SCM obj, SCM initargs);
SCM_API SCM scm_sys_prep_layout_x (SCM c);
SCM_API SCM scm_sys_inherit_magic_x (SCM c, SCM dsupers);
SCM_API SCM scm_instance_p (SCM obj);
SCM_API SCM scm_class_name (SCM obj);
SCM_API SCM scm_class_direct_supers (SCM obj);
SCM_API SCM scm_class_direct_slots (SCM obj);
SCM_API SCM scm_class_direct_subclasses (SCM obj);
SCM_API SCM scm_class_direct_methods (SCM obj);
SCM_API SCM scm_class_precedence_list (SCM obj);
SCM_API SCM scm_class_slots (SCM obj);
SCM_API SCM scm_class_environment (SCM obj);
SCM_API SCM scm_generic_function_name (SCM obj);
SCM_API SCM scm_generic_function_methods (SCM obj);
SCM_API SCM scm_method_generic_function (SCM obj);
SCM_API SCM scm_method_specializers (SCM obj);
SCM_API SCM scm_method_procedure (SCM obj);
SCM_API SCM scm_accessor_method_slot_definition (SCM obj);
SCM_API SCM scm_sys_tag_body (SCM body);
SCM_API SCM scm_sys_fast_slot_ref (SCM obj, SCM index);
SCM_API SCM scm_sys_fast_slot_set_x (SCM obj, SCM index, SCM value);
SCM_API SCM scm_slot_ref_using_class (SCM cls, SCM obj, SCM slot_name);
SCM_API SCM scm_slot_set_using_class_x (SCM cls, SCM obj, SCM slot_name, SCM value);
SCM_API SCM scm_slot_bound_using_class_p (SCM cls, SCM obj, SCM slot_name);
SCM_API SCM scm_slot_exists_using_class_p (SCM cls, SCM obj, SCM slot_name);
SCM_API SCM scm_slot_bound_p (SCM obj, SCM slot_name);
SCM_API SCM scm_slot_exists_p (SCM obj, SCM slot_name);
SCM_API SCM scm_sys_modify_instance (SCM old, SCM newinst);
SCM_API SCM scm_sys_modify_class (SCM old, SCM newcls);
SCM_API SCM scm_sys_invalidate_class (SCM cls);
SCM_API SCM scm_make_method_cache (SCM gf);
SCM_API SCM scm_sys_invalidate_method_cache_x (SCM gf);
SCM_API SCM scm_generic_capability_p (SCM proc);
SCM_API SCM scm_enable_primitive_generic_x (SCM subrs);
SCM_API SCM scm_primitive_generic_generic (SCM subr);
SCM_API void scm_c_extend_primitive_generic (SCM subr, SCM extension);
SCM_API SCM stklos_version (void);
SCM_API SCM scm_make (SCM args);
SCM_API SCM scm_find_method (SCM args);
SCM_API SCM scm_sys_method_more_specific_p (SCM m1, SCM m2, SCM targs);

SCM_API SCM scm_init_goops_builtins (void);
SCM_API void scm_init_goops (void);

#if (SCM_ENABLE_DEPRECATED == 1)

#define SCM_INST_TYPE(x)       SCM_OBJ_CLASS_FLAGS (x)
#define SCM_SIMPLEMETHODP(x) \
  (SCM_STRUCTP (x) && (SCM_STRUCT_VTABLE_FLAGS (x) & SCM_CLASSF_SIMPLE_METHOD))
#define SCM_FASTMETHODP(x) \
  (SCM_STRUCTP (x) && (SCM_STRUCT_VTABLE_FLAGS (x) \
                       & (SCM_CLASSF_ACCESSOR_METHOD \
			  | SCM_CLASSF_SIMPLE_METHOD)))


#endif

#endif  /* SCM_GOOPS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
