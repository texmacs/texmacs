/* classes: h_files */

#ifndef SCM_HASHTAB_H
#define SCM_HASHTAB_H

/* Copyright (C) 1995,1996,1999,2000,2001, 2003, 2004, 2006 Free Software Foundation, Inc.
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

#include "weaks.h"



#define SCM_HASHTABLEF_WEAK_CAR SCM_WVECTF_WEAK_KEY
#define SCM_HASHTABLEF_WEAK_CDR SCM_WVECTF_WEAK_VALUE

SCM_API scm_t_bits scm_tc16_hashtable;

#define SCM_HASHTABLE_P(x)	   SCM_SMOB_PREDICATE (scm_tc16_hashtable, x)
#define SCM_VALIDATE_HASHTABLE(pos, arg) \
  SCM_MAKE_VALIDATE_MSG (pos, arg, HASHTABLE_P, "hash-table")
#define SCM_HASHTABLE_VECTOR(h)  SCM_SMOB_OBJECT (h)
#define SCM_SET_HASHTABLE_VECTOR(x, v) SCM_SET_SMOB_OBJECT ((x), (v))
#define SCM_HASHTABLE(x)	   ((scm_t_hashtable *) SCM_SMOB_DATA_2 (x))
#define SCM_HASHTABLE_NEXT(x)	   SCM_SMOB_OBJECT_3 (x)
#define SCM_HASHTABLE_NEXTLOC(x)   SCM_SMOB_OBJECT_3_LOC (x)
#define SCM_SET_HASHTABLE_NEXT(x, n) SCM_SET_SMOB_OBJECT_3 ((x), (n))
#define SCM_HASHTABLE_FLAGS(x)	   (SCM_HASHTABLE (x)->flags)
#define SCM_HASHTABLE_WEAK_KEY_P(x) \
  (SCM_HASHTABLE_FLAGS (x) & SCM_HASHTABLEF_WEAK_CAR)
#define SCM_HASHTABLE_WEAK_VALUE_P(x) \
  (SCM_HASHTABLE_FLAGS (x) & SCM_HASHTABLEF_WEAK_CDR)
#define SCM_HASHTABLE_DOUBLY_WEAK_P(x)				\
  ((SCM_HASHTABLE_FLAGS (x)					\
    & (SCM_HASHTABLEF_WEAK_CAR | SCM_HASHTABLEF_WEAK_CDR))	\
   == (SCM_HASHTABLEF_WEAK_CAR | SCM_HASHTABLEF_WEAK_CDR))
#define SCM_HASHTABLE_WEAK_P(x)	   SCM_HASHTABLE_FLAGS (x)
#define SCM_HASHTABLE_N_ITEMS(x)   (SCM_HASHTABLE (x)->n_items)
#define SCM_SET_HASHTABLE_N_ITEMS(x, n)   (SCM_HASHTABLE (x)->n_items = n)
#define SCM_HASHTABLE_INCREMENT(x) (SCM_HASHTABLE_N_ITEMS(x)++)
#define SCM_HASHTABLE_DECREMENT(x) (SCM_HASHTABLE_N_ITEMS(x)--)
#define SCM_HASHTABLE_UPPER(x)     (SCM_HASHTABLE (x)->upper)
#define SCM_HASHTABLE_LOWER(x)     (SCM_HASHTABLE (x)->lower)

#define SCM_HASHTABLE_N_BUCKETS(h) \
 SCM_SIMPLE_VECTOR_LENGTH (SCM_HASHTABLE_VECTOR (h))
#define SCM_HASHTABLE_BUCKET(h, i) \
  SCM_SIMPLE_VECTOR_REF (SCM_HASHTABLE_VECTOR (h), i)
#define SCM_SET_HASHTABLE_BUCKET(h, i, x) \
  SCM_SIMPLE_VECTOR_SET (SCM_HASHTABLE_VECTOR (h), i, x)

typedef struct scm_t_hashtable {
  int flags;			/* properties of table */
  unsigned long n_items;	/* number of items in table */
  unsigned long lower;		/* when to shrink */
  unsigned long upper;		/* when to grow */
  int size_index;		/* index into hashtable_size */
  int min_size_index;		/* minimum size_index */
  unsigned long (*hash_fn) ();  /* for rehashing after a GC. */
} scm_t_hashtable;



#if 0
typedef unsigned int scm_t_hash_fn (SCM obj, unsigned int d, void *closure);
typedef SCM scm_t_assoc_fn (SCM key, SCM alist, void *closure);
typedef SCM scm_t_delete_fn (SCM elt, SCM list);
#endif

SCM_API SCM scm_vector_to_hash_table (SCM vector);
SCM_API SCM scm_c_make_hash_table (unsigned long k);
SCM_API SCM scm_make_hash_table (SCM n);
SCM_API SCM scm_make_weak_key_hash_table (SCM k);
SCM_API SCM scm_make_weak_value_hash_table (SCM k);
SCM_API SCM scm_make_doubly_weak_hash_table (SCM k);

SCM_API SCM scm_hash_table_p (SCM h);
SCM_API SCM scm_weak_key_hash_table_p (SCM h);
SCM_API SCM scm_weak_value_hash_table_p (SCM h);
SCM_API SCM scm_doubly_weak_hash_table_p (SCM h);

SCM_API void scm_i_rehash (SCM table, unsigned long (*hash_fn)(), void *closure, const char*func_name);
SCM_API void scm_i_scan_weak_hashtables (void);

SCM_API SCM scm_hash_fn_get_handle (SCM table, SCM obj, unsigned long (*hash_fn) (), SCM (*assoc_fn) (), void * closure);
SCM_API SCM scm_hash_fn_create_handle_x (SCM table, SCM obj, SCM init, unsigned long (*hash_fn) (), SCM (*assoc_fn) (), void * closure);
SCM_API SCM scm_hash_fn_ref (SCM table, SCM obj, SCM dflt, unsigned long (*hash_fn) (), SCM (*assoc_fn) (), void * closure);
SCM_API SCM scm_hash_fn_set_x (SCM table, SCM obj, SCM val, unsigned long (*hash_fn) (), SCM (*assoc_fn) (), void * closure);
SCM_API SCM scm_hash_fn_remove_x (SCM table, SCM obj, unsigned long (*hash_fn) (), SCM (*assoc_fn) (), void * closure);
SCM_API SCM scm_internal_hash_fold (SCM (*fn) (), void *closure, SCM init, SCM table);
SCM_API void scm_internal_hash_for_each_handle (SCM (*fn) (), void *closure, SCM table);
SCM_API SCM scm_hash_clear_x (SCM table);

SCM_API SCM scm_hashq_get_handle (SCM table, SCM obj);
SCM_API SCM scm_hashq_create_handle_x (SCM table, SCM obj, SCM init);
SCM_API SCM scm_hashq_ref (SCM table, SCM obj, SCM dflt);
SCM_API SCM scm_hashq_set_x (SCM table, SCM obj, SCM val);
SCM_API SCM scm_hashq_remove_x (SCM table, SCM obj);
SCM_API SCM scm_hashv_get_handle (SCM table, SCM obj);
SCM_API SCM scm_hashv_create_handle_x (SCM table, SCM obj, SCM init);
SCM_API SCM scm_hashv_ref (SCM table, SCM obj, SCM dflt);
SCM_API SCM scm_hashv_set_x (SCM table, SCM obj, SCM val);
SCM_API SCM scm_hashv_remove_x (SCM table, SCM obj);
SCM_API SCM scm_hash_get_handle (SCM table, SCM obj);
SCM_API SCM scm_hash_create_handle_x (SCM table, SCM obj, SCM init);
SCM_API SCM scm_hash_ref (SCM table, SCM obj, SCM dflt);
SCM_API SCM scm_hash_set_x (SCM table, SCM obj, SCM val);
SCM_API SCM scm_hash_remove_x (SCM table, SCM obj);
SCM_API SCM scm_hashx_get_handle (SCM hash, SCM assoc, SCM table, SCM obj);
SCM_API SCM scm_hashx_create_handle_x (SCM hash, SCM assoc, SCM table, SCM obj, SCM init);
SCM_API SCM scm_hashx_ref (SCM hash, SCM assoc, SCM table, SCM obj, SCM dflt);
SCM_API SCM scm_hashx_set_x (SCM hash, SCM assoc, SCM table, SCM obj, SCM val);
SCM_API SCM scm_hashx_remove_x (SCM hash, SCM assoc, SCM table, SCM obj);
SCM_API SCM scm_hash_fold (SCM proc, SCM init, SCM hash);
SCM_API SCM scm_hash_for_each (SCM proc, SCM hash);
SCM_API SCM scm_hash_for_each_handle (SCM proc, SCM hash);
SCM_API SCM scm_hash_map_to_list (SCM proc, SCM hash);
SCM_API void scm_hashtab_prehistory (void);
SCM_API void scm_init_hashtab (void);

#endif  /* SCM_HASHTAB_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
