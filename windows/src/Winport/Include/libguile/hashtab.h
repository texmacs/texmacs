/* classes: h_files */

#ifndef HASHTABH
#define HASHTABH
/*	Copyright (C) 1995, 1996, 1999, 2000 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */

#include "libguile/__scm.h"



#if 0
typedef unsigned int scm_hash_fn_t (SCM obj, unsigned int d, void *closure);
typedef SCM scm_assoc_fn_t (SCM key, SCM alist, void *closure);
typedef SCM scm_delete_fn_t (SCM elt, SCM list);
#endif

GUILE_API extern SCM scm_hash_fn_get_handle (SCM table, SCM obj, unsigned int (*hash_fn) (), SCM (*assoc_fn) (), void * closure);
GUILE_API extern SCM scm_hash_fn_create_handle_x (SCM table, SCM obj, SCM init, unsigned int (*hash_fn) (), SCM (*assoc_fn) (), void * closure);
GUILE_API extern SCM scm_hash_fn_ref (SCM table, SCM obj, SCM dflt, unsigned int (*hash_fn) (), SCM (*assoc_fn) (), void * closure);
GUILE_API extern SCM scm_hash_fn_set_x (SCM table, SCM obj, SCM val, unsigned int (*hash_fn) (), SCM (*assoc_fn) (), void * closure);
GUILE_API extern SCM scm_hash_fn_remove_x (SCM table, SCM obj, unsigned int (*hash_fn) (), SCM (*assoc_fn) (), SCM (*delete_fn) (), void * closure);
GUILE_API extern SCM scm_internal_hash_fold (SCM (*fn) (), void *closure, SCM init, SCM table);

GUILE_API extern SCM scm_hashq_get_handle (SCM table, SCM obj);
GUILE_API extern SCM scm_hashq_create_handle_x (SCM table, SCM obj, SCM init);
GUILE_API extern SCM scm_hashq_ref (SCM table, SCM obj, SCM dflt);
GUILE_API extern SCM scm_hashq_set_x (SCM table, SCM obj, SCM val);
GUILE_API extern SCM scm_hashq_remove_x (SCM table, SCM obj);
GUILE_API extern SCM scm_hashv_get_handle (SCM table, SCM obj);
GUILE_API extern SCM scm_hashv_create_handle_x (SCM table, SCM obj, SCM init);
GUILE_API extern SCM scm_hashv_ref (SCM table, SCM obj, SCM dflt);
GUILE_API extern SCM scm_hashv_set_x (SCM table, SCM obj, SCM val);
GUILE_API extern SCM scm_hashv_remove_x (SCM table, SCM obj);
GUILE_API extern SCM scm_hash_get_handle (SCM table, SCM obj);
GUILE_API extern SCM scm_hash_create_handle_x (SCM table, SCM obj, SCM init);
GUILE_API extern SCM scm_hash_ref (SCM table, SCM obj, SCM dflt);
GUILE_API extern SCM scm_hash_set_x (SCM table, SCM obj, SCM val);
GUILE_API extern SCM scm_hash_remove_x (SCM table, SCM obj);
GUILE_API extern SCM scm_hashx_get_handle (SCM hash, SCM assoc, SCM table, SCM obj);
GUILE_API extern SCM scm_hashx_create_handle_x (SCM hash, SCM assoc, SCM table, SCM obj, SCM init);
GUILE_API extern SCM scm_hashx_ref (SCM hash, SCM assoc, SCM table, SCM obj, SCM dflt);
GUILE_API extern SCM scm_hashx_set_x (SCM hash, SCM assoc, SCM table, SCM obj, SCM val);
GUILE_API extern SCM scm_hashx_remove_x (SCM hash, SCM assoc, SCM del, SCM table, SCM obj);
GUILE_API extern SCM scm_hash_fold (SCM proc, SCM init, SCM hash);
GUILE_API extern void scm_init_hashtab (void);

#endif  /* HASHTABH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
