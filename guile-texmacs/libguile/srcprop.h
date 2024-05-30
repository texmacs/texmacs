/* classes: h_files */

#ifndef SCM_SRCPROP_H
#define SCM_SRCPROP_H

/* Copyright (C) 1995,1996,2000,2001, 2006 Free Software Foundation, Inc.
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



/* {The old whash table interface}
 * *fixme* This is a temporary solution until weak hash table access
 * has been optimized for speed (which is quite necessary, if they are
 * used for recording of source code positions...)
 */

#define scm_whash_handle SCM

#define scm_whash_get_handle(whash, key) scm_hash_fn_get_handle (whash, key, scm_ihashq, scm_sloppy_assq, 0)
#define SCM_WHASHFOUNDP(h) (scm_is_true (h))
#define SCM_WHASHREF(whash, handle) SCM_CDR (handle)
#define SCM_WHASHSET(whash, handle, obj) SCM_SETCDR (handle, obj)
#define scm_whash_create_handle(whash, key) scm_hash_fn_create_handle_x (whash, key, SCM_UNSPECIFIED, scm_ihashq, scm_sloppy_assq, 0)
#define scm_whash_lookup(whash, obj) scm_hash_fn_ref (whash, obj, SCM_BOOL_F, scm_ihashq, scm_sloppy_assq, 0)
#define scm_whash_insert(whash, key, obj) \
do { \
  register SCM w = (whash); \
  SCM_WHASHSET (w, scm_whash_create_handle (w, key), obj); \
} while (0)


/* {Source properties}
 */
#define SCM_PROCTRACEP(x) (scm_is_true (scm_procedure_property (x, scm_sym_trace)))
#define SCM_SOURCE_PROPERTY_FLAG_BREAK 1

SCM_API scm_t_bits scm_tc16_srcprops;

SCM_API SCM scm_sym_filename;
SCM_API SCM scm_sym_copy;
SCM_API SCM scm_sym_line;
SCM_API SCM scm_sym_column;
SCM_API SCM scm_sym_breakpoint;



SCM_API int scm_c_source_property_breakpoint_p (SCM form);
SCM_API SCM scm_srcprops_to_plist (SCM obj);
SCM_API SCM scm_make_srcprops (long line, int col, SCM fname, SCM copy, SCM plist);
SCM_API SCM scm_source_property (SCM obj, SCM key);
SCM_API SCM scm_set_source_property_x (SCM obj, SCM key, SCM datum);
SCM_API SCM scm_source_properties (SCM obj);
SCM_API SCM scm_set_source_properties_x (SCM obj, SCM props);
SCM_API void scm_finish_srcprop (void);
SCM_API void scm_init_srcprop (void);

#if SCM_ENABLE_DEPRECATED == 1
#define SRCBRKP(x) (scm_source_property_breakpoint_p (x))
#endif

#endif  /* SCM_SRCPROP_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
