/* classes: h_files */

#ifndef SCM_LOAD_H
#define SCM_LOAD_H

/* Copyright (C) 1995,1996,1998,2000,2001, 2006 Free Software Foundation, Inc.
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


SCM_API SCM scm_parse_path (SCM path, SCM tail);
SCM_API void scm_init_load_path (void);
SCM_API SCM scm_primitive_load (SCM filename);
SCM_API SCM scm_c_primitive_load (const char *filename);
SCM_API SCM scm_sys_package_data_dir (void);
SCM_API SCM scm_sys_library_dir (void);
SCM_API SCM scm_sys_site_dir (void);
SCM_API SCM scm_search_path (SCM path, SCM filename, SCM exts);
SCM_API SCM scm_sys_search_load_path (SCM filename);
SCM_API SCM scm_primitive_load_path (SCM filename);
SCM_API SCM scm_c_primitive_load_path (const char *filename);
SCM_API void scm_init_load (void);

#endif  /* SCM_LOAD_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
