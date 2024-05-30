/* classes: h_files */

#ifndef SCM_SCRIPT_H
#define SCM_SCRIPT_H

/* Copyright (C) 1997,1998,2000, 2006 Free Software Foundation, Inc.
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


SCM_API char *scm_find_executable (const char *name);
SCM_API char *scm_find_impl_file (char *exec_path,
				  const char *generic_name,
				  const char *initname,
				  const char *sep);
SCM_API char **scm_get_meta_args (int argc, char **argv);
SCM_API int scm_count_argv (char **argv);
SCM_API void scm_shell_usage (int fatal, char *message);
SCM_API SCM scm_compile_shell_switches (int argc, char **argv);
SCM_API void scm_shell (int argc, char **argv);
SCM_API char *scm_usage_name;
SCM_API void scm_init_script (void);

#endif  /* SCM_SCRIPT_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
