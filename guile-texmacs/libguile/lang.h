/* classes: h_files */

#ifndef SCM_LANG_H
#define SCM_LANG_H

/* Copyright (C) 1998, 2004, 2006 Free Software Foundation, Inc.
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



#if SCM_ENABLE_ELISP

#define SCM_NILP(x) (scm_is_eq ((x), SCM_ELISP_NIL))

SCM_API void scm_init_lang (void);

#else  /* ! SCM_ENABLE_ELISP */

#define SCM_NILP(x) 0

#endif /* ! SCM_ENABLE_ELISP */

#define SCM_NULL_OR_NIL_P(x) (scm_is_null (x) || SCM_NILP (x))

#endif  /* SCM_LANG_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
