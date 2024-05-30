/* classes: h_files */

#ifndef SCM_OBJPROP_H
#define SCM_OBJPROP_H

/* Copyright (C) 1995,2000,2001, 2006 Free Software Foundation, Inc.
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



SCM_API SCM scm_object_properties (SCM obj);
SCM_API SCM scm_set_object_properties_x (SCM obj, SCM plist);
SCM_API SCM scm_object_property (SCM obj, SCM key);
SCM_API SCM scm_set_object_property_x (SCM obj, SCM key, SCM val);
SCM_API void scm_init_objprop (void);

#endif  /* SCM_OBJPROP_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
