/* classes: h_files */

#ifndef SCM_OPTIONS_H
#define SCM_OPTIONS_H

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



typedef struct scm_t_option
{
  unsigned int type;
  const char *name;
  scm_t_bits val;
  char *doc;
} scm_t_option;


#define SCM_OPTION_BOOLEAN 0
#define SCM_OPTION_INTEGER 1
#define SCM_OPTION_SCM     2


SCM_API SCM scm_options (SCM, scm_t_option [], unsigned int, const char*);
SCM_API void scm_init_opts (SCM (*) (SCM), scm_t_option [], unsigned int n);
SCM_API void scm_init_options (void);

#endif  /* SCM_OPTIONS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
