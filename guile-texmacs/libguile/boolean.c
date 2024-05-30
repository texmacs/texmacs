/*	Copyright (C) 1995, 1996, 2000, 2001, 2006, 2008 Free Software Foundation, Inc.
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
# include <config.h>
#endif

#include "libguile/_scm.h"

#include "libguile/validate.h"
#include "libguile/boolean.h"
#include "libguile/lang.h"
#include "libguile/tags.h"




SCM_DEFINE (scm_not, "not", 1, 0, 0, 
            (SCM x),
            "Return @code{#t} iff @var{x} is @code{#f}, else return @code{#f}.")
#define FUNC_NAME s_scm_not
{
  return scm_from_bool (scm_is_false (x) || SCM_NILP (x));
}
#undef FUNC_NAME


SCM_DEFINE (scm_boolean_p, "boolean?", 1, 0, 0, 
           (SCM obj),
            "Return @code{#t} iff @var{obj} is either @code{#t} or @code{#f}.")
#define FUNC_NAME s_scm_boolean_p
{
  return scm_from_bool (scm_is_bool (obj) || SCM_NILP (obj));
}
#undef FUNC_NAME

int
scm_is_bool (SCM x)
{
  return scm_is_eq (x, SCM_BOOL_F) || scm_is_eq (x, SCM_BOOL_T);
}

int
scm_to_bool (SCM x)
{
  if (scm_is_eq (x, SCM_BOOL_F))
    return 0;
  else if (scm_is_eq (x, SCM_BOOL_T))
    return 1;
  else    
    scm_wrong_type_arg (NULL, 0, x);
}

void
scm_init_boolean ()
{
#include "libguile/boolean.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
