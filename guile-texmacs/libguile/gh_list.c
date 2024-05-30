/*      Copyright (C) 1995,1996,1997, 2000, 2001, 2004, 2006, 2008 Free Software Foundation, Inc.

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


/* list manipulation */

#include "libguile/gh.h"

#if SCM_ENABLE_DEPRECATED

/* returns the length of a list */
unsigned long 
gh_length (SCM l)
{
  return gh_scm2ulong (scm_length (l));
}

/* list operations */

/* gh_list(SCM elt, ...) is implemented as a macro in gh.h. */

/* gh_append() takes a args, which is a list of lists, and appends
   them all together into a single list, which is returned.  This is
   equivalent to the Scheme procedure (append list1 list2 ...) */
SCM
gh_append (SCM args)
{
  return scm_append (args);
}

SCM
gh_append2 (SCM l1, SCM l2)
{
  return scm_append (scm_list_2 (l1, l2));
}

SCM
gh_append3(SCM l1, SCM l2, SCM l3)
{
  return scm_append (scm_list_3 (l1, l2, l3));
}

SCM
gh_append4 (SCM l1, SCM l2, SCM l3, SCM l4)
{
  return scm_append (scm_list_4 (l1, l2, l3, l4));
}

/* gh_reverse() is defined as a macro in gh.h */
/* gh_list_tail() is defined as a macro in gh.h */
/* gh_list_ref() is defined as a macro in gh.h */
/* gh_memq() is defined as a macro in gh.h */
/* gh_memv() is defined as a macro in gh.h */
/* gh_member() is defined as a macro in gh.h */
/* gh_assq() is defined as a macro in gh.h */
/* gh_assv() is defined as a macro in gh.h */
/* gh_assoc() is defined as a macro in gh.h */

/* analogous to the Scheme cons operator */
SCM 
gh_cons (SCM x, SCM y)
{
  return scm_cons (x, y);
}

/* analogous to the Scheme car operator */
SCM 
gh_car (SCM x)
{
  return scm_car (x);
}

/* analogous to the Scheme cdr operator */
SCM 
gh_cdr (SCM x)
{
  return scm_cdr (x);
}

/* now for the multiple car/cdr utility procedures */
SCM 
gh_caar (SCM x)
{
  return scm_caar (x);
}
SCM 
gh_cadr (SCM x)
{
  return scm_cadr (x);
}
SCM 
gh_cdar (SCM x)
{
  return scm_cdar (x);
}
SCM 
gh_cddr (SCM x)
{
  return scm_cddr (x);
}

SCM 
gh_caaar (SCM x)
{
  return scm_caaar (x);
}
SCM 
gh_caadr (SCM x)
{
  return scm_caadr (x);
}
SCM 
gh_cadar (SCM x)
{
  return scm_cadar (x);
}
SCM 
gh_caddr (SCM x)
{
  return scm_caddr (x);
}
SCM 
gh_cdaar (SCM x)
{
  return scm_cdaar (x);
}
SCM 
gh_cdadr (SCM x)
{
  return scm_cdadr (x);
}
SCM 
gh_cddar (SCM x)
{
  return scm_cddar (x);
}
SCM 
gh_cdddr (SCM x)
{
  return scm_cdddr (x);
}

/* equivalent to (set-car! pair value) */
SCM
gh_set_car_x(SCM pair, SCM value)
{
  return scm_set_car_x(pair, value);
}

/* equivalent to (set-cdr! pair value) */
SCM
gh_set_cdr_x(SCM pair, SCM value)
{
  return scm_set_cdr_x(pair, value);
}

#endif /* SCM_ENABLE_DEPRECATED */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
