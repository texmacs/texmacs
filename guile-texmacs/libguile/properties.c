/* Copyright (C) 1995,1996,2000,2001, 2003, 2006, 2008 Free Software Foundation, Inc.
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
#include "libguile/hashtab.h"
#include "libguile/alist.h"
#include "libguile/root.h"
#include "libguile/weaks.h"
#include "libguile/validate.h"
#include "libguile/eval.h"

#include "libguile/properties.h"


/* {Properties}
 */

SCM_DEFINE (scm_primitive_make_property, "primitive-make-property", 1, 0, 0,
	    (SCM not_found_proc),
	    "Create a @dfn{property token} that can be used with\n"
	    "@code{primitive-property-ref} and @code{primitive-property-set!}.\n"
	    "See @code{primitive-property-ref} for the significance of\n"
	    "@var{not_found_proc}.")
#define FUNC_NAME s_scm_primitive_make_property
{
  if (not_found_proc != SCM_BOOL_F)
    SCM_VALIDATE_PROC (SCM_ARG1, not_found_proc);
  return scm_cons (not_found_proc, SCM_EOL);
}
#undef FUNC_NAME


SCM_DEFINE (scm_primitive_property_ref, "primitive-property-ref", 2, 0, 0,
	    (SCM prop, SCM obj),
	    "Return the property @var{prop} of @var{obj}.\n"
	    "\n"
	    "When no value has yet been associated with @var{prop} and\n"
	    "@var{obj}, the @var{not-found-proc} from @var{prop} is used.  A\n"
	    "call @code{(@var{not-found-proc} @var{prop} @var{obj})} is made\n"
	    "and the result set as the property value.  If\n"
	    "@var{not-found-proc} is @code{#f} then @code{#f} is the\n"
	    "property value.")
#define FUNC_NAME s_scm_primitive_property_ref
{
  SCM h;

  SCM_VALIDATE_CONS (SCM_ARG1, prop);

  h = scm_hashq_get_handle (scm_properties_whash, obj);
  if (scm_is_true (h))
    {
      SCM assoc = scm_assq (prop, SCM_CDR (h));
      if (scm_is_true (assoc))
	return SCM_CDR (assoc);
    }

  if (scm_is_false (SCM_CAR (prop)))
    return SCM_BOOL_F;
  else
    {
      SCM val = scm_call_2 (SCM_CAR (prop), prop, obj);
      if (scm_is_false (h))
	h = scm_hashq_create_handle_x (scm_properties_whash, obj, SCM_EOL);
      SCM_SETCDR (h, scm_acons (prop, val, SCM_CDR (h)));
      return val;
    }
}
#undef FUNC_NAME


SCM_DEFINE (scm_primitive_property_set_x, "primitive-property-set!", 3, 0, 0,
	    (SCM prop, SCM obj, SCM val),
	    "Set the property @var{prop} of @var{obj} to @var{val}.")
#define FUNC_NAME s_scm_primitive_property_set_x
{
  SCM h, assoc;
  SCM_VALIDATE_CONS (SCM_ARG1, prop);
  h = scm_hashq_create_handle_x (scm_properties_whash, obj, SCM_EOL);
  assoc = scm_assq (prop, SCM_CDR (h));
  if (SCM_NIMP (assoc))
    SCM_SETCDR (assoc, val);
  else
    {
      assoc = scm_acons (prop, val, SCM_CDR (h));
      SCM_SETCDR (h, assoc);
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_primitive_property_del_x, "primitive-property-del!", 2, 0, 0,
	    (SCM prop, SCM obj),
	    "Remove any value associated with @var{prop} and @var{obj}.")
#define FUNC_NAME s_scm_primitive_property_del_x
{
  SCM h;
  SCM_VALIDATE_CONS (SCM_ARG1, prop);
  h = scm_hashq_get_handle (scm_properties_whash, obj);
  if (scm_is_true (h))
    SCM_SETCDR (h, scm_assq_remove_x (SCM_CDR (h), prop));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


void
scm_init_properties ()
{
  scm_properties_whash = scm_make_weak_key_hash_table (SCM_UNDEFINED);
#include "libguile/properties.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
