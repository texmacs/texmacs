/* extensions.c - registering and loading extensions.
 *
 * Copyright (C) 2001, 2006 Free Software Foundation, Inc.
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
#  include <config.h>
#endif

#include <string.h>

#include "libguile/_scm.h"
#include "libguile/strings.h"
#include "libguile/gc.h"
#include "libguile/dynl.h"
#include "libguile/dynwind.h"

#include "libguile/extensions.h"

typedef struct extension_t
{
  struct extension_t *next;
  const char *lib;
  const char *init;
  void (*func)(void *);
  void *data;
} extension_t;

static extension_t *registered_extensions;

/* Register a LIB/INIT pair for use by `scm_load_extension'.  LIB is
   allowed to be NULL and then only INIT is used to identify the
   registered entry.  This is useful when you don't know the library
   name (which isn't really relevant anyway in a completely linked
   program) and you are sure that INIT is unique (which it must be for
   static linking).  Hmm, given this reasoning, what use is LIB
   anyway?
*/

void
scm_c_register_extension (const char *lib, const char *init,
			  void (*func) (void *), void *data)
{
  extension_t *ext = scm_malloc (sizeof(extension_t));
  if (lib)
    ext->lib = scm_strdup (lib);
  else
    ext->lib = NULL;
  ext->init = scm_strdup (init);
  ext->func = func;
  ext->data = data;

  ext->next = registered_extensions;
  registered_extensions = ext;
}

static void
load_extension (SCM lib, SCM init)
{
  /* Search the registry. */
  if (registered_extensions != NULL)
    {
      extension_t *ext;
      char *clib, *cinit;
      int found = 0;

      scm_dynwind_begin (0);

      clib = scm_to_locale_string (lib);
      scm_dynwind_free (clib);
      cinit = scm_to_locale_string (init);
      scm_dynwind_free (cinit);

      for (ext = registered_extensions; ext; ext = ext->next)
	if ((ext->lib == NULL || !strcmp (ext->lib, clib))
	    && !strcmp (ext->init, cinit))
	  {
	    ext->func (ext->data);
            found = 1;
	    break;
	  }

      scm_dynwind_end ();

      if (found)
        return;
    }

  /* Dynamically link the library. */
  scm_dynamic_call (init, scm_dynamic_link (lib));
}

void
scm_c_load_extension (const char *lib, const char *init)
{
  load_extension (scm_from_locale_string (lib), scm_from_locale_string (init));
}

SCM_DEFINE (scm_load_extension, "load-extension", 2, 0, 0,
	    (SCM lib, SCM init),
	    "Load and initialize the extension designated by LIB and INIT.\n"
	    "When there is no pre-registered function for LIB/INIT, this is\n"
	    "equivalent to\n"
	    "\n"
	    "@lisp\n"
	    "(dynamic-call INIT (dynamic-link LIB))\n"
	    "@end lisp\n"
	    "\n"
	    "When there is a pre-registered function, that function is called\n"
	    "instead.\n"
	    "\n"
	    "Normally, there is no pre-registered function.  This option exists\n"
	    "only for situations where dynamic linking is unavailable or unwanted.\n"
	    "In that case, you would statically link your program with the desired\n"
	    "library, and register its init function right after Guile has been\n"
	    "initialized.\n"
	    "\n"
	    "LIB should be a string denoting a shared library without any file type\n"
	    "suffix such as \".so\".  The suffix is provided automatically.  It\n"
	    "should also not contain any directory components.  Libraries that\n"
	    "implement Guile Extensions should be put into the normal locations for\n"
	    "shared libraries.  We recommend to use the naming convention\n"
	    "libguile-bla-blum for a extension related to a module `(bla blum)'.\n"
	    "\n"
	    "The normal way for a extension to be used is to write a small Scheme\n"
	    "file that defines a module, and to load the extension into this\n"
	    "module.  When the module is auto-loaded, the extension is loaded as\n"
	    "well.  For example,\n"
	    "\n"
	    "@lisp\n"
	    "(define-module (bla blum))\n"
	    "\n"
	    "(load-extension \"libguile-bla-blum\" \"bla_init_blum\")\n"
	    "@end lisp")
#define FUNC_NAME s_scm_load_extension
{
  load_extension (lib, init);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_init_extensions ()
{
  registered_extensions = NULL;
#include "libguile/extensions.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
