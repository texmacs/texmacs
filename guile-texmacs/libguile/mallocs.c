/* classes: src_files 
 * Copyright (C) 1995,1997,1998,2000,2001, 2006 Free Software Foundation, Inc.
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

#include "libguile/_scm.h"
#include "libguile/ports.h"
#include "libguile/smob.h"

#include "libguile/mallocs.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif



scm_t_bits scm_tc16_malloc;


static size_t
malloc_free (SCM ptr)
{
  if (SCM_MALLOCDATA (ptr))
    free (SCM_MALLOCDATA (ptr));
  return 0;
}


static int
malloc_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_puts("#<malloc ", port);
  scm_uintprint (SCM_SMOB_DATA (exp), 16, port);
  scm_putc('>', port);
  return 1;
}


SCM
scm_malloc_obj (size_t n)
{
  scm_t_bits mem = n ? (scm_t_bits) scm_gc_malloc (n, "malloc smob") : 0;
  if (n && !mem)
    return SCM_BOOL_F;
  SCM_RETURN_NEWSMOB (scm_tc16_malloc, mem);
}



void 
scm_init_mallocs ()
{
  scm_tc16_malloc = scm_make_smob_type ("malloc", 0);
  scm_set_smob_free (scm_tc16_malloc, malloc_free);
  scm_set_smob_print (scm_tc16_malloc, malloc_print);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
