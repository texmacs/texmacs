/* Copyright (C) 2008 Free Software Foundation, Inc.
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


/* Test whether `scm_with_guile ()' can be called several times from a given
   thread, but from a different stack depth.  Up to 1.8.5, `scm_with_guile
   ()' would not update the thread's `base' field, which would then confuse
   the GC.

   See http://lists.gnu.org/archive/html/guile-devel/2008-11/msg00037.html
   for a detailed report.  */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libguile.h>

static void *
entry_point (void *arg)
{
  /* Invoke the GC.  If `THREAD->base' is incorrect, then Guile will just
     segfault somewhere in `scm_mark_locations ()'.  */
  scm_gc ();

  return NULL;
}

static void
go_deeper_into_the_stack (unsigned level)
{
  /* The assumption is that the compiler is not smart enough to optimize this
     out.  */
  if (level > 0)
    go_deeper_into_the_stack (level - 1);
  else
    scm_with_guile (entry_point, NULL);
}


int
main (int argc, char *argv[])
{
  /* Invoke `scm_with_guile ()' from someplace deep into the stack.  */
  go_deeper_into_the_stack (100);

  /* Invoke it from much higher into the stack.  This time, Guile is expected
     to update the `base' field of the current thread.  */
  scm_with_guile (entry_point, NULL);

  return 0;
}
