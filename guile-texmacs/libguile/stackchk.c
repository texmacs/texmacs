/*	Copyright (C) 1995,1996,1997, 2000, 2001, 2006, 2008 Free Software Foundation, Inc.
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
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/threads.h"

#include "libguile/stackchk.h"


/* {Stack Checking}
 */

#ifdef STACK_CHECKING
int scm_stack_checking_enabled_p;

SCM_SYMBOL (scm_stack_overflow_key, "stack-overflow");

void
scm_report_stack_overflow ()
{
  scm_stack_checking_enabled_p = 0;
  scm_error (scm_stack_overflow_key,
	     NULL,
	     "Stack overflow",
	     SCM_BOOL_F,
	     SCM_BOOL_F);
}

#endif

long
scm_stack_size (SCM_STACKITEM *start)
{
  SCM_STACKITEM stack;
#if SCM_STACK_GROWS_UP
  return &stack - start;
#else
  return start - &stack;
#endif /* SCM_STACK_GROWS_UP */
}


void 
scm_stack_report ()
{
  SCM port = scm_current_error_port ();
  SCM_STACKITEM stack;
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;

  scm_uintprint ((scm_stack_size (thread->continuation_base) 
		  * sizeof (SCM_STACKITEM)),
		 16, port);
  scm_puts (" of stack: 0x", port);
  scm_uintprint ((scm_t_bits) thread->continuation_base, 16, port);
  scm_puts (" - 0x", port);
  scm_uintprint ((scm_t_bits) &stack, 16, port);
  scm_puts ("\n", port);
}


SCM_DEFINE (scm_sys_get_stack_size, "%get-stack-size", 0, 0, 0,
	    (),
	    "Return the current thread's C stack size (in Scheme objects).")
#define FUNC_NAME s_scm_sys_get_stack_size
{
  return scm_from_long (scm_stack_size (SCM_I_CURRENT_THREAD->base));
}
#undef FUNC_NAME


void
scm_init_stackchk ()
{
#include "libguile/stackchk.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
