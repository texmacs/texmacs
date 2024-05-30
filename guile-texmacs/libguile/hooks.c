/* Copyright (C) 1995,1996,1998,1999,2000,2001, 2003, 2006, 2008 Free Software Foundation, Inc.
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

#include <stdio.h>
#include "libguile/_scm.h"

#include "libguile/eval.h"
#include "libguile/ports.h"
#include "libguile/procprop.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/strings.h"

#include "libguile/validate.h"
#include "libguile/hooks.h"


/* C level hooks
 *
 * Currently, this implementation is separate from the Scheme level
 * hooks.  The possibility exists to implement the Scheme level hooks
 * using C level hooks.
 */

void
scm_c_hook_init (scm_t_c_hook *hook, void *hook_data, scm_t_c_hook_type type)
{
  hook->first = 0;
  hook->type = type;
  hook->data = hook_data;
}

void
scm_c_hook_add (scm_t_c_hook *hook,
		scm_t_c_hook_function func,
		void *fn_data, 
		int appendp)
{
  scm_t_c_hook_entry *entry = scm_malloc (sizeof (scm_t_c_hook_entry));
  scm_t_c_hook_entry **loc = &hook->first;
  if (appendp)
    while (*loc)
      loc = &(*loc)->next;
  entry->next = *loc;
  entry->func = func;
  entry->data = fn_data;
  *loc = entry;
}

void
scm_c_hook_remove (scm_t_c_hook *hook,
		   scm_t_c_hook_function func,
		   void *fn_data)
{
  scm_t_c_hook_entry **loc = &hook->first;
  while (*loc)
    {
      if ((*loc)->func == func && (*loc)->data == fn_data)
	{
	  scm_t_c_hook_entry *entry = *loc;
	  *loc = (*loc)->next;
	  free (entry);
	  return;
	}
      loc = &(*loc)->next;
    }
  fprintf (stderr, "Attempt to remove non-existent hook function\n");
  abort ();
}

void *
scm_c_hook_run (scm_t_c_hook *hook, void *data)
{
  scm_t_c_hook_entry *entry = hook->first;
  scm_t_c_hook_type type = hook->type;
  void *res = 0;
  while (entry)
    {
      res = (entry->func) (hook->data, entry->data, data);
      if (res)
	{
	  if (type == SCM_C_HOOK_OR)
	    break;
	}
      else
	{
	  if (type == SCM_C_HOOK_AND)
	    break;
	}
      entry = entry->next;
    }
  return res;
}


/* Scheme level hooks
 *
 * A hook is basically a list of procedures to be called at well defined
 * points in time.
 *
 * Hook arity is not a full member of this type and therefore lacks an
 * accessor.  It exists to aid debugging and is not intended to be used in
 * programs.
 */

scm_t_bits scm_tc16_hook;


static int
hook_print (SCM hook, SCM port, scm_print_state *pstate)
{
  SCM ls, name;
  scm_puts ("#<hook ", port);
  scm_intprint (SCM_HOOK_ARITY (hook), 10, port);
  scm_putc (' ', port);
  scm_uintprint (SCM_UNPACK (hook), 16, port);
  ls = SCM_HOOK_PROCEDURES (hook);
  while (SCM_NIMP (ls))
    {
      scm_putc (' ', port);
      name = scm_procedure_name (SCM_CAR (ls));
      if (scm_is_true (name))
	scm_iprin1 (name, port, pstate);
      else
	scm_putc ('?', port);
      ls = SCM_CDR (ls);
    }
  scm_putc ('>', port);
  return 1;
}


SCM_DEFINE (scm_make_hook, "make-hook", 0, 1, 0, 
            (SCM n_args),
	    "Create a hook for storing procedure of arity @var{n_args}.\n"
	    "@var{n_args} defaults to zero.  The returned value is a hook\n"
	    "object to be used with the other hook procedures.")
#define FUNC_NAME s_scm_make_hook
{
  unsigned int n;

  if (SCM_UNBNDP (n_args))
    n = 0;
  else
    n = scm_to_unsigned_integer (n_args, 0, 16);
  
  SCM_RETURN_NEWSMOB (scm_tc16_hook + (n << 16), SCM_UNPACK (SCM_EOL));
}
#undef FUNC_NAME


SCM_DEFINE (scm_hook_p, "hook?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} if @var{x} is a hook, @code{#f} otherwise.")
#define FUNC_NAME s_scm_hook_p
{
  return scm_from_bool (SCM_HOOKP (x));
}
#undef FUNC_NAME


SCM_DEFINE (scm_hook_empty_p, "hook-empty?", 1, 0, 0, 
            (SCM hook),
	    "Return @code{#t} if @var{hook} is an empty hook, @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_hook_empty_p
{
  SCM_VALIDATE_HOOK (1, hook);
  return scm_from_bool (scm_is_null (SCM_HOOK_PROCEDURES (hook)));
}
#undef FUNC_NAME


SCM_DEFINE (scm_add_hook_x, "add-hook!", 2, 1, 0, 
            (SCM hook, SCM proc, SCM append_p),
	    "Add the procedure @var{proc} to the hook @var{hook}. The\n"
	    "procedure is added to the end if @var{append_p} is true,\n"
	    "otherwise it is added to the front.  The return value of this\n"
	    "procedure is not specified.")
#define FUNC_NAME s_scm_add_hook_x
{
  SCM arity, rest;
  int n_args;
  SCM_VALIDATE_HOOK (1, hook);
  SCM_ASSERT (scm_is_true (arity = scm_i_procedure_arity (proc)),
	      proc, SCM_ARG2, FUNC_NAME);
  n_args = SCM_HOOK_ARITY (hook);
  if (scm_to_int (SCM_CAR (arity)) > n_args
      || (scm_is_false (SCM_CADDR (arity))
	  && (scm_to_int (SCM_CAR (arity)) + scm_to_int (SCM_CADR (arity))
	      < n_args)))
    scm_wrong_type_arg (FUNC_NAME, 2, proc);
  rest = scm_delq_x (proc, SCM_HOOK_PROCEDURES (hook));
  SCM_SET_HOOK_PROCEDURES (hook,
			   (!SCM_UNBNDP (append_p) && scm_is_true (append_p)
			    ? scm_append_x (scm_list_2 (rest, scm_list_1 (proc)))
			    : scm_cons (proc, rest)));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_remove_hook_x, "remove-hook!", 2, 0, 0, 
            (SCM hook, SCM proc),
	    "Remove the procedure @var{proc} from the hook @var{hook}.  The\n"
	    "return value of this procedure is not specified.")
#define FUNC_NAME s_scm_remove_hook_x
{
  SCM_VALIDATE_HOOK (1, hook);
  SCM_SET_HOOK_PROCEDURES (hook,
			   scm_delq_x (proc, SCM_HOOK_PROCEDURES (hook)));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_reset_hook_x, "reset-hook!", 1, 0, 0, 
            (SCM hook),
	    "Remove all procedures from the hook @var{hook}.  The return\n"
	    "value of this procedure is not specified.")
#define FUNC_NAME s_scm_reset_hook_x
{
  SCM_VALIDATE_HOOK (1, hook);
  SCM_SET_HOOK_PROCEDURES (hook, SCM_EOL);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_run_hook, "run-hook", 1, 0, 1, 
            (SCM hook, SCM args),
	    "Apply all procedures from the hook @var{hook} to the arguments\n"
	    "@var{args}.  The order of the procedure application is first to\n"
	    "last.  The return value of this procedure is not specified.")
#define FUNC_NAME s_scm_run_hook
{
  SCM_VALIDATE_HOOK (1, hook);
  if (scm_ilength (args) != SCM_HOOK_ARITY (hook))
    SCM_MISC_ERROR ("Hook ~S requires ~A arguments",
		    scm_list_2 (hook, scm_from_int (SCM_HOOK_ARITY (hook))));
  scm_c_run_hook (hook, args);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


void
scm_c_run_hook (SCM hook, SCM args)
{
  SCM procs = SCM_HOOK_PROCEDURES (hook);
  while (SCM_NIMP (procs))
    {
      scm_apply_0 (SCM_CAR (procs), args);
      procs = SCM_CDR (procs);
    }
}


SCM_DEFINE (scm_hook_to_list, "hook->list", 1, 0, 0, 
            (SCM hook),
	    "Convert the procedure list of @var{hook} to a list.")
#define FUNC_NAME s_scm_hook_to_list
{
  SCM_VALIDATE_HOOK (1, hook);
  return scm_list_copy (SCM_HOOK_PROCEDURES (hook));
}
#undef FUNC_NAME




void
scm_init_hooks ()
{
  scm_tc16_hook = scm_make_smob_type ("hook", 0);
  scm_set_smob_mark (scm_tc16_hook, scm_markcdr);
  scm_set_smob_print (scm_tc16_hook, hook_print);
#include "libguile/hooks.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
