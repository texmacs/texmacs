/* Copyright (C) 1995,1996,1997,1998,2000,2001,2002,2003, 2006, 2008 Free Software Foundation, Inc.
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
#include "libguile/alist.h" /* for SCM_EXTEND_ENV (well...) */
#include "libguile/eval.h"
#include "libguile/ports.h"
#include "libguile/print.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/deprecation.h"

#include "libguile/validate.h"
#include "libguile/macros.h"

scm_t_bits scm_tc16_macro;


static int
macro_print (SCM macro, SCM port, scm_print_state *pstate)
{
  SCM code = SCM_MACRO_CODE (macro);
  if (!SCM_CLOSUREP (code)
      || scm_is_false (scm_procedure_p (SCM_PRINT_CLOSURE))
      || scm_is_false (scm_printer_apply (SCM_PRINT_CLOSURE,
					macro, port, pstate)))
    {
      if (!SCM_CLOSUREP (code))
	scm_puts ("#<primitive-", port);
      else
	scm_puts ("#<", port);

      if (SCM_MACRO_TYPE (macro) == 0)
	scm_puts ("syntax", port);
#if SCM_ENABLE_DEPRECATED == 1
      if (SCM_MACRO_TYPE (macro) == 1)
	scm_puts ("macro", port);
#endif
      if (SCM_MACRO_TYPE (macro) == 2)
	scm_puts ("macro!", port);
      if (SCM_MACRO_TYPE (macro) == 3)
	scm_puts ("builtin-macro!", port);

      scm_putc (' ', port);
      scm_iprin1 (scm_macro_name (macro), port, pstate);

      if (SCM_CLOSUREP (code) && SCM_PRINT_SOURCE_P)
	{
	  SCM formals = SCM_CLOSURE_FORMALS (code);
	  SCM env = SCM_ENV (code);
	  SCM xenv = SCM_EXTEND_ENV (formals, SCM_EOL, env);
	  SCM src = scm_i_unmemocopy_body (SCM_CODE (code), xenv);
	  scm_putc (' ', port);
	  scm_iprin1 (src, port, pstate);
	}

      scm_putc ('>', port);
    }

  return 1;
}

static SCM
makmac (SCM code, scm_t_bits flags)
{
  SCM z;
  SCM_NEWSMOB (z, scm_tc16_macro, SCM_UNPACK (code));
  SCM_SET_SMOB_FLAGS (z, flags);
  return z;
}

/* Return a mmacro that is known to be one of guile's built in macros. */
SCM
scm_i_makbimacro (SCM code)
#define FUNC_NAME "scm_i_makbimacro"
{
  SCM_VALIDATE_PROC (1, code);
  return makmac (code, 3);
}
#undef FUNC_NAME


SCM_DEFINE (scm_makmmacro, "procedure->memoizing-macro", 1, 0, 0, 
           (SCM code),
	    "Return a @dfn{macro} which, when a symbol defined to this value\n"
	    "appears as the first symbol in an expression, evaluates the\n"
	    "result of applying @var{code} to the expression and the\n"
	    "environment.\n\n"
	    "@code{procedure->memoizing-macro} is the same as\n"
	    "@code{procedure->macro}, except that the expression returned by\n"
	    "@var{code} replaces the original macro expression in the memoized\n"
	    "form of the containing code.")
#define FUNC_NAME s_scm_makmmacro
{
  SCM_VALIDATE_PROC (1, code);
  return makmac (code, 2);
}
#undef FUNC_NAME


SCM_DEFINE (scm_makacro, "procedure->syntax", 1, 0, 0,
            (SCM code),
	    "Return a @dfn{macro} which, when a symbol defined to this value\n"
	    "appears as the first symbol in an expression, returns the\n"
	    "result of applying @var{code} to the expression and the\n"
	    "environment.")
#define FUNC_NAME s_scm_makacro
{
  SCM_VALIDATE_PROC (1, code);
  return makmac (code, 0);
}
#undef FUNC_NAME


#if SCM_ENABLE_DEPRECATED == 1

SCM_DEFINE (scm_makmacro, "procedure->macro", 1, 0, 0, 
           (SCM code),
	    "Return a @dfn{macro} which, when a symbol defined to this value\n"
	    "appears as the first symbol in an expression, evaluates the\n"
	    "result of applying @var{code} to the expression and the\n"
	    "environment.  For example:\n"
	    "\n"
	    "@lisp\n"
	    "(define trace\n"
	    "  (procedure->macro\n"
	    "   (lambda (x env) `(set! ,(cadr x) (tracef ,(cadr x) ',(cadr x))))))\n\n"
	    "(trace @i{foo}) @equiv{} (set! @i{foo} (tracef @i{foo} '@i{foo})).\n"
	    "@end lisp")
#define FUNC_NAME s_scm_makmacro
{
  scm_c_issue_deprecation_warning
    ("The function procedure->macro is deprecated, and so are"
     " non-memoizing macros in general.  Use memoizing macros"
     " or r5rs macros instead.");

  SCM_VALIDATE_PROC (1, code);
  return makmac (code, 1);
}
#undef FUNC_NAME

#endif


SCM_DEFINE (scm_macro_p, "macro?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a regular macro, a memoizing macro or a\n"
	    "syntax transformer.")
#define FUNC_NAME s_scm_macro_p
{
  return scm_from_bool (SCM_SMOB_PREDICATE (scm_tc16_macro, obj));
}
#undef FUNC_NAME


SCM_SYMBOL (scm_sym_syntax, "syntax");
#if SCM_ENABLE_DEPRECATED == 1
SCM_SYMBOL (scm_sym_macro, "macro");
#endif
SCM_SYMBOL (scm_sym_mmacro, "macro!");
SCM_SYMBOL (scm_sym_bimacro, "builtin-macro!");

SCM_DEFINE (scm_macro_type, "macro-type", 1, 0, 0, 
            (SCM m),
	    "Return one of the symbols @code{syntax}, @code{macro} or\n"
	    "@code{macro!}, depending on whether @var{m} is a syntax\n"
	    "transformer, a regular macro, or a memoizing macro,\n"
	    "respectively.  If @var{m} is not a macro, @code{#f} is\n"
	    "returned.")
#define FUNC_NAME s_scm_macro_type
{
  if (!SCM_SMOB_PREDICATE (scm_tc16_macro, m))
    return SCM_BOOL_F;
  switch (SCM_MACRO_TYPE (m))
    {
    case 0: return scm_sym_syntax;
#if SCM_ENABLE_DEPRECATED == 1
    case 1: return scm_sym_macro;
#endif
    case 2: return scm_sym_mmacro;
    case 3: return scm_sym_bimacro;
    default: scm_wrong_type_arg (FUNC_NAME, 1, m);
    }
}
#undef FUNC_NAME


SCM_DEFINE (scm_macro_name, "macro-name", 1, 0, 0, 
            (SCM m),
	    "Return the name of the macro @var{m}.")
#define FUNC_NAME s_scm_macro_name
{
  SCM_VALIDATE_SMOB (1, m, macro);
  return scm_procedure_name (SCM_PACK (SCM_SMOB_DATA (m)));
}
#undef FUNC_NAME


SCM_DEFINE (scm_macro_transformer, "macro-transformer", 1, 0, 0, 
            (SCM m),
	    "Return the transformer of the macro @var{m}.")
#define FUNC_NAME s_scm_macro_transformer
{
  SCM_VALIDATE_SMOB (1, m, macro);
  return ((SCM_CLOSUREP (SCM_PACK (SCM_SMOB_DATA (m)))) ?
	  SCM_PACK(SCM_SMOB_DATA (m)) : SCM_BOOL_F);
}
#undef FUNC_NAME

SCM
scm_make_synt (const char *name, SCM (*macroizer) (), SCM (*fcn)() )
{
  SCM var = scm_c_define (name, SCM_UNDEFINED);
  SCM transformer = scm_c_make_subr (name, scm_tc7_subr_2, fcn);
  SCM_VARIABLE_SET (var, macroizer (transformer));
  return SCM_UNSPECIFIED;
}

void
scm_init_macros ()
{
  scm_tc16_macro = scm_make_smob_type ("macro", 0);
  scm_set_smob_mark (scm_tc16_macro, scm_markcdr);
  scm_set_smob_print (scm_tc16_macro, macro_print);
#include "libguile/macros.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
