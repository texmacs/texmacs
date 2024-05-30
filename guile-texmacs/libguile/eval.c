/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008
 * Free Software Foundation, Inc.
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



/* This file is read twice in order to produce debugging versions of ceval and
 * scm_apply.  These functions, deval and scm_dapply, are produced when we
 * define the preprocessor macro DEVAL.  The file is divided into sections
 * which are treated differently with respect to DEVAL.  The heads of these
 * sections are marked with the string "SECTION:".  */

/* SECTION: This code is compiled once.
 */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "libguile/__scm.h"

#ifndef DEVAL

/* This blob per the Autoconf manual (under "Particular Functions"), updated
   to match that of Gnulib.  */
#ifndef alloca
# if HAVE_ALLOCA_H
#  include <alloca.h>
# elif defined __GNUC__
#  define alloca __builtin_alloca
# elif defined _AIX
#  define alloca __alloca
# elif defined _MSC_VER
#  include <malloc.h>
#  define alloca _alloca
# else
#  include <stddef.h>
#  ifdef  __cplusplus
extern "C"
#  endif
void *alloca (size_t);
# endif
#endif

#include <assert.h>
#include "libguile/_scm.h"
#include "libguile/alist.h"
#include "libguile/async.h"
#include "libguile/continuations.h"
#include "libguile/debug.h"
#include "libguile/deprecation.h"
#include "libguile/dynwind.h"
#include "libguile/eq.h"
#include "libguile/feature.h"
#include "libguile/fluids.h"
#include "libguile/futures.h"
#include "libguile/goops.h"
#include "libguile/hash.h"
#include "libguile/hashtab.h"
#include "libguile/lang.h"
#include "libguile/list.h"
#include "libguile/macros.h"
#include "libguile/modules.h"
#include "libguile/objects.h"
#include "libguile/ports.h"
#include "libguile/print.h"
#include "libguile/procprop.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/srcprop.h"
#include "libguile/stackchk.h"
#include "libguile/strings.h"
#include "libguile/threads.h"
#include "libguile/throw.h"
#include "libguile/validate.h"
#include "libguile/values.h"
#include "libguile/vectors.h"

#include "libguile/eval.h"



static SCM unmemoize_exprs (SCM expr, SCM env);
static SCM canonicalize_define (SCM expr);
static SCM *scm_lookupcar1 (SCM vloc, SCM genv, int check);
static SCM unmemoize_builtin_macro (SCM expr, SCM env);
static void eval_letrec_inits (SCM env, SCM init_forms, SCM **init_values_eol);



/* {Syntax Errors}
 *
 * This section defines the message strings for the syntax errors that can be
 * detected during memoization and the functions and macros that shall be
 * called by the memoizer code to signal syntax errors.  */


/* Syntax errors that can be detected during memoization: */

/* Circular or improper lists do not form valid scheme expressions.  If a
 * circular list or an improper list is detected in a place where a scheme
 * expression is expected, a 'Bad expression' error is signalled.  */
static const char s_bad_expression[] = "Bad expression";

/* If a form is detected that holds a different number of expressions than are
 * required in that context, a 'Missing or extra expression' error is
 * signalled.  */
static const char s_expression[] = "Missing or extra expression in";

/* If a form is detected that holds less expressions than are required in that
 * context, a 'Missing expression' error is signalled.  */
static const char s_missing_expression[] = "Missing expression in";

/* If a form is detected that holds more expressions than are allowed in that
 * context, an 'Extra expression' error is signalled.  */
static const char s_extra_expression[] = "Extra expression in";

/* The empty combination '()' is not allowed as an expression in scheme.  If
 * it is detected in a place where an expression is expected, an 'Illegal
 * empty combination' error is signalled.  Note: If you encounter this error
 * message, it is very likely that you intended to denote the empty list.  To
 * do so, you need to quote the empty list like (quote ()) or '().  */
static const char s_empty_combination[] = "Illegal empty combination";

/* A body may hold an arbitrary number of internal defines, followed by a
 * non-empty sequence of expressions.  If a body with an empty sequence of
 * expressions is detected, a 'Missing body expression' error is signalled.
 */
static const char s_missing_body_expression[] = "Missing body expression in";

/* A body may hold an arbitrary number of internal defines, followed by a
 * non-empty sequence of expressions.  Each the definitions and the
 * expressions may be grouped arbitraryly with begin, but it is not allowed to
 * mix definitions and expressions.  If a define form in a body mixes
 * definitions and expressions, a 'Mixed definitions and expressions' error is
 * signalled.  */
static const char s_mixed_body_forms[] = "Mixed definitions and expressions in";
/* Definitions are only allowed on the top level and at the start of a body.
 * If a definition is detected anywhere else, a 'Bad define placement' error
 * is signalled.  */
static const char s_bad_define[] = "Bad define placement";

/* Case or cond expressions must have at least one clause.  If a case or cond
 * expression without any clauses is detected, a 'Missing clauses' error is
 * signalled.  */
static const char s_missing_clauses[] = "Missing clauses";

/* If there is an 'else' clause in a case or a cond statement, it must be the
 * last clause.  If after the 'else' case clause further clauses are detected,
 * a 'Misplaced else clause' error is signalled.  */
static const char s_misplaced_else_clause[] = "Misplaced else clause";

/* If a case clause is detected that is not in the format
 *   (<label(s)> <expression1> <expression2> ...)
 * a 'Bad case clause' error is signalled.  */
static const char s_bad_case_clause[] = "Bad case clause";

/* If a case clause is detected where the <label(s)> element is neither a
 * proper list nor (in case of the last clause) the syntactic keyword 'else',
 * a 'Bad case labels' error is signalled.  Note: If you encounter this error
 * for an else-clause which seems to be syntactically correct, check if 'else'
 * is really a syntactic keyword in that context.  If 'else' is bound in the
 * local or global environment, it is not considered a syntactic keyword, but
 * will be treated as any other variable.  */
static const char s_bad_case_labels[] = "Bad case labels";

/* In a case statement all labels have to be distinct.  If in a case statement
 * a label occurs more than once, a 'Duplicate case label' error is
 * signalled.  */
static const char s_duplicate_case_label[] = "Duplicate case label";

/* If a cond clause is detected that is not in one of the formats
 *   (<test> <expression1> ...) or (else <expression1> <expression2> ...)
 * a 'Bad cond clause' error is signalled.  */
static const char s_bad_cond_clause[] = "Bad cond clause";

/* If a cond clause is detected that uses the alternate '=>' form, but does
 * not hold a recipient element for the test result, a 'Missing recipient'
 * error is signalled.  */
static const char s_missing_recipient[] = "Missing recipient in";

/* If in a position where a variable name is required some other object is
 * detected, a 'Bad variable' error is signalled.  */
static const char s_bad_variable[] = "Bad variable";

/* Bindings for forms like 'let' and 'do' have to be given in a proper,
 * possibly empty list.  If any other object is detected in a place where a
 * list of bindings was required, a 'Bad bindings' error is signalled.  */
static const char s_bad_bindings[] = "Bad bindings";

/* Depending on the syntactic context, a binding has to be in the format
 * (<variable> <expression>) or (<variable> <expression1> <expression2>).
 * If anything else is detected in a place where a binding was expected, a
 * 'Bad binding' error is signalled.  */
static const char s_bad_binding[] = "Bad binding";

/* Some syntactic forms don't allow variable names to appear more than once in
 * a list of bindings.  If such a situation is nevertheless detected, a
 * 'Duplicate binding' error is signalled.  */
static const char s_duplicate_binding[] = "Duplicate binding";

/* If the exit form of a 'do' expression is not in the format
 *   (<test> <expression> ...)
 * a 'Bad exit clause' error is signalled.  */
static const char s_bad_exit_clause[] = "Bad exit clause";

/* The formal function arguments of a lambda expression have to be either a
 * single symbol or a non-cyclic list.  For anything else a 'Bad formals'
 * error is signalled.  */
static const char s_bad_formals[] = "Bad formals";

/* If in a lambda expression something else than a symbol is detected at a
 * place where a formal function argument is required, a 'Bad formal' error is
 * signalled.  */
static const char s_bad_formal[] = "Bad formal";

/* If in the arguments list of a lambda expression an argument name occurs
 * more than once, a 'Duplicate formal' error is signalled.  */
static const char s_duplicate_formal[] = "Duplicate formal";

/* If the evaluation of an unquote-splicing expression gives something else
 * than a proper list, a 'Non-list result for unquote-splicing' error is
 * signalled.  */
static const char s_splicing[] = "Non-list result for unquote-splicing";

/* If something else than an exact integer is detected as the argument for
 * @slot-ref and @slot-set!, a 'Bad slot number' error is signalled.  */
static const char s_bad_slot_number[] = "Bad slot number";


/* Signal a syntax error.  We distinguish between the form that caused the
 * error and the enclosing expression.  The error message will print out as
 * shown in the following pattern.  The file name and line number are only
 * given when they can be determined from the erroneous form or from the
 * enclosing expression.
 *
 * <filename>: In procedure memoization:
 * <filename>: In file <name>, line <nr>: <error-message> in <expression>.  */

SCM_SYMBOL (syntax_error_key, "syntax-error");

/* The prototype is needed to indicate that the function does not return.  */
static void
syntax_error (const char* const, const SCM, const SCM) SCM_NORETURN;

static void 
syntax_error (const char* const msg, const SCM form, const SCM expr)
{
  SCM msg_string = scm_from_locale_string (msg);
  SCM filename = SCM_BOOL_F;
  SCM linenr = SCM_BOOL_F;
  const char *format;
  SCM args;

  if (scm_is_pair (form))
    {
      filename = scm_source_property (form, scm_sym_filename);
      linenr = scm_source_property (form, scm_sym_line);
    }

  if (scm_is_false (filename) && scm_is_false (linenr) && scm_is_pair (expr))
    {
      filename = scm_source_property (expr, scm_sym_filename);
      linenr = scm_source_property (expr, scm_sym_line);
    }

  if (!SCM_UNBNDP (expr))
    {
      if (scm_is_true (filename))
	{
	  format = "In file ~S, line ~S: ~A ~S in expression ~S.";
	  args = scm_list_5 (filename, linenr, msg_string, form, expr);
	}
      else if (scm_is_true (linenr))
	{
	  format = "In line ~S: ~A ~S in expression ~S.";
	  args = scm_list_4 (linenr, msg_string, form, expr);
	}
      else
	{
	  format = "~A ~S in expression ~S.";
	  args = scm_list_3 (msg_string, form, expr);
	}
    }
  else
    {
      if (scm_is_true (filename))
	{
	  format = "In file ~S, line ~S: ~A ~S.";
	  args = scm_list_4 (filename, linenr, msg_string, form);
	}
      else if (scm_is_true (linenr))
	{
	  format = "In line ~S: ~A ~S.";
	  args = scm_list_3 (linenr, msg_string, form);
	}
      else
	{
	  format = "~A ~S.";
	  args = scm_list_2 (msg_string, form);
	}
    }

  scm_error (syntax_error_key, "memoization", format, args, SCM_BOOL_F);
}


/* Shortcut macros to simplify syntax error handling. */
#define ASSERT_SYNTAX(cond, message, form)		\
  { if (SCM_UNLIKELY (!(cond)))			\
      syntax_error (message, form, SCM_UNDEFINED); }
#define ASSERT_SYNTAX_2(cond, message, form, expr)	\
  { if (SCM_UNLIKELY (!(cond)))			\
      syntax_error (message, form, expr); }



/* {Ilocs}
 *
 * Ilocs are memoized references to variables in local environment frames.
 * They are represented as three values:  The relative offset of the
 * environment frame, the number of the binding within that frame, and a
 * boolean value indicating whether the binding is the last binding in the
 * frame.
 *
 * Frame numbers have 11 bits, relative offsets have 12 bits.
 */

#define SCM_ILOC00		SCM_MAKE_ITAG8(0L, scm_tc8_iloc)
#define SCM_IFRINC		(0x00000100L)
#define SCM_ICDR		(0x00080000L)
#define SCM_IDINC		(0x00100000L)
#define SCM_IFRAME(n) 		((long)((SCM_ICDR-SCM_IFRINC)>>8) \
				 & (SCM_UNPACK (n) >> 8))
#define SCM_IDIST(n) 		(SCM_UNPACK (n) >> 20)
#define SCM_ICDRP(n) 		(SCM_ICDR & SCM_UNPACK (n))
#define SCM_IDSTMSK		(-SCM_IDINC)
#define SCM_IFRAMEMAX           ((1<<11)-1)
#define SCM_IDISTMAX            ((1<<12)-1)
#define SCM_MAKE_ILOC(frame_nr, binding_nr, last_p) \
  SCM_PACK ( \
    ((frame_nr) << 8) \
    + ((binding_nr) << 20) \
    + ((last_p) ? SCM_ICDR : 0) \
    + scm_tc8_iloc )

void
scm_i_print_iloc (SCM iloc, SCM port)
{
  scm_puts ("#@", port);
  scm_intprint ((long) SCM_IFRAME (iloc), 10, port);
  scm_putc (SCM_ICDRP (iloc) ? '-' : '+', port);
  scm_intprint ((long) SCM_IDIST (iloc), 10, port);
}

#if (SCM_DEBUG_DEBUGGING_SUPPORT == 1)

SCM scm_dbg_make_iloc (SCM frame, SCM binding, SCM cdrp);

SCM_DEFINE (scm_dbg_make_iloc, "dbg-make-iloc", 3, 0, 0,
            (SCM frame, SCM binding, SCM cdrp),
	    "Return a new iloc with frame offset @var{frame}, binding\n"
	    "offset @var{binding} and the cdr flag @var{cdrp}.")
#define FUNC_NAME s_scm_dbg_make_iloc
{
  return SCM_MAKE_ILOC ((scm_t_bits) scm_to_unsigned_integer (frame, 0, SCM_IFRAMEMAX),
			(scm_t_bits) scm_to_unsigned_integer (binding, 0, SCM_IDISTMAX),
			scm_is_true (cdrp));
}
#undef FUNC_NAME

SCM scm_dbg_iloc_p (SCM obj);

SCM_DEFINE (scm_dbg_iloc_p, "dbg-iloc?", 1, 0, 0, 
          (SCM obj),
	    "Return @code{#t} if @var{obj} is an iloc.")
#define FUNC_NAME s_scm_dbg_iloc_p
{
  return scm_from_bool (SCM_ILOCP (obj));
}
#undef FUNC_NAME

#endif



/* {Evaluator byte codes (isyms)}
 */

#define ISYMNUM(n) 		(SCM_ITAG8_DATA (n))

/* This table must agree with the list of SCM_IM_ constants in tags.h */
static const char *const isymnames[] =
{
  "#@and",
  "#@begin",
  "#@case",
  "#@cond",
  "#@do",
  "#@if",
  "#@lambda",
  "#@let",
  "#@let*",
  "#@letrec",
  "#@or",
  "#@quote",
  "#@set!",
  "#@define",
  "#@apply",
  "#@call-with-current-continuation",
  "#@dispatch",
  "#@slot-ref",
  "#@slot-set!",
  "#@delay",
  "#@future",
  "#@call-with-values",
  "#@else",
  "#@arrow",
  "#@nil-cond",
  "#@bind"
};

void
scm_i_print_isym (SCM isym, SCM port)
{
  const size_t isymnum = ISYMNUM (isym);
  if (isymnum < (sizeof isymnames / sizeof (char *)))
    scm_puts (isymnames[isymnum], port);
  else
    scm_ipruk ("isym", isym, port);
}



/* The function lookup_symbol is used during memoization: Lookup the symbol in
 * the environment.  If there is no binding for the symbol, SCM_UNDEFINED is
 * returned.  If the symbol is a global variable, the variable object to which
 * the symbol is bound is returned.  Finally, if the symbol is a local
 * variable the corresponding iloc object is returned.  */

/* A helper function for lookup_symbol: Try to find the symbol in the top
 * level environment frame.  The function returns SCM_UNDEFINED if the symbol
 * is unbound and it returns a variable object if the symbol is a global
 * variable.  */
static SCM
lookup_global_symbol (const SCM symbol, const SCM top_level)
{
  const SCM variable = scm_sym2var (symbol, top_level, SCM_BOOL_F);
  if (scm_is_false (variable))
    return SCM_UNDEFINED;
  else
    return variable;
}

static SCM
lookup_symbol (const SCM symbol, const SCM env)
{
  SCM frame_idx;
  unsigned int frame_nr;

  for (frame_idx = env, frame_nr = 0;
       !scm_is_null (frame_idx);
       frame_idx = SCM_CDR (frame_idx), ++frame_nr)
    {
      const SCM frame = SCM_CAR (frame_idx);
      if (scm_is_pair (frame))
	{
	  /* frame holds a local environment frame */
	  SCM symbol_idx;
	  unsigned int symbol_nr;

	  for (symbol_idx = SCM_CAR (frame), symbol_nr = 0;
	       scm_is_pair (symbol_idx);
	       symbol_idx = SCM_CDR (symbol_idx), ++symbol_nr)
	    {
	      if (scm_is_eq (SCM_CAR (symbol_idx), symbol))
		/* found the symbol, therefore return the iloc */
		return SCM_MAKE_ILOC (frame_nr, symbol_nr, 0);
	    }
	  if (scm_is_eq (symbol_idx, symbol))
	    /* found the symbol as the last element of the current frame */
	    return SCM_MAKE_ILOC (frame_nr, symbol_nr, 1);
	}
      else
	{
	  /* no more local environment frames */
	  return lookup_global_symbol (symbol, frame);
	}
    }

  return lookup_global_symbol (symbol, SCM_BOOL_F);
}


/* Return true if the symbol is - from the point of view of a macro
 * transformer - a literal in the sense specified in chapter "pattern
 * language" of R5RS.  In the code below, however, we don't match the
 * definition of R5RS exactly:  It returns true if the identifier has no
 * binding or if it is a syntactic keyword.  */
static int
literal_p (const SCM symbol, const SCM env)
{
  const SCM variable = lookup_symbol (symbol, env);
  if (SCM_UNBNDP (variable))
    return 1;
  if (SCM_VARIABLEP (variable) && SCM_MACROP (SCM_VARIABLE_REF (variable)))
    return 1;
  else
    return 0;
}


/* Return true if the expression is self-quoting in the memoized code.  Thus,
 * some other objects (like e. g. vectors) are reported as self-quoting, which
 * according to R5RS would need to be quoted.  */
static int
is_self_quoting_p (const SCM expr)
{
  if (scm_is_pair (expr))
    return 0;
  else if (scm_is_symbol (expr))
    return 0;
  else if (scm_is_null (expr))
    return 0;
  else return 1;
}


SCM_SYMBOL (sym_three_question_marks, "???");

static SCM
unmemoize_expression (const SCM expr, const SCM env)
{
  if (SCM_ILOCP (expr))
    {
      SCM frame_idx;
      unsigned long int frame_nr;
      SCM symbol_idx;
      unsigned long int symbol_nr;

      for (frame_idx = env, frame_nr = SCM_IFRAME (expr);
           frame_nr != 0; 
           frame_idx = SCM_CDR (frame_idx), --frame_nr)
        ;
      for (symbol_idx = SCM_CAAR (frame_idx), symbol_nr = SCM_IDIST (expr);
           symbol_nr != 0;
           symbol_idx = SCM_CDR (symbol_idx), --symbol_nr)
        ;
      return SCM_ICDRP (expr) ? symbol_idx : SCM_CAR (symbol_idx);
    }
  else if (SCM_VARIABLEP (expr))
    {
      const SCM sym = scm_module_reverse_lookup (scm_env_module (env), expr);
      return scm_is_true (sym) ? sym : sym_three_question_marks;
    }
  else if (scm_is_simple_vector (expr))
    {
      return scm_list_2 (scm_sym_quote, expr);
    }
  else if (!scm_is_pair (expr))
    {
      return expr;
    }
  else if (SCM_ISYMP (SCM_CAR (expr)))
    {
      return unmemoize_builtin_macro (expr, env);
    }
  else
    {
      return unmemoize_exprs (expr, env);
    }
}


static SCM
unmemoize_exprs (const SCM exprs, const SCM env)
{
  SCM r_result = SCM_EOL;
  SCM expr_idx = exprs;
  SCM um_expr;

  /* Note that due to the current lazy memoizer we may find partially memoized
   * code during execution.  In such code we have to expect improper lists of
   * expressions: On the one hand, for such code syntax checks have not yet
   * fully been performed, on the other hand, there may be even legal code
   * like '(a . b) appear as an improper list of expressions as long as the
   * quote expression is still in its unmemoized form.  For this reason, the
   * following code handles improper lists of expressions until memoization
   * and execution have been completely separated.  */
  for (; scm_is_pair (expr_idx); expr_idx = SCM_CDR (expr_idx))
    {
      const SCM expr = SCM_CAR (expr_idx);

      /* In partially memoized code, lists of expressions that stem from a
       * body form may start with an ISYM if the body itself has not yet been
       * memoized.  This isym is just an internal marker to indicate that the
       * body still needs to be memoized.  An isym may occur at the very
       * beginning of the body or after one or more comment strings.  It is
       * dropped during unmemoization.  */
      if (!SCM_ISYMP (expr))
        {
          um_expr = unmemoize_expression (expr, env);
          r_result = scm_cons (um_expr, r_result);
        }
    }
  um_expr = unmemoize_expression (expr_idx, env);
  if (!scm_is_null (r_result))
    {
      const SCM result = scm_reverse_x (r_result, SCM_UNDEFINED);
      SCM_SETCDR (r_result, um_expr);
      return result;
    }
  else
    {
      return um_expr;
    }
}


/* Rewrite the body (which is given as the list of expressions forming the
 * body) into its internal form.  The internal form of a body (<expr> ...) is
 * just the body itself, but prefixed with an ISYM that denotes to what kind
 * of outer construct this body belongs: (<ISYM> <expr> ...).  A lambda body
 * starts with SCM_IM_LAMBDA, for example, a body of a let starts with
 * SCM_IM_LET, etc.
 *
 * It is assumed that the calling expression has already made sure that the
 * body is a proper list.  */
static SCM
m_body (SCM op, SCM exprs)
{
  /* Don't add another ISYM if one is present already. */
  if (SCM_ISYMP (SCM_CAR (exprs)))
    return exprs;
  else
    return scm_cons (op, exprs);
}


/* The function m_expand_body memoizes a proper list of expressions forming a
 * body.  This function takes care of dealing with internal defines and
 * transforming them into an equivalent letrec expression.  The list of
 * expressions is rewritten in place.  */ 

/* This is a helper function for m_expand_body.  If the argument expression is
 * a symbol that denotes a syntactic keyword, the corresponding macro object
 * is returned, in all other cases the function returns SCM_UNDEFINED.  */ 
static SCM
try_macro_lookup (const SCM expr, const SCM env)
{
  if (scm_is_symbol (expr))
    {
      const SCM variable = lookup_symbol (expr, env);
      if (SCM_VARIABLEP (variable))
        {
          const SCM value = SCM_VARIABLE_REF (variable);
          if (SCM_MACROP (value))
            return value;
        }
    }

  return SCM_UNDEFINED;
}

/* This is a helper function for m_expand_body.  It expands user macros,
 * because for the correct translation of a body we need to know whether they
 * expand to a definition. */ 
static SCM
expand_user_macros (SCM expr, const SCM env)
{
  while (scm_is_pair (expr))
    {
      const SCM car_expr = SCM_CAR (expr);
      const SCM new_car = expand_user_macros (car_expr, env);
      const SCM value = try_macro_lookup (new_car, env);

      if (SCM_MACROP (value) && SCM_MACRO_TYPE (value) == 2)
	{
	  /* User macros transform code into code.  */
	  expr = scm_call_2 (SCM_MACRO_CODE (value), expr, env);
	  /* We need to reiterate on the transformed code.  */
	}
      else
	{
	  /* No user macro: return.  */
	  SCM_SETCAR (expr, new_car);
	  return expr;
	}
    }

  return expr;
}

/* This is a helper function for m_expand_body.  It determines if a given form
 * represents an application of a given built-in macro.  The built-in macro to
 * check for is identified by its syntactic keyword.  The form is an
 * application of the given macro if looking up the car of the form in the
 * given environment actually returns the built-in macro.  */
static int
is_system_macro_p (const SCM syntactic_keyword, const SCM form, const SCM env)
{
  if (scm_is_pair (form))
    {
      const SCM car_form = SCM_CAR (form);
      const SCM value = try_macro_lookup (car_form, env);
      if (SCM_BUILTIN_MACRO_P (value))
        {
          const SCM macro_name = scm_macro_name (value);
          return scm_is_eq (macro_name, syntactic_keyword);
        }
    }

  return 0;
}

static void
m_expand_body (const SCM forms, const SCM env)
{
  /* The first body form can be skipped since it is known to be the ISYM that
   * was prepended to the body by m_body.  */
  SCM cdr_forms = SCM_CDR (forms);
  SCM form_idx = cdr_forms;
  SCM definitions = SCM_EOL;
  SCM sequence = SCM_EOL;

  /* According to R5RS, the list of body forms consists of two parts: a number
   * (maybe zero) of definitions, followed by a non-empty sequence of
   * expressions.  Each the definitions and the expressions may be grouped
   * arbitrarily with begin, but it is not allowed to mix definitions and
   * expressions.  The task of the following loop therefore is to split the
   * list of body forms into the list of definitions and the sequence of
   * expressions.  */ 
  while (!scm_is_null (form_idx))
    {
      const SCM form = SCM_CAR (form_idx);
      const SCM new_form = expand_user_macros (form, env);
      if (is_system_macro_p (scm_sym_define, new_form, env))
	{
	  definitions = scm_cons (new_form, definitions);
	  form_idx = SCM_CDR (form_idx);
	}
      else if (is_system_macro_p (scm_sym_begin, new_form, env))
	{
          /* We have encountered a group of forms.  This has to be either a
           * (possibly empty) group of (possibly further grouped) definitions,
           * or a non-empty group of (possibly further grouped)
           * expressions.  */
          const SCM grouped_forms = SCM_CDR (new_form);
          unsigned int found_definition = 0;
          unsigned int found_expression = 0;
          SCM grouped_form_idx = grouped_forms;
          while (!found_expression && !scm_is_null (grouped_form_idx))
            {
              const SCM inner_form = SCM_CAR (grouped_form_idx);
              const SCM new_inner_form = expand_user_macros (inner_form, env);
              if (is_system_macro_p (scm_sym_define, new_inner_form, env))
                {
                  found_definition = 1;
                  definitions = scm_cons (new_inner_form, definitions);
                  grouped_form_idx = SCM_CDR (grouped_form_idx);
                }
              else if (is_system_macro_p (scm_sym_begin, new_inner_form, env))
                {
                  const SCM inner_group = SCM_CDR (new_inner_form);
                  grouped_form_idx
                    = scm_append (scm_list_2 (inner_group,
                                              SCM_CDR (grouped_form_idx)));
                }
              else
                {
                  /* The group marks the start of the expressions of the body.
                   * We have to make sure that within the same group we have
                   * not encountered a definition before.  */
                  ASSERT_SYNTAX (!found_definition, s_mixed_body_forms, form);
                  found_expression = 1;
                  grouped_form_idx = SCM_EOL;
                }
            }

          /* We have finished processing the group.  If we have not yet
           * encountered an expression we continue processing the forms of the
           * body to collect further definition forms.  Otherwise, the group
           * marks the start of the sequence of expressions of the body.  */
          if (!found_expression)
            {
              form_idx = SCM_CDR (form_idx);
            }
          else
            {
              sequence = form_idx;
              form_idx = SCM_EOL;
            }
	}
      else
	{
          /* We have detected a form which is no definition.  This marks the
           * start of the sequence of expressions of the body.  */
          sequence = form_idx;
          form_idx = SCM_EOL;
	}
    }

  /* FIXME: forms does not hold information about the file location.  */
  ASSERT_SYNTAX (scm_is_pair (sequence), s_missing_body_expression, cdr_forms);

  if (!scm_is_null (definitions))
    {
      SCM definition_idx;
      SCM letrec_tail;
      SCM letrec_expression;
      SCM new_letrec_expression;

      SCM bindings = SCM_EOL;
      for (definition_idx = definitions;
           !scm_is_null (definition_idx);
           definition_idx = SCM_CDR (definition_idx))
	{
	  const SCM definition = SCM_CAR (definition_idx);
	  const SCM canonical_definition = canonicalize_define (definition);
	  const SCM binding = SCM_CDR (canonical_definition);
	  bindings = scm_cons (binding, bindings);
	};

      letrec_tail = scm_cons (bindings, sequence);
      /* FIXME: forms does not hold information about the file location.  */
      letrec_expression = scm_cons_source (forms, scm_sym_letrec, letrec_tail);
      new_letrec_expression = scm_m_letrec (letrec_expression, env);
      SCM_SETCAR (forms, new_letrec_expression);
      SCM_SETCDR (forms, SCM_EOL);
    }
  else
    {
      SCM_SETCAR (forms, SCM_CAR (sequence));
      SCM_SETCDR (forms, SCM_CDR (sequence));
    }
}

static SCM
macroexp (SCM x, SCM env)
{
  SCM res, proc, orig_sym;

  /* Don't bother to produce error messages here.  We get them when we
     eventually execute the code for real. */

 macro_tail:
  orig_sym = SCM_CAR (x);
  if (!scm_is_symbol (orig_sym))
    return x;

  {
    SCM *proc_ptr = scm_lookupcar1 (x, env, 0);
    if (proc_ptr == NULL)
      {
	/* We have lost the race. */
	goto macro_tail;
      }
    proc = *proc_ptr;
  }
  
  /* Only handle memoizing macros.  `Acros' and `macros' are really
     special forms and should not be evaluated here. */

  if (!SCM_MACROP (proc)
      || (SCM_MACRO_TYPE (proc) != 2 && !SCM_BUILTIN_MACRO_P (proc)))
    return x;

  SCM_SETCAR (x, orig_sym);  /* Undo memoizing effect of lookupcar */
  res = scm_call_2 (SCM_MACRO_CODE (proc), x, env);

  if (scm_ilength (res) <= 0)
    /* Result of expansion is not a list.  */
    return (scm_list_2 (SCM_IM_BEGIN, res));
  else
    {
      /* njrev: Several queries here: (1) I don't see how it can be
	 correct that the SCM_SETCAR 2 lines below this comment needs
	 protection, but the SCM_SETCAR 6 lines above does not, so
	 something here is probably wrong.  (2) macroexp() is now only
	 used in one place - scm_m_generalized_set_x - whereas all other
	 macro expansion happens through expand_user_macros.  Therefore
	 (2.1) perhaps macroexp() could be eliminated completely now?
	 (2.2) Does expand_user_macros need any critical section
	 protection? */

      SCM_CRITICAL_SECTION_START;
      SCM_SETCAR (x, SCM_CAR (res));
      SCM_SETCDR (x, SCM_CDR (res));
      SCM_CRITICAL_SECTION_END;

      goto macro_tail;
    }
}

/* Start of the memoizers for the standard R5RS builtin macros.  */


SCM_SYNTAX (s_and, "and", scm_i_makbimacro, scm_m_and);
SCM_GLOBAL_SYMBOL (scm_sym_and, s_and);

SCM
scm_m_and (SCM expr, SCM env SCM_UNUSED)
{
  const SCM cdr_expr = SCM_CDR (expr);
  const long length = scm_ilength (cdr_expr);

  ASSERT_SYNTAX (length >= 0, s_bad_expression, expr);

  if (length == 0)
    {
      /* Special case:  (and) is replaced by #t. */
      return SCM_BOOL_T;
    }
  else
    {
      SCM_SETCAR (expr, SCM_IM_AND);
      return expr;
    }
}

static SCM
unmemoize_and (const SCM expr, const SCM env)
{
  return scm_cons (scm_sym_and, unmemoize_exprs (SCM_CDR (expr), env));
}


SCM_SYNTAX (s_begin, "begin", scm_i_makbimacro, scm_m_begin);
SCM_GLOBAL_SYMBOL (scm_sym_begin, s_begin);

SCM
scm_m_begin (SCM expr, SCM env SCM_UNUSED)
{
  const SCM cdr_expr = SCM_CDR (expr);
  /* Dirk:FIXME:: An empty begin clause is not generally allowed by R5RS.
   * That means, there should be a distinction between uses of begin where an
   * empty clause is OK and where it is not.  */
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);

  SCM_SETCAR (expr, SCM_IM_BEGIN);
  return expr;
}

static SCM
unmemoize_begin (const SCM expr, const SCM env)
{
  return scm_cons (scm_sym_begin, unmemoize_exprs (SCM_CDR (expr), env));
}


SCM_SYNTAX (s_case, "case", scm_i_makbimacro, scm_m_case);
SCM_GLOBAL_SYMBOL (scm_sym_case, s_case);
SCM_GLOBAL_SYMBOL (scm_sym_else, "else");

SCM
scm_m_case (SCM expr, SCM env)
{
  SCM clauses;
  SCM all_labels = SCM_EOL;

  /* Check, whether 'else is a literal, i. e. not bound to a value. */
  const int else_literal_p = literal_p (scm_sym_else, env);

  const SCM cdr_expr = SCM_CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 2, s_missing_clauses, expr);

  clauses = SCM_CDR (cdr_expr);
  while (!scm_is_null (clauses))
    {
      SCM labels;

      const SCM clause = SCM_CAR (clauses);
      ASSERT_SYNTAX_2 (scm_ilength (clause) >= 2, 
		       s_bad_case_clause, clause, expr);

      labels = SCM_CAR (clause);
      if (scm_is_pair (labels))
        {
          ASSERT_SYNTAX_2 (scm_ilength (labels) >= 0,
                           s_bad_case_labels, labels, expr);
          all_labels = scm_append (scm_list_2 (labels, all_labels));
        }
      else if (scm_is_null (labels))
        {
          /* The list of labels is empty.  According to R5RS this is allowed.
           * It means that the sequence of expressions will never be executed.
           * Therefore, as an optimization, we could remove the whole
           * clause.  */
        }
      else
        {
          ASSERT_SYNTAX_2 (scm_is_eq (labels, scm_sym_else) && else_literal_p,
                           s_bad_case_labels, labels, expr);
          ASSERT_SYNTAX_2 (scm_is_null (SCM_CDR (clauses)),
                           s_misplaced_else_clause, clause, expr);
        }

      /* build the new clause */
      if (scm_is_eq (labels, scm_sym_else))
        SCM_SETCAR (clause, SCM_IM_ELSE);

      clauses = SCM_CDR (clauses);
    }

  /* Check whether all case labels are distinct. */
  for (; !scm_is_null (all_labels); all_labels = SCM_CDR (all_labels))
    {
      const SCM label = SCM_CAR (all_labels);
      ASSERT_SYNTAX_2 (scm_is_false (scm_c_memq (label, SCM_CDR (all_labels))),
                       s_duplicate_case_label, label, expr);
    }

  SCM_SETCAR (expr, SCM_IM_CASE);
  return expr;
}

static SCM
unmemoize_case (const SCM expr, const SCM env)
{
  const SCM um_key_expr = unmemoize_expression (SCM_CADR (expr), env);
  SCM um_clauses = SCM_EOL;
  SCM clause_idx;

  for (clause_idx = SCM_CDDR (expr);
       !scm_is_null (clause_idx);
       clause_idx = SCM_CDR (clause_idx))
    {
      const SCM clause = SCM_CAR (clause_idx);
      const SCM labels = SCM_CAR (clause);
      const SCM exprs = SCM_CDR (clause);

      const SCM um_exprs = unmemoize_exprs (exprs, env);
      const SCM um_labels = (scm_is_eq (labels, SCM_IM_ELSE))
        ? scm_sym_else
        : scm_i_finite_list_copy (labels);
      const SCM um_clause = scm_cons (um_labels, um_exprs);

      um_clauses = scm_cons (um_clause, um_clauses);
    }
  um_clauses = scm_reverse_x (um_clauses, SCM_UNDEFINED);

  return scm_cons2 (scm_sym_case, um_key_expr, um_clauses);
}


SCM_SYNTAX (s_cond, "cond", scm_i_makbimacro, scm_m_cond);
SCM_GLOBAL_SYMBOL (scm_sym_cond, s_cond);
SCM_GLOBAL_SYMBOL (scm_sym_arrow, "=>");

SCM
scm_m_cond (SCM expr, SCM env)
{
  /* Check, whether 'else or '=> is a literal, i. e. not bound to a value. */
  const int else_literal_p = literal_p (scm_sym_else, env);
  const int arrow_literal_p = literal_p (scm_sym_arrow, env);

  const SCM clauses = SCM_CDR (expr);
  SCM clause_idx;

  ASSERT_SYNTAX (scm_ilength (clauses) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (clauses) >= 1, s_missing_clauses, expr);

  for (clause_idx = clauses;
       !scm_is_null (clause_idx);
       clause_idx = SCM_CDR (clause_idx))
    {
      SCM test;

      const SCM clause = SCM_CAR (clause_idx);
      const long length = scm_ilength (clause);
      ASSERT_SYNTAX_2 (length >= 1, s_bad_cond_clause, clause, expr);

      test = SCM_CAR (clause);
      if (scm_is_eq (test, scm_sym_else) && else_literal_p)
	{
	  const int last_clause_p = scm_is_null (SCM_CDR (clause_idx));
          ASSERT_SYNTAX_2 (length >= 2,
                           s_bad_cond_clause, clause, expr);
          ASSERT_SYNTAX_2 (last_clause_p,
                           s_misplaced_else_clause, clause, expr);
          SCM_SETCAR (clause, SCM_IM_ELSE);
	}
      else if (length >= 2
               && scm_is_eq (SCM_CADR (clause), scm_sym_arrow)
               && arrow_literal_p)
        {
          ASSERT_SYNTAX_2 (length > 2, s_missing_recipient, clause, expr);
          ASSERT_SYNTAX_2 (length == 3, s_extra_expression, clause, expr);
          SCM_SETCAR (SCM_CDR (clause), SCM_IM_ARROW);
	}
      /* SRFI 61 extended cond */
      else if (length >= 3
	       && scm_is_eq (SCM_CADDR (clause), scm_sym_arrow)
	       && arrow_literal_p)
	{
	  ASSERT_SYNTAX_2 (length > 3, s_missing_recipient, clause, expr);
	  ASSERT_SYNTAX_2 (length == 4, s_extra_expression, clause, expr);
	  SCM_SETCAR (SCM_CDDR (clause), SCM_IM_ARROW);
	}
    }

  SCM_SETCAR (expr, SCM_IM_COND);
  return expr;
}

static SCM
unmemoize_cond (const SCM expr, const SCM env)
{
  SCM um_clauses = SCM_EOL;
  SCM clause_idx;

  for (clause_idx = SCM_CDR (expr);
       !scm_is_null (clause_idx);
       clause_idx = SCM_CDR (clause_idx))
    {
      const SCM clause = SCM_CAR (clause_idx);
      const SCM sequence = SCM_CDR (clause);
      const SCM test = SCM_CAR (clause);
      SCM um_test;
      SCM um_sequence;
      SCM um_clause;

      if (scm_is_eq (test, SCM_IM_ELSE))
        um_test = scm_sym_else;
      else
        um_test = unmemoize_expression (test, env);

      if (!scm_is_null (sequence) && scm_is_eq (SCM_CAR (sequence),
					      SCM_IM_ARROW))
        {
          const SCM target = SCM_CADR (sequence);
          const SCM um_target = unmemoize_expression (target, env);
          um_sequence = scm_list_2 (scm_sym_arrow, um_target);
        }
      else
        {
          um_sequence = unmemoize_exprs (sequence, env);
        }

      um_clause = scm_cons (um_test, um_sequence);
      um_clauses = scm_cons (um_clause, um_clauses);
    }
  um_clauses = scm_reverse_x (um_clauses, SCM_UNDEFINED);

  return scm_cons (scm_sym_cond, um_clauses);
}


SCM_SYNTAX (s_define, "define", scm_i_makbimacro, scm_m_define);
SCM_GLOBAL_SYMBOL (scm_sym_define, s_define);

/* Guile provides an extension to R5RS' define syntax to represent function
 * currying in a compact way.  With this extension, it is allowed to write
 * (define <nested-variable> <body>), where <nested-variable> has of one of
 * the forms (<nested-variable> <formals>), (<nested-variable> . <formal>),  
 * (<variable> <formals>) or (<variable> . <formal>).  As in R5RS, <formals>
 * should be either a sequence of zero or more variables, or a sequence of one
 * or more variables followed by a space-delimited period and another
 * variable.  Each level of argument nesting wraps the <body> within another
 * lambda expression.  For example, the following forms are allowed, each one
 * followed by an equivalent, more explicit implementation.
 * Example 1:
 *   (define ((a b . c) . d) <body>)  is equivalent to
 *   (define a (lambda (b . c) (lambda d <body>)))
 * Example 2:
 *   (define (((a) b) c . d) <body>)  is equivalent to
 *   (define a (lambda () (lambda (b) (lambda (c . d) <body>))))
 */
/* Dirk:FIXME:: We should provide an implementation for 'define' in the R5RS
 * module that does not implement this extension.  */
static SCM
canonicalize_define (const SCM expr)
{
  SCM body;
  SCM variable;

  const SCM cdr_expr = SCM_CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 2, s_missing_expression, expr);

  body = SCM_CDR (cdr_expr);
  variable = SCM_CAR (cdr_expr);
  while (scm_is_pair (variable))
    {
      /* This while loop realizes function currying by variable nesting.
       * Variable is known to be a nested-variable.  In every iteration of the
       * loop another level of lambda expression is created, starting with the
       * innermost one.  Note that we don't check for duplicate formals here:
       * This will be done by the memoizer of the lambda expression.  */
      const SCM formals = SCM_CDR (variable);
      const SCM tail = scm_cons (formals, body);

      /* Add source properties to each new lambda expression:  */
      const SCM lambda = scm_cons_source (variable, scm_sym_lambda, tail);

      body = scm_list_1 (lambda);
      variable = SCM_CAR (variable);
    }
  ASSERT_SYNTAX_2 (scm_is_symbol (variable), s_bad_variable, variable, expr);
  ASSERT_SYNTAX (scm_ilength (body) == 1, s_expression, expr);

  SCM_SETCAR (cdr_expr, variable);
  SCM_SETCDR (cdr_expr, body);
  return expr;
}

/* According to Section 5.2.1 of R5RS we first have to make sure that the
   variable is bound, and then perform the `(set! variable expression)'
   operation.  However, EXPRESSION _can_ be evaluated before VARIABLE is
   bound.  This means that EXPRESSION won't necessarily be able to assign
   values to VARIABLE as in `(define foo (begin (set! foo 1) (+ foo 1)))'.  */
SCM
scm_m_define (SCM expr, SCM env)
{
  ASSERT_SYNTAX (SCM_TOP_LEVEL (env), s_bad_define, expr);

  {
    const SCM canonical_definition = canonicalize_define (expr);
    const SCM cdr_canonical_definition = SCM_CDR (canonical_definition);
    const SCM variable = SCM_CAR (cdr_canonical_definition);
    const SCM value = scm_eval_car (SCM_CDR (cdr_canonical_definition), env);
    const SCM location
      = scm_sym2var (variable, scm_env_top_level (env), SCM_BOOL_T);

    if (SCM_REC_PROCNAMES_P)
      {
        SCM tmp = value;
        while (SCM_MACROP (tmp))
          tmp = SCM_MACRO_CODE (tmp);
        if (scm_is_true (scm_procedure_p (tmp))
            /* Only the first definition determines the name. */
            && scm_is_false (scm_procedure_property (tmp, scm_sym_name)))
          scm_set_procedure_property_x (tmp, scm_sym_name, variable);
      }

    SCM_VARIABLE_SET (location, value);

    return SCM_UNSPECIFIED;
  }
}


/* This is a helper function for forms (<keyword> <expression>) that are
 * transformed into (#@<keyword> '() <memoized_expression>) in order to allow
 * for easy creation of a thunk (i. e. a closure without arguments) using the
 * ('() <memoized_expression>) tail of the memoized form.  */
static SCM
memoize_as_thunk_prototype (const SCM expr, const SCM env SCM_UNUSED)
{
  const SCM cdr_expr = SCM_CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) == 1, s_expression, expr);

  SCM_SETCDR (expr, scm_cons (SCM_EOL, cdr_expr));

  return expr;
}


SCM_SYNTAX (s_delay, "delay", scm_i_makbimacro, scm_m_delay);
SCM_GLOBAL_SYMBOL (scm_sym_delay, s_delay);

/* Promises are implemented as closures with an empty parameter list.  Thus,
 * (delay <expression>) is transformed into (#@delay '() <expression>), where
 * the empty list represents the empty parameter list.  This representation
 * allows for easy creation of the closure during evaluation.  */
SCM
scm_m_delay (SCM expr, SCM env)
{
  const SCM new_expr = memoize_as_thunk_prototype (expr, env);
  SCM_SETCAR (new_expr, SCM_IM_DELAY);
  return new_expr;
}

static SCM
unmemoize_delay (const SCM expr, const SCM env)
{
  const SCM thunk_expr = SCM_CADDR (expr);
  /* A promise is implemented as a closure, and when applying a
     closure the evaluator adds a new frame to the environment - even
     though, in the case of a promise, the added frame is always
     empty.  We need to extend the environment here in the same way,
     so that any ILOCs in thunk_expr can be unmemoized correctly. */
  const SCM new_env = SCM_EXTEND_ENV (SCM_EOL, SCM_EOL, env);
  return scm_list_2 (scm_sym_delay, unmemoize_expression (thunk_expr, new_env));
}


SCM_SYNTAX(s_do, "do", scm_i_makbimacro, scm_m_do);
SCM_GLOBAL_SYMBOL(scm_sym_do, s_do);

/* DO gets the most radically altered syntax.  The order of the vars is
 * reversed here.  During the evaluation this allows for simple consing of the
 * results of the inits and steps:

   (do ((<var1> <init1> <step1>)
        (<var2> <init2>)
        ... )
       (<test> <return>)
     <body>)

   ;; becomes

   (#@do (<init1> <init2> ... <initn>)
         (varn ... var2 var1)
         (<test> <return>)
         (<body>)
     <step1> <step2> ... <stepn>) ;; missing steps replaced by var
 */
SCM 
scm_m_do (SCM expr, SCM env SCM_UNUSED)
{
  SCM variables = SCM_EOL;
  SCM init_forms = SCM_EOL;
  SCM step_forms = SCM_EOL;
  SCM binding_idx;
  SCM cddr_expr;
  SCM exit_clause;
  SCM commands;
  SCM tail;

  const SCM cdr_expr = SCM_CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 2, s_missing_expression, expr);

  /* Collect variables, init and step forms. */
  binding_idx = SCM_CAR (cdr_expr);
  ASSERT_SYNTAX_2 (scm_ilength (binding_idx) >= 0,
                   s_bad_bindings, binding_idx, expr);
  for (; !scm_is_null (binding_idx); binding_idx = SCM_CDR (binding_idx))
    {
      const SCM binding = SCM_CAR (binding_idx);
      const long length = scm_ilength (binding);
      ASSERT_SYNTAX_2 (length == 2 || length == 3,
                       s_bad_binding, binding, expr);

      {
        const SCM name = SCM_CAR (binding);
        const SCM init = SCM_CADR (binding);
        const SCM step = (length == 2) ? name : SCM_CADDR (binding);
        ASSERT_SYNTAX_2 (scm_is_symbol (name), s_bad_variable, name, expr);
        ASSERT_SYNTAX_2 (scm_is_false (scm_c_memq (name, variables)),
                         s_duplicate_binding, name, expr);

        variables = scm_cons (name, variables);
        init_forms = scm_cons (init, init_forms);
        step_forms = scm_cons (step, step_forms);
      }
    }
  init_forms = scm_reverse_x (init_forms, SCM_UNDEFINED);
  step_forms = scm_reverse_x (step_forms, SCM_UNDEFINED);

  /* Memoize the test form and the exit sequence. */
  cddr_expr = SCM_CDR (cdr_expr);
  exit_clause = SCM_CAR (cddr_expr);
  ASSERT_SYNTAX_2 (scm_ilength (exit_clause) >= 1,
                   s_bad_exit_clause, exit_clause, expr);

  commands = SCM_CDR (cddr_expr);
  tail = scm_cons2 (exit_clause, commands, step_forms);
  tail = scm_cons2 (init_forms, variables, tail);
  SCM_SETCAR (expr, SCM_IM_DO);
  SCM_SETCDR (expr, tail);
  return expr;
}

static SCM
unmemoize_do (const SCM expr, const SCM env)
{
  const SCM cdr_expr = SCM_CDR (expr);
  const SCM cddr_expr = SCM_CDR (cdr_expr);
  const SCM rnames = SCM_CAR (cddr_expr);
  const SCM extended_env = SCM_EXTEND_ENV (rnames, SCM_EOL, env);
  const SCM cdddr_expr = SCM_CDR (cddr_expr);
  const SCM exit_sequence = SCM_CAR (cdddr_expr);
  const SCM um_exit_sequence = unmemoize_exprs (exit_sequence, extended_env);
  const SCM cddddr_expr = SCM_CDR (cdddr_expr);
  const SCM um_body = unmemoize_exprs (SCM_CAR (cddddr_expr), extended_env);

  /* build transformed binding list */
  SCM um_names = scm_reverse (rnames);
  SCM um_inits = unmemoize_exprs (SCM_CAR (cdr_expr), env);
  SCM um_steps = unmemoize_exprs (SCM_CDR (cddddr_expr), extended_env);
  SCM um_bindings = SCM_EOL;
  while (!scm_is_null (um_names))
    {
      const SCM name = SCM_CAR (um_names);
      const SCM init = SCM_CAR (um_inits);
      SCM step = SCM_CAR (um_steps);
      step = scm_is_eq (step, name) ? SCM_EOL : scm_list_1 (step);

      um_bindings = scm_cons (scm_cons2 (name, init, step), um_bindings);

      um_names = SCM_CDR (um_names);
      um_inits = SCM_CDR (um_inits);
      um_steps = SCM_CDR (um_steps);
    }
  um_bindings = scm_reverse_x (um_bindings, SCM_UNDEFINED);

  return scm_cons (scm_sym_do,
                   scm_cons2 (um_bindings, um_exit_sequence, um_body));
}


SCM_SYNTAX (s_if, "if", scm_i_makbimacro, scm_m_if);
SCM_GLOBAL_SYMBOL (scm_sym_if, s_if);

SCM
scm_m_if (SCM expr, SCM env SCM_UNUSED)
{
  const SCM cdr_expr = SCM_CDR (expr);
  const long length = scm_ilength (cdr_expr);
  ASSERT_SYNTAX (length == 2 || length == 3, s_expression, expr);
  SCM_SETCAR (expr, SCM_IM_IF);
  return expr;
}

static SCM
unmemoize_if (const SCM expr, const SCM env)
{
  const SCM cdr_expr = SCM_CDR (expr);
  const SCM um_condition = unmemoize_expression (SCM_CAR (cdr_expr), env);
  const SCM cddr_expr = SCM_CDR (cdr_expr);
  const SCM um_then = unmemoize_expression (SCM_CAR (cddr_expr), env);
  const SCM cdddr_expr = SCM_CDR (cddr_expr);

  if (scm_is_null (cdddr_expr))
    {
      return scm_list_3 (scm_sym_if, um_condition, um_then);
    }
  else
    {
      const SCM um_else = unmemoize_expression (SCM_CAR (cdddr_expr), env);
      return scm_list_4 (scm_sym_if, um_condition, um_then, um_else);
    }
}


SCM_SYNTAX (s_lambda, "lambda", scm_i_makbimacro, scm_m_lambda);
SCM_GLOBAL_SYMBOL (scm_sym_lambda, s_lambda);

/* A helper function for memoize_lambda to support checking for duplicate
 * formal arguments: Return true if OBJ is `eq?' to one of the elements of
 * LIST or to the cdr of the last cons.  Therefore, LIST may have any of the
 * forms that a formal argument can have:
 *   <rest>, (<arg1> ...), (<arg1> ...  .  <rest>) */
static int
c_improper_memq (SCM obj, SCM list)
{
  for (; scm_is_pair (list); list = SCM_CDR (list))
    {
      if (scm_is_eq (SCM_CAR (list), obj))
        return 1;
    }
  return scm_is_eq (list, obj);
}

SCM
scm_m_lambda (SCM expr, SCM env SCM_UNUSED)
{
  SCM formals;
  SCM formals_idx;
  SCM cddr_expr;
  int documentation;
  SCM body;
  SCM new_body;

  const SCM cdr_expr = SCM_CDR (expr);
  const long length = scm_ilength (cdr_expr);
  ASSERT_SYNTAX (length >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (length >= 2, s_missing_expression, expr);

  /* Before iterating the list of formal arguments, make sure the formals
   * actually are given as either a symbol or a non-cyclic list.  */
  formals = SCM_CAR (cdr_expr);
  if (scm_is_pair (formals))
    {
      /* Dirk:FIXME:: We should check for a cyclic list of formals, and if
       * detected, report a 'Bad formals' error.  */
    }
  else
    {
      ASSERT_SYNTAX_2 (scm_is_symbol (formals) || scm_is_null (formals),
                       s_bad_formals, formals, expr);
    }

  /* Now iterate the list of formal arguments to check if all formals are
   * symbols, and that there are no duplicates.  */
  formals_idx = formals;
  while (scm_is_pair (formals_idx))
    {
      const SCM formal = SCM_CAR (formals_idx);
      const SCM next_idx = SCM_CDR (formals_idx);
      ASSERT_SYNTAX_2 (scm_is_symbol (formal), s_bad_formal, formal, expr);
      ASSERT_SYNTAX_2 (!c_improper_memq (formal, next_idx),
                       s_duplicate_formal, formal, expr);
      formals_idx = next_idx;
    }
  ASSERT_SYNTAX_2 (scm_is_null (formals_idx) || scm_is_symbol (formals_idx),
                   s_bad_formal, formals_idx, expr);

  /* Memoize the body.  Keep a potential documentation string.  */
  /* Dirk:FIXME:: We should probably extract the documentation string to
   * some external database.  Otherwise it will slow down execution, since
   * the documentation string will have to be skipped with every execution
   * of the closure.  */
  cddr_expr = SCM_CDR (cdr_expr);
  documentation = (length >= 3 && scm_is_string (SCM_CAR (cddr_expr)));
  body = documentation ? SCM_CDR (cddr_expr) : cddr_expr;
  new_body = m_body (SCM_IM_LAMBDA, body);

  SCM_SETCAR (expr, SCM_IM_LAMBDA);
  if (documentation)
    SCM_SETCDR (cddr_expr, new_body);
  else
    SCM_SETCDR (cdr_expr, new_body);
  return expr;
}

static SCM
unmemoize_lambda (const SCM expr, const SCM env)
{
  const SCM formals = SCM_CADR (expr);
  const SCM body = SCM_CDDR (expr);

  const SCM new_env = SCM_EXTEND_ENV (formals, SCM_EOL, env);
  const SCM um_formals = scm_i_finite_list_copy (formals);
  const SCM um_body = unmemoize_exprs (body, new_env);

  return scm_cons2 (scm_sym_lambda, um_formals, um_body);
}


/* Check if the format of the bindings is ((<symbol> <init-form>) ...).  */
static void
check_bindings (const SCM bindings, const SCM expr)
{
  SCM binding_idx;

  ASSERT_SYNTAX_2 (scm_ilength (bindings) >= 0,
                   s_bad_bindings, bindings, expr);

  binding_idx = bindings;
  for (; !scm_is_null (binding_idx); binding_idx = SCM_CDR (binding_idx))
    {
      SCM name;         /* const */

      const SCM binding = SCM_CAR (binding_idx);
      ASSERT_SYNTAX_2 (scm_ilength (binding) == 2,
                       s_bad_binding, binding, expr);

      name = SCM_CAR (binding);
      ASSERT_SYNTAX_2 (scm_is_symbol (name), s_bad_variable, name, expr);
    }
}


/* The bindings, which must have the format ((v1 i1) (v2 i2) ... (vn in)), are
 * transformed to the lists (vn ... v2 v1) and (i1 i2 ... in).  That is, the
 * variables are returned in a list with their order reversed, and the init
 * forms are returned in a list in the same order as they are given in the
 * bindings.  If a duplicate variable name is detected, an error is
 * signalled.  */
static void
transform_bindings (
  const SCM bindings, const SCM expr,
  SCM *const rvarptr, SCM *const initptr )
{
  SCM rvariables = SCM_EOL;
  SCM rinits = SCM_EOL;
  SCM binding_idx = bindings;
  for (; !scm_is_null (binding_idx); binding_idx = SCM_CDR (binding_idx))
    {
      const SCM binding = SCM_CAR (binding_idx);
      const SCM cdr_binding = SCM_CDR (binding);
      const SCM name = SCM_CAR (binding);
      ASSERT_SYNTAX_2 (scm_is_false (scm_c_memq (name, rvariables)),
                       s_duplicate_binding, name, expr);
      rvariables = scm_cons (name, rvariables);
      rinits = scm_cons (SCM_CAR (cdr_binding), rinits);
    }
  *rvarptr = rvariables;
  *initptr = scm_reverse_x (rinits, SCM_UNDEFINED);
}


SCM_SYNTAX(s_let, "let", scm_i_makbimacro, scm_m_let);
SCM_GLOBAL_SYMBOL(scm_sym_let, s_let);

/* This function is a helper function for memoize_let.  It transforms
 * (let name ((var init) ...) body ...) into
 * ((letrec ((name (lambda (var ...) body ...))) name) init ...)
 * and memoizes the expression.  It is assumed that the caller has checked
 * that name is a symbol and that there are bindings and a body.  */
static SCM
memoize_named_let (const SCM expr, const SCM env SCM_UNUSED)
{
  SCM rvariables;
  SCM variables;
  SCM inits;

  const SCM cdr_expr = SCM_CDR (expr);
  const SCM name = SCM_CAR (cdr_expr);
  const SCM cddr_expr = SCM_CDR (cdr_expr);
  const SCM bindings = SCM_CAR (cddr_expr);
  check_bindings (bindings, expr);

  transform_bindings (bindings, expr, &rvariables, &inits);
  variables = scm_reverse_x (rvariables, SCM_UNDEFINED);

  {
    const SCM let_body = SCM_CDR (cddr_expr);
    const SCM lambda_body = m_body (SCM_IM_LET, let_body);
    const SCM lambda_tail = scm_cons (variables, lambda_body);
    const SCM lambda_form = scm_cons_source (expr, scm_sym_lambda, lambda_tail);

    const SCM rvar = scm_list_1 (name);
    const SCM init = scm_list_1 (lambda_form);
    const SCM body = m_body (SCM_IM_LET, scm_list_1 (name));
    const SCM letrec_tail = scm_cons (rvar, scm_cons (init, body));
    const SCM letrec_form = scm_cons_source (expr, SCM_IM_LETREC, letrec_tail);
    return scm_cons_source (expr, letrec_form, inits);
  }
}

/* (let ((v1 i1) (v2 i2) ...) body) with variables v1 .. vn and initializers
 * i1 .. in is transformed to (#@let (vn ... v2 v1) (i1 i2 ...) body).  */
SCM
scm_m_let (SCM expr, SCM env)
{
  SCM bindings;

  const SCM cdr_expr = SCM_CDR (expr);
  const long length = scm_ilength (cdr_expr);
  ASSERT_SYNTAX (length >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (length >= 2, s_missing_expression, expr);

  bindings = SCM_CAR (cdr_expr);
  if (scm_is_symbol (bindings))
    {
      ASSERT_SYNTAX (length >= 3, s_missing_expression, expr);
      return memoize_named_let (expr, env);
    }

  check_bindings (bindings, expr);
  if (scm_is_null (bindings) || scm_is_null (SCM_CDR (bindings)))
    {
      /* Special case: no bindings or single binding => let* is faster. */
      const SCM body = m_body (SCM_IM_LET, SCM_CDR (cdr_expr));
      return scm_m_letstar (scm_cons2 (SCM_CAR (expr), bindings, body), env);
    }
  else
    {
      /* plain let */
      SCM rvariables;
      SCM inits;
      transform_bindings (bindings, expr, &rvariables, &inits);

      {
        const SCM new_body = m_body (SCM_IM_LET, SCM_CDR (cdr_expr));
        const SCM new_tail = scm_cons2 (rvariables, inits, new_body);
        SCM_SETCAR (expr, SCM_IM_LET);
        SCM_SETCDR (expr, new_tail);
        return expr;
      }
    }
}

static SCM
build_binding_list (SCM rnames, SCM rinits)
{
  SCM bindings = SCM_EOL;
  while (!scm_is_null (rnames))
    {
      const SCM binding = scm_list_2 (SCM_CAR (rnames), SCM_CAR (rinits));
      bindings = scm_cons (binding, bindings);
      rnames = SCM_CDR (rnames);
      rinits = SCM_CDR (rinits);
    }
  return bindings;
}

static SCM
unmemoize_let (const SCM expr, const SCM env)
{
  const SCM cdr_expr = SCM_CDR (expr);
  const SCM um_rnames = SCM_CAR (cdr_expr);
  const SCM extended_env = SCM_EXTEND_ENV (um_rnames, SCM_EOL, env);
  const SCM cddr_expr = SCM_CDR (cdr_expr);
  const SCM um_inits = unmemoize_exprs (SCM_CAR (cddr_expr), env);
  const SCM um_rinits = scm_reverse_x (um_inits, SCM_UNDEFINED);
  const SCM um_bindings = build_binding_list (um_rnames, um_rinits);
  const SCM um_body = unmemoize_exprs (SCM_CDR (cddr_expr), extended_env);

  return scm_cons2 (scm_sym_let, um_bindings, um_body);
}


SCM_SYNTAX(s_letrec, "letrec", scm_i_makbimacro, scm_m_letrec);
SCM_GLOBAL_SYMBOL(scm_sym_letrec, s_letrec);

SCM 
scm_m_letrec (SCM expr, SCM env)
{
  SCM bindings;

  const SCM cdr_expr = SCM_CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 2, s_missing_expression, expr);

  bindings = SCM_CAR (cdr_expr);
  if (scm_is_null (bindings))
    {
      /* no bindings, let* is executed faster */
      SCM body = m_body (SCM_IM_LETREC, SCM_CDR (cdr_expr));
      return scm_m_letstar (scm_cons2 (SCM_CAR (expr), SCM_EOL, body), env);
    }
  else
    {
      SCM rvariables;
      SCM inits;
      SCM new_body;

      check_bindings (bindings, expr);
      transform_bindings (bindings, expr, &rvariables, &inits);
      new_body = m_body (SCM_IM_LETREC, SCM_CDR (cdr_expr));
      return scm_cons2 (SCM_IM_LETREC, rvariables, scm_cons (inits, new_body));
    }
}

static SCM
unmemoize_letrec (const SCM expr, const SCM env)
{
  const SCM cdr_expr = SCM_CDR (expr);
  const SCM um_rnames = SCM_CAR (cdr_expr);
  const SCM extended_env = SCM_EXTEND_ENV (um_rnames, SCM_EOL, env);
  const SCM cddr_expr = SCM_CDR (cdr_expr);
  const SCM um_inits = unmemoize_exprs (SCM_CAR (cddr_expr), extended_env);
  const SCM um_rinits = scm_reverse_x (um_inits, SCM_UNDEFINED);
  const SCM um_bindings = build_binding_list (um_rnames, um_rinits);
  const SCM um_body = unmemoize_exprs (SCM_CDR (cddr_expr), extended_env);

  return scm_cons2 (scm_sym_letrec, um_bindings, um_body);
}



SCM_SYNTAX (s_letstar, "let*", scm_i_makbimacro, scm_m_letstar);
SCM_GLOBAL_SYMBOL (scm_sym_letstar, s_letstar);

/* (let* ((v1 i1) (v2 i2) ...) body) with variables v1 .. vn and initializers
 * i1 .. in is transformed into the form (#@let* (v1 i1 v2 i2 ...) body).  */
SCM
scm_m_letstar (SCM expr, SCM env SCM_UNUSED)
{
  SCM binding_idx;
  SCM new_body;

  const SCM cdr_expr = SCM_CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 2, s_missing_expression, expr);

  binding_idx = SCM_CAR (cdr_expr);
  check_bindings (binding_idx, expr);

  /* Transform ((v1 i1) (v2 i2) ...) into (v1 i1 v2 i2 ...).  The
   * transformation is done in place.  At the beginning of one iteration of
   * the loop the variable binding_idx holds the form
   *   P1:( (vn . P2:(in . ())) . P3:( (vn+1 in+1) ... ) ),
   * where P1, P2 and P3 indicate the pairs, that are relevant for the
   * transformation.  P1 and P2 are modified in the loop, P3 remains
   * untouched.  After the execution of the loop, P1 will hold
   *   P1:( vn . P2:(in . P3:( (vn+1 in+1) ... )) )
   * and binding_idx will hold P3.  */
  while (!scm_is_null (binding_idx))
    {
      const SCM cdr_binding_idx = SCM_CDR (binding_idx);  /* remember P3 */
      const SCM binding = SCM_CAR (binding_idx);
      const SCM name = SCM_CAR (binding);
      const SCM cdr_binding = SCM_CDR (binding);

      SCM_SETCDR (cdr_binding, cdr_binding_idx);        /* update P2 */
      SCM_SETCAR (binding_idx, name);                   /* update P1 */
      SCM_SETCDR (binding_idx, cdr_binding);            /* update P1 */

      binding_idx = cdr_binding_idx;                    /* continue with P3 */
    }

  new_body = m_body (SCM_IM_LETSTAR, SCM_CDR (cdr_expr));
  SCM_SETCAR (expr, SCM_IM_LETSTAR);
  /* the bindings have been changed in place */
  SCM_SETCDR (cdr_expr, new_body);
  return expr;
}

static SCM
unmemoize_letstar (const SCM expr, const SCM env)
{
  const SCM cdr_expr = SCM_CDR (expr);
  const SCM body = SCM_CDR (cdr_expr);
  SCM bindings = SCM_CAR (cdr_expr);
  SCM um_bindings = SCM_EOL;
  SCM extended_env = env;
  SCM um_body;

  while (!scm_is_null (bindings))
    {
      const SCM variable = SCM_CAR (bindings);
      const SCM init = SCM_CADR (bindings);
      const SCM um_init = unmemoize_expression (init, extended_env);
      um_bindings = scm_cons (scm_list_2 (variable, um_init), um_bindings);
      extended_env = SCM_EXTEND_ENV (variable, SCM_BOOL_F, extended_env);
      bindings = SCM_CDDR (bindings);
    }
  um_bindings = scm_reverse_x (um_bindings, SCM_UNDEFINED);

  um_body = unmemoize_exprs (body, extended_env);

  return scm_cons2 (scm_sym_letstar, um_bindings, um_body);
}


SCM_SYNTAX (s_or, "or", scm_i_makbimacro, scm_m_or);
SCM_GLOBAL_SYMBOL (scm_sym_or, s_or);

SCM
scm_m_or (SCM expr, SCM env SCM_UNUSED)
{
  const SCM cdr_expr = SCM_CDR (expr);
  const long length = scm_ilength (cdr_expr);

  ASSERT_SYNTAX (length >= 0, s_bad_expression, expr);

  if (length == 0)
    {
      /* Special case:  (or) is replaced by #f. */
      return SCM_BOOL_F;
    }
  else
    {
      SCM_SETCAR (expr, SCM_IM_OR);
      return expr;
    }
}

static SCM
unmemoize_or (const SCM expr, const SCM env)
{
  return scm_cons (scm_sym_or, unmemoize_exprs (SCM_CDR (expr), env));
}


SCM_SYNTAX (s_quasiquote, "quasiquote", scm_makacro, scm_m_quasiquote);
SCM_GLOBAL_SYMBOL (scm_sym_quasiquote, s_quasiquote);
SCM_GLOBAL_SYMBOL (scm_sym_unquote, "unquote");
SCM_GLOBAL_SYMBOL (scm_sym_uq_splicing, "unquote-splicing");

/* Internal function to handle a quasiquotation:  'form' is the parameter in
 * the call (quasiquotation form), 'env' is the environment where unquoted
 * expressions will be evaluated, and 'depth' is the current quasiquotation
 * nesting level and is known to be greater than zero.  */
static SCM 
iqq (SCM form, SCM env, unsigned long int depth)
{
  if (scm_is_pair (form))
    {
      const SCM tmp = SCM_CAR (form);
      if (scm_is_eq (tmp, scm_sym_quasiquote))
	{
	  const SCM args = SCM_CDR (form);
	  ASSERT_SYNTAX (scm_ilength (args) == 1, s_expression, form);
	  return scm_list_2 (tmp, iqq (SCM_CAR (args), env, depth + 1));
	}
      else if (scm_is_eq (tmp, scm_sym_unquote))
	{
	  const SCM args = SCM_CDR (form);
	  ASSERT_SYNTAX (scm_ilength (args) == 1, s_expression, form);
	  if (depth - 1 == 0)
	    return scm_eval_car (args, env);
	  else
	    return scm_list_2 (tmp, iqq (SCM_CAR (args), env, depth - 1));
	}
      else if (scm_is_pair (tmp)
	       && scm_is_eq (SCM_CAR (tmp), scm_sym_uq_splicing))
	{
	  const SCM args = SCM_CDR (tmp);
	  ASSERT_SYNTAX (scm_ilength (args) == 1, s_expression, form);
	  if (depth - 1 == 0)
	    {
	      const SCM list = scm_eval_car (args, env);
	      const SCM rest = SCM_CDR (form);
	      ASSERT_SYNTAX_2 (scm_ilength (list) >= 0,
			       s_splicing, list, form);
	      return scm_append (scm_list_2 (list, iqq (rest, env, depth)));
	    }
	  else
	    return scm_cons (iqq (SCM_CAR (form), env, depth - 1),
			     iqq (SCM_CDR (form), env, depth));
	}
      else
	return scm_cons (iqq (SCM_CAR (form), env, depth),
			 iqq (SCM_CDR (form), env, depth));
    }
  else if (scm_is_vector (form))
    return scm_vector (iqq (scm_vector_to_list (form), env, depth));
  else
    return form;
}

SCM 
scm_m_quasiquote (SCM expr, SCM env)
{
  const SCM cdr_expr = SCM_CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) == 1, s_expression, expr);
  return iqq (SCM_CAR (cdr_expr), env, 1);
}


SCM_SYNTAX (s_quote, "quote", scm_i_makbimacro, scm_m_quote);
SCM_GLOBAL_SYMBOL (scm_sym_quote, s_quote);

SCM
scm_m_quote (SCM expr, SCM env SCM_UNUSED)
{
  SCM quotee;

  const SCM cdr_expr = SCM_CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) == 1, s_expression, expr);
  quotee = SCM_CAR (cdr_expr);
  if (is_self_quoting_p (quotee))
    return quotee;

  SCM_SETCAR (expr, SCM_IM_QUOTE);
  SCM_SETCDR (expr, quotee);
  return expr;
}

static SCM
unmemoize_quote (const SCM expr, const SCM env SCM_UNUSED)
{
  return scm_list_2 (scm_sym_quote, SCM_CDR (expr));
}


/* Will go into the RnRS module when Guile is factorized.
SCM_SYNTAX (s_set_x, "set!", scm_i_makbimacro, scm_m_set_x); */
static const char s_set_x[] = "set!";
SCM_GLOBAL_SYMBOL (scm_sym_set_x, s_set_x);

SCM
scm_m_set_x (SCM expr, SCM env SCM_UNUSED)
{
  SCM variable;
  SCM new_variable;

  const SCM cdr_expr = SCM_CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) == 2, s_expression, expr);
  variable = SCM_CAR (cdr_expr);

  /* Memoize the variable form. */
  ASSERT_SYNTAX_2 (scm_is_symbol (variable), s_bad_variable, variable, expr);
  new_variable = lookup_symbol (variable, env);
  /* Leave the memoization of unbound symbols to lazy memoization: */
  if (SCM_UNBNDP (new_variable))
    new_variable = variable;

  SCM_SETCAR (expr, SCM_IM_SET_X);
  SCM_SETCAR (cdr_expr, new_variable);
  return expr;
}

static SCM
unmemoize_set_x (const SCM expr, const SCM env)
{
  return scm_cons (scm_sym_set_x, unmemoize_exprs (SCM_CDR (expr), env));
}


/* Start of the memoizers for non-R5RS builtin macros.  */


SCM_SYNTAX (s_atapply, "@apply", scm_i_makbimacro, scm_m_apply);
SCM_GLOBAL_SYMBOL (scm_sym_atapply, s_atapply);
SCM_GLOBAL_SYMBOL (scm_sym_apply, s_atapply + 1);

SCM 
scm_m_apply (SCM expr, SCM env SCM_UNUSED)
{
  const SCM cdr_expr = SCM_CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) == 2, s_missing_expression, expr);

  SCM_SETCAR (expr, SCM_IM_APPLY);
  return expr;
}

static SCM
unmemoize_apply (const SCM expr, const SCM env)
{
  return scm_list_2 (scm_sym_atapply, unmemoize_exprs (SCM_CDR (expr), env));
}


SCM_SYNTAX (s_atbind, "@bind", scm_i_makbimacro, scm_m_atbind);

/* FIXME: The following explanation should go into the documentation: */
/* (@bind ((var init) ...) body ...) will assign the values of the `init's to
 * the global variables named by `var's (symbols, not evaluated), creating
 * them if they don't exist, executes body, and then restores the previous
 * values of the `var's.  Additionally, whenever control leaves body, the
 * values of the `var's are saved and restored when control returns.  It is an
 * error when a symbol appears more than once among the `var's.  All `init's
 * are evaluated before any `var' is set.
 *
 * Think of this as `let' for dynamic scope.
 */

/* (@bind ((var1 exp1) ... (varn expn)) body ...) is memoized into
 * (#@bind ((varn ... var1) . (exp1 ... expn)) body ...).
 *
 * FIXME - also implement `@bind*'.
 */
SCM
scm_m_atbind (SCM expr, SCM env)
{
  SCM bindings;
  SCM rvariables;
  SCM inits;
  SCM variable_idx;

  const SCM top_level = scm_env_top_level (env);

  const SCM cdr_expr = SCM_CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 2, s_missing_expression, expr);
  bindings = SCM_CAR (cdr_expr);
  check_bindings (bindings, expr);
  transform_bindings (bindings, expr, &rvariables, &inits);

  for (variable_idx = rvariables;
       !scm_is_null (variable_idx);
       variable_idx = SCM_CDR (variable_idx))
    {
      /* The first call to scm_sym2var will look beyond the current module,
       * while the second call wont.  */
      const SCM variable = SCM_CAR (variable_idx);
      SCM new_variable = scm_sym2var (variable, top_level, SCM_BOOL_F);
      if (scm_is_false (new_variable))
	new_variable = scm_sym2var (variable, top_level, SCM_BOOL_T);
      SCM_SETCAR (variable_idx, new_variable);
    }

  SCM_SETCAR (expr, SCM_IM_BIND);
  SCM_SETCAR (cdr_expr, scm_cons (rvariables, inits));
  return expr;
}


SCM_SYNTAX(s_atcall_cc, "@call-with-current-continuation", scm_i_makbimacro, scm_m_cont);
SCM_GLOBAL_SYMBOL(scm_sym_atcall_cc, s_atcall_cc);

SCM 
scm_m_cont (SCM expr, SCM env SCM_UNUSED)
{
  const SCM cdr_expr = SCM_CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) == 1, s_expression, expr);

  SCM_SETCAR (expr, SCM_IM_CONT);
  return expr;
}

static SCM
unmemoize_atcall_cc (const SCM expr, const SCM env)
{
  return scm_list_2 (scm_sym_atcall_cc, unmemoize_exprs (SCM_CDR (expr), env));
}


SCM_SYNTAX (s_at_call_with_values, "@call-with-values", scm_i_makbimacro, scm_m_at_call_with_values);
SCM_GLOBAL_SYMBOL(scm_sym_at_call_with_values, s_at_call_with_values);

SCM
scm_m_at_call_with_values (SCM expr, SCM env SCM_UNUSED)
{
  const SCM cdr_expr = SCM_CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) == 2, s_expression, expr);

  SCM_SETCAR (expr, SCM_IM_CALL_WITH_VALUES);
  return expr;
}

static SCM
unmemoize_at_call_with_values (const SCM expr, const SCM env)
{
  return scm_list_2 (scm_sym_at_call_with_values,
                     unmemoize_exprs (SCM_CDR (expr), env));
}

#if 0

/* See futures.h for a comment why futures are not enabled.
 */

SCM_SYNTAX (s_future, "future", scm_i_makbimacro, scm_m_future);
SCM_GLOBAL_SYMBOL (scm_sym_future, s_future);

/* Like promises, futures are implemented as closures with an empty
 * parameter list.  Thus, (future <expression>) is transformed into
 * (#@future '() <expression>), where the empty list represents the
 * empty parameter list.  This representation allows for easy creation
 * of the closure during evaluation.  */
SCM
scm_m_future (SCM expr, SCM env)
{
  const SCM new_expr = memoize_as_thunk_prototype (expr, env);
  SCM_SETCAR (new_expr, SCM_IM_FUTURE);
  return new_expr;
}

static SCM
unmemoize_future (const SCM expr, const SCM env)
{
  const SCM thunk_expr = SCM_CADDR (expr);
  return scm_list_2 (scm_sym_future, unmemoize_expression (thunk_expr, env));
}

#endif

SCM_SYNTAX (s_gset_x, "set!", scm_i_makbimacro, scm_m_generalized_set_x);
SCM_SYMBOL (scm_sym_setter, "setter");

SCM 
scm_m_generalized_set_x (SCM expr, SCM env)
{
  SCM target, exp_target;

  const SCM cdr_expr = SCM_CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) == 2, s_expression, expr);

  target = SCM_CAR (cdr_expr);
  if (!scm_is_pair (target))
    {
      /* R5RS usage */
      return scm_m_set_x (expr, env);
    }
  else
    {
      /* (set! (foo bar ...) baz) becomes ((setter foo) bar ... baz) */
      /* Macroexpanding the target might return things of the form
	 (begin <atom>).  In that case, <atom> must be a symbol or a
	 variable and we memoize to (set! <atom> ...).
      */
      exp_target = macroexp (target, env);
      if (scm_is_eq (SCM_CAR (exp_target), SCM_IM_BEGIN)
	  && !scm_is_null (SCM_CDR (exp_target))
	  && scm_is_null (SCM_CDDR (exp_target)))
	{
	  exp_target= SCM_CADR (exp_target);
	  ASSERT_SYNTAX_2 (scm_is_symbol (exp_target)
			   || SCM_VARIABLEP (exp_target),
			   s_bad_variable, exp_target, expr);
	  return scm_cons (SCM_IM_SET_X, scm_cons (exp_target,
						   SCM_CDR (cdr_expr)));
	}
      else
	{
	  const SCM setter_proc_tail = scm_list_1 (SCM_CAR (target));
	  const SCM setter_proc = scm_cons_source (expr, scm_sym_setter,
						   setter_proc_tail);

	  const SCM cddr_expr = SCM_CDR (cdr_expr);
	  const SCM setter_args = scm_append_x (scm_list_2 (SCM_CDR (target),
							    cddr_expr));

	  SCM_SETCAR (expr, setter_proc);
	  SCM_SETCDR (expr, setter_args);
	  return expr;
	}
    }
}


/* @slot-ref is bound privately in the (oop goops) module from goops.c.  As
 * soon as the module system allows us to more freely create bindings in
 * arbitrary modules during the startup phase, the code from goops.c should be
 * moved here.  */

SCM_SYMBOL (sym_atslot_ref, "@slot-ref");

SCM
scm_m_atslot_ref (SCM expr, SCM env SCM_UNUSED)
{
  SCM slot_nr;

  const SCM cdr_expr = SCM_CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) == 2, s_expression, expr);
  slot_nr = SCM_CADR (cdr_expr);
  ASSERT_SYNTAX_2 (SCM_I_INUMP (slot_nr), s_bad_slot_number, slot_nr, expr);

  SCM_SETCAR (expr, SCM_IM_SLOT_REF);
  SCM_SETCDR (cdr_expr, slot_nr);
  return expr;
}

static SCM
unmemoize_atslot_ref (const SCM expr, const SCM env)
{
  const SCM instance = SCM_CADR (expr);
  const SCM um_instance = unmemoize_expression (instance, env);
  const SCM slot_nr = SCM_CDDR (expr);
  return scm_list_3 (sym_atslot_ref, um_instance, slot_nr);
}


/* @slot-set! is bound privately in the (oop goops) module from goops.c.  As
 * soon as the module system allows us to more freely create bindings in
 * arbitrary modules during the startup phase, the code from goops.c should be
 * moved here.  */

SCM_SYMBOL (sym_atslot_set_x, "@slot-set!");

SCM
scm_m_atslot_set_x (SCM expr, SCM env SCM_UNUSED)
{
  SCM slot_nr;

  const SCM cdr_expr = SCM_CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) == 3, s_expression, expr);
  slot_nr = SCM_CADR (cdr_expr);
  ASSERT_SYNTAX_2 (SCM_I_INUMP (slot_nr), s_bad_slot_number, slot_nr, expr);

  SCM_SETCAR (expr, SCM_IM_SLOT_SET_X);
  return expr;
}

static SCM
unmemoize_atslot_set_x (const SCM expr, const SCM env)
{
  const SCM cdr_expr = SCM_CDR (expr);
  const SCM instance = SCM_CAR (cdr_expr);
  const SCM um_instance = unmemoize_expression (instance, env);
  const SCM cddr_expr = SCM_CDR (cdr_expr);
  const SCM slot_nr = SCM_CAR (cddr_expr);
  const SCM cdddr_expr = SCM_CDR (cddr_expr);
  const SCM value = SCM_CAR (cdddr_expr);
  const SCM um_value = unmemoize_expression (value, env);
  return scm_list_4 (sym_atslot_set_x, um_instance, slot_nr, um_value);
}


#if SCM_ENABLE_ELISP

static const char s_defun[] = "Symbol's function definition is void";

SCM_SYNTAX (s_nil_cond, "nil-cond", scm_i_makbimacro, scm_m_nil_cond);

/* nil-cond expressions have the form
 *   (nil-cond COND VAL COND VAL ... ELSEVAL)  */
SCM
scm_m_nil_cond (SCM expr, SCM env SCM_UNUSED)
{
  const long length = scm_ilength (SCM_CDR (expr));
  ASSERT_SYNTAX (length >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (length >= 1 && (length % 2) == 1, s_expression, expr);

  SCM_SETCAR (expr, SCM_IM_NIL_COND);
  return expr;
}


SCM_SYNTAX (s_atfop, "@fop", scm_i_makbimacro, scm_m_atfop);

/* The @fop-macro handles procedure and macro applications for elisp.  The
 * input expression must have the form
 *    (@fop <var> (transformer-macro <expr> ...))
 * where <var> must be a symbol.  The expression is transformed into the
 * memoized form of either
 *    (apply <un-aliased var> (transformer-macro <expr> ...))
 * if the value of var (across all aliasing) is not a macro, or
 *    (<un-aliased var> <expr> ...)
 * if var is a macro. */
SCM
scm_m_atfop (SCM expr, SCM env SCM_UNUSED)
{
  SCM location;
  SCM symbol;

  const SCM cdr_expr = SCM_CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 1, s_missing_expression, expr);

  symbol = SCM_CAR (cdr_expr);
  ASSERT_SYNTAX_2 (scm_is_symbol (symbol), s_bad_variable, symbol, expr);

  location = scm_symbol_fref (symbol);
  ASSERT_SYNTAX_2 (SCM_VARIABLEP (location), s_defun, symbol, expr);

  /* The elisp function `defalias' allows to define aliases for symbols.  To
   * look up such definitions, the chain of symbol definitions has to be
   * followed up to the terminal symbol.  */
  while (scm_is_symbol (SCM_VARIABLE_REF (location)))
    {
      const SCM alias = SCM_VARIABLE_REF (location);
      location = scm_symbol_fref (alias);
      ASSERT_SYNTAX_2 (SCM_VARIABLEP (location), s_defun, symbol, expr);
    }

  /* Memoize the value location belonging to the terminal symbol.  */
  SCM_SETCAR (cdr_expr, location);

  if (!SCM_MACROP (SCM_VARIABLE_REF (location)))
    {
      /* Since the location does not contain a macro, the form is a procedure
       * application.  Replace `@fop' by `@apply' and transform the expression
       * including the `transformer-macro'.  */
      SCM_SETCAR (expr, SCM_IM_APPLY);
      return expr;
    }
  else
    {
      /* Since the location contains a macro, the arguments should not be
       * transformed, so the `transformer-macro' is cut out.  The resulting
       * expression starts with the memoized variable, that is at the cdr of
       * the input expression.  */
      SCM_SETCDR (cdr_expr, SCM_CDADR (cdr_expr));
      return cdr_expr;
    }
}

#endif /* SCM_ENABLE_ELISP */


static SCM
unmemoize_builtin_macro (const SCM expr, const SCM env)
{
  switch (ISYMNUM (SCM_CAR (expr)))
    {
    case (ISYMNUM (SCM_IM_AND)):
      return unmemoize_and (expr, env);

    case (ISYMNUM (SCM_IM_BEGIN)):
      return unmemoize_begin (expr, env);

    case (ISYMNUM (SCM_IM_CASE)):
      return unmemoize_case (expr, env);

    case (ISYMNUM (SCM_IM_COND)):
      return unmemoize_cond (expr, env);

    case (ISYMNUM (SCM_IM_DELAY)):
      return unmemoize_delay (expr, env);

    case (ISYMNUM (SCM_IM_DO)):
      return unmemoize_do (expr, env);

    case (ISYMNUM (SCM_IM_IF)):
      return unmemoize_if (expr, env);

    case (ISYMNUM (SCM_IM_LAMBDA)):
      return unmemoize_lambda (expr, env);

    case (ISYMNUM (SCM_IM_LET)):
      return unmemoize_let (expr, env);

    case (ISYMNUM (SCM_IM_LETREC)):
      return unmemoize_letrec (expr, env);

    case (ISYMNUM (SCM_IM_LETSTAR)):
      return unmemoize_letstar (expr, env);

    case (ISYMNUM (SCM_IM_OR)):
      return unmemoize_or (expr, env);

    case (ISYMNUM (SCM_IM_QUOTE)):
      return unmemoize_quote (expr, env);

    case (ISYMNUM (SCM_IM_SET_X)):
      return unmemoize_set_x (expr, env);

    case (ISYMNUM (SCM_IM_APPLY)):
      return unmemoize_apply (expr, env);

    case (ISYMNUM (SCM_IM_BIND)):
      return unmemoize_exprs (expr, env);  /* FIXME */

    case (ISYMNUM (SCM_IM_CONT)):
      return unmemoize_atcall_cc (expr, env);

    case (ISYMNUM (SCM_IM_CALL_WITH_VALUES)):
      return unmemoize_at_call_with_values (expr, env);

#if 0
    /* See futures.h for a comment why futures are not enabled.
     */
    case (ISYMNUM (SCM_IM_FUTURE)):
      return unmemoize_future (expr, env);
#endif

    case (ISYMNUM (SCM_IM_SLOT_REF)):
      return unmemoize_atslot_ref (expr, env);

    case (ISYMNUM (SCM_IM_SLOT_SET_X)):
      return unmemoize_atslot_set_x (expr, env);

    case (ISYMNUM (SCM_IM_NIL_COND)):
      return unmemoize_exprs (expr, env);  /* FIXME */

    default:
      return unmemoize_exprs (expr, env);  /* FIXME */
    }
}


/* scm_i_unmemocopy_expr and scm_i_unmemocopy_body take a memoized expression
 * respectively a memoized body together with its environment and rewrite it
 * to its original form.  Thus, these functions are the inversion of the
 * rewrite rules above.  The procedure is not optimized for speed.  It's used
 * in scm_i_unmemoize_expr, scm_procedure_source, macro_print and scm_iprin1.
 *
 * Unmemoizing is not a reliable process.  You cannot in general expect to get
 * the original source back.
 *
 * However, GOOPS currently relies on this for method compilation.  This ought
 * to change.  */

SCM
scm_i_unmemocopy_expr (SCM expr, SCM env)
{
  const SCM source_properties = scm_whash_lookup (scm_source_whash, expr);
  const SCM um_expr = unmemoize_expression (expr, env);

  if (scm_is_true (source_properties))
    scm_whash_insert (scm_source_whash, um_expr, source_properties);

  return um_expr;
}

SCM
scm_i_unmemocopy_body (SCM forms, SCM env)
{
  const SCM source_properties = scm_whash_lookup (scm_source_whash, forms);
  const SCM um_forms = unmemoize_exprs (forms, env);

  if (scm_is_true (source_properties))
    scm_whash_insert (scm_source_whash, um_forms, source_properties);

  return um_forms;
}


#if (SCM_ENABLE_DEPRECATED == 1)

/* Deprecated in guile 1.7.0 on 2003-11-09.  */
SCM
scm_m_expand_body (SCM exprs, SCM env)
{
  scm_c_issue_deprecation_warning 
    ("`scm_m_expand_body' is deprecated.");
  m_expand_body (exprs, env);
  return exprs;
}


SCM_SYNTAX (s_undefine, "undefine", scm_makacro, scm_m_undefine);

SCM
scm_m_undefine (SCM expr, SCM env)
{
  SCM variable;
  SCM location;

  const SCM cdr_expr = SCM_CDR (expr);
  ASSERT_SYNTAX (SCM_TOP_LEVEL (env), "Bad undefine placement in", expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) == 1, s_expression, expr);

  scm_c_issue_deprecation_warning
    ("`undefine' is deprecated.\n");

  variable = SCM_CAR (cdr_expr);
  ASSERT_SYNTAX_2 (scm_is_symbol (variable), s_bad_variable, variable, expr);
  location = scm_sym2var (variable, scm_env_top_level (env), SCM_BOOL_F);
  ASSERT_SYNTAX_2 (scm_is_true (location)
                   && !SCM_UNBNDP (SCM_VARIABLE_REF (location)),
                   "variable already unbound ", variable, expr);
  SCM_VARIABLE_SET (location, SCM_UNDEFINED);
  return SCM_UNSPECIFIED;
}

SCM
scm_macroexp (SCM x, SCM env)
{
  scm_c_issue_deprecation_warning
    ("`scm_macroexp' is deprecated.");
  return macroexp (x, env);
}

#endif


#if (SCM_ENABLE_DEPRECATED == 1)

SCM
scm_unmemocar (SCM form, SCM env)
{
  scm_c_issue_deprecation_warning 
    ("`scm_unmemocar' is deprecated.");

  if (!scm_is_pair (form))
    return form;
  else
    {
      SCM c = SCM_CAR (form);
      if (SCM_VARIABLEP (c))
	{
	  SCM sym = scm_module_reverse_lookup (scm_env_module (env), c);
	  if (scm_is_false (sym))
	    sym = sym_three_question_marks;
	  SCM_SETCAR (form, sym);
	}
      else if (SCM_ILOCP (c))
	{
	  unsigned long int ir;

	  for (ir = SCM_IFRAME (c); ir != 0; --ir)
	    env = SCM_CDR (env);
	  env = SCM_CAAR (env);
	  for (ir = SCM_IDIST (c); ir != 0; --ir)
	    env = SCM_CDR (env);

	  SCM_SETCAR (form, SCM_ICDRP (c) ? env : SCM_CAR (env));
	}
      return form;
    }
}

#endif

/*****************************************************************************/
/*****************************************************************************/
/*                 The definitions for execution start here.                 */
/*****************************************************************************/
/*****************************************************************************/

SCM_GLOBAL_SYMBOL (scm_sym_enter_frame, "enter-frame");
SCM_GLOBAL_SYMBOL (scm_sym_apply_frame, "apply-frame");
SCM_GLOBAL_SYMBOL (scm_sym_exit_frame, "exit-frame");
SCM_GLOBAL_SYMBOL (scm_sym_trace, "trace");
SCM_SYMBOL (sym_instead, "instead");

/* A function object to implement "apply" for non-closure functions.  */
static SCM f_apply;
/* An endless list consisting of #<undefined> objects:  */
static SCM undefineds;


int
scm_badargsp (SCM formals, SCM args)
{
  while (!scm_is_null (formals))
    {
      if (!scm_is_pair (formals)) 
        return 0;
      if (scm_is_null (args)) 
        return 1;
      formals = SCM_CDR (formals);
      args = SCM_CDR (args);
    }
  return !scm_is_null (args) ? 1 : 0;
}



/* The evaluator contains a plethora of EVAL symbols.  This is an attempt at
 * explanation.
 *
 * The following macros should be used in code which is read twice (where the
 * choice of evaluator is hard soldered):
 *
 *   CEVAL is the symbol used within one evaluator to call itself.
 *   Originally, it is defined to ceval, but is redefined to deval during the
 *   second pass.
 *  
 *   SCM_I_EVALIM is used when it is known that the expression is an
 *   immediate.  (This macro never calls an evaluator.)
 *
 *   EVAL evaluates an expression that is expected to have its symbols already
 *   memoized.  Expressions that are not of the form '(<form> <form> ...)' are
 *   evaluated inline without calling an evaluator.
 *
 *   EVALCAR evaluates the car of an expression 'X:(Y:<form> <form> ...)',
 *   potentially replacing a symbol at the position Y:<form> by its memoized
 *   variable.  If Y:<form> is not of the form '(<form> <form> ...)', the
 *   evaluation is performed inline without calling an evaluator.
 *  
 * The following macros should be used in code which is read once
 * (where the choice of evaluator is dynamic):
 *
 *   SCM_I_XEVAL corresponds to EVAL, but uses ceval *or* deval depending on the
 *   debugging mode.
 *  
 *   SCM_I_XEVALCAR corresponds to EVALCAR, but uses ceval *or* deval depending
 *   on the debugging mode.
 *
 * The main motivation for keeping this plethora is efficiency
 * together with maintainability (=> locality of code).
 */

static SCM ceval (SCM x, SCM env);
static SCM deval (SCM x, SCM env);
#define CEVAL ceval


#define SCM_I_EVALIM2(x) \
  ((scm_is_eq ((x), SCM_EOL) \
    ? syntax_error (s_empty_combination, (x), SCM_UNDEFINED), 0 \
    : 0), \
   (x))

#define SCM_I_EVALIM(x, env) (SCM_ILOCP (x) \
                            ? *scm_ilookup ((x), (env)) \
			    : SCM_I_EVALIM2(x))

#define SCM_I_XEVAL(x, env) \
  (SCM_IMP (x) \
   ? SCM_I_EVALIM2 (x) \
   : (SCM_VARIABLEP (x) \
      ? SCM_VARIABLE_REF (x) \
      : (scm_is_pair (x) \
         ? (scm_debug_mode_p \
            ? deval ((x), (env)) \
            : ceval ((x), (env))) \
         : (x))))

#define SCM_I_XEVALCAR(x, env) \
  (SCM_IMP (SCM_CAR (x)) \
   ? SCM_I_EVALIM (SCM_CAR (x), (env)) \
   : (SCM_VARIABLEP (SCM_CAR (x)) \
      ? SCM_VARIABLE_REF (SCM_CAR (x)) \
      : (scm_is_pair (SCM_CAR (x)) \
         ? (scm_debug_mode_p \
            ? deval (SCM_CAR (x), (env)) \
            : ceval (SCM_CAR (x), (env))) \
         : (!scm_is_symbol (SCM_CAR (x)) \
            ? SCM_CAR (x) \
            : *scm_lookupcar ((x), (env), 1)))))

#define EVAL(x, env) \
  (SCM_IMP (x) \
   ? SCM_I_EVALIM ((x), (env)) \
   : (SCM_VARIABLEP (x) \
      ? SCM_VARIABLE_REF (x) \
      : (scm_is_pair (x) \
         ? CEVAL ((x), (env)) \
         : (x))))

#define EVALCAR(x, env) \
  (SCM_IMP (SCM_CAR (x)) \
   ? SCM_I_EVALIM (SCM_CAR (x), (env)) \
   : (SCM_VARIABLEP (SCM_CAR (x)) \
      ? SCM_VARIABLE_REF (SCM_CAR (x)) \
      : (scm_is_pair (SCM_CAR (x)) \
         ? CEVAL (SCM_CAR (x), (env)) \
         : (!scm_is_symbol (SCM_CAR (x)) \
            ? SCM_CAR (x) \
            :  *scm_lookupcar ((x), (env), 1)))))

scm_i_pthread_mutex_t source_mutex;


/* Lookup a given local variable in an environment.  The local variable is
 * given as an iloc, that is a triple <frame, binding, last?>, where frame
 * indicates the relative number of the environment frame (counting upwards
 * from the innermost environment frame), binding indicates the number of the
 * binding within the frame, and last? (which is extracted from the iloc using
 * the macro SCM_ICDRP) indicates whether the binding forms the binding at the
 * very end of the improper list of bindings.  */
SCM *
scm_ilookup (SCM iloc, SCM env)
{
  unsigned int frame_nr = SCM_IFRAME (iloc);
  unsigned int binding_nr = SCM_IDIST (iloc);
  SCM frames = env;
  SCM bindings;
 
  for (; 0 != frame_nr; --frame_nr)
    frames = SCM_CDR (frames);

  bindings = SCM_CAR (frames);
  for (; 0 != binding_nr; --binding_nr)
    bindings = SCM_CDR (bindings);

  if (SCM_ICDRP (iloc))
    return SCM_CDRLOC (bindings);
  return SCM_CARLOC (SCM_CDR (bindings));
}


SCM_SYMBOL (scm_unbound_variable_key, "unbound-variable");

static void error_unbound_variable (SCM symbol) SCM_NORETURN;
static void error_defined_variable (SCM symbol) SCM_NORETURN;

/* Call this for variables that are unfound.
 */
static void
error_unbound_variable (SCM symbol)
{
  scm_error (scm_unbound_variable_key, NULL,
	     "Unbound variable: ~S",
	     scm_list_1 (symbol), SCM_BOOL_F);
}

/* Call this for variables that are found but contain SCM_UNDEFINED.
 */
static void
error_defined_variable (SCM symbol)
{
  /* We use the 'unbound-variable' key here as well, since it
     basically is the same kind of error, with a slight variation in
     the displayed message.
  */
  scm_error (scm_unbound_variable_key, NULL,
	     "Variable used before given a value: ~S",
	     scm_list_1 (symbol), SCM_BOOL_F);
}


/* The Lookup Car Race
    - by Eva Luator

   Memoization of variables and special forms is done while executing
   the code for the first time.  As long as there is only one thread
   everything is fine, but as soon as two threads execute the same
   code concurrently `for the first time' they can come into conflict.

   This memoization includes rewriting variable references into more
   efficient forms and expanding macros.  Furthermore, macro expansion
   includes `compiling' special forms like `let', `cond', etc. into
   tree-code instructions.

   There shouldn't normally be a problem with memoizing local and
   global variable references (into ilocs and variables), because all
   threads will mutate the code in *exactly* the same way and (if I
   read the C code correctly) it is not possible to observe a half-way
   mutated cons cell.  The lookup procedure can handle this
   transparently without any critical sections.

   It is different with macro expansion, because macro expansion
   happens outside of the lookup procedure and can't be
   undone. Therefore the lookup procedure can't cope with it.  It has
   to indicate failure when it detects a lost race and hope that the
   caller can handle it.  Luckily, it turns out that this is the case.

   An example to illustrate this: Suppose that the following form will
   be memoized concurrently by two threads

       (let ((x 12)) x)

   Let's first examine the lookup of X in the body.  The first thread
   decides that it has to find the symbol "x" in the environment and
   starts to scan it.  Then the other thread takes over and actually
   overtakes the first.  It looks up "x" and substitutes an
   appropriate iloc for it.  Now the first thread continues and
   completes its lookup.  It comes to exactly the same conclusions as
   the second one and could - without much ado - just overwrite the
   iloc with the same iloc.

   But let's see what will happen when the race occurs while looking
   up the symbol "let" at the start of the form.  It could happen that
   the second thread interrupts the lookup of the first thread and not
   only substitutes a variable for it but goes right ahead and
   replaces it with the compiled form (#@let* (x 12) x).  Now, when
   the first thread completes its lookup, it would replace the #@let*
   with a variable containing the "let" binding, effectively reverting
   the form to (let (x 12) x).  This is wrong.  It has to detect that
   it has lost the race and the evaluator has to reconsider the
   changed form completely.

   This race condition could be resolved with some kind of traffic
   light (like mutexes) around scm_lookupcar, but I think that it is
   best to avoid them in this case.  They would serialize memoization
   completely and because lookup involves calling arbitrary Scheme
   code (via the lookup-thunk), threads could be blocked for an
   arbitrary amount of time or even deadlock.  But with the current
   solution a lot of unnecessary work is potentially done. */

/* SCM_LOOKUPCAR1 is what SCM_LOOKUPCAR used to be but is allowed to
   return NULL to indicate a failed lookup due to some race conditions
   between threads.  This only happens when VLOC is the first cell of
   a special form that will eventually be memoized (like `let', etc.)
   In that case the whole lookup is bogus and the caller has to
   reconsider the complete special form.

   SCM_LOOKUPCAR is still there, of course.  It just calls
   SCM_LOOKUPCAR1 and aborts on receiving NULL.  So SCM_LOOKUPCAR
   should only be called when it is known that VLOC is not the first
   pair of a special form.  Otherwise, use SCM_LOOKUPCAR1 and check
   for NULL.  I think I've found the only places where this
   applies. */

static SCM *
scm_lookupcar1 (SCM vloc, SCM genv, int check)
{
  SCM env = genv;
  register SCM *al, fl, var = SCM_CAR (vloc);
  register SCM iloc = SCM_ILOC00;
  for (; SCM_NIMP (env); env = SCM_CDR (env))
    {
      if (!scm_is_pair (SCM_CAR (env)))
	break;
      al = SCM_CARLOC (env);
      for (fl = SCM_CAR (*al); SCM_NIMP (fl); fl = SCM_CDR (fl))
	{
	  if (!scm_is_pair (fl))
	    {
	      if (scm_is_eq (fl, var))
	      {
		if (!scm_is_eq (SCM_CAR (vloc), var))
		  goto race;
		SCM_SET_CELL_WORD_0 (vloc, SCM_UNPACK (iloc) + SCM_ICDR);
		return SCM_CDRLOC (*al);
	      }
	      else
		break;
	    }
	  al = SCM_CDRLOC (*al);
	  if (scm_is_eq (SCM_CAR (fl), var))
	    {
	      if (SCM_UNBNDP (SCM_CAR (*al)))
		error_defined_variable (var);
	      if (!scm_is_eq (SCM_CAR (vloc), var))
		goto race;
	      SCM_SETCAR (vloc, iloc);
	      return SCM_CARLOC (*al);
	    }
	  iloc = SCM_PACK (SCM_UNPACK (iloc) + SCM_IDINC);
	}
      iloc = SCM_PACK ((~SCM_IDSTMSK) & (SCM_UNPACK(iloc) + SCM_IFRINC));
    }
  {
    SCM top_thunk, real_var;
    if (SCM_NIMP (env))
      {
	top_thunk = SCM_CAR (env); /* env now refers to a
				      top level env thunk */
	env = SCM_CDR (env);
      }
    else
      top_thunk = SCM_BOOL_F;
    real_var = scm_sym2var (var, top_thunk, SCM_BOOL_F);
    if (scm_is_false (real_var))
      goto errout;

    if (!scm_is_null (env) || SCM_UNBNDP (SCM_VARIABLE_REF (real_var)))
      {
      errout:
	if (check)
	  {
	    if (scm_is_null (env))
              error_unbound_variable (var);
	    else
	      scm_misc_error (NULL, "Damaged environment: ~S",
			      scm_list_1 (var));
	  }
	else 
	  {
	    /* A variable could not be found, but we shall
	       not throw an error. */
	    static SCM undef_object = SCM_UNDEFINED;
	    return &undef_object;
	  }
      }

    if (!scm_is_eq (SCM_CAR (vloc), var))
      {
	/* Some other thread has changed the very cell we are working
	   on.  In effect, it must have done our job or messed it up
	   completely. */
      race:
	var = SCM_CAR (vloc);
	if (SCM_VARIABLEP (var))
	  return SCM_VARIABLE_LOC (var);
	if (SCM_ILOCP (var))
	  return scm_ilookup (var, genv);
	/* We can't cope with anything else than variables and ilocs.  When
	   a special form has been memoized (i.e. `let' into `#@let') we
	   return NULL and expect the calling function to do the right
	   thing.  For the evaluator, this means going back and redoing
	   the dispatch on the car of the form. */
	return NULL;
      }

    SCM_SETCAR (vloc, real_var);
    return SCM_VARIABLE_LOC (real_var);
  }
}

SCM *
scm_lookupcar (SCM vloc, SCM genv, int check)
{
  SCM *loc = scm_lookupcar1 (vloc, genv, check);
  if (loc == NULL)
    abort ();
  return loc;
}


/* During execution, look up a symbol in the top level of the given local
 * environment and return the corresponding variable object.  If no binding
 * for the symbol can be found, an 'Unbound variable' error is signalled.  */
static SCM
lazy_memoize_variable (const SCM symbol, const SCM environment)
{
  const SCM top_level = scm_env_top_level (environment);
  const SCM variable = scm_sym2var (symbol, top_level, SCM_BOOL_F);

  if (scm_is_false (variable))
    error_unbound_variable (symbol);
  else
    return variable;
}


SCM
scm_eval_car (SCM pair, SCM env)
{
  return SCM_I_XEVALCAR (pair, env);
}


SCM 
scm_eval_args (SCM l, SCM env, SCM proc)
{
  SCM results = SCM_EOL, *lloc = &results, res;
  while (scm_is_pair (l))
    {
      res = EVALCAR (l, env);

      *lloc = scm_list_1 (res);
      lloc = SCM_CDRLOC (*lloc);
      l = SCM_CDR (l);
    }
  if (!scm_is_null (l))
    scm_wrong_num_args (proc);
  return results;
}


SCM
scm_eval_body (SCM code, SCM env)
{
  SCM next;

 again:
  next = SCM_CDR (code);
  while (!scm_is_null (next))
    {
      if (SCM_IMP (SCM_CAR (code)))
	{
	  if (SCM_ISYMP (SCM_CAR (code)))
	    {
	      scm_dynwind_begin (0);
	      scm_i_dynwind_pthread_mutex_lock (&source_mutex);
	      /* check for race condition */
	      if (SCM_ISYMP (SCM_CAR (code)))
		m_expand_body (code, env);
	      scm_dynwind_end ();
	      goto again;
	    }
	}
      else
	SCM_I_XEVAL (SCM_CAR (code), env);
      code = next;
      next = SCM_CDR (code);
    }
  return SCM_I_XEVALCAR (code, env);
}

#endif /* !DEVAL */


/* SECTION: This code is specific for the debugging support.  One
 * branch is read when DEVAL isn't defined, the other when DEVAL is
 * defined.
 */

#ifndef DEVAL

#define SCM_APPLY scm_apply
#define PREP_APPLY(proc, args)
#define ENTER_APPLY
#define RETURN(x) do { return x; } while (0)
#ifdef STACK_CHECKING
#ifndef NO_CEVAL_STACK_CHECKING
#define EVAL_STACK_CHECKING
#endif
#endif

#else /* !DEVAL */

#undef CEVAL
#define CEVAL deval	/* Substitute all uses of ceval */

#undef SCM_APPLY
#define SCM_APPLY scm_dapply

#undef PREP_APPLY
#define PREP_APPLY(p, l) \
{ ++debug.info; debug.info->a.proc = p; debug.info->a.args = l; }

#undef ENTER_APPLY
#define ENTER_APPLY \
do { \
  SCM_SET_ARGSREADY (debug);\
  if (scm_check_apply_p && SCM_TRAPS_P)\
    if (SCM_APPLY_FRAME_P || (SCM_TRACE_P && SCM_PROCTRACEP (proc)))\
      {\
	SCM tmp, tail = scm_from_bool(SCM_TRACED_FRAME_P (debug)); \
	SCM_SET_TRACED_FRAME (debug); \
	SCM_TRAPS_P = 0;\
        tmp = scm_make_debugobj (&debug);\
	scm_call_3 (SCM_APPLY_FRAME_HDLR, scm_sym_apply_frame, tmp, tail);\
	SCM_TRAPS_P = 1;\
      }\
} while (0)

#undef RETURN
#define RETURN(e) do { proc = (e); goto exit; } while (0)

#ifdef STACK_CHECKING
#ifndef EVAL_STACK_CHECKING
#define EVAL_STACK_CHECKING
#endif
#endif


/* scm_last_debug_frame contains a pointer to the last debugging information
 * stack frame.  It is accessed very often from the debugging evaluator, so it
 * should probably not be indirectly addressed.  Better to save and restore it
 * from the current root at any stack swaps.
 */

/* scm_debug_eframe_size is the number of slots available for pseudo
 * stack frames at each real stack frame.
 */

long scm_debug_eframe_size;

int scm_debug_mode_p;
int scm_check_entry_p;
int scm_check_apply_p;
int scm_check_exit_p;

long scm_eval_stack;

scm_t_option scm_eval_opts[] = {
  { SCM_OPTION_INTEGER, "stack", 22000, "Size of thread stacks (in machine words)." }
};

scm_t_option scm_debug_opts[] = {
  { SCM_OPTION_BOOLEAN, "cheap", 1,
    "*This option is now obsolete.  Setting it has no effect." },
  { SCM_OPTION_BOOLEAN, "breakpoints", 0, "*Check for breakpoints." },
  { SCM_OPTION_BOOLEAN, "trace", 0, "*Trace mode." },
  { SCM_OPTION_BOOLEAN, "procnames", 1,
    "Record procedure names at definition." },
  { SCM_OPTION_BOOLEAN, "backwards", 0,
    "Display backtrace in anti-chronological order." },
  { SCM_OPTION_INTEGER, "width", 79, "Maximal width of backtrace." },
  { SCM_OPTION_INTEGER, "indent", 10, "Maximal indentation in backtrace." },
  { SCM_OPTION_INTEGER, "frames", 3,
    "Maximum number of tail-recursive frames in backtrace." },
  { SCM_OPTION_INTEGER, "maxdepth", 1000,
    "Maximal number of stored backtrace frames." },
  { SCM_OPTION_INTEGER, "depth", 20, "Maximal length of printed backtrace." },
  { SCM_OPTION_BOOLEAN, "backtrace", 0, "Show backtrace on error." },
  { SCM_OPTION_BOOLEAN, "debug", 0, "Use the debugging evaluator." },
  { SCM_OPTION_INTEGER, "stack", 20000, "Stack size limit (measured in words; 0 = no check)." },
  { SCM_OPTION_SCM, "show-file-name", (unsigned long)SCM_BOOL_T, "Show file names and line numbers in backtraces when not `#f'.  A value of `base' displays only base names, while `#t' displays full names."},
  { SCM_OPTION_BOOLEAN, "warn-deprecated", 0, "Warn when deprecated features are used." }
};

scm_t_option scm_evaluator_trap_table[] = {
  { SCM_OPTION_BOOLEAN, "traps", 0, "Enable evaluator traps." },
  { SCM_OPTION_BOOLEAN, "enter-frame", 0, "Trap when eval enters new frame." },
  { SCM_OPTION_BOOLEAN, "apply-frame", 0, "Trap when entering apply." },
  { SCM_OPTION_BOOLEAN, "exit-frame", 0, "Trap when exiting eval or apply." },
  { SCM_OPTION_SCM, "enter-frame-handler", (unsigned long)SCM_BOOL_F, "Handler for enter-frame traps." },
  { SCM_OPTION_SCM, "apply-frame-handler", (unsigned long)SCM_BOOL_F, "Handler for apply-frame traps." },
  { SCM_OPTION_SCM, "exit-frame-handler", (unsigned long)SCM_BOOL_F, "Handler for exit-frame traps." }
};

SCM_DEFINE (scm_eval_options_interface, "eval-options-interface", 0, 1, 0, 
            (SCM setting),
	    "Option interface for the evaluation options. Instead of using\n"
	    "this procedure directly, use the procedures @code{eval-enable},\n"
	    "@code{eval-disable}, @code{eval-set!} and @code{eval-options}.")
#define FUNC_NAME s_scm_eval_options_interface
{
  SCM ans;
  
  scm_dynwind_begin (0);
  scm_dynwind_critical_section (SCM_BOOL_F);
  ans = scm_options (setting,
		     scm_eval_opts,
		     SCM_N_EVAL_OPTIONS,
		     FUNC_NAME);
  scm_eval_stack = SCM_EVAL_STACK * sizeof (void *);
  scm_dynwind_end ();

  return ans;
}
#undef FUNC_NAME


SCM_DEFINE (scm_evaluator_traps, "evaluator-traps-interface", 0, 1, 0, 
            (SCM setting),
	    "Option interface for the evaluator trap options.")
#define FUNC_NAME s_scm_evaluator_traps
{
  SCM ans;
  SCM_CRITICAL_SECTION_START;
  ans = scm_options (setting,
		     scm_evaluator_trap_table,
		     SCM_N_EVALUATOR_TRAPS,
		     FUNC_NAME);
  /* njrev: same again. */
  SCM_RESET_DEBUG_MODE;
  SCM_CRITICAL_SECTION_END;
  return ans;
}
#undef FUNC_NAME


static SCM
deval_args (SCM l, SCM env, SCM proc, SCM *lloc)
{
  SCM *results = lloc;
  while (scm_is_pair (l))
    {
      const SCM res = EVALCAR (l, env);

      *lloc = scm_list_1 (res);
      lloc = SCM_CDRLOC (*lloc);
      l = SCM_CDR (l);
    }
  if (!scm_is_null (l))
    scm_wrong_num_args (proc);
  return *results;
}

static void
eval_letrec_inits (SCM env, SCM init_forms, SCM **init_values_eol)
{
  SCM argv[10];
  int i = 0, imax = sizeof (argv) / sizeof (SCM);

  while (!scm_is_null (init_forms))
    {
      if (imax == i)
	{
	  eval_letrec_inits (env, init_forms, init_values_eol);
	  break;
	}
      argv[i++] = EVALCAR (init_forms, env);
      init_forms = SCM_CDR (init_forms);
    }

  for (i--; i >= 0; i--)
    {
      **init_values_eol = scm_list_1 (argv[i]);
      *init_values_eol = SCM_CDRLOC (**init_values_eol);
    }
}

#endif /* !DEVAL */


/* SECTION: This code is compiled twice.
 */


/* Update the toplevel environment frame ENV so that it refers to the
 * current module.  */
#define UPDATE_TOPLEVEL_ENV(env) \
  do { \
    SCM p = scm_current_module_lookup_closure (); \
    if (p != SCM_CAR (env)) \
      env = scm_top_level_env (p); \
  } while (0)


#define SCM_VALIDATE_NON_EMPTY_COMBINATION(x) \
  ASSERT_SYNTAX (!scm_is_eq ((x), SCM_EOL), s_empty_combination, x)


/* This is the evaluator.  Like any real monster, it has three heads:
 *
 * ceval is the non-debugging evaluator, deval is the debugging version.  Both
 * are implemented using a common code base, using the following mechanism:
 * CEVAL is a macro, which is either defined to ceval or deval.  Thus, there
 * is no function CEVAL, but the code for CEVAL actually compiles to either
 * ceval or deval.  When CEVAL is defined to ceval, it is known that the macro
 * DEVAL is not defined.  When CEVAL is defined to deval, then the macro DEVAL
 * is known to be defined.  Thus, in CEVAL parts for the debugging evaluator
 * are enclosed within #ifdef DEVAL ... #endif.
 *
 * All three (ceval, deval and their common implementation CEVAL) take two
 * input parameters, x and env: x is a single expression to be evalutated.
 * env is the environment in which bindings are searched.
 *
 * x is known to be a pair.  Since x is a single expression, it is necessarily
 * in a tail position.  If x is just a call to another function like in the
 * expression (foo exp1 exp2 ...), the realization of that call therefore
 * _must_not_ increase stack usage (the evaluation of exp1, exp2 etc.,
 * however, may do so).  This is realized by making extensive use of 'goto'
 * statements within the evaluator: The gotos replace recursive calls to
 * CEVAL, thus re-using the same stack frame that CEVAL was already using.
 * If, however, x represents some form that requires to evaluate a sequence of
 * expressions like (begin exp1 exp2 ...), then recursive calls to CEVAL are
 * performed for all but the last expression of that sequence.  */

static SCM
CEVAL (SCM x, SCM env)
{
  SCM proc, arg1;
#ifdef DEVAL
  scm_t_debug_frame debug;
  scm_t_debug_info *debug_info_end;
  debug.prev = scm_i_last_debug_frame ();
  debug.status = 0;
  /*
   * The debug.vect contains twice as much scm_t_debug_info frames as the
   * user has specified with (debug-set! frames <n>).
   *
   * Even frames are eval frames, odd frames are apply frames.
   */
  debug.vect = (scm_t_debug_info *) alloca (scm_debug_eframe_size
					    * sizeof (scm_t_debug_info));
  debug.info = debug.vect;
  debug_info_end = debug.vect + scm_debug_eframe_size;
  scm_i_set_last_debug_frame (&debug);
#endif
#ifdef EVAL_STACK_CHECKING
  if (scm_stack_checking_enabled_p && SCM_STACK_OVERFLOW_P (&proc))
    {
#ifdef DEVAL
      debug.info->e.exp = x;
      debug.info->e.env = env;
#endif
      scm_report_stack_overflow ();
    }
#endif

#ifdef DEVAL
  goto start;
#endif

loop:
#ifdef DEVAL
  SCM_CLEAR_ARGSREADY (debug);
  if (SCM_OVERFLOWP (debug))
    --debug.info;
  /*
   * In theory, this should be the only place where it is necessary to
   * check for space in debug.vect since both eval frames and
   * available space are even.
   *
   * For this to be the case, however, it is necessary that primitive
   * special forms which jump back to `loop', `begin' or some similar
   * label call PREP_APPLY.
   */
  else if (++debug.info >= debug_info_end)
    {
      SCM_SET_OVERFLOW (debug);
      debug.info -= 2;
    }

start:
  debug.info->e.exp = x;
  debug.info->e.env = env;
  if (scm_check_entry_p && SCM_TRAPS_P)
    {
      if (SCM_ENTER_FRAME_P
	  || (SCM_BREAKPOINTS_P && scm_c_source_property_breakpoint_p (x)))
	{
	  SCM stackrep;
	  SCM tail = scm_from_bool (SCM_TAILRECP (debug));
	  SCM_SET_TAILREC (debug);
	  stackrep = scm_make_debugobj (&debug);
	  SCM_TRAPS_P = 0;
	  stackrep = scm_call_4 (SCM_ENTER_FRAME_HDLR,
				 scm_sym_enter_frame,
				 stackrep,
				 tail,
				 unmemoize_expression (x, env));
	  SCM_TRAPS_P = 1;
	  if (scm_is_pair (stackrep) &&
	      scm_is_eq (SCM_CAR (stackrep), sym_instead))
	    {
	      /* This gives the possibility for the debugger to modify
		 the source expression before evaluation. */
	      x = SCM_CDR (stackrep);
	      if (SCM_IMP (x))
		RETURN (x);
	    }
	}
    }
#endif
dispatch:
  SCM_TICK;
  if (SCM_ISYMP (SCM_CAR (x)))
    {
      switch (ISYMNUM (SCM_CAR (x)))
        {
        case (ISYMNUM (SCM_IM_AND)):
          x = SCM_CDR (x);
          while (!scm_is_null (SCM_CDR (x)))
            {
              SCM test_result = EVALCAR (x, env);
              if (scm_is_false (test_result) || SCM_NILP (test_result))
                RETURN (SCM_BOOL_F);
              else
                x = SCM_CDR (x);
            }
          PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
          goto carloop;

        case (ISYMNUM (SCM_IM_BEGIN)):
          x = SCM_CDR (x);
          if (scm_is_null (x))
            RETURN (SCM_UNSPECIFIED);

          PREP_APPLY (SCM_UNDEFINED, SCM_EOL);

        begin:
          /* If we are on toplevel with a lookup closure, we need to sync
             with the current module. */
          if (scm_is_pair (env) && !scm_is_pair (SCM_CAR (env)))
            {
              UPDATE_TOPLEVEL_ENV (env);
              while (!scm_is_null (SCM_CDR (x)))
                {
                  EVALCAR (x, env);
                  UPDATE_TOPLEVEL_ENV (env);
                  x = SCM_CDR (x);
                }
              goto carloop;
            }
          else
            goto nontoplevel_begin;

        nontoplevel_begin:
          while (!scm_is_null (SCM_CDR (x)))
            {
              const SCM form = SCM_CAR (x);
              if (SCM_IMP (form))
                {
                  if (SCM_ISYMP (form))
                    {
		      scm_dynwind_begin (0);
		      scm_i_dynwind_pthread_mutex_lock (&source_mutex);
                      /* check for race condition */
                      if (SCM_ISYMP (SCM_CAR (x)))
                        m_expand_body (x, env);
		      scm_dynwind_end ();
                      goto nontoplevel_begin;
                    }
                  else
                    SCM_VALIDATE_NON_EMPTY_COMBINATION (form);
                }
              else
                (void) EVAL (form, env);
              x = SCM_CDR (x);
            }

        carloop:
          {
            /* scm_eval last form in list */
            const SCM last_form = SCM_CAR (x);

            if (scm_is_pair (last_form))
              {
                /* This is by far the most frequent case. */
                x = last_form;
                goto loop;		/* tail recurse */
              }
            else if (SCM_IMP (last_form))
              RETURN (SCM_I_EVALIM (last_form, env));
            else if (SCM_VARIABLEP (last_form))
              RETURN (SCM_VARIABLE_REF (last_form));
            else if (scm_is_symbol (last_form))
              RETURN (*scm_lookupcar (x, env, 1));
            else
              RETURN (last_form);
          }


        case (ISYMNUM (SCM_IM_CASE)):
          x = SCM_CDR (x);
          {
            const SCM key = EVALCAR (x, env);
            x = SCM_CDR (x);
            while (!scm_is_null (x))
              {
                const SCM clause = SCM_CAR (x);
                SCM labels = SCM_CAR (clause);
                if (scm_is_eq (labels, SCM_IM_ELSE))
                  {
                    x = SCM_CDR (clause);
                    PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
                    goto begin;
                  }
                while (!scm_is_null (labels))
                  {
                    const SCM label = SCM_CAR (labels);
                    if (scm_is_eq (label, key)
                        || scm_is_true (scm_eqv_p (label, key)))
                      {
                        x = SCM_CDR (clause);
                        PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
                        goto begin;
                      }
                    labels = SCM_CDR (labels);
                  }
                x = SCM_CDR (x);
              }
          }
          RETURN (SCM_UNSPECIFIED);


        case (ISYMNUM (SCM_IM_COND)):
          x = SCM_CDR (x);
          while (!scm_is_null (x))
            {
              const SCM clause = SCM_CAR (x);
              if (scm_is_eq (SCM_CAR (clause), SCM_IM_ELSE))
                {
                  x = SCM_CDR (clause);
                  PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
                  goto begin;
                }
              else
                {
                  arg1 = EVALCAR (clause, env);
		  /* SRFI 61 extended cond */
		  if (!scm_is_null (SCM_CDR (clause))
		      && !scm_is_null (SCM_CDDR (clause))
		      && scm_is_eq (SCM_CADDR (clause), SCM_IM_ARROW))
		    {
		      SCM xx, guard_result;
		      if (SCM_VALUESP (arg1))
			arg1 = scm_struct_ref (arg1, SCM_INUM0);
		      else
			arg1 = scm_list_1 (arg1);
		      xx = SCM_CDR (clause);
		      proc = EVALCAR (xx, env);
		      guard_result = SCM_APPLY (proc, arg1, SCM_EOL);
		      if (scm_is_true (guard_result)
			  && !SCM_NILP (guard_result))
			{
			  proc = SCM_CDDR (xx);
			  proc = EVALCAR (proc, env);
			  PREP_APPLY (proc, arg1);
			  goto apply_proc;
			}
		    }
                  else if (scm_is_true (arg1) && !SCM_NILP (arg1))
                    {
                      x = SCM_CDR (clause);
                      if (scm_is_null (x))
                        RETURN (arg1);
                      else if (!scm_is_eq (SCM_CAR (x), SCM_IM_ARROW))
                        {
                          PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
                          goto begin;
                        }
                      else
                        {
                          proc = SCM_CDR (x);
                          proc = EVALCAR (proc, env);
                          PREP_APPLY (proc, scm_list_1 (arg1));
                          ENTER_APPLY;
                          goto evap1;
                        }
                    }
                  x = SCM_CDR (x);
                }
            }
          RETURN (SCM_UNSPECIFIED);


        case (ISYMNUM (SCM_IM_DO)):
          x = SCM_CDR (x);
          {
            /* Compute the initialization values and the initial environment.  */
            SCM init_forms = SCM_CAR (x);
            SCM init_values = SCM_EOL;
            while (!scm_is_null (init_forms))
              {
                init_values = scm_cons (EVALCAR (init_forms, env), init_values);
                init_forms = SCM_CDR (init_forms);
              }
            x = SCM_CDR (x);
            env = SCM_EXTEND_ENV (SCM_CAR (x), init_values, env);
          }
          x = SCM_CDR (x);
          {
            SCM test_form = SCM_CAR (x);
            SCM body_forms = SCM_CADR (x);
            SCM step_forms = SCM_CDDR (x);

            SCM test_result = EVALCAR (test_form, env);

            while (scm_is_false (test_result) || SCM_NILP (test_result))
              {
                {
                  /* Evaluate body forms.  */
                  SCM temp_forms;
                  for (temp_forms = body_forms;
                       !scm_is_null (temp_forms);
                       temp_forms = SCM_CDR (temp_forms))
                    {
                      SCM form = SCM_CAR (temp_forms);
                      /* Dirk:FIXME: We only need to eval forms that may have
                       * a side effect here.  This is only true for forms that
                       * start with a pair.  All others are just constants.
                       * Since with the current memoizer 'form' may hold a
                       * constant, we call EVAL here to handle the constant
                       * cases.  In the long run it would make sense to have
                       * the macro transformer of 'do' eliminate all forms
                       * that have no sideeffect.  Then instead of EVAL we
                       * could call CEVAL directly here.  */
                      (void) EVAL (form, env);
                    }
                }

                {
                  /* Evaluate the step expressions.  */
                  SCM temp_forms;
                  SCM step_values = SCM_EOL;
                  for (temp_forms = step_forms;
                       !scm_is_null (temp_forms);
                       temp_forms = SCM_CDR (temp_forms))
                    {
                      const SCM value = EVALCAR (temp_forms, env);
                      step_values = scm_cons (value, step_values);
                    }
                  env = SCM_EXTEND_ENV (SCM_CAAR (env),
                                        step_values,
                                        SCM_CDR (env));
                }

                test_result = EVALCAR (test_form, env);
              }
          }
          x = SCM_CDAR (x);
          if (scm_is_null (x))
            RETURN (SCM_UNSPECIFIED);
          PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
          goto nontoplevel_begin;


        case (ISYMNUM (SCM_IM_IF)):
          x = SCM_CDR (x);
          {
            SCM test_result = EVALCAR (x, env);
            x = SCM_CDR (x);  /* then expression */
            if (scm_is_false (test_result) || SCM_NILP (test_result))
              {
                x = SCM_CDR (x);  /* else expression */
                if (scm_is_null (x))
                  RETURN (SCM_UNSPECIFIED);
              }
          }
          PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
          goto carloop;


        case (ISYMNUM (SCM_IM_LET)):
          x = SCM_CDR (x);
          {
            SCM init_forms = SCM_CADR (x);
            SCM init_values = SCM_EOL;
            do
              {
                init_values = scm_cons (EVALCAR (init_forms, env), init_values);
                init_forms = SCM_CDR (init_forms);
              }
            while (!scm_is_null (init_forms));
            env = SCM_EXTEND_ENV (SCM_CAR (x), init_values, env);
          }
          x = SCM_CDDR (x);
          PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
          goto nontoplevel_begin;


        case (ISYMNUM (SCM_IM_LETREC)):
          x = SCM_CDR (x);
          env = SCM_EXTEND_ENV (SCM_CAR (x), undefineds, env);
          x = SCM_CDR (x);
          {
            SCM init_forms = SCM_CAR (x);
	    SCM init_values = scm_list_1 (SCM_BOOL_T);
	    SCM *init_values_eol = SCM_CDRLOC (init_values);
	    eval_letrec_inits (env, init_forms, &init_values_eol);
            SCM_SETCDR (SCM_CAR (env), SCM_CDR (init_values));
          }
          x = SCM_CDR (x);
          PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
          goto nontoplevel_begin;


        case (ISYMNUM (SCM_IM_LETSTAR)):
          x = SCM_CDR (x);
          {
            SCM bindings = SCM_CAR (x);
            if (!scm_is_null (bindings))
              {
                do
                  {
                    SCM name = SCM_CAR (bindings);
                    SCM init = SCM_CDR (bindings);
                    env = SCM_EXTEND_ENV (name, EVALCAR (init, env), env);
                    bindings = SCM_CDR (init);
                  }
                while (!scm_is_null (bindings));
              }
          }
          x = SCM_CDR (x);
          PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
          goto nontoplevel_begin;


        case (ISYMNUM (SCM_IM_OR)):
          x = SCM_CDR (x);
          while (!scm_is_null (SCM_CDR (x)))
            {
              SCM val = EVALCAR (x, env);
              if (scm_is_true (val) && !SCM_NILP (val))
                RETURN (val);
              else
                x = SCM_CDR (x);
            }
          PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
          goto carloop;


        case (ISYMNUM (SCM_IM_LAMBDA)):
          RETURN (scm_closure (SCM_CDR (x), env));


        case (ISYMNUM (SCM_IM_QUOTE)):
          RETURN (SCM_CDR (x));


        case (ISYMNUM (SCM_IM_SET_X)):
          x = SCM_CDR (x);
          {
            SCM *location;
            SCM variable = SCM_CAR (x);
            if (SCM_ILOCP (variable))
              location = scm_ilookup (variable, env);
            else if (SCM_VARIABLEP (variable))
              location = SCM_VARIABLE_LOC (variable);
            else
              {
                /* (scm_is_symbol (variable)) is known to be true */
                variable = lazy_memoize_variable (variable, env);
                SCM_SETCAR (x, variable);
                location = SCM_VARIABLE_LOC (variable);
              }
            x = SCM_CDR (x);
            *location = EVALCAR (x, env);
          }
          RETURN (SCM_UNSPECIFIED);


	case (ISYMNUM (SCM_IM_APPLY)):
          /* Evaluate the procedure to be applied.  */
	  x = SCM_CDR (x);
	  proc = EVALCAR (x, env);
          PREP_APPLY (proc, SCM_EOL);

          /* Evaluate the argument holding the list of arguments */
          x = SCM_CDR (x);
          arg1 = EVALCAR (x, env);

        apply_proc:
          /* Go here to tail-apply a procedure.  PROC is the procedure and
           * ARG1 is the list of arguments. PREP_APPLY must have been called
           * before jumping to apply_proc.  */
	  if (SCM_CLOSUREP (proc))
	    {
              SCM formals = SCM_CLOSURE_FORMALS (proc);
#ifdef DEVAL
              debug.info->a.args = arg1;
#endif
              if (SCM_UNLIKELY (scm_badargsp (formals, arg1)))
                scm_wrong_num_args (proc);
              ENTER_APPLY;
              /* Copy argument list */
              if (SCM_NULL_OR_NIL_P (arg1))
                env = SCM_EXTEND_ENV (formals, SCM_EOL, SCM_ENV (proc));
              else
                {
                  SCM args = scm_list_1 (SCM_CAR (arg1));
                  SCM tail = args;
                  arg1 = SCM_CDR (arg1);
                  while (!SCM_NULL_OR_NIL_P (arg1))
                    {
                      SCM new_tail = scm_list_1 (SCM_CAR (arg1));
                      SCM_SETCDR (tail, new_tail);
                      tail = new_tail;
                      arg1 = SCM_CDR (arg1);
                    }
                  env = SCM_EXTEND_ENV (formals, args, SCM_ENV (proc));
                }

              x = SCM_CLOSURE_BODY (proc);
              goto nontoplevel_begin;
	    }
	  else
	    {
              ENTER_APPLY;
              RETURN (SCM_APPLY (proc, arg1, SCM_EOL));
	    }


	case (ISYMNUM (SCM_IM_CONT)):
	  {
	    int first;
	    SCM val = scm_make_continuation (&first);

	    if (!first)
	      RETURN (val);
	    else
	      {
		arg1 = val;
		proc = SCM_CDR (x);
		proc = EVALCAR (proc, env);
		PREP_APPLY (proc, scm_list_1 (arg1));
		ENTER_APPLY;
		goto evap1;
	      }
	  }


	case (ISYMNUM (SCM_IM_DELAY)):
	  RETURN (scm_makprom (scm_closure (SCM_CDR (x), env)));

#if 0
	  /* See futures.h for a comment why futures are not enabled.
	   */
	case (ISYMNUM (SCM_IM_FUTURE)):
	  RETURN (scm_i_make_future (scm_closure (SCM_CDR (x), env)));
#endif

	  /* PLACEHOLDER for case (ISYMNUM (SCM_IM_DISPATCH)): The following
	     code (type_dispatch) is intended to be the tail of the case
	     clause for the internal macro SCM_IM_DISPATCH.  Please don't
	     remove it from this location without discussing it with Mikael
	     <djurfeldt@nada.kth.se>  */
	  
	  /* The type dispatch code is duplicated below
	   * (c.f. objects.c:scm_mcache_compute_cmethod) since that
	   * cuts down execution time for type dispatch to 50%.  */
	type_dispatch: /* inputs: x, arg1 */
	  /* Type dispatch means to determine from the types of the function
	   * arguments (i. e. the 'signature' of the call), which method from
	   * a generic function is to be called.  This process of selecting
	   * the right method takes some time.  To speed it up, guile uses
	   * caching:  Together with the macro call to dispatch the signatures
	   * of some previous calls to that generic function from the same
	   * place are stored (in the code!) in a cache that we call the
	   * 'method cache'.  This is done since it is likely, that
	   * consecutive calls to dispatch from that position in the code will
	   * have the same signature.  Thus, the type dispatch works as
	   * follows: First, determine a hash value from the signature of the
	   * actual arguments.  Second, use this hash value as an index to
	   * find that same signature in the method cache stored at this
	   * position in the code.  If found, you have also found the 
	   * corresponding method that belongs to that signature.  If the
	   * signature is not found in the method cache, you have to perform a
	   * full search over all signatures stored with the generic
	   * function.  */
	{
	    unsigned long int specializers;
	    unsigned long int hash_value;
	    unsigned long int cache_end_pos;
	    unsigned long int mask;
	    SCM method_cache;

	    {
	      SCM z = SCM_CDDR (x);
	      SCM tmp = SCM_CADR (z);
	      specializers = scm_to_ulong (SCM_CAR (z));

	      /* Compute a hash value for searching the method cache.  There
	       * are two variants for computing the hash value, a (rather)
	       * complicated one, and a simple one.  For the complicated one
	       * explained below, tmp holds a number that is used in the
	       * computation.  */
	      if (scm_is_simple_vector (tmp))
		{
		  /* This method of determining the hash value is much
		   * simpler:  Set the hash value to zero and just perform a
		   * linear search through the method cache.  */
		  method_cache = tmp;
		  mask = (unsigned long int) ((long) -1);
		  hash_value = 0;
		  cache_end_pos = SCM_SIMPLE_VECTOR_LENGTH (method_cache);
		}
	      else
		{
		  /* Use the signature of the actual arguments to determine
		   * the hash value.  This is done as follows:  Each class has
		   * an array of random numbers, that are determined when the
		   * class is created.  The integer 'hashset' is an index into
		   * that array of random numbers.  Now, from all classes that
		   * are part of the signature of the actual arguments, the
		   * random numbers at index 'hashset' are taken and summed
		   * up, giving the hash value.  The value of 'hashset' is
		   * stored at the call to dispatch.  This allows to have
		   * different 'formulas' for calculating the hash value at
		   * different places where dispatch is called.  This allows
		   * to optimize the hash formula at every individual place
		   * where dispatch is called, such that hopefully the hash
		   * value that is computed will directly point to the right
		   * method in the method cache.  */
		  unsigned long int hashset = scm_to_ulong (tmp);
		  unsigned long int counter = specializers + 1;
		  SCM tmp_arg = arg1;
		  hash_value = 0;
		  while (!scm_is_null (tmp_arg) && counter != 0)
		    {
		      SCM class = scm_class_of (SCM_CAR (tmp_arg));
		      hash_value += SCM_INSTANCE_HASH (class, hashset);
		      tmp_arg = SCM_CDR (tmp_arg);
		      counter--;
		    }
		  z = SCM_CDDR (z);
		  method_cache = SCM_CADR (z);
		  mask = scm_to_ulong (SCM_CAR (z));
		  hash_value &= mask;
		  cache_end_pos = hash_value;
		}
	    }

	    {
	      /* Search the method cache for a method with a matching
	       * signature.  Start the search at position 'hash_value'.  The
	       * hashing implementation uses linear probing for conflict
	       * resolution, that is, if the signature in question is not
	       * found at the starting index in the hash table, the next table
	       * entry is tried, and so on, until in the worst case the whole
	       * cache has been searched, but still the signature has not been
	       * found.  */
	      SCM z;
	      do
		{
		  SCM args = arg1; /* list of arguments */
		  z = SCM_SIMPLE_VECTOR_REF (method_cache, hash_value);
		  while (!scm_is_null (args))
		    {
		      /* More arguments than specifiers => CLASS != ENV */
		      SCM class_of_arg = scm_class_of (SCM_CAR (args));
		      if (!scm_is_eq (class_of_arg, SCM_CAR (z)))
			goto next_method;
		      args = SCM_CDR (args);
		      z = SCM_CDR (z);
		    }
		  /* Fewer arguments than specifiers => CAR != ENV */
		  if (scm_is_null (SCM_CAR (z)) || scm_is_pair (SCM_CAR (z)))
		    goto apply_cmethod;
		next_method:
		  hash_value = (hash_value + 1) & mask;
		} while (hash_value != cache_end_pos);

	      /* No appropriate method was found in the cache.  */
	      z = scm_memoize_method (x, arg1);

	    apply_cmethod: /* inputs: z, arg1 */
	      {
		SCM formals = SCM_CMETHOD_FORMALS (z);
		env = SCM_EXTEND_ENV (formals, arg1, SCM_CMETHOD_ENV (z));
		x = SCM_CMETHOD_BODY (z);
		goto nontoplevel_begin;
	      }
	    }
	  }


	case (ISYMNUM (SCM_IM_SLOT_REF)):
	  x = SCM_CDR (x);
	  {
	    SCM instance = EVALCAR (x, env);
	    unsigned long int slot = SCM_I_INUM (SCM_CDR (x));
	    RETURN (SCM_PACK (SCM_STRUCT_DATA (instance) [slot]));
	  }


	case (ISYMNUM (SCM_IM_SLOT_SET_X)):
	  x = SCM_CDR (x);
	  {
	    SCM instance = EVALCAR (x, env);
	    unsigned long int slot = SCM_I_INUM (SCM_CADR (x));
	    SCM value = EVALCAR (SCM_CDDR (x), env);
	    SCM_STRUCT_DATA (instance) [slot] = SCM_UNPACK (value);
	    RETURN (SCM_UNSPECIFIED);
	  }


#if SCM_ENABLE_ELISP
	  
	case (ISYMNUM (SCM_IM_NIL_COND)):
	  {
	    SCM test_form = SCM_CDR (x);
	    x = SCM_CDR (test_form);
	    while (!SCM_NULL_OR_NIL_P (x))
	      {
		SCM test_result = EVALCAR (test_form, env);
		if (!(scm_is_false (test_result)
		      || SCM_NULL_OR_NIL_P (test_result)))
		  {
		    if (scm_is_eq (SCM_CAR (x), SCM_UNSPECIFIED))
		      RETURN (test_result);
		    PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
		    goto carloop;
		  }
		else
		  {
		    test_form = SCM_CDR (x);
		    x = SCM_CDR (test_form);
		  }
	      }
	    x = test_form;
	    PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
	    goto carloop;
	  }

#endif /* SCM_ENABLE_ELISP */

	case (ISYMNUM (SCM_IM_BIND)):
	  {
	    SCM vars, exps, vals;

	    x = SCM_CDR (x);
	    vars = SCM_CAAR (x);
	    exps = SCM_CDAR (x);
	    vals = SCM_EOL;
	    while (!scm_is_null (exps))
	      {
		vals = scm_cons (EVALCAR (exps, env), vals);
		exps = SCM_CDR (exps);
	      }
	    
	    scm_swap_bindings (vars, vals);
	    scm_i_set_dynwinds (scm_acons (vars, vals, scm_i_dynwinds ()));

	    /* Ignore all but the last evaluation result.  */
	    for (x = SCM_CDR (x); !scm_is_null (SCM_CDR (x)); x = SCM_CDR (x))
	      {
		if (scm_is_pair (SCM_CAR (x)))
		  CEVAL (SCM_CAR (x), env);
	      }
	    proc = EVALCAR (x, env);
	  
	    scm_i_set_dynwinds (SCM_CDR (scm_i_dynwinds ()));
	    scm_swap_bindings (vars, vals);

	    RETURN (proc);
	  }


	case (ISYMNUM (SCM_IM_CALL_WITH_VALUES)):
	  {
            SCM producer;

	    x = SCM_CDR (x);
	    producer = EVALCAR (x, env);
	    x = SCM_CDR (x);
	    proc = EVALCAR (x, env);  /* proc is the consumer. */
	    arg1 = SCM_APPLY (producer, SCM_EOL, SCM_EOL);
	    if (SCM_VALUESP (arg1))
              {
                /* The list of arguments is not copied.  Rather, it is assumed
                 * that this has been done by the 'values' procedure.  */
                arg1 = scm_struct_ref (arg1, SCM_INUM0);
              }
	    else
              {
                arg1 = scm_list_1 (arg1);
              }
            PREP_APPLY (proc, arg1);
            goto apply_proc;
	  }


	default:
	  break;
	}
    }
  else
    {
      if (SCM_VARIABLEP (SCM_CAR (x)))
        proc = SCM_VARIABLE_REF (SCM_CAR (x));
      else if (SCM_ILOCP (SCM_CAR (x)))
        proc = *scm_ilookup (SCM_CAR (x), env);
      else if (scm_is_pair (SCM_CAR (x)))
	proc = CEVAL (SCM_CAR (x), env);
      else if (scm_is_symbol (SCM_CAR (x)))
	{
	  SCM orig_sym = SCM_CAR (x);
	  {
	    SCM *location = scm_lookupcar1 (x, env, 1);
	    if (location == NULL)
	      {
		/* we have lost the race, start again. */
		goto dispatch;
	      }
	    proc = *location;
	  }

	  if (SCM_MACROP (proc))
	    {
	      SCM_SETCAR (x, orig_sym);  /* Undo memoizing effect of
					    lookupcar */
	    handle_a_macro: /* inputs: x, env, proc */
#ifdef DEVAL
	      /* Set a flag during macro expansion so that macro
		 application frames can be deleted from the backtrace. */
	      SCM_SET_MACROEXP (debug);
#endif
	      arg1 = SCM_APPLY (SCM_MACRO_CODE (proc), x,
                                scm_cons (env, scm_listofnull));
#ifdef DEVAL
	      SCM_CLEAR_MACROEXP (debug);
#endif
	      switch (SCM_MACRO_TYPE (proc))
		{
		case 3:
		case 2:
		  if (!scm_is_pair (arg1))
		    arg1 = scm_list_2 (SCM_IM_BEGIN, arg1);

                  assert (!scm_is_eq (x, SCM_CAR (arg1))
                          && !scm_is_eq (x, SCM_CDR (arg1)));

#ifdef DEVAL
		  if (!SCM_CLOSUREP (SCM_MACRO_CODE (proc)))
		    {
		      SCM_CRITICAL_SECTION_START;
		      SCM_SETCAR (x, SCM_CAR (arg1));
		      SCM_SETCDR (x, SCM_CDR (arg1));
		      SCM_CRITICAL_SECTION_END;
		      goto dispatch;
		    }
		  /* Prevent memoizing of debug info expression. */
		  debug.info->e.exp = scm_cons_source (debug.info->e.exp,
						       SCM_CAR (x),
						       SCM_CDR (x));
#endif
		  SCM_CRITICAL_SECTION_START;
		  SCM_SETCAR (x, SCM_CAR (arg1));
		  SCM_SETCDR (x, SCM_CDR (arg1));
		  SCM_CRITICAL_SECTION_END;
		  PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
		  goto loop;
#if SCM_ENABLE_DEPRECATED == 1
		case 1:
		  x = arg1;
		  if (SCM_NIMP (x))
		    {
		      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
		      goto loop;
		    }
		  else
		    RETURN (arg1);
#endif
		case 0:
		  RETURN (arg1);
		}
	    }
	}
      else
        proc = SCM_CAR (x);

      if (SCM_MACROP (proc))
	goto handle_a_macro;
    }


  /* When reaching this part of the code, the following is granted: Variable x
   * holds the first pair of an expression of the form (<function> arg ...).
   * Variable proc holds the object that resulted from the evaluation of
   * <function>.  In the following, the arguments (if any) will be evaluated,
   * and proc will be applied to them.  If proc does not really hold a
   * function object, this will be signalled as an error on the scheme
   * level.  If the number of arguments does not match the number of arguments
   * that are allowed to be passed to proc, also an error on the scheme level
   * will be signalled.  */
  PREP_APPLY (proc, SCM_EOL);
  if (scm_is_null (SCM_CDR (x))) {
    ENTER_APPLY;
  evap0:
    SCM_ASRTGO (!SCM_IMP (proc), badfun);
    switch (SCM_TYP7 (proc))
      {				/* no arguments given */
      case scm_tc7_subr_0:
	RETURN (SCM_SUBRF (proc) ());
      case scm_tc7_subr_1o:
	RETURN (SCM_SUBRF (proc) (SCM_UNDEFINED));
      case scm_tc7_lsubr:
	RETURN (SCM_SUBRF (proc) (SCM_EOL));
      case scm_tc7_rpsubr:
	RETURN (SCM_BOOL_T);
      case scm_tc7_asubr:
	RETURN (SCM_SUBRF (proc) (SCM_UNDEFINED, SCM_UNDEFINED));
      case scm_tc7_smob:
	if (!SCM_SMOB_APPLICABLE_P (proc))
	  goto badfun;
	RETURN (SCM_SMOB_APPLY_0 (proc));
      case scm_tc7_cclo:
	arg1 = proc;
	proc = SCM_CCLO_SUBR (proc);
#ifdef DEVAL
	debug.info->a.proc = proc;
	debug.info->a.args = scm_list_1 (arg1);
#endif
	goto evap1;
      case scm_tc7_pws:
	proc = SCM_PROCEDURE (proc);
#ifdef DEVAL
	debug.info->a.proc = proc;
#endif
	if (!SCM_CLOSUREP (proc))
	  goto evap0;
        /* fallthrough */
      case scm_tcs_closures:
        {
          const SCM formals = SCM_CLOSURE_FORMALS (proc);
          if (SCM_UNLIKELY (scm_is_pair (formals)))
            goto wrongnumargs;
          x = SCM_CLOSURE_BODY (proc);
          env = SCM_EXTEND_ENV (formals, SCM_EOL, SCM_ENV (proc));
          goto nontoplevel_begin;
        }
      case scm_tcs_struct:
	if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	  {
	    x = SCM_ENTITY_PROCEDURE (proc);
	    arg1 = SCM_EOL;
	    goto type_dispatch;
	  }
	else if (SCM_I_OPERATORP (proc))
	  {
	    arg1 = proc;
	    proc = (SCM_I_ENTITYP (proc)
		    ? SCM_ENTITY_PROCEDURE (proc)
		    : SCM_OPERATOR_PROCEDURE (proc));
#ifdef DEVAL
	    debug.info->a.proc = proc;
	    debug.info->a.args = scm_list_1 (arg1);
#endif
            goto evap1;
	  }
        else
          goto badfun;
      case scm_tc7_subr_1:
      case scm_tc7_subr_2:
      case scm_tc7_subr_2o:
      case scm_tc7_dsubr:
      case scm_tc7_cxr:
      case scm_tc7_subr_3:
      case scm_tc7_lsubr_2:
      wrongnumargs:
	scm_wrong_num_args (proc);
      default:
      badfun:
        scm_misc_error (NULL, "Wrong type to apply: ~S", scm_list_1 (proc));
      }
  }

  /* must handle macros by here */
  x = SCM_CDR (x);
  if (SCM_LIKELY (scm_is_pair (x)))
    arg1 = EVALCAR (x, env);
  else
    scm_wrong_num_args (proc);
#ifdef DEVAL
  debug.info->a.args = scm_list_1 (arg1);
#endif
  x = SCM_CDR (x);
  {
    SCM arg2;
    if (scm_is_null (x))
      {
	ENTER_APPLY;
      evap1: /* inputs: proc, arg1 */
        SCM_ASRTGO (!SCM_IMP (proc), badfun);
	switch (SCM_TYP7 (proc))
	  {				/* have one argument in arg1 */
	  case scm_tc7_subr_2o:
	    RETURN (SCM_SUBRF (proc) (arg1, SCM_UNDEFINED));
	  case scm_tc7_subr_1:
	  case scm_tc7_subr_1o:
	    RETURN (SCM_SUBRF (proc) (arg1));
	  case scm_tc7_dsubr:
            if (SCM_I_INUMP (arg1))
              {
                RETURN (scm_from_double (SCM_DSUBRF (proc) ((double) SCM_I_INUM (arg1))));
              }
            else if (SCM_REALP (arg1))
              {
                RETURN (scm_from_double (SCM_DSUBRF (proc) (SCM_REAL_VALUE (arg1))));
              }
            else if (SCM_BIGP (arg1))
              {
                RETURN (scm_from_double (SCM_DSUBRF (proc) (scm_i_big2dbl (arg1))));
              }
	    else if (SCM_FRACTIONP (arg1))
	      {
                RETURN (scm_from_double (SCM_DSUBRF (proc) (scm_i_fraction2double (arg1))));
	      }
	    SCM_WTA_DISPATCH_1 (*SCM_SUBR_GENERIC (proc), arg1,
                                SCM_ARG1,
				scm_i_symbol_chars (SCM_SNAME (proc)));
	  case scm_tc7_cxr:
	    RETURN (scm_i_chase_pairs (arg1, (scm_t_bits) SCM_SUBRF (proc)));
	  case scm_tc7_rpsubr:
	    RETURN (SCM_BOOL_T);
	  case scm_tc7_asubr:
	    RETURN (SCM_SUBRF (proc) (arg1, SCM_UNDEFINED));
	  case scm_tc7_lsubr:
#ifdef DEVAL
	    RETURN (SCM_SUBRF (proc) (debug.info->a.args));
#else
	    RETURN (SCM_SUBRF (proc) (scm_list_1 (arg1)));
#endif
	  case scm_tc7_smob:
	    if (!SCM_SMOB_APPLICABLE_P (proc))
	      goto badfun;
	    RETURN (SCM_SMOB_APPLY_1 (proc, arg1));
	  case scm_tc7_cclo:
	    arg2 = arg1;
	    arg1 = proc;
	    proc = SCM_CCLO_SUBR (proc);
#ifdef DEVAL
	    debug.info->a.args = scm_cons (arg1, debug.info->a.args);
	    debug.info->a.proc = proc;
#endif
	    goto evap2;
	  case scm_tc7_pws:
	    proc = SCM_PROCEDURE (proc);
#ifdef DEVAL
	    debug.info->a.proc = proc;
#endif
	    if (!SCM_CLOSUREP (proc))
	      goto evap1;
            /* fallthrough */
	  case scm_tcs_closures:
            {
              /* clos1: */
              const SCM formals = SCM_CLOSURE_FORMALS (proc);
              if (SCM_UNLIKELY (scm_is_null (formals)
				|| (scm_is_pair (formals) &&
				    scm_is_pair (SCM_CDR (formals)))))
                goto wrongnumargs;
              x = SCM_CLOSURE_BODY (proc);
#ifdef DEVAL
              env = SCM_EXTEND_ENV (formals,
                                    debug.info->a.args,
                                    SCM_ENV (proc));
#else
              env = SCM_EXTEND_ENV (formals,
                                    scm_list_1 (arg1),
                                    SCM_ENV (proc));
#endif
              goto nontoplevel_begin;
            }
	  case scm_tcs_struct:
	    if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	      {
		x = SCM_ENTITY_PROCEDURE (proc);
#ifdef DEVAL
		arg1 = debug.info->a.args;
#else
		arg1 = scm_list_1 (arg1);
#endif
		goto type_dispatch;
	      }
	    else if (SCM_I_OPERATORP (proc))
	      {
		arg2 = arg1;
		arg1 = proc;
		proc = (SCM_I_ENTITYP (proc)
			? SCM_ENTITY_PROCEDURE (proc)
			: SCM_OPERATOR_PROCEDURE (proc));
#ifdef DEVAL
		debug.info->a.args = scm_cons (arg1, debug.info->a.args);
		debug.info->a.proc = proc;
#endif
                goto evap2;
	      }
            else
              goto badfun;
	  case scm_tc7_subr_2:
	  case scm_tc7_subr_0:
	  case scm_tc7_subr_3:
	  case scm_tc7_lsubr_2:
	    scm_wrong_num_args (proc);
	  default:
	    goto badfun;
	  }
      }
    if (SCM_LIKELY (scm_is_pair (x)))
      arg2 = EVALCAR (x, env);
    else
      scm_wrong_num_args (proc);

    {				/* have two or more arguments */
#ifdef DEVAL
      debug.info->a.args = scm_list_2 (arg1, arg2);
#endif
      x = SCM_CDR (x);
      if (scm_is_null (x)) {
	ENTER_APPLY;
      evap2:
        SCM_ASRTGO (!SCM_IMP (proc), badfun);
	switch (SCM_TYP7 (proc))
	  {			/* have two arguments */
	  case scm_tc7_subr_2:
	  case scm_tc7_subr_2o:
	    RETURN (SCM_SUBRF (proc) (arg1, arg2));
	  case scm_tc7_lsubr:
#ifdef DEVAL
	    RETURN (SCM_SUBRF (proc) (debug.info->a.args));
#else
	    RETURN (SCM_SUBRF (proc) (scm_list_2 (arg1, arg2)));
#endif
	  case scm_tc7_lsubr_2:
	    RETURN (SCM_SUBRF (proc) (arg1, arg2, SCM_EOL));
	  case scm_tc7_rpsubr:
	  case scm_tc7_asubr:
	    RETURN (SCM_SUBRF (proc) (arg1, arg2));
	  case scm_tc7_smob:
	    if (!SCM_SMOB_APPLICABLE_P (proc))
	      goto badfun;
	    RETURN (SCM_SMOB_APPLY_2 (proc, arg1, arg2));
	  cclon:
	  case scm_tc7_cclo:
#ifdef DEVAL
	    RETURN (SCM_APPLY (SCM_CCLO_SUBR (proc),
			       scm_cons (proc, debug.info->a.args),
			       SCM_EOL));
#else
	    RETURN (SCM_APPLY (SCM_CCLO_SUBR (proc),
			       scm_cons2 (proc, arg1,
					  scm_cons (arg2,
						    scm_eval_args (x,
								   env,
								   proc))),
			       SCM_EOL));
#endif
	  case scm_tcs_struct:
	    if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	      {
		x = SCM_ENTITY_PROCEDURE (proc);
#ifdef DEVAL
		arg1 = debug.info->a.args;
#else
		arg1 = scm_list_2 (arg1, arg2);
#endif
		goto type_dispatch;
	      }
	    else if (SCM_I_OPERATORP (proc))
	      {
	      operatorn:
#ifdef DEVAL
		RETURN (SCM_APPLY (SCM_I_ENTITYP (proc)
				   ? SCM_ENTITY_PROCEDURE (proc)
				   : SCM_OPERATOR_PROCEDURE (proc),
				   scm_cons (proc, debug.info->a.args),
				   SCM_EOL));
#else
		RETURN (SCM_APPLY (SCM_I_ENTITYP (proc)
				   ? SCM_ENTITY_PROCEDURE (proc)
				   : SCM_OPERATOR_PROCEDURE (proc),
				   scm_cons2 (proc, arg1,
					      scm_cons (arg2,
							scm_eval_args (x,
								       env,
								       proc))),
				   SCM_EOL));
#endif
	      }
            else
              goto badfun;
	  case scm_tc7_subr_0:
	  case scm_tc7_dsubr:
	  case scm_tc7_cxr:
	  case scm_tc7_subr_1o:
	  case scm_tc7_subr_1:
	  case scm_tc7_subr_3:
	    scm_wrong_num_args (proc);
	  default:
	    goto badfun;
	  case scm_tc7_pws:
	    proc = SCM_PROCEDURE (proc);
#ifdef DEVAL
	    debug.info->a.proc = proc;
#endif
	    if (!SCM_CLOSUREP (proc))
	      goto evap2;
            /* fallthrough */
	  case scm_tcs_closures:
            {
              /* clos2: */
              const SCM formals = SCM_CLOSURE_FORMALS (proc);
              if (SCM_UNLIKELY
		  (scm_is_null (formals)
		   || (scm_is_pair (formals)
		       && (scm_is_null (SCM_CDR (formals))
			   || (scm_is_pair (SCM_CDR (formals))
			       && scm_is_pair (SCM_CDDR (formals)))))))
                goto wrongnumargs;
#ifdef DEVAL
              env = SCM_EXTEND_ENV (formals,
                                    debug.info->a.args,
                                    SCM_ENV (proc));
#else
              env = SCM_EXTEND_ENV (formals,
                                    scm_list_2 (arg1, arg2),
                                    SCM_ENV (proc));
#endif
              x = SCM_CLOSURE_BODY (proc);
              goto nontoplevel_begin;
            }
	  }
      }
      if (SCM_UNLIKELY (!scm_is_pair (x)))
	scm_wrong_num_args (proc);
#ifdef DEVAL
      debug.info->a.args = scm_cons2 (arg1, arg2,
				      deval_args (x, env, proc,
						  SCM_CDRLOC (SCM_CDR (debug.info->a.args))));
#endif
      ENTER_APPLY;
    evap3:
      SCM_ASRTGO (!SCM_IMP (proc), badfun);
      switch (SCM_TYP7 (proc))
	{			/* have 3 or more arguments */
#ifdef DEVAL
	case scm_tc7_subr_3:
	  if (SCM_UNLIKELY (!scm_is_null (SCM_CDR (x))))
	    scm_wrong_num_args (proc);
	  else
	    RETURN (SCM_SUBRF (proc) (arg1, arg2,
				      SCM_CADDR (debug.info->a.args)));
	case scm_tc7_asubr:
	  arg1 = SCM_SUBRF(proc)(arg1, arg2);
	  arg2 = SCM_CDDR (debug.info->a.args);
	  do
	    {
	      arg1 = SCM_SUBRF(proc)(arg1, SCM_CAR (arg2));
	      arg2 = SCM_CDR (arg2);
	    }
	  while (SCM_NIMP (arg2));
	  RETURN (arg1);
	case scm_tc7_rpsubr:
	  if (scm_is_false (SCM_SUBRF (proc) (arg1, arg2)))
	    RETURN (SCM_BOOL_F);
	  arg1 = SCM_CDDR (debug.info->a.args);
	  do
	    {
	      if (scm_is_false (SCM_SUBRF (proc) (arg2, SCM_CAR (arg1))))
		RETURN (SCM_BOOL_F);
	      arg2 = SCM_CAR (arg1);
	      arg1 = SCM_CDR (arg1);
	    }
	  while (SCM_NIMP (arg1));
	  RETURN (SCM_BOOL_T);
	case scm_tc7_lsubr_2:
	  RETURN (SCM_SUBRF (proc) (arg1, arg2,
				    SCM_CDDR (debug.info->a.args)));
	case scm_tc7_lsubr:
	  RETURN (SCM_SUBRF (proc) (debug.info->a.args));
	case scm_tc7_smob:
	  if (!SCM_SMOB_APPLICABLE_P (proc))
	    goto badfun;
	  RETURN (SCM_SMOB_APPLY_3 (proc, arg1, arg2,
				    SCM_CDDR (debug.info->a.args)));
	case scm_tc7_cclo:
	  goto cclon;
	case scm_tc7_pws:
	  proc = SCM_PROCEDURE (proc);
	  debug.info->a.proc = proc;
	  if (!SCM_CLOSUREP (proc))
	    goto evap3;
          /* fallthrough */
	case scm_tcs_closures:
          {
            const SCM formals = SCM_CLOSURE_FORMALS (proc);
            if (scm_is_null (formals)
                || (scm_is_pair (formals)
                    && (scm_is_null (SCM_CDR (formals))
                        || (scm_is_pair (SCM_CDR (formals))
                            && scm_badargsp (SCM_CDDR (formals), x)))))
              goto wrongnumargs;
            SCM_SET_ARGSREADY (debug);
            env = SCM_EXTEND_ENV (formals,
                                  debug.info->a.args,
                                  SCM_ENV (proc));
            x = SCM_CLOSURE_BODY (proc);
            goto nontoplevel_begin;
          }
#else /* DEVAL */
	case scm_tc7_subr_3:
	  if (SCM_UNLIKELY (!scm_is_null (SCM_CDR (x))))
	    scm_wrong_num_args (proc);
	  else
	    RETURN (SCM_SUBRF (proc) (arg1, arg2, EVALCAR (x, env)));
	case scm_tc7_asubr:
	  arg1 = SCM_SUBRF (proc) (arg1, arg2);
	  do
	    {
	      arg1 = SCM_SUBRF(proc)(arg1, EVALCAR(x, env));
	      x = SCM_CDR(x);
	    }
	  while (!scm_is_null (x));
	  RETURN (arg1);
	case scm_tc7_rpsubr:
	  if (scm_is_false (SCM_SUBRF (proc) (arg1, arg2)))
	    RETURN (SCM_BOOL_F);
	  do
	    {
	      arg1 = EVALCAR (x, env);
	      if (scm_is_false (SCM_SUBRF (proc) (arg2, arg1)))
		RETURN (SCM_BOOL_F);
	      arg2 = arg1;
	      x = SCM_CDR (x);
	    }
	  while (!scm_is_null (x));
	  RETURN (SCM_BOOL_T);
	case scm_tc7_lsubr_2:
	  RETURN (SCM_SUBRF (proc) (arg1, arg2, scm_eval_args (x, env, proc)));
	case scm_tc7_lsubr:
	  RETURN (SCM_SUBRF (proc) (scm_cons2 (arg1,
					       arg2,
					       scm_eval_args (x, env, proc))));
	case scm_tc7_smob:
	  if (!SCM_SMOB_APPLICABLE_P (proc))
	    goto badfun;
	  RETURN (SCM_SMOB_APPLY_3 (proc, arg1, arg2,
				    scm_eval_args (x, env, proc)));
	case scm_tc7_cclo:
	  goto cclon;
	case scm_tc7_pws:
	  proc = SCM_PROCEDURE (proc);
	  if (!SCM_CLOSUREP (proc))
	    goto evap3;
          /* fallthrough */
	case scm_tcs_closures:
	  {
	    const SCM formals = SCM_CLOSURE_FORMALS (proc);
	    if (scm_is_null (formals)
		|| (scm_is_pair (formals)
		    && (scm_is_null (SCM_CDR (formals))
			|| (scm_is_pair (SCM_CDR (formals))
			    && scm_badargsp (SCM_CDDR (formals), x)))))
	      goto wrongnumargs;
            env = SCM_EXTEND_ENV (formals,
                                  scm_cons2 (arg1,
                                             arg2,
                                             scm_eval_args (x, env, proc)),
                                  SCM_ENV (proc));
            x = SCM_CLOSURE_BODY (proc);
            goto nontoplevel_begin;
	  }
#endif /* DEVAL */
	case scm_tcs_struct:
	  if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	    {
#ifdef DEVAL
	      arg1 = debug.info->a.args;
#else
	      arg1 = scm_cons2 (arg1, arg2, scm_eval_args (x, env, proc));
#endif
	      x = SCM_ENTITY_PROCEDURE (proc);
	      goto type_dispatch;
	    }
	  else if (SCM_I_OPERATORP (proc))
	    goto operatorn;
	  else
	    goto badfun;
	case scm_tc7_subr_2:
	case scm_tc7_subr_1o:
	case scm_tc7_subr_2o:
	case scm_tc7_subr_0:
	case scm_tc7_dsubr:
	case scm_tc7_cxr:
	case scm_tc7_subr_1:
	  scm_wrong_num_args (proc);
	default:
	  goto badfun;
	}
    }
  }
#ifdef DEVAL
exit:
  if (scm_check_exit_p && SCM_TRAPS_P)
    if (SCM_EXIT_FRAME_P || (SCM_TRACE_P && SCM_TRACED_FRAME_P (debug)))
      {
	SCM_CLEAR_TRACED_FRAME (debug);
	arg1 = scm_make_debugobj (&debug);
	SCM_TRAPS_P = 0;
	arg1 = scm_call_3 (SCM_EXIT_FRAME_HDLR, scm_sym_exit_frame, arg1, proc);
	SCM_TRAPS_P = 1;
	if (scm_is_pair (arg1) && scm_is_eq (SCM_CAR (arg1), sym_instead))
	  proc = SCM_CDR (arg1);
      }
  scm_i_set_last_debug_frame (debug.prev);
  return proc;
#endif
}


/* SECTION: This code is compiled once.
 */

#ifndef DEVAL



/* Simple procedure calls
 */

SCM
scm_call_0 (SCM proc)
{
  return scm_apply (proc, SCM_EOL, SCM_EOL);
}

SCM
scm_call_1 (SCM proc, SCM arg1)
{
  return scm_apply (proc, arg1, scm_listofnull);
}

SCM
scm_call_2 (SCM proc, SCM arg1, SCM arg2)
{
  return scm_apply (proc, arg1, scm_cons (arg2, scm_listofnull));
}

SCM
scm_call_3 (SCM proc, SCM arg1, SCM arg2, SCM arg3)
{
  return scm_apply (proc, arg1, scm_cons2 (arg2, arg3, scm_listofnull));
}

SCM
scm_call_4 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  return scm_apply (proc, arg1, scm_cons2 (arg2, arg3,
					   scm_cons (arg4, scm_listofnull)));
}

/* Simple procedure applies
 */

SCM
scm_apply_0 (SCM proc, SCM args)
{
  return scm_apply (proc, args, SCM_EOL);
}

SCM
scm_apply_1 (SCM proc, SCM arg1, SCM args)
{
  return scm_apply (proc, scm_cons (arg1, args), SCM_EOL);
}

SCM
scm_apply_2 (SCM proc, SCM arg1, SCM arg2, SCM args)
{
  return scm_apply (proc, scm_cons2 (arg1, arg2, args), SCM_EOL);
}

SCM
scm_apply_3 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM args)
{
  return scm_apply (proc, scm_cons (arg1, scm_cons2 (arg2, arg3, args)),
		    SCM_EOL);
}

/* This code processes the arguments to apply:

   (apply PROC ARG1 ... ARGS)

   Given a list (ARG1 ... ARGS), this function conses the ARG1
   ... arguments onto the front of ARGS, and returns the resulting
   list.  Note that ARGS is a list; thus, the argument to this
   function is a list whose last element is a list.

   Apply calls this function, and applies PROC to the elements of the
   result.  apply:nconc2last takes care of building the list of
   arguments, given (ARG1 ... ARGS).

   Rather than do new consing, apply:nconc2last destroys its argument.
   On that topic, this code came into my care with the following
   beautifully cryptic comment on that topic: "This will only screw
   you if you do (scm_apply scm_apply '( ... ))"  If you know what
   they're referring to, send me a patch to this comment.  */

SCM_DEFINE (scm_nconc2last, "apply:nconc2last", 1, 0, 0, 
	    (SCM lst),
	    "Given a list (@var{arg1} @dots{} @var{args}), this function\n"
	    "conses the @var{arg1} @dots{} arguments onto the front of\n"
	    "@var{args}, and returns the resulting list. Note that\n"
	    "@var{args} is a list; thus, the argument to this function is\n"
	    "a list whose last element is a list.\n"
	    "Note: Rather than do new consing, @code{apply:nconc2last}\n"
	    "destroys its argument, so use with care.")
#define FUNC_NAME s_scm_nconc2last
{
  SCM *lloc;
  SCM_VALIDATE_NONEMPTYLIST (1, lst);
  lloc = &lst;
  while (!scm_is_null (SCM_CDR (*lloc))) /* Perhaps should be
                                          SCM_NULL_OR_NIL_P, but not
                                          needed in 99.99% of cases,
                                          and it could seriously hurt
                                          performance. - Neil */
    lloc = SCM_CDRLOC (*lloc);
  SCM_ASSERT (scm_ilength (SCM_CAR (*lloc)) >= 0, lst, SCM_ARG1, FUNC_NAME);
  *lloc = SCM_CAR (*lloc);
  return lst;
}
#undef FUNC_NAME

#endif /* !DEVAL */


/* SECTION: When DEVAL is defined this code yields scm_dapply.
 * It is compiled twice.
 */

#if 0
SCM 
scm_apply (SCM proc, SCM arg1, SCM args)
{}
#endif

#if 0
SCM 
scm_dapply (SCM proc, SCM arg1, SCM args)
{}
#endif


/* Apply a function to a list of arguments.

   This function is exported to the Scheme level as taking two
   required arguments and a tail argument, as if it were:
	(lambda (proc arg1 . args) ...)
   Thus, if you just have a list of arguments to pass to a procedure,
   pass the list as ARG1, and '() for ARGS.  If you have some fixed
   args, pass the first as ARG1, then cons any remaining fixed args
   onto the front of your argument list, and pass that as ARGS.  */

SCM 
SCM_APPLY (SCM proc, SCM arg1, SCM args)
{
#ifdef DEVAL
  scm_t_debug_frame debug;
  scm_t_debug_info debug_vect_body;
  debug.prev = scm_i_last_debug_frame ();
  debug.status = SCM_APPLYFRAME;
  debug.vect = &debug_vect_body;
  debug.vect[0].a.proc = proc;
  debug.vect[0].a.args = SCM_EOL;
  scm_i_set_last_debug_frame (&debug);
#else
  if (scm_debug_mode_p)
    return scm_dapply (proc, arg1, args);
#endif

  SCM_ASRTGO (SCM_NIMP (proc), badproc);

  /* If ARGS is the empty list, then we're calling apply with only two
     arguments --- ARG1 is the list of arguments for PROC.  Whatever
     the case, futz with things so that ARG1 is the first argument to
     give to PROC (or SCM_UNDEFINED if no args), and ARGS contains the
     rest.

     Setting the debug apply frame args this way is pretty messy.
     Perhaps we should store arg1 and args directly in the frame as
     received, and let scm_frame_arguments unpack them, because that's
     a relatively rare operation.  This works for now; if the Guile
     developer archives are still around, see Mikael's post of
     11-Apr-97.  */
  if (scm_is_null (args))
    {
      if (scm_is_null (arg1))
	{
	  arg1 = SCM_UNDEFINED;
#ifdef DEVAL
	  debug.vect[0].a.args = SCM_EOL;
#endif
	}
      else
	{
#ifdef DEVAL
	  debug.vect[0].a.args = arg1;
#endif
	  args = SCM_CDR (arg1);
	  arg1 = SCM_CAR (arg1);
	}
    }
  else
    {
      args = scm_nconc2last (args);
#ifdef DEVAL
      debug.vect[0].a.args = scm_cons (arg1, args);
#endif
    }
#ifdef DEVAL
  if (SCM_ENTER_FRAME_P && SCM_TRAPS_P)
    {
      SCM tmp = scm_make_debugobj (&debug);
      SCM_TRAPS_P = 0;
      scm_call_2 (SCM_ENTER_FRAME_HDLR, scm_sym_enter_frame, tmp);
      SCM_TRAPS_P = 1;
    }
  ENTER_APPLY;
#endif
tail:
  switch (SCM_TYP7 (proc))
    {
    case scm_tc7_subr_2o:
      if (SCM_UNLIKELY (SCM_UNBNDP (arg1)))
	scm_wrong_num_args (proc);
      if (scm_is_null (args))
        args = SCM_UNDEFINED;
      else
        {
          if (SCM_UNLIKELY (!scm_is_null (SCM_CDR (args))))
            scm_wrong_num_args (proc);
          args = SCM_CAR (args);
        }
      RETURN (SCM_SUBRF (proc) (arg1, args));
    case scm_tc7_subr_2:
      if (SCM_UNLIKELY (scm_is_null (args) || !scm_is_null (SCM_CDR (args))))
	scm_wrong_num_args (proc);
      args = SCM_CAR (args);
      RETURN (SCM_SUBRF (proc) (arg1, args));
    case scm_tc7_subr_0:
      if (SCM_UNLIKELY (!SCM_UNBNDP (arg1)))
	scm_wrong_num_args (proc);
      else
	RETURN (SCM_SUBRF (proc) ());
    case scm_tc7_subr_1:
      if (SCM_UNLIKELY (SCM_UNBNDP (arg1)))
	scm_wrong_num_args (proc);
    case scm_tc7_subr_1o:
      if (SCM_UNLIKELY (!scm_is_null (args)))
	scm_wrong_num_args (proc);
      else
	RETURN (SCM_SUBRF (proc) (arg1));
    case scm_tc7_dsubr:
      if (SCM_UNLIKELY (SCM_UNBNDP (arg1) || !scm_is_null (args)))
	scm_wrong_num_args (proc);
      if (SCM_I_INUMP (arg1))
        {
          RETURN (scm_from_double (SCM_DSUBRF (proc) ((double) SCM_I_INUM (arg1))));
        }
      else if (SCM_REALP (arg1))
        {
          RETURN (scm_from_double (SCM_DSUBRF (proc) (SCM_REAL_VALUE (arg1))));
        }
      else if (SCM_BIGP (arg1))
	{
	  RETURN (scm_from_double (SCM_DSUBRF (proc) (scm_i_big2dbl (arg1))));
	}
      else if (SCM_FRACTIONP (arg1))
	{
	  RETURN (scm_from_double (SCM_DSUBRF (proc) (scm_i_fraction2double (arg1))));
	}
      SCM_WTA_DISPATCH_1 (*SCM_SUBR_GENERIC (proc), arg1,
                          SCM_ARG1, scm_i_symbol_chars (SCM_SNAME (proc)));
    case scm_tc7_cxr:
      if (SCM_UNLIKELY (SCM_UNBNDP (arg1) || !scm_is_null (args)))
	scm_wrong_num_args (proc);
      RETURN (scm_i_chase_pairs (arg1, (scm_t_bits) SCM_SUBRF (proc)));
    case scm_tc7_subr_3:
      if (SCM_UNLIKELY (scm_is_null (args)
			|| scm_is_null (SCM_CDR (args))
			|| !scm_is_null (SCM_CDDR (args))))
	scm_wrong_num_args (proc);
      else
	RETURN (SCM_SUBRF (proc) (arg1, SCM_CAR (args), SCM_CADR (args)));
    case scm_tc7_lsubr:
#ifdef DEVAL
      RETURN (SCM_SUBRF (proc) (SCM_UNBNDP (arg1) ? SCM_EOL : debug.vect[0].a.args));
#else
      RETURN (SCM_SUBRF (proc) (SCM_UNBNDP (arg1) ? SCM_EOL : scm_cons (arg1, args)));
#endif
    case scm_tc7_lsubr_2:
      if (SCM_UNLIKELY (!scm_is_pair (args)))
	scm_wrong_num_args (proc);
      else
	RETURN (SCM_SUBRF (proc) (arg1, SCM_CAR (args), SCM_CDR (args)));
    case scm_tc7_asubr:
      if (scm_is_null (args))
	RETURN (SCM_SUBRF (proc) (arg1, SCM_UNDEFINED));
      while (SCM_NIMP (args))
	{
	  SCM_ASSERT (scm_is_pair (args), args, SCM_ARG2, "apply");
	  arg1 = SCM_SUBRF (proc) (arg1, SCM_CAR (args));
	  args = SCM_CDR (args);
	}
      RETURN (arg1);
    case scm_tc7_rpsubr:
      if (scm_is_null (args))
	RETURN (SCM_BOOL_T);
      while (SCM_NIMP (args))
	{
	  SCM_ASSERT (scm_is_pair (args), args, SCM_ARG2, "apply");
	  if (scm_is_false (SCM_SUBRF (proc) (arg1, SCM_CAR (args))))
	    RETURN (SCM_BOOL_F);
	  arg1 = SCM_CAR (args);
	  args = SCM_CDR (args);
	}
      RETURN (SCM_BOOL_T);
    case scm_tcs_closures:
#ifdef DEVAL
      arg1 = (SCM_UNBNDP (arg1) ? SCM_EOL : debug.vect[0].a.args);
#else
      arg1 = (SCM_UNBNDP (arg1) ? SCM_EOL : scm_cons (arg1, args));
#endif
      if (SCM_UNLIKELY (scm_badargsp (SCM_CLOSURE_FORMALS (proc), arg1)))
	scm_wrong_num_args (proc);
      
      /* Copy argument list */
      if (SCM_IMP (arg1))
	args = arg1;
      else
	{
	  SCM tl = args = scm_cons (SCM_CAR (arg1), SCM_UNSPECIFIED);
	  for (arg1 = SCM_CDR (arg1); scm_is_pair (arg1); arg1 = SCM_CDR (arg1))
	    {
	      SCM_SETCDR (tl, scm_cons (SCM_CAR (arg1), SCM_UNSPECIFIED));
	      tl = SCM_CDR (tl);
	    }
	  SCM_SETCDR (tl, arg1);
	}
      
      args = SCM_EXTEND_ENV (SCM_CLOSURE_FORMALS (proc),
                             args,
                             SCM_ENV (proc));
      proc = SCM_CLOSURE_BODY (proc);
    again:
      arg1 = SCM_CDR (proc);
      while (!scm_is_null (arg1))
	{
	  if (SCM_IMP (SCM_CAR (proc)))
	    {
	      if (SCM_ISYMP (SCM_CAR (proc)))
		{
		  scm_dynwind_begin (0);
		  scm_i_dynwind_pthread_mutex_lock (&source_mutex);
		  /* check for race condition */
		  if (SCM_ISYMP (SCM_CAR (proc)))
		    m_expand_body (proc, args);
		  scm_dynwind_end ();
		  goto again;
		}
	      else
		SCM_VALIDATE_NON_EMPTY_COMBINATION (SCM_CAR (proc));
	    }
	  else
	    (void) EVAL (SCM_CAR (proc), args);
	  proc = arg1;
          arg1 = SCM_CDR (proc);
	}
      RETURN (EVALCAR (proc, args));
    case scm_tc7_smob:
      if (!SCM_SMOB_APPLICABLE_P (proc))
	goto badproc;
      if (SCM_UNBNDP (arg1))
	RETURN (SCM_SMOB_APPLY_0 (proc));
      else if (scm_is_null (args))
	RETURN (SCM_SMOB_APPLY_1 (proc, arg1));
      else if (scm_is_null (SCM_CDR (args)))
	RETURN (SCM_SMOB_APPLY_2 (proc, arg1, SCM_CAR (args)));
      else
	RETURN (SCM_SMOB_APPLY_3 (proc, arg1, SCM_CAR (args), SCM_CDR (args)));
    case scm_tc7_cclo:
#ifdef DEVAL
      args = (SCM_UNBNDP(arg1) ? SCM_EOL : debug.vect[0].a.args);
      arg1 = proc;
      proc = SCM_CCLO_SUBR (proc);
      debug.vect[0].a.proc = proc;
      debug.vect[0].a.args = scm_cons (arg1, args);
#else
      args = (SCM_UNBNDP(arg1) ? SCM_EOL : scm_cons (arg1, args));
      arg1 = proc;
      proc = SCM_CCLO_SUBR (proc);
#endif
      goto tail;
    case scm_tc7_pws:
      proc = SCM_PROCEDURE (proc);
#ifdef DEVAL
      debug.vect[0].a.proc = proc;
#endif
      goto tail;
    case scm_tcs_struct:
      if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	{
#ifdef DEVAL
	  args = (SCM_UNBNDP(arg1) ? SCM_EOL : debug.vect[0].a.args);
#else
	  args = (SCM_UNBNDP(arg1) ? SCM_EOL : scm_cons (arg1, args));
#endif
	  RETURN (scm_apply_generic (proc, args));
	}
      else if (SCM_I_OPERATORP (proc))
	{
	  /* operator */
#ifdef DEVAL
	  args = (SCM_UNBNDP(arg1) ? SCM_EOL : debug.vect[0].a.args);
#else
	  args = (SCM_UNBNDP(arg1) ? SCM_EOL : scm_cons (arg1, args));
#endif
	  arg1 = proc;
	  proc = (SCM_I_ENTITYP (proc)
		  ? SCM_ENTITY_PROCEDURE (proc)
		  : SCM_OPERATOR_PROCEDURE (proc));
#ifdef DEVAL
	  debug.vect[0].a.proc = proc;
	  debug.vect[0].a.args = scm_cons (arg1, args);
#endif
	  if (SCM_NIMP (proc))
	    goto tail;
	  else
	    goto badproc;
	}
      else
        goto badproc;
    default:
    badproc:
      scm_wrong_type_arg ("apply", SCM_ARG1, proc);
    }
#ifdef DEVAL
exit:
  if (scm_check_exit_p && SCM_TRAPS_P)
    if (SCM_EXIT_FRAME_P || (SCM_TRACE_P && SCM_TRACED_FRAME_P (debug)))
      {
	SCM_CLEAR_TRACED_FRAME (debug);
	arg1 = scm_make_debugobj (&debug);
	SCM_TRAPS_P = 0;
	arg1 = scm_call_3 (SCM_EXIT_FRAME_HDLR, scm_sym_exit_frame, arg1, proc);
	SCM_TRAPS_P = 1;
	if (scm_is_pair (arg1) && scm_is_eq (SCM_CAR (arg1), sym_instead))
	  proc = SCM_CDR (arg1);
      }
  scm_i_set_last_debug_frame (debug.prev);
  return proc;
#endif
}


/* SECTION: The rest of this file is only read once.
 */

#ifndef DEVAL

/* Trampolines
 *  
 * Trampolines make it possible to move procedure application dispatch
 * outside inner loops.  The motivation was clean implementation of
 * efficient replacements of R5RS primitives in SRFI-1.
 *
 * The semantics is clear: scm_trampoline_N returns an optimized
 * version of scm_call_N (or NULL if the procedure isn't applicable
 * on N args).
 *
 * Applying the optimization to map and for-each increased efficiency
 * noticeably.  For example, (map abs ls) is now 8 times faster than
 * before.
 */

static SCM
call_subr0_0 (SCM proc)
{
  return SCM_SUBRF (proc) ();
}

static SCM
call_subr1o_0 (SCM proc)
{
  return SCM_SUBRF (proc) (SCM_UNDEFINED);
}

static SCM
call_lsubr_0 (SCM proc)
{
  return SCM_SUBRF (proc) (SCM_EOL);
}

SCM 
scm_i_call_closure_0 (SCM proc)
{
  const SCM env = SCM_EXTEND_ENV (SCM_CLOSURE_FORMALS (proc),
                                  SCM_EOL,
                                  SCM_ENV (proc));
  const SCM result = scm_eval_body (SCM_CLOSURE_BODY (proc), env);
  return result;
}

scm_t_trampoline_0
scm_trampoline_0 (SCM proc)
{
  scm_t_trampoline_0 trampoline;

  if (SCM_IMP (proc))
    return NULL;

  switch (SCM_TYP7 (proc))
    {
    case scm_tc7_subr_0:
      trampoline = call_subr0_0;
      break;
    case scm_tc7_subr_1o:
      trampoline = call_subr1o_0;
      break;
    case scm_tc7_lsubr:
      trampoline = call_lsubr_0;
      break;
    case scm_tcs_closures:
      {
	SCM formals = SCM_CLOSURE_FORMALS (proc);
	if (scm_is_null (formals) || !scm_is_pair (formals))
	  trampoline = scm_i_call_closure_0;
	else
	  return NULL;
        break;
      }
    case scm_tcs_struct:
      if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	trampoline = scm_call_generic_0;
      else if (SCM_I_OPERATORP (proc))
        trampoline = scm_call_0;
      else
        return NULL;
      break;
    case scm_tc7_smob:
      if (SCM_SMOB_APPLICABLE_P (proc))
	trampoline = SCM_SMOB_DESCRIPTOR (proc).apply_0;
      else
	return NULL;
      break;
    case scm_tc7_asubr:
    case scm_tc7_rpsubr:
    case scm_tc7_cclo:
    case scm_tc7_pws:
      trampoline = scm_call_0;
      break;
    default:
      return NULL; /* not applicable on zero arguments */
    }
  /* We only reach this point if a valid trampoline was determined.  */

  /* If debugging is enabled, we want to see all calls to proc on the stack.
   * Thus, we replace the trampoline shortcut with scm_call_0.  */
  if (scm_debug_mode_p)
    return scm_call_0;
  else
    return trampoline;
}

static SCM
call_subr1_1 (SCM proc, SCM arg1)
{
  return SCM_SUBRF (proc) (arg1);
}

static SCM
call_subr2o_1 (SCM proc, SCM arg1)
{
  return SCM_SUBRF (proc) (arg1, SCM_UNDEFINED);
}

static SCM
call_lsubr_1 (SCM proc, SCM arg1)
{
  return SCM_SUBRF (proc) (scm_list_1 (arg1));
}

static SCM
call_dsubr_1 (SCM proc, SCM arg1)
{
  if (SCM_I_INUMP (arg1))
    {
      RETURN (scm_from_double (SCM_DSUBRF (proc) ((double) SCM_I_INUM (arg1))));
    }
  else if (SCM_REALP (arg1))
    {
      RETURN (scm_from_double (SCM_DSUBRF (proc) (SCM_REAL_VALUE (arg1))));
    }
  else if (SCM_BIGP (arg1))
    {
      RETURN (scm_from_double (SCM_DSUBRF (proc) (scm_i_big2dbl (arg1))));
    }
  else if (SCM_FRACTIONP (arg1))
    {
      RETURN (scm_from_double (SCM_DSUBRF (proc) (scm_i_fraction2double (arg1))));
    }
  SCM_WTA_DISPATCH_1 (*SCM_SUBR_GENERIC (proc), arg1,
		      SCM_ARG1, scm_i_symbol_chars (SCM_SNAME (proc)));
}

static SCM
call_cxr_1 (SCM proc, SCM arg1)
{
  return scm_i_chase_pairs (arg1, (scm_t_bits) SCM_SUBRF (proc));
}

static SCM 
call_closure_1 (SCM proc, SCM arg1)
{
  const SCM env = SCM_EXTEND_ENV (SCM_CLOSURE_FORMALS (proc),
                                  scm_list_1 (arg1),
                                  SCM_ENV (proc));
  const SCM result = scm_eval_body (SCM_CLOSURE_BODY (proc), env);
  return result;
}

scm_t_trampoline_1
scm_trampoline_1 (SCM proc)
{
  scm_t_trampoline_1 trampoline;

  if (SCM_IMP (proc))
    return NULL;

  switch (SCM_TYP7 (proc))
    {
    case scm_tc7_subr_1:
    case scm_tc7_subr_1o:
      trampoline = call_subr1_1;
      break;
    case scm_tc7_subr_2o:
      trampoline = call_subr2o_1;
      break;
    case scm_tc7_lsubr:
      trampoline = call_lsubr_1;
      break;
    case scm_tc7_dsubr:
      trampoline = call_dsubr_1;
      break;
    case scm_tc7_cxr:
      trampoline = call_cxr_1;
      break;
    case scm_tcs_closures:
      {
	SCM formals = SCM_CLOSURE_FORMALS (proc);
	if (!scm_is_null (formals)
	    && (!scm_is_pair (formals) || !scm_is_pair (SCM_CDR (formals))))
	  trampoline = call_closure_1;
	else
	  return NULL;
        break;
      }
    case scm_tcs_struct:
      if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	trampoline = scm_call_generic_1;
      else if (SCM_I_OPERATORP (proc))
        trampoline = scm_call_1;
      else
        return NULL;
      break;
    case scm_tc7_smob:
      if (SCM_SMOB_APPLICABLE_P (proc))
	trampoline = SCM_SMOB_DESCRIPTOR (proc).apply_1;
      else
	return NULL;
      break;
    case scm_tc7_asubr:
    case scm_tc7_rpsubr:
    case scm_tc7_cclo:
    case scm_tc7_pws:
      trampoline = scm_call_1;
      break;
    default:
      return NULL; /* not applicable on one arg */
    }
  /* We only reach this point if a valid trampoline was determined.  */

  /* If debugging is enabled, we want to see all calls to proc on the stack.
   * Thus, we replace the trampoline shortcut with scm_call_1.  */
  if (scm_debug_mode_p)
    return scm_call_1;
  else
    return trampoline;
}

static SCM
call_subr2_2 (SCM proc, SCM arg1, SCM arg2)
{
  return SCM_SUBRF (proc) (arg1, arg2);
}

static SCM
call_lsubr2_2 (SCM proc, SCM arg1, SCM arg2)
{
  return SCM_SUBRF (proc) (arg1, arg2, SCM_EOL);
}

static SCM
call_lsubr_2 (SCM proc, SCM arg1, SCM arg2)
{
  return SCM_SUBRF (proc) (scm_list_2 (arg1, arg2));
}

static SCM 
call_closure_2 (SCM proc, SCM arg1, SCM arg2)
{
  const SCM env = SCM_EXTEND_ENV (SCM_CLOSURE_FORMALS (proc),
                                  scm_list_2 (arg1, arg2),
                                  SCM_ENV (proc));
  const SCM result = scm_eval_body (SCM_CLOSURE_BODY (proc), env);
  return result;
}

scm_t_trampoline_2
scm_trampoline_2 (SCM proc)
{
  scm_t_trampoline_2 trampoline;

  if (SCM_IMP (proc))
    return NULL;

  switch (SCM_TYP7 (proc))
    {
    case scm_tc7_subr_2:
    case scm_tc7_subr_2o:
    case scm_tc7_rpsubr:
    case scm_tc7_asubr:
      trampoline = call_subr2_2;
      break;
    case scm_tc7_lsubr_2:
      trampoline = call_lsubr2_2;
      break;
    case scm_tc7_lsubr:
      trampoline = call_lsubr_2;
      break;
    case scm_tcs_closures:
      {
	SCM formals = SCM_CLOSURE_FORMALS (proc);
	if (!scm_is_null (formals)
	    && (!scm_is_pair (formals)
		|| (!scm_is_null (SCM_CDR (formals))
		    && (!scm_is_pair (SCM_CDR (formals))
			|| !scm_is_pair (SCM_CDDR (formals))))))
	  trampoline = call_closure_2;
	else
	  return NULL;
        break;
      }
    case scm_tcs_struct:
      if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	trampoline = scm_call_generic_2;
      else if (SCM_I_OPERATORP (proc))
        trampoline = scm_call_2;
      else
        return NULL;
      break;
    case scm_tc7_smob:
      if (SCM_SMOB_APPLICABLE_P (proc))
	trampoline = SCM_SMOB_DESCRIPTOR (proc).apply_2;
      else
	return NULL;
      break;
    case scm_tc7_cclo:
    case scm_tc7_pws:
      trampoline = scm_call_2;
      break;
    default:
      return NULL; /* not applicable on two args */
    }
  /* We only reach this point if a valid trampoline was determined.  */

  /* If debugging is enabled, we want to see all calls to proc on the stack.
   * Thus, we replace the trampoline shortcut with scm_call_2.  */
  if (scm_debug_mode_p)
    return scm_call_2;
  else
    return trampoline;
}

/* Typechecking for multi-argument MAP and FOR-EACH.

   Verify that each element of the vector ARGV, except for the first,
   is a proper list whose length is LEN.  Attribute errors to WHO,
   and claim that the i'th element of ARGV is WHO's i+2'th argument.  */
static inline void
check_map_args (SCM argv,
		long len,
		SCM gf,
		SCM proc,
		SCM args,
		const char *who)
{
  long i;

  for (i = SCM_SIMPLE_VECTOR_LENGTH (argv) - 1; i >= 1; i--)
    {
      SCM elt = SCM_SIMPLE_VECTOR_REF (argv, i);
      long elt_len = scm_ilength (elt);

      if (elt_len < 0)
	{
	  if (gf)
	    scm_apply_generic (gf, scm_cons (proc, args));
	  else
	    scm_wrong_type_arg (who, i + 2, elt);
	}

      if (elt_len != len)
	scm_out_of_range_pos (who, elt, scm_from_long (i + 2));
    }
}


SCM_GPROC (s_map, "map", 2, 0, 1, scm_map, g_map);

/* Note: Currently, scm_map applies PROC to the argument list(s)
   sequentially, starting with the first element(s).  This is used in
   evalext.c where the Scheme procedure `map-in-order', which guarantees
   sequential behaviour, is implemented using scm_map.  If the
   behaviour changes, we need to update `map-in-order'.
*/

SCM 
scm_map (SCM proc, SCM arg1, SCM args)
#define FUNC_NAME s_map
{
  long i, len;
  SCM res = SCM_EOL;
  SCM *pres = &res;

  len = scm_ilength (arg1);
  SCM_GASSERTn (len >= 0,
		g_map, scm_cons2 (proc, arg1, args), SCM_ARG2, s_map);
  SCM_VALIDATE_REST_ARGUMENT (args);
  if (scm_is_null (args))
    {
      scm_t_trampoline_1 call = scm_trampoline_1 (proc);
      SCM_GASSERT2 (call, g_map, proc, arg1, SCM_ARG1, s_map);
      while (SCM_NIMP (arg1))
	{
	  *pres = scm_list_1 (call (proc, SCM_CAR (arg1)));
	  pres = SCM_CDRLOC (*pres);
	  arg1 = SCM_CDR (arg1);
	}
      return res;
    }
  if (scm_is_null (SCM_CDR (args)))
    {
      SCM arg2 = SCM_CAR (args);
      int len2 = scm_ilength (arg2);
      scm_t_trampoline_2 call = scm_trampoline_2 (proc);
      SCM_GASSERTn (call,
		    g_map, scm_cons2 (proc, arg1, args), SCM_ARG1, s_map);
      SCM_GASSERTn (len2 >= 0,
		    g_map, scm_cons2 (proc, arg1, args), SCM_ARG3, s_map);
      if (len2 != len)
	SCM_OUT_OF_RANGE (3, arg2);
      while (SCM_NIMP (arg1))
	{
	  *pres = scm_list_1 (call (proc, SCM_CAR (arg1), SCM_CAR (arg2)));
	  pres = SCM_CDRLOC (*pres);
	  arg1 = SCM_CDR (arg1);
	  arg2 = SCM_CDR (arg2);
	}
      return res;
    }
  arg1 = scm_cons (arg1, args);
  args = scm_vector (arg1);
  check_map_args (args, len, g_map, proc, arg1, s_map);
  while (1)
    {
      arg1 = SCM_EOL;
      for (i = SCM_SIMPLE_VECTOR_LENGTH (args) - 1; i >= 0; i--)
	{
	  SCM elt = SCM_SIMPLE_VECTOR_REF (args, i);
	  if (SCM_IMP (elt)) 
	    return res;
	  arg1 = scm_cons (SCM_CAR (elt), arg1);
	  SCM_SIMPLE_VECTOR_SET (args, i, SCM_CDR (elt));
	}
      *pres = scm_list_1 (scm_apply (proc, arg1, SCM_EOL));
      pres = SCM_CDRLOC (*pres);
    }
}
#undef FUNC_NAME


SCM_GPROC (s_for_each, "for-each", 2, 0, 1, scm_for_each, g_for_each);

SCM 
scm_for_each (SCM proc, SCM arg1, SCM args)
#define FUNC_NAME s_for_each
{
  long i, len;
  len = scm_ilength (arg1);
  SCM_GASSERTn (len >= 0, g_for_each, scm_cons2 (proc, arg1, args),
		SCM_ARG2, s_for_each);
  SCM_VALIDATE_REST_ARGUMENT (args);
  if (scm_is_null (args))
    {
      scm_t_trampoline_1 call = scm_trampoline_1 (proc);
      SCM_GASSERT2 (call, g_for_each, proc, arg1, SCM_ARG1, s_for_each);
      while (SCM_NIMP (arg1))
	{
	  call (proc, SCM_CAR (arg1));
	  arg1 = SCM_CDR (arg1);
	}
      return SCM_UNSPECIFIED;
    }
  if (scm_is_null (SCM_CDR (args)))
    {
      SCM arg2 = SCM_CAR (args);
      int len2 = scm_ilength (arg2);
      scm_t_trampoline_2 call = scm_trampoline_2 (proc);
      SCM_GASSERTn (call, g_for_each,
		    scm_cons2 (proc, arg1, args), SCM_ARG1, s_for_each);
      SCM_GASSERTn (len2 >= 0, g_for_each,
		    scm_cons2 (proc, arg1, args), SCM_ARG3, s_for_each);
      if (len2 != len)
	SCM_OUT_OF_RANGE (3, arg2);
      while (SCM_NIMP (arg1))
	{
	  call (proc, SCM_CAR (arg1), SCM_CAR (arg2));
	  arg1 = SCM_CDR (arg1);
	  arg2 = SCM_CDR (arg2);
	}
      return SCM_UNSPECIFIED;
    }
  arg1 = scm_cons (arg1, args);
  args = scm_vector (arg1);
  check_map_args (args, len, g_for_each, proc, arg1, s_for_each);
  while (1)
    {
      arg1 = SCM_EOL;
      for (i = SCM_SIMPLE_VECTOR_LENGTH (args) - 1; i >= 0; i--)
	{
	  SCM elt = SCM_SIMPLE_VECTOR_REF (args, i);
	  if (SCM_IMP (elt))
	    return SCM_UNSPECIFIED;
	  arg1 = scm_cons (SCM_CAR (elt), arg1);
	  SCM_SIMPLE_VECTOR_SET (args, i, SCM_CDR (elt));
	}
      scm_apply (proc, arg1, SCM_EOL);
    }
}
#undef FUNC_NAME


SCM 
scm_closure (SCM code, SCM env)
{
  SCM z;
  SCM closcar = scm_cons (code, SCM_EOL);
  z = scm_cell (SCM_UNPACK (closcar) + scm_tc3_closure, (scm_t_bits) env);
  scm_remember_upto_here (closcar);
  return z;
}


scm_t_bits scm_tc16_promise;

SCM 
scm_makprom (SCM code)
{
  SCM_RETURN_NEWSMOB2 (scm_tc16_promise,
		       SCM_UNPACK (code),
		       scm_make_recursive_mutex ());
}

static SCM
promise_mark (SCM promise)
{
  scm_gc_mark (SCM_PROMISE_MUTEX (promise));
  return SCM_PROMISE_DATA (promise);
}

static size_t
promise_free (SCM promise)
{
  return 0;
}

static int 
promise_print (SCM exp, SCM port, scm_print_state *pstate)
{
  int writingp = SCM_WRITINGP (pstate);
  scm_puts ("#<promise ", port);
  SCM_SET_WRITINGP (pstate, 1);
  scm_iprin1 (SCM_PROMISE_DATA (exp), port, pstate);
  SCM_SET_WRITINGP (pstate, writingp);
  scm_putc ('>', port);
  return !0;
}

SCM_DEFINE (scm_force, "force", 1, 0, 0, 
	    (SCM promise),
	    "If the promise @var{x} has not been computed yet, compute and\n"
	    "return @var{x}, otherwise just return the previously computed\n"
	    "value.")
#define FUNC_NAME s_scm_force
{
  SCM_VALIDATE_SMOB (1, promise, promise);
  scm_lock_mutex (SCM_PROMISE_MUTEX (promise));
  if (!SCM_PROMISE_COMPUTED_P (promise))
    {
      SCM ans = scm_call_0 (SCM_PROMISE_DATA (promise));
      if (!SCM_PROMISE_COMPUTED_P (promise))
	{
	  SCM_SET_PROMISE_DATA (promise, ans);
	  SCM_SET_PROMISE_COMPUTED (promise);
	}
    }
  scm_unlock_mutex (SCM_PROMISE_MUTEX (promise));
  return SCM_PROMISE_DATA (promise);
}
#undef FUNC_NAME


SCM_DEFINE (scm_promise_p, "promise?", 1, 0, 0, 
            (SCM obj),
	    "Return true if @var{obj} is a promise, i.e. a delayed computation\n"
	    "(@pxref{Delayed evaluation,,,r5rs.info,The Revised^5 Report on Scheme}).")
#define FUNC_NAME s_scm_promise_p
{
  return scm_from_bool (SCM_TYP16_PREDICATE (scm_tc16_promise, obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_cons_source, "cons-source", 3, 0, 0, 
            (SCM xorig, SCM x, SCM y),
	    "Create and return a new pair whose car and cdr are @var{x} and @var{y}.\n"
	    "Any source properties associated with @var{xorig} are also associated\n"
	    "with the new pair.")
#define FUNC_NAME s_scm_cons_source
{
  SCM p, z;
  z = scm_cons (x, y);
  /* Copy source properties possibly associated with xorig. */
  p = scm_whash_lookup (scm_source_whash, xorig);
  if (scm_is_true (p))
    scm_whash_insert (scm_source_whash, z, p);
  return z;
}
#undef FUNC_NAME


/* The function scm_copy_tree is used to copy an expression tree to allow the
 * memoizer to modify the expression during memoization.  scm_copy_tree
 * creates deep copies of pairs and vectors, but not of any other data types,
 * since only pairs and vectors will be parsed by the memoizer.
 *
 * To avoid infinite recursion due to cyclic structures, the hare-and-tortoise
 * pattern is used to detect cycles.  In fact, the pattern is used in two
 * dimensions, vertical (indicated in the code by the variable names 'hare'
 * and 'tortoise') and horizontal ('rabbit' and 'turtle').  In both
 * dimensions, the hare/rabbit will take two steps when the tortoise/turtle
 * takes one.
 *
 * The vertical dimension corresponds to recursive calls to function
 * copy_tree: This happens when descending into vector elements, into cars of
 * lists and into the cdr of an improper list.  In this dimension, the
 * tortoise follows the hare by using the processor stack: Every stack frame
 * will hold an instance of struct t_trace.  These instances are connected in
 * a way that represents the trace of the hare, which thus can be followed by
 * the tortoise.  The tortoise will always point to struct t_trace instances
 * relating to SCM objects that have already been copied.  Thus, a cycle is
 * detected if the tortoise and the hare point to the same object,
 *
 * The horizontal dimension is within one execution of copy_tree, when the
 * function cdr's along the pairs of a list.  This is the standard
 * hare-and-tortoise implementation, found several times in guile.  */

struct t_trace {
  struct t_trace *trace; /* These pointers form a trace along the stack. */
  SCM obj;               /* The object handled at the respective stack frame.*/
};

static SCM
copy_tree (
  struct t_trace *const hare,
  struct t_trace *tortoise,
  unsigned int tortoise_delay )
{
  if (!scm_is_pair (hare->obj) && !scm_is_simple_vector (hare->obj))
    {
      return hare->obj;
    }
  else
    {
      /* Prepare the trace along the stack.  */
      struct t_trace new_hare;
      hare->trace = &new_hare;

      /* The tortoise will make its step after the delay has elapsed.  Note
       * that in contrast to the typical hare-and-tortoise pattern, the step
       * of the tortoise happens before the hare takes its steps.  This is, in
       * principle, no problem, except for the start of the algorithm: Then,
       * it has to be made sure that the hare actually gets its advantage of
       * two steps.  */
      if (tortoise_delay == 0)
        {
          tortoise_delay = 1;
          tortoise = tortoise->trace;
          ASSERT_SYNTAX (!scm_is_eq (hare->obj, tortoise->obj),
                         s_bad_expression, hare->obj);
        }
      else
        {
          --tortoise_delay;
        }

      if (scm_is_simple_vector (hare->obj))
        {
          size_t length = SCM_SIMPLE_VECTOR_LENGTH (hare->obj);
          SCM new_vector = scm_c_make_vector (length, SCM_UNSPECIFIED);

          /* Each vector element is copied by recursing into copy_tree, having
           * the tortoise follow the hare into the depths of the stack.  */
          unsigned long int i;
          for (i = 0; i < length; ++i)
            {
              SCM new_element;
              new_hare.obj = SCM_SIMPLE_VECTOR_REF (hare->obj, i);
              new_element = copy_tree (&new_hare, tortoise, tortoise_delay);
              SCM_SIMPLE_VECTOR_SET (new_vector, i, new_element);
            }

          return new_vector;
        }
      else /* scm_is_pair (hare->obj) */
        {
          SCM result;
          SCM tail;

          SCM rabbit = hare->obj;
          SCM turtle = hare->obj;

          SCM copy;

          /* The first pair of the list is treated specially, in order to
           * preserve a potential source code position.  */
          result = tail = scm_cons_source (rabbit, SCM_EOL, SCM_EOL);
          new_hare.obj = SCM_CAR (rabbit);
          copy = copy_tree (&new_hare, tortoise, tortoise_delay);
          SCM_SETCAR (tail, copy);

          /* The remaining pairs of the list are copied by, horizontally,
           * having the turtle follow the rabbit, and, vertically, having the
           * tortoise follow the hare into the depths of the stack.  */
          rabbit = SCM_CDR (rabbit);
          while (scm_is_pair (rabbit))
            {
              new_hare.obj = SCM_CAR (rabbit);
              copy = copy_tree (&new_hare, tortoise, tortoise_delay);
              SCM_SETCDR (tail, scm_cons (copy, SCM_UNDEFINED));
              tail = SCM_CDR (tail);

              rabbit = SCM_CDR (rabbit);
              if (scm_is_pair (rabbit))
                {
                  new_hare.obj = SCM_CAR (rabbit);
                  copy = copy_tree (&new_hare, tortoise, tortoise_delay);
                  SCM_SETCDR (tail, scm_cons (copy, SCM_UNDEFINED));
                  tail = SCM_CDR (tail);
                  rabbit = SCM_CDR (rabbit);

                  turtle = SCM_CDR (turtle);
                  ASSERT_SYNTAX (!scm_is_eq (rabbit, turtle),
                                 s_bad_expression, rabbit);
                }
            }

          /* We have to recurse into copy_tree again for the last cdr, in
           * order to handle the situation that it holds a vector.  */
          new_hare.obj = rabbit;
          copy = copy_tree (&new_hare, tortoise, tortoise_delay);
          SCM_SETCDR (tail, copy);

          return result;
        }
    }
}

SCM_DEFINE (scm_copy_tree, "copy-tree", 1, 0, 0, 
            (SCM obj),
	    "Recursively copy the data tree that is bound to @var{obj}, and return a\n"
	    "the new data structure.  @code{copy-tree} recurses down the\n"
	    "contents of both pairs and vectors (since both cons cells and vector\n"
	    "cells may point to arbitrary objects), and stops recursing when it hits\n"
	    "any other object.")
#define FUNC_NAME s_scm_copy_tree
{
  /* Prepare the trace along the stack.  */
  struct t_trace trace;
  trace.obj = obj;

  /* In function copy_tree, if the tortoise makes its step, it will do this
   * before the hare has the chance to move.  Thus, we have to make sure that
   * the very first step of the tortoise will not happen after the hare has
   * really made two steps.  This is achieved by passing '2' as the initial
   * delay for the tortoise.  NOTE: Since cycles are unlikely, giving the hare
   * a bigger advantage may improve performance slightly.  */
  return copy_tree (&trace, &trace, 2);
}
#undef FUNC_NAME


/* We have three levels of EVAL here:

   - scm_i_eval (exp, env)

     evaluates EXP in environment ENV.  ENV is a lexical environment
     structure as used by the actual tree code evaluator.  When ENV is
     a top-level environment, then changes to the current module are
     tracked by updating ENV so that it continues to be in sync with
     the current module.

   - scm_primitive_eval (exp)

     evaluates EXP in the top-level environment as determined by the
     current module.  This is done by constructing a suitable
     environment and calling scm_i_eval.  Thus, changes to the
     top-level module are tracked normally.

   - scm_eval (exp, mod_or_state)

     evaluates EXP while MOD_OR_STATE is the current module or current
     dynamic state (as appropriate).  This is done by setting the
     current module (or dynamic state) to MOD_OR_STATE, invoking
     scm_primitive_eval on EXP, and then restoring the current module
     (or dynamic state) to the value it had previously.  That is,
     while EXP is evaluated, changes to the current module (or dynamic
     state) are tracked, but these changes do not persist when
     scm_eval returns.

  For each level of evals, there are two variants, distinguished by a
  _x suffix: the ordinary variant does not modify EXP while the _x
  variant can destructively modify EXP into something completely
  unintelligible.  A Scheme data structure passed as EXP to one of the
  _x variants should not ever be used again for anything.  So when in
  doubt, use the ordinary variant.

*/

SCM 
scm_i_eval_x (SCM exp, SCM env)
{
  if (scm_is_symbol (exp))
    return *scm_lookupcar (scm_cons (exp, SCM_UNDEFINED), env, 1);
  else
    return SCM_I_XEVAL (exp, env);
}

SCM 
scm_i_eval (SCM exp, SCM env)
{
  exp = scm_copy_tree (exp);
  if (scm_is_symbol (exp))
    return *scm_lookupcar (scm_cons (exp, SCM_UNDEFINED), env, 1);
  else
    return SCM_I_XEVAL (exp, env);
}

SCM
scm_primitive_eval_x (SCM exp)
{
  SCM env;
  SCM transformer = scm_current_module_transformer ();
  if (SCM_NIMP (transformer))
    exp = scm_call_1 (transformer, exp);
  env = scm_top_level_env (scm_current_module_lookup_closure ());
  return scm_i_eval_x (exp, env);
}

SCM_DEFINE (scm_primitive_eval, "primitive-eval", 1, 0, 0,
	    (SCM exp),
	    "Evaluate @var{exp} in the top-level environment specified by\n"
	    "the current module.")
#define FUNC_NAME s_scm_primitive_eval
{
  SCM env;
  SCM transformer = scm_current_module_transformer ();
  if (scm_is_true (transformer))
    exp = scm_call_1 (transformer, exp);
  env = scm_top_level_env (scm_current_module_lookup_closure ());
  return scm_i_eval (exp, env);
}
#undef FUNC_NAME


/* Eval does not take the second arg optionally.  This is intentional
 * in order to be R5RS compatible, and to prepare for the new module
 * system, where we would like to make the choice of evaluation
 * environment explicit.  */

SCM
scm_eval_x (SCM exp, SCM module_or_state)
{
  SCM res;

  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  if (scm_is_dynamic_state (module_or_state))
    scm_dynwind_current_dynamic_state (module_or_state);
  else
    scm_dynwind_current_module (module_or_state);

  res = scm_primitive_eval_x (exp);

  scm_dynwind_end ();
  return res;
}

SCM_DEFINE (scm_eval, "eval", 2, 0, 0, 
	    (SCM exp, SCM module_or_state),
	    "Evaluate @var{exp}, a list representing a Scheme expression,\n"
            "in the top-level environment specified by\n"
	    "@var{module_or_state}.\n"
            "While @var{exp} is evaluated (using @code{primitive-eval}),\n"
            "@var{module_or_state} is made the current module when\n"
	    "it is a module, or the current dynamic state when it is\n"
	    "a dynamic state."
	    "Example: (eval '(+ 1 2) (interaction-environment))")
#define FUNC_NAME s_scm_eval
{
  SCM res;

  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  if (scm_is_dynamic_state (module_or_state))
    scm_dynwind_current_dynamic_state (module_or_state);
  else
    {
      SCM_VALIDATE_MODULE (2, module_or_state);
      scm_dynwind_current_module (module_or_state);
    }

  res = scm_primitive_eval (exp);

  scm_dynwind_end ();
  return res;
}
#undef FUNC_NAME


/* At this point, deval and scm_dapply are generated.
 */

#define DEVAL
#include "eval.c"


#if (SCM_ENABLE_DEPRECATED == 1)

/* Deprecated in guile 1.7.0 on 2004-03-29.  */
SCM scm_ceval (SCM x, SCM env)
{
  if (scm_is_pair (x))
    return ceval (x, env);
  else if (scm_is_symbol (x))
    return *scm_lookupcar (scm_cons (x, SCM_UNDEFINED), env, 1);
  else
    return SCM_I_XEVAL (x, env);
}

/* Deprecated in guile 1.7.0 on 2004-03-29.  */
SCM scm_deval (SCM x, SCM env)
{
  if (scm_is_pair (x))
    return deval (x, env);
  else if (scm_is_symbol (x))
    return *scm_lookupcar (scm_cons (x, SCM_UNDEFINED), env, 1);
  else
    return SCM_I_XEVAL (x, env);
}

static SCM
dispatching_eval (SCM x, SCM env)
{
  if (scm_debug_mode_p)
    return scm_deval (x, env);
  else
    return scm_ceval (x, env);
}

/* Deprecated in guile 1.7.0 on 2004-03-29.  */
SCM (*scm_ceval_ptr) (SCM x, SCM env) = dispatching_eval;

#endif


void 
scm_init_eval ()
{
  scm_i_pthread_mutex_init (&source_mutex,
			    scm_i_pthread_mutexattr_recursive);

  scm_init_opts (scm_evaluator_traps,
		 scm_evaluator_trap_table,
		 SCM_N_EVALUATOR_TRAPS);
  scm_init_opts (scm_eval_options_interface,
		 scm_eval_opts,
		 SCM_N_EVAL_OPTIONS);
  
  scm_tc16_promise = scm_make_smob_type ("promise", 0);
  scm_set_smob_mark (scm_tc16_promise, promise_mark);
  scm_set_smob_free (scm_tc16_promise, promise_free);
  scm_set_smob_print (scm_tc16_promise, promise_print);

  undefineds = scm_list_1 (SCM_UNDEFINED);
  SCM_SETCDR (undefineds, undefineds);
  scm_permanent_object (undefineds);

  scm_listofnull = scm_list_1 (SCM_EOL);

  f_apply = scm_c_define_subr ("apply", scm_tc7_lsubr_2, scm_apply);
  scm_permanent_object (f_apply);

#include "libguile/eval.x"

  scm_add_feature ("delay");
}

#endif /* !DEVAL */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
