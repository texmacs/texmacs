/* Copyright (C) 1995-1999,2000,2001, 2002, 2003, 2004, 2006, 2008 Free Software Foundation, Inc.
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

#include <errno.h>

#include "libguile/_scm.h"
#include "libguile/chars.h"
#include "libguile/continuations.h"
#include "libguile/smob.h"
#include "libguile/eval.h"
#include "libguile/macros.h"
#include "libguile/procprop.h"
#include "libguile/read.h"
#include "libguile/weaks.h"
#include "libguile/unif.h"
#include "libguile/alist.h"
#include "libguile/struct.h"
#include "libguile/objects.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/strports.h"
#include "libguile/vectors.h"
#include "libguile/lang.h"
#include "libguile/numbers.h"

#include "libguile/validate.h"
#include "libguile/print.h"


/* {Names of immediate symbols}
 * 
 * This table must agree with the declarations in scm.h: {Immediate Symbols}.
 */

/* This table must agree with the list of flags in tags.h.  */
static const char *iflagnames[] =
{
  "#f",
  "#t",
  "#<undefined>",
  "#<eof>",
  "()",
  "#<unspecified>",

  /* Unbound slot marker for GOOPS.  For internal use in GOOPS only.  */
  "#<unbound>",

  /* Elisp nil value.  This is its Scheme name; whenever it's printed in
   * Elisp, it should appear as the symbol `nil'.  */
  "#nil"
};

SCM_SYMBOL (sym_reader, "reader");

scm_t_option scm_print_opts[] = {
  { SCM_OPTION_SCM, "closure-hook", SCM_UNPACK (SCM_BOOL_F),
    "Hook for printing closures (should handle macros as well)." },
  { SCM_OPTION_BOOLEAN, "source", 0,
    "Print closures with source." },
  { SCM_OPTION_SCM, "highlight-prefix", (unsigned long)SCM_BOOL_F,
    "The string to print before highlighted values." },
  { SCM_OPTION_SCM, "highlight-suffix", (unsigned long)SCM_BOOL_F,
    "The string to print after highlighted values." },
  { SCM_OPTION_SCM, "quote-keywordish-symbols", (unsigned long)SCM_BOOL_F,
    "How to print symbols that have a colon as their first or last character. "
    "The value '#f' does not quote the colons; '#t' quotes them; "
    "'reader' quotes them when the reader option 'keywords' is not '#f'." 
  }
};

SCM_DEFINE (scm_print_options, "print-options-interface", 0, 1, 0, 
            (SCM setting),
	    "Option interface for the print options. Instead of using\n"
	    "this procedure directly, use the procedures\n"
	    "@code{print-enable}, @code{print-disable}, @code{print-set!}\n"
	    "and @code{print-options}.")
#define FUNC_NAME s_scm_print_options
{
  SCM ans = scm_options (setting,
			 scm_print_opts,
			 SCM_N_PRINT_OPTIONS,
			 FUNC_NAME);
  return ans;
}
#undef FUNC_NAME


/* {Printing of Scheme Objects}
 */

/* Detection of circular references.
 *
 * Due to other constraints in the implementation, this code has bad
 * time complexity (O (depth * N)), The printer code can be
 * rewritten to be O(N).
 */
#define PUSH_REF(pstate, obj)			\
do						\
{						\
  PSTATE_STACK_SET (pstate, pstate->top, obj);	\
  pstate->top++;				\
  if (pstate->top == pstate->ceiling)		\
    grow_ref_stack (pstate);			\
} while(0)

#define ENTER_NESTED_DATA(pstate, obj, label)			\
do								\
{								\
  register unsigned long i;					\
  for (i = 0; i < pstate->top; ++i)				\
    if (scm_is_eq (PSTATE_STACK_REF (pstate, i), (obj)))	\
      goto label;						\
  if (pstate->fancyp)						\
    {								\
      if (pstate->top - pstate->list_offset >= pstate->level)	\
	{							\
	  scm_putc ('#', port);					\
	  return;						\
	}							\
    }								\
  PUSH_REF(pstate, obj);					\
} while(0)

#define EXIT_NESTED_DATA(pstate)				\
do								\
{								\
  --pstate->top;						\
  PSTATE_STACK_SET (pstate, pstate->top, SCM_UNDEFINED);	\
}								\
while (0)

SCM scm_print_state_vtable = SCM_BOOL_F;
static SCM print_state_pool = SCM_EOL;
scm_i_pthread_mutex_t print_state_mutex = SCM_I_PTHREAD_MUTEX_INITIALIZER;

#ifdef GUILE_DEBUG /* Used for debugging purposes */

SCM_DEFINE (scm_current_pstate, "current-pstate", 0, 0, 0, 
           (),
	    "Return the current-pstate -- the car of the\n"
	    "@code{print_state_pool}.  @code{current-pstate} is only\n"
	    "included in @code{--enable-guile-debug} builds.")
#define FUNC_NAME s_scm_current_pstate
{
  if (!scm_is_null (print_state_pool))
    return SCM_CAR (print_state_pool);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

#endif

#define PSTATE_SIZE 50L

static SCM
make_print_state (void)
{
  SCM print_state
    = scm_make_struct (scm_print_state_vtable, SCM_INUM0, SCM_EOL);
  scm_print_state *pstate = SCM_PRINT_STATE (print_state);
  pstate->ref_vect = scm_c_make_vector (PSTATE_SIZE, SCM_UNDEFINED);
  pstate->ceiling = SCM_SIMPLE_VECTOR_LENGTH (pstate->ref_vect);
  pstate->highlight_objects = SCM_EOL;
  return print_state;
}

SCM
scm_make_print_state ()
{
  SCM answer = SCM_BOOL_F;

  /* First try to allocate a print state from the pool */
  scm_i_pthread_mutex_lock (&print_state_mutex);
  if (!scm_is_null (print_state_pool))
    {
      answer = SCM_CAR (print_state_pool);
      print_state_pool = SCM_CDR (print_state_pool);
    }
  scm_i_pthread_mutex_unlock (&print_state_mutex);
  
  return scm_is_false (answer) ? make_print_state () : answer;
}

void
scm_free_print_state (SCM print_state)
{
  SCM handle;
  scm_print_state *pstate = SCM_PRINT_STATE (print_state);
  /* Cleanup before returning print state to pool.
   * It is better to do it here.  Doing it in scm_prin1
   * would cost more since that function is called much more
   * often.
   */
  pstate->fancyp = 0;
  pstate->revealed = 0;
  pstate->highlight_objects = SCM_EOL;
  scm_i_pthread_mutex_lock (&print_state_mutex);
  handle = scm_cons (print_state, print_state_pool);
  print_state_pool = handle;
  scm_i_pthread_mutex_unlock (&print_state_mutex);
}

SCM
scm_i_port_with_print_state (SCM port, SCM print_state)
{
  if (SCM_UNBNDP (print_state))
    {
      if (SCM_PORT_WITH_PS_P (port))
	return port;
      else
	print_state = scm_make_print_state ();
      /* port does not need to be coerced since it doesn't have ps */
    }
  else
    port = SCM_COERCE_OUTPORT (port);
  SCM_RETURN_NEWSMOB (scm_tc16_port_with_ps,
		      SCM_UNPACK (scm_cons (port, print_state)));
}

static void
grow_ref_stack (scm_print_state *pstate)
{
  SCM old_vect = pstate->ref_vect;
  size_t old_size = SCM_SIMPLE_VECTOR_LENGTH (old_vect);
  size_t new_size = 2 * pstate->ceiling;
  SCM new_vect = scm_c_make_vector (new_size, SCM_UNDEFINED);
  unsigned long int i;

  for (i = 0; i != old_size; ++i)
    SCM_SIMPLE_VECTOR_SET (new_vect, i, SCM_SIMPLE_VECTOR_REF (old_vect, i));

  pstate->ref_vect = new_vect;
  pstate->ceiling = new_size;
}

#define PSTATE_STACK_REF(p,i)   SCM_SIMPLE_VECTOR_REF((p)->ref_vect, (i))
#define PSTATE_STACK_SET(p,i,v) SCM_SIMPLE_VECTOR_SET((p)->ref_vect, (i), (v))

static void
print_circref (SCM port, scm_print_state *pstate, SCM ref)
{
  register long i;
  long self = pstate->top - 1;
  i = pstate->top - 1;
  if (scm_is_pair (PSTATE_STACK_REF (pstate, i)))
    {
      while (i > 0)
	{
	  if (!scm_is_pair (PSTATE_STACK_REF (pstate, i-1))
	      || !scm_is_eq (SCM_CDR (PSTATE_STACK_REF (pstate, i-1)), 
			     SCM_CDR (PSTATE_STACK_REF (pstate, i))))
	    break;
	  --i;
	}
      self = i;
    }
  for (i = pstate->top - 1; 1; --i)
    if (scm_is_eq (PSTATE_STACK_REF(pstate, i), ref))
      break;
  scm_putc ('#', port);
  scm_intprint (i - self, 10, port);
  scm_putc ('#', port);
}

/* Print the name of a symbol. */

static int
quote_keywordish_symbol (const char *str, size_t len)
{
  SCM option;

  /* LEN is guaranteed to be > 0.
   */
  if (str[0] != ':' && str[len-1] != ':')
    return 0;

  option = SCM_PRINT_KEYWORD_STYLE;
  if (scm_is_false (option))
    return 0;
  if (scm_is_eq (option, sym_reader))
    return scm_is_true (SCM_PACK (SCM_KEYWORD_STYLE));
  return 1;
}

void
scm_print_symbol_name (const char *str, size_t len, SCM port)
{
  /* This points to the first character that has not yet been written to the
   * port. */
  size_t pos = 0;
  /* This points to the character we're currently looking at. */
  size_t end;
  /* If the name contains weird characters, we'll escape them with
   * backslashes and set this flag; it indicates that we should surround the
   * name with "#{" and "}#". */
  int weird = 0;
  /* Backslashes are not sufficient to make a name weird, but if a name is
   * weird because of other characters, backslahes need to be escaped too.
   * The first time we see a backslash, we set maybe_weird, and mw_pos points
   * to the backslash.  Then if the name turns out to be weird, we re-process
   * everything starting from mw_pos.
   * We could instead make backslashes always weird.  This is not necessary
   * to ensure that the output is (read)-able, but it would make this code
   * simpler and faster. */
  int maybe_weird = 0;
  size_t mw_pos = 0;

  if (len == 0 || str[0] == '\'' || str[0] == '`' || str[0] == ','
      || quote_keywordish_symbol (str, len)
      || (str[0] == '.' && len == 1)
      || scm_is_true (scm_c_locale_stringn_to_number (str, len, 10)))
    {
      scm_lfwrite ("#{", 2, port);
      weird = 1;
    }

  for (end = pos; end < len; ++end)
    switch (str[end])
      {
#ifdef BRACKETS_AS_PARENS
      case '[':
      case ']':
#endif
      case '(':
      case ')':
      case '"':
      case ';':
      case '#':
      case SCM_WHITE_SPACES:
      case SCM_LINE_INCREMENTORS:
      weird_handler:
	if (maybe_weird)
	  {
	    end = mw_pos;
	    maybe_weird = 0;
	  }
	if (!weird)
	  {
	    scm_lfwrite ("#{", 2, port);
	    weird = 1;
	  }
	if (pos < end)
	  scm_lfwrite (str + pos, end - pos, port);
	{
	  char buf[2];
	  buf[0] = '\\';
	  buf[1] = str[end];
	  scm_lfwrite (buf, 2, port);
	}
	pos = end + 1;
	break;
      case '\\':
	if (weird)
	  goto weird_handler;
	if (!maybe_weird)
	  {
	    maybe_weird = 1;
	    mw_pos = pos;
	  }
	break;
      default:
	break;
      }
  if (pos < end)
    scm_lfwrite (str + pos, end - pos, port);
  if (weird)
    scm_lfwrite ("}#", 2, port);
}

/* Print generally.  Handles both write and display according to PSTATE.
 */
SCM_GPROC(s_write, "write", 1, 1, 0, scm_write, g_write);
SCM_GPROC(s_display, "display", 1, 1, 0, scm_display, g_display);

static void iprin1 (SCM exp, SCM port, scm_print_state *pstate);

void 
scm_iprin1 (SCM exp, SCM port, scm_print_state *pstate)
{
  if (pstate->fancyp
      && scm_is_true (scm_memq (exp, pstate->highlight_objects)))
    {
      scm_display (SCM_PRINT_HIGHLIGHT_PREFIX, port);
      iprin1 (exp, port, pstate);
      scm_display (SCM_PRINT_HIGHLIGHT_SUFFIX, port);
    }
  else
    iprin1 (exp, port, pstate);
}

static void
iprin1 (SCM exp, SCM port, scm_print_state *pstate)
{
  switch (SCM_ITAG3 (exp))
    {
    case scm_tc3_closure:
    case scm_tc3_tc7_1:
    case scm_tc3_tc7_2:
      /* These tc3 tags should never occur in an immediate value.  They are
       * only used in cell types of non-immediates, i. e. the value returned
       * by SCM_CELL_TYPE (exp) can use these tags.
       */
      scm_ipruk ("immediate", exp, port);
      break;
    case scm_tc3_int_1:
    case scm_tc3_int_2:
      scm_intprint (SCM_I_INUM (exp), 10, port);
      break;
    case scm_tc3_imm24:
      if (SCM_CHARP (exp))
	{
	  long i = SCM_CHAR (exp);

	  if (SCM_WRITINGP (pstate))
	    {
	      scm_puts ("#\\", port);
	      if ((i >= 0) && (i <= ' ') && scm_charnames[i])
		scm_puts (scm_charnames[i], port);
#ifndef EBCDIC
	      else if (i == '\177')
		scm_puts (scm_charnames[scm_n_charnames - 1], port);
#endif
	      else if (i < 0 || i > '\177')
		scm_intprint (i, 8, port);
	      else
		scm_putc (i, port);
	    }
	  else
	    scm_putc (i, port);
	}
      else if (SCM_IFLAGP (exp)
	       && ((size_t) SCM_IFLAGNUM (exp) < (sizeof iflagnames / sizeof (char *))))
        {
          scm_puts (iflagnames [SCM_IFLAGNUM (exp)], port);
        }
      else if (SCM_ISYMP (exp))
        {
          scm_i_print_isym (exp, port);
        }
      else if (SCM_ILOCP (exp))
	{
          scm_i_print_iloc (exp, port);
	}
      else
	{
	  /* unknown immediate value */
	  scm_ipruk ("immediate", exp, port);
	}
      break;
    case scm_tc3_cons:
      switch (SCM_TYP7 (exp))
	{
	case scm_tcs_struct:
	  {
	    ENTER_NESTED_DATA (pstate, exp, circref);
	    if (SCM_OBJ_CLASS_FLAGS (exp) & SCM_CLASSF_GOOPS)
	      {
		SCM pwps, print = pstate->writingp ? g_write : g_display;
		if (!print)
		  goto print_struct;
		pwps = scm_i_port_with_print_state (port, pstate->handle);
		pstate->revealed = 1;
		scm_call_generic_2 (print, exp, pwps);
	      }
	    else
	      {
	      print_struct:
		scm_print_struct (exp, port, pstate);
	      }
	    EXIT_NESTED_DATA (pstate);
	  }
	  break;
	case scm_tcs_cons_imcar:
	case scm_tcs_cons_nimcar:
	  ENTER_NESTED_DATA (pstate, exp, circref);
	  scm_iprlist ("(", exp, ')', port, pstate);
	  EXIT_NESTED_DATA (pstate);
	  break;
	circref:
	  print_circref (port, pstate, exp);
	  break;
	case scm_tcs_closures:
	  if (scm_is_false (scm_procedure_p (SCM_PRINT_CLOSURE))
	      || scm_is_false (scm_printer_apply (SCM_PRINT_CLOSURE,
						exp, port, pstate)))
	    {
	      SCM formals = SCM_CLOSURE_FORMALS (exp);
	      scm_puts ("#<procedure", port);
	      scm_putc (' ', port);
	      scm_iprin1 (scm_procedure_name (exp), port, pstate);
	      scm_putc (' ', port);
	      if (SCM_PRINT_SOURCE_P)
		{
		  SCM env = SCM_ENV (exp);
		  SCM xenv = SCM_EXTEND_ENV (formals, SCM_EOL, env);
		  SCM src = scm_i_unmemocopy_body (SCM_CODE (exp), xenv);
		  ENTER_NESTED_DATA (pstate, exp, circref);
		  scm_iprin1 (src, port, pstate);
		  EXIT_NESTED_DATA (pstate);
		}
	      else
		scm_iprin1 (formals, port, pstate);
	      scm_putc ('>', port);
	    }
	  break;
	case scm_tc7_number:
          switch SCM_TYP16 (exp) {
          case scm_tc16_big:
            scm_bigprint (exp, port, pstate);
            break;
          case scm_tc16_real:
            scm_print_real (exp, port, pstate);
            break;
          case scm_tc16_complex:
            scm_print_complex (exp, port, pstate);
            break;
          case scm_tc16_fraction:
            scm_i_print_fraction (exp, port, pstate);
            break;
          }
	  break;
	case scm_tc7_string:
	  if (SCM_WRITINGP (pstate))
	    {
	      size_t i, j, len;
	      const char *data;

	      scm_putc ('"', port);
	      len = scm_i_string_length (exp);
	      data = scm_i_string_chars (exp);
	      for (i = 0, j = 0; i < len; ++i)
		{
		  unsigned char ch = data[i];
		  if ((ch < 32 && ch != '\n') || (127 <= ch && ch < 148))
		    {
		      static char const hex[]="0123456789abcdef";
		      char buf[4];

		      scm_lfwrite (data+j, i-j, port);
		      buf[0] = '\\';
		      buf[1] = 'x';
		      buf[2] =  hex [ch / 16];
		      buf[3] = hex [ch % 16];
		      scm_lfwrite (buf, 4, port);
		      data = scm_i_string_chars (exp);
		      j = i+1;
		    }
		  else if (ch == '"' || ch == '\\')
		    {
		      scm_lfwrite (data+j, i-j, port);
		      scm_putc ('\\', port);
		      data = scm_i_string_chars (exp);
		      j = i;
		    }
		}
	      scm_lfwrite (data+j, i-j, port);
	      scm_putc ('"', port);
	      scm_remember_upto_here_1 (exp);
	    }
	  else
	    scm_lfwrite (scm_i_string_chars (exp), scm_i_string_length (exp),
			 port);
	  scm_remember_upto_here_1 (exp);
	  break;
	case scm_tc7_symbol:
	  if (scm_i_symbol_is_interned (exp))
	    {
	      scm_print_symbol_name (scm_i_symbol_chars (exp),
				     scm_i_symbol_length (exp),
				     port);
	      scm_remember_upto_here_1 (exp);
	    }
	  else
	    {
	      scm_puts ("#<uninterned-symbol ", port);
	      scm_print_symbol_name (scm_i_symbol_chars (exp),
				     scm_i_symbol_length (exp),
				     port);
	      scm_putc (' ', port);
	      scm_uintprint (SCM_UNPACK (exp), 16, port);
	      scm_putc ('>', port);
	    }
	  break;
	case scm_tc7_variable:
	  scm_i_variable_print (exp, port, pstate);
	  break;
	case scm_tc7_wvect:
	  ENTER_NESTED_DATA (pstate, exp, circref);
	  if (SCM_IS_WHVEC (exp))
	    scm_puts ("#wh(", port);
	  else
	    scm_puts ("#w(", port);
	  goto common_vector_printer;

	case scm_tc7_vector:
	  ENTER_NESTED_DATA (pstate, exp, circref);
	  scm_puts ("#(", port);
	common_vector_printer:
	  {
	    register long i;
	    long last = SCM_SIMPLE_VECTOR_LENGTH (exp) - 1;
	    int cutp = 0;
	    if (pstate->fancyp
		&& SCM_SIMPLE_VECTOR_LENGTH (exp) > pstate->length)
	      {
		last = pstate->length - 1;
		cutp = 1;
	      }
	    for (i = 0; i < last; ++i)
	      {
		/* CHECK_INTS; */
		scm_iprin1 (SCM_SIMPLE_VECTOR_REF (exp, i), port, pstate);
		scm_putc (' ', port);
	      }
	    if (i == last)
	      {
		/* CHECK_INTS; */
		scm_iprin1 (SCM_SIMPLE_VECTOR_REF (exp, i), port, pstate);
	      }
	    if (cutp)
	      scm_puts (" ...", port);
	    scm_putc (')', port);
	  }
	  EXIT_NESTED_DATA (pstate);
	  break;
	case scm_tcs_subrs:
	  scm_puts (SCM_SUBR_GENERIC (exp)
		    ? "#<primitive-generic "
		    : "#<primitive-procedure ",
		    port);
	  scm_puts (scm_i_symbol_chars (SCM_SNAME (exp)), port);
	  scm_putc ('>', port);
	  break;
#ifdef CCLO
	case scm_tc7_cclo:
	  {
	    SCM proc = SCM_CCLO_SUBR (exp);
	    if (scm_is_eq (proc, scm_f_gsubr_apply))
	      {
		/* Print gsubrs as primitives */
		SCM name = scm_procedure_name (exp);
		scm_puts ("#<primitive-procedure", port);
		if (scm_is_true (name))
		  {
		    scm_putc (' ', port);
		    scm_puts (scm_i_symbol_chars (name), port);
		  }
	      }
	    else
	      {
		scm_puts ("#<compiled-closure ", port);
		scm_iprin1 (proc, port, pstate);
	      }
	    scm_putc ('>', port);
	  }
	  break;
#endif
	case scm_tc7_pws:
	  scm_puts ("#<procedure-with-setter", port);
	  {
	    SCM name = scm_procedure_name (exp);
	    if (scm_is_true (name))
	      {
		scm_putc (' ', port);
		scm_display (name, port);
	      }
	  }
	  scm_putc ('>', port);
	  break;
	case scm_tc7_port:
	  {
	    register long i = SCM_PTOBNUM (exp);
	    if (i < scm_numptob
		&& scm_ptobs[i].print
		&& (scm_ptobs[i].print) (exp, port, pstate))
	      break;
	    goto punk;
	  }
	case scm_tc7_smob:
	  ENTER_NESTED_DATA (pstate, exp, circref);
	  SCM_SMOB_DESCRIPTOR (exp).print (exp, port, pstate);
	  EXIT_NESTED_DATA (pstate);
	  break;
	default:
	punk:
	  scm_ipruk ("type", exp, port);
	}
    }
}

/* Print states are necessary for circular reference safe printing.
 * They are also expensive to allocate.  Therefore print states are
 * kept in a pool so that they can be reused.
 */

/* The PORT argument can also be a print-state/port pair, which will
 * then be used instead of allocating a new print state.  This is
 * useful for continuing a chain of print calls from Scheme.  */

void 
scm_prin1 (SCM exp, SCM port, int writingp)
{
  SCM handle = SCM_BOOL_F; /* Will GC protect the handle whilst unlinked */
  SCM pstate_scm;
  scm_print_state *pstate;
  int old_writingp;

  /* If PORT is a print-state/port pair, use that.  Else create a new
     print-state. */

  if (SCM_PORT_WITH_PS_P (port))
    {
      pstate_scm = SCM_PORT_WITH_PS_PS (port);
      port = SCM_PORT_WITH_PS_PORT (port);
    }
  else
    {
      /* First try to allocate a print state from the pool */
      scm_i_pthread_mutex_lock (&print_state_mutex);
      if (!scm_is_null (print_state_pool))
	{
	  handle = print_state_pool;
	  print_state_pool = SCM_CDR (print_state_pool);
	}
      scm_i_pthread_mutex_unlock (&print_state_mutex);
      if (scm_is_false (handle))
	handle = scm_list_1 (make_print_state ());
      pstate_scm = SCM_CAR (handle);
    }

  pstate = SCM_PRINT_STATE (pstate_scm);
  old_writingp = pstate->writingp;
  pstate->writingp = writingp;
  scm_iprin1 (exp, port, pstate);
  pstate->writingp = old_writingp;

  /* Return print state to pool if it has been created above and
     hasn't escaped to Scheme. */

  if (scm_is_true (handle) && !pstate->revealed)
    {
      scm_i_pthread_mutex_lock (&print_state_mutex);
      SCM_SETCDR (handle, print_state_pool);
      print_state_pool = handle;
      scm_i_pthread_mutex_unlock (&print_state_mutex);
    }
}


/* Print an integer.
 */

void 
scm_intprint (scm_t_intmax n, int radix, SCM port)
{
  char num_buf[SCM_INTBUFLEN];
  scm_lfwrite (num_buf, scm_iint2str (n, radix, num_buf), port);
}

void 
scm_uintprint (scm_t_uintmax n, int radix, SCM port)
{
  char num_buf[SCM_INTBUFLEN];
  scm_lfwrite (num_buf, scm_iuint2str (n, radix, num_buf), port);
}

/* Print an object of unrecognized type.
 */

void 
scm_ipruk (char *hdr, SCM ptr, SCM port)
{
  scm_puts ("#<unknown-", port);
  scm_puts (hdr, port);
  if (scm_in_heap_p (ptr))
    {
      scm_puts (" (0x", port);
      scm_uintprint (SCM_CELL_WORD_0 (ptr), 16, port);
      scm_puts (" . 0x", port);
      scm_uintprint (SCM_CELL_WORD_1 (ptr), 16, port);
      scm_puts (") @", port);
    }
  scm_puts (" 0x", port);
  scm_uintprint (SCM_UNPACK (ptr), 16, port);
  scm_putc ('>', port);
}


/* Print a list.
 */
void 
scm_iprlist (char *hdr, SCM exp, int tlr, SCM port, scm_print_state *pstate)
{
  register SCM hare, tortoise;
  long floor = pstate->top - 2;
  scm_puts (hdr, port);
  /* CHECK_INTS; */
  if (pstate->fancyp)
    goto fancy_printing;
  
  /* Run a hare and tortoise so that total time complexity will be
     O(depth * N) instead of O(N^2). */
  hare = SCM_CDR (exp);
  tortoise = exp;
  while (scm_is_pair (hare))
    {
      if (scm_is_eq (hare, tortoise))
	goto fancy_printing;
      hare = SCM_CDR (hare);
      if (!scm_is_pair (hare))
	break;
      hare = SCM_CDR (hare);
      tortoise = SCM_CDR (tortoise);
    }
  
  /* No cdr cycles intrinsic to this list */
  scm_iprin1 (SCM_CAR (exp), port, pstate);
  for (exp = SCM_CDR (exp); scm_is_pair (exp); exp = SCM_CDR (exp))
    {
      register long i;

      for (i = floor; i >= 0; --i)
	if (scm_is_eq (PSTATE_STACK_REF(pstate, i), exp))
	  goto circref;
      PUSH_REF (pstate, exp);
      scm_putc (' ', port);
      /* CHECK_INTS; */
      scm_iprin1 (SCM_CAR (exp), port, pstate);
    }
  if (!SCM_NULL_OR_NIL_P (exp))
    {
      scm_puts (" . ", port);
      scm_iprin1 (exp, port, pstate);
    }

end:
  scm_putc (tlr, port);
  pstate->top = floor + 2;
  return;
  
fancy_printing:
  {
    long n = pstate->length;
    
    scm_iprin1 (SCM_CAR (exp), port, pstate);
    exp = SCM_CDR (exp); --n;
    for (; scm_is_pair (exp); exp = SCM_CDR (exp))
      {
	register unsigned long i;

	for (i = 0; i < pstate->top; ++i)
	  if (scm_is_eq (PSTATE_STACK_REF(pstate, i), exp))
	    goto fancy_circref;
	if (pstate->fancyp)
	  {
	    if (n == 0)
	      {
		scm_puts (" ...", port);
		goto skip_tail;
	      }
	    else
	      --n;
	  }
	PUSH_REF(pstate, exp);
	++pstate->list_offset;
	scm_putc (' ', port);
	/* CHECK_INTS; */
	scm_iprin1 (SCM_CAR (exp), port, pstate);
      }
  }
  if (!SCM_NULL_OR_NIL_P (exp))
    {
      scm_puts (" . ", port);
      scm_iprin1 (exp, port, pstate);
    }
skip_tail:
  pstate->list_offset -= pstate->top - floor - 2;
  goto end;

fancy_circref:
  pstate->list_offset -= pstate->top - floor - 2;
  
circref:
  scm_puts (" . ", port);
  print_circref (port, pstate, exp);
  goto end;
}



int
scm_valid_oport_value_p	(SCM val)
{
  return (SCM_OPOUTPORTP (val)
          || (SCM_PORT_WITH_PS_P (val)
              && SCM_OPOUTPORTP (SCM_PORT_WITH_PS_PORT (val))));
}

/* SCM_GPROC(s_write, "write", 1, 1, 0, scm_write, g_write); */

SCM 
scm_write (SCM obj, SCM port)
{
  if (SCM_UNBNDP (port))
    port = scm_current_output_port ();

  SCM_ASSERT (scm_valid_oport_value_p (port), port, SCM_ARG2, s_write);

  scm_prin1 (obj, port, 1);
#if 0
#ifdef HAVE_PIPE
# ifdef EPIPE
  if (EPIPE == errno)
    scm_close_port (port);
# endif
#endif
#endif
  return SCM_UNSPECIFIED;
}


/* SCM_GPROC(s_display, "display", 1, 1, 0, scm_display, g_display); */

SCM 
scm_display (SCM obj, SCM port)
{
  if (SCM_UNBNDP (port))
    port = scm_current_output_port ();

  SCM_ASSERT (scm_valid_oport_value_p (port), port, SCM_ARG2, s_display);

  scm_prin1 (obj, port, 0);
#if 0
#ifdef HAVE_PIPE
# ifdef EPIPE
  if (EPIPE == errno)
    scm_close_port (port);
# endif
#endif
#endif
  return SCM_UNSPECIFIED;
}


SCM_DEFINE (scm_simple_format, "simple-format", 2, 0, 1,
            (SCM destination, SCM message, SCM args),
	    "Write @var{message} to @var{destination}, defaulting to\n"
	    "the current output port.\n"
	    "@var{message} can contain @code{~A} (was @code{%s}) and\n"
	    "@code{~S} (was @code{%S}) escapes.  When printed,\n"
	    "the escapes are replaced with corresponding members of\n"
	    "@var{ARGS}:\n"
	    "@code{~A} formats using @code{display} and @code{~S} formats\n"
	    "using @code{write}.\n"
	    "If @var{destination} is @code{#t}, then use the current output\n"
	    "port, if @var{destination} is @code{#f}, then return a string\n"
	    "containing the formatted text. Does not add a trailing newline.")
#define FUNC_NAME s_scm_simple_format
{
  SCM port, answer = SCM_UNSPECIFIED;
  int fReturnString = 0;
  int writingp;
  const char *start;
  const char *end;
  const char *p;

  if (scm_is_eq (destination, SCM_BOOL_T))
    {
      destination = port = scm_current_output_port ();
    }
  else if (scm_is_false (destination))
    {
      fReturnString = 1;
      port = scm_mkstrport (SCM_INUM0, 
			    scm_make_string (SCM_INUM0, SCM_UNDEFINED),
			    SCM_OPN | SCM_WRTNG,
			    FUNC_NAME);
      destination = port;
    }
  else
    {
      SCM_VALIDATE_OPORT_VALUE (1, destination);
      port = SCM_COERCE_OUTPORT (destination);
    }
  SCM_VALIDATE_STRING (2, message);
  SCM_VALIDATE_REST_ARGUMENT (args);

  start = scm_i_string_chars (message);
  end = start + scm_i_string_length (message);
  for (p = start; p != end; ++p)
    if (*p == '~')
      {
	if (++p == end)
	  break;

	switch (*p) 
	  {
	  case 'A': case 'a':
	    writingp = 0;
	    break;
	  case 'S': case 's':
	    writingp = 1;
	    break;
	  case '~':
	    scm_lfwrite (start, p - start, port);
	    start = p + 1;
	    continue;
	  case '%':
	    scm_lfwrite (start, p - start - 1, port);
	    scm_newline (port);
	    start = p + 1;
	    continue;
	  default:
	    SCM_MISC_ERROR ("FORMAT: Unsupported format option ~~~A - use (ice-9 format) instead",
			    scm_list_1 (SCM_MAKE_CHAR (*p)));
	    
	  }


	if (!scm_is_pair (args))
	  SCM_MISC_ERROR ("FORMAT: Missing argument for ~~~A",
			  scm_list_1 (SCM_MAKE_CHAR (*p)));
			  		
	scm_lfwrite (start, p - start - 1, port);
	/* we pass destination here */
	scm_prin1 (SCM_CAR (args), destination, writingp);
	args = SCM_CDR (args);
	start = p + 1;
      }

  scm_lfwrite (start, p - start, port);
  if (!scm_is_eq (args, SCM_EOL))
    SCM_MISC_ERROR ("FORMAT: ~A superfluous arguments",
		    scm_list_1 (scm_length (args)));

  if (fReturnString)
    answer = scm_strport_to_string (destination);

  return scm_return_first (answer, message);
}
#undef FUNC_NAME


SCM_DEFINE (scm_newline, "newline", 0, 1, 0, 
            (SCM port),
	    "Send a newline to @var{port}.\n"
	    "If @var{port} is omitted, send to the current output port.")
#define FUNC_NAME s_scm_newline
{
  if (SCM_UNBNDP (port))
    port = scm_current_output_port ();

  SCM_VALIDATE_OPORT_VALUE (1, port);

  scm_putc ('\n', SCM_COERCE_OUTPORT (port));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_write_char, "write-char", 1, 1, 0,
            (SCM chr, SCM port),
	    "Send character @var{chr} to @var{port}.")
#define FUNC_NAME s_scm_write_char
{
  if (SCM_UNBNDP (port))
    port = scm_current_output_port ();

  SCM_VALIDATE_CHAR (1, chr);
  SCM_VALIDATE_OPORT_VALUE (2, port);

  scm_putc ((int) SCM_CHAR (chr), SCM_COERCE_OUTPORT (port));
#if 0
#ifdef HAVE_PIPE
# ifdef EPIPE
  if (EPIPE == errno)
    scm_close_port (port);
# endif
#endif
#endif
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* Call back to Scheme code to do the printing of special objects
 * (like structs).  SCM_PRINTER_APPLY applies PROC to EXP and a smob
 * containing PORT and PSTATE.  This object can be used as the port for
 * display/write etc to continue the current print chain.  The REVEALED
 * field of PSTATE is set to true to indicate that the print state has
 * escaped to Scheme and thus has to be freed by the GC.
 */

scm_t_bits scm_tc16_port_with_ps;

/* Print exactly as the port itself would */

static int
port_with_ps_print (SCM obj, SCM port, scm_print_state *pstate)
{
  obj = SCM_PORT_WITH_PS_PORT (obj);
  return scm_ptobs[SCM_PTOBNUM (obj)].print (obj, port, pstate);
}

SCM
scm_printer_apply (SCM proc, SCM exp, SCM port, scm_print_state *pstate)
{
  pstate->revealed = 1;
  return scm_call_2 (proc, exp,
		     scm_i_port_with_print_state (port, pstate->handle));
}

SCM_DEFINE (scm_port_with_print_state, "port-with-print-state", 1, 1, 0, 
            (SCM port, SCM pstate),
	    "Create a new port which behaves like @var{port}, but with an\n"
	    "included print state @var{pstate}.  @var{pstate} is optional.\n"
	    "If @var{pstate} isn't supplied and @var{port} already has\n"
	    "a print state, the old print state is reused.")
#define FUNC_NAME s_scm_port_with_print_state
{
  SCM_VALIDATE_OPORT_VALUE (1, port);
  if (!SCM_UNBNDP (pstate))
    SCM_VALIDATE_PRINTSTATE (2, pstate);
  return scm_i_port_with_print_state (port, pstate);
}
#undef FUNC_NAME

SCM_DEFINE (scm_get_print_state, "get-print-state", 1, 0, 0, 
            (SCM port),
	    "Return the print state of the port @var{port}. If @var{port}\n"
	    "has no associated print state, @code{#f} is returned.")
#define FUNC_NAME s_scm_get_print_state
{
  if (SCM_PORT_WITH_PS_P (port))
    return SCM_PORT_WITH_PS_PS (port);
  if (SCM_OUTPUT_PORT_P (port))
    return SCM_BOOL_F;
  SCM_WRONG_TYPE_ARG (1, port);
}
#undef FUNC_NAME



void
scm_init_print ()
{
  SCM vtable, layout, type;

  scm_init_opts (scm_print_options, scm_print_opts, SCM_N_PRINT_OPTIONS);

  scm_print_options (scm_list_4 (scm_from_locale_symbol ("highlight-prefix"),
				 scm_from_locale_string ("{"),
				 scm_from_locale_symbol ("highlight-suffix"),
				 scm_from_locale_string ("}")));

  scm_gc_register_root (&print_state_pool);
  scm_gc_register_root (&scm_print_state_vtable);
  vtable = scm_make_vtable_vtable (scm_nullstr, SCM_INUM0, SCM_EOL);
  layout =
    scm_make_struct_layout (scm_from_locale_string (SCM_PRINT_STATE_LAYOUT));
  type = scm_make_struct (vtable, SCM_INUM0, scm_list_1 (layout));
  scm_set_struct_vtable_name_x (type, scm_from_locale_symbol ("print-state"));
  scm_print_state_vtable = type;

  /* Don't want to bind a wrapper class in GOOPS, so pass 0 as arg1. */
  scm_tc16_port_with_ps = scm_make_smob_type (0, 0);
  scm_set_smob_mark (scm_tc16_port_with_ps, scm_markcdr);
  scm_set_smob_print (scm_tc16_port_with_ps, port_with_ps_print);

#include "libguile/print.x"

  scm_print_opts[SCM_PRINT_KEYWORD_STYLE_I].val = SCM_UNPACK (sym_reader);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
