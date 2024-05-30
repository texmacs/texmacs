/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2003, 2004, 2006, 2009 Free Software Foundation, Inc.
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
#include "libguile/chars.h"
#include "libguile/eval.h"
#include "libguile/hash.h"
#include "libguile/smob.h"
#include "libguile/variable.h"
#include "libguile/alist.h"
#include "libguile/fluids.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"
#include "libguile/hashtab.h"
#include "libguile/weaks.h"
#include "libguile/modules.h"
#include "libguile/read.h"
#include "libguile/srfi-13.h"

#include "libguile/validate.h"
#include "libguile/symbols.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif



static SCM symbols;

#ifdef GUILE_DEBUG
SCM_DEFINE (scm_sys_symbols, "%symbols", 0, 0, 0,
	    (),
	    "Return the system symbol obarray.")
#define FUNC_NAME s_scm_sys_symbols
{
  return symbols;
}
#undef FUNC_NAME
#endif



/* {Symbols}
 */

/* In order to optimize reading speed, this function breaks part of
 * the hashtable abstraction.  The optimizations are:
 *
 * 1. The argument string can be compared directly to symbol objects
 *    without first creating an SCM string object.  (This would have
 *    been necessary if we had used the hashtable API in hashtab.h.)
 *
 * 2. We can use the raw hash value stored in scm_i_symbol_hash (sym)
 *    to speed up lookup.
 *
 * Both optimizations might be possible without breaking the
 * abstraction if the API in hashtab.c is improved.
 */

unsigned long
scm_i_hash_symbol (SCM obj, unsigned long n, void *closure)
{
  return scm_i_symbol_hash (obj) % n;
}

static SCM
lookup_interned_symbol (const char *name, size_t len,
			unsigned long raw_hash)
{
  /* Try to find the symbol in the symbols table */
  SCM l;
  unsigned long hash = raw_hash % SCM_HASHTABLE_N_BUCKETS (symbols);

  for (l = SCM_HASHTABLE_BUCKET (symbols, hash);
       !scm_is_null (l);
       l = SCM_CDR (l))
    {
      SCM sym = SCM_CAAR (l);
      if (scm_i_symbol_hash (sym) == raw_hash
	  && scm_i_symbol_length (sym) == len)
	{
	  const char *chrs = scm_i_symbol_chars (sym);
	  size_t i = len;

	  while (i != 0)
	    {
	      --i;
	      if (name[i] != chrs[i])
		goto next_symbol;
	    }

	  return sym;
	}
    next_symbol:
      ;
    }

  return SCM_BOOL_F;
}

/* Intern SYMBOL, an uninterned symbol.  */
static void
intern_symbol (SCM symbol)
{
  SCM slot, cell;
  unsigned long hash;

  hash = scm_i_symbol_hash (symbol) % SCM_HASHTABLE_N_BUCKETS (symbols);
  slot = SCM_HASHTABLE_BUCKET (symbols, hash);
  cell = scm_cons (symbol, SCM_UNDEFINED);

  SCM_SET_HASHTABLE_BUCKET (symbols, hash, scm_cons (cell, slot));
  SCM_HASHTABLE_INCREMENT (symbols);

  if (SCM_HASHTABLE_N_ITEMS (symbols) > SCM_HASHTABLE_UPPER (symbols))
    scm_i_rehash (symbols, scm_i_hash_symbol, 0, "intern_symbol");
}

static SCM
scm_i_c_mem2symbol (const char *name, size_t len)
{
  SCM symbol;
  size_t raw_hash = scm_string_hash ((const unsigned char *) name, len);

  symbol = lookup_interned_symbol (name, len, raw_hash);
  if (scm_is_false (symbol))
    {
      /* The symbol was not found, create it.  */
      symbol = scm_i_c_make_symbol (name, len, 0, raw_hash,
				    scm_cons (SCM_BOOL_F, SCM_EOL));
      intern_symbol (symbol);
    }

  return symbol;
}

static SCM
scm_i_mem2symbol (SCM str)
{
  SCM symbol;
  const char *name = scm_i_string_chars (str);
  size_t len = scm_i_string_length (str);
  size_t raw_hash = scm_string_hash ((const unsigned char *) name, len);

  symbol = lookup_interned_symbol (name, len, raw_hash);
  if (scm_is_false (symbol))
    {
      /* The symbol was not found, create it.  */
      symbol = scm_i_make_symbol (str, 0, raw_hash,
				  scm_cons (SCM_BOOL_F, SCM_EOL));
      intern_symbol (symbol);
    }

  return symbol;
}


static SCM
scm_i_mem2uninterned_symbol (SCM str)
{
  const char *name = scm_i_string_chars (str);
  size_t len = scm_i_string_length (str);
  size_t raw_hash = scm_string_hash ((const unsigned char *) name, len);

  return scm_i_make_symbol (str, SCM_I_F_SYMBOL_UNINTERNED, 
			    raw_hash, scm_cons (SCM_BOOL_F, SCM_EOL));
}

SCM_DEFINE (scm_symbol_p, "symbol?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a symbol, otherwise return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_symbol_p
{
  return scm_from_bool (scm_is_symbol (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_symbol_interned_p, "symbol-interned?", 1, 0, 0, 
	    (SCM symbol),
	    "Return @code{#t} if @var{symbol} is interned, otherwise return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_symbol_interned_p
{
  SCM_VALIDATE_SYMBOL (1, symbol);
  return scm_from_bool (scm_i_symbol_is_interned (symbol));
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_symbol, "make-symbol", 1, 0, 0,
	    (SCM name),
	    "Return a new uninterned symbol with the name @var{name}.  " 
	    "The returned symbol is guaranteed to be unique and future "
	    "calls to @code{string->symbol} will not return it.")
#define FUNC_NAME s_scm_make_symbol
{
  SCM_VALIDATE_STRING (1, name);
  return scm_i_mem2uninterned_symbol (name);
}
#undef FUNC_NAME

SCM_DEFINE (scm_symbol_to_string, "symbol->string", 1, 0, 0, 
           (SCM s),
	    "Return the name of @var{symbol} as a string.  If the symbol was\n"
	    "part of an object returned as the value of a literal expression\n"
	    "(section @pxref{Literal expressions,,,r5rs, The Revised^5\n"
	    "Report on Scheme}) or by a call to the @code{read} procedure,\n"
	    "and its name contains alphabetic characters, then the string\n"
	    "returned will contain characters in the implementation's\n"
	    "preferred standard case---some implementations will prefer\n"
	    "upper case, others lower case.  If the symbol was returned by\n"
	    "@code{string->symbol}, the case of characters in the string\n"
	    "returned will be the same as the case in the string that was\n"
	    "passed to @code{string->symbol}.  It is an error to apply\n"
	    "mutation procedures like @code{string-set!} to strings returned\n"
	    "by this procedure.\n"
	    "\n"
	    "The following examples assume that the implementation's\n"
	    "standard case is lower case:\n"
	    "\n"
	    "@lisp\n"
	    "(symbol->string 'flying-fish)    @result{} \"flying-fish\"\n"
	    "(symbol->string 'Martin)         @result{}  \"martin\"\n"
	    "(symbol->string\n"
	    "   (string->symbol \"Malvina\")) @result{} \"Malvina\"\n"
	    "@end lisp")
#define FUNC_NAME s_scm_symbol_to_string
{
  SCM_VALIDATE_SYMBOL (1, s);
  return scm_i_symbol_substring (s, 0, scm_i_symbol_length (s));
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_to_symbol, "string->symbol", 1, 0, 0, 
	    (SCM string),
	    "Return the symbol whose name is @var{string}. This procedure\n"
	    "can create symbols with names containing special characters or\n"
	    "letters in the non-standard case, but it is usually a bad idea\n"
	    "to create such symbols because in some implementations of\n"
	    "Scheme they cannot be read as themselves.  See\n"
	    "@code{symbol->string}.\n"
	    "\n"
	    "The following examples assume that the implementation's\n"
	    "standard case is lower case:\n"
	    "\n"
	    "@lisp\n"
	    "(eq? 'mISSISSIppi 'mississippi) @result{} #t\n"
	    "(string->symbol \"mISSISSIppi\") @result{} @r{the symbol with name \"mISSISSIppi\"}\n"
	    "(eq? 'bitBlt (string->symbol \"bitBlt\")) @result{} #f\n"
	    "(eq? 'JollyWog\n"
	    "  (string->symbol (symbol->string 'JollyWog))) @result{} #t\n"
	    "(string=? \"K. Harper, M.D.\"\n"
	    "  (symbol->string\n"
	    "    (string->symbol \"K. Harper, M.D.\"))) @result{}#t\n"
	    "@end lisp")
#define FUNC_NAME s_scm_string_to_symbol
{
  SCM_VALIDATE_STRING (1, string);
  return scm_i_mem2symbol (string);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_ci_to_symbol, "string-ci->symbol", 1, 0, 0,
	    (SCM str),
	    "Return the symbol whose name is @var{str}.  @var{str} is\n"
	    "converted to lowercase before the conversion is done, if Guile\n"
	    "is currently reading symbols case-insensitively.")
#define FUNC_NAME s_scm_string_ci_to_symbol
{
  return scm_string_to_symbol (SCM_CASE_INSENSITIVE_P
			       ? scm_string_downcase(str)
			       : str);
}
#undef FUNC_NAME

#define MAX_PREFIX_LENGTH 30

SCM_DEFINE (scm_gensym, "gensym", 0, 1, 0,
            (SCM prefix),
	    "Create a new symbol with a name constructed from a prefix and\n"
	    "a counter value. The string @var{prefix} can be specified as\n"
	    "an optional argument. Default prefix is @code{ g}.  The counter\n"
	    "is increased by 1 at each call. There is no provision for\n"
	    "resetting the counter.")
#define FUNC_NAME s_scm_gensym
{
  static int gensym_counter = 0;
  
  SCM suffix, name;
  int n, n_digits;
  char buf[SCM_INTBUFLEN];

  if (SCM_UNBNDP (prefix))
    prefix = scm_from_locale_string (" g");
  
  /* mutex in case another thread looks and incs at the exact same moment */
  scm_i_scm_pthread_mutex_lock (&scm_i_misc_mutex);
  n = gensym_counter++;
  scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);

  n_digits = scm_iint2str (n, 10, buf);
  suffix = scm_from_locale_stringn (buf, n_digits);
  name = scm_string_append (scm_list_2 (prefix, suffix));
  return scm_string_to_symbol (name);
}
#undef FUNC_NAME

SCM_DEFINE (scm_symbol_hash, "symbol-hash", 1, 0, 0, 
	    (SCM symbol),
	    "Return a hash value for @var{symbol}.")
#define FUNC_NAME s_scm_symbol_hash
{
  SCM_VALIDATE_SYMBOL (1, symbol);
  return scm_from_ulong (scm_i_symbol_hash (symbol));
}
#undef FUNC_NAME

SCM_DEFINE (scm_symbol_fref, "symbol-fref", 1, 0, 0, 
           (SCM s),
	    "Return the contents of @var{symbol}'s @dfn{function slot}.")
#define FUNC_NAME s_scm_symbol_fref
{
  SCM_VALIDATE_SYMBOL (1, s);
  return SCM_CAR (SCM_CELL_OBJECT_3 (s));
}
#undef FUNC_NAME


SCM_DEFINE (scm_symbol_pref, "symbol-pref", 1, 0, 0, 
           (SCM s),
	    "Return the @dfn{property list} currently associated with @var{symbol}.")
#define FUNC_NAME s_scm_symbol_pref
{
  SCM_VALIDATE_SYMBOL (1, s);
  return SCM_CDR (SCM_CELL_OBJECT_3 (s));
}
#undef FUNC_NAME


SCM_DEFINE (scm_symbol_fset_x, "symbol-fset!", 2, 0, 0, 
           (SCM s, SCM val),
	    "Change the binding of @var{symbol}'s function slot.")
#define FUNC_NAME s_scm_symbol_fset_x
{
  SCM_VALIDATE_SYMBOL (1, s);
  SCM_SETCAR (SCM_CELL_OBJECT_3 (s), val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_symbol_pset_x, "symbol-pset!", 2, 0, 0,
           (SCM s, SCM val),
	    "Change the binding of @var{symbol}'s property slot.")
#define FUNC_NAME s_scm_symbol_pset_x
{
  SCM_VALIDATE_SYMBOL (1, s);
  SCM_SETCDR (SCM_CELL_OBJECT_3 (s), val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
scm_from_locale_symbol (const char *sym)
{
  return scm_i_c_mem2symbol (sym, strlen (sym));
}

SCM
scm_from_locale_symboln (const char *sym, size_t len)
{
  return scm_i_c_mem2symbol (sym, len);
}

SCM
scm_take_locale_symboln (char *sym, size_t len)
{
  SCM res;
  unsigned long raw_hash;

  if (len == (size_t)-1)
    len = strlen (sym);
  else
    {
      /* Ensure STR is null terminated.  A realloc for 1 extra byte should
         often be satisfied from the alignment padding after the block, with
         no actual data movement.  */
      sym = scm_realloc (sym, len+1);
      sym[len] = '\0';
    }

  raw_hash = scm_string_hash ((unsigned char *)sym, len);
  res = lookup_interned_symbol (sym, len, raw_hash);
  if (scm_is_false (res))
    {
      res = scm_i_c_take_symbol (sym, len, 0, raw_hash,
				 scm_cons (SCM_BOOL_F, SCM_EOL));
      intern_symbol (res);
    }
  else
    free (sym);

  return res;
}

SCM
scm_take_locale_symbol (char *sym)
{
  return scm_take_locale_symboln (sym, (size_t)-1);
}

void
scm_symbols_prehistory ()
{
  symbols = scm_make_weak_key_hash_table (scm_from_int (2139));
  scm_permanent_object (symbols);
}


void
scm_init_symbols ()
{
#include "libguile/symbols.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
