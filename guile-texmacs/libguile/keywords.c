/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2003, 2004, 2006, 2008 Free Software Foundation, Inc.
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

#include <string.h>

#include "libguile/_scm.h"
#include "libguile/async.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/hashtab.h"

#include "libguile/validate.h"
#include "libguile/keywords.h"
#include "libguile/strings.h"



scm_t_bits scm_tc16_keyword;

#define KEYWORDP(X)	(SCM_SMOB_PREDICATE (scm_tc16_keyword, (X)))
#define KEYWORDSYM(X)	(SCM_SMOB_OBJECT (X))

static int
keyword_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_puts ("#:", port);
  scm_display (KEYWORDSYM (exp), port);
  return 1;
}

SCM_DEFINE (scm_keyword_p, "keyword?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if the argument @var{obj} is a keyword, else\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_keyword_p
{
  return scm_from_bool (KEYWORDP (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_symbol_to_keyword, "symbol->keyword", 1, 0, 0,
	    (SCM symbol),
	    "Return the keyword with the same name as @var{symbol}.")
#define FUNC_NAME s_scm_symbol_to_keyword
{
  SCM keyword;

  SCM_ASSERT_TYPE (scm_is_symbol (symbol), symbol, 0, NULL, "symbol");

  SCM_CRITICAL_SECTION_START;
  /* njrev: NEWSMOB and hashq_set_x can raise errors */
  keyword = scm_hashq_ref (scm_keyword_obarray, symbol, SCM_BOOL_F);
  if (scm_is_false (keyword))
    {
      SCM_NEWSMOB (keyword, scm_tc16_keyword, SCM_UNPACK (symbol));
      scm_hashq_set_x (scm_keyword_obarray, symbol, keyword);
    }
  SCM_CRITICAL_SECTION_END;
  return keyword;
}
#undef FUNC_NAME

SCM_DEFINE (scm_keyword_to_symbol, "keyword->symbol", 1, 0, 0,
	    (SCM keyword),
	    "Return the symbol with the same name as @var{keyword}.")
#define FUNC_NAME s_scm_keyword_to_symbol
{
  scm_assert_smob_type (scm_tc16_keyword, keyword);
  return KEYWORDSYM (keyword);
}
#undef FUNC_NAME

int
scm_is_keyword (SCM val)
{
  return KEYWORDP (val);
}

SCM
scm_from_locale_keyword (const char *str)
{
  return scm_symbol_to_keyword (scm_from_locale_symbol (str));
}

SCM
scm_from_locale_keywordn (const char *str, size_t len)
{
  return scm_symbol_to_keyword (scm_from_locale_symboln (str, len));
}

/* njrev: critical sections reviewed so far up to here */
void
scm_init_keywords ()
{
  scm_tc16_keyword = scm_make_smob_type ("keyword", 0);
  scm_set_smob_mark (scm_tc16_keyword, scm_markcdr);
  scm_set_smob_print (scm_tc16_keyword, keyword_print);

  scm_keyword_obarray = scm_c_make_hash_table (0);
#include "libguile/keywords.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
