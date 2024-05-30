/* classes: h_files */

#ifndef SCM_SYMBOLS_H
#define SCM_SYMBOLS_H

/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2003, 2004, 2006 Free Software Foundation, Inc.
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



#include "libguile/__scm.h"


#define scm_is_symbol(x)            (!SCM_IMP (x) \
                                     && (SCM_TYP7 (x) == scm_tc7_symbol))
#define scm_i_symbol_hash(x)        ((unsigned long) SCM_CELL_WORD_2 (x))
#define scm_i_symbol_is_interned(x) \
  (!(SCM_CELL_WORD_0 (x) & SCM_I_F_SYMBOL_UNINTERNED))

#define SCM_I_F_SYMBOL_UNINTERNED   0x100



#ifdef GUILE_DEBUG
SCM_API SCM scm_sys_symbols (void);
#endif

SCM_API SCM scm_symbol_p (SCM x);
SCM_API SCM scm_symbol_interned_p (SCM sym);
SCM_API SCM scm_make_symbol (SCM name);
SCM_API SCM scm_symbol_to_string (SCM s);
SCM_API SCM scm_string_to_symbol (SCM s);
SCM_API SCM scm_string_ci_to_symbol (SCM s);

SCM_API SCM scm_symbol_fref (SCM s);
SCM_API SCM scm_symbol_pref (SCM s);
SCM_API SCM scm_symbol_fset_x (SCM s, SCM val);
SCM_API SCM scm_symbol_pset_x (SCM s, SCM val);

SCM_API SCM scm_symbol_hash (SCM s);
SCM_API SCM scm_gensym (SCM prefix);

SCM_API SCM scm_from_locale_symbol (const char *str);
SCM_API SCM scm_from_locale_symboln (const char *str, size_t len);
SCM_API SCM scm_take_locale_symbol (char *sym);
SCM_API SCM scm_take_locale_symboln (char *sym, size_t len);

/* internal functions. */

SCM_API unsigned long scm_i_hash_symbol (SCM obj, unsigned long n,
					 void *closure);

SCM_API void scm_symbols_prehistory (void);
SCM_API void scm_init_symbols (void);

#endif  /* SCM_SYMBOLS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
