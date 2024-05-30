/* Copyright (C) 2009 Free Software Foundation, Inc.
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

/* Exercise `scm_take_locale_symbol ()', making sure it returns an interned
   symbol.  See https://savannah.gnu.org/bugs/index.php?25865 .  */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libguile.h>

#include <stdlib.h>
#include <string.h>


static void *
do_test (void *result)
{
  SCM taken_sym, sym;

  taken_sym = scm_take_locale_symbol (strdup ("some random symbol"));
  sym = scm_from_locale_symbol ("some random symbol");

  if (scm_is_true (scm_symbol_p (sym))
      && scm_is_true (scm_symbol_p (taken_sym))

      /* Relying solely on `scm_symbol_interned_p ()' is insufficient since
	 it doesn't reflect the actual state of the symbol hashtable, hence
	 the additional `scm_is_eq' test.  */
      && scm_is_true (scm_symbol_interned_p (sym))
      && scm_is_true (scm_symbol_interned_p (taken_sym))
      && scm_is_eq (taken_sym, sym))
    * (int *) result = EXIT_SUCCESS;
  else
    * (int *) result = EXIT_FAILURE;

  return NULL;
}

int
main (int argc, char *argv[])
{
  int result;

  scm_with_guile (do_test, &result);

  return result;
}
