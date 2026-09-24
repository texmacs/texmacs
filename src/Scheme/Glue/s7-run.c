
/******************************************************************************
* MODULE     : s7-run.c
* DESCRIPTION: Minimal command line interpreter based on the vendored S7,
*              used to run the glue generators when TeXmacs is built with S7.
*              It accepts the subset of Guile's options used by build-glue:
*                s7-run [-l FILE]... [-c EXPR]...
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <stdio.h>
#include <string.h>
#include "s7.h"

int
main (int argc, char **argv) {
  s7_scheme *sc= s7_init ();
  s7_eval_c_string (sc, "(define (quit . args) (exit))");
  for (int i= 1; i < argc; i++) {
    if (strcmp (argv[i], "-l") == 0 && i + 1 < argc) {
      if (!s7_load (sc, argv[++i])) {
        fprintf (stderr, "s7-run: cannot load %s\n", argv[i]);
        return 1;
      }
    }
    else if (strcmp (argv[i], "-c") == 0 && i + 1 < argc)
      s7_eval_c_string (sc, argv[++i]);
    else {
      fprintf (stderr, "usage: s7-run [-l FILE]... [-c EXPR]...\n");
      return 1;
    }
  }
  fflush (stdout);
  return 0;
}
