/******************************************************************************
* MODULE     : tm_coq.c
* DESCRIPTION: Glue between TeXmacs and Coq
* COPYRIGHT  : (C) 2014  François Poulain, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "pico_xml.h"

FILE *coqin, *coqout;

static void
fatal(char *mess) {
  fprintf(stderr, "Cannot %s", mess);
  exit(1);
}

static void 
session (void) {
  int c= 0, r= 0;
  xml_state xs;
  while (1) {
    init_xml_state (&xs);
    while (1) {
      // get entry from stdin
      c= getc (stdin);
      if (c == EOF) {
        fprintf (stderr, "Unable to read on stdin\n");
        exit (1);
      }

      // sent it to coq on coqin
      r= putc (c, coqin);

      update_xml_state (&xs, c);

      if (ended_xml_root_tag (&xs)) {
        fflush (coqin);
        break;
      }
      if (r == EOF) {
        fprintf (stderr, "Unable to write on coqin\n");
        exit (1);
      }
    }

    init_xml_state (&xs);
    while (1) {
      // get coq answer from coqout
      c= getc (coqout);
      if (c == EOF) {
        fprintf (stderr, "Unable to write on coqout\n");
        exit (1);
      }

      // send it to TeXmacs on stdout
      r= putc (c, stdout);

      update_xml_state (&xs, c);

      if (c == EOF || ended_xml_root_tag (&xs)) {
        r= fputs ("\5\2coqtopml:", stdout);
        fflush (stdout);
        break;
      }
    }
  }
}

int
main () {
  int p1[2], p2[2];
  if (pipe (p1) < 0)
    fatal ("create pipe");
  if (pipe (p2) < 0)
    fatal ("create pipe");
  switch (fork ()) {
    case -1: fatal ("fork");
    case  0: /* Coq */
      dup2   (p1[1], STDOUT_FILENO);
      close  (p1[0]);
      close  (p1[1]);
      dup2   (p2[0], STDIN_FILENO);
      close  (p2[1]);
      close  (p2[0]);
      execlp ("coqtop", "coqtop", "-main-channel", "stdfds", "-ideslave", NULL);
      fatal  ("exec coqtop");
    default: /* tm_coq */
      close  (p1[1]);
      close  (p2[0]);
      coqout= fdopen (p1[0], "r");
      coqin=  fdopen (p2[1], "w");
      fputs  ("\2verbatim:", stdout);
      system ("coqtop --version");
      fputs  ("\5\2coqtopml:", stdout);
      fflush (stdout);
      session ();
  }
  return 0;
}
