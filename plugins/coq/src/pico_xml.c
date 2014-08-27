/******************************************************************************
* MODULE     : pico_xml.c
* DESCRIPTION: Small filter helping to cut an XML flow between root tags
* COPYRIGHT  : (C) 2014  François Poulain, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <stdio.h>
#include "pico_xml.h"

void
init_xml_state (xml_state *xs) {
  xs->old_c= 0;
  xs->deepness= 0;
  xs->opened_tag= 0;
  xs->closing_tag= 0;
  xs->has_content= 0;
}

void
print_xml_state (xml_state *xs) {
      fprintf (stderr, "old_c= %c (0x%x), deepness= %d, opened_tag= %d, \
          closing_tag= %d, has_content= %d\n\n", xs->old_c, xs->old_c,
          xs->deepness, xs->opened_tag, xs->closing_tag, xs->has_content);
}

void
update_xml_state (xml_state *xs, int c) {
      if (xs->opened_tag < 0 || xs->deepness < 0)
        init_xml_state (xs);

      if (c == '<') {
        xs->opened_tag++;
        xs->has_content= 1;
      }
      else if (c == '>' && xs->closing_tag) {
        xs->opened_tag--;
        xs->deepness--;
        xs->closing_tag= 0;
      }
      else if (c == '>' && xs->old_c == '/') {
        xs->opened_tag--;
      }
      else if (c == '>') {
        xs->opened_tag--;
        xs->deepness++;
      }
      else if (c == '/' && xs->old_c == '<')
        xs->closing_tag= 1;

      xs->old_c= c;
}

int
ended_xml_root_tag (xml_state *xs) {
  return xs->has_content && xs->opened_tag == 0 && xs->deepness == 0;
}
