/******************************************************************************
* MODULE     : pico_xml.h
* DESCRIPTION: Small filter helping to cut an XML flow between root tags
* COPYRIGHT  : (C) 2014  François Poulain, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PICO_XML_H
#define PICO_XML_H

typedef struct xml_state {
  int old_c, deepness, opened_tag, closing_tag, has_content;
} xml_state;

void init_xml_state (xml_state *xs);
void print_xml_state (xml_state *xs);
void update_xml_state (xml_state *xs, int c);
int  ended_xml_root_tag (xml_state *xs);

#endif // defined PICO_XML_H
