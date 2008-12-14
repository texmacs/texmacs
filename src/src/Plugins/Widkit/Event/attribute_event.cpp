
/******************************************************************************
* MODULE     : attribute_event.cpp
* DESCRIPTION: Events for setting and retrieving attributes of a widget.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "renderer.hpp"
#include "Widkit/Event/attribute_event.hpp"

/******************************************************************************
* Events for getting attributes
******************************************************************************/

get_integer_event_rep::get_integer_event_rep (string w2, int& i2):
  event_rep (GET_INTEGER_EVENT), which (w2), i (i2) {}
get_integer_event_rep::operator tree () {
  return tree (TUPLE, "get_integer_event", which); }
event get_integer (string which, int& i) {
  return new get_integer_event_rep (which, i); }

get_double_event_rep::get_double_event_rep (string w2, double& x2):
  event_rep (GET_DOUBLE_EVENT), which (w2), x (x2) {}
get_double_event_rep::operator tree () {
  return tree (TUPLE, "get_double_event", which); }
event get_double (string which, double& x) {
  return new get_double_event_rep (which, x); }

get_string_event_rep::get_string_event_rep (string w2, string& s2):
  event_rep (GET_STRING_EVENT), which (w2), s (s2) {}
get_string_event_rep::operator tree () {
  return tree (TUPLE, "get_string_event", which); }
event get_string (string which, string& s) {
  return new get_string_event_rep (which, s); }

get_coord1_event_rep::get_coord1_event_rep (string w2, SI& c1b):
  event_rep (GET_COORD1_EVENT), which (w2), c1 (c1b) {}
get_coord1_event_rep::operator tree () {
  return tree (TUPLE, "get_coord1_event", which); }
event get_coord1 (string which, SI& c1) {
  return new get_coord1_event_rep (which, c1); }

get_coord2_event_rep::get_coord2_event_rep (string w2, SI& c1b, SI& c2b):
  event_rep (GET_COORD2_EVENT), which (w2), c1 (c1b), c2 (c2b) {}
get_coord2_event_rep::operator tree () {
  return tree (TUPLE, "get_coord2_event", which); }
event get_coord2 (string which, SI& c1, SI& c2) {
  return new get_coord2_event_rep (which, c1, c2); }

get_coord3_event_rep::get_coord3_event_rep (string w2,
  SI& c1b, SI& c2b, SI& c3b): event_rep (GET_COORD3_EVENT),
    which (w2), c1 (c1b), c2 (c2b), c3 (c3b) {}
get_coord3_event_rep::operator tree () {
  return tree (TUPLE, "get_coord3_event", which); }
event get_coord3 (string which, SI& c1, SI& c2, SI& c3) {
  return new get_coord3_event_rep (which, c1, c2, c3); }

get_coord4_event_rep::get_coord4_event_rep (string w2,
  SI& c1b, SI& c2b, SI& c3b, SI& c4b): event_rep (GET_COORD4_EVENT),
    which (w2), c1 (c1b), c2 (c2b), c3 (c3b), c4 (c4b) {}
get_coord4_event_rep::operator tree () {
  return tree (TUPLE, "get_coord4_event", which); }
event get_coord4 (string which, SI& c1, SI& c2, SI& c3, SI& c4) {
  return new get_coord4_event_rep (which, c1, c2, c3, c4); }

/******************************************************************************
* Events for setting attributes
******************************************************************************/

set_integer_event_rep::set_integer_event_rep (string w2, int i2):
  event_rep (SET_INTEGER_EVENT), which (w2), i (i2) {}
set_integer_event_rep::operator tree () {
  return tree (TUPLE, "set_integer_event", which, as_string (i)); }
event set_integer (string which, int i) {
  return new set_integer_event_rep (which, i); }

set_double_event_rep::set_double_event_rep (string w2, double x2):
  event_rep (SET_DOUBLE_EVENT), which (w2), x (x2) {}
set_double_event_rep::operator tree () {
  return tree (TUPLE, "set_double_event", which, as_string (x)); }
event set_double (string which, double x) {
  return new set_double_event_rep (which, x); }

set_string_event_rep::set_string_event_rep (string w2, string s2):
  event_rep (SET_STRING_EVENT), which (w2), s (s2) {}
set_string_event_rep::operator tree () {
  return tree (TUPLE, "set_string_event", which, s); }
event set_string (string which, string s) {
  return new set_string_event_rep (which, s); }

set_coord1_event_rep::set_coord1_event_rep (string w2, SI c1b):
  event_rep (SET_COORD1_EVENT), which (w2), c1 (c1b) {}
set_coord1_event_rep::operator tree () {
  return tree (TUPLE, "set_coord1_event", which, as_string (c1/PIXEL)); }
event set_coord1 (string which, SI c1) {
  return new set_coord1_event_rep (which, c1); }

set_coord2_event_rep::set_coord2_event_rep (string w2, SI c1b, SI c2b):
  event_rep (SET_COORD2_EVENT), which (w2), c1 (c1b), c2 (c2b) {}
set_coord2_event_rep::operator tree () {
  return tree (TUPLE, "set_coord2_event", which,
	       as_string (c1/PIXEL), as_string (c2/PIXEL)); }
event set_coord2 (string which, SI c1, SI c2) {
  return new set_coord2_event_rep (which, c1, c2); }

set_coord3_event_rep::set_coord3_event_rep (string w2,
  SI c1b, SI c2b, SI c3b): event_rep (SET_COORD3_EVENT),
    which (w2), c1 (c1b), c2 (c2b), c3 (c3b) {}
set_coord3_event_rep::operator tree () {
  tree t (TUPLE, "set_coord3_event", which);
  t << as_string (c1/PIXEL) << as_string (c2/PIXEL) << as_string (c3/PIXEL);
  return t; }
event set_coord3 (string which, SI c1, SI c2, SI c3) {
  return new set_coord3_event_rep (which, c1, c2, c3); }

set_coord4_event_rep::set_coord4_event_rep (string w2,
  SI c1b, SI c2b, SI c3b, SI c4b): event_rep (SET_COORD4_EVENT),
    which (w2), c1 (c1b), c2 (c2b), c3 (c3b), c4 (c4b) {}
set_coord4_event_rep::operator tree () {
  tree t (TUPLE, 6);
  t[0]= "set_coord4_event";
  t[1]= which;
  t[2]= as_string (c1/PIXEL);
  t[3]= as_string (c2/PIXEL);
  t[4]= as_string (c3/PIXEL);
  t[5]= as_string (c4/PIXEL);
  return t; }
event set_coord4 (string which, SI c1, SI c2, SI c3, SI c4) {
  return new set_coord4_event_rep (which, c1, c2, c3, c4); }
