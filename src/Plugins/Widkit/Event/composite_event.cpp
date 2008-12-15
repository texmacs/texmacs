
/******************************************************************************
* MODULE     : composite_event.cpp
* DESCRIPTION: Events for modification of composite widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Widkit/Event/composite_event.hpp"

/******************************************************************************
* Events for composite widgets
******************************************************************************/

clean_event_rep::clean_event_rep ():
  event_rep (CLEAN_EVENT) {}
clean_event_rep::operator tree () { return "clean_event"; }
event emit_clean () {
  return tm_new<clean_event_rep> (); }

insert_event_rep::insert_event_rep (string s2, wk_widget w2):
  event_rep (INSERT_EVENT), s (s2), w (w2) {}
insert_event_rep::operator tree () {
  return tree (TUPLE, "insert_event", s); }
event emit_insert (string s, wk_widget w) {
  return tm_new<insert_event_rep> (s, w); }

remove_event_rep::remove_event_rep (string s2):
  event_rep (REMOVE_EVENT), s (s2) {}
remove_event_rep::operator tree () {
  return tree (TUPLE, "remove_event", s); }
event emit_remove (string s) {
  return tm_new<remove_event_rep> (s); }
