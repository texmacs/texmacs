
/******************************************************************************
* MODULE     : composite_event.cpp
* DESCRIPTION: Events for modification of composite widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Event/composite_event.hpp"

/******************************************************************************
* Events for composite widgets
******************************************************************************/

clean_event_rep::clean_event_rep ():
  event_rep (CLEAN_EVENT) {}
clean_event_rep::operator tree () { return "clean_event"; }
event emit_clean () {
  return new clean_event_rep (); }

insert_event_rep::insert_event_rep (string s2, widget w2):
  event_rep (INSERT_EVENT), s (s2), w (w2) {}
insert_event_rep::operator tree () {
  return tree (TUPLE, "insert_event", s); }
event emit_insert (string s, widget w) {
  return new insert_event_rep (s, w); }

remove_event_rep::remove_event_rep (string s2):
  event_rep (REMOVE_EVENT), s (s2) {}
remove_event_rep::operator tree () {
  return tree (TUPLE, "remove_event", s); }
event emit_remove (string s) {
  return new remove_event_rep (s); }
