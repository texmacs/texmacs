
/******************************************************************************
* MODULE     : scroll_event.cpp
* DESCRIPTION: Events for scrollbars and scrollable widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "widget.hpp"
#include "ps_device.hpp"
#include "Widkit/Event/scroll_event.hpp"

/******************************************************************************
* The scroll event
******************************************************************************/

scroll_event_rep::scroll_event_rep (string which2, SI& c1b, SI& c2b, SI& c3b):
  event_rep (SCROLL_EVENT), which (which2), c1 (c1b), c2 (c2b), c3 (c3b) {}
scroll_event_rep::operator tree () {
  tree t (TUPLE, "scroll_event", which);
  t << as_string (c1/PIXEL) << as_string (c2/PIXEL) << as_string (c3/PIXEL);
  return t; }
/******************************************************************************
* Other events for internal use
******************************************************************************/

event get_width (SI& w) { return get_coord1 ("width", w); }
event get_height (SI& h) { return get_coord1 ("height", h); }
event set_hor_bar (widget bar) { return set_widget ("hor-bar",bar); }
event set_ver_bar (widget bar) { return set_widget ("ver-bar",bar); }
event emit_bar_set_extents (SI min, SI max) {
  return set_coord2 ("extents", min, max); }
event emit_bar_scroll_to (SI pos) {
  return set_coord1 ("scroll position", pos); }
event emit_scroll (SI& c1, SI& c2, SI& c3) {
  return new scroll_event_rep ("this", c1, c2, c3); }
event emit_hor_scroll (SI& c1, SI& c2, SI& c3) {
  return new scroll_event_rep ("hor-bar", c1, c2, c3); }
event emit_ver_scroll (SI& c1, SI& c2, SI& c3) {
  return new scroll_event_rep ("ver-bar", c1, c2, c3); }

/******************************************************************************
* Exported events
******************************************************************************/

event set_extents(SI x1, SI y1, SI x2, SI y2) {
  return set_coord4 ("extents", x1, y1, x2, y2); }
event get_extents (SI& x1, SI& y1, SI& x2, SI& y2) {
  return get_coord4 ("extents", x1, y1, x2, y2); }
event get_visible (SI& x1, SI& y1, SI& x2, SI& y2) {
  return get_coord4 ("visible", x1, y1, x2, y2); }
event set_scroll_pos (SI x, SI y) {
  return set_coord2 ("scroll position", x, y); }
