
/******************************************************************************
* MODULE     : scroll_widget.cpp
* DESCRIPTION: Common base for scrollbars and scrollable widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Widget/attribute_widget.hpp"
#include "Widget/scroll_widget.hpp"
#include "Event/scroll_event.hpp"

/******************************************************************************
* Scroll widgets
******************************************************************************/


scroll_widget_rep::scroll_widget_rep (array<widget> a2,
  gravity grav2):
  attribute_widget_rep (a2, grav2) {}

bool
scroll_widget_rep::handle (event ev) {
  if (ev->type != SCROLL_EVENT) return attribute_widget_rep::handle (ev);
  scroll_event e (ev);
  handle_scroll (e);
  return true;
}
