
/******************************************************************************
* MODULE     : refresh_widget.cpp
* DESCRIPTION: Widgets which are capable of being refreshed
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Widkit/attribute_widget.hpp"
#include "Widkit/layout.hpp"

/******************************************************************************
* Refresh widget
******************************************************************************/

wk_widget
refresh_wk_widget (string tmwid) {
  (void) tmwid;
  return glue_wk_widget ();
}
