
/******************************************************************************
* MODULE     : tab.cpp
* DESCRIPTION: spacing
* COPYRIGHT  : (C) 1999  David Allouche
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tab.hpp"

/******************************************************************************
* Constructor
******************************************************************************/

tab_rep::tab_rep (int pos2, tree t): pos (pos2) {
  if (N(t) <= 1) {
    kind= tab_all; weight= 1.0; }
  else if (t[1] == "first") {
    kind= tab_first; weight= 0; }
  else if (t[1] == "last") {
    kind= tab_last; weight= 0; }
  else {
    kind= tab_all; weight= as_double (t[1]); }
}
