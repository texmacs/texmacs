
/******************************************************************************
* MODULE     : tab.cpp
* DESCRIPTION: spacing
* COPYRIGHT  : (C) 1999  David Allouche
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
