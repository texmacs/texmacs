
/******************************************************************************
* MODULE     : tab.hpp
* DESCRIPTION: spacing
* COPYRIGHT  : (C) 1999  David Allouche
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TAB_H
#define TAB_H
#include "tree.hpp"

enum tab_kind { tab_all, tab_first, tab_last };

class tab_rep: concrete_struct {
public:
  int pos;
  double weight;
  tab_kind kind;

  inline tab_rep () {}
  tab_rep (int pos, tree t);

  friend class tab;
};

class tab {
  CONCRETE(tab);
  inline tab (): rep (new tab_rep ()) {}
  inline tab (int pos, tree t): rep (new tab_rep (pos, t)) {}
};
CONCRETE_CODE(tab);

#endif // defined TAB_H
