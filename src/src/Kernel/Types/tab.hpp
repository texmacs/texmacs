
/******************************************************************************
* MODULE     : tab.hpp
* DESCRIPTION: spacing
* COPYRIGHT  : (C) 1999  David Allouche
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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
  inline tab (): rep (tm_new<tab_rep> ()) {}
  inline tab (int pos, tree t): rep (tm_new<tab_rep> (pos, t)) {}
};
CONCRETE_CODE(tab);

#endif // defined TAB_H
