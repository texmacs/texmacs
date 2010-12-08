
/******************************************************************************
* MODULE     : tree_select.hpp
* DESCRIPTION: abstract cursor handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TREE_SELECT_H
#define TREE_SELECT_H
#include "tree_cursor.hpp"
#include "tree_traverse.hpp"

void selection_correct (tree t, path i1, path i2, path& o1, path& o2);
tree selection_compute (tree t, path i1, path i2);

#endif // defined TREE_SELECT_H
