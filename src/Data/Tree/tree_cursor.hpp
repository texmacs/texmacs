
/******************************************************************************
* MODULE     : tree_cursor.hpp
* DESCRIPTION: abstract cursor handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TREE_CURSOR_H
#define TREE_CURSOR_H
#include "path.hpp"

bool is_inside (tree t, path p);
path closest_inside (tree t, path p);
bool is_accessible_cursor (tree t, path p);
path closest_accessible (tree t, path p, int dir= 0);
path closest_accessible_inside (tree t, path p, int dir= 0);
void show_hidden_upwards (tree t, path p);

bool valid_cursor (tree t, path p, bool start_flag= false);
path start (tree t, path p);
path end (tree t, path p);
path start (tree t);
path end (tree t);
path correct_cursor (tree t, path p, bool forwards= false);
path super_correct (tree t, path p, bool forwards= false);
path shift (tree t, path p, int dir);

#endif // defined TREE_CURSOR_H
