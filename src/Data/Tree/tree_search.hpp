
/******************************************************************************
* MODULE     : tree_search.hpp
* DESCRIPTION: Searching inside trees
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TREE_SEARCH_H
#define TREE_SEARCH_H
#include "tree_select.hpp"

range_set search (tree t, tree what, path p, int limit= 1000000);
range_set search (tree t, tree what, path p, path pos, int limit);
range_set previous_search_hit (range_set sel, path cur, bool strict);
range_set next_search_hit (range_set sel, path cur, bool strict);
range_set navigate_search_hit (path cur, bool fw, bool extreme, bool strict);

range_set spell (string lan, tree t, path p, int limit= 1000000);
range_set spell (string lan, tree t, path p, path pos, int limit);

#endif // defined TREE_SEARCH_H
