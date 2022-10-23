
/******************************************************************************
* MODULE     : tree_math_stats.hpp
* DESCRIPTION: compile statistics for math formulas in documents
* COPYRIGHT  : (C) 2022  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TREE_MATH_STATS_H
#define TREE_MATH_STATS_H
#include "drd_std.hpp"

void compile_stats (string id, tree t, string mode);
int number_occurrences (string id, tree t);
int number_in_role (string id, tree t);

#endif // defined TREE_MATH_STATS_H
