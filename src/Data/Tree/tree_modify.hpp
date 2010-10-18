
/******************************************************************************
* MODULE     : tree_modify.hpp
* DESCRIPTION: high level tree modification subroutines
* COPYRIGHT  : (C) 2010  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TREE_MODIFY_H
#define TREE_MODIFY_H
#include "drd_std.hpp"

void correct_node (tree& t);
void correct_downwards (tree& t);
void correct_upwards (tree& t);

#endif // defined TREE_MODIFY_H
