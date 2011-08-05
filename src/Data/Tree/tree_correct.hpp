
/******************************************************************************
* MODULE     : tree_correct.hpp
* DESCRIPTION: make a tree syntactically match a drd
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TREE_CORRECT_H
#define TREE_CORRECT_H
#include "drd_std.hpp"

tree drd_correct (drd_info drd, tree t);
tree with_correct (tree t);
tree superfluous_with_correct (tree t);
tree superfluous_invisible_correct (tree t);
tree missing_invisible_correct (tree t, int force= -1);
tree upgrade_brackets (tree t, string mode= "text");
tree upgrade_big (tree t);
tree downgrade_brackets (tree t, bool del_miss= false, bool big_dot= true);
tree downgrade_big (tree t);
tree move_brackets (tree t);

int  count_math_errors (tree t, int mode= 0);
void math_status_cumul (tree t);
void math_status_print ();
void math_status_reset ();

tree latex_correct (tree t);
tree automatic_correct (tree t, string version);
tree manual_correct (tree t);

#endif // defined TREE_CORRECT_H
