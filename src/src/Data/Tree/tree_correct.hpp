
/******************************************************************************
* MODULE     : tree_correct.hpp
* DESCRIPTION: make a tree syntactically match a drd
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TREE_CORRECT_H
#define TREE_CORRECT_H
#include "drd_std.hpp"

tree drd_correct (drd_info drd, tree t);

#endif // defined TREE_CORRECT_H
