
/******************************************************************************
* MODULE     : tree_cursor.hpp
* DESCRIPTION: abstract cursor handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TREE_CURSOR_H
#define TREE_CURSOR_H
#include "path.hpp"

bool valid_cursor (tree t, path p, bool start_flag= false);
path start (tree t, path p);
path end (tree t, path p);
path start (tree t);
path end (tree t);
path correct_cursor (tree t, path p);
path super_correct (tree t, path p);

#endif // defined TREE_CURSOR_H
