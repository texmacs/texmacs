
/******************************************************************************
* MODULE     : path.hpp
* DESCRIPTION: paths are integer lists,
*              which are for instance useful to select subtrees in trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef PATH_H
#define PATH_H
#include "list.hpp"
typedef list<int> path;
#include "tree_cursor.hpp"

/******************************************************************************
* General routines
******************************************************************************/

bool   zero_path (path p);
int    hash (path p);
string as_string (path p);
path   as_path (string s);
bool   version_inf_eq (string v1, string v2);
bool   version_inf (string v1, string v2);

/******************************************************************************
* Operations on paths
******************************************************************************/

path path_up (path p);
path path_up (path p, int times);
bool path_inf (path p1, path p2);
bool path_inf_eq (path p1, path p2);
bool path_less (path p1, path p2);
bool path_less_eq (path p1, path p2);
path path_add (path p, int plus);
path path_add (path p, int plus, int pos);
#define path_inc(p) path_add(p,1)
#define path_dec(p) path_add(p,-1)
path operator - (path p, path q);

/******************************************************************************
* Getting subtrees from paths
******************************************************************************/

tree& subtree (tree& t, path p);
tree& parent_subtree (tree& t, path p);

#endif // defined PATH_H
