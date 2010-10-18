
/******************************************************************************
* MODULE     : path.hpp
* DESCRIPTION: paths are integer lists,
*              which are for instance useful to select subtrees in trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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
path operator / (path p, path q);
path common (path start, path end);

/******************************************************************************
* Getting subtrees from paths
******************************************************************************/

bool  has_subtree (tree t, path p);
tree& subtree (tree& t, path p);
tree& parent_subtree (tree& t, path p);

#endif // defined PATH_H
