
/******************************************************************************
* MODULE     : tree_label.cpp
* DESCRIPTION: labels of trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tree_label.hpp"
#include "hashmap.hpp"

hashmap<int,string> CONSTRUCTOR_NAME ("?");
hashmap<string,int> CONSTRUCTOR_CODE (UNKNOWN);

/******************************************************************************
* Setting up the conversion tables
******************************************************************************/

static tree_label next_tree_label= START_EXTENSIONS;

void
make_tree_label (tree_label l, string s) {
  CONSTRUCTOR_NAME ((int) l) = s;
  CONSTRUCTOR_CODE (s)       = (int) l;
}

tree_label
make_tree_label (string s) {
  if (CONSTRUCTOR_CODE->contains (s))
    return (tree_label) CONSTRUCTOR_CODE[s];
  tree_label l= next_tree_label;
  next_tree_label= (tree_label) (((int) next_tree_label) + 1);
  make_tree_label (l, s);
  return l;
}

/******************************************************************************
* Conversions between tree_labels and strings
******************************************************************************/

string
as_string (tree_label l) {
  return CONSTRUCTOR_NAME[(int) l];
}

tree_label
as_tree_label (string s) {
  return (tree_label) CONSTRUCTOR_CODE[s];
}

bool
existing_tree_label (string s) {
  return CONSTRUCTOR_CODE->contains (s);
}
