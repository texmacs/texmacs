
/******************************************************************************
* MODULE     : tag_info.hpp
* DESCRIPTION: DRD information about tags
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TAG_INFO_H
#define TAG_INFO_H
#include "tree.hpp"

/******************************************************************************
* The parent_info class contains outer information about tags
*
* -  The arity fields together with the child_mode determine the possible
*    arities and how to convert logical indices of children to
*    physical indices in the array 'ci'.
*
*    o ARITY_NORMAL    : the arity is given by arity_base+arity_extra
*    o ARITY_OPTIONS   : arity_base <= arity < arity_base+arity_extra
*    o ARITY_REPEAT    : arity= arity_base + any_number * arity_extra
*    o ARITY_VAR_REPEAT: as ARITY_REPEAT, but repetition "comes first"
*
*    o CHILD_UNIFORM : all children have the same properties
*    o CHILD_BIFORM  : two types of properties (corresponds to base/extra)
*    o CHILD_DETAILED: as many as arity_base+arity_extra types of properties
*
* - The no_border field specifies whether the cursor may be put behind and
*   before the tag or not (the 0 and 1 paths). For instance, this field
*   is true for CONCAT and false for FRAC.
*
* - The block field specifies when the parent should be considered
*   as a block. In the case of BLOCK_OR, the parent is a block
*   if one of the children satisfying BLOCK_REQUIRE_NONE is a block.
*
* - The freeze_* fields specify that the contents of the corresponding
*   fields may not be overwritten during the heuristic determination of
*   missing drd information.
******************************************************************************/

#define ARITY_NORMAL          0
#define ARITY_OPTIONS         1
#define ARITY_REPEAT          2
#define ARITY_VAR_REPEAT      3

#define CHILD_UNIFORM         0
#define CHILD_BIFORM          1
#define CHILD_DETAILED        2

#define BLOCK_NO              0
#define BLOCK_YES             1
#define BLOCK_OR              2

struct parent_info {
  unsigned arity_mode       : 2; // arity layout
  unsigned arity_base       : 6; // base arity (minimal arity)
  unsigned arity_extra      : 4; // extra arity (optional, repeated, etc.)
  unsigned child_mode       : 2; // child layout
  unsigned no_border        : 1; // is the border inaccessible?
  unsigned block            : 2; // is a block structure?
  unsigned freeze_arity     : 1; // true => disable heuristic determination
  unsigned freeze_no_border : 1;
  unsigned freeze_block     : 1;

  parent_info (int arity, int extra, int amode, int cmode, bool frozen= false);
  parent_info (string s);
  inline ~parent_info () {}
  operator string ();
  bool operator == (const parent_info& pi);
  bool operator != (const parent_info& pi);
  friend ostream& operator << (ostream& out, parent_info pi);
};

/******************************************************************************
* The child_info class contains more detailed information about each of
* the children of the tag.
*
* - The accessible field specifies whether the field can be edited
*   while active.
*
* - The block field specifies whether the field is required to be
*   a block structure, an inline structure, or any of the two.
*
* - The freeze_* fields specify that the contents of the corresponding
*   fields may not be overwritten during the heuristic determination of
*   missing drd information.
******************************************************************************/

#define BLOCK_REQUIRE_BLOCK   0
#define BLOCK_REQUIRE_INLINE  1
#define BLOCK_REQUIRE_NONE    2

struct child_info {
  unsigned accessible       : 1; // child is accessible?
  unsigned block            : 2; // require children to be blocks?
  unsigned freeze_accessible: 1; // true => disable heuristic determination
  unsigned freeze_block     : 1;

  child_info (bool frozen= false);
  child_info (string s);
  inline ~child_info () {}
  operator string ();
  bool operator == (const child_info& pi);
  bool operator != (const child_info& pi);
  friend ostream& operator << (ostream& out, child_info ci);
};

class tag_info;
class tag_info_rep: concrete_struct {
public:
  parent_info       pi;
  array<child_info> ci;

  tag_info_rep (parent_info pi, array<child_info> ci);
  tag_info_rep (int arity, int extra, int amode, int cmode, bool frozen);
  inline ~tag_info_rep () {}

  tag_info no_border ();
  tag_info accessible (int i);
  int      get_index (int child, int n);

  friend class tag_info;
};

/******************************************************************************
* The main tag_info class consists of parent_info and an array of child_info
******************************************************************************/

class tag_info {
  CONCRETE(tag_info);
  tag_info (parent_info pi, array<child_info> ci);
  tag_info (int arity=0, int extra=0,
	    int am=ARITY_NORMAL, int cm= CHILD_UNIFORM,
	    bool frozen= false);
  tag_info (tree t);
  operator tree ();
  child_info& operator () (int child, int n);
};
CONCRETE_CODE(tag_info);

bool operator == (tag_info ti1, tag_info ti2);
bool operator != (tag_info ti1, tag_info ti2);
ostream& operator << (ostream& out, tag_info ti);
tag_info copy (tag_info ti);

#endif // defined TAG_INFO_H
