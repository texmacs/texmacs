
/******************************************************************************
* MODULE     : tag_info.hpp
* DESCRIPTION: DRD information about tags
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TAG_INFO_H
#define TAG_INFO_H
#include "tree.hpp"

#define TYPE_INVALID         -1
#define TYPE_REGULAR          0
#define TYPE_ADHOC            1
#define TYPE_RAW              2
#define TYPE_VARIABLE         3
#define TYPE_ARGUMENT         4
#define TYPE_BINDING          5
#define TYPE_BOOLEAN          6
#define TYPE_INTEGER          7
#define TYPE_STRING           8
#define TYPE_LENGTH           9
#define TYPE_NUMERIC         10
#define TYPE_CODE            11
#define TYPE_IDENTIFIER      12
#define TYPE_URL             13
#define TYPE_COLOR           14
#define TYPE_GRAPHICAL       15
#define TYPE_POINT           16
#define TYPE_CONSTRAINT      17
#define TYPE_GRAPHICAL_ID    18
#define TYPE_EFFECT          19
#define TYPE_ANIMATION       20
#define TYPE_DURATION        21
#define TYPE_FONT_SIZE       22
#define TYPE_OBSOLETE        23
#define TYPE_UNKNOWN         24
#define TYPE_ERROR           25

int    drd_encode (tree t);
tree   drd_decode (int i);
int    drd_encode_type (string s);
string drd_decode_type (int i);

/******************************************************************************
* The parent_info class contains outer information about tags
*
* - The type field specifies the type of the return value of the tag.
*   For instance, regular content has the type TYPE_REGULAR and
*   the tag PLUS which performs a numerical operation admits
*   the type TYPE_NUMERIC.
*
* - The arity fields together with the child_mode determine the possible
*   arities and how to convert logical indices of children to
*   physical indices in the array 'ci'.
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
* - The border_mode field specifies whether the cursor may be put behind and
*   before the tag or not (the 0 and 1 paths). For instance, this field
*   is BORDER_INNER for CONCAT and BORDER_YES for FRAC.
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

#define BORDER_YES            0
#define BORDER_INNER          1
#define BORDER_OUTER          2
#define BORDER_NO             3

#define VAR_MACRO             0
#define VAR_PARAMETER         1
#define VAR_MACRO_PARAMETER   2

struct parent_info {
  unsigned type             : 5; // the type
  unsigned arity_mode       : 2; // arity layout
  unsigned arity_base       : 6; // base arity (minimal arity)
  unsigned arity_extra      : 4; // extra arity (optional, repeated, etc.)
  unsigned child_mode       : 2; // child layout
  unsigned border_mode      : 2; // is the border inaccessible?
  unsigned block            : 2; // is a block structure?
  unsigned with_like        : 1; // is only an environment modifier?
  unsigned var_type         : 2; // macro, parameter or macro parameter
  unsigned freeze_type      : 1; // true => disable heuristic determination
  unsigned freeze_arity     : 1;
  unsigned freeze_border    : 1;
  unsigned freeze_block     : 1;
  unsigned freeze_with      : 1;
  unsigned freeze_var_type  : 1;

  parent_info (int arity, int extra, int amode, int cmode, bool frozen= false);
  parent_info (tree t);
  inline ~parent_info () {}
  operator tree ();
  bool operator == (const parent_info& pi);
  bool operator != (const parent_info& pi);
  friend tm_ostream& operator << (tm_ostream& out, parent_info pi);
};

/******************************************************************************
* The child_info class contains more detailed information about each of
* the children of the tag.
*
* - The type field specifies the type of the field.
*   Fields with regular content admit TYPE_REGULAR as their type.
*
* - The accessible field specifies whether the field can be accessed.
*   ACCESSIBLE_ALWAYS children can always be accessed, ACCESSIBLE_NEVER
*   children can only be accessed in source mode and ACCESSIBLE_HIDDEN
*   children may require unfolding in order to be accessed.
*
* - The writability field specifies whether an accessible field can be edited.
*   When the writability of a child is disabled, its whole descendance
*   becomes read-only, except for those parts whose writability are re-enabled.
*
* - The block field specifies whether the field is required to be
*   a block structure, an inline structure, or any of the two.
*
* - The mode field specifies the mode for each child. In case of MODE_PARENT,
*   the mode of the child is the same as the mode of its parent.
*
* - The freeze_* fields specify that the contents of the corresponding
*   fields may not be overwritten during the heuristic determination of
*   missing drd information.
******************************************************************************/

#define ACCESSIBLE_NEVER      0
#define ACCESSIBLE_HIDDEN     1
#define ACCESSIBLE_ALWAYS     2

#define WRITABILITY_NORMAL    0
#define WRITABILITY_DISABLE   1
#define WRITABILITY_ENABLE    2

#define BLOCK_REQUIRE_BLOCK   0
#define BLOCK_REQUIRE_INLINE  1
#define BLOCK_REQUIRE_NONE    2

struct child_info {
  unsigned type              :  5; // argument type
  unsigned accessible        :  2; // child is accessible?
  unsigned writability       :  2; // writability of child
  unsigned block             :  2; // require children to be blocks?
  unsigned env               : 16; // environment of the child?
  unsigned freeze_type       :  1; // true => disable heuristic determination
  unsigned freeze_accessible :  1;
  unsigned freeze_writability:  1;
  unsigned freeze_block      :  1;
  unsigned freeze_env        :  1;

  child_info (bool frozen= false);
  child_info (tree t);
  inline ~child_info () {}
  operator tree ();
  bool operator == (const child_info& pi);
  bool operator != (const child_info& pi);
  friend tm_ostream& operator << (tm_ostream& out, child_info ci);
};

class tag_info;
class tag_info_rep: concrete_struct {
public:
  parent_info       pi;
  array<child_info> ci;
  tree              extra;

  tag_info_rep (parent_info pi, array<child_info> ci, tree extra);
  tag_info_rep (int arity, int extra, int amode, int cmode, bool frozen);
  inline ~tag_info_rep () {}

  tag_info inner_border ();
  tag_info outer_border ();
  tag_info with_like ();
  tag_info var_parameter ();
  tag_info var_macro_parameter ();
  tag_info type (int tp);
  tag_info type (int i, int tp);
  tag_info accessible (int i);
  tag_info hidden (int i);
  tag_info disable_writable (int i);
  tag_info enable_writable (int i);
  tag_info locals (int i, string var, string val);
  tag_info name (string s);
  tag_info long_name (string s);
  tag_info name (int i, string s);
  tag_info long_name (int i, string s);
  int      get_index (int child, int n);
  void     set_attribute (string which, tree val);
  tree     get_attribute (string which);

  friend class tag_info;
};

/******************************************************************************
* The main tag_info class consists of parent_info and an array of child_info
******************************************************************************/

class tag_info {
  CONCRETE(tag_info);
  tag_info (parent_info pi, array<child_info> ci, tree extra);
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
tm_ostream& operator << (tm_ostream& out, tag_info ti);
tag_info copy (tag_info ti);

#endif // defined TAG_INFO_H
