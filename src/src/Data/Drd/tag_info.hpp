
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

#define ACCESSIBLE_MASK              15
#define NOT_ACCESSIBLE                0
#define ACCESSIBLE                    1
#define FIRST_ACCESSIBLE              2
#define LAST_ACCESSIBLE               3
#define TAIL_ACCESSIBLE               4
#define TABLE_ACCESSIBLE              5
#define HIDE_EXPAND_ACCESSIBLE        6
#define CUSTOM_ACCESSIBLE             7
#define BORDER_ACCESSIBLE_MASK       16
#define BORDER_NOT_ACCESSIBLE        16
#define DYNAMIC_MASK                 32
#define DYNAMIC                      32

#define FROZEN_MASK                 192
#define FROZEN_ARITY                 64
#define FROZEN_ACCESSIBLE           128

#define CUSTOM_ACCESSIBLE_SHIFT      16
#define CUSTOM_ACCESSIBLE_MAX        12
#define CUSTOM_ACCESSIBLE_MASK \
  (((1 << CUSTOM_ACCESSIBLE_MAX) - 1) * (1 << CUSTOM_ACCESSIBLE_SHIFT))

#define ACCESSIBLE_EXCEPT_BORDER (ACCESSIBLE+BORDER_NOT_ACCESSIBLE)
#define ONLY_LAST_ACCESSIBLE (LAST_ACCESSIBLE+BORDER_NOT_ACCESSIBLE)

#define CHILD_UNIFORM       0
#define CHILD_LEFT_SPECIAL  1
#define CHILD_RIGHT_SPECIAL 2
#define CHILD_RIGHT_OPTIONS 3
#define CHILD_LEFT_REPEAT   4
#define CHILD_RIGHT_REPEAT  5

struct parent_info {
  unsigned child_mode       : 3; // child layout
  unsigned arity_min        : 6; // minimal number of arguments
  unsigned arity_extra      : 4; // extra arguments
  unsigned no_border        : 1; // true => inaccessible border
  unsigned block            : 1; // is a block structure
  unsigned dynamic          : 1; // admits inactive variant

  unsigned freeze_child     : 1; // true => disable heuristic determination
  unsigned freeze_arity     : 1;
  unsigned freeze_no_border : 1;
  unsigned freeze_block     : 1;
  unsigned freeze_dynamic   : 1;
};

struct child_info {
  unsigned accessible  : 1;
  unsigned block       : 1;

  unsigned mask_access : 1;
  unsigned mask_block  : 1;
};

class tag_info_rep: concrete_struct {
public:
  int    arity; // arity of the tag (-1 if several arities are possible)
  int    props; // properties of the tag

  tag_info_rep (int arity, int props);

  friend class tag_info;
};

class tag_info {
  CONCRETE(tag_info);
  tag_info (int arity= -1, int props= 0);
  operator tree ();
};
CONCRETE_CODE(tag_info);

bool operator != (tag_info ti1, tag_info ti2);
ostream& operator << (ostream& out, tag_info ti);
tag_info copy (tag_info ti);

#endif // defined TAG_INFO_H
