
/******************************************************************************
* MODULE     : line_item.cpp
* DESCRIPTION: Control routines for typesetting paragraphs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "formatter.hpp"
#include "Format/line_item.hpp"

/*****************************************************************************/
// Routines for the line item class
/*****************************************************************************/

line_item_rep::line_item_rep (int type2, box b2, int pen2):
  type (type2), b (b2), spc (0), penalty (pen2), limits (false)
    { DEBUG(line_item_count++); }
line_item_rep::line_item_rep (int type2, box b2, int pen2, language lan2):
  type (type2), b (b2), spc (0), penalty (pen2), limits (false),
  lan (lan2) { DEBUG(line_item_count++); }
line_item_rep::line_item_rep (int type2, box b2,
  int pen2, tree t2):
  type (type2), b (b2), spc (0), penalty (pen2), limits (false),
  t (t2) { DEBUG(line_item_count++); }
line_item_rep::~line_item_rep () {
  DEBUG(line_item_count--); }
line_item::line_item (int type, box b, int penalty):
  rep (new line_item_rep (type, b, penalty)) {}
line_item::line_item (int type, box b, int penalty, language lan):
  rep (new line_item_rep (type, b, penalty, lan)) {}
line_item::line_item (int type, box b, int penalty, tree t):
  rep (new line_item_rep (type, b, penalty, t)) {}
bool line_item::operator == (line_item item2) { return rep==item2.rep; }
bool line_item::operator != (line_item item2) { return rep!=item2.rep; }

ostream&
operator << (ostream& out, line_item item) {
  switch (item->type) {
  case OBSOLETE_ITEM: return out << "obsolete";
  case CONTROL_ITEM: return out << "control (" << item->t << ")";
  case STD_ITEM: return out << "std";
  case STRING_ITEM: return out << item->b->get_leaf_string ();
  case LEFT_BRACKET_ITEM:
    return out << "left" << item->b->get_leaf_string ();
  case MIDDLE_BRACKET_ITEM:
    return out << "middle" << item->b->get_leaf_string ();
  case RIGHT_BRACKET_ITEM:
    return out << "right" << item->b->get_leaf_string ();
  case FLOAT_ITEM:
    return out << "float (" << item->b->get_leaf_lazy () << ")";
  case LEFT_SUB_ITEM: return out << "left-sub";
  case LEFT_SUP_ITEM: return out << "left-sup";
  case RIGHT_SUB_ITEM: return out << "right-sub";
  case RIGHT_SUP_ITEM: return out << "right-sup";
  case GLUE_LEFT_ITEM: return out << "glue-left";
  case GLUE_RIGHT_ITEM: return out << "glue-right";
  case GLUE_BOTH_ITEM: return out << "glue-both";
  case GLUE_LEFT_SUBS_ITEM: return out << "glue-lsubs";
  case GLUE_RIGHT_SUBS_ITEM: return out << "glue-rsubs";
  }
  return out << "unknown";
}
