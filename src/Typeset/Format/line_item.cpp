
/******************************************************************************
* MODULE     : line_item.cpp
* DESCRIPTION: Control routines for typesetting paragraphs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "formatter.hpp"
#include "Format/line_item.hpp"

/*****************************************************************************/
// Routines for the line item class
/*****************************************************************************/

line_item_rep::line_item_rep (int type2, box b2, int pen2):
  type (type2), b (b2), spc (0), penalty (pen2), limits (false)
    { TM_DEBUG(line_item_count++); }
line_item_rep::line_item_rep (int type2, box b2, int pen2, language lan2):
  type (type2), b (b2), spc (0), penalty (pen2), limits (false),
  lan (lan2) { TM_DEBUG(line_item_count++); }
line_item_rep::line_item_rep (int type2, box b2,
  int pen2, tree t2):
  type (type2), b (b2), spc (0), penalty (pen2), limits (false),
  t (t2) { TM_DEBUG(line_item_count++); }
line_item_rep::~line_item_rep () {
  TM_DEBUG(line_item_count--); }
line_item::line_item (int type, box b, int penalty):
  rep (tm_new<line_item_rep> (type, b, penalty)) {}
line_item::line_item (int type, box b, int penalty, language lan):
  rep (tm_new<line_item_rep> (type, b, penalty, lan)) {}
line_item::line_item (int type, box b, int penalty, tree t):
  rep (tm_new<line_item_rep> (type, b, penalty, t)) {}
bool line_item::operator == (line_item item2) { return rep==item2.rep; }
bool line_item::operator != (line_item item2) { return rep!=item2.rep; }

tm_ostream&
operator << (tm_ostream& out, line_item item) {
  switch (item->type) {
  case OBSOLETE_ITEM: return out << "obsolete";
  case STD_ITEM: return out << "std";
  case MARKER_ITEM: return out << "marker";
  case STRING_ITEM: return out << item->b->get_leaf_string ();
  case LEFT_BRACKET_ITEM:
    return out << "left" << item->b->get_leaf_string ();
  case MIDDLE_BRACKET_ITEM:
    return out << "middle" << item->b->get_leaf_string ();
  case RIGHT_BRACKET_ITEM:
    return out << "right" << item->b->get_leaf_string ();
  case CONTROL_ITEM: return out << "control (" << item->t << ")";
  case FLOAT_ITEM:
    return out << "float (" << item->b->get_leaf_lazy () << ")";
  case LSUB_ITEM: return out << "lsub";
  case LSUP_ITEM: return out << "lsup";
  case RSUB_ITEM: return out << "rsub";
  case RSUP_ITEM: return out << "rsup";
  case GLUE_LEFT_ITEM: return out << "glue-left";
  case GLUE_RIGHT_ITEM: return out << "glue-right";
  case GLUE_BOTH_ITEM: return out << "glue-both";
  case GLUE_LSUBS_ITEM: return out << "glue-lsubs";
  case GLUE_RSUBS_ITEM: return out << "glue-rsubs";
  }
  return out << "unknown";
}
