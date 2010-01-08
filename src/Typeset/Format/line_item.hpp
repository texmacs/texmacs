
/******************************************************************************
* MODULE     : line_item.hpp
* DESCRIPTION: A line in a paragraph consist of an array of line items.
*              A line item contains spacing and hyphenation information.
*              Furthermore, different types of line items exist,
*              in order to typeset brackets, scripts and large operators.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef LINE_ITEM_H
#define LINE_ITEM_H
#include "boxes.hpp"
#include "env.hpp"

#define OBSOLETE_ITEM          0
#define STD_ITEM               1
#define STRING_ITEM            2
#define LEFT_BRACKET_ITEM      3
#define MIDDLE_BRACKET_ITEM    4
#define RIGHT_BRACKET_ITEM     5
#define CONTROL_ITEM           6
#define FLOAT_ITEM             7

#define LSUB_ITEM              8
#define LSUP_ITEM              9
#define RSUB_ITEM             10
#define RSUP_ITEM             11
#define GLUE_LSUBS_ITEM       12
#define GLUE_RSUBS_ITEM       13
#define GLUE_LEFT_ITEM        14
#define GLUE_RIGHT_ITEM       15
#define GLUE_BOTH_ITEM        16

extern int line_item_count;
class line_item;
class line_item_rep: public concrete_struct {
public:
  int        type;    // type of the line item
  box        b;       // the box
  space      spc;     // separation space
  int        penalty; // penalty for a linebreak after this line_item
  bool       limits;  // line items has limits
  language   lan;     // language for hyphenating strings
  tree       t;       // for control items

  line_item_rep (int type, box b, int penalty);
  line_item_rep (int type, box b, int penalty, language lan);
  line_item_rep (int type, box b, int penalty, tree t);
  ~line_item_rep ();
};

class line_item {
  CONCRETE_NULL(line_item);
  line_item (int type, box b, int penalty);
  line_item (int type, box b, int penalty, language lan);
  line_item (int type, box b, int penalty, tree t);
  bool operator == (line_item item2);
  bool operator != (line_item item2);
};
CONCRETE_NULL_CODE(line_item);

tm_ostream& operator << (tm_ostream& out, line_item item);

#endif // defined LINE_ITEM_H
