
/******************************************************************************
* MODULE     : page_item.hpp
* DESCRIPTION: A typesetted document consists of an array of page_items.
*              Each page item contains spacing and page breaking information.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PAGE_ITEM_H
#define PAGE_ITEM_H
#include "boxes.hpp"
#include "formatter.hpp"

#define PAGE_LINE_ITEM      0
#define PAGE_HIDDEN_ITEM    1
#define PAGE_CONTROL_ITEM   2

class page_item;
class page_item_rep: public concrete_struct {
public:
  int          type;    // type of the page item

  box          b;       // the box
  space        spc;     // separation space
  int          penalty; // penalty for a linebreak after this page_item

  array<lazy>  fl;      // floating objects attached to this item
  int          nr_cols; // number of columns
  tree         t;       // for page control items

  page_item_rep (box b, array<lazy> fl, int nr_cols);
  page_item_rep (tree t, int nr_cols);
  page_item_rep (int type, box b, space spc, int pen,
		 array<lazy> fl, int nr_cols, tree t);
};

class page_item {
  CONCRETE_NULL(page_item);
  page_item (box b, array<lazy> fl= 0, int nr_cols= 1);
  page_item (tree t, int nr_cols);
  page_item (int type, box b, space spc, int penalty,
	     array<lazy> fl, int nr_cols, tree t);
  bool operator == (page_item item2);
  bool operator != (page_item item2);
  friend page_item copy (page_item l);
};
CONCRETE_NULL_CODE(page_item);

tm_ostream& operator << (tm_ostream& out, page_item item);

#endif // defined PAGE_ITEM_H
