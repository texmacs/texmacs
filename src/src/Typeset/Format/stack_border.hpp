
/******************************************************************************
* MODULE     : stack_border.hpp
* DESCRIPTION: Border properties of a stack of page items
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef STACK_BORDER_H
#define STACK_BORDER_H
#include "space.hpp"

class stack_border_rep: public concrete_struct {
public:
  SI    height;  // default distance between successive base lines
  SI    sep;     // minimal separation of ink
  SI    hor_sep; // min. hor. ink sep. when lines are shoved into each other
  SI    bot;     // logical bottom of lines
  SI    top;     // logical top of lines

  space vspc_before, vspc_after;
  bool  nobr_before, nobr_after;

  inline stack_border_rep ():
    height (0), sep (0), hor_sep (0), bot (0), top (0),
    vspc_before (0), vspc_after (0),
    nobr_before (false), nobr_after (false) {}
};

class stack_border {
  CONCRETE(stack_border);
  inline stack_border (): rep (new stack_border_rep ()) {}
};
CONCRETE_CODE(stack_border);

#endif // defined STACK_BORDER_H
