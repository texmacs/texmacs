
/******************************************************************************
* MODULE     : stack_border.hpp
* DESCRIPTION: Border properties of a stack of page items
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef STACK_BORDER_H
#define STACK_BORDER_H
#include "space.hpp"

class stack_border_rep: public concrete_struct {
public:
  SI    height;  // default distance between successive base lines
  SI    sep;     // (~~PAR_SEP) sep-ver_sep is maximal amount of shoving
  SI    hor_sep; // min. hor. ink sep. when lines are shoved into each other
  SI    ver_sep; // minimal separation of ink
  SI    bot;     // logical bottom of lines
  SI    top;     // logical top of lines

  SI    height_before;
  SI    sep_before;
  SI    hor_sep_before;
  SI    ver_sep_before;

  space vspc_before, vspc_after;
  bool  nobr_before, nobr_after;

  inline stack_border_rep ():
    height (0), sep (0), hor_sep (0), ver_sep (0), bot (0), top (0),
    height_before (0), sep_before (0), hor_sep_before (0), ver_sep_before (0),
    vspc_before (0), vspc_after (0),
    nobr_before (false), nobr_after (false) {}
};

class stack_border {
  CONCRETE(stack_border);
  inline stack_border (): rep (tm_new<stack_border_rep> ()) {}
};
CONCRETE_CODE(stack_border);

inline tm_ostream&
operator << (tm_ostream& out, stack_border sb) {
  return out << "[Before: " << sb->vspc_before
	     << ", " << sb->nobr_before << "; "
	     << "After: " << sb->vspc_after
	     << ", " << sb->nobr_after << "]";
}

#endif // defined STACK_BORDER_H
