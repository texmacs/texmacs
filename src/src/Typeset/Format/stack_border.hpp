
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
  SI    sep;     // (~~PAR_SEP) sep-ver_sep is maximal amount of shoving
  SI    hor_sep; // min. hor. ink sep. when lines are shoved into each other
  SI    ver_sep; // minimal separation of ink
  SI    bot;     // logical bottom of lines
  SI    top;     // logical top of lines

  space vspc_before, vspc_after;
  bool  nobr_before, nobr_after;

  inline stack_border_rep ():
    height (0), sep (0), hor_sep (0), ver_sep (0), bot (0), top (0),
    vspc_before (0), vspc_after (0),
    nobr_before (false), nobr_after (false) {}
  inline stack_border_rep (SI h, SI s, SI hsep, SI vsep, SI b, SI t):
    height (h), sep (s), hor_sep (hsep), ver_sep (vsep), bot (b), top (t) {}
};

class stack_border {
  CONCRETE(stack_border);
  inline stack_border (): rep (new stack_border_rep ()) {}
  inline stack_border (SI h, SI s, SI hsep, SI vsep, SI b, SI t):
    rep (new stack_border_rep (h, s, hsep, vsep, b, t)) {}
};
CONCRETE_CODE(stack_border);

// FIXME: from TeXmacs-1.0.4.1 on, the separation parameters between
// successive lines are the maximum of the parameters for each line.
// This may be further refined by allowing a "par-sep before and after",
// and similarly for par-hor-sep, par-ver-sep, etc. Ideally speaking,
// the parameters would be determined for individual boxes on each line
// and the maxima of the individual values are taken on each line.

inline stack_border
max (stack_border above, stack_border below) {
  return
    stack_border (max (above->height , below->height),
		  max (above->sep    , below->sep),
		  max (above->hor_sep, below->hor_sep),
		  max (above->ver_sep, below->ver_sep),
		  above->bot, above->top);
}

#endif // defined STACK_BORDER_H
