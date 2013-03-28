
/******************************************************************************
* MODULE     : brush.cpp
* DESCRIPTION: brushes for painting
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "brush.hpp"
#include "gui.hpp"

brush_rep::brush_rep (tree p, int a):
  kind (brush_none), c (0xffffffff), pattern (p), alpha (a)
{
  if (p == "");
  else if (is_atomic (p)) {
    kind= brush_color;
    c= named_color (p->label, a);
  }
  else if (is_func (p, PATTERN)) {
    kind= brush_pattern;
    if (N(p) == 4) c= named_color (as_string (p[3]), a);
  }
}
