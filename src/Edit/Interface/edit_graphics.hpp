
/******************************************************************************
* MODULE     : edit_graphics.hpp
* DESCRIPTION: the interface for TeXmacs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef EDIT_GRAPHICS_H
#define EDIT_GRAPHICS_H
#include "editor.hpp"
#include "timer.hpp"

class edit_graphics_rep: virtual public editor_rep {
protected:
  point cur_pos;

public:
  edit_graphics_rep ();
  ~edit_graphics_rep ();

  bool   inside_graphics ();
  frame  find_frame ();
  void   find_limits (point& lim1, point& lim2);
  point  adjust (point p);
  tree   find_point (point p);
  bool   mouse_graphics (string s, SI x, SI y, time_t t);
};

#endif // defined EDIT_GRAPHICS_H
