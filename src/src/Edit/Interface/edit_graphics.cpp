
/******************************************************************************
* MODULE     : edit_graphics.cpp
* DESCRIPTION: graphics between the editor and the window manager
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Interface/edit_graphics.hpp"
#include "server.hpp"
#include "scheme.hpp"
#include <math.h>

/******************************************************************************
* Constructors and destructors
******************************************************************************/

edit_graphics_rep::edit_graphics_rep () {}
edit_graphics_rep::~edit_graphics_rep () {}

/******************************************************************************
* Main edit_graphics routines
******************************************************************************/

bool
edit_graphics_rep::inside_graphics () {
  path p   = path_up (tp);
  bool flag= false;
  tree st  = et;
  while (!nil (p)) {
    if (is_func (st, GRAPHICS)) flag= true;
    if (is_func (st, TEXT_AT )) flag= false;
    st= st[p->item];
    p = p->next;
  }
  return flag || (L(st) == GRAPHICS);
}

frame
edit_graphics_rep::find_frame () {
  bool bp_found;
  path bp= eb->find_box_path (tp, bp_found);
  if (bp_found) return eb->find_frame (path_up (bp));
  else return frame ();
}

void
edit_graphics_rep::find_limits (point& lim1, point& lim2) {
  lim1= point (); lim2= point ();
  bool bp_found;
  path bp= eb->find_box_path (tp, bp_found);
  if (bp_found) eb->find_limits (path_up (bp), lim1, lim2);
}

point
edit_graphics_rep::adjust (point p) {
  double x= floor (10.0*p[0] + 0.5);
  double y= floor (10.0*p[1] + 0.5);
  return point (x / 10.0, y / 10.0);
}

tree
edit_graphics_rep::find_point (point p) {
  return tree (_POINT, as_string (p[0]), as_string (p[1]));
}

bool
edit_graphics_rep::mouse_graphics (string type, SI x, SI y, time_t t) {
  (void) t;
  frame f= find_frame ();
  if (!nil (f)) {
    point lim1, lim2;
    find_limits (lim1, lim2);
    point p = adjust (f [point (x, y)]);
    // cout << type << " at " << p << " [" << lim1 << ", " << lim2 << "]\n";
    if (N(lim1) == 2)
      if ((p[0]<lim1[0]) || (p[0]>lim2[0]) ||
	  (p[1]<lim1[1]) || (p[1]>lim2[1]))
	return false;
    string sx= as_string (p[0]);
    string sy= as_string (p[1]);
    //if ((type == "move") && (!win->check_event (MOTION_EVENT)))
    //call ("graphics-move-point", sx, sy);
    if (type == "release-left"  ) call ("graphics-insert-point", sx, sy);
    if (type == "release-middle") call ("graphics-remove-point", sx, sy);
    if (type == "release-right" ) call ("graphics-last-point"  , sx, sy);
    notify_change (THE_CURSOR);
    return true;
  }
  return false;
}
