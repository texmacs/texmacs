
/******************************************************************************
* MODULE     : xpm_widget.cpp
* DESCRIPTION: Xpm widgets for output only
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "ps_device.hpp"
#include "image_files.hpp"
#include "Widkit/basic_widget.hpp"
#include "Widkit/layout.hpp"

/******************************************************************************
* Xpm widgets
******************************************************************************/

class xpm_widget_rep: public basic_widget_rep {
  url  name;
  bool transparent;
  SI   dx, dy;
  int  ww, hh;

public:
  xpm_widget_rep (url name, bool trans, int dx, int dy);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_repaint (repaint_event ev);
};

xpm_widget_rep::xpm_widget_rep (
  url name2, bool t2, int dx2, int dy2):
    basic_widget_rep (center), name (name2),
    transparent (t2), dx (dx2), dy (dy2)
{
  xpm_size (name, ww, hh);
  ww= ((ww+1) >> 1) << 1;
  hh= ((hh+1) >> 1) << 1;
}

xpm_widget_rep::operator tree () {
  return tree (TUPLE, "xpm", as_string (name));
}

void
xpm_widget_rep::handle_get_size (get_size_event ev) {
  ev->w = ww*PIXEL+ 2*dx;
  ev->h = hh*PIXEL+ 2*dy;
  abs_round (ev->w, ev->h);  
}

void
xpm_widget_rep::handle_repaint (repaint_event ev) { (void) ev;
  if (!transparent) layout_default (win, -(w>>1), -(h>>1), w>>1, h>>1);
  win->xpm (name, -(ww>>1)*PIXEL, (hh>>1)*PIXEL);
}

/******************************************************************************
* Interface
******************************************************************************/

wk_widget
xpm_wk_widget (url name, bool transp) {
  return new xpm_widget_rep (name, transp, PIXEL, PIXEL);
}
