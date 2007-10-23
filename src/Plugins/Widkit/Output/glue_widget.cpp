
/******************************************************************************
* MODULE     : glue_widget.cpp
* DESCRIPTION: Widgets for filling up space between a group of widgets.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "display.hpp"
#include "Widkit/layout.hpp"
#include "Widkit/basic_widget.hpp"

/******************************************************************************
* Glue widgets
******************************************************************************/

class glue_widget_rep: public basic_widget_rep {
  bool hflag, vflag;  // may be extended horizontally resp. vertically
  SI   minw, minh;    // minimal width and height in pixels

public:
  glue_widget_rep (bool hflag, bool vflag, SI minw, SI minh);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_repaint (repaint_event ev);
};

glue_widget_rep::glue_widget_rep (bool hflag2, bool vflag2, SI w2, SI h2):
  basic_widget_rep (),
  hflag (hflag2), vflag (vflag2), minw (w2), minh (h2) {}

glue_widget_rep::operator tree () {
  return "glue";
}

void
glue_widget_rep::handle_get_size (get_size_event ev) {
  if (ev->mode==0) {
    if (!hflag) ev->w= minw;
    if (!vflag) ev->h= minh;
  }
  if (ev->mode==-1) {
    ev->w= minw;
    ev->h= minh;
  }
  if (ev->mode==1) {
    the_display->get_max_size (ev->w, ev->h);
    if (!hflag) ev->w= minw;
    if (!vflag) ev->h= minh;
  }
}

void
glue_widget_rep::handle_repaint (repaint_event ev) {
  layout_default (win, ev->x1, ev->y1, ev->x2, ev->y2);
}

/******************************************************************************
* Interface
******************************************************************************/

wk_widget
glue_wk_widget (bool hflag, bool vflag, SI minw, SI minh) {
  return new glue_widget_rep (hflag, vflag, minw, minh);
}
