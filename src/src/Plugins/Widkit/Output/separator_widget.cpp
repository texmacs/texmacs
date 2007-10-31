
/******************************************************************************
* MODULE     : separator_widget.cpp
* DESCRIPTION: Widgets for filling up space between a group of widgets.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "gui.hpp"
#include "ps_device.hpp"
#include "window.hpp"
#include "Widkit/attribute_widget.hpp"
#include "Widkit/layout.hpp"

/******************************************************************************
* Glue widgets
******************************************************************************/

class separator_widget_rep: public attribute_widget_rep {
  SI   pre, post;
  bool vert;

public:
  separator_widget_rep (SI pre, SI post, bool vert);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_repaint (repaint_event ev);
  void handle_get_coord2 (get_coord2_event ev);
  void handle_set_coord2 (set_coord2_event ev);
};

separator_widget_rep::separator_widget_rep (
  SI pre2, SI post2, bool vert2):
    attribute_widget_rep (), pre (pre2), post (post2), vert (vert2) {}

separator_widget_rep::operator tree () {
  return "separator";
}

void
separator_widget_rep::handle_get_size (get_size_event ev) {
  if (vert) {
    if (ev->mode==1) gui_maximal_extents (ev->w, ev->h);
    ev->w= 2*PIXEL+ pre+ post;
    ev->h= 0;
  }
  else {
    if (ev->mode==-1) ev->w= 0;
    if (ev->mode==1) gui_maximal_extents (ev->w, ev->h);
    ev->h= 2*PIXEL+ pre+ post;
  }
}

void
separator_widget_rep::handle_repaint (repaint_event ev) {
  layout_default (win, ev->x1, ev->y1, ev->x2, ev->y2);
  win->set_color (layout_dark (win));
  win->set_line_style (PIXEL);
  if (vert) win->line (pre+PIXEL, ev->y1, pre+PIXEL, ev->y2);
  else win->line (ev->x1, -pre-PIXEL, ev->x2, -pre-PIXEL);
  win->set_color (white);
  win->set_line_style (PIXEL);
  if (vert) win->line (pre+2*PIXEL, ev->y1, pre+2*PIXEL, ev->y2);
  else win->line (ev->x1, -pre-2*PIXEL, ev->x2, -pre-2*PIXEL);
}

void
separator_widget_rep::handle_get_coord2 (get_coord2_event ev) {
  if (ev->which != "extra width") attribute_widget_rep::handle_get_coord2 (ev);
  else { ev->c1= 0; ev->c2= 0; }
}

void
separator_widget_rep::handle_set_coord2 (set_coord2_event ev) {
  if (ev->which != "extra width") attribute_widget_rep::handle_set_coord2 (ev);
}

/******************************************************************************
* Interface
******************************************************************************/

wk_widget
separator_wk_widget (SI pre, SI post, bool vert) {
  return new separator_widget_rep (pre, post, vert);
}
