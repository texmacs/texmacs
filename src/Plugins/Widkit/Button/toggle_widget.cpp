
/******************************************************************************
* MODULE     : toggle_widget.cpp
* DESCRIPTION: Toggles
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "renderer.hpp"
#include "Widkit/layout.hpp"
#include "Widkit/basic_widget.hpp"
#include "Scheme/object.hpp"

/******************************************************************************
* Command buttons
******************************************************************************/

class toggle_widget_rep: public basic_widget_rep {
  command cmd;
  bool on;
  int style;
  SI sz;
  SI dy;
public:
  toggle_widget_rep (command cmd, bool on, int style);
  operator tree () { return tree ("toggle_widget"); }
  void handle_get_size (get_size_event ev);
  void handle_repaint (repaint_event ev);
  void handle_mouse (mouse_event ev);
};

toggle_widget_rep::toggle_widget_rep (command cmd2, bool on2, int style2):
  basic_widget_rep (south_west),
  cmd (cmd2), on (on2), style (style2), dy (3*PIXEL)
{
  bool mini= (style & WIDGET_STYLE_MINI) != 0;
  if (mini) sz= 11*PIXEL;
  else sz= 14*PIXEL;
}

void
toggle_widget_rep::handle_get_size (get_size_event ev) {
  ev->w= sz;
  ev->h= sz + dy;
}

void
toggle_widget_rep::handle_repaint (repaint_event ev) { (void) ev;
  renderer ren= win->get_renderer ();
  layout_default (ren, 0, 0, w, h);
  layout_pastel_lower (ren, 0, dy, sz, dy + sz);
  if (on) {
    color dark= layout_dark (ren);
    ren->set_color (dark);
    ren->line (3*PIXEL, dy + 3*PIXEL, sz - 4*PIXEL, dy + sz - 4*PIXEL);
    ren->line (3*PIXEL, dy + sz - 4*PIXEL, sz - 4*PIXEL, dy + 3*PIXEL);
  }
}

void
toggle_widget_rep::handle_mouse (mouse_event ev) {
  string type= ev->type;
  SI     x= ev->x, y= ev->y;
  bool   inside= (y>=0) && (y<h) && (x>=0) && (x<w);
  bool   pressed= (type == "release-left" || type == "release-right");
  if (inside && pressed) {
    on= !on;
    if (attached ()) this << emit_invalidate_all ();
    if (!is_nil (cmd)) cmd (list_object (object (on)));
  }
}

/******************************************************************************
* Interface
******************************************************************************/

wk_widget
toggle_wk_widget (command cmd, bool on, int style) {
  return tm_new<toggle_widget_rep> (cmd, on, style);
}
