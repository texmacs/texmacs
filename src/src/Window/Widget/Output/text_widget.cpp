
/******************************************************************************
* MODULE     : text_widget.cpp
* DESCRIPTION: Text widgets for output only
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Widget/basic_widget.hpp"
#include "Widget/layout.hpp"
#include "window.hpp"
#include "font.hpp"
#include "analyze.hpp"

/******************************************************************************
* Text widgets
******************************************************************************/

class text_widget_rep: public basic_widget_rep {
  string  original, s;
  color   col;
  bool    transparent;
  string  lan;
  bool    tt;
  metric  ex;
  int     dw, dh;

public:
  text_widget_rep (display dis, string s, color col, bool trans,
		   string lan, bool tt, int dw, int dh);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_repaint (repaint_event ev);
};

text_widget_rep::text_widget_rep (
  display dis, string s2, color c2, bool t2,
  string l2, bool tt2, int dw2, int dh2):
    basic_widget_rep (dis, south_west),
    original (s2), s (s2), col (c2),
    transparent (t2), lan (l2), tt (tt2),
    dw (dw2+2*PIXEL), dh (dh2+2*PIXEL) {}

text_widget_rep::operator tree () {
  return tree (TUPLE, "text", s);
}

void
text_widget_rep::handle_get_size (get_size_event ev) {
  s= tm_var_encode (dis->translate (original, lan, dis->out_lan));
  font fn= dis->default_font (tt);
  fn->var_get_extents (s, ex);
  ev->w = ((ex->x2- ex->x1+ 2)/3)+ 2*dw;
  ev->h = ((fn->y2- fn->y1+ 2)/3)+ 2*dh;
  abs_round (ev->w, ev->h);
}

void
text_widget_rep::handle_repaint (repaint_event ev) { (void) ev;
  if (!transparent) layout_default (win, 0, 0, w, h);
  win->set_color (col);
  font fn= dis->default_font (tt);
  win->set_shrinking_factor (3);
  fn ->var_draw (win, s, 3*dw- ex->x1, 3*dh- fn->y1);
  win->set_shrinking_factor (1);
}

/******************************************************************************
* Interface
******************************************************************************/

widget
text_widget (string s, bool tsp, string lan) {
  display dis= current_display ();
  return new text_widget_rep (dis, s, dis->black, tsp, lan, false, 3*PIXEL, 0);
}

widget
menu_text_widget (string s, color col, string lan, bool tt) {
  display dis= current_display ();
  return new text_widget_rep (dis, s, col, true, lan, tt, 3*PIXEL, 0);
}
