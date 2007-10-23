
/******************************************************************************
* MODULE     : wait.cpp
* DESCRIPTION: Popup wait indicators
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "display.hpp"
#include "analyze.hpp"
#include "window.hpp"
#include "ps_device.hpp"
#include "font.hpp"
#include "Widkit/basic_widget.hpp"

/******************************************************************************
* Wait widgets
******************************************************************************/

class wait_widget_rep: public basic_widget_rep {
  string wait_s;
  string message;
public:
  wait_widget_rep (SI width, SI height, string message);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_repaint (repaint_event ev);
};

/******************************************************************************
* Implementation of wait decoration widgets
******************************************************************************/

wait_widget_rep::wait_widget_rep (SI width, SI height, string s):
  basic_widget_rep (0, south_west), message (s)
{
  w= width; h= height;
  wait_s = the_display->translate ("please wait", "english",
				   the_display->out_lan);
  message= tm_var_encode (the_display->translate (s, "english",
						  the_display->out_lan));
  wait_s= upcase_all (wait_s);
}

wait_widget_rep::operator tree () {
  return tree (TUPLE, "wait indicator", message);
}

void
wait_widget_rep::handle_get_size (get_size_event ev) {
  ev->w= w;
  ev->h= h;
}

extern font the_default_wait_font;

void
wait_widget_rep::handle_repaint (repaint_event ev) {
  (void) ev;
  win->set_background (the_display->rgb (255, 255, 160));
  win->clear (0, 0, w, h);
  win->set_color (the_display->black);
  win->line (0, 0, w-PIXEL, 0);
  win->line (0, h-PIXEL, w-PIXEL, h-PIXEL);
  win->line (0, 0, 0, h);
  win->line (w-PIXEL, 0, w-PIXEL, h-PIXEL);

  font fn= the_default_wait_font;
  win->set_shrinking_factor (3);
  metric ex;
  fn->var_get_extents (wait_s, ex);
  SI x= (3*w - (ex->x1+ex->x2)) >> 1;
  SI y= 2*h - ((ex->y1+ex->y2) >> 1);
  win->set_color (the_display->red);
  fn->var_draw (win, wait_s, x, y);
  fn->var_get_extents (message, ex);
  x= (3*w - (ex->x1+ex->x2)) >> 1;
  y= h - ((ex->y1+ex->y2) >> 1);
  win->set_color (the_display->black);
  fn->var_draw (win, message, x, y);
  win->set_shrinking_factor (1);
}

/******************************************************************************
* exported routines
******************************************************************************/

wk_widget
wait_wk_widget (SI width, SI height, string message) {
  return new wait_widget_rep (width, height, message);
}
