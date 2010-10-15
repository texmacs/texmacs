
/******************************************************************************
* MODULE     : wait.cpp
* DESCRIPTION: Popup wait indicators
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "analyze.hpp"
#include "gui.hpp"
#include "window.hpp"
#include "renderer.hpp"
#include "font.hpp"
#include "Widkit/basic_widget.hpp"
#include "dictionary.hpp"

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
  string out_lan= get_output_language ();
  wait_s = translate ("please wait");
  message= tm_var_encode (s);
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
  renderer ren= win->get_renderer ();
  ren->set_background (rgb_color (255, 255, 160));
  ren->clear (0, 0, w, h);
  ren->set_color (black);
  ren->line (0, 0, w-PIXEL, 0);
  ren->line (0, h-PIXEL, w-PIXEL, h-PIXEL);
  ren->line (0, 0, 0, h);
  ren->line (w-PIXEL, 0, w-PIXEL, h-PIXEL);

  font fn= the_default_wait_font;
  ren->set_shrinking_factor (3);
  metric ex;
  fn->var_get_extents (wait_s, ex);
  SI x= (3*w - (ex->x1+ex->x2)) >> 1;
  SI y= 2*h - ((ex->y1+ex->y2) >> 1);
  ren->set_color (red);
  fn->var_draw (ren, wait_s, x, y);
  fn->var_get_extents (message, ex);
  x= (3*w - (ex->x1+ex->x2)) >> 1;
  y= h - ((ex->y1+ex->y2) >> 1);
  ren->set_color (black);
  fn->var_draw (ren, message, x, y);
  ren->set_shrinking_factor (1);
}

/******************************************************************************
* exported routines
******************************************************************************/

wk_widget
wait_wk_widget (SI width, SI height, string message) {
  return tm_new<wait_widget_rep> (width, height, message);
}
