
/******************************************************************************
* MODULE     : balloon.cpp
* DESCRIPTION: Balloon widgets for showing help
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "window.hpp"
#include "Widkit/basic_widget.hpp"

/******************************************************************************
* Balloon widgets
******************************************************************************/

class balloon_widget_rep: public basic_widget_rep {
  wk_widget balloon_wid;

public:
  balloon_widget_rep (wk_widget w, wk_widget help);
  operator tree ();

  void balloon_handle_mouse (mouse_event ev);
  bool handle (event ev);
};

/******************************************************************************
* Implementation of balloon widgets
******************************************************************************/

balloon_widget_rep::balloon_widget_rep (wk_widget w, wk_widget help):
  basic_widget_rep (1)
{
  a[0]       = w;
  balloon_wid= help;
}

balloon_widget_rep::operator tree () {
  return tree (TUPLE, "balloon", (tree) a[0], balloon_wid);
}

void
balloon_widget_rep::balloon_handle_mouse (mouse_event ev) {
  if (/* (!ev->pressed ("left")) &&
	 (!ev->pressed ("right")) && */
      ((ev->type == "enter") || (ev->type == "move"))) {
    SI x, y;
    win->get_position (x, y);
    SI xx= x+ ev->x+ 10*PIXEL;
    SI yy= y+ ev->y- 5*PIXEL;
    set_help_balloon (abstract (balloon_wid), xx, yy);
  }
}

bool
balloon_widget_rep::handle (event ev) {
  switch (ev->type) {
  case MOUSE_EVENT: {
    mouse_event e (ev);
    balloon_handle_mouse (e);
  }
  case GET_SIZE_EVENT:
  case SET_WIDGET_EVENT:
  case ATTACH_WINDOW_EVENT:
  case POSITION_EVENT:
  case UPDATE_EVENT:
  case INVALIDATE_EVENT:
  case REPAINT_EVENT:
  case FIND_CHILD_EVENT:
    return basic_widget_rep::handle (ev);
  default:
    a[0] << ev;
    return true;
  }
}

/******************************************************************************
* Balloon decoration
******************************************************************************/

class balloon_decoration_widget_rep: public basic_widget_rep {
public:
  balloon_decoration_widget_rep (wk_widget w);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_position (position_event ev);
  void handle_repaint (repaint_event ev);
};

/******************************************************************************
* Implementation of balloon decoration widgets
******************************************************************************/

balloon_decoration_widget_rep::balloon_decoration_widget_rep (wk_widget w):
  basic_widget_rep (1, south_west) { a[0]= w; }

balloon_decoration_widget_rep::operator tree () {
  return tree (TUPLE, "decoration", (tree) a[0]);
}

void
balloon_decoration_widget_rep::handle_get_size (get_size_event ev) {
  basic_widget_rep::handle_get_size (ev);
  ev->w += 4*PIXEL;
  ev->h += 4*PIXEL;
}

void
balloon_decoration_widget_rep::handle_position (position_event ev) {
  (void) ev;
  SI ww= w-4*PIXEL;
  SI hh= h-4*PIXEL;
  a[0] << emit_position (2*PIXEL, 2*PIXEL, ww, hh, south_west);
}

void
balloon_decoration_widget_rep::handle_repaint (repaint_event ev) {
  (void) ev;
  win->set_background (rgb_color (255, 255, 160));
  win->clear (0, 0, w, h);
  win->set_color (black);
  win->line (0, 0, w-PIXEL, 0);
  win->line (0, h-PIXEL, w-PIXEL, h-PIXEL);
  win->line (0, 0, 0, h);
  win->line (w-PIXEL, 0, w-PIXEL, h-PIXEL);
}

/******************************************************************************
* exported routines
******************************************************************************/

wk_widget
balloon_widget (wk_widget w, wk_widget help) {
  return new balloon_widget_rep (w, new balloon_decoration_widget_rep (help));
}
