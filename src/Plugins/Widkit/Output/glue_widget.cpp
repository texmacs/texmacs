
/******************************************************************************
* MODULE     : glue_widget.cpp
* DESCRIPTION: Widgets for filling up space between a group of widgets.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gui.hpp"
#include "window.hpp"
#include "Widkit/layout.hpp"
#include "Widkit/basic_widget.hpp"

/******************************************************************************
* Glue widgets
******************************************************************************/

class glue_widget_rep: public basic_widget_rep {
  tree col;           // color or empty string for default background
  bool hflag, vflag;  // may be extended horizontally resp. vertically
  SI   minw, minh;    // minimal width and height in pixels

public:
  glue_widget_rep (bool hflag, bool vflag, SI minw, SI minh);
  glue_widget_rep (tree col, bool hflag, bool vflag, SI minw, SI minh);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_repaint (repaint_event ev);
};

glue_widget_rep::glue_widget_rep (bool hflag2, bool vflag2, SI w2, SI h2):
  basic_widget_rep (),
  col (""), hflag (hflag2), vflag (vflag2), minw (w2), minh (h2) {}

glue_widget_rep::glue_widget_rep (tree c2, bool hf2, bool vf2, SI w2, SI h2):
  basic_widget_rep (),
  col (c2), hflag (hf2), vflag (vf2), minw (w2), minh (h2) {}

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
    gui_maximal_extents (ev->w, ev->h);
    if (!hflag) ev->w= minw;
    if (!vflag) ev->h= minh;
  }
}

void
glue_widget_rep::handle_repaint (repaint_event ev) {
  renderer ren= win->get_renderer ();
  if (col == "")
    layout_default (ren, ev->x1, ev->y1, ev->x2, ev->y2);
  else {
    if (is_atomic (col)) {
      color c= named_color (col->label);
      ren->set_background (c);
      ren->set_color (c);
      ren->fill (ev->x1, ev->y1, ev->x2, ev->y2);
    }
    else {
      ren->set_shrinking_factor (5);
      tree old_bg= ren->get_background_pattern ();
      ren->set_background_pattern (col);
      ren->clear_pattern (5*ev->x1, 5*ev->y1, 5*ev->x2, 5*ev->y2);
      ren->set_background_pattern (old_bg);
      ren->set_shrinking_factor (1);
    }
  }
}

/******************************************************************************
* Interface
******************************************************************************/

wk_widget
glue_wk_widget (bool hflag, bool vflag, SI minw, SI minh) {
  return tm_new<glue_widget_rep> (hflag, vflag, minw, minh);
}

wk_widget
glue_wk_widget (tree col, bool hflag, bool vflag, SI minw, SI minh) {
  return tm_new<glue_widget_rep> (col, hflag, vflag, minw, minh);
}
