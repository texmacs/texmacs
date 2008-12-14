
/******************************************************************************
* MODULE     : scroll_widget.hpp
* DESCRIPTION: Scrollable widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SCROLL_WIDGET_H
#define SCROLL_WIDGET_H
#include "Widkit/attribute_widget.hpp"
#include "Widkit/Event/scroll_event.hpp"

/******************************************************************************
* Scroll widgets
******************************************************************************/

class scroll_widget_rep: public attribute_widget_rep {
public:
  scroll_widget_rep (array<wk_widget> a, gravity grav);
  virtual void handle_scroll (scroll_event ev) = 0;
  virtual bool handle (event ev);
};

/******************************************************************************
* Scrollable widgets
******************************************************************************/

class scrollable_widget_rep: public scroll_widget_rep {
  SI             scx, scy;    // scroll x,y position
  SI             ex1, ey1;    // extents of scrolled window lo-left
  SI             ex2, ey2;    // extents of scrolled window hi-right
  wk_widget_rep* hor;         // the horizontal scroll bar
  wk_widget_rep* ver;         // the vertical scroll bar
  gravity        backup;      // for a dirty bugfix

  void scroll_to (SI scx, SI scy);
  void set_extents (SI ex1, SI ey1, SI ex2, SI ey2);
  void scroll_event_hor (SI& x, SI& before, SI& after);
  void scroll_event_ver (SI& y, SI& before, SI& after);

public:
  scrollable_widget_rep (wk_widget child, gravity grav);
  operator tree ();

  void handle_get_size   (get_size_event ev);
  void handle_position   (position_event ev);
  void handle_set_widget (set_widget_event ev);
  void handle_get_coord1 (get_coord1_event ev);
  void handle_get_coord2 (get_coord2_event ev);
  void handle_get_coord4 (get_coord4_event ev);
  void handle_set_coord2 (set_coord2_event ev);
  void handle_set_coord4 (set_coord4_event ev);
  void handle_scroll     (scroll_event ev);
};

/******************************************************************************
* Abstract scrollbars
******************************************************************************/

class scrollbar_rep: public scroll_widget_rep {
protected:
  wk_widget_rep* ref;
  SI                 sc_min, sc_max, sc_pos, before, after;
  double             factor;
  bool               gripped;
  bool               scrolling;
  int                increment;

public:
  scrollbar_rep (wk_widget ref);

  void handle_set_coord1 (set_coord1_event ev);
  void handle_set_coord2 (set_coord2_event ev);
};

/******************************************************************************
* Horizontal scrollbars
******************************************************************************/

class hor_scrollbar_widget_rep: public scrollbar_rep {
  void decode_position (SI& x1, SI& x2);
  SI   encode_position (SI x);

public:
  hor_scrollbar_widget_rep (wk_widget ref);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_repaint (repaint_event ev);
  void handle_mouse (mouse_event ev);
  void handle_scroll (scroll_event ev);
  void handle_alarm (alarm_event ev);
};

/******************************************************************************
* Vertical scrollbars
******************************************************************************/

class ver_scrollbar_widget_rep: public scrollbar_rep {
  void decode_position (SI& y1, SI& y2);
  SI   encode_position (SI y);

public:
  ver_scrollbar_widget_rep (wk_widget ref);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_repaint (repaint_event ev);
  void handle_mouse (mouse_event ev);
  void handle_scroll (scroll_event ev);
  void handle_alarm (alarm_event ev);
};

#endif // defined SCROLL_WIDGET_H
