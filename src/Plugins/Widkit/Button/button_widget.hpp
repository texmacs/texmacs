
/******************************************************************************
* MODULE     : button_widget.hpp
* DESCRIPTION: Buttons
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef BUTTON_WIDGET_H
#define BUTTON_WIDGET_H
#include "Widkit/attribute_widget.hpp"

/******************************************************************************
* Abstract button_widgets
******************************************************************************/

class button_widget_rep: public attribute_widget_rep {
protected:
  SI     X1, Y1, X2, Y2;
  SI     extra_left;
  SI     extra_right;
  bool   rflag;
  int    style;
  bool   button_flag;
  bool   enabled;
  bool   centered;
  bool   has_pull_down;

public:
  bool   status;
  bool   inside;

  button_widget_rep (wk_widget w, bool rflag=false,
		     int style= 0, bool button_flag= false);
  button_widget_rep (wk_widget lw, wk_widget rw,
		     int style= 0);
  button_widget_rep (wk_widget lw, wk_widget cw, wk_widget rw,
		     int style= 0, bool e=true, bool c=false);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_position (position_event ev);
  void handle_repaint (repaint_event ev);
  void handle_get_coord2 (get_coord2_event ev);
  void handle_set_coord2 (set_coord2_event ev);
};

#endif // defined BUTTON_WIDGET_H
