
/******************************************************************************
* MODULE     : button_widget.gen.h
* DESCRIPTION: Buttons
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef BUTTON_WIDGET_H
#define BUTTON_WIDGET_H
#include "Widget/attribute_widget.hpp"

/******************************************************************************
* Abstract button_widgets
******************************************************************************/

class button_widget_rep: public attribute_widget_rep {
protected:
  SI     X1, Y1, X2, Y2;
  SI     extra_left;
  SI     extra_right;
  bool   rflag;
  bool   button_flag;
  bool   enabled;
  bool   centered;

public:
  bool   status;
  bool   inside;

  button_widget_rep (widget w, bool rflag=false, bool button_flag= false);
  button_widget_rep (widget lw, widget rw);
  button_widget_rep (widget lw, widget cw, widget rw,
		     bool e=true, bool c=false);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_position (position_event ev);
  void handle_repaint (repaint_event ev);
  void handle_get_coord2 (get_coord2_event ev);
  void handle_set_coord2 (set_coord2_event ev);
};

#endif // defined BUTTON_WIDGET_H
