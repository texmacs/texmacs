
/******************************************************************************
* MODULE     : button_widget.cpp
* DESCRIPTION: Buttons
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Widget/Button/button_widget.hpp"
#include "Event/composite_event.hpp"
#include "ps_device.hpp"
#include "Widget/layout.hpp"

/******************************************************************************
* Routines for abstract button_widgets
******************************************************************************/

button_widget_rep::button_widget_rep (widget w2, bool rf2, bool bf2):
  attribute_widget_rep (w2->dis, 1, south_west),
  extra_left (0), extra_right (0), rflag (rf2), button_flag (bf2),
  enabled(true), centered(false), status (false), inside (false)
{ a[0]= w2; }
    
button_widget_rep::button_widget_rep (widget lw, widget rw):
  attribute_widget_rep (lw->dis, 2, south_west),
  extra_left (0), extra_right (0), rflag (false), button_flag (false),
  enabled(true), centered(false), status (false), inside (false)
{ a[0]= lw; a[1]= rw; }
    
button_widget_rep::button_widget_rep (
  widget lw, widget cw, widget rw, bool e, bool c):
    attribute_widget_rep (cw->dis, 3, south_west),
    extra_left (0), extra_right (0), rflag (false), button_flag (false),
    enabled(e), centered(c), status (false), inside (false)
{ a[0]= lw; a[1]= cw; a[2]= rw; }

button_widget_rep::operator tree () {
  if (N(a)==1) return tree (TUPLE, "button", tree (a[0]));
  else if (N(a)==2) return tree (TUPLE, "button", tree (a[0]), tree (a[1]));
  else return tree (TUPLE, "button", tree (a[0]), tree (a[1]), tree (a[2]));
}

void
button_widget_rep::handle_get_size (get_size_event ev) {
  if (N(a)==1) {
    attribute_widget_rep::handle_get_size (ev);
    if (rflag) ev->w += extra_left + max (extra_right, 16*PIXEL);
    else ev->w += extra_left + extra_right;
  }
  else if (N(a)==2) {
    SI pw1= 0, ph1= 0, pw2= 0, ph2= 0;
    a[0] << get_size (pw1, ph1, -1);
    a[1] << get_size (pw2, ph2, -1);
    ev->w = pw1+ extra_left+ max (extra_right, pw2);
    if (extra_right>0 || pw2>0) ev->w += 8*PIXEL;
    ev->h = max (ph1, ph2);
  }
  else {    
    SI pw1= 0, ph1= 0, pw2= 0, ph2= 0, pw3= 0, ph3= 0;
    a[0] << get_size (pw1, ph1, -1);
    a[1] << get_size (pw2, ph2, -1);
    a[2] << get_size (pw3, ph3, -1);
    ev->w = pw2+ max(extra_left, pw1)+ max (extra_right, pw3);
    if (extra_right>0 || pw3>0) ev->w += 8*PIXEL;
    ev->h = max (ph1, max (ph2, ph3));
  }
  abs_round (ev->w, ev->h);
  ev->w += 2*PIXEL;
  ev->h += 2*PIXEL;
}

void
button_widget_rep::handle_position (position_event ev) {
  (event) ev;
  if (N(a)==1) {
    SI ww= w-2*PIXEL-extra_left;
    SI hh= h-2*PIXEL;
    if (rflag) ww -= 16*PIXEL;
    a[0] << emit_position (extra_left+PIXEL, PIXEL, ww, hh, south_west);
  }
  else if (N(a)==2) {
    SI pw1= 0, ph1= 0, pw2= 0, ph2= 0;
    a[0] << get_size (pw1, ph1, -1);
    a[1] << get_size (pw2, ph2, -1);
    a[0] << emit_position (extra_left+PIXEL, PIXEL, pw1, ph1, south_west);
    a[1] << emit_position (w-PIXEL-extra_right, PIXEL, pw2, ph2, south_west);
  }
  else {
    SI pw1= 0, ph1= 0, pw2= 0, ph2= 0, pw3= 0, ph3= 0;
    a[0] << get_size (pw1, ph1, -1);
    a[1] << get_size (pw2, ph2, -1);
    a[2] << get_size (pw3, ph3, -1);
    SI rounded_x, rounded_y= (h-ph1)/2; abs_round (rounded_y);
    a[0] << emit_position (PIXEL, rounded_y, pw1, ph1, south_west);
    if (centered) rounded_x= (w-pw2)/2;
    else rounded_x= extra_left+PIXEL;
    abs_round (rounded_x);
    a[1] << emit_position (rounded_x, PIXEL, pw2, ph2, south_west); 
    a[2] << emit_position (w-PIXEL-extra_right, PIXEL, pw3, ph3, south_west);
  }
}

void
button_widget_rep::handle_repaint (repaint_event ev) { (void) ev;
  layout_default (win, 0, 0, w, h);
  if (button_flag) layout_higher (win, 0, 0, w, h);
  if (status) {
    layout_dark (win, 0, 0, w, h);
    layout_lower (win, 0, 0, w, h);
  }
  if (rflag) layout_submenu_triangle (win, w-10*PIXEL, h>>1);
}

void
button_widget_rep::handle_get_coord2 (get_coord2_event ev) {
  if (ev->which != "extra width") attribute_widget_rep::handle_get_coord2 (ev);
  else {
    SI dummy;
    ev->c1= 0; ev->c2= 0;
    if (N(a) == 2) a[1] << get_size (ev->c2, dummy, -1);
    if (N(a) == 3) {
      a[0] << get_size (ev->c1, dummy, -1);
      a[2] << get_size (ev->c2, dummy, -1);
    }
  }
}

void
button_widget_rep::handle_set_coord2 (set_coord2_event ev) {
  if (ev->which != "extra width") attribute_widget_rep::handle_set_coord2 (ev);
  else {
    extra_left = ev->c1;
    extra_right= ev->c2;
  }
}

/******************************************************************************
* Command buttons
******************************************************************************/

class command_button_rep: public button_widget_rep {
  command cmd;
public:
  command_button_rep (widget w, command cmd, bool button_flag= false);
  command_button_rep (widget lw, widget rw, command cmd);
  command_button_rep (widget lw, widget cw, widget rw, command cmd,
		      bool e, bool c);
  void handle_mouse (mouse_event ev);
};

command_button_rep::command_button_rep (widget w, command cmd2, bool bf):
  button_widget_rep (w, false, bf), cmd (cmd2) {}

command_button_rep::command_button_rep (widget lw, widget rw, command cmd2):
  button_widget_rep (lw, rw), cmd (cmd2) {}

command_button_rep::command_button_rep (
  widget lw, widget cw, widget rw, command cmd2, bool e, bool c):
    button_widget_rep (lw, cw, rw, e, c), cmd (cmd2) {}

void
command_button_rep::handle_mouse (mouse_event ev) {
  string type= ev->type;
  SI     x= ev->x, y= ev->y;
  // cout << "Command button[" << status << "] "
  //      << s << ": " << ((event) ev) << "\n";

  bool old= status;
  inside= (y>=0) && (y<h) && (x>=0) && (x<w);
  status= inside && enabled && (ev->pressed ("left") || ev->pressed ("right"));

  if (status!=old) {
    this << emit_invalidate_all ();
    if ((type == "release-left") || (type == "release-right"))
      if (!nil (cmd)) cmd ();
  }
}

/******************************************************************************
* Interface
******************************************************************************/

widget
command_button (widget w, command cmd, bool button_flag) {
  return new command_button_rep (w, cmd, button_flag);
}

widget
command_button (widget lw, widget rw, command cmd) {
  return new command_button_rep (lw, rw, cmd);
}

widget
command_button (widget lw, widget cw, widget rw, command cmd, bool e, bool c) {
  return new command_button_rep (lw, cw, rw, cmd, e, c);
}
