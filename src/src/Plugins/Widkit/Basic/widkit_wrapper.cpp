
/******************************************************************************
* MODULE     : widkit_wrapper.hpp
* DESCRIPTION: Conversions between widget and wk_widget
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "promise.hpp"
#include "url.hpp"
#include "Widkit/wk_widget.hpp"

/******************************************************************************
* Type conversions
******************************************************************************/

array<widget>
abstract (array<wk_widget> a) {
  int i, n= N(a);
  array<widget> b (n);
  for (i=0; i<n; i++) b[i]= abstract (a[i]);
  return b;
}

array<wk_widget>
concrete (array<widget> a) {
  int i, n= N(a);
  array<wk_widget> b (n);
  for (i=0; i<n; i++) b[i]= concrete (a[i]);
  return b;
}

class abstract_promise_rep: public promise_rep<widget> {
  promise<wk_widget> p;
public:
  abstract_promise_rep (promise<wk_widget> p2): p (p2) {}
  ostream& print (ostream& out) { return out << p; }
  widget eval () { return abstract (p ()); }
};

promise<widget>
abstract (promise<wk_widget> pw) {
  return new abstract_promise_rep (pw);
}

class concrete_promise_rep: public promise_rep<wk_widget> {
  promise<widget> p;
public:
  concrete_promise_rep (promise<widget> p2): p (p2) {}
  ostream& print (ostream& out) { return out << p; }
  wk_widget eval () { return concrete (p ()); }
};

promise<wk_widget>
concrete (promise<widget> pw) {
  return new concrete_promise_rep (pw);
}

/******************************************************************************
* Exported special widgets
******************************************************************************/

widget
horizontal_list (array<widget> a) {
  return abstract (horizontal_list (concrete (a)));
}

widget
horizontal_list (array<widget> a, array<string> name) {
  return abstract (horizontal_list (concrete (a), name));
}

widget
vertical_list (array<widget> a) {
  return abstract (vertical_list (concrete (a)));
}

widget
vertical_list (array<widget> a, array<string> name) {
  return abstract (vertical_list (concrete (a), name));
}

widget
vertical_menu (array<widget> a) {
  return abstract (vertical_menu (concrete (a)));
}

widget
tile (array<widget> a, int cols) {
  return abstract (tile (concrete (a), cols));
}

widget
tile (array<widget> a, int cols, array<string> name) {
  return abstract (tile (concrete (a), cols, name));
}

widget
horizontal_array (array<widget> a, int stretch_me) {
  return abstract (horizontal_array (concrete (a), stretch_me));
}

widget
horizontal_array (array<widget> a, array<string> s, int stretch_me) {
  return abstract (horizontal_array (concrete (a), s, stretch_me));
}

widget
switch_widget (array<widget> a, array<string> name, int init) {
  return abstract (switch_widget (concrete (a), name, init));
}

widget
optional_widget (widget w, bool on) {
  return abstract (optional_widget (concrete (w), on));
}

widget
glue_widget (bool hx, bool vx, SI w, SI h) {
  return abstract (glue_wk_widget (hx, vx, w, h));
}

widget
separator_widget (SI pre, SI post, bool vert) {
  return abstract (separator_wk_widget (pre, post, vert));
}

widget
text_widget (string s, bool tsp, string lan) {
  return abstract (text_wk_widget (s, tsp, lan));
}

widget
menu_text_widget (string s, color col, string lan, bool tt) {
  return abstract (menu_text_wk_widget (s, col, lan, tt));
}

widget
xpm_widget (url file_name, bool transp) {
  return abstract (xpm_wk_widget (file_name, transp));
}

widget
command_button (widget w, command cmd, bool button_flag) {
  return abstract (command_button (concrete (w), cmd, button_flag));
}

widget
command_button (widget lw, widget rw, command cmd) {
  return abstract (command_button (concrete (lw), concrete (rw), cmd));
}

widget
command_button (widget lw, widget cw, widget rw, command cmd, bool e, bool c) {
  return abstract (command_button (concrete (lw), concrete (cw),
				   concrete (rw), cmd, e, c));
}

widget
pulldown_button (widget w, widget m, bool button_flag) {
  return abstract (pulldown_button (concrete (w), concrete (m), button_flag));
}

widget
pullright_button (widget w, widget m, bool button_flag) {
  return abstract (pullright_button (concrete (w), concrete (m), button_flag));
}

widget
pulldown_button (widget w, promise<widget> pw) {
  return abstract (pulldown_button (concrete (w), concrete (pw)));
}

widget
pullright_button (widget w, promise<widget> pw) {
  return abstract (pullright_button (concrete (w), concrete (pw)));
}

widget
popup_widget (widget w, gravity quit) {
  return abstract (popup_widget (concrete (w), quit));
}

widget
canvas_widget (widget w, gravity grav) {
  return abstract (canvas_widget (concrete (w), grav));
}

widget
input_text_widget (command call_back) {
  return abstract (input_text_wk_widget (call_back));
}

widget
input_text_widget (command call_back, string type, array<string> def) {
  return abstract (input_text_wk_widget (call_back, type, def));
}

widget
inputs_list_widget (command call_back, array<string> prompts) {
  return abstract (inputs_list_wk_widget (call_back, prompts));
}

widget
file_chooser_widget (command cmd, string type, string mgn) {
  return abstract (file_chooser_wk_widget (cmd, type, mgn));
}

widget
balloon_widget (widget w, widget help) {
  return abstract (balloon_widget (concrete (w), concrete (help)));
}

widget
wait_widget (SI w, SI h, string message) {
  return abstract (wait_wk_widget (w, h, message));
}
