
/******************************************************************************
* MODULE     : inputs_list_widget.cpp
* DESCRIPTION: A list of textual inputs with ok and cancel buttons
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "analyze.hpp"
#include "Widkit/basic_widget.hpp"
#include "Widkit/attribute_widget.hpp"
#include "Widkit/layout.hpp"

/******************************************************************************
* Inputs list widgets
******************************************************************************/

class inputs_list_widget_rep: public attribute_widget_rep {
  command       cmd;
  array<string> prompts;
  bool          ok;

public:
  inputs_list_widget_rep (command cmd, array<string> prompts);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_set_string (set_string_event ev);
  void handle_get_string (get_string_event ev);
  void handle_destroy (destroy_event ev);
};

/******************************************************************************
* Inputs list commands
******************************************************************************/

class inputs_list_command_rep: public command_rep {
  widkit_widget_rep* ilw;
  int         i;  // -1 corresponds to cancel
  int         n;
public:
  inputs_list_command_rep (widget w, int i2, int n2):
    ilw (w.rep), i (i2), n (n2) {}
  void apply ();
  ostream& print (ostream& out) {
    return out << "Input list command (" << i << ")"; }
};

void
inputs_list_command_rep::apply () {
  widget il_wid (ilw);
  if (i < 0)
    il_wid << set_string ("return", "#f");
  else if (i == n-1)
    il_wid << set_string ("return", "#t");
  else {
    string answer;
    il_wid[0]["inputs"][i]["input"] << get_string ("input", answer);
    if (answer == "#f") il_wid << set_string ("return", "#f");
    else il_wid->win->set_keyboard_focus (il_wid[0]["inputs"][i+1]["input"]);
  }
}

command
inputs_list_command (widget ilw, int i, int n) {
  return new inputs_list_command_rep (ilw, i, n);
}

/******************************************************************************
* Implementation of inputs list widgets
******************************************************************************/

inputs_list_widget_rep::inputs_list_widget_rep (
  command cmd2, array<string> prompts2):
    attribute_widget_rep (1), cmd (cmd2), prompts (prompts2), ok (true)
{
  ref_count++;

  int i, n= N (prompts);
  array<widget> fields_w (n);
  for (i=0; i<n; i++) {
    array<widget> line_w (5);
    array<string> line_n (5);
    line_w[0]= glue_widget (false, false, 2*PIXEL);
    line_w[1]= text_widget (prompts[i]);
    line_n[1]= "prompt";
    line_w[2]= glue_widget (false, false, 5*PIXEL);
    line_w[3]= input_text_widget (inputs_list_command (this, i, n));
    line_n[3]= "input";
    line_w[4]= glue_widget (false, false, 2*PIXEL);
    fields_w[i]= horizontal_list (line_w, line_n);
  }

  array<widget> buttons_w (5);
  buttons_w[0]= glue_widget (true, false);
  buttons_w[1]= command_button (text_widget ("Cancel", false, "english"),
				inputs_list_command (this, -1, n), true);
  buttons_w[2]= glue_widget (false, false, 5*PIXEL);
  buttons_w[3]= command_button (text_widget ("Ok", false, "english"),
				inputs_list_command (this, n-1, n), true);
#ifdef OS_DARWIN
  buttons_w[4]= glue_widget (false, false, 19*PIXEL);
#else
  buttons_w[4]= glue_widget (false, false, 5*PIXEL);
#endif

  array<widget> main_w (5);
  array<string> main_n (5);
  main_w[0]= glue_widget (false, false, 0, 5*PIXEL);
  main_w[1]= vertical_list (fields_w);
  main_n[1]= "inputs";
  main_w[2]= glue_widget (false, false, 0, 5*PIXEL);
  main_w[3]= horizontal_list (buttons_w);
  main_n[3]= "buttons";
  main_w[4]= glue_widget (false, false, 0, 5*PIXEL);

  a[0]= vertical_list (main_w, main_n);

  ref_count--;
}

inputs_list_widget_rep::operator tree () {
  return tree (TUPLE, "inputs_list_widget", (tree) a[0]);
}

void
inputs_list_widget_rep::handle_get_size (get_size_event ev) {
  SI dummy;
  a[0] << get_size (ev->w, ev->h, ev->mode);
  if (ev->mode < 1) ev->w= max (ev->w, 300*PIXEL);
  else the_display->get_max_size (ev->w, dummy);
}

void
inputs_list_widget_rep::handle_set_string (set_string_event ev) {
  if (ev->which == "return") {
    ok= (ev->s == "#t");
    cmd ();
  }
  else attribute_widget_rep::handle_set_string (ev);
}

void
inputs_list_widget_rep::handle_get_string (get_string_event ev) {
  string s= ev->which;
  if (s == "input") s= "input-0";
  if (starts (s, "input-")) {
    widget ch= widget(this)[0]["inputs"][as_int (s (6, N(s)))]["input"];
    ch << get_string ("input", ev->s);
    if (!ok) ev->s= "#f";
  }
  else attribute_widget_rep::handle_get_string (ev);
}

void
inputs_list_widget_rep::handle_destroy (destroy_event ev) {
  (void) ev;
  this << set_string ("return", "#f");
}

/******************************************************************************
* exported routines
******************************************************************************/

widget
inputs_list_widget (command cmd, array<string> fields) {
  return new inputs_list_widget_rep (cmd, fields);
}
