
/******************************************************************************
* MODULE     : enum_widget.cpp
* DESCRIPTION: Select a possibility among a given list of possibilities
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "window.hpp"
#include "Widkit/wk_widget.hpp"
#include "Scheme/object.hpp"

/******************************************************************************
* Enumeration commands
******************************************************************************/

class enum_command_rep: public command_rep {
  wk_widget_rep* in;
  string val;
  command cb;
public:
  enum_command_rep (wk_widget w, string val2, command cb2):
    in (w.rep), val (val2), cb (cb2) {}
  void apply ();
  tm_ostream& print (tm_ostream& out) {
    return out << "Enum widget command"; }
};

void
enum_command_rep::apply () {
  in << set_string ("input", val);
  cb (list_object (object (val)));
}

command
enum_command (wk_widget in, string val, command cb) {
  return tm_new<enum_command_rep> (in, val, cb);
}

/******************************************************************************
* Enumeration widgets
******************************************************************************/

wk_widget
enum_wk_widget (command cb, array<string> vals, string val,
                int style, string w) {
  int i, n= N(vals);
  array<string> def (1);
  def[0]= val;
  wk_widget in= input_text_wk_widget (cb, "string", def, style, w, true);
  if (n == 0) return in;
  bool editable= (val == "" || vals[n-1] == "");
  if (vals[n-1] == "") { n= n-1; vals= range (vals, 0, n); }
  array<wk_widget> entries (n);
  for (i=0; i<n; i++) {
    wk_widget txt= text_wk_widget (vals[i], style);
    command cmd= enum_command (in, vals[i], cb);
    entries[i]= command_button (txt, cmd, style);
  }
  wk_widget menu= vertical_menu (entries);
  if (!editable)
    return pulldown_button (in, menu, style);
  wk_widget v= text_wk_widget ("v", style);
  wk_widget v_but= command_button (v, command (), style|WIDGET_STYLE_BUTTON);
  wk_widget v_pop= pulldown_button (v_but, menu, style);
  array<wk_widget> ret (2);
  ret[0]= in;
  ret[1]= v_pop;
  return horizontal_list (ret);
}
