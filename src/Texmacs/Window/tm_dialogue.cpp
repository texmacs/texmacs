
/******************************************************************************
* MODULE     : tm_dialogue.cpp
* DESCRIPTION: Dialogues
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tm_frame.hpp"
#include "tm_window.hpp"
#include "convert.hpp"
#include "file.hpp"
#include "analyze.hpp"
#include "message.hpp"

/******************************************************************************
* Dialogues
******************************************************************************/

class dialogue_command_rep: public command_rep {
  server_rep* sv;
  object      fun;
  int         nr_args;

public:
  dialogue_command_rep (server_rep* sv2, object fun2, int nr_args2):
    sv (sv2), fun (fun2), nr_args (nr_args2) {}
  void apply ();
  ostream& print (ostream& out) {
    return out << "Dialogue"; }
};

void
dialogue_command_rep::apply () {
  int i;
  object cmd  = null_object ();
  object learn= null_object ();
  for (i=nr_args-1; i>=0; i--) {
    string s_arg;
    sv->dialogue_inquire (i, s_arg);
    if (s_arg == "#f") {
      exec_delayed (scheme_cmd ("(dialogue-end)"));
      return;
    }
    object arg= string_to_object (s_arg);
    learn= cons (cons (object (as_string (i)), arg), learn);
    cmd= cons (arg, cmd);
    //call ("learn-interactive-arg", fun, object (i), arg);
  }
  call ("learn-interactive", fun, learn);
  cmd= cons (fun, cmd);
  exec_delayed (scheme_cmd ("(dialogue-end)"));
  exec_delayed (scheme_cmd (cmd));
}

command
dialogue_command (server_rep* sv, object fun, int nr_args) {
  return new dialogue_command_rep (sv, fun, nr_args);
}

void
tm_frame_rep::dialogue_start (string name, widget wid) {
  if (nil (dialogue_win)) {
    string lan= the_display->out_lan;
    if (lan == "russian") lan= "english";
    name= the_display->translate (name, "english", lan);
    char* _name= as_charp (name);
    widget win= get_window () -> wid;
    SI ox, oy, dx, dy, ex= 0, ey= 0;
    concrete(win)->win->get_position (ox, oy);
    concrete(win)->win->get_size (dx, dy);
    concrete (wid) << get_size (ex, ey, -1);
    ox += (dx - ex) >> 1;
    oy -= (dy - ey) >> 1;
    dialogue_wid= wid;
    dialogue_win= plain_window_widget (dialogue_wid, _name, 0, 0, ox, oy);
    concrete(dialogue_win)->win->map ();
    delete[] _name;
  }
}

void
tm_frame_rep::dialogue_inquire (int i, string& arg) {
  string s= "input";
  if (i>0) s= "input-" * as_string (i);
  concrete (dialogue_wid) << get_string (s, arg);
}

void
tm_frame_rep::dialogue_end () {
  if (!nil (dialogue_win)) {
    concrete(dialogue_win)->win->unmap ();
    destroy_window_widget (dialogue_win);
    dialogue_win= widget ();
    dialogue_wid= widget ();
  }
}

static int
gcd (int i, int j) {
  if (i<j)  return gcd (j, i);
  if (j==0) return i;
  return gcd (j, i%j);
}

void
tm_frame_rep::choose_file (object fun, string title, string type) {
  string magn;
  if (type == "image") {
    editor ed   = get_editor ();
    int dpi     = as_int (ed->get_env_string (DPI));
    int sfactor = get_window () -> get_shrinking_factor ();
    int num     = 75*sfactor;
    int den     = dpi;
    int g       = gcd (num, den);
    num /= g; den /= g;
    if (num != 1) magn << "*" << as_string (num);
    if (den != 1) magn << "/" << as_string (den);
  }

  url      name= get_name_buffer ();
  command  cb  = dialogue_command (get_server(), fun, 1);
  widget   wid = file_chooser_widget (cb, type, magn);
  if (!is_scratch (name)) {
    set_directory (wid, as_string (head (name)));
    if ((type != "image") && (type != "")) {
      url u= tail (name);
      string old_suf= suffix (u);
      string new_suf= format_to_suffix (type);
      if ((suffix_to_format (suffix (u)) != type) &&
	  (old_suf != "") && (new_suf != ""))
	{
	  u= unglue (u, N(old_suf) + 1);
	  u= glue (u, "." * new_suf);
	}
      set_file (wid, as_string (u));
    }
  }
  else set_directory (wid, ".");
  dialogue_start (title, wid);
  if (type == "directory")
    concrete(dialogue_win)->win->
      set_keyboard_focus (get_directory (dialogue_wid));
  else concrete(dialogue_win)->win->
	 set_keyboard_focus (get_file (dialogue_wid));
}

/******************************************************************************
* Interactive commands
******************************************************************************/

static string
get_prompt (scheme_tree p, int i) {
  if (is_atomic (p[i]) && is_quoted (p[i]->label))
    return scm_unquote (p[i]->label);
  else if (is_tuple (p[i]) && N(p[i])>0 &&
	   is_atomic (p[i][0]) && is_quoted (p[i][0]->label))
    return scm_unquote (p[i][0]->label);
  return "Input:";
}

static string
get_type (scheme_tree p, int i) {
  if (is_tuple (p[i]) && N(p[i])>1 &&
      is_atomic (p[i][1]) && is_quoted (p[i][1]->label))
    return scm_unquote (p[i][1]->label);
  return "string";
}

static array<string>
get_proposals (scheme_tree p, int i) {
  array<string> a;
  if (is_tuple (p[i]) && N(p[i]) >= 2) {
    int j, n= N(p[i]);
    for (j=2; j<n; j++)
      if (is_atomic (p[i][j]) && is_quoted (p[i][j]->label))
	a << scm_unquote (p[i][j]->label);
  }
  return a;
}

class interactive_command_rep: public command_rep {
  server_rep*   sv;   // the underlying server
  tm_window     win;  // the underlying TeXmacs window
  object        fun;  // the function which is applied to the arguments
  scheme_tree   p;    // the interactive arguments
  int           i;    // counter where we are
  array<string> s;    // feedback from interaction with user

public:
  interactive_command_rep (
    server_rep* sv2, tm_window win2, object fun2, scheme_tree p2):
      sv (sv2), win (win2), fun (fun2), p (p2), i (0), s (N(p)) {}
  void apply ();
  ostream& print (ostream& out) {
    return out << "interactive command " << p; }
};

void
interactive_command_rep::apply () {
  if ((i>0) && (s[i-1] == "#f")) return;
  if (i == N(p)) {
    object learn= null_object ();
    array<object> params (N(p));
    for (i=N(p)-1; i>=0; i--) {
      params[i]= string_to_object (s[i]);
      learn= cons (cons (object (as_string (i)), params[i]), learn);
    }
    call ("learn-interactive", fun, learn);
    string ret= object_to_string (call (fun, params));
    if (ret != "" && ret != "<unspecified>" && ret != "#<unspecified>")
      sv->set_message (ret, "interactive command");
  }
  else {
    s[i]= string ("");
    string prompt= get_prompt (p, i);
    string type  = get_type (p, i);
    array<string> proposals= get_proposals (p, i);
    win->interactive (prompt, type, proposals, s[i], this);
    i++;
  }
}

void
tm_frame_rep::interactive (object fun, scheme_tree p) {
  if (!is_tuple (p))
    fatal_error ("tuple expected", "edit_interface_rep::interactive");
  if (get_preference ("interactive questions") == "popup") {
    int i, n= N(p);
    array<string> prompts (n);
    for (i=0; i<n; i++)
      prompts[i]= get_prompt (p, i);
    command cb= dialogue_command (get_server(), fun, n);
    widget wid= inputs_list_widget (cb, prompts);
    for (i=0; i<n; i++) {
      widget input_wid= get_form_field (wid, i);
      set_input_type (input_wid, get_type (p, i));
      array<string> proposals= get_proposals (p, i);
      int j, k= N(proposals);
      if (k > 0) set_string_input (input_wid, proposals[0]);
      for (j=0; j<k; j++) add_input_proposal (input_wid, proposals[j]);
    }
    string title= "Enter data";
    if (ends (prompts[0], "?")) title= "Question";
    dialogue_start (title, wid);
    concrete(dialogue_win)->win->
      set_keyboard_focus (get_form_field (dialogue_wid, 0));
  }
  else {
    if (get_window () -> get_interactive_mode ()) beep ();
    else {
      command interactive_cmd=
	new interactive_command_rep (this, get_window (), fun, p);
      interactive_cmd ();
    }
  }
}
