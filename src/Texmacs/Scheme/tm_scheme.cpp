
/******************************************************************************
* MODULE     : tm_scheme.cpp
* DESCRIPTION: The TeXmacs-lisp motor
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tm_scheme.hpp"
#include "convert.hpp"
#include "file.hpp"

/******************************************************************************
* Constructor and destructor
******************************************************************************/

tm_scheme_rep::tm_scheme_rep () { dialogue_win= NULL; }
tm_scheme_rep::~tm_scheme_rep () {}

/******************************************************************************
* Execution of commands
******************************************************************************/

bool
tm_scheme_rep::exec_file (url u) {
  object ret= eval_file (materialize (u));
  return ret != object ("#<unspecified>");
}

string
tm_scheme_rep::preference (string var) {
  return as_string (call ("get-preference", var));
}

/******************************************************************************
* Delayed execution of commands
******************************************************************************/

void
tm_scheme_rep::exec_delayed (string s) {
  exec_delayed (as_command (s));
}

void
tm_scheme_rep::exec_delayed (command cmd) {
  cmds << cmd;
}

void
tm_scheme_rep::exec_pending_commands () {
  array<command> a= cmds;
  cmds= array<command> (0);
  int i, n= N(a);
  for (i=0; i<n; i++)
    a[i]();
  /*
  while (N(cmds)!=0) {
    int i;
    scheme_tree p= cmds[0];
    array<scheme_tree> a (N(cmds)-1);
    for (i=1; i<N(cmds); i++) a[i-1]= cmds[i];
    cmds= a;
    (void) eval (scheme_tree_to_string (p));
  }
  */
}

/******************************************************************************
* Dialogues
******************************************************************************/

class dialogue_command_rep: public command_rep {
  server_rep* sv;
  scheme_tree prg;
public:
  dialogue_command_rep (server_rep* sv2, scheme_tree prg2):
    sv (sv2), prg (prg2) {}
  void apply () {
    scheme_tree arg;
    sv->dialogue_inquire (arg);
    string s= scheme_tree_to_string (tree (TUPLE, prg, arg));
    if (arg != "cancel") sv->exec_delayed (s);
    sv->exec_delayed ("(dialogue-end)"); }
  ostream& print (ostream& out) {
    return out << "Dialogue"; }
};

command
dialogue_command (server_rep* sv, scheme_tree prg) {
  return new dialogue_command_rep (sv, prg);
}

void
tm_scheme_rep::dialogue_start (string name, widget wid, scheme_tree prg) {
  (void) prg;
  if (dialogue_win == NULL) {
    string lan= get_display()->out_lan;
    if (lan == "russian") lan= "english";
    name= get_display()->translate (name, "english", lan);
    char* _name= as_charp (name);
    dialogue_wid= wid;
    dialogue_win= plain_window (dialogue_wid, _name);
    dialogue_win->map ();
    delete[] _name;
  }
}

void
tm_scheme_rep::dialogue_inquire (scheme_tree& arg) {
  string s;
  dialogue_wid << get_string ("input", s);
  arg= string_to_scheme_tree (s);
}

void
tm_scheme_rep::dialogue_end () {
  if (dialogue_win != NULL) {
    dialogue_win->unmap ();
    delete dialogue_win;
    dialogue_win= NULL;
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
tm_scheme_rep::choose_file (string title, string type, scheme_tree prg) {
  string magn;
  if (type == "image") {
    tm_widget meta = get_meta ();
    editor ed      = get_editor ();
    int dpi        = as_int (ed->get_env_string (DPI));
    int sfactor    = meta->get_shrinking_factor ();
    int num        = 75*sfactor;
    int den        = dpi;
    int g          = gcd (num, den);
    num /= g; den /= g;
    if (num != 1) magn << "*" << as_string (num);
    if (den != 1) magn << "/" << as_string (den);
  }

  url     name= get_name_buffer ();
  command cb  = dialogue_command (get_server(), prg);
  widget  wid = file_chooser_widget (cb, type, magn);
  if (!is_without_name (name)) {
    wid << set_string ("directory", as_string (head (name)));
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
      wid << set_string ("file", as_string (u));
    }
  }
  else wid << set_string ("directory", ".");
  dialogue_start (title, wid, prg);
  if (type == "directory")
    dialogue_win->set_keyboard_focus (dialogue_wid[0]["directory"]["input"]);
  else dialogue_win->set_keyboard_focus (dialogue_wid[0]["file"]["input"]);
}
