
/******************************************************************************
* MODULE     : edit_keyboard.cpp
* DESCRIPTION: Keyboard handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "edit_interface.hpp"
#include "analyze.hpp"
#include "tm_buffer.hpp"
#include "archiver.hpp"

/******************************************************************************
* Basic subroutines for keyboard handling
******************************************************************************/

int
edit_interface_rep::get_input_mode () {
  return input_mode;
}

void
edit_interface_rep::set_input_mode (int mode) {
  interrupt_shortcut ();
  // avoids keyboard shortcuts when using the menu between two keystrokes

  if ((mode == INPUT_NORMAL) && (input_mode != INPUT_NORMAL)) {
    selection_cancel ();
    completions= array<string> ();
  }
  input_mode= mode;
}

void
edit_interface_rep::set_input_normal () {
  set_input_mode (INPUT_NORMAL);
}

bool
edit_interface_rep::in_normal_mode () {
  return input_mode == INPUT_NORMAL;
}

bool
edit_interface_rep::in_search_mode () {
  return input_mode == INPUT_SEARCH;
}

bool
edit_interface_rep::in_replace_mode () {
  return input_mode == INPUT_REPLACE;
}

bool
edit_interface_rep::in_spell_mode () {
  return input_mode == INPUT_SPELL;
}

bool
edit_interface_rep::kbd_get_command (string which, string& help, command& c) {
  return sv->kbd_get_command (which, help, c);
}

/******************************************************************************
* Main keyboard routines
******************************************************************************/

void
edit_interface_rep::interrupt_shortcut () {
  if (sh_mark != 0) mark_end (sh_mark);
  sh_s= "";
  sh_mark= 0;
}

bool
edit_interface_rep::try_shortcut (string comb) {
  int     status;
  command cmd;
  string  shorth;
  string  help;

  sv->get_keycomb (comb, status, cmd, shorth, help);
  //cout << "Try " << comb << " -> " << shorth << ", " << help
  //<< "; " << sh_mark << ", " << status << "\n";
  if (status != 0) {
    if (status >= 3) {
      interrupt_shortcut ();
      status -= 3;
      if (status == 0) return false;
    }
    else {
      if (sh_mark != 0 && !mark_cancel (sh_mark)) {
	sh_mark= 0;
	return false;
      }
    }
    sh_s= comb;
    sh_mark= new_marker ();
    mark_start (sh_mark);
    string rew_s= sv->kbd_post_rewrite (sh_s);
    tree rew= sv->kbd_system_rewrite (rew_s);
    if (N(help)>0) set_message (help, rew);
    tree rhs= (shorth == rew_s? tree (""): sv->kbd_system_rewrite (shorth));
    if ((search_forwards (" ", comb) >= 0 && comb != " ") ||
	(search_forwards ("-", comb) >= 0 && comb != "-"))
      call ("set-temporary-message",
	    tree (CONCAT, "keyboard shortcut: ", rew), rhs,
	    shorth == ""? 1: 3000);
    if ((status & 1) == 1) cmd ();
    else if (N(shorth) > 0) insert_tree (shorth);
    //cout << "Mark= " << sh_mark << "\n";
    return true;    
  }

  return false;
}

void
edit_interface_rep::key_press (string key) {
  if (pre_edit_mark != 0) {
    ASSERT (sh_mark == 0, "invalid shortcut during pre-edit");
    mark_end (pre_edit_mark);
    pre_edit_s= "";
    pre_edit_mark= 0;
  }
  if (starts (key, "pre-edit:") ) {
    interrupt_shortcut ();
    string s= key (9, N(key));
    if (s == "") return;
    pre_edit_s= s;
    pre_edit_mark= new_marker ();
    mark_start (pre_edit_mark);
    insert_tree (compound ("pre-edit", s));
    return;
  }

  string new_sh= N(sh_s)==0? key: sh_s * " " * key;
  if (try_shortcut (new_sh)) return;
  if (new_sh != key) {
    interrupt_shortcut ();
    if (try_shortcut (key)) return;
  }

  string rew= sv->kbd_post_rewrite (key);
  if (N(rew) == 1) {
    int i ((unsigned char) rew[0]);
    if ((i >= 32 && i <= 127) || (i >= 128 && i <= 255))
      if (!inside_active_graphics ())
	insert_tree (rew);
    interrupt_shortcut ();
  }
  else if (contains_unicode_char (rew)) {
    insert_tree (key);
    interrupt_shortcut ();    
  }
}

void
edit_interface_rep::emulate_keyboard (string keys, string action) {
  string s= keys;
  while (s != "") {
    int i;
    for (i=1; i<N(s); i++)
      if (s[i]==' ') break;
    call ("keyboard-press", object (s (0, i)), object ((double) 0));
    if (i<N(s)) i++;
    s= s (i, N(s));
  }
  if (N (action) != 0)
    set_message (concat ("You can also obtain ", action, " by typing ", keys),
		 action);
}

/******************************************************************************
* Retrieving keyboard shortcuts
******************************************************************************/

tree
edit_interface_rep::kbd (string s) {
  return sv->kbd_system_rewrite (s);
}

tree
edit_interface_rep::kbd_shortcut (string cmd) {
  string s= as_string (eval ("(kbd-find-inv-binding '" * cmd * ")"));
  return kbd (s);
}

/******************************************************************************
* Event handlers
******************************************************************************/

void
edit_interface_rep::handle_keypress (string key, time_t t) {
  if (DEBUG_KEYBOARD)
    cout << "Keyboard] Pressed " << key << " at " << t << "\n";
  //time_t t1= texmacs_time ();
  if (is_nil (eb)) apply_changes ();
  start_editing ();
  call ("keyboard-press", object (key), object ((double) t));
  notify_change (THE_DECORATIONS);
  end_editing ();
  //time_t t2= texmacs_time ();
  //if (t2 - t1 >= 10) cout << "handle_keypress took " << t2-t1 << "ms\n";
}

void
edit_interface_rep::handle_keyboard_focus (bool has_focus, time_t t) {
  if (DEBUG_KEYBOARD) {
    if (has_focus) cout << "Keyboard] Got focus at " << t << "\n";
    else cout << "Keyboard] Lost focus at " << t << "\n";
  }
  got_focus= has_focus; (void) t;
  notify_change (THE_FOCUS);
  if (got_focus) {
    focus_on_this_editor ();
    notify_change (THE_DECORATIONS);
  }
  call ("keyboard-focus", object (has_focus), object ((double) t));
}
