
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
* Showing the keystrokes while typing
******************************************************************************/

bool          kbd_show_keys= false;
array<string> kbd_shown_keys;
array<string> kbd_last_keys;
array<time_t> kbd_last_times;
int           kbd_erase_delay= 1500;
int           kbd_hide_delay = 3000;

bool get_show_kbd () { return kbd_show_keys; }
void set_show_kbd (bool flag) { kbd_show_keys= flag; }

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
    archive_state ();
    string rew_s= sv->kbd_post_rewrite (sh_s);
    tree rew= sv->kbd_system_rewrite (rew_s);
    if (N(help)>0) set_message (help, rew);
    tree rhs= (shorth == rew_s? tree (""): sv->kbd_system_rewrite (shorth));
    //cout << "Shortcut: " << sh_s << " -> " << rew << "\n";
    if ((search_forwards (" ", comb) >= 0 && comb != " ") ||
	(search_forwards ("-", comb) >= 0 && comb != "-")) {
      tree t= rhs;
      if (is_compound (t, "render-key", 1)) t= t[0];
      if (is_func (t, WITH)) t= t[N(t)-1];
      string r= as_string (t);
      if (starts (r, "<") && !starts (r, "<#"))
        if (cork_to_utf8 (r) != r)
          rhs= tree (CONCAT, rhs, " (" * r(1, N(r)-1) * ")");
      call ("set-temporary-message",
	    tree (CONCAT, "keyboard shortcut: ", rew), rhs,
	    shorth == ""? 1: 3000);
    }
    if ((status & 1) == 1) cmd ();
    else if (N(shorth) > 0) call ("kbd-insert", shorth);
    //cout << "Mark= " << sh_mark << "\n";
    return true;    
  }

  return false;
}

void
edit_interface_rep::key_press (string gkey) {
  string zero= "a"; zero[0]= '\0';
  string key= replace (gkey, "<#0>", zero);
  if (pre_edit_mark != 0) {
    ASSERT (sh_mark == 0, "invalid shortcut during pre-edit");
    mark_cancel (pre_edit_mark);
    pre_edit_s= "";
    pre_edit_mark= 0;
  }
  if (starts (key, "pre-edit:")) {
    string s= key (9, N(key));
    int i, n= N(s), pos= N(s);
    for (i=0; i<n; i++)
      if (s[i] == ':' && is_int (s (0, i))) {
        int k= as_int (s (0, i));
        s= s (i+1, n);
        pos= 0;
        for (int j=0; j<k && pos<N(s); j++)
          tm_char_forwards (s, pos);
        break;
      }
    if (as_bool (call ("disable-pre-edit?", cork_to_utf8 (s)))) {
      pre_edit_skip= false;
      if (s == "") return;
      pre_edit_skip= true;
      key= s;
      if (key == zero) key= "`";
    }
    else if (pre_edit_skip) {
      if (s == "") pre_edit_skip= false;
      return;
    }
    else {
      if (s == "") return;
      interrupt_shortcut ();
      pre_edit_s= s;
      pre_edit_mark= new_marker ();
      mark_start (pre_edit_mark);
      archive_state ();
      insert_tree (compound ("pre-edit", s), path (0, pos));
      return;
    }
  }
  else if (pre_edit_skip) {
    string u= cork_to_utf8 (key);
    string r= as_string (call ("downgrade-pre-edit", u));
    if (r == "") return;
    else key= r;
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
    if ((i >= 32 && i <= 127) || (i >= 128 && i <= 255) || (i == 25))
      if (!inside_active_graphics ()) {
        archive_state ();
        call ("kbd-insert", rew);
      }
    interrupt_shortcut ();
  }
  else if (contains_unicode_char (rew)) {
    archive_state ();
    call ("kbd-insert", key);
    interrupt_shortcut ();    
  }
  else if (DEBUG_KEYBOARD)
    debug_keyboard
      << "unrecognized key " << key << ". "
      << "Undefined shortcut or key missing in the encoding files.\n";
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
  bool started= false;
#ifdef USE_EXCEPTIONS
  try {
#endif
    if (kbd_show_keys) {
      if (N(kbd_last_times) > 0 &&
          kbd_last_times[N(kbd_last_times)-1] + kbd_erase_delay < t) {
        kbd_last_keys = array<string> ();
        kbd_last_times= array<time_t> ();
      }
      if (!starts (key, "pre-edit:")) {
        kbd_last_keys  << key;
        kbd_last_times << t;
      }
    }
    if (DEBUG_KEYBOARD) {
      //for (int i=0; i<N(key); i++)
      //  cout << ((int) (unsigned char) key[i]) << " ";
      //cout << "\n";
      debug_keyboard << "Pressed " << key << " at " << t << "\n";
      debug_keyboard << "  Codes";
      for (int i=0; i<N(key); i++)
	debug_keyboard << " " << (unsigned int) (unsigned char) key[i];
      debug_keyboard << "\n";      
    }
    //time_t t1= texmacs_time ();
    if (is_nil (eb)) apply_changes ();
    start_editing ();
    started= true;
    string zero= "a"; zero[0]= '\0';
    string gkey= replace (key, zero, "<#0>");
    if (gkey == "<#3000>") gkey= "space";
    call ("keyboard-press", object (gkey), object ((double) t));
    update_focus_loci ();
    if (!is_nil (focus_ids))
      call ("link-follow-ids", object (focus_ids), object ("focus"));
    notify_change (THE_DECORATIONS);
    end_editing ();
    //time_t t2= texmacs_time ();
    //if (t2 - t1 >= 10) cout << "handle_keypress took " << t2-t1 << "ms\n";
#ifdef USE_EXCEPTIONS
  }
  catch (string msg) {
    if (started) {
      cancel_editing ();
      interrupt_shortcut ();
    }
  }
  handle_exceptions ();
#endif
}

void drag_left_reset ();
void drag_right_reset ();

void
edit_interface_rep::handle_keyboard_focus (bool has_focus, time_t t) {
  if (DEBUG_KEYBOARD) {
    if (has_focus) debug_keyboard << "Got focus at " << t << "\n";
    else debug_keyboard << "Lost focus at " << t << "\n";
  }
  if (got_focus != has_focus) {
    drag_left_reset ();
    drag_right_reset ();
  }
  got_focus= has_focus; (void) t;
  notify_change (THE_FOCUS);
  if (got_focus) {
    focus_on_this_editor ();
    notify_change (THE_DECORATIONS);
  }
  call ("keyboard-focus", object (has_focus), object ((double) t));
}
