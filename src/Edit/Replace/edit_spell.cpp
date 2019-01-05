
/******************************************************************************
* MODULE     : edit_spell.cpp
* DESCRIPTION: spell checker based on ispell
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "analyze.hpp"
#include "Replace/edit_replace.hpp"
#include "Interface/edit_interface.hpp"

#ifdef MACOSX_EXTENSIONS
#include "MacOS/mac_spellservice.h"
#define ispell_start mac_spell_start
#define ispell_check mac_spell_check
#define ispell_accept mac_spell_accept
#define ispell_insert mac_spell_insert
#define ispell_done mac_spell_done
#else
#include "Ispell/ispell.hpp"
#endif


/******************************************************************************
* Start and end spell checking
******************************************************************************/

void
edit_replace_rep::spell_start () {
  /********** get paths ***********/
  search_at   = start (et, rp);
  spell_end_p = end (et, rp);
  path spell_p= copy (tp);
  if (selection_active_normal ()) {
    get_selection (search_at, spell_end_p);
    spell_p= copy (search_at);
  }

  /********** initialize spell checker ***********/
  search_mode = copy (as_string (get_env_value (MODE, spell_p)));
  search_lan  =
    copy (as_string (get_env_value (MODE_LANGUAGE (search_mode), spell_p)));

  string message= ispell_start (search_lan);
  if (starts (message, "Error: ")) {
    spell_end ();
    std_error << message (7, N(message)) << LF;
    set_message (message, "correct text");
    return;
  }

  /********** start spell checking ***********/
  set_input_mode (INPUT_SPELL);
  forward      = true;
  nr_replaced  = 0;
  spell_dicmod = false;

  spell_next ();
  if (search_at == rp)
    set_message ("no spelling errors found in text", "correct text");
}

void
edit_replace_rep::spell_end () {
  if (spell_dicmod) {
    ispell_done (search_lan);
    set_message ("personal dictionary has been modified", "correct text");
  }
  else if (nr_replaced == 1)
    set_message ("one spelling error has been corrected", "correct text");
  else if (nr_replaced > 1)
    set_message (concat (as_string (nr_replaced),
			 " spelling errors have been corrected"),
		 "correct text");
  else set_message ("spell checking complete", "correct text");
  beep ();
  set_input_normal ();
}

/******************************************************************************
* Find next word to be spelled
******************************************************************************/

path
edit_replace_rep::test_spellable (path p) {
  tree st= subtree (et, path_up (p));
  if (is_compound (st)) return p;
  string s= st->label;
  int    b= last_item (p);
  int    e= b;
  if ((e > 0) && ((is_iso_alpha (s[e-1])) || (is_digit (s[e-1])))) return p;
  while ((e < N(s)) && (is_iso_alpha (s[e]))) e++;
  if ((e < N(s)) && (is_digit (s[e]))) return p;
  if (e == b) return p;
  spell_s= s (b, e);
  return path_add (p, e - b);
}

static string
message_ispell (tree t) {
  int i;
  string s= "a: accept, r: replace, i: insert";
  for (i=1; i<N(t); i++) {
    s << ", " << as_string (i) << ": " << t[i]->label;
    if (i==9) return s << ", ...";
  }
  return s;
}

void
edit_replace_rep::spell_next () {
  while (true) {
    if (path_inf (spell_end_p, search_at))
      search_at= rp;
    if (search_at == rp) {
      spell_end ();
      return;
    }
    search_end= test_spellable (search_at);
    if (search_end != search_at) {
      spell_t= ispell_check (search_lan, spell_s);
      if (is_atomic (spell_t) && starts (spell_t->label, "Error: ")) {
	spell_end ();
	set_message (spell_t->label, "spelling text");
	return;
      }
      if (spell_t != "ok") {
	string mode= as_string (get_env_value (MODE, search_at));
	string lan =
	  as_string (get_env_value (MODE_LANGUAGE (mode), search_at));
	if ((search_mode == mode) && (search_lan == lan)) {
	  set_selection (search_at, search_end);
	  notify_change (THE_SELECTION);
	  go_to (copy (search_end));
	  set_message (message_ispell (spell_t), "spelling error");
	  return;
	}
      }
    }
    step_horizontal (forward);
  }
}

/******************************************************************************
* Spell checking commands
******************************************************************************/

void
edit_replace_rep::spell_replace (string by) {
  go_to (copy (search_at));
  cut (search_at, search_end);
  insert_tree (copy (by));
  nr_replaced++;
  spell_next ();
}

bool
edit_replace_rep::spell_keypress (string s) {
  set_message ("", "");
  if (s == "space") s= " ";
  if ((s == "C-c") || (s == "C-g") || (s == "escape"))
    spell_end ();
  else if ((s == "a") || (s == "A")) {
    ispell_accept (search_lan, spell_s);
    step_horizontal (forward);
    spell_next ();
  }
  else if ((s == "r") || (s == "R"))
    (void) eval ("(interactive spell-replace \"Replace by\")");
  else if ((s == "i") || (s == "I")) {
    ispell_insert (search_lan, spell_s);
    spell_dicmod= true;
    step_horizontal (forward);
    spell_next ();
  }
  else if ((N(s)==1) && (is_digit (s[0])) && (s != "0")) {
    int i= as_int (s);
    int r= as_int (spell_t[0]);
    if (i <= r) {
      go_to (copy (search_end));
      cut (search_at, search_end);
      insert_tree (copy (spell_t[i]));
      search_at= copy (tp);
      nr_replaced++;
      spell_next ();
    }
    else if (i < N (spell_t)) {
      ispell_accept (search_lan, spell_s);
      step_horizontal (forward);
      spell_next ();
    }
    else set_message (message_ispell (spell_t), "spelling error");
  }
  else set_message (message_ispell (spell_t), "spelling error");
  return true;
}
