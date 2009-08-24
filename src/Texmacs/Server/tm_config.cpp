
/******************************************************************************
* MODULE     : tm_config.cpp
* DESCRIPTION: Configuration routines for TeXmacs server
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_config.hpp"
#include "analyze.hpp"

/******************************************************************************
* Constructor and destructor
******************************************************************************/

tm_config_rep::tm_config_rep ():
  var_suffix (" tab"), unvar_suffix (" S-tab") {}

tm_config_rep::~tm_config_rep () {}

/******************************************************************************
* User preferences
******************************************************************************/

string
tm_config_rep::get_preference (string var) {
  return as_string (call ("get-preference", var));
}

/******************************************************************************
* Setup TeXmacs fonts
******************************************************************************/

void
tm_config_rep::set_font_rules (scheme_tree rules) {
  int i, n= arity (rules);
  for (i=0; i<n; i++)
    if (arity (rules [i]) == 2) {
      tree l= (tree) rules[i][0];
      tree r= (tree) rules[i][1];
      font_rule (l, r);
    }  
}

/******************************************************************************
* Latex and user commands
******************************************************************************/

bool
tm_config_rep::kbd_get_command (string which, string& help, command& cmd) {
  object im= call ("kbd-get-command", which);
  if (im == object (false)) return false;
  help= as_string (car (im));
  cmd = as_command (cdr (im));
  return true;
}

/******************************************************************************
* Wildcards for keyboard bindings
******************************************************************************/

static string
apply_wildcards (string s, hashmap<string,tree> w) {
  int len, start, end, n= N(s);
  for (len=n; len>0; len--) {
    for (start=0; start <= (n-len); start++) {
      end= start+ len;
      if ((start>0) && (s[start-1] != ' ') && (s[start-1] != '-')) continue;
      if ((end<n) && (s[end-1] != ' ') && (s[end-1] != '-')) continue;
      string ss= s (start, end);
      if (s[end-1] == ' ') ss= s (start, end-1);

      // cout << "  " << ss << " => " << w[ss] << LF;
      if (w->contains (ss)) {
	tree t= w[ss];
	string rr= t[0]->label;
	bool lflag= (t[1]->label != "") || (start == 0);
	bool rflag= (t[2]->label != "") || (end == n);
	if (lflag && rflag) {
	  if ((end<n) && (rr != "") && (rr[N(rr)-1] != '-')) rr= rr * " ";
	  string r= s (0, start) * rr * s (end, n);
	  return apply_wildcards (r, w);
	}
      }      
    }
  }
  return s;
}

void
tm_config_rep::insert_kbd_wildcard (
  string key, string im, bool post, bool l, bool r)
{
  tree t= tuple (im,
		 l? string ("*"): string (""),
		 r? string ("*"): string (""));
  if (post) post_kbd_wildcards (key)= t;
  else pre_kbd_wildcards (key)= t;
}

/******************************************************************************
* Variants
******************************************************************************/

void
tm_config_rep::set_variant_keys (string var, string unvar) {
  var_suffix= " " * var;
  unvar_suffix= " " * unvar;
}

#define rewrite_find_key_binding(s) \
  find_key_binding (apply_wildcards (s, post_kbd_wildcards))

void
tm_config_rep::variant_simplification (string& which) {
  if (ends (which, var_suffix)) {
    object obj= rewrite_find_key_binding (which);
    // cout << which << " => " << obj << LF;
    if (obj == object (false))
      while (ends (which, var_suffix))
	which= which (0, N(which) - N(var_suffix));
  }
  if (ends (which, unvar_suffix)) {
    if (ends (which, var_suffix * unvar_suffix))
      which= which (0, N(which) - N(var_suffix) - N(unvar_suffix));
    else {
      which= which (0, N(which) - N(unvar_suffix));
      while (true) {
	if (rewrite_find_key_binding (which * var_suffix) == object (false))
	  break;
	which= which * var_suffix;
      }
    }
  }
}

/******************************************************************************
* Server keyboard mappings and shorthands
******************************************************************************/

object
tm_config_rep::find_key_binding (string key) {
  return call ("kbd-find-key-binding", key);
}

string
tm_config_rep::kbd_pre_rewrite (string s) {
  return apply_wildcards (s, pre_kbd_wildcards);
}

string
tm_config_rep::kbd_post_rewrite (string s) {
  variant_simplification (s);
  return apply_wildcards (s, post_kbd_wildcards);
}

void
tm_config_rep::get_keycomb (
  string& which, int& status, command& cmd, string& shorth, string& help)
{
  string orig= which;
  // cout << which;
  variant_simplification (which);
  // cout << " -> " << which;
  string rew= apply_wildcards (which, post_kbd_wildcards);
  // cout << " -> " << rew << LF;
  object obj= find_key_binding (rew);
  // cout << rew << " => " << obj << LF;
  if (obj == object (false) || (orig != which && !is_string (car (obj)))) {
    status= 0;
    cmd   = command ();
    shorth= copy (rew);
    help  = "";
  }
  else if (!is_string (car (obj))) {
    status= 1;
    cmd   = as_command (car (obj));
    shorth= copy (rew);
    help  = as_string (cadr (obj));
  }
  else {
    status= 2;
    cmd   = command ();
    shorth= as_string (car (obj));
    help  = as_string (cadr (obj));
  }
}
