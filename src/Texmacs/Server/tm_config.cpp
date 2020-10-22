
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

#ifdef Q_OS_MAC
extern hashmap<int,string> qtcomposemap;
#endif

/******************************************************************************
* Constructor and destructor
******************************************************************************/

tm_config_rep::tm_config_rep ():
  var_suffix (" tab"), unvar_suffix (" S-tab") {}

tm_config_rep::~tm_config_rep () {}

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
  //cout << "Wildcard " << key << " -> " << im << "\n";
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
tm_config_rep::kbd_post_rewrite (string s, bool var_flag) {
  if (var_flag) variant_simplification (s);
  return apply_wildcards (s, post_kbd_wildcards);
}

void
tm_config_rep::get_keycomb (
  string& which, int& status, command& cmd, string& shorth, string& help)
{
  string orig= which;
  if (DEBUG_KEYBOARD) debug_keyboard << which;
  variant_simplification (which);
#ifdef Q_OS_MAC
  if (N(which) == 3 && starts (which, "A-") &&
      N(orig) == 7 && (orig == which * " tab") &&
      qtcomposemap->contains ((int) (unsigned char) which[2])) {
    string compose= qtcomposemap[(int) (unsigned char) which[2]];
    if (N(compose) > 1 &&
        cork_to_utf8 ("<" * compose * ">") != ("<" * compose * ">"))
      compose= "<" * compose * ">";
    which = orig;
    status= 2;
    cmd   = command ();
    shorth= copy (compose);
    help  = copy (compose);
    return;
  }
#endif
  if (DEBUG_KEYBOARD) debug_keyboard << " -> " << which;
  string rew= apply_wildcards (which, post_kbd_wildcards);
  bool no_var= false;
  if (rew * var_suffix == orig) {
    no_var= true;
    rew= var_suffix (1, N(var_suffix));
  }
  if (rew * unvar_suffix == orig) {
    no_var= true;
    rew= unvar_suffix (1, N(unvar_suffix));
  }
  if (DEBUG_KEYBOARD) debug_keyboard << " -> " << rew << LF;
  object obj= find_key_binding (rew);
  //cout << rew << " => " << obj << LF;
  //if (obj == object (false) || (orig != which && !is_string (car (obj)))) {
  if (obj == object (false)) {
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
  if (no_var) status += 3;
}

/******************************************************************************
* System dependent rendering of keyboard shortcuts
******************************************************************************/

static tree
localize (string s, bool mod_flag= false) {
  if (mod_flag) return tree (CONCAT, localize (s), "+");
  else return compound ("localize", s);
}

tree
mathop (string s) {
  return compound ("math", compound ("op", s));
}

static void
system_kbd_initialize (hashmap<string,tree>& h) {
  if (N(h) != 0);
  else if (use_macos_fonts ()) {
    h ("S-")= "<#21E7>";
    h ("C-")= "<#2303>";
    h ("A-")= "<#2325>";
    h ("M-")= "<#2318>";
    h ("H-")= localize ("Hyper");
    h ("windows")= localize ("Windows");
    h ("capslock")= "<#21EA>";
    h ("return")= "<#21A9>";
    h ("delete")= "<#2326>";
    h ("backspace")= "<#232B>";
    h ("clear")= "<#2327>";
    h ("escape")= "<#238B>";
    h ("space")= "Space";
    h ("var")= "<#21E5>";
    h ("tab")= "<#21E5>";
    h ("left")= "<#2190>";
    h ("right")= "<#2192>";
    h ("up")= "<#2191>";
    h ("down")= "<#2193>";
    h ("home")= "<#2196>";
    h ("end")= "<#2198>";
    h ("pageup")= "<#21DE>";
    h ("pagedown")= "<#21DF>";
    h ("space")= "<#2423>";
    h ("section")= "\237";
    h ("<less>")= "<#3C>";
    h ("<gtr>")= "<#3E>";
  }
  else if (gui_is_qt ()) {
    h ("S-")= localize ("Shift", true);
    h ("C-")= localize ("Ctrl", true);
    h ("A-")= localize ("Alt", true);
    h ("M-")= localize ("Meta", true);
    h ("H-")= localize ("Hyper", true);
    h ("windows")= localize ("Windows");
    h ("capslock")= localize ("Capslock");
    h ("return")= localize ("Return");
    h ("delete")= localize ("Delete");
    h ("backspace")= localize ("Backspace");
    h ("escape")= localize ("Escape");
    h ("space")= localize ("Space");
    h ("var")= localize ("Tab");
    h ("tab")= localize ("Tab");
    h ("left")= mathop ("<leftarrow>");
    h ("right")= mathop ("<rightarrow>");
    h ("up")= mathop ("<uparrow>");
    h ("down")= mathop ("<downarrow>");
    h ("home")= localize ("Home");
    h ("end")= localize ("End");
    h ("pageup")= localize ("PageUp");
    h ("pagedown")= localize ("PageDown");
    h ("section")= "\237";
  }
  else {
    h ("S-")= "S-";
    h ("C-")= "C-";
    h ("A-")= "A-";
    h ("M-")= "M-";
    h ("H-")= "H-";
    h ("windows")= localize ("windows");
    h ("capslock")= localize ("capslock");
    h ("return")= localize ("return");
    h ("delete")= localize ("delete");
    h ("backspace")= localize ("backspace");
    h ("escape")= localize ("escape");
    h ("space")= localize ("space");
    h ("var")= localize ("tab");
    h ("tab")= localize ("tab");
    h ("left")= mathop ("<leftarrow>");
    h ("right")= mathop ("<rightarrow>");
    h ("up")= mathop ("<uparrow>");
    h ("down")= mathop ("<downarrow>");
    h ("home")= localize ("home");
    h ("end")= localize ("end");
    h ("pageup")= localize ("pageup");
    h ("pagedown")= localize ("pagedown");
    h ("section")= "\237";
  }
}

static tree
kbd_render (tree t) {
  if (use_macos_fonts ())
    t= tree (WITH, "font", "apple-lucida", t);
  return compound ("render-key", t);
}

static string
kbd_system_prevails (string s) {
  string laf= get_preference ("look and feel");
  bool   mac= os_macos () && (laf == "default" || laf == "macos");
  if (mac && starts (s, "A-")) {
    string ss= s (2, N(s));
    string r = "escape " * ss;
    if (starts (ss, "S-")) ss= ss (2, N(ss));
    if (N(ss) == 1) return r;
    else return s;
  }
  else return s;
}

tree
tm_config_rep::kbd_system_rewrite (string s) {
  bool cs= (get_preference ("case sensitive shortcuts") == "on");
  system_kbd_initialize (system_kbd_decode);
  int start= 0, i;
  for (i=0; i <= N(s); i++)
    if (i == N(s) || s[i] == ' ') {
      string ss= s (start, i);
      string rr= kbd_system_prevails (ss);
      if (rr != ss)
        return kbd_system_rewrite (s (0, start) * rr * s (i, N(s)));
      start= i+1;
    }

  tree k (CONCAT);
  tree r (CONCAT);
  start= i= 0;
  while (true)
    if (i == N(s) || s[i] == '-' || s[i] == ' ') {
      if (i < N(s) && s[i] == '-') i++;
      string ss= s (start, i);
      if (system_kbd_decode->contains (ss)) r << system_kbd_decode[ss];
      else if (N(ss) == 1 && (use_macos_fonts () || gui_is_qt ()) && !cs) {
        if (is_locase (ss[0])) r << upcase_all (ss);
        else if (is_upcase (ss[0])) r << system_kbd_decode ("S-") << ss;
        else r << ss;
      }
      else r << ss;
      if (i == N(s) || s[i] == ' ') {
        k << kbd_render (simplify_concat (r));
        r= tree (CONCAT);
        if (i == N(s)) break;
        i++;
      }
      start= i;
    }
    else i++;
  return simplify_concat (k);
}
