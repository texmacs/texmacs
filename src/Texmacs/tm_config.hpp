
/******************************************************************************
* MODULE     : tm_config.hpp
* DESCRIPTION: Configuration routines for TeXmacs server
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TM_CONFIG_H
#define TM_CONFIG_H
#include "server.hpp"
#include "hashmap.hpp"
#include "hashset.hpp"

class tm_config_rep: virtual public server_rep {
protected:
  string var_suffix;                        // space + the variant key
  string unvar_suffix;                      // space + the unvariant key
  hashmap<string,tree> pre_kbd_wildcards;   // wildcards applied to defns
  hashmap<string,tree> post_kbd_wildcards;  // wildcards applied at lookup
  hashmap<string,tree> system_kbd_decode;   // for printing of shortcuts

public:
  tm_config_rep ();
  ~tm_config_rep ();

  /* User preferences */
  string get_preference (string var);

  /* Font setup */
  void set_font_rules (scheme_tree rules);

  /* Keyboard behaviour */
  bool kbd_get_command (string which, string& help, command& cmd);

  void insert_kbd_wildcard (string key, string im, bool post, bool l, bool r);
  object find_key_binding (string key);
  string kbd_pre_rewrite (string l);
  string kbd_post_rewrite (string l, bool var_flag);
  tree kbd_system_rewrite (string l);
  void set_variant_keys (string var, string unvar);
  void variant_simplification (string& which);
  void get_keycomb (string& which, int& status,
		    command& cmd, string& shorth, string& help);
};

#endif // defined TM_CONFIG_H
