
/******************************************************************************
* MODULE     : tm_config.hpp
* DESCRIPTION: Configuration routines for TeXmacs server
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TM_CONFIG_H
#define TM_CONFIG_H
#include "server.hpp"
#include "hashmap.hpp"
#include "hashset.hpp"

class tm_config_rep: virtual public server_rep {
protected:
  string in_lan;                             // current input language
  string out_lan;                            // current output language
  string var_suffix;                         // space + the variant key
  string unvar_suffix;                       // space + the unvariant key
  hashmap<string,tree>   pre_kbd_wildcards;  // wildcards applied to defns
  hashmap<string,tree>   post_kbd_wildcards; // wildcards applied at lookup

public:
  tm_config_rep ();
  ~tm_config_rep ();

  /* User preferences */
  string get_preference (string var);

  /* Set and get input and output languages */
  void   set_input_language (string s);
  void   set_output_language (string s);
  string get_input_language ();
  string get_output_language ();

  /* Font setup */
  void set_font_rules (scheme_tree rules);

  /* Keyboard behaviour */
  bool kbd_get_command (string which, string& help, command& cmd);

  void insert_kbd_wildcard (string key, string im, bool post, bool l, bool r);
  object find_key_binding (string key);
  string kbd_pre_rewrite (string l);
  string kbd_post_rewrite (string l);
  void set_variant_keys (string var, string unvar);
  void variant_simplification (string& which);
  void get_keycomb (string& which, int& status,
		    command& cmd, string& shorth, string& help);
};

#endif // defined TM_CONFIG_H
