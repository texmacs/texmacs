
/******************************************************************************
* MODULE     : edit_typeset.hpp
* DESCRIPTION: the typeset structure for the mathematical editor
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef EDIT_TYPESET_H
#define EDIT_TYPESET_H
#include "env.hpp"
#include "typesetter.hpp"
#include "editor.hpp"
#include "hashset.hpp"

class document_rep;
class edit_typeset_rep: virtual public editor_rep {
protected:
  tree the_style;                         // document style
  hashmap<path,hashmap<string,tree> > cur; // environment at different paths
  hashmap<string,tree> stydef;            // environment after styles
  hashmap<string,tree> pre;               // environment after styles and init
  hashmap<string,tree> init;              // environment changes w.r.t. style
  hashmap<string,tree> fin;               // environment changes w.r.t. doc
  hashmap<string,tree> grefs;             // global references
  edit_env env;                           // the environment for typesetting
  typesetter ttt;                         // the (not) yet typesetted document

protected:
  typesetter           get_typesetter ();
  tree                 get_style ();
  void                 set_style (tree t);
  hashmap<string,tree> get_init ();
  hashmap<string,tree> get_fin ();
  hashmap<string,tree> get_att ();
  void                 set_init (hashmap<string,tree> init= tree ("?"));
  void                 add_init (hashmap<string,tree> init);
  void                 set_fin (hashmap<string,tree> fin);
  void                 set_att (hashmap<string,tree> att);

public:
  edit_typeset_rep ();
  ~edit_typeset_rep ();
  void clear_local_info ();
  void set_data (new_data data);
  void get_data (new_data& data);

  SI       as_length (string l);
  string   add_lengths (string l1, string l2);
  string   sub_lengths (string l1, string l2);
  string   max_lengths (string l1, string l2);
  string   min_lengths (string l1, string l2);
  string   multiply_length (double x, string l);
  bool     is_length (string s);
  double   divide_lengths (string l1, string l2);

  void     init_update ();
  void     drd_update ();
#ifdef EXPERIMENTAL
  void     environment_update ();
#endif
  tree     get_full_env ();
  bool     defined_at_cursor (string var_name);
  bool     defined_at_init (string var_name);
  bool     defined_in_init (string var_name);
  tree     get_env_value (string var_name, path p);
  tree     get_env_value (string var_name);
  tree     get_init_value (string var_name);
  string   get_env_string (string var_name);
  string   get_init_string (string var_name);
  int      get_env_int (string var_name);
  int      get_init_int (string var_name);
  double   get_env_double (string var_name);
  double   get_init_double (string var_name);
  language get_env_language ();
  int      get_page_count ();
  SI       get_page_width (bool deco);
  SI       get_pages_width (bool deco);
  SI       get_page_height (bool deco);
  SI       get_total_width (bool deco);
  SI       get_total_height (bool deco);

  tree     exec (tree t, hashmap<string,tree> env, bool expand_refs= true);
  tree     exec_texmacs (tree t, path p);
  tree     exec_texmacs (tree t);
  tree     exec_verbatim (tree t, path p);
  tree     exec_verbatim (tree t);
  tree     exec_html (tree t, path p);
  tree     exec_html (tree t);
  tree     exec_latex (tree t, path p);
  tree     exec_latex (tree t);
  tree     texmacs_exec (tree t);
  tree     var_texmacs_exec (tree t);

  tree     checkout_animation (tree t);
  tree     commit_animation (tree t);

  void     change_style (tree style);
  tree     get_init_all ();
  void     init_env (string var, tree by);
  void     init_default (string var);
  void     init_style ();
  void     init_style (string style);
  tree     get_att (string key);
  void     set_att (string key, tree im);
  void     reset_att (string key);
  array<string> list_atts ();

  void     typeset_style_use_cache (tree style);
  void     typeset_preamble ();
  void     typeset_prepare ();
  void     typeset_invalidate_env ();
  void     typeset_exec_until (path p);
  void     typeset_invalidate (path p);
  void     typeset_invalidate_all ();
  void     typeset_invalidate_players (path p, bool reattach);
  void     typeset_sub (SI& x1, SI& y1, SI& x2, SI& y2);
  void     typeset (SI& x1, SI& y1, SI& x2, SI& y2);
  void     typeset_forced ();

  friend class tm_window_rep;
  friend class tm_server_rep;
};

#endif // defined EDIT_TYPESET_H
