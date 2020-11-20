
/******************************************************************************
* MODULE     : edit_main.hpp
* DESCRIPTION: the main structure for the mathematical editor
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef EDIT_MAIN_H
#define EDIT_MAIN_H
#include "Interface/edit_interface.hpp"
#include "Interface/edit_cursor.hpp"
#include "Interface/edit_graphics.hpp"
#include "Editor/edit_typeset.hpp"
#include "Modify/edit_modify.hpp"
#include "Modify/edit_text.hpp"
#include "Modify/edit_math.hpp"
#include "Modify/edit_table.hpp"
#include "Modify/edit_dynamic.hpp"
#include "Process/edit_process.hpp"
#include "Replace/edit_select.hpp"
#include "Replace/edit_replace.hpp"

class edit_main_rep:
  public edit_interface_rep,
  public edit_cursor_rep,
  public edit_graphics_rep,
  public edit_typeset_rep,
  public edit_modify_rep,
  public edit_text_rep,
  public edit_math_rep,
  public edit_table_rep,
  public edit_dynamic_rep,
  public edit_process_rep,
  public edit_select_rep,
  public edit_replace_rep
{
private:
  hashmap<tree,tree> props;   // properties associated to the editor
  observer           ed_obs;  // edit observer attached to root of tree

public:
  edit_main_rep (server_rep* sv, tm_buffer buf);
  ~edit_main_rep ();
  virtual inline void* derived_this () {return (edit_main_rep*)this; }

  void set_property (scheme_tree what, scheme_tree val);
  void set_bool_property (string what, bool val);
  void set_int_property (string what, int val);
  void set_string_property (string what, string val);
  scheme_tree get_property (scheme_tree what);
  bool get_bool_property (string what);
  int get_int_property (string what);
  string get_string_property (string what);
  
  void clear_buffer ();
  void new_window ();
  void clone_window ();
  void tex_buffer ();
  url  get_name ();
  void focus_on_this_editor ();
  void notify_page_change ();

  string get_metadata (string kind);
  int  nr_pages ();
  void print_doc (url ps_name, bool to_file, int first, int last);
  void print_to_file (url ps_name, string first="1", string last="1000000");
  void print_buffer (string first="1", string last="1000000");
  void export_ps (url ps_name, string first="1", string last="1000000");
  array<int> print_snippet (url u, tree t, bool conserve_preamble);
  bool graphics_file_to_clipboard (url output);
  void footer_eval (string s);
  tree the_line ();
  tree the_root ();
  tree the_buffer ();
  bool test_subtree (path p);
  tree the_subtree (path p);
  path the_buffer_path ();
  path the_path ();
  path the_shifted_path ();

  void show_tree ();
  void show_env ();
  void show_path ();
  void show_cursor ();
  void show_selection ();
  void show_meminfo ();
  void edit_special ();
  void edit_test ();

  friend class editor;
};

#endif // defined EDIT_MAIN_H
