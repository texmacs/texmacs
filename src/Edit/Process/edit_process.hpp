
/******************************************************************************
* MODULE     : edit_process.hpp
* DESCRIPTION: Interface for automatically generated content
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef EDIT_PROCESS_H
#define EDIT_PROCESS_H
#include "editor.hpp"

class edit_process_rep: virtual public editor_rep {
protected:
  bool   new_mutators;
  bool   mutators_updated;
  int    nr_mutators;
  time_t next_mutate;
  time_t mutator_time;
  bool   math_input;
  string message_l;
  string message_r;
  hashmap<tree,tree> last_prompt;

public:
  edit_process_rep ();
  ~edit_process_rep ();

  void process_mutators ();
  path get_mutator_path ();
  time_t get_mutator_time ();
  void invalidate_mutators ();
  void insert_mutator (tree body, string cmd);

  void make_session (string lan, string session);
  void start_input (string lan, string session, path p);
  void process_input ();
  void start_output ();
  void session_message (string l, string r);
  void session_use_math_input (bool flag);
  bool session_is_using_math_input ();
  int  status_connection ();
  bool busy_connection ();
  void interrupt_connection ();
  void stop_connection ();

  void session_var_go_up ();
  void session_var_go_down ();
  void session_go_up ();
  void session_go_down ();
  void session_go_page_up ();
  void session_go_page_down ();
  void session_remove (bool forward);
  void session_insert_text_field ();
  void session_insert_input_at (path p);
  void session_insert_input_below ();
  void session_insert_input_above ();
  void session_fold_input ();
  void session_remove_input (bool forward);
  void session_remove_all_outputs ();
  void session_remove_previous_output ();
  void session_split ();
  bool session_complete_try (tree t);

  void generate_bibliography (string bib, string style, string fname);
  void generate_table_of_contents (string toc);
  void generate_index (string idx);
  void generate_glossary (string glo);
  void generate_aux (string which= "");
  bool get_save_aux ();

private:
  void generate_aux_recursively (string which, tree tt, path ip);
};

#endif // defined EDIT_PROCESS_H
