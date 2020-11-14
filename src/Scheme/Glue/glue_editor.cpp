
/******************************************************************************
*
* This file has been generated automatically using build-glue.scm
* from build-glue-editor.scm. Please do not edit its contents.
* Copyright (C) 2000 Joris van der Hoeven
*
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*
******************************************************************************/

tmscm
tmg_root_tree () {
  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->the_root ();
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_buffer_path () {
  // TMSCM_DEFER_INTS;
  path out= get_current_editor()->the_buffer_path ();
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_buffer_tree () {
  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->the_buffer ();
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_paragraph_tree () {
  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->the_line ();
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_cursor_path () {
  // TMSCM_DEFER_INTS;
  path out= get_current_editor()->the_path ();
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_cursor_path_dot () {
  // TMSCM_DEFER_INTS;
  path out= get_current_editor()->the_shifted_path ();
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_selection_tree () {
  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->selection_get ();
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_path_2tree (tmscm arg1) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "path->tree");

  path in1= tmscm_to_path (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->the_subtree (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_path_correct_old (tmscm arg1) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "path-correct-old");

  path in1= tmscm_to_path (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->correct (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_path_insert_with (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "path-insert-with");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "path-insert-with");
  TMSCM_ASSERT_CONTENT (arg3, TMSCM_ARG3, "path-insert-with");

  path in1= tmscm_to_path (arg1);
  string in2= tmscm_to_string (arg2);
  content in3= tmscm_to_content (arg3);

  // TMSCM_DEFER_INTS;
  get_current_editor()->insert_with (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_path_remove_with (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "path-remove-with");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "path-remove-with");

  path in1= tmscm_to_path (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->remove_with (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_position_new_path (tmscm arg1) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "position-new-path");

  path in1= tmscm_to_path (arg1);

  // TMSCM_DEFER_INTS;
  observer out= get_current_editor()->position_new (in1);
  // TMSCM_ALLOW_INTS;

  return observer_to_tmscm (out);
}

tmscm
tmg_position_delete (tmscm arg1) {
  TMSCM_ASSERT_OBSERVER (arg1, TMSCM_ARG1, "position-delete");

  observer in1= tmscm_to_observer (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->position_delete (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_position_set (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_OBSERVER (arg1, TMSCM_ARG1, "position-set");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "position-set");

  observer in1= tmscm_to_observer (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->position_set (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_position_get (tmscm arg1) {
  TMSCM_ASSERT_OBSERVER (arg1, TMSCM_ARG1, "position-get");

  observer in1= tmscm_to_observer (arg1);

  // TMSCM_DEFER_INTS;
  path out= get_current_editor()->position_get (in1);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_insideP (tmscm arg1) {
  TMSCM_ASSERT_TREE_LABEL (arg1, TMSCM_ARG1, "inside?");

  tree_label in1= tmscm_to_tree_label (arg1);

  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->inside (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_cpp_insert (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "cpp-insert");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->insert_tree (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_insert_go_to (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "cpp-insert-go-to");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "cpp-insert-go-to");

  content in1= tmscm_to_content (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->var_insert_tree (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_insert_raw_go_to (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "insert-raw-go-to");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "insert-raw-go-to");

  content in1= tmscm_to_content (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->insert_tree (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_insert_raw_return () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->insert_return ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_remove_text (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "remove-text");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->remove_text (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_remove_structure (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "remove-structure");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->remove_structure (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_remove_structure_upwards () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->remove_structure_upwards ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_make (tmscm arg1) {
  TMSCM_ASSERT_TREE_LABEL (arg1, TMSCM_ARG1, "cpp-make");

  tree_label in1= tmscm_to_tree_label (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_compound (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_make_arity (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE_LABEL (arg1, TMSCM_ARG1, "cpp-make-arity");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "cpp-make-arity");

  tree_label in1= tmscm_to_tree_label (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_compound (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_activate () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->activate ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_insert_argument (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "insert-argument");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->insert_argument (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_remove_argument (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "remove-argument");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->remove_argument (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_insert_argument_at (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "insert-argument-at");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "insert-argument-at");

  path in1= tmscm_to_path (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->insert_argument (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_remove_argument_at (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "remove-argument-at");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "remove-argument-at");

  path in1= tmscm_to_path (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->remove_argument (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_make_with (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cpp-make-with");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "cpp-make-with");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_with (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_make_mod_active (tmscm arg1) {
  TMSCM_ASSERT_TREE_LABEL (arg1, TMSCM_ARG1, "make-mod-active");

  tree_label in1= tmscm_to_tree_label (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_mod_active (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_make_style_with (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "make-style-with");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "make-style-with");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_style_with (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_make_hybrid () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->make_hybrid ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_activate_latex () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->activate_latex ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_activate_hybrid (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "activate-hybrid");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->activate_hybrid (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_activate_symbol () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->activate_symbol ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_make_return_before () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->make_return_before ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_make_return_after () {
  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->make_return_after ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_temp_proof_fix () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->temp_proof_fix ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_full_env () {
  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->get_full_env ();
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_get_all_inits () {
  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->get_init_all ();
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_init_default_one (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "init-default-one");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->init_default (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_init_env (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "init-env");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "init-env");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->init_env (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_init_env_tree (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "init-env-tree");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "init-env-tree");

  string in1= tmscm_to_string (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->init_env (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_init_style (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "init-style");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->init_style (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_style_tree () {
  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->get_style ();
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_set_style_tree (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "set-style-tree");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->change_style (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_env (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "get-env");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= get_current_editor()->get_env_string (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_get_env_tree (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "get-env-tree");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->get_env_value (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_get_env_tree_at (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "get-env-tree-at");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "get-env-tree-at");

  string in1= tmscm_to_string (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->get_env_value (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_get_init (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "get-init");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= get_current_editor()->get_init_string (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_get_init_tree (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "get-init-tree");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->get_init_value (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_context_hasP (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "context-has?");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->defined_at_cursor (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_style_hasP (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "style-has?");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->defined_at_init (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_init_hasP (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "init-has?");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->defined_in_init (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_get_page_count () {
  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->get_page_count ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_get_page_width (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "get-page-width");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->get_page_width (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_get_pages_width (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "get-pages-width");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->get_pages_width (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_get_page_height (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "get-page-height");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->get_page_height (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_get_total_width (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "get-total-width");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->get_total_width (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_get_total_height (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "get-total-height");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->get_total_height (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_get_reference (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "get-reference");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->get_ref (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_set_reference (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "set-reference");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "set-reference");

  string in1= tmscm_to_string (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->set_ref (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_reset_reference (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "reset-reference");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->reset_ref (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_find_references (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "find-references");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= get_current_editor()->find_refs (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_list_references () {
  // TMSCM_DEFER_INTS;
  array_string out= get_current_editor()->list_refs ();
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_list_references_dot (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "list-references*");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= get_current_editor()->list_refs (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_get_auxiliary (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "get-auxiliary");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->get_aux (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_set_auxiliary (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "set-auxiliary");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "set-auxiliary");

  string in1= tmscm_to_string (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->set_aux (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_reset_auxiliary (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "reset-auxiliary");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->reset_aux (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_list_auxiliaries () {
  // TMSCM_DEFER_INTS;
  array_string out= get_current_editor()->list_auxs ();
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_list_auxiliaries_dot (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "list-auxiliaries*");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= get_current_editor()->list_auxs (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_get_attachment (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "get-attachment");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->get_att (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_set_attachment (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "set-attachment");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "set-attachment");

  string in1= tmscm_to_string (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->set_att (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_reset_attachment (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "reset-attachment");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->reset_att (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_list_attachments () {
  // TMSCM_DEFER_INTS;
  array_string out= get_current_editor()->list_atts ();
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_list_attachments_dot (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "list-attachments*");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= get_current_editor()->list_atts (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_make_htab (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "make-htab");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_htab (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_make_space (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "make-space");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_space (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_make_var_space (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "make-var-space");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "make-var-space");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "make-var-space");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_space (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_make_hspace (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "make-hspace");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_hspace (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_make_var_hspace (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "make-var-hspace");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "make-var-hspace");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "make-var-hspace");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_hspace (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_make_vspace_before (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "make-vspace-before");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_vspace_before (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_make_var_vspace_before (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "make-var-vspace-before");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "make-var-vspace-before");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "make-var-vspace-before");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_vspace_before (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_make_vspace_after (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "make-vspace-after");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_vspace_after (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_make_var_vspace_after (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "make-var-vspace-after");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "make-var-vspace-after");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "make-var-vspace-after");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_vspace_after (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_make_image (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5, tmscm arg6) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "make-image");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "make-image");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "make-image");
  TMSCM_ASSERT_STRING (arg4, TMSCM_ARG4, "make-image");
  TMSCM_ASSERT_STRING (arg5, TMSCM_ARG5, "make-image");
  TMSCM_ASSERT_STRING (arg6, TMSCM_ARG6, "make-image");

  string in1= tmscm_to_string (arg1);
  bool in2= tmscm_to_bool (arg2);
  string in3= tmscm_to_string (arg3);
  string in4= tmscm_to_string (arg4);
  string in5= tmscm_to_string (arg5);
  string in6= tmscm_to_string (arg6);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_image (in1, in2, in3, in4, in5, in6);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_length_decode (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "length-decode");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->as_length (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_length_add (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "length-add");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "length-add");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  string out= get_current_editor()->add_lengths (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_length_sub (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "length-sub");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "length-sub");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  string out= get_current_editor()->sub_lengths (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_length_max (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "length-max");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "length-max");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  string out= get_current_editor()->max_lengths (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_length_min (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "length-min");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "length-min");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  string out= get_current_editor()->min_lengths (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_length_mult (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_DOUBLE (arg1, TMSCM_ARG1, "length-mult");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "length-mult");

  double in1= tmscm_to_double (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  string out= get_current_editor()->multiply_length (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_lengthP (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "length?");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->is_length (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_length_divide (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "length-divide");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "length-divide");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  double out= get_current_editor()->divide_lengths (in1, in2);
  // TMSCM_ALLOW_INTS;

  return double_to_tmscm (out);
}

tmscm
tmg_cpp_make_rigid () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->make_rigid ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_make_lprime (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cpp-make-lprime");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_lprime (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_make_rprime (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cpp-make-rprime");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_rprime (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_make_below () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->make_below ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_make_above () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->make_above ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_make_script (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "cpp-make-script");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "cpp-make-script");

  bool in1= tmscm_to_bool (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_script (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_make_fraction () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->make_fraction ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_make_sqrt () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->make_sqrt ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_make_wide (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cpp-make-wide");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_wide (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_make_wide_under (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cpp-make-wide-under");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->make_wide_under (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_make_var_sqrt () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->make_var_sqrt ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_make_neg () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->make_neg ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_make_tree () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->make_tree ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_make_subtable () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->make_subtable ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_table_deactivate () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->table_deactivate ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_table_extract_format () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->table_extract_format ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_table_insert_row (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "table-insert-row");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->table_insert_row (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_table_insert_column (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "table-insert-column");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->table_insert_column (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_table_remove_row (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "table-remove-row");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->table_remove_row (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_table_remove_column (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "table-remove-column");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->table_remove_column (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_table_nr_rows () {
  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->table_nr_rows ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_table_nr_columns () {
  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->table_nr_columns ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_table_get_extents () {
  // TMSCM_DEFER_INTS;
  array_int out= get_current_editor()->table_get_extents ();
  // TMSCM_ALLOW_INTS;

  return array_int_to_tmscm (out);
}

tmscm
tmg_table_set_extents (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "table-set-extents");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "table-set-extents");

  int in1= tmscm_to_int (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->table_set_extents (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_table_which_row () {
  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->table_which_row ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_table_which_column () {
  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->table_which_column ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_table_which_cells () {
  // TMSCM_DEFER_INTS;
  array_int out= get_current_editor()->table_which_cells ();
  // TMSCM_ALLOW_INTS;

  return array_int_to_tmscm (out);
}

tmscm
tmg_table_cell_path (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "table-cell-path");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "table-cell-path");

  int in1= tmscm_to_int (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  path out= get_current_editor()->table_search_cell (in1, in2);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_table_go_to (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "table-go-to");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "table-go-to");

  int in1= tmscm_to_int (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->table_go_to (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_table_set_format (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "table-set-format");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "table-set-format");

  string in1= tmscm_to_string (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->table_set_format (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_table_get_format_all () {
  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->table_get_format ();
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_table_get_format (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "table-get-format");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= get_current_editor()->table_get_format (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_table_del_format (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "table-del-format");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->table_del_format (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_table_row_decoration (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "table-row-decoration");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->table_row_decoration (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_table_column_decoration (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "table-column-decoration");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->table_column_decoration (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_table_format_center () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->table_format_center ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_table_correct_block_content () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->table_correct_block_content ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_cell_mode (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "set-cell-mode");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->set_cell_mode (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_cell_mode () {
  // TMSCM_DEFER_INTS;
  string out= get_current_editor()->get_cell_mode ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_cell_set_format (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cell-set-format");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "cell-set-format");

  string in1= tmscm_to_string (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->cell_set_format (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cell_get_format (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cell-get-format");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= get_current_editor()->cell_get_format (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_cell_del_format (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cell-del-format");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->cell_del_format (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_table_test () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->table_test ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_key_press (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "key-press");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->key_press (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_raw_emulate_keyboard (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "raw-emulate-keyboard");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->emulate_keyboard (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_complete_tryP () {
  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->complete_try ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_get_input_mode () {
  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->get_input_mode ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_key_press_search (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "key-press-search");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->search_keypress (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_key_press_replace (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "key-press-replace");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->replace_keypress (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_key_press_spell (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "key-press-spell");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->spell_keypress (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_key_press_complete (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "key-press-complete");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->complete_keypress (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_mouse_any (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "mouse-any");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "mouse-any");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "mouse-any");
  TMSCM_ASSERT_INT (arg4, TMSCM_ARG4, "mouse-any");
  TMSCM_ASSERT_DOUBLE (arg5, TMSCM_ARG5, "mouse-any");

  string in1= tmscm_to_string (arg1);
  int in2= tmscm_to_int (arg2);
  int in3= tmscm_to_int (arg3);
  int in4= tmscm_to_int (arg4);
  double in5= tmscm_to_double (arg5);

  // TMSCM_DEFER_INTS;
  get_current_editor()->mouse_any (in1, in2, in3, in4, in5);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_mouse_position () {
  // TMSCM_DEFER_INTS;
  array_int out= get_current_editor()->get_mouse_position ();
  // TMSCM_ALLOW_INTS;

  return array_int_to_tmscm (out);
}

tmscm
tmg_set_mouse_pointer (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "set-mouse-pointer");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "set-mouse-pointer");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->set_pointer (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_predef_mouse_pointer (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "set-predef-mouse-pointer");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->set_pointer (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_go_to_path (tmscm arg1) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "go-to-path");

  path in1= tmscm_to_path (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->go_to (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_go_left () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->go_left ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_go_right () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->go_right ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_go_up () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->go_up ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_go_down () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->go_down ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_go_start () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->go_start ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_go_end () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->go_end ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_go_start_of (tmscm arg1) {
  TMSCM_ASSERT_TREE_LABEL (arg1, TMSCM_ARG1, "go-start-of");

  tree_label in1= tmscm_to_tree_label (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->go_start_of (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_go_end_of (tmscm arg1) {
  TMSCM_ASSERT_TREE_LABEL (arg1, TMSCM_ARG1, "go-end-of");

  tree_label in1= tmscm_to_tree_label (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->go_end_of (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_go_start_with (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "go-start-with");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "go-start-with");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->go_start_with (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_go_end_with (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "go-end-with");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "go-end-with");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->go_end_with (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_go_start_line () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->go_start_line ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_go_end_line () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->go_end_line ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_go_page_up () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->go_page_up ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_go_page_down () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->go_page_down ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_go_start_paragraph () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->go_start_paragraph ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_go_end_paragraph () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->go_end_paragraph ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_label_2path (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "label->path");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  path out= get_current_editor()->search_label (in1);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_go_to_label (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "go-to-label");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->go_to_label (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cursor_accessibleP () {
  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->cursor_is_accessible ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_cursor_show_if_hidden () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->show_cursor_if_hidden ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_select_all () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->select_all ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_select_line () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->select_line ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_select_from_cursor () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->select_from_cursor ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_select_from_cursor_if_active () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->select_from_cursor_if_active ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_select_from_keyboard (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "select-from-keyboard");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->select_from_keyboard (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_select_from_shift_keyboard () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->select_from_shift_keyboard ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_select_enlarge () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->select_enlarge ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_select_enlarge_environmental () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->select_enlarge_environmental ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_selection_active_anyP () {
  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->selection_active_any ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_selection_active_normalP () {
  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->selection_active_normal ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_selection_active_tableP () {
  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->selection_active_table ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_selection_active_smallP () {
  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->selection_active_small ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_selection_active_enlargingP () {
  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->selection_active_enlarging ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_selection_set_start () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->selection_set_start ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_selection_set_end () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->selection_set_end ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_selection_get_start () {
  // TMSCM_DEFER_INTS;
  path out= get_current_editor()->selection_get_start ();
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_selection_get_end () {
  // TMSCM_DEFER_INTS;
  path out= get_current_editor()->selection_get_end ();
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_selection_get_start_dot () {
  // TMSCM_DEFER_INTS;
  path out= get_current_editor()->selection_var_get_start ();
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_selection_get_end_dot () {
  // TMSCM_DEFER_INTS;
  path out= get_current_editor()->selection_var_get_end ();
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_selection_path () {
  // TMSCM_DEFER_INTS;
  path out= get_current_editor()->selection_get_path ();
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_selection_set (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "selection-set");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "selection-set");

  path in1= tmscm_to_path (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->selection_set_paths (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_selection_set_range_set (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_PATH (arg1, TMSCM_ARG1, "selection-set-range-set");

  array_path in1= tmscm_to_array_path (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->selection_set_range_set (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_clipboard_set (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "clipboard-set");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "clipboard-set");

  string in1= tmscm_to_string (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->selection_set (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_clipboard_get (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "clipboard-get");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->selection_get (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_cpp_clipboard_copy (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cpp-clipboard-copy");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->selection_copy (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_clipboard_cut (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cpp-clipboard-cut");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->selection_cut (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_clipboard_cut_at (tmscm arg1) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "clipboard-cut-at");

  path in1= tmscm_to_path (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->cut (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_clipboard_cut_between (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "clipboard-cut-between");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "clipboard-cut-between");

  path in1= tmscm_to_path (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->cut (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_clipboard_paste (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cpp-clipboard-paste");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->selection_paste (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_selection_move () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->selection_move ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_clipboard_clear (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "clipboard-clear");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->selection_clear (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_selection_cancel () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->selection_cancel ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_clipboard_set_import (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "clipboard-set-import");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->selection_set_import (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_clipboard_set_export (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "clipboard-set-export");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->selection_set_export (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_clipboard_get_import () {
  // TMSCM_DEFER_INTS;
  string out= get_current_editor()->selection_get_import ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_clipboard_get_export () {
  // TMSCM_DEFER_INTS;
  string out= get_current_editor()->selection_get_export ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_set_manual_focus_path (tmscm arg1) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "set-manual-focus-path");

  path in1= tmscm_to_path (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->manual_focus_set (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_manual_focus_path () {
  // TMSCM_DEFER_INTS;
  path out= get_current_editor()->manual_focus_get ();
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_get_focus_path () {
  // TMSCM_DEFER_INTS;
  path out= get_current_editor()->focus_get ();
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_set_alt_selection (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "set-alt-selection");
  TMSCM_ASSERT_ARRAY_PATH (arg2, TMSCM_ARG2, "set-alt-selection");

  string in1= tmscm_to_string (arg1);
  array_path in2= tmscm_to_array_path (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->set_alt_selection (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_alt_selection (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "get-alt-selection");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  array_path out= get_current_editor()->get_alt_selection (in1);
  // TMSCM_ALLOW_INTS;

  return array_path_to_tmscm (out);
}

tmscm
tmg_cancel_alt_selection (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cancel-alt-selection");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->cancel_alt_selection (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cancel_alt_selections () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->cancel_alt_selections ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_clear_undo_history () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->clear_undo_history ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_commit_changes () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->end_editing ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_start_slave (tmscm arg1) {
  TMSCM_ASSERT_DOUBLE (arg1, TMSCM_ARG1, "start-slave");

  double in1= tmscm_to_double (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->start_slave (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_mark_start (tmscm arg1) {
  TMSCM_ASSERT_DOUBLE (arg1, TMSCM_ARG1, "mark-start");

  double in1= tmscm_to_double (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->mark_start (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_mark_end (tmscm arg1) {
  TMSCM_ASSERT_DOUBLE (arg1, TMSCM_ARG1, "mark-end");

  double in1= tmscm_to_double (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->mark_end (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_mark_cancel (tmscm arg1) {
  TMSCM_ASSERT_DOUBLE (arg1, TMSCM_ARG1, "mark-cancel");

  double in1= tmscm_to_double (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->mark_cancel (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_remove_undo_mark () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->remove_undo_mark ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_add_undo_mark () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->add_undo_mark ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_unredoable_undo () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->unredoable_undo ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_undo_possibilities () {
  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->undo_possibilities ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_undo (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "undo");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->undo (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_redo_possibilities () {
  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->redo_possibilities ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_redo (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "redo");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->redo (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_show_history () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->show_history ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_archive_state () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->archive_state ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_start_editing () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->start_editing ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_end_editing () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->end_editing ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cancel_editing () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->cancel_editing ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_in_graphicsP () {
  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->inside_graphics ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_get_graphical_x () {
  // TMSCM_DEFER_INTS;
  double out= get_current_editor()->get_x ();
  // TMSCM_ALLOW_INTS;

  return double_to_tmscm (out);
}

tmscm
tmg_get_graphical_y () {
  // TMSCM_DEFER_INTS;
  double out= get_current_editor()->get_y ();
  // TMSCM_ALLOW_INTS;

  return double_to_tmscm (out);
}

tmscm
tmg_get_graphical_object () {
  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->get_graphical_object ();
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_set_graphical_object (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "set-graphical-object");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->set_graphical_object (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_invalidate_graphical_object () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->invalidate_graphical_object ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_graphical_select (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_DOUBLE (arg1, TMSCM_ARG1, "graphical-select");
  TMSCM_ASSERT_DOUBLE (arg2, TMSCM_ARG2, "graphical-select");

  double in1= tmscm_to_double (arg1);
  double in2= tmscm_to_double (arg2);

  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->graphical_select (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_graphical_select_area (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_DOUBLE (arg1, TMSCM_ARG1, "graphical-select-area");
  TMSCM_ASSERT_DOUBLE (arg2, TMSCM_ARG2, "graphical-select-area");
  TMSCM_ASSERT_DOUBLE (arg3, TMSCM_ARG3, "graphical-select-area");
  TMSCM_ASSERT_DOUBLE (arg4, TMSCM_ARG4, "graphical-select-area");

  double in1= tmscm_to_double (arg1);
  double in2= tmscm_to_double (arg2);
  double in3= tmscm_to_double (arg3);
  double in4= tmscm_to_double (arg4);

  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->graphical_select (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_in_normal_modeP () {
  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->in_normal_mode ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_in_search_modeP () {
  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->in_search_mode ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_in_replace_modeP () {
  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->in_replace_mode ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_in_spell_modeP () {
  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->in_spell_mode ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_search_start (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "search-start");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->search_start (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_search_button_next () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->search_button_next ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_replace_start (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "replace-start");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "replace-start");
  TMSCM_ASSERT_BOOL (arg3, TMSCM_ARG3, "replace-start");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  bool in3= tmscm_to_bool (arg3);

  // TMSCM_DEFER_INTS;
  get_current_editor()->replace_start (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_spell_start () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->spell_start ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_spell_replace (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "spell-replace");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->spell_replace (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_session_complete_command (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "session-complete-command");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  string out= get_current_editor()->session_complete_command (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_custom_complete (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "custom-complete");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->custom_complete (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_keyboard_focus_on (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "keyboard-focus-on");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->keyboard_focus_on (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_view_set_property (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "view-set-property");
  TMSCM_ASSERT_SCHEME_TREE (arg2, TMSCM_ARG2, "view-set-property");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);
  scheme_tree in2= tmscm_to_scheme_tree (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->set_property (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_view_get_property (tmscm arg1) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "view-get-property");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);

  // TMSCM_DEFER_INTS;
  scheme_tree out= get_current_editor()->get_property (in1);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_get_window_width () {
  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->get_window_width ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_get_window_height () {
  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->get_window_height ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_get_window_x () {
  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->get_window_x ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_get_window_y () {
  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->get_window_y ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_get_canvas_x () {
  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->get_canvas_x ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_get_canvas_y () {
  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->get_canvas_y ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_get_scroll_x () {
  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->get_scroll_x ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_get_scroll_y () {
  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->get_scroll_y ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_clear_buffer () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->clear_buffer ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tex_buffer () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->tex_buffer ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_clear_local_info () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->clear_local_info ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_refresh_window () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->invalidate_all ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_update_forced () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->typeset_forced ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_update_path (tmscm arg1) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "update-path");

  path in1= tmscm_to_path (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->typeset_invalidate (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_update_current_buffer () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->typeset_invalidate_all ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_update_players (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "update-players");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "update-players");

  path in1= tmscm_to_path (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->typeset_invalidate_players (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_generate_all_aux () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->generate_aux ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_generate_aux (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "generate-aux");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->generate_aux (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_notify_page_change () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->notify_page_change ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_notify_change (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "notify-change");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->notify_change (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_metadata (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "get-metadata");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= get_current_editor()->get_metadata (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_cpp_nr_pages () {
  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->nr_pages ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_print_to_file (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "print-to-file");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->print_to_file (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_print_pages_to_file (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "print-pages-to-file");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "print-pages-to-file");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "print-pages-to-file");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  get_current_editor()->print_to_file (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_print () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->print_buffer ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_print_pages (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "print-pages");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "print-pages");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  get_current_editor()->print_buffer (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_print_snippet (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "print-snippet");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "print-snippet");
  TMSCM_ASSERT_BOOL (arg3, TMSCM_ARG3, "print-snippet");

  url in1= tmscm_to_url (arg1);
  content in2= tmscm_to_content (arg2);
  bool in3= tmscm_to_bool (arg3);

  // TMSCM_DEFER_INTS;
  array_int out= get_current_editor()->print_snippet (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return array_int_to_tmscm (out);
}

tmscm
tmg_graphics_file_to_clipboard (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "graphics-file-to-clipboard");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= get_current_editor()->graphics_file_to_clipboard (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_export_postscript (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "export-postscript");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->export_ps (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_export_pages_postscript (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "export-pages-postscript");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "export-pages-postscript");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "export-pages-postscript");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  get_current_editor()->export_ps (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_footer_eval (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "footer-eval");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_current_editor()->footer_eval (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_texmacs_exec (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "texmacs-exec");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->texmacs_exec (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_texmacs_exec_dot (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "texmacs-exec*");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->var_texmacs_exec (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_texmacs_expand (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "texmacs-expand");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->exec_texmacs (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_verbatim_expand (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "verbatim-expand");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->exec_verbatim (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_latex_expand (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "latex-expand");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->exec_latex (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_html_expand (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "html-expand");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->exec_html (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_animate_checkout (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "animate-checkout");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->checkout_animation (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_animate_commit (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "animate-commit");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->commit_animation (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_idle_time () {
  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->idle_time ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_change_time () {
  // TMSCM_DEFER_INTS;
  int out= get_current_editor()->change_time ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_menu_before_action () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->before_menu_action ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_menu_after_action () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->after_menu_action ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_update_menus () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->update_menus ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_show_tree () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->show_tree ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_show_env () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->show_env ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_show_path () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->show_path ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_show_cursor () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->show_cursor ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_show_selection () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->show_selection ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_show_meminfo () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->show_meminfo ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_edit_special () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->edit_special ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_edit_test () {
  // TMSCM_DEFER_INTS;
  get_current_editor()->edit_test ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

void
initialize_glue_editor () {
  tmscm_install_procedure ("root-tree",  tmg_root_tree, 0, 0, 0);
  tmscm_install_procedure ("buffer-path",  tmg_buffer_path, 0, 0, 0);
  tmscm_install_procedure ("buffer-tree",  tmg_buffer_tree, 0, 0, 0);
  tmscm_install_procedure ("paragraph-tree",  tmg_paragraph_tree, 0, 0, 0);
  tmscm_install_procedure ("cursor-path",  tmg_cursor_path, 0, 0, 0);
  tmscm_install_procedure ("cursor-path*",  tmg_cursor_path_dot, 0, 0, 0);
  tmscm_install_procedure ("selection-tree",  tmg_selection_tree, 0, 0, 0);
  tmscm_install_procedure ("path->tree",  tmg_path_2tree, 1, 0, 0);
  tmscm_install_procedure ("path-correct-old",  tmg_path_correct_old, 1, 0, 0);
  tmscm_install_procedure ("path-insert-with",  tmg_path_insert_with, 3, 0, 0);
  tmscm_install_procedure ("path-remove-with",  tmg_path_remove_with, 2, 0, 0);
  tmscm_install_procedure ("position-new-path",  tmg_position_new_path, 1, 0, 0);
  tmscm_install_procedure ("position-delete",  tmg_position_delete, 1, 0, 0);
  tmscm_install_procedure ("position-set",  tmg_position_set, 2, 0, 0);
  tmscm_install_procedure ("position-get",  tmg_position_get, 1, 0, 0);
  tmscm_install_procedure ("inside?",  tmg_insideP, 1, 0, 0);
  tmscm_install_procedure ("cpp-insert",  tmg_cpp_insert, 1, 0, 0);
  tmscm_install_procedure ("cpp-insert-go-to",  tmg_cpp_insert_go_to, 2, 0, 0);
  tmscm_install_procedure ("insert-raw-go-to",  tmg_insert_raw_go_to, 2, 0, 0);
  tmscm_install_procedure ("insert-raw-return",  tmg_insert_raw_return, 0, 0, 0);
  tmscm_install_procedure ("remove-text",  tmg_remove_text, 1, 0, 0);
  tmscm_install_procedure ("remove-structure",  tmg_remove_structure, 1, 0, 0);
  tmscm_install_procedure ("remove-structure-upwards",  tmg_remove_structure_upwards, 0, 0, 0);
  tmscm_install_procedure ("cpp-make",  tmg_cpp_make, 1, 0, 0);
  tmscm_install_procedure ("cpp-make-arity",  tmg_cpp_make_arity, 2, 0, 0);
  tmscm_install_procedure ("activate",  tmg_activate, 0, 0, 0);
  tmscm_install_procedure ("insert-argument",  tmg_insert_argument, 1, 0, 0);
  tmscm_install_procedure ("remove-argument",  tmg_remove_argument, 1, 0, 0);
  tmscm_install_procedure ("insert-argument-at",  tmg_insert_argument_at, 2, 0, 0);
  tmscm_install_procedure ("remove-argument-at",  tmg_remove_argument_at, 2, 0, 0);
  tmscm_install_procedure ("cpp-make-with",  tmg_cpp_make_with, 2, 0, 0);
  tmscm_install_procedure ("make-mod-active",  tmg_make_mod_active, 1, 0, 0);
  tmscm_install_procedure ("make-style-with",  tmg_make_style_with, 2, 0, 0);
  tmscm_install_procedure ("cpp-make-hybrid",  tmg_cpp_make_hybrid, 0, 0, 0);
  tmscm_install_procedure ("activate-latex",  tmg_activate_latex, 0, 0, 0);
  tmscm_install_procedure ("activate-hybrid",  tmg_activate_hybrid, 1, 0, 0);
  tmscm_install_procedure ("activate-symbol",  tmg_activate_symbol, 0, 0, 0);
  tmscm_install_procedure ("make-return-before",  tmg_make_return_before, 0, 0, 0);
  tmscm_install_procedure ("make-return-after",  tmg_make_return_after, 0, 0, 0);
  tmscm_install_procedure ("temp-proof-fix",  tmg_temp_proof_fix, 0, 0, 0);
  tmscm_install_procedure ("get-full-env",  tmg_get_full_env, 0, 0, 0);
  tmscm_install_procedure ("get-all-inits",  tmg_get_all_inits, 0, 0, 0);
  tmscm_install_procedure ("init-default-one",  tmg_init_default_one, 1, 0, 0);
  tmscm_install_procedure ("init-env",  tmg_init_env, 2, 0, 0);
  tmscm_install_procedure ("init-env-tree",  tmg_init_env_tree, 2, 0, 0);
  tmscm_install_procedure ("init-style",  tmg_init_style, 1, 0, 0);
  tmscm_install_procedure ("get-style-tree",  tmg_get_style_tree, 0, 0, 0);
  tmscm_install_procedure ("set-style-tree",  tmg_set_style_tree, 1, 0, 0);
  tmscm_install_procedure ("get-env",  tmg_get_env, 1, 0, 0);
  tmscm_install_procedure ("get-env-tree",  tmg_get_env_tree, 1, 0, 0);
  tmscm_install_procedure ("get-env-tree-at",  tmg_get_env_tree_at, 2, 0, 0);
  tmscm_install_procedure ("get-init",  tmg_get_init, 1, 0, 0);
  tmscm_install_procedure ("get-init-tree",  tmg_get_init_tree, 1, 0, 0);
  tmscm_install_procedure ("context-has?",  tmg_context_hasP, 1, 0, 0);
  tmscm_install_procedure ("style-has?",  tmg_style_hasP, 1, 0, 0);
  tmscm_install_procedure ("init-has?",  tmg_init_hasP, 1, 0, 0);
  tmscm_install_procedure ("get-page-count",  tmg_get_page_count, 0, 0, 0);
  tmscm_install_procedure ("get-page-width",  tmg_get_page_width, 1, 0, 0);
  tmscm_install_procedure ("get-pages-width",  tmg_get_pages_width, 1, 0, 0);
  tmscm_install_procedure ("get-page-height",  tmg_get_page_height, 1, 0, 0);
  tmscm_install_procedure ("get-total-width",  tmg_get_total_width, 1, 0, 0);
  tmscm_install_procedure ("get-total-height",  tmg_get_total_height, 1, 0, 0);
  tmscm_install_procedure ("get-reference",  tmg_get_reference, 1, 0, 0);
  tmscm_install_procedure ("set-reference",  tmg_set_reference, 2, 0, 0);
  tmscm_install_procedure ("reset-reference",  tmg_reset_reference, 1, 0, 0);
  tmscm_install_procedure ("find-references",  tmg_find_references, 1, 0, 0);
  tmscm_install_procedure ("list-references",  tmg_list_references, 0, 0, 0);
  tmscm_install_procedure ("list-references*",  tmg_list_references_dot, 1, 0, 0);
  tmscm_install_procedure ("get-auxiliary",  tmg_get_auxiliary, 1, 0, 0);
  tmscm_install_procedure ("set-auxiliary",  tmg_set_auxiliary, 2, 0, 0);
  tmscm_install_procedure ("reset-auxiliary",  tmg_reset_auxiliary, 1, 0, 0);
  tmscm_install_procedure ("list-auxiliaries",  tmg_list_auxiliaries, 0, 0, 0);
  tmscm_install_procedure ("list-auxiliaries*",  tmg_list_auxiliaries_dot, 1, 0, 0);
  tmscm_install_procedure ("get-attachment",  tmg_get_attachment, 1, 0, 0);
  tmscm_install_procedure ("set-attachment",  tmg_set_attachment, 2, 0, 0);
  tmscm_install_procedure ("reset-attachment",  tmg_reset_attachment, 1, 0, 0);
  tmscm_install_procedure ("list-attachments",  tmg_list_attachments, 0, 0, 0);
  tmscm_install_procedure ("list-attachments*",  tmg_list_attachments_dot, 1, 0, 0);
  tmscm_install_procedure ("make-htab",  tmg_make_htab, 1, 0, 0);
  tmscm_install_procedure ("make-space",  tmg_make_space, 1, 0, 0);
  tmscm_install_procedure ("make-var-space",  tmg_make_var_space, 3, 0, 0);
  tmscm_install_procedure ("make-hspace",  tmg_make_hspace, 1, 0, 0);
  tmscm_install_procedure ("make-var-hspace",  tmg_make_var_hspace, 3, 0, 0);
  tmscm_install_procedure ("make-vspace-before",  tmg_make_vspace_before, 1, 0, 0);
  tmscm_install_procedure ("make-var-vspace-before",  tmg_make_var_vspace_before, 3, 0, 0);
  tmscm_install_procedure ("make-vspace-after",  tmg_make_vspace_after, 1, 0, 0);
  tmscm_install_procedure ("make-var-vspace-after",  tmg_make_var_vspace_after, 3, 0, 0);
  tmscm_install_procedure ("make-image",  tmg_make_image, 6, 0, 0);
  tmscm_install_procedure ("length-decode",  tmg_length_decode, 1, 0, 0);
  tmscm_install_procedure ("length-add",  tmg_length_add, 2, 0, 0);
  tmscm_install_procedure ("length-sub",  tmg_length_sub, 2, 0, 0);
  tmscm_install_procedure ("length-max",  tmg_length_max, 2, 0, 0);
  tmscm_install_procedure ("length-min",  tmg_length_min, 2, 0, 0);
  tmscm_install_procedure ("length-mult",  tmg_length_mult, 2, 0, 0);
  tmscm_install_procedure ("length?",  tmg_lengthP, 1, 0, 0);
  tmscm_install_procedure ("length-divide",  tmg_length_divide, 2, 0, 0);
  tmscm_install_procedure ("cpp-make-rigid",  tmg_cpp_make_rigid, 0, 0, 0);
  tmscm_install_procedure ("cpp-make-lprime",  tmg_cpp_make_lprime, 1, 0, 0);
  tmscm_install_procedure ("cpp-make-rprime",  tmg_cpp_make_rprime, 1, 0, 0);
  tmscm_install_procedure ("cpp-make-below",  tmg_cpp_make_below, 0, 0, 0);
  tmscm_install_procedure ("cpp-make-above",  tmg_cpp_make_above, 0, 0, 0);
  tmscm_install_procedure ("cpp-make-script",  tmg_cpp_make_script, 2, 0, 0);
  tmscm_install_procedure ("cpp-make-fraction",  tmg_cpp_make_fraction, 0, 0, 0);
  tmscm_install_procedure ("cpp-make-sqrt",  tmg_cpp_make_sqrt, 0, 0, 0);
  tmscm_install_procedure ("cpp-make-wide",  tmg_cpp_make_wide, 1, 0, 0);
  tmscm_install_procedure ("cpp-make-wide-under",  tmg_cpp_make_wide_under, 1, 0, 0);
  tmscm_install_procedure ("cpp-make-var-sqrt",  tmg_cpp_make_var_sqrt, 0, 0, 0);
  tmscm_install_procedure ("cpp-make-neg",  tmg_cpp_make_neg, 0, 0, 0);
  tmscm_install_procedure ("cpp-make-tree",  tmg_cpp_make_tree, 0, 0, 0);
  tmscm_install_procedure ("make-subtable",  tmg_make_subtable, 0, 0, 0);
  tmscm_install_procedure ("table-deactivate",  tmg_table_deactivate, 0, 0, 0);
  tmscm_install_procedure ("table-extract-format",  tmg_table_extract_format, 0, 0, 0);
  tmscm_install_procedure ("table-insert-row",  tmg_table_insert_row, 1, 0, 0);
  tmscm_install_procedure ("table-insert-column",  tmg_table_insert_column, 1, 0, 0);
  tmscm_install_procedure ("table-remove-row",  tmg_table_remove_row, 1, 0, 0);
  tmscm_install_procedure ("table-remove-column",  tmg_table_remove_column, 1, 0, 0);
  tmscm_install_procedure ("table-nr-rows",  tmg_table_nr_rows, 0, 0, 0);
  tmscm_install_procedure ("table-nr-columns",  tmg_table_nr_columns, 0, 0, 0);
  tmscm_install_procedure ("table-get-extents",  tmg_table_get_extents, 0, 0, 0);
  tmscm_install_procedure ("table-set-extents",  tmg_table_set_extents, 2, 0, 0);
  tmscm_install_procedure ("table-which-row",  tmg_table_which_row, 0, 0, 0);
  tmscm_install_procedure ("table-which-column",  tmg_table_which_column, 0, 0, 0);
  tmscm_install_procedure ("table-which-cells",  tmg_table_which_cells, 0, 0, 0);
  tmscm_install_procedure ("table-cell-path",  tmg_table_cell_path, 2, 0, 0);
  tmscm_install_procedure ("table-go-to",  tmg_table_go_to, 2, 0, 0);
  tmscm_install_procedure ("table-set-format",  tmg_table_set_format, 2, 0, 0);
  tmscm_install_procedure ("table-get-format-all",  tmg_table_get_format_all, 0, 0, 0);
  tmscm_install_procedure ("table-get-format",  tmg_table_get_format, 1, 0, 0);
  tmscm_install_procedure ("table-del-format",  tmg_table_del_format, 1, 0, 0);
  tmscm_install_procedure ("table-row-decoration",  tmg_table_row_decoration, 1, 0, 0);
  tmscm_install_procedure ("table-column-decoration",  tmg_table_column_decoration, 1, 0, 0);
  tmscm_install_procedure ("table-format-center",  tmg_table_format_center, 0, 0, 0);
  tmscm_install_procedure ("table-correct-block-content",  tmg_table_correct_block_content, 0, 0, 0);
  tmscm_install_procedure ("set-cell-mode",  tmg_set_cell_mode, 1, 0, 0);
  tmscm_install_procedure ("get-cell-mode",  tmg_get_cell_mode, 0, 0, 0);
  tmscm_install_procedure ("cell-set-format",  tmg_cell_set_format, 2, 0, 0);
  tmscm_install_procedure ("cell-get-format",  tmg_cell_get_format, 1, 0, 0);
  tmscm_install_procedure ("cell-del-format",  tmg_cell_del_format, 1, 0, 0);
  tmscm_install_procedure ("table-test",  tmg_table_test, 0, 0, 0);
  tmscm_install_procedure ("key-press",  tmg_key_press, 1, 0, 0);
  tmscm_install_procedure ("raw-emulate-keyboard",  tmg_raw_emulate_keyboard, 1, 0, 0);
  tmscm_install_procedure ("complete-try?",  tmg_complete_tryP, 0, 0, 0);
  tmscm_install_procedure ("get-input-mode",  tmg_get_input_mode, 0, 0, 0);
  tmscm_install_procedure ("key-press-search",  tmg_key_press_search, 1, 0, 0);
  tmscm_install_procedure ("key-press-replace",  tmg_key_press_replace, 1, 0, 0);
  tmscm_install_procedure ("key-press-spell",  tmg_key_press_spell, 1, 0, 0);
  tmscm_install_procedure ("key-press-complete",  tmg_key_press_complete, 1, 0, 0);
  tmscm_install_procedure ("mouse-any",  tmg_mouse_any, 5, 0, 0);
  tmscm_install_procedure ("get-mouse-position",  tmg_get_mouse_position, 0, 0, 0);
  tmscm_install_procedure ("set-mouse-pointer",  tmg_set_mouse_pointer, 2, 0, 0);
  tmscm_install_procedure ("set-predef-mouse-pointer",  tmg_set_predef_mouse_pointer, 1, 0, 0);
  tmscm_install_procedure ("go-to-path",  tmg_go_to_path, 1, 0, 0);
  tmscm_install_procedure ("go-left",  tmg_go_left, 0, 0, 0);
  tmscm_install_procedure ("go-right",  tmg_go_right, 0, 0, 0);
  tmscm_install_procedure ("go-up",  tmg_go_up, 0, 0, 0);
  tmscm_install_procedure ("go-down",  tmg_go_down, 0, 0, 0);
  tmscm_install_procedure ("go-start",  tmg_go_start, 0, 0, 0);
  tmscm_install_procedure ("go-end",  tmg_go_end, 0, 0, 0);
  tmscm_install_procedure ("go-start-of",  tmg_go_start_of, 1, 0, 0);
  tmscm_install_procedure ("go-end-of",  tmg_go_end_of, 1, 0, 0);
  tmscm_install_procedure ("go-start-with",  tmg_go_start_with, 2, 0, 0);
  tmscm_install_procedure ("go-end-with",  tmg_go_end_with, 2, 0, 0);
  tmscm_install_procedure ("go-start-line",  tmg_go_start_line, 0, 0, 0);
  tmscm_install_procedure ("go-end-line",  tmg_go_end_line, 0, 0, 0);
  tmscm_install_procedure ("go-page-up",  tmg_go_page_up, 0, 0, 0);
  tmscm_install_procedure ("go-page-down",  tmg_go_page_down, 0, 0, 0);
  tmscm_install_procedure ("go-start-paragraph",  tmg_go_start_paragraph, 0, 0, 0);
  tmscm_install_procedure ("go-end-paragraph",  tmg_go_end_paragraph, 0, 0, 0);
  tmscm_install_procedure ("label->path",  tmg_label_2path, 1, 0, 0);
  tmscm_install_procedure ("go-to-label",  tmg_go_to_label, 1, 0, 0);
  tmscm_install_procedure ("cursor-accessible?",  tmg_cursor_accessibleP, 0, 0, 0);
  tmscm_install_procedure ("cursor-show-if-hidden",  tmg_cursor_show_if_hidden, 0, 0, 0);
  tmscm_install_procedure ("select-all",  tmg_select_all, 0, 0, 0);
  tmscm_install_procedure ("select-line",  tmg_select_line, 0, 0, 0);
  tmscm_install_procedure ("select-from-cursor",  tmg_select_from_cursor, 0, 0, 0);
  tmscm_install_procedure ("select-from-cursor-if-active",  tmg_select_from_cursor_if_active, 0, 0, 0);
  tmscm_install_procedure ("select-from-keyboard",  tmg_select_from_keyboard, 1, 0, 0);
  tmscm_install_procedure ("select-from-shift-keyboard",  tmg_select_from_shift_keyboard, 0, 0, 0);
  tmscm_install_procedure ("select-enlarge",  tmg_select_enlarge, 0, 0, 0);
  tmscm_install_procedure ("select-enlarge-environmental",  tmg_select_enlarge_environmental, 0, 0, 0);
  tmscm_install_procedure ("selection-active-any?",  tmg_selection_active_anyP, 0, 0, 0);
  tmscm_install_procedure ("selection-active-normal?",  tmg_selection_active_normalP, 0, 0, 0);
  tmscm_install_procedure ("selection-active-table?",  tmg_selection_active_tableP, 0, 0, 0);
  tmscm_install_procedure ("selection-active-small?",  tmg_selection_active_smallP, 0, 0, 0);
  tmscm_install_procedure ("selection-active-enlarging?",  tmg_selection_active_enlargingP, 0, 0, 0);
  tmscm_install_procedure ("selection-set-start",  tmg_selection_set_start, 0, 0, 0);
  tmscm_install_procedure ("selection-set-end",  tmg_selection_set_end, 0, 0, 0);
  tmscm_install_procedure ("selection-get-start",  tmg_selection_get_start, 0, 0, 0);
  tmscm_install_procedure ("selection-get-end",  tmg_selection_get_end, 0, 0, 0);
  tmscm_install_procedure ("selection-get-start*",  tmg_selection_get_start_dot, 0, 0, 0);
  tmscm_install_procedure ("selection-get-end*",  tmg_selection_get_end_dot, 0, 0, 0);
  tmscm_install_procedure ("selection-path",  tmg_selection_path, 0, 0, 0);
  tmscm_install_procedure ("selection-set",  tmg_selection_set, 2, 0, 0);
  tmscm_install_procedure ("selection-set-range-set",  tmg_selection_set_range_set, 1, 0, 0);
  tmscm_install_procedure ("clipboard-set",  tmg_clipboard_set, 2, 0, 0);
  tmscm_install_procedure ("clipboard-get",  tmg_clipboard_get, 1, 0, 0);
  tmscm_install_procedure ("cpp-clipboard-copy",  tmg_cpp_clipboard_copy, 1, 0, 0);
  tmscm_install_procedure ("cpp-clipboard-cut",  tmg_cpp_clipboard_cut, 1, 0, 0);
  tmscm_install_procedure ("clipboard-cut-at",  tmg_clipboard_cut_at, 1, 0, 0);
  tmscm_install_procedure ("clipboard-cut-between",  tmg_clipboard_cut_between, 2, 0, 0);
  tmscm_install_procedure ("cpp-clipboard-paste",  tmg_cpp_clipboard_paste, 1, 0, 0);
  tmscm_install_procedure ("selection-move",  tmg_selection_move, 0, 0, 0);
  tmscm_install_procedure ("clipboard-clear",  tmg_clipboard_clear, 1, 0, 0);
  tmscm_install_procedure ("selection-cancel",  tmg_selection_cancel, 0, 0, 0);
  tmscm_install_procedure ("clipboard-set-import",  tmg_clipboard_set_import, 1, 0, 0);
  tmscm_install_procedure ("clipboard-set-export",  tmg_clipboard_set_export, 1, 0, 0);
  tmscm_install_procedure ("clipboard-get-import",  tmg_clipboard_get_import, 0, 0, 0);
  tmscm_install_procedure ("clipboard-get-export",  tmg_clipboard_get_export, 0, 0, 0);
  tmscm_install_procedure ("set-manual-focus-path",  tmg_set_manual_focus_path, 1, 0, 0);
  tmscm_install_procedure ("get-manual-focus-path",  tmg_get_manual_focus_path, 0, 0, 0);
  tmscm_install_procedure ("get-focus-path",  tmg_get_focus_path, 0, 0, 0);
  tmscm_install_procedure ("set-alt-selection",  tmg_set_alt_selection, 2, 0, 0);
  tmscm_install_procedure ("get-alt-selection",  tmg_get_alt_selection, 1, 0, 0);
  tmscm_install_procedure ("cancel-alt-selection",  tmg_cancel_alt_selection, 1, 0, 0);
  tmscm_install_procedure ("cancel-alt-selections",  tmg_cancel_alt_selections, 0, 0, 0);
  tmscm_install_procedure ("clear-undo-history",  tmg_clear_undo_history, 0, 0, 0);
  tmscm_install_procedure ("commit-changes",  tmg_commit_changes, 0, 0, 0);
  tmscm_install_procedure ("start-slave",  tmg_start_slave, 1, 0, 0);
  tmscm_install_procedure ("mark-start",  tmg_mark_start, 1, 0, 0);
  tmscm_install_procedure ("mark-end",  tmg_mark_end, 1, 0, 0);
  tmscm_install_procedure ("mark-cancel",  tmg_mark_cancel, 1, 0, 0);
  tmscm_install_procedure ("remove-undo-mark",  tmg_remove_undo_mark, 0, 0, 0);
  tmscm_install_procedure ("add-undo-mark",  tmg_add_undo_mark, 0, 0, 0);
  tmscm_install_procedure ("unredoable-undo",  tmg_unredoable_undo, 0, 0, 0);
  tmscm_install_procedure ("undo-possibilities",  tmg_undo_possibilities, 0, 0, 0);
  tmscm_install_procedure ("undo",  tmg_undo, 1, 0, 0);
  tmscm_install_procedure ("redo-possibilities",  tmg_redo_possibilities, 0, 0, 0);
  tmscm_install_procedure ("redo",  tmg_redo, 1, 0, 0);
  tmscm_install_procedure ("show-history",  tmg_show_history, 0, 0, 0);
  tmscm_install_procedure ("archive-state",  tmg_archive_state, 0, 0, 0);
  tmscm_install_procedure ("start-editing",  tmg_start_editing, 0, 0, 0);
  tmscm_install_procedure ("end-editing",  tmg_end_editing, 0, 0, 0);
  tmscm_install_procedure ("cancel-editing",  tmg_cancel_editing, 0, 0, 0);
  tmscm_install_procedure ("in-graphics?",  tmg_in_graphicsP, 0, 0, 0);
  tmscm_install_procedure ("get-graphical-x",  tmg_get_graphical_x, 0, 0, 0);
  tmscm_install_procedure ("get-graphical-y",  tmg_get_graphical_y, 0, 0, 0);
  tmscm_install_procedure ("get-graphical-object",  tmg_get_graphical_object, 0, 0, 0);
  tmscm_install_procedure ("set-graphical-object",  tmg_set_graphical_object, 1, 0, 0);
  tmscm_install_procedure ("invalidate-graphical-object",  tmg_invalidate_graphical_object, 0, 0, 0);
  tmscm_install_procedure ("graphical-select",  tmg_graphical_select, 2, 0, 0);
  tmscm_install_procedure ("graphical-select-area",  tmg_graphical_select_area, 4, 0, 0);
  tmscm_install_procedure ("in-normal-mode?",  tmg_in_normal_modeP, 0, 0, 0);
  tmscm_install_procedure ("in-search-mode?",  tmg_in_search_modeP, 0, 0, 0);
  tmscm_install_procedure ("in-replace-mode?",  tmg_in_replace_modeP, 0, 0, 0);
  tmscm_install_procedure ("in-spell-mode?",  tmg_in_spell_modeP, 0, 0, 0);
  tmscm_install_procedure ("search-start",  tmg_search_start, 1, 0, 0);
  tmscm_install_procedure ("search-button-next",  tmg_search_button_next, 0, 0, 0);
  tmscm_install_procedure ("replace-start",  tmg_replace_start, 3, 0, 0);
  tmscm_install_procedure ("spell-start",  tmg_spell_start, 0, 0, 0);
  tmscm_install_procedure ("spell-replace",  tmg_spell_replace, 1, 0, 0);
  tmscm_install_procedure ("session-complete-command",  tmg_session_complete_command, 1, 0, 0);
  tmscm_install_procedure ("custom-complete",  tmg_custom_complete, 1, 0, 0);
  tmscm_install_procedure ("keyboard-focus-on",  tmg_keyboard_focus_on, 1, 0, 0);
  tmscm_install_procedure ("view-set-property",  tmg_view_set_property, 2, 0, 0);
  tmscm_install_procedure ("view-get-property",  tmg_view_get_property, 1, 0, 0);
  tmscm_install_procedure ("get-window-width",  tmg_get_window_width, 0, 0, 0);
  tmscm_install_procedure ("get-window-height",  tmg_get_window_height, 0, 0, 0);
  tmscm_install_procedure ("get-window-x",  tmg_get_window_x, 0, 0, 0);
  tmscm_install_procedure ("get-window-y",  tmg_get_window_y, 0, 0, 0);
  tmscm_install_procedure ("get-canvas-x",  tmg_get_canvas_x, 0, 0, 0);
  tmscm_install_procedure ("get-canvas-y",  tmg_get_canvas_y, 0, 0, 0);
  tmscm_install_procedure ("get-scroll-x",  tmg_get_scroll_x, 0, 0, 0);
  tmscm_install_procedure ("get-scroll-y",  tmg_get_scroll_y, 0, 0, 0);
  tmscm_install_procedure ("clear-buffer",  tmg_clear_buffer, 0, 0, 0);
  tmscm_install_procedure ("tex-buffer",  tmg_tex_buffer, 0, 0, 0);
  tmscm_install_procedure ("clear-local-info",  tmg_clear_local_info, 0, 0, 0);
  tmscm_install_procedure ("refresh-window",  tmg_refresh_window, 0, 0, 0);
  tmscm_install_procedure ("update-forced",  tmg_update_forced, 0, 0, 0);
  tmscm_install_procedure ("update-path",  tmg_update_path, 1, 0, 0);
  tmscm_install_procedure ("update-current-buffer",  tmg_update_current_buffer, 0, 0, 0);
  tmscm_install_procedure ("update-players",  tmg_update_players, 2, 0, 0);
  tmscm_install_procedure ("generate-all-aux",  tmg_generate_all_aux, 0, 0, 0);
  tmscm_install_procedure ("generate-aux",  tmg_generate_aux, 1, 0, 0);
  tmscm_install_procedure ("notify-page-change",  tmg_notify_page_change, 0, 0, 0);
  tmscm_install_procedure ("notify-change",  tmg_notify_change, 1, 0, 0);
  tmscm_install_procedure ("get-metadata",  tmg_get_metadata, 1, 0, 0);
  tmscm_install_procedure ("cpp-nr-pages",  tmg_cpp_nr_pages, 0, 0, 0);
  tmscm_install_procedure ("print-to-file",  tmg_print_to_file, 1, 0, 0);
  tmscm_install_procedure ("print-pages-to-file",  tmg_print_pages_to_file, 3, 0, 0);
  tmscm_install_procedure ("print",  tmg_print, 0, 0, 0);
  tmscm_install_procedure ("print-pages",  tmg_print_pages, 2, 0, 0);
  tmscm_install_procedure ("print-snippet",  tmg_print_snippet, 3, 0, 0);
  tmscm_install_procedure ("graphics-file-to-clipboard",  tmg_graphics_file_to_clipboard, 1, 0, 0);
  tmscm_install_procedure ("export-postscript",  tmg_export_postscript, 1, 0, 0);
  tmscm_install_procedure ("export-pages-postscript",  tmg_export_pages_postscript, 3, 0, 0);
  tmscm_install_procedure ("footer-eval",  tmg_footer_eval, 1, 0, 0);
  tmscm_install_procedure ("texmacs-exec",  tmg_texmacs_exec, 1, 0, 0);
  tmscm_install_procedure ("texmacs-exec*",  tmg_texmacs_exec_dot, 1, 0, 0);
  tmscm_install_procedure ("texmacs-expand",  tmg_texmacs_expand, 1, 0, 0);
  tmscm_install_procedure ("verbatim-expand",  tmg_verbatim_expand, 1, 0, 0);
  tmscm_install_procedure ("latex-expand",  tmg_latex_expand, 1, 0, 0);
  tmscm_install_procedure ("html-expand",  tmg_html_expand, 1, 0, 0);
  tmscm_install_procedure ("animate-checkout",  tmg_animate_checkout, 1, 0, 0);
  tmscm_install_procedure ("animate-commit",  tmg_animate_commit, 1, 0, 0);
  tmscm_install_procedure ("idle-time",  tmg_idle_time, 0, 0, 0);
  tmscm_install_procedure ("change-time",  tmg_change_time, 0, 0, 0);
  tmscm_install_procedure ("menu-before-action",  tmg_menu_before_action, 0, 0, 0);
  tmscm_install_procedure ("menu-after-action",  tmg_menu_after_action, 0, 0, 0);
  tmscm_install_procedure ("update-menus",  tmg_update_menus, 0, 0, 0);
  tmscm_install_procedure ("show-tree",  tmg_show_tree, 0, 0, 0);
  tmscm_install_procedure ("show-env",  tmg_show_env, 0, 0, 0);
  tmscm_install_procedure ("show-path",  tmg_show_path, 0, 0, 0);
  tmscm_install_procedure ("show-cursor",  tmg_show_cursor, 0, 0, 0);
  tmscm_install_procedure ("show-selection",  tmg_show_selection, 0, 0, 0);
  tmscm_install_procedure ("show-meminfo",  tmg_show_meminfo, 0, 0, 0);
  tmscm_install_procedure ("edit-special",  tmg_edit_special, 0, 0, 0);
  tmscm_install_procedure ("edit-test",  tmg_edit_test, 0, 0, 0);
}
