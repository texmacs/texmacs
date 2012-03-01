
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

SCM
tmg_root_tree () {
  // SCM_DEFER_INTS;
  tree out= get_server()->get_editor()->the_root ();
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_buffer_path () {
  // SCM_DEFER_INTS;
  path out= get_server()->get_editor()->the_buffer_path ();
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_buffer_tree () {
  // SCM_DEFER_INTS;
  tree out= get_server()->get_editor()->the_buffer ();
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_paragraph_tree () {
  // SCM_DEFER_INTS;
  tree out= get_server()->get_editor()->the_line ();
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_cursor_path () {
  // SCM_DEFER_INTS;
  path out= get_server()->get_editor()->the_path ();
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_cursor_path_dot () {
  // SCM_DEFER_INTS;
  path out= get_server()->get_editor()->the_shifted_path ();
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_selection_tree () {
  // SCM_DEFER_INTS;
  tree out= get_server()->get_editor()->selection_get ();
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_path_2tree (SCM arg1) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "path->tree");

  path in1= scm_to_path (arg1);

  // SCM_DEFER_INTS;
  tree out= get_server()->get_editor()->the_subtree (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_path_correct_old (SCM arg1) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "path-correct-old");

  path in1= scm_to_path (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->correct (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_path_insert_with (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "path-insert-with");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "path-insert-with");
  SCM_ASSERT_CONTENT (arg3, SCM_ARG3, "path-insert-with");

  path in1= scm_to_path (arg1);
  string in2= scm_to_string (arg2);
  content in3= scm_to_content (arg3);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->insert_with (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_path_remove_with (SCM arg1, SCM arg2) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "path-remove-with");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "path-remove-with");

  path in1= scm_to_path (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->remove_with (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_position_new_path (SCM arg1) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "position-new-path");

  path in1= scm_to_path (arg1);

  // SCM_DEFER_INTS;
  observer out= get_server()->get_editor()->position_new (in1);
  // SCM_ALLOW_INTS;

  return observer_to_scm (out);
}

SCM
tmg_position_delete (SCM arg1) {
  SCM_ASSERT_OBSERVER (arg1, SCM_ARG1, "position-delete");

  observer in1= scm_to_observer (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->position_delete (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_position_set (SCM arg1, SCM arg2) {
  SCM_ASSERT_OBSERVER (arg1, SCM_ARG1, "position-set");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "position-set");

  observer in1= scm_to_observer (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->position_set (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_position_get (SCM arg1) {
  SCM_ASSERT_OBSERVER (arg1, SCM_ARG1, "position-get");

  observer in1= scm_to_observer (arg1);

  // SCM_DEFER_INTS;
  path out= get_server()->get_editor()->position_get (in1);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_insideP (SCM arg1) {
  SCM_ASSERT_TREE_LABEL (arg1, SCM_ARG1, "inside?");

  tree_label in1= scm_to_tree_label (arg1);

  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->inside (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_insert (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "insert");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->insert_tree (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_insert_go_to (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "insert-go-to");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "insert-go-to");

  content in1= scm_to_content (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->var_insert_tree (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_insert_raw_go_to (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "insert-raw-go-to");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "insert-raw-go-to");

  content in1= scm_to_content (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->insert_tree (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_insert_raw_return () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->insert_return ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_remove_text (SCM arg1) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "remove-text");

  bool in1= scm_to_bool (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->remove_text (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_remove_structure (SCM arg1) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "remove-structure");

  bool in1= scm_to_bool (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->remove_structure (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_remove_structure_upwards () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->remove_structure_upwards ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make (SCM arg1) {
  SCM_ASSERT_TREE_LABEL (arg1, SCM_ARG1, "make");

  tree_label in1= scm_to_tree_label (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_compound (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_arity (SCM arg1, SCM arg2) {
  SCM_ASSERT_TREE_LABEL (arg1, SCM_ARG1, "make-arity");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "make-arity");

  tree_label in1= scm_to_tree_label (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_compound (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_activate () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->activate ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_insert_argument (SCM arg1) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "insert-argument");

  bool in1= scm_to_bool (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->insert_argument (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_remove_argument (SCM arg1) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "remove-argument");

  bool in1= scm_to_bool (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->remove_argument (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_insert_argument_at (SCM arg1, SCM arg2) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "insert-argument-at");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "insert-argument-at");

  path in1= scm_to_path (arg1);
  bool in2= scm_to_bool (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->insert_argument (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_remove_argument_at (SCM arg1, SCM arg2) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "remove-argument-at");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "remove-argument-at");

  path in1= scm_to_path (arg1);
  bool in2= scm_to_bool (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->remove_argument (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_with (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "make-with");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "make-with");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_with (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_mod_active (SCM arg1) {
  SCM_ASSERT_TREE_LABEL (arg1, SCM_ARG1, "make-mod-active");

  tree_label in1= scm_to_tree_label (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_mod_active (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_style_with (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "make-style-with");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "make-style-with");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_style_with (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_hybrid () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_hybrid ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_activate_latex () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->activate_latex ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_activate_hybrid (SCM arg1) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "activate-hybrid");

  bool in1= scm_to_bool (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->activate_hybrid (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_activate_symbol () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->activate_symbol ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_return_before () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_return_before ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_return_after () {
  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->make_return_after ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_temp_proof_fix () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->temp_proof_fix ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_init_default_one (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "init-default-one");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->init_default (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_init_env (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "init-env");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "init-env");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->init_env (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_init_env_tree (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "init-env-tree");
  SCM_ASSERT_CONTENT (arg2, SCM_ARG2, "init-env-tree");

  string in1= scm_to_string (arg1);
  content in2= scm_to_content (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->init_env (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_init_style (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "init-style");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->init_style (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_init_add_package (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "init-add-package");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->init_add_package (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_init_remove_package (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "init-remove-package");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->init_remove_package (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_get_style_tree () {
  // SCM_DEFER_INTS;
  tree out= get_server()->get_editor()->get_style ();
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_set_style_tree (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "set-style-tree");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->set_style (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_get_env (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "get-env");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= get_server()->get_editor()->get_env_string (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_get_env_tree (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "get-env-tree");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tree out= get_server()->get_editor()->get_env_value (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_get_env_tree_at (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "get-env-tree-at");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "get-env-tree-at");

  string in1= scm_to_string (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  tree out= get_server()->get_editor()->get_env_value (in1, in2);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_get_init (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "get-init");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= get_server()->get_editor()->get_init_string (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_get_init_tree (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "get-init-tree");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tree out= get_server()->get_editor()->get_init_value (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_context_hasP (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "context-has?");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->defined_at_cursor (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_style_hasP (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "style-has?");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->defined_at_init (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_init_hasP (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "init-has?");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->defined_in_init (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_make_htab (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "make-htab");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_htab (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_space (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "make-space");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_space (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_var_space (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "make-var-space");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "make-var-space");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "make-var-space");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_space (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_hspace (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "make-hspace");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_hspace (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_var_hspace (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "make-var-hspace");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "make-var-hspace");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "make-var-hspace");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_hspace (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_vspace_before (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "make-vspace-before");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_vspace_before (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_var_vspace_before (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "make-var-vspace-before");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "make-var-vspace-before");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "make-var-vspace-before");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_vspace_before (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_vspace_after (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "make-vspace-after");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_vspace_after (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_var_vspace_after (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "make-var-vspace-after");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "make-var-vspace-after");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "make-var-vspace-after");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_vspace_after (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_image (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "make-image");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "make-image");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "make-image");
  SCM_ASSERT_STRING (arg4, SCM_ARG4, "make-image");
  SCM_ASSERT_STRING (arg5, SCM_ARG5, "make-image");
  SCM_ASSERT_STRING (arg6, SCM_ARG6, "make-image");

  string in1= scm_to_string (arg1);
  bool in2= scm_to_bool (arg2);
  string in3= scm_to_string (arg3);
  string in4= scm_to_string (arg4);
  string in5= scm_to_string (arg5);
  string in6= scm_to_string (arg6);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_image (in1, in2, in3, in4, in5, in6);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_length_decode (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "length-decode");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  int out= get_server()->get_editor()->as_length (in1);
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_length_add (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "length-add");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "length-add");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  string out= get_server()->get_editor()->add_lengths (in1, in2);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_length_mult (SCM arg1, SCM arg2) {
  SCM_ASSERT_DOUBLE (arg1, SCM_ARG1, "length-mult");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "length-mult");

  double in1= scm_to_double (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  string out= get_server()->get_editor()->multiply_length (in1, in2);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_lengthP (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "length?");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->is_length (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_length_divide (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "length-divide");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "length-divide");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  double out= get_server()->get_editor()->divide_lengths (in1, in2);
  // SCM_ALLOW_INTS;

  return double_to_scm (out);
}

SCM
tmg_make_rigid () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_rigid ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_lprime (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "make-lprime");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_lprime (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_rprime (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "make-rprime");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_rprime (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_below () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_below ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_above () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_above ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_script (SCM arg1, SCM arg2) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "make-script");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "make-script");

  bool in1= scm_to_bool (arg1);
  bool in2= scm_to_bool (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_script (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_fraction () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_fraction ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_sqrt () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_sqrt ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_wide (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "make-wide");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_wide (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_wide_under (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "make-wide-under");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_wide_under (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_var_sqrt () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_var_sqrt ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_neg () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_neg ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_tree () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_tree ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_subtable () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_subtable ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_table_disactivate () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->table_disactivate ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_table_extract_format () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->table_extract_format ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_table_insert_row (SCM arg1) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "table-insert-row");

  bool in1= scm_to_bool (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->table_insert_row (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_table_insert_column (SCM arg1) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "table-insert-column");

  bool in1= scm_to_bool (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->table_insert_column (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_table_remove_row (SCM arg1) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "table-remove-row");

  bool in1= scm_to_bool (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->table_remove_row (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_table_remove_column (SCM arg1) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "table-remove-column");

  bool in1= scm_to_bool (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->table_remove_column (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_table_nr_rows () {
  // SCM_DEFER_INTS;
  int out= get_server()->get_editor()->table_nr_rows ();
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_table_nr_columns () {
  // SCM_DEFER_INTS;
  int out= get_server()->get_editor()->table_nr_columns ();
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_table_set_extents (SCM arg1, SCM arg2) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "table-set-extents");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "table-set-extents");

  int in1= scm_to_int (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->table_set_extents (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_table_which_row () {
  // SCM_DEFER_INTS;
  int out= get_server()->get_editor()->table_which_row ();
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_table_which_column () {
  // SCM_DEFER_INTS;
  int out= get_server()->get_editor()->table_which_column ();
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_table_cell_path (SCM arg1, SCM arg2) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "table-cell-path");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "table-cell-path");

  int in1= scm_to_int (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  path out= get_server()->get_editor()->table_search_cell (in1, in2);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_table_go_to (SCM arg1, SCM arg2) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "table-go-to");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "table-go-to");

  int in1= scm_to_int (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->table_go_to (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_table_set_format (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "table-set-format");
  SCM_ASSERT_CONTENT (arg2, SCM_ARG2, "table-set-format");

  string in1= scm_to_string (arg1);
  content in2= scm_to_content (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->table_set_format (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_table_get_format (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "table-get-format");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= get_server()->get_editor()->table_get_format (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_table_del_format (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "table-del-format");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->table_del_format (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_table_row_decoration (SCM arg1) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "table-row-decoration");

  bool in1= scm_to_bool (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->table_row_decoration (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_table_column_decoration (SCM arg1) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "table-column-decoration");

  bool in1= scm_to_bool (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->table_column_decoration (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_table_format_center () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->table_format_center ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_table_correct_block_content () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->table_correct_block_content ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_set_cell_mode (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "set-cell-mode");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->set_cell_mode (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_get_cell_mode () {
  // SCM_DEFER_INTS;
  string out= get_server()->get_editor()->get_cell_mode ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_cell_set_format (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "cell-set-format");
  SCM_ASSERT_CONTENT (arg2, SCM_ARG2, "cell-set-format");

  string in1= scm_to_string (arg1);
  content in2= scm_to_content (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->cell_set_format (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_cell_get_format (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "cell-get-format");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= get_server()->get_editor()->cell_get_format (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_cell_del_format (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "cell-del-format");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->cell_del_format (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_table_test () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->table_test ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_key_press (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "key-press");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->key_press (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_raw_emulate_keyboard (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "raw-emulate-keyboard");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->emulate_keyboard (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_complete_tryP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->complete_try ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_get_input_mode () {
  // SCM_DEFER_INTS;
  int out= get_server()->get_editor()->get_input_mode ();
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_key_press_search (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "key-press-search");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->search_keypress (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_key_press_replace (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "key-press-replace");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->replace_keypress (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_key_press_spell (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "key-press-spell");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->spell_keypress (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_key_press_complete (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "key-press-complete");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->complete_keypress (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_mouse_any (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "mouse-any");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "mouse-any");
  SCM_ASSERT_INT (arg3, SCM_ARG3, "mouse-any");
  SCM_ASSERT_INT (arg4, SCM_ARG4, "mouse-any");
  SCM_ASSERT_DOUBLE (arg5, SCM_ARG5, "mouse-any");

  string in1= scm_to_string (arg1);
  int in2= scm_to_int (arg2);
  int in3= scm_to_int (arg3);
  int in4= scm_to_int (arg4);
  double in5= scm_to_double (arg5);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->mouse_any (in1, in2, in3, in4, in5);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_set_mouse_pointer (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "set-mouse-pointer");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "set-mouse-pointer");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->set_pointer (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_set_predef_mouse_pointer (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "set-predef-mouse-pointer");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->set_pointer (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_go_to_path (SCM arg1) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "go-to-path");

  path in1= scm_to_path (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->go_to (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_go_left () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->go_left ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_go_right () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->go_right ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_go_up () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->go_up ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_go_down () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->go_down ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_go_start () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->go_start ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_go_end () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->go_end ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_go_start_of (SCM arg1) {
  SCM_ASSERT_TREE_LABEL (arg1, SCM_ARG1, "go-start-of");

  tree_label in1= scm_to_tree_label (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->go_start_of (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_go_end_of (SCM arg1) {
  SCM_ASSERT_TREE_LABEL (arg1, SCM_ARG1, "go-end-of");

  tree_label in1= scm_to_tree_label (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->go_end_of (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_go_start_with (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "go-start-with");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "go-start-with");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->go_start_with (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_go_end_with (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "go-end-with");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "go-end-with");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->go_end_with (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_go_start_line () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->go_start_line ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_go_end_line () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->go_end_line ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_go_page_up () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->go_page_up ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_go_page_down () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->go_page_down ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_go_start_paragraph () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->go_start_paragraph ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_go_end_paragraph () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->go_end_paragraph ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_go_to_label (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "go-to-label");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->go_to_label (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_cursor_show_if_hidden () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->show_cursor_if_hidden ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_select_all () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->select_all ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_select_line () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->select_line ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_select_from_cursor () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->select_from_cursor ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_select_from_keyboard (SCM arg1) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "select-from-keyboard");

  bool in1= scm_to_bool (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->select_from_keyboard (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_select_from_shift_keyboard () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->select_from_shift_keyboard ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_select_enlarge () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->select_enlarge ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_select_enlarge_environmental () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->select_enlarge_environmental ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_selection_active_anyP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->selection_active_any ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_selection_active_normalP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->selection_active_normal ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_selection_active_tableP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->selection_active_table ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_selection_active_smallP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->selection_active_small ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_selection_active_enlargingP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->selection_active_enlarging ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_selection_set_start () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->selection_set_start ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_selection_set_end () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->selection_set_end ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_selection_get_start () {
  // SCM_DEFER_INTS;
  path out= get_server()->get_editor()->selection_get_start ();
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_selection_get_end () {
  // SCM_DEFER_INTS;
  path out= get_server()->get_editor()->selection_get_end ();
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_selection_path () {
  // SCM_DEFER_INTS;
  path out= get_server()->get_editor()->selection_get_path ();
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_selection_set (SCM arg1, SCM arg2) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "selection-set");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "selection-set");

  path in1= scm_to_path (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->selection_set_paths (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_clipboard_copy (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "clipboard-copy");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->selection_copy (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_clipboard_cut (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "clipboard-cut");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->selection_cut (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_clipboard_cut_at (SCM arg1) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "clipboard-cut-at");

  path in1= scm_to_path (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->cut (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_clipboard_paste (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "clipboard-paste");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->selection_paste (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_selection_move () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->selection_move ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_clipboard_clear (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "clipboard-clear");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->selection_clear (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_selection_cancel () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->selection_cancel ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_clipboard_set_import (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "clipboard-set-import");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->selection_set_import (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_clipboard_set_export (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "clipboard-set-export");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->selection_set_export (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_clipboard_get_import () {
  // SCM_DEFER_INTS;
  string out= get_server()->get_editor()->selection_get_import ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_clipboard_get_export () {
  // SCM_DEFER_INTS;
  string out= get_server()->get_editor()->selection_get_export ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_set_manual_focus_path (SCM arg1) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "set-manual-focus-path");

  path in1= scm_to_path (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->manual_focus_set (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_get_manual_focus_path () {
  // SCM_DEFER_INTS;
  path out= get_server()->get_editor()->manual_focus_get ();
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_get_focus_path () {
  // SCM_DEFER_INTS;
  path out= get_server()->get_editor()->focus_get ();
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_clear_undo_history () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->clear_undo_history ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_commit_changes () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->end_editing ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_start_slave (SCM arg1) {
  SCM_ASSERT_DOUBLE (arg1, SCM_ARG1, "start-slave");

  double in1= scm_to_double (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->start_slave (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_mark_start (SCM arg1) {
  SCM_ASSERT_DOUBLE (arg1, SCM_ARG1, "mark-start");

  double in1= scm_to_double (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->mark_start (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_mark_end (SCM arg1) {
  SCM_ASSERT_DOUBLE (arg1, SCM_ARG1, "mark-end");

  double in1= scm_to_double (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->mark_end (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_mark_cancel (SCM arg1) {
  SCM_ASSERT_DOUBLE (arg1, SCM_ARG1, "mark-cancel");

  double in1= scm_to_double (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->mark_cancel (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_remove_undo_mark () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->remove_undo_mark ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_add_undo_mark () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->add_undo_mark ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_unredoable_undo () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->unredoable_undo ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_undo_possibilities () {
  // SCM_DEFER_INTS;
  int out= get_server()->get_editor()->undo_possibilities ();
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_undo (SCM arg1) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "undo");

  int in1= scm_to_int (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->undo (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_redo_possibilities () {
  // SCM_DEFER_INTS;
  int out= get_server()->get_editor()->redo_possibilities ();
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_redo (SCM arg1) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "redo");

  int in1= scm_to_int (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->redo (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_show_history () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->show_history ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_in_graphicsP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->inside_graphics ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_get_graphical_x () {
  // SCM_DEFER_INTS;
  double out= get_server()->get_editor()->get_x ();
  // SCM_ALLOW_INTS;

  return double_to_scm (out);
}

SCM
tmg_get_graphical_y () {
  // SCM_DEFER_INTS;
  double out= get_server()->get_editor()->get_y ();
  // SCM_ALLOW_INTS;

  return double_to_scm (out);
}

SCM
tmg_get_graphical_object () {
  // SCM_DEFER_INTS;
  tree out= get_server()->get_editor()->get_graphical_object ();
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_set_graphical_object (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "set-graphical-object");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->set_graphical_object (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_invalidate_graphical_object () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->invalidate_graphical_object ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_graphical_select (SCM arg1, SCM arg2) {
  SCM_ASSERT_DOUBLE (arg1, SCM_ARG1, "graphical-select");
  SCM_ASSERT_DOUBLE (arg2, SCM_ARG2, "graphical-select");

  double in1= scm_to_double (arg1);
  double in2= scm_to_double (arg2);

  // SCM_DEFER_INTS;
  tree out= get_server()->get_editor()->graphical_select (in1, in2);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_graphical_select_area (SCM arg1, SCM arg2, SCM arg3, SCM arg4) {
  SCM_ASSERT_DOUBLE (arg1, SCM_ARG1, "graphical-select-area");
  SCM_ASSERT_DOUBLE (arg2, SCM_ARG2, "graphical-select-area");
  SCM_ASSERT_DOUBLE (arg3, SCM_ARG3, "graphical-select-area");
  SCM_ASSERT_DOUBLE (arg4, SCM_ARG4, "graphical-select-area");

  double in1= scm_to_double (arg1);
  double in2= scm_to_double (arg2);
  double in3= scm_to_double (arg3);
  double in4= scm_to_double (arg4);

  // SCM_DEFER_INTS;
  tree out= get_server()->get_editor()->graphical_select (in1, in2, in3, in4);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_in_normal_modeP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->in_normal_mode ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_in_search_modeP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->in_search_mode ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_in_replace_modeP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->in_replace_mode ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_in_spell_modeP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->get_editor()->in_spell_mode ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_search_start (SCM arg1) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "search-start");

  bool in1= scm_to_bool (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->search_start (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_search_button_next () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->search_button_next ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_replace_start (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "replace-start");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "replace-start");
  SCM_ASSERT_BOOL (arg3, SCM_ARG3, "replace-start");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  bool in3= scm_to_bool (arg3);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->replace_start (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_spell_start () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->spell_start ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_spell_replace (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "spell-replace");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->spell_replace (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_session_complete_command (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "session-complete-command");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  string out= get_server()->get_editor()->session_complete_command (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_custom_complete (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "custom-complete");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->custom_complete (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_view_set_property (SCM arg1, SCM arg2) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "view-set-property");
  SCM_ASSERT_SCHEME_TREE (arg2, SCM_ARG2, "view-set-property");

  scheme_tree in1= scm_to_scheme_tree (arg1);
  scheme_tree in2= scm_to_scheme_tree (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->set_property (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_view_get_property (SCM arg1) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "view-get-property");

  scheme_tree in1= scm_to_scheme_tree (arg1);

  // SCM_DEFER_INTS;
  scheme_tree out= get_server()->get_editor()->get_property (in1);
  // SCM_ALLOW_INTS;

  return scheme_tree_to_scm (out);
}

SCM
tmg_clear_buffer () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->clear_buffer ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_tex_buffer () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->tex_buffer ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_clear_local_info () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->clear_local_info ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_update_path (SCM arg1) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "update-path");

  path in1= scm_to_path (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->typeset_invalidate (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_update_buffer () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->typeset_invalidate_all ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_generate_all_aux () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->generate_aux ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_generate_aux (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "generate-aux");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->generate_aux (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_notify_page_change () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->notify_page_change ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_notify_change (SCM arg1) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "notify-change");

  int in1= scm_to_int (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->notify_change (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_print_to_file (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "print-to-file");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->print_to_file (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_print_pages_to_file (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "print-pages-to-file");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "print-pages-to-file");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "print-pages-to-file");

  url in1= scm_to_url (arg1);
  string in2= scm_to_string (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->print_to_file (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_print () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->print_buffer ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_print_pages (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "print-pages");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "print-pages");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->print_buffer (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_print_snippet (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "print-snippet");
  SCM_ASSERT_CONTENT (arg2, SCM_ARG2, "print-snippet");

  url in1= scm_to_url (arg1);
  content in2= scm_to_content (arg2);

  // SCM_DEFER_INTS;
  array_int out= get_server()->get_editor()->print_snippet (in1, in2);
  // SCM_ALLOW_INTS;

  return array_int_to_scm (out);
}

SCM
tmg_export_postscript (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "export-postscript");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->export_ps (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_export_pages_postscript (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "export-pages-postscript");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "export-pages-postscript");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "export-pages-postscript");

  url in1= scm_to_url (arg1);
  string in2= scm_to_string (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->export_ps (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_footer_eval (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "footer-eval");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->footer_eval (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_texmacs_exec (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "texmacs-exec");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  tree out= get_server()->get_editor()->texmacs_exec (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_texmacs_expand (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "texmacs-expand");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  tree out= get_server()->get_editor()->exec_texmacs (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_verbatim_expand (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "verbatim-expand");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  tree out= get_server()->get_editor()->exec_verbatim (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_latex_expand (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "latex-expand");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  tree out= get_server()->get_editor()->exec_latex (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_html_expand (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "html-expand");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  tree out= get_server()->get_editor()->exec_html (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_idle_time () {
  // SCM_DEFER_INTS;
  int out= get_server()->get_editor()->idle_time ();
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_change_time () {
  // SCM_DEFER_INTS;
  int out= get_server()->get_editor()->change_time ();
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_menu_before_action () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->before_menu_action ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_menu_after_action () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->after_menu_action ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_show_tree () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->show_tree ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_show_env () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->show_env ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_show_path () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->show_path ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_show_cursor () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->show_cursor ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_show_selection () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->show_selection ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_show_meminfo () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->show_meminfo ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_edit_special () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->edit_special ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_edit_test () {
  // SCM_DEFER_INTS;
  get_server()->get_editor()->edit_test ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

void
initialize_glue_editor () {
  scm_install_procedure ("root-tree",  tmg_root_tree, 0, 0, 0);
  scm_install_procedure ("buffer-path",  tmg_buffer_path, 0, 0, 0);
  scm_install_procedure ("buffer-tree",  tmg_buffer_tree, 0, 0, 0);
  scm_install_procedure ("paragraph-tree",  tmg_paragraph_tree, 0, 0, 0);
  scm_install_procedure ("cursor-path",  tmg_cursor_path, 0, 0, 0);
  scm_install_procedure ("cursor-path*",  tmg_cursor_path_dot, 0, 0, 0);
  scm_install_procedure ("selection-tree",  tmg_selection_tree, 0, 0, 0);
  scm_install_procedure ("path->tree",  tmg_path_2tree, 1, 0, 0);
  scm_install_procedure ("path-correct-old",  tmg_path_correct_old, 1, 0, 0);
  scm_install_procedure ("path-insert-with",  tmg_path_insert_with, 3, 0, 0);
  scm_install_procedure ("path-remove-with",  tmg_path_remove_with, 2, 0, 0);
  scm_install_procedure ("position-new-path",  tmg_position_new_path, 1, 0, 0);
  scm_install_procedure ("position-delete",  tmg_position_delete, 1, 0, 0);
  scm_install_procedure ("position-set",  tmg_position_set, 2, 0, 0);
  scm_install_procedure ("position-get",  tmg_position_get, 1, 0, 0);
  scm_install_procedure ("inside?",  tmg_insideP, 1, 0, 0);
  scm_install_procedure ("insert",  tmg_insert, 1, 0, 0);
  scm_install_procedure ("insert-go-to",  tmg_insert_go_to, 2, 0, 0);
  scm_install_procedure ("insert-raw-go-to",  tmg_insert_raw_go_to, 2, 0, 0);
  scm_install_procedure ("insert-raw-return",  tmg_insert_raw_return, 0, 0, 0);
  scm_install_procedure ("remove-text",  tmg_remove_text, 1, 0, 0);
  scm_install_procedure ("remove-structure",  tmg_remove_structure, 1, 0, 0);
  scm_install_procedure ("remove-structure-upwards",  tmg_remove_structure_upwards, 0, 0, 0);
  scm_install_procedure ("make",  tmg_make, 1, 0, 0);
  scm_install_procedure ("make-arity",  tmg_make_arity, 2, 0, 0);
  scm_install_procedure ("activate",  tmg_activate, 0, 0, 0);
  scm_install_procedure ("insert-argument",  tmg_insert_argument, 1, 0, 0);
  scm_install_procedure ("remove-argument",  tmg_remove_argument, 1, 0, 0);
  scm_install_procedure ("insert-argument-at",  tmg_insert_argument_at, 2, 0, 0);
  scm_install_procedure ("remove-argument-at",  tmg_remove_argument_at, 2, 0, 0);
  scm_install_procedure ("make-with",  tmg_make_with, 2, 0, 0);
  scm_install_procedure ("make-mod-active",  tmg_make_mod_active, 1, 0, 0);
  scm_install_procedure ("make-style-with",  tmg_make_style_with, 2, 0, 0);
  scm_install_procedure ("make-hybrid",  tmg_make_hybrid, 0, 0, 0);
  scm_install_procedure ("activate-latex",  tmg_activate_latex, 0, 0, 0);
  scm_install_procedure ("activate-hybrid",  tmg_activate_hybrid, 1, 0, 0);
  scm_install_procedure ("activate-symbol",  tmg_activate_symbol, 0, 0, 0);
  scm_install_procedure ("make-return-before",  tmg_make_return_before, 0, 0, 0);
  scm_install_procedure ("make-return-after",  tmg_make_return_after, 0, 0, 0);
  scm_install_procedure ("temp-proof-fix",  tmg_temp_proof_fix, 0, 0, 0);
  scm_install_procedure ("init-default-one",  tmg_init_default_one, 1, 0, 0);
  scm_install_procedure ("init-env",  tmg_init_env, 2, 0, 0);
  scm_install_procedure ("init-env-tree",  tmg_init_env_tree, 2, 0, 0);
  scm_install_procedure ("init-style",  tmg_init_style, 1, 0, 0);
  scm_install_procedure ("init-add-package",  tmg_init_add_package, 1, 0, 0);
  scm_install_procedure ("init-remove-package",  tmg_init_remove_package, 1, 0, 0);
  scm_install_procedure ("get-style-tree",  tmg_get_style_tree, 0, 0, 0);
  scm_install_procedure ("set-style-tree",  tmg_set_style_tree, 1, 0, 0);
  scm_install_procedure ("get-env",  tmg_get_env, 1, 0, 0);
  scm_install_procedure ("get-env-tree",  tmg_get_env_tree, 1, 0, 0);
  scm_install_procedure ("get-env-tree-at",  tmg_get_env_tree_at, 2, 0, 0);
  scm_install_procedure ("get-init",  tmg_get_init, 1, 0, 0);
  scm_install_procedure ("get-init-tree",  tmg_get_init_tree, 1, 0, 0);
  scm_install_procedure ("context-has?",  tmg_context_hasP, 1, 0, 0);
  scm_install_procedure ("style-has?",  tmg_style_hasP, 1, 0, 0);
  scm_install_procedure ("init-has?",  tmg_init_hasP, 1, 0, 0);
  scm_install_procedure ("make-htab",  tmg_make_htab, 1, 0, 0);
  scm_install_procedure ("make-space",  tmg_make_space, 1, 0, 0);
  scm_install_procedure ("make-var-space",  tmg_make_var_space, 3, 0, 0);
  scm_install_procedure ("make-hspace",  tmg_make_hspace, 1, 0, 0);
  scm_install_procedure ("make-var-hspace",  tmg_make_var_hspace, 3, 0, 0);
  scm_install_procedure ("make-vspace-before",  tmg_make_vspace_before, 1, 0, 0);
  scm_install_procedure ("make-var-vspace-before",  tmg_make_var_vspace_before, 3, 0, 0);
  scm_install_procedure ("make-vspace-after",  tmg_make_vspace_after, 1, 0, 0);
  scm_install_procedure ("make-var-vspace-after",  tmg_make_var_vspace_after, 3, 0, 0);
  scm_install_procedure ("make-image",  tmg_make_image, 6, 0, 0);
  scm_install_procedure ("length-decode",  tmg_length_decode, 1, 0, 0);
  scm_install_procedure ("length-add",  tmg_length_add, 2, 0, 0);
  scm_install_procedure ("length-mult",  tmg_length_mult, 2, 0, 0);
  scm_install_procedure ("length?",  tmg_lengthP, 1, 0, 0);
  scm_install_procedure ("length-divide",  tmg_length_divide, 2, 0, 0);
  scm_install_procedure ("make-rigid",  tmg_make_rigid, 0, 0, 0);
  scm_install_procedure ("make-lprime",  tmg_make_lprime, 1, 0, 0);
  scm_install_procedure ("make-rprime",  tmg_make_rprime, 1, 0, 0);
  scm_install_procedure ("make-below",  tmg_make_below, 0, 0, 0);
  scm_install_procedure ("make-above",  tmg_make_above, 0, 0, 0);
  scm_install_procedure ("make-script",  tmg_make_script, 2, 0, 0);
  scm_install_procedure ("make-fraction",  tmg_make_fraction, 0, 0, 0);
  scm_install_procedure ("make-sqrt",  tmg_make_sqrt, 0, 0, 0);
  scm_install_procedure ("make-wide",  tmg_make_wide, 1, 0, 0);
  scm_install_procedure ("make-wide-under",  tmg_make_wide_under, 1, 0, 0);
  scm_install_procedure ("make-var-sqrt",  tmg_make_var_sqrt, 0, 0, 0);
  scm_install_procedure ("make-neg",  tmg_make_neg, 0, 0, 0);
  scm_install_procedure ("make-tree",  tmg_make_tree, 0, 0, 0);
  scm_install_procedure ("make-subtable",  tmg_make_subtable, 0, 0, 0);
  scm_install_procedure ("table-disactivate",  tmg_table_disactivate, 0, 0, 0);
  scm_install_procedure ("table-extract-format",  tmg_table_extract_format, 0, 0, 0);
  scm_install_procedure ("table-insert-row",  tmg_table_insert_row, 1, 0, 0);
  scm_install_procedure ("table-insert-column",  tmg_table_insert_column, 1, 0, 0);
  scm_install_procedure ("table-remove-row",  tmg_table_remove_row, 1, 0, 0);
  scm_install_procedure ("table-remove-column",  tmg_table_remove_column, 1, 0, 0);
  scm_install_procedure ("table-nr-rows",  tmg_table_nr_rows, 0, 0, 0);
  scm_install_procedure ("table-nr-columns",  tmg_table_nr_columns, 0, 0, 0);
  scm_install_procedure ("table-set-extents",  tmg_table_set_extents, 2, 0, 0);
  scm_install_procedure ("table-which-row",  tmg_table_which_row, 0, 0, 0);
  scm_install_procedure ("table-which-column",  tmg_table_which_column, 0, 0, 0);
  scm_install_procedure ("table-cell-path",  tmg_table_cell_path, 2, 0, 0);
  scm_install_procedure ("table-go-to",  tmg_table_go_to, 2, 0, 0);
  scm_install_procedure ("table-set-format",  tmg_table_set_format, 2, 0, 0);
  scm_install_procedure ("table-get-format",  tmg_table_get_format, 1, 0, 0);
  scm_install_procedure ("table-del-format",  tmg_table_del_format, 1, 0, 0);
  scm_install_procedure ("table-row-decoration",  tmg_table_row_decoration, 1, 0, 0);
  scm_install_procedure ("table-column-decoration",  tmg_table_column_decoration, 1, 0, 0);
  scm_install_procedure ("table-format-center",  tmg_table_format_center, 0, 0, 0);
  scm_install_procedure ("table-correct-block-content",  tmg_table_correct_block_content, 0, 0, 0);
  scm_install_procedure ("set-cell-mode",  tmg_set_cell_mode, 1, 0, 0);
  scm_install_procedure ("get-cell-mode",  tmg_get_cell_mode, 0, 0, 0);
  scm_install_procedure ("cell-set-format",  tmg_cell_set_format, 2, 0, 0);
  scm_install_procedure ("cell-get-format",  tmg_cell_get_format, 1, 0, 0);
  scm_install_procedure ("cell-del-format",  tmg_cell_del_format, 1, 0, 0);
  scm_install_procedure ("table-test",  tmg_table_test, 0, 0, 0);
  scm_install_procedure ("key-press",  tmg_key_press, 1, 0, 0);
  scm_install_procedure ("raw-emulate-keyboard",  tmg_raw_emulate_keyboard, 1, 0, 0);
  scm_install_procedure ("complete-try?",  tmg_complete_tryP, 0, 0, 0);
  scm_install_procedure ("get-input-mode",  tmg_get_input_mode, 0, 0, 0);
  scm_install_procedure ("key-press-search",  tmg_key_press_search, 1, 0, 0);
  scm_install_procedure ("key-press-replace",  tmg_key_press_replace, 1, 0, 0);
  scm_install_procedure ("key-press-spell",  tmg_key_press_spell, 1, 0, 0);
  scm_install_procedure ("key-press-complete",  tmg_key_press_complete, 1, 0, 0);
  scm_install_procedure ("mouse-any",  tmg_mouse_any, 5, 0, 0);
  scm_install_procedure ("set-mouse-pointer",  tmg_set_mouse_pointer, 2, 0, 0);
  scm_install_procedure ("set-predef-mouse-pointer",  tmg_set_predef_mouse_pointer, 1, 0, 0);
  scm_install_procedure ("go-to-path",  tmg_go_to_path, 1, 0, 0);
  scm_install_procedure ("go-left",  tmg_go_left, 0, 0, 0);
  scm_install_procedure ("go-right",  tmg_go_right, 0, 0, 0);
  scm_install_procedure ("go-up",  tmg_go_up, 0, 0, 0);
  scm_install_procedure ("go-down",  tmg_go_down, 0, 0, 0);
  scm_install_procedure ("go-start",  tmg_go_start, 0, 0, 0);
  scm_install_procedure ("go-end",  tmg_go_end, 0, 0, 0);
  scm_install_procedure ("go-start-of",  tmg_go_start_of, 1, 0, 0);
  scm_install_procedure ("go-end-of",  tmg_go_end_of, 1, 0, 0);
  scm_install_procedure ("go-start-with",  tmg_go_start_with, 2, 0, 0);
  scm_install_procedure ("go-end-with",  tmg_go_end_with, 2, 0, 0);
  scm_install_procedure ("go-start-line",  tmg_go_start_line, 0, 0, 0);
  scm_install_procedure ("go-end-line",  tmg_go_end_line, 0, 0, 0);
  scm_install_procedure ("go-page-up",  tmg_go_page_up, 0, 0, 0);
  scm_install_procedure ("go-page-down",  tmg_go_page_down, 0, 0, 0);
  scm_install_procedure ("go-start-paragraph",  tmg_go_start_paragraph, 0, 0, 0);
  scm_install_procedure ("go-end-paragraph",  tmg_go_end_paragraph, 0, 0, 0);
  scm_install_procedure ("go-to-label",  tmg_go_to_label, 1, 0, 0);
  scm_install_procedure ("cursor-show-if-hidden",  tmg_cursor_show_if_hidden, 0, 0, 0);
  scm_install_procedure ("select-all",  tmg_select_all, 0, 0, 0);
  scm_install_procedure ("select-line",  tmg_select_line, 0, 0, 0);
  scm_install_procedure ("select-from-cursor",  tmg_select_from_cursor, 0, 0, 0);
  scm_install_procedure ("select-from-keyboard",  tmg_select_from_keyboard, 1, 0, 0);
  scm_install_procedure ("select-from-shift-keyboard",  tmg_select_from_shift_keyboard, 0, 0, 0);
  scm_install_procedure ("select-enlarge",  tmg_select_enlarge, 0, 0, 0);
  scm_install_procedure ("select-enlarge-environmental",  tmg_select_enlarge_environmental, 0, 0, 0);
  scm_install_procedure ("selection-active-any?",  tmg_selection_active_anyP, 0, 0, 0);
  scm_install_procedure ("selection-active-normal?",  tmg_selection_active_normalP, 0, 0, 0);
  scm_install_procedure ("selection-active-table?",  tmg_selection_active_tableP, 0, 0, 0);
  scm_install_procedure ("selection-active-small?",  tmg_selection_active_smallP, 0, 0, 0);
  scm_install_procedure ("selection-active-enlarging?",  tmg_selection_active_enlargingP, 0, 0, 0);
  scm_install_procedure ("selection-set-start",  tmg_selection_set_start, 0, 0, 0);
  scm_install_procedure ("selection-set-end",  tmg_selection_set_end, 0, 0, 0);
  scm_install_procedure ("selection-get-start",  tmg_selection_get_start, 0, 0, 0);
  scm_install_procedure ("selection-get-end",  tmg_selection_get_end, 0, 0, 0);
  scm_install_procedure ("selection-path",  tmg_selection_path, 0, 0, 0);
  scm_install_procedure ("selection-set",  tmg_selection_set, 2, 0, 0);
  scm_install_procedure ("clipboard-copy",  tmg_clipboard_copy, 1, 0, 0);
  scm_install_procedure ("clipboard-cut",  tmg_clipboard_cut, 1, 0, 0);
  scm_install_procedure ("clipboard-cut-at",  tmg_clipboard_cut_at, 1, 0, 0);
  scm_install_procedure ("clipboard-paste",  tmg_clipboard_paste, 1, 0, 0);
  scm_install_procedure ("selection-move",  tmg_selection_move, 0, 0, 0);
  scm_install_procedure ("clipboard-clear",  tmg_clipboard_clear, 1, 0, 0);
  scm_install_procedure ("selection-cancel",  tmg_selection_cancel, 0, 0, 0);
  scm_install_procedure ("clipboard-set-import",  tmg_clipboard_set_import, 1, 0, 0);
  scm_install_procedure ("clipboard-set-export",  tmg_clipboard_set_export, 1, 0, 0);
  scm_install_procedure ("clipboard-get-import",  tmg_clipboard_get_import, 0, 0, 0);
  scm_install_procedure ("clipboard-get-export",  tmg_clipboard_get_export, 0, 0, 0);
  scm_install_procedure ("set-manual-focus-path",  tmg_set_manual_focus_path, 1, 0, 0);
  scm_install_procedure ("get-manual-focus-path",  tmg_get_manual_focus_path, 0, 0, 0);
  scm_install_procedure ("get-focus-path",  tmg_get_focus_path, 0, 0, 0);
  scm_install_procedure ("clear-undo-history",  tmg_clear_undo_history, 0, 0, 0);
  scm_install_procedure ("commit-changes",  tmg_commit_changes, 0, 0, 0);
  scm_install_procedure ("start-slave",  tmg_start_slave, 1, 0, 0);
  scm_install_procedure ("mark-start",  tmg_mark_start, 1, 0, 0);
  scm_install_procedure ("mark-end",  tmg_mark_end, 1, 0, 0);
  scm_install_procedure ("mark-cancel",  tmg_mark_cancel, 1, 0, 0);
  scm_install_procedure ("remove-undo-mark",  tmg_remove_undo_mark, 0, 0, 0);
  scm_install_procedure ("add-undo-mark",  tmg_add_undo_mark, 0, 0, 0);
  scm_install_procedure ("unredoable-undo",  tmg_unredoable_undo, 0, 0, 0);
  scm_install_procedure ("undo-possibilities",  tmg_undo_possibilities, 0, 0, 0);
  scm_install_procedure ("undo",  tmg_undo, 1, 0, 0);
  scm_install_procedure ("redo-possibilities",  tmg_redo_possibilities, 0, 0, 0);
  scm_install_procedure ("redo",  tmg_redo, 1, 0, 0);
  scm_install_procedure ("show-history",  tmg_show_history, 0, 0, 0);
  scm_install_procedure ("in-graphics?",  tmg_in_graphicsP, 0, 0, 0);
  scm_install_procedure ("get-graphical-x",  tmg_get_graphical_x, 0, 0, 0);
  scm_install_procedure ("get-graphical-y",  tmg_get_graphical_y, 0, 0, 0);
  scm_install_procedure ("get-graphical-object",  tmg_get_graphical_object, 0, 0, 0);
  scm_install_procedure ("set-graphical-object",  tmg_set_graphical_object, 1, 0, 0);
  scm_install_procedure ("invalidate-graphical-object",  tmg_invalidate_graphical_object, 0, 0, 0);
  scm_install_procedure ("graphical-select",  tmg_graphical_select, 2, 0, 0);
  scm_install_procedure ("graphical-select-area",  tmg_graphical_select_area, 4, 0, 0);
  scm_install_procedure ("in-normal-mode?",  tmg_in_normal_modeP, 0, 0, 0);
  scm_install_procedure ("in-search-mode?",  tmg_in_search_modeP, 0, 0, 0);
  scm_install_procedure ("in-replace-mode?",  tmg_in_replace_modeP, 0, 0, 0);
  scm_install_procedure ("in-spell-mode?",  tmg_in_spell_modeP, 0, 0, 0);
  scm_install_procedure ("search-start",  tmg_search_start, 1, 0, 0);
  scm_install_procedure ("search-button-next",  tmg_search_button_next, 0, 0, 0);
  scm_install_procedure ("replace-start",  tmg_replace_start, 3, 0, 0);
  scm_install_procedure ("spell-start",  tmg_spell_start, 0, 0, 0);
  scm_install_procedure ("spell-replace",  tmg_spell_replace, 1, 0, 0);
  scm_install_procedure ("session-complete-command",  tmg_session_complete_command, 1, 0, 0);
  scm_install_procedure ("custom-complete",  tmg_custom_complete, 1, 0, 0);
  scm_install_procedure ("view-set-property",  tmg_view_set_property, 2, 0, 0);
  scm_install_procedure ("view-get-property",  tmg_view_get_property, 1, 0, 0);
  scm_install_procedure ("clear-buffer",  tmg_clear_buffer, 0, 0, 0);
  scm_install_procedure ("tex-buffer",  tmg_tex_buffer, 0, 0, 0);
  scm_install_procedure ("clear-local-info",  tmg_clear_local_info, 0, 0, 0);
  scm_install_procedure ("update-path",  tmg_update_path, 1, 0, 0);
  scm_install_procedure ("update-buffer",  tmg_update_buffer, 0, 0, 0);
  scm_install_procedure ("generate-all-aux",  tmg_generate_all_aux, 0, 0, 0);
  scm_install_procedure ("generate-aux",  tmg_generate_aux, 1, 0, 0);
  scm_install_procedure ("notify-page-change",  tmg_notify_page_change, 0, 0, 0);
  scm_install_procedure ("notify-change",  tmg_notify_change, 1, 0, 0);
  scm_install_procedure ("print-to-file",  tmg_print_to_file, 1, 0, 0);
  scm_install_procedure ("print-pages-to-file",  tmg_print_pages_to_file, 3, 0, 0);
  scm_install_procedure ("print",  tmg_print, 0, 0, 0);
  scm_install_procedure ("print-pages",  tmg_print_pages, 2, 0, 0);
  scm_install_procedure ("print-snippet",  tmg_print_snippet, 2, 0, 0);
  scm_install_procedure ("export-postscript",  tmg_export_postscript, 1, 0, 0);
  scm_install_procedure ("export-pages-postscript",  tmg_export_pages_postscript, 3, 0, 0);
  scm_install_procedure ("footer-eval",  tmg_footer_eval, 1, 0, 0);
  scm_install_procedure ("texmacs-exec",  tmg_texmacs_exec, 1, 0, 0);
  scm_install_procedure ("texmacs-expand",  tmg_texmacs_expand, 1, 0, 0);
  scm_install_procedure ("verbatim-expand",  tmg_verbatim_expand, 1, 0, 0);
  scm_install_procedure ("latex-expand",  tmg_latex_expand, 1, 0, 0);
  scm_install_procedure ("html-expand",  tmg_html_expand, 1, 0, 0);
  scm_install_procedure ("idle-time",  tmg_idle_time, 0, 0, 0);
  scm_install_procedure ("change-time",  tmg_change_time, 0, 0, 0);
  scm_install_procedure ("menu-before-action",  tmg_menu_before_action, 0, 0, 0);
  scm_install_procedure ("menu-after-action",  tmg_menu_after_action, 0, 0, 0);
  scm_install_procedure ("show-tree",  tmg_show_tree, 0, 0, 0);
  scm_install_procedure ("show-env",  tmg_show_env, 0, 0, 0);
  scm_install_procedure ("show-path",  tmg_show_path, 0, 0, 0);
  scm_install_procedure ("show-cursor",  tmg_show_cursor, 0, 0, 0);
  scm_install_procedure ("show-selection",  tmg_show_selection, 0, 0, 0);
  scm_install_procedure ("show-meminfo",  tmg_show_meminfo, 0, 0, 0);
  scm_install_procedure ("edit-special",  tmg_edit_special, 0, 0, 0);
  scm_install_procedure ("edit-test",  tmg_edit_test, 0, 0, 0);
}

