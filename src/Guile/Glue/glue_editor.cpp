
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
tmg_make_move (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "make-move");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "make-move");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_move (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_resize (SCM arg1, SCM arg2, SCM arg3, SCM arg4) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "make-resize");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "make-resize");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "make-resize");
  SCM_ASSERT_STRING (arg4, SCM_ARG4, "make-resize");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  string in3= scm_to_string (arg3);
  string in4= scm_to_string (arg4);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_resize (in1, in2, in3, in4);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_make_postscript (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7, SCM arg8) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "make-postscript");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "make-postscript");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "make-postscript");
  SCM_ASSERT_STRING (arg4, SCM_ARG4, "make-postscript");
  SCM_ASSERT_STRING (arg5, SCM_ARG5, "make-postscript");
  SCM_ASSERT_STRING (arg6, SCM_ARG6, "make-postscript");
  SCM_ASSERT_STRING (arg7, SCM_ARG7, "make-postscript");
  SCM_ASSERT_STRING (arg8, SCM_ARG8, "make-postscript");

  string in1= scm_to_string (arg1);
  bool in2= scm_to_bool (arg2);
  string in3= scm_to_string (arg3);
  string in4= scm_to_string (arg4);
  string in5= scm_to_string (arg5);
  string in6= scm_to_string (arg6);
  string in7= scm_to_string (arg7);
  string in8= scm_to_string (arg8);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->make_postscript (in1, in2, in3, in4, in5, in6, in7, in8);
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
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "table-set-format");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

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
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "cell-set-format");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

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
tmg_selection_set_start_path (SCM arg1) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "selection-set-start-path");

  path in1= scm_to_path (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->selection_set_start (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_selection_set_end_path (SCM arg1) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "selection-set-end-path");

  path in1= scm_to_path (arg1);

  // SCM_DEFER_INTS;
  get_server()->get_editor()->selection_set_end (in1);
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
  scm_new_procedure ("root-tree", (FN) tmg_root_tree, 0, 0, 0);
  scm_new_procedure ("buffer-path", (FN) tmg_buffer_path, 0, 0, 0);
  scm_new_procedure ("buffer-tree", (FN) tmg_buffer_tree, 0, 0, 0);
  scm_new_procedure ("paragraph-tree", (FN) tmg_paragraph_tree, 0, 0, 0);
  scm_new_procedure ("cursor-path", (FN) tmg_cursor_path, 0, 0, 0);
  scm_new_procedure ("cursor-path*", (FN) tmg_cursor_path_dot, 0, 0, 0);
  scm_new_procedure ("selection-tree", (FN) tmg_selection_tree, 0, 0, 0);
  scm_new_procedure ("path->tree", (FN) tmg_path_2tree, 1, 0, 0);
  scm_new_procedure ("path-correct-old", (FN) tmg_path_correct_old, 1, 0, 0);
  scm_new_procedure ("path-insert-with", (FN) tmg_path_insert_with, 3, 0, 0);
  scm_new_procedure ("path-remove-with", (FN) tmg_path_remove_with, 2, 0, 0);
  scm_new_procedure ("position-new-path", (FN) tmg_position_new_path, 1, 0, 0);
  scm_new_procedure ("position-delete", (FN) tmg_position_delete, 1, 0, 0);
  scm_new_procedure ("position-set", (FN) tmg_position_set, 2, 0, 0);
  scm_new_procedure ("position-get", (FN) tmg_position_get, 1, 0, 0);
  scm_new_procedure ("inside?", (FN) tmg_insideP, 1, 0, 0);
  scm_new_procedure ("insert", (FN) tmg_insert, 1, 0, 0);
  scm_new_procedure ("insert-go-to", (FN) tmg_insert_go_to, 2, 0, 0);
  scm_new_procedure ("insert-raw-go-to", (FN) tmg_insert_raw_go_to, 2, 0, 0);
  scm_new_procedure ("insert-raw-return", (FN) tmg_insert_raw_return, 0, 0, 0);
  scm_new_procedure ("remove-text", (FN) tmg_remove_text, 1, 0, 0);
  scm_new_procedure ("remove-structure", (FN) tmg_remove_structure, 1, 0, 0);
  scm_new_procedure ("remove-structure-upwards", (FN) tmg_remove_structure_upwards, 0, 0, 0);
  scm_new_procedure ("make", (FN) tmg_make, 1, 0, 0);
  scm_new_procedure ("make-arity", (FN) tmg_make_arity, 2, 0, 0);
  scm_new_procedure ("activate", (FN) tmg_activate, 0, 0, 0);
  scm_new_procedure ("insert-argument", (FN) tmg_insert_argument, 1, 0, 0);
  scm_new_procedure ("remove-argument", (FN) tmg_remove_argument, 1, 0, 0);
  scm_new_procedure ("make-with", (FN) tmg_make_with, 2, 0, 0);
  scm_new_procedure ("make-mod-active", (FN) tmg_make_mod_active, 1, 0, 0);
  scm_new_procedure ("make-style-with", (FN) tmg_make_style_with, 2, 0, 0);
  scm_new_procedure ("make-hybrid", (FN) tmg_make_hybrid, 0, 0, 0);
  scm_new_procedure ("activate-latex", (FN) tmg_activate_latex, 0, 0, 0);
  scm_new_procedure ("activate-hybrid", (FN) tmg_activate_hybrid, 1, 0, 0);
  scm_new_procedure ("activate-symbol", (FN) tmg_activate_symbol, 0, 0, 0);
  scm_new_procedure ("make-return-before", (FN) tmg_make_return_before, 0, 0, 0);
  scm_new_procedure ("make-return-after", (FN) tmg_make_return_after, 0, 0, 0);
  scm_new_procedure ("temp-proof-fix", (FN) tmg_temp_proof_fix, 0, 0, 0);
  scm_new_procedure ("init-default-one", (FN) tmg_init_default_one, 1, 0, 0);
  scm_new_procedure ("init-env", (FN) tmg_init_env, 2, 0, 0);
  scm_new_procedure ("init-env-tree", (FN) tmg_init_env_tree, 2, 0, 0);
  scm_new_procedure ("init-style", (FN) tmg_init_style, 1, 0, 0);
  scm_new_procedure ("init-add-package", (FN) tmg_init_add_package, 1, 0, 0);
  scm_new_procedure ("init-remove-package", (FN) tmg_init_remove_package, 1, 0, 0);
  scm_new_procedure ("get-style-tree", (FN) tmg_get_style_tree, 0, 0, 0);
  scm_new_procedure ("set-style-tree", (FN) tmg_set_style_tree, 1, 0, 0);
  scm_new_procedure ("get-env", (FN) tmg_get_env, 1, 0, 0);
  scm_new_procedure ("get-env-tree", (FN) tmg_get_env_tree, 1, 0, 0);
  scm_new_procedure ("get-env-tree-at", (FN) tmg_get_env_tree_at, 2, 0, 0);
  scm_new_procedure ("get-init-tree", (FN) tmg_get_init_tree, 1, 0, 0);
  scm_new_procedure ("context-has?", (FN) tmg_context_hasP, 1, 0, 0);
  scm_new_procedure ("style-has?", (FN) tmg_style_hasP, 1, 0, 0);
  scm_new_procedure ("init-has?", (FN) tmg_init_hasP, 1, 0, 0);
  scm_new_procedure ("make-htab", (FN) tmg_make_htab, 1, 0, 0);
  scm_new_procedure ("make-space", (FN) tmg_make_space, 1, 0, 0);
  scm_new_procedure ("make-var-space", (FN) tmg_make_var_space, 3, 0, 0);
  scm_new_procedure ("make-hspace", (FN) tmg_make_hspace, 1, 0, 0);
  scm_new_procedure ("make-var-hspace", (FN) tmg_make_var_hspace, 3, 0, 0);
  scm_new_procedure ("make-vspace-before", (FN) tmg_make_vspace_before, 1, 0, 0);
  scm_new_procedure ("make-var-vspace-before", (FN) tmg_make_var_vspace_before, 3, 0, 0);
  scm_new_procedure ("make-vspace-after", (FN) tmg_make_vspace_after, 1, 0, 0);
  scm_new_procedure ("make-var-vspace-after", (FN) tmg_make_var_vspace_after, 3, 0, 0);
  scm_new_procedure ("make-move", (FN) tmg_make_move, 2, 0, 0);
  scm_new_procedure ("make-resize", (FN) tmg_make_resize, 4, 0, 0);
  scm_new_procedure ("make-postscript", (FN) tmg_make_postscript, 8, 0, 0);
  scm_new_procedure ("length-decode", (FN) tmg_length_decode, 1, 0, 0);
  scm_new_procedure ("length-add", (FN) tmg_length_add, 2, 0, 0);
  scm_new_procedure ("length-mult", (FN) tmg_length_mult, 2, 0, 0);
  scm_new_procedure ("length?", (FN) tmg_lengthP, 1, 0, 0);
  scm_new_procedure ("length-divide", (FN) tmg_length_divide, 2, 0, 0);
  scm_new_procedure ("make-rigid", (FN) tmg_make_rigid, 0, 0, 0);
  scm_new_procedure ("make-lprime", (FN) tmg_make_lprime, 1, 0, 0);
  scm_new_procedure ("make-rprime", (FN) tmg_make_rprime, 1, 0, 0);
  scm_new_procedure ("make-below", (FN) tmg_make_below, 0, 0, 0);
  scm_new_procedure ("make-above", (FN) tmg_make_above, 0, 0, 0);
  scm_new_procedure ("make-script", (FN) tmg_make_script, 2, 0, 0);
  scm_new_procedure ("make-fraction", (FN) tmg_make_fraction, 0, 0, 0);
  scm_new_procedure ("make-sqrt", (FN) tmg_make_sqrt, 0, 0, 0);
  scm_new_procedure ("make-wide", (FN) tmg_make_wide, 1, 0, 0);
  scm_new_procedure ("make-wide-under", (FN) tmg_make_wide_under, 1, 0, 0);
  scm_new_procedure ("make-var-sqrt", (FN) tmg_make_var_sqrt, 0, 0, 0);
  scm_new_procedure ("make-neg", (FN) tmg_make_neg, 0, 0, 0);
  scm_new_procedure ("make-tree", (FN) tmg_make_tree, 0, 0, 0);
  scm_new_procedure ("make-subtable", (FN) tmg_make_subtable, 0, 0, 0);
  scm_new_procedure ("table-disactivate", (FN) tmg_table_disactivate, 0, 0, 0);
  scm_new_procedure ("table-extract-format", (FN) tmg_table_extract_format, 0, 0, 0);
  scm_new_procedure ("table-insert-row", (FN) tmg_table_insert_row, 1, 0, 0);
  scm_new_procedure ("table-insert-column", (FN) tmg_table_insert_column, 1, 0, 0);
  scm_new_procedure ("table-remove-row", (FN) tmg_table_remove_row, 1, 0, 0);
  scm_new_procedure ("table-remove-column", (FN) tmg_table_remove_column, 1, 0, 0);
  scm_new_procedure ("table-nr-rows", (FN) tmg_table_nr_rows, 0, 0, 0);
  scm_new_procedure ("table-nr-columns", (FN) tmg_table_nr_columns, 0, 0, 0);
  scm_new_procedure ("table-which-row", (FN) tmg_table_which_row, 0, 0, 0);
  scm_new_procedure ("table-which-column", (FN) tmg_table_which_column, 0, 0, 0);
  scm_new_procedure ("table-cell-path", (FN) tmg_table_cell_path, 2, 0, 0);
  scm_new_procedure ("table-go-to", (FN) tmg_table_go_to, 2, 0, 0);
  scm_new_procedure ("table-set-format", (FN) tmg_table_set_format, 2, 0, 0);
  scm_new_procedure ("table-get-format", (FN) tmg_table_get_format, 1, 0, 0);
  scm_new_procedure ("table-del-format", (FN) tmg_table_del_format, 1, 0, 0);
  scm_new_procedure ("table-row-decoration", (FN) tmg_table_row_decoration, 1, 0, 0);
  scm_new_procedure ("table-column-decoration", (FN) tmg_table_column_decoration, 1, 0, 0);
  scm_new_procedure ("table-format-center", (FN) tmg_table_format_center, 0, 0, 0);
  scm_new_procedure ("table-correct-block-content", (FN) tmg_table_correct_block_content, 0, 0, 0);
  scm_new_procedure ("set-cell-mode", (FN) tmg_set_cell_mode, 1, 0, 0);
  scm_new_procedure ("get-cell-mode", (FN) tmg_get_cell_mode, 0, 0, 0);
  scm_new_procedure ("cell-set-format", (FN) tmg_cell_set_format, 2, 0, 0);
  scm_new_procedure ("cell-get-format", (FN) tmg_cell_get_format, 1, 0, 0);
  scm_new_procedure ("cell-del-format", (FN) tmg_cell_del_format, 1, 0, 0);
  scm_new_procedure ("table-test", (FN) tmg_table_test, 0, 0, 0);
  scm_new_procedure ("key-press", (FN) tmg_key_press, 1, 0, 0);
  scm_new_procedure ("raw-emulate-keyboard", (FN) tmg_raw_emulate_keyboard, 1, 0, 0);
  scm_new_procedure ("complete-try?", (FN) tmg_complete_tryP, 0, 0, 0);
  scm_new_procedure ("mouse-any", (FN) tmg_mouse_any, 5, 0, 0);
  scm_new_procedure ("set-mouse-pointer", (FN) tmg_set_mouse_pointer, 2, 0, 0);
  scm_new_procedure ("set-predef-mouse-pointer", (FN) tmg_set_predef_mouse_pointer, 1, 0, 0);
  scm_new_procedure ("go-to-path", (FN) tmg_go_to_path, 1, 0, 0);
  scm_new_procedure ("go-left", (FN) tmg_go_left, 0, 0, 0);
  scm_new_procedure ("go-right", (FN) tmg_go_right, 0, 0, 0);
  scm_new_procedure ("go-up", (FN) tmg_go_up, 0, 0, 0);
  scm_new_procedure ("go-down", (FN) tmg_go_down, 0, 0, 0);
  scm_new_procedure ("go-start", (FN) tmg_go_start, 0, 0, 0);
  scm_new_procedure ("go-end", (FN) tmg_go_end, 0, 0, 0);
  scm_new_procedure ("go-start-of", (FN) tmg_go_start_of, 1, 0, 0);
  scm_new_procedure ("go-end-of", (FN) tmg_go_end_of, 1, 0, 0);
  scm_new_procedure ("go-start-with", (FN) tmg_go_start_with, 2, 0, 0);
  scm_new_procedure ("go-end-with", (FN) tmg_go_end_with, 2, 0, 0);
  scm_new_procedure ("go-start-line", (FN) tmg_go_start_line, 0, 0, 0);
  scm_new_procedure ("go-end-line", (FN) tmg_go_end_line, 0, 0, 0);
  scm_new_procedure ("go-page-up", (FN) tmg_go_page_up, 0, 0, 0);
  scm_new_procedure ("go-page-down", (FN) tmg_go_page_down, 0, 0, 0);
  scm_new_procedure ("go-start-paragraph", (FN) tmg_go_start_paragraph, 0, 0, 0);
  scm_new_procedure ("go-end-paragraph", (FN) tmg_go_end_paragraph, 0, 0, 0);
  scm_new_procedure ("go-to-label", (FN) tmg_go_to_label, 1, 0, 0);
  scm_new_procedure ("cursor-show-if-hidden", (FN) tmg_cursor_show_if_hidden, 0, 0, 0);
  scm_new_procedure ("select-all", (FN) tmg_select_all, 0, 0, 0);
  scm_new_procedure ("select-line", (FN) tmg_select_line, 0, 0, 0);
  scm_new_procedure ("select-from-cursor", (FN) tmg_select_from_cursor, 0, 0, 0);
  scm_new_procedure ("select-from-keyboard", (FN) tmg_select_from_keyboard, 1, 0, 0);
  scm_new_procedure ("select-from-shift-keyboard", (FN) tmg_select_from_shift_keyboard, 0, 0, 0);
  scm_new_procedure ("select-enlarge", (FN) tmg_select_enlarge, 0, 0, 0);
  scm_new_procedure ("select-enlarge-environmental", (FN) tmg_select_enlarge_environmental, 0, 0, 0);
  scm_new_procedure ("selection-active-any?", (FN) tmg_selection_active_anyP, 0, 0, 0);
  scm_new_procedure ("selection-active-normal?", (FN) tmg_selection_active_normalP, 0, 0, 0);
  scm_new_procedure ("selection-active-table?", (FN) tmg_selection_active_tableP, 0, 0, 0);
  scm_new_procedure ("selection-active-small?", (FN) tmg_selection_active_smallP, 0, 0, 0);
  scm_new_procedure ("selection-active-enlarging?", (FN) tmg_selection_active_enlargingP, 0, 0, 0);
  scm_new_procedure ("selection-set-start", (FN) tmg_selection_set_start, 0, 0, 0);
  scm_new_procedure ("selection-set-end", (FN) tmg_selection_set_end, 0, 0, 0);
  scm_new_procedure ("selection-get-start", (FN) tmg_selection_get_start, 0, 0, 0);
  scm_new_procedure ("selection-get-end", (FN) tmg_selection_get_end, 0, 0, 0);
  scm_new_procedure ("selection-path", (FN) tmg_selection_path, 0, 0, 0);
  scm_new_procedure ("selection-set-start-path", (FN) tmg_selection_set_start_path, 1, 0, 0);
  scm_new_procedure ("selection-set-end-path", (FN) tmg_selection_set_end_path, 1, 0, 0);
  scm_new_procedure ("clipboard-copy", (FN) tmg_clipboard_copy, 1, 0, 0);
  scm_new_procedure ("clipboard-cut", (FN) tmg_clipboard_cut, 1, 0, 0);
  scm_new_procedure ("clipboard-cut-at", (FN) tmg_clipboard_cut_at, 1, 0, 0);
  scm_new_procedure ("clipboard-paste", (FN) tmg_clipboard_paste, 1, 0, 0);
  scm_new_procedure ("selection-move", (FN) tmg_selection_move, 0, 0, 0);
  scm_new_procedure ("clipboard-clear", (FN) tmg_clipboard_clear, 1, 0, 0);
  scm_new_procedure ("selection-cancel", (FN) tmg_selection_cancel, 0, 0, 0);
  scm_new_procedure ("clipboard-set-import", (FN) tmg_clipboard_set_import, 1, 0, 0);
  scm_new_procedure ("clipboard-set-export", (FN) tmg_clipboard_set_export, 1, 0, 0);
  scm_new_procedure ("clipboard-get-import", (FN) tmg_clipboard_get_import, 0, 0, 0);
  scm_new_procedure ("clipboard-get-export", (FN) tmg_clipboard_get_export, 0, 0, 0);
  scm_new_procedure ("clear-undo-history", (FN) tmg_clear_undo_history, 0, 0, 0);
  scm_new_procedure ("commit-changes", (FN) tmg_commit_changes, 0, 0, 0);
  scm_new_procedure ("start-slave", (FN) tmg_start_slave, 1, 0, 0);
  scm_new_procedure ("remove-undo-mark", (FN) tmg_remove_undo_mark, 0, 0, 0);
  scm_new_procedure ("add-undo-mark", (FN) tmg_add_undo_mark, 0, 0, 0);
  scm_new_procedure ("unredoable-undo", (FN) tmg_unredoable_undo, 0, 0, 0);
  scm_new_procedure ("undo-possibilities", (FN) tmg_undo_possibilities, 0, 0, 0);
  scm_new_procedure ("undo", (FN) tmg_undo, 1, 0, 0);
  scm_new_procedure ("redo-possibilities", (FN) tmg_redo_possibilities, 0, 0, 0);
  scm_new_procedure ("redo", (FN) tmg_redo, 1, 0, 0);
  scm_new_procedure ("show-history", (FN) tmg_show_history, 0, 0, 0);
  scm_new_procedure ("in-graphics?", (FN) tmg_in_graphicsP, 0, 0, 0);
  scm_new_procedure ("get-graphical-x", (FN) tmg_get_graphical_x, 0, 0, 0);
  scm_new_procedure ("get-graphical-y", (FN) tmg_get_graphical_y, 0, 0, 0);
  scm_new_procedure ("get-graphical-object", (FN) tmg_get_graphical_object, 0, 0, 0);
  scm_new_procedure ("set-graphical-object", (FN) tmg_set_graphical_object, 1, 0, 0);
  scm_new_procedure ("invalidate-graphical-object", (FN) tmg_invalidate_graphical_object, 0, 0, 0);
  scm_new_procedure ("graphical-select", (FN) tmg_graphical_select, 2, 0, 0);
  scm_new_procedure ("graphical-select-area", (FN) tmg_graphical_select_area, 4, 0, 0);
  scm_new_procedure ("in-normal-mode?", (FN) tmg_in_normal_modeP, 0, 0, 0);
  scm_new_procedure ("in-search-mode?", (FN) tmg_in_search_modeP, 0, 0, 0);
  scm_new_procedure ("in-replace-mode?", (FN) tmg_in_replace_modeP, 0, 0, 0);
  scm_new_procedure ("in-spell-mode?", (FN) tmg_in_spell_modeP, 0, 0, 0);
  scm_new_procedure ("search-start", (FN) tmg_search_start, 1, 0, 0);
  scm_new_procedure ("search-button-next", (FN) tmg_search_button_next, 0, 0, 0);
  scm_new_procedure ("replace-start", (FN) tmg_replace_start, 3, 0, 0);
  scm_new_procedure ("spell-start", (FN) tmg_spell_start, 0, 0, 0);
  scm_new_procedure ("spell-replace", (FN) tmg_spell_replace, 1, 0, 0);
  scm_new_procedure ("session-complete-command", (FN) tmg_session_complete_command, 1, 0, 0);
  scm_new_procedure ("custom-complete", (FN) tmg_custom_complete, 1, 0, 0);
  scm_new_procedure ("view-set-property", (FN) tmg_view_set_property, 2, 0, 0);
  scm_new_procedure ("view-get-property", (FN) tmg_view_get_property, 1, 0, 0);
  scm_new_procedure ("clear-buffer", (FN) tmg_clear_buffer, 0, 0, 0);
  scm_new_procedure ("tex-buffer", (FN) tmg_tex_buffer, 0, 0, 0);
  scm_new_procedure ("clear-local-info", (FN) tmg_clear_local_info, 0, 0, 0);
  scm_new_procedure ("update-path", (FN) tmg_update_path, 1, 0, 0);
  scm_new_procedure ("update-buffer", (FN) tmg_update_buffer, 0, 0, 0);
  scm_new_procedure ("generate-all-aux", (FN) tmg_generate_all_aux, 0, 0, 0);
  scm_new_procedure ("generate-aux", (FN) tmg_generate_aux, 1, 0, 0);
  scm_new_procedure ("notify-page-change", (FN) tmg_notify_page_change, 0, 0, 0);
  scm_new_procedure ("notify-change", (FN) tmg_notify_change, 1, 0, 0);
  scm_new_procedure ("print-to-file", (FN) tmg_print_to_file, 1, 0, 0);
  scm_new_procedure ("print-pages-to-file", (FN) tmg_print_pages_to_file, 3, 0, 0);
  scm_new_procedure ("print", (FN) tmg_print, 0, 0, 0);
  scm_new_procedure ("print-pages", (FN) tmg_print_pages, 2, 0, 0);
  scm_new_procedure ("print-snippet", (FN) tmg_print_snippet, 2, 0, 0);
  scm_new_procedure ("export-postscript", (FN) tmg_export_postscript, 1, 0, 0);
  scm_new_procedure ("export-pages-postscript", (FN) tmg_export_pages_postscript, 3, 0, 0);
  scm_new_procedure ("footer-eval", (FN) tmg_footer_eval, 1, 0, 0);
  scm_new_procedure ("texmacs-exec", (FN) tmg_texmacs_exec, 1, 0, 0);
  scm_new_procedure ("idle-time", (FN) tmg_idle_time, 0, 0, 0);
  scm_new_procedure ("change-time", (FN) tmg_change_time, 0, 0, 0);
  scm_new_procedure ("menu-before-action", (FN) tmg_menu_before_action, 0, 0, 0);
  scm_new_procedure ("menu-after-action", (FN) tmg_menu_after_action, 0, 0, 0);
  scm_new_procedure ("show-tree", (FN) tmg_show_tree, 0, 0, 0);
  scm_new_procedure ("show-env", (FN) tmg_show_env, 0, 0, 0);
  scm_new_procedure ("show-path", (FN) tmg_show_path, 0, 0, 0);
  scm_new_procedure ("show-cursor", (FN) tmg_show_cursor, 0, 0, 0);
  scm_new_procedure ("show-selection", (FN) tmg_show_selection, 0, 0, 0);
  scm_new_procedure ("show-meminfo", (FN) tmg_show_meminfo, 0, 0, 0);
  scm_new_procedure ("edit-special", (FN) tmg_edit_special, 0, 0, 0);
  scm_new_procedure ("edit-test", (FN) tmg_edit_test, 0, 0, 0);
}
