
/******************************************************************************
*
* This file has been generated automatically using build-glue.scm
* from build-glue-basic.scm. Please do not edit its contents.
* Copyright (C) 2000 Joris van der Hoeven
*
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*
******************************************************************************/

SCM
tmg_texmacs_version_release (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "texmacs-version-release");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= texmacs_version (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_version_beforeP (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "version-before?");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "version-before?");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  bool out= version_inf (in1, in2);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_os_win32P () {
  // SCM_DEFER_INTS;
  bool out= os_win32 ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_os_mingwP () {
  // SCM_DEFER_INTS;
  bool out= os_mingw ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_os_macosP () {
  // SCM_DEFER_INTS;
  bool out= os_macos ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_x_guiP () {
  // SCM_DEFER_INTS;
  bool out= gui_is_x ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_qt_guiP () {
  // SCM_DEFER_INTS;
  bool out= gui_is_qt ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_default_look_and_feel () {
  // SCM_DEFER_INTS;
  string out= default_look_and_feel ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_default_chinese_font () {
  // SCM_DEFER_INTS;
  string out= default_chinese_font_name ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_default_japanese_font () {
  // SCM_DEFER_INTS;
  string out= default_japanese_font_name ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_default_korean_font () {
  // SCM_DEFER_INTS;
  string out= default_korean_font_name ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tm_output (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tm-output");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tm_output (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_tm_errput (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tm-errput");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tm_errput (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_win32_display (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "win32-display");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  win32_display (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_cpp_error () {
  // SCM_DEFER_INTS;
  cpp_error ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_scheme_dialect () {
  // SCM_DEFER_INTS;
  string out= scheme_dialect ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_get_texmacs_path () {
  // SCM_DEFER_INTS;
  string out= get_texmacs_path ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_plugin_list () {
  // SCM_DEFER_INTS;
  scheme_tree out= plugin_list ();
  // SCM_ALLOW_INTS;

  return scheme_tree_to_scm (out);
}

SCM
tmg_set_fast_environments (SCM arg1) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "set-fast-environments");

  bool in1= scm_to_bool (arg1);

  // SCM_DEFER_INTS;
  set_fast_environments (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_font_exists_in_ttP (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "font-exists-in-tt?");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  bool out= tt_font_exists (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_eval_system (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "eval-system");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= eval_system (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_var_eval_system (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "var-eval-system");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= var_eval_system (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_get_locale_language () {
  // SCM_DEFER_INTS;
  string out= get_locale_language ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_texmacs_time () {
  // SCM_DEFER_INTS;
  int out= texmacs_time ();
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_texmacs_memory () {
  // SCM_DEFER_INTS;
  int out= mem_used ();
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_bench_print (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "bench-print");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  bench_print (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_bench_print_all () {
  // SCM_DEFER_INTS;
  bench_print ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_system_wait (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "system-wait");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "system-wait");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  system_wait (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_set_bibtex_command (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "set-bibtex-command");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  set_bibtex_command (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_math_symbol_group (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "math-symbol-group");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= math_symbol_group (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_math_group_members (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "math-group-members");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  array_string out= math_group_members (in1);
  // SCM_ALLOW_INTS;

  return array_string_to_scm (out);
}

SCM
tmg_math_symbol_type (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "math-symbol-type");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= math_symbol_type (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_object_2command (SCM arg1) {
  SCM_ASSERT_OBJECT (arg1, SCM_ARG1, "object->command");

  object in1= scm_to_object (arg1);

  // SCM_DEFER_INTS;
  command out= as_command (in1);
  // SCM_ALLOW_INTS;

  return command_to_scm (out);
}

SCM
tmg_exec_delayed (SCM arg1) {
  SCM_ASSERT_OBJECT (arg1, SCM_ARG1, "exec-delayed");

  object in1= scm_to_object (arg1);

  // SCM_DEFER_INTS;
  exec_delayed (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_exec_delayed_pause (SCM arg1) {
  SCM_ASSERT_OBJECT (arg1, SCM_ARG1, "exec-delayed-pause");

  object in1= scm_to_object (arg1);

  // SCM_DEFER_INTS;
  exec_delayed_pause (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_notify_preferences_loaded () {
  // SCM_DEFER_INTS;
  notify_preferences_loaded ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_set_input_language (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "set-input-language");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  set_input_language (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_get_input_language () {
  // SCM_DEFER_INTS;
  string out= get_input_language ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_set_output_language (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "set-output-language");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  gui_set_output_language (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_get_output_language () {
  // SCM_DEFER_INTS;
  string out= get_output_language ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_translate (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "translate");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  string out= translate (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_translate_from_to (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "translate-from-to");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "translate-from-to");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "translate-from-to");

  content in1= scm_to_content (arg1);
  string in2= scm_to_string (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  string out= translate (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tree_translate (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "tree-translate");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  tree out= tree_translate (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_translate_from_to (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "tree-translate-from-to");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "tree-translate-from-to");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "tree-translate-from-to");

  content in1= scm_to_content (arg1);
  string in2= scm_to_string (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  tree out= tree_translate (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_color (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "color");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  int out= named_color (in1);
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_new_author () {
  // SCM_DEFER_INTS;
  double out= new_author ();
  // SCM_ALLOW_INTS;

  return double_to_scm (out);
}

SCM
tmg_set_author (SCM arg1) {
  SCM_ASSERT_DOUBLE (arg1, SCM_ARG1, "set-author");

  double in1= scm_to_double (arg1);

  // SCM_DEFER_INTS;
  set_author (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_get_author () {
  // SCM_DEFER_INTS;
  double out= get_author ();
  // SCM_ALLOW_INTS;

  return double_to_scm (out);
}

SCM
tmg_debug_set (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "debug-set");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "debug-set");

  string in1= scm_to_string (arg1);
  bool in2= scm_to_bool (arg2);

  // SCM_DEFER_INTS;
  debug_set (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_debug_get (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "debug-get");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  bool out= debug_get (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_cout_buffer () {
  // SCM_DEFER_INTS;
  cout_buffer ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_cout_unbuffer () {
  // SCM_DEFER_INTS;
  string out= cout_unbuffer ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_mark_new () {
  // SCM_DEFER_INTS;
  double out= new_marker ();
  // SCM_ALLOW_INTS;

  return double_to_scm (out);
}

SCM
tmg_glyph_register (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "glyph-register");
  SCM_ASSERT_ARRAY_ARRAY_ARRAY_DOUBLE (arg2, SCM_ARG2, "glyph-register");

  string in1= scm_to_string (arg1);
  array_array_array_double in2= scm_to_array_array_array_double (arg2);

  // SCM_DEFER_INTS;
  register_glyph (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_glyph_recognize (SCM arg1) {
  SCM_ASSERT_ARRAY_ARRAY_ARRAY_DOUBLE (arg1, SCM_ARG1, "glyph-recognize");

  array_array_array_double in1= scm_to_array_array_array_double (arg1);

  // SCM_DEFER_INTS;
  string out= recognize_glyph (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);

}

SCM
tmg_image_2psdoc (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "image->psdoc");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  string out= image_to_psdoc (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tree_2stree (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree->stree");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  scheme_tree out= tree_to_scheme_tree (in1);
  // SCM_ALLOW_INTS;

  return scheme_tree_to_scm (out);
}

SCM
tmg_stree_2tree (SCM arg1) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "stree->tree");

  scheme_tree in1= scm_to_scheme_tree (arg1);

  // SCM_DEFER_INTS;
  tree out= scheme_tree_to_tree (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_2string (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree->string");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  string out= coerce_tree_string (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_string_2tree (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "string->tree");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tree out= coerce_string_tree (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tm_2tree (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "tm->tree");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  tree out= tree (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_atomicP (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-atomic?");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  bool out= is_atomic (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_tree_compoundP (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-compound?");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  bool out= is_compound (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_tree_label (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-label");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  tree_label out= L (in1);
  // SCM_ALLOW_INTS;

  return tree_label_to_scm (out);
}

SCM
tmg_tree_children (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-children");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  array_tree out= A (in1);
  // SCM_ALLOW_INTS;

  return array_tree_to_scm (out);
}

SCM
tmg_tree_arity (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-arity");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  int out= N (in1);
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_tree_child_ref (SCM arg1, SCM arg2) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-child-ref");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tree-child-ref");

  tree in1= scm_to_tree (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  tree out= tree_ref (in1, in2);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_child_setS (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-child-set!");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tree-child-set!");
  SCM_ASSERT_CONTENT (arg3, SCM_ARG3, "tree-child-set!");

  tree in1= scm_to_tree (arg1);
  int in2= scm_to_int (arg2);
  content in3= scm_to_content (arg3);

  // SCM_DEFER_INTS;
  tree out= tree_set (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_child_insert (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "tree-child-insert");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tree-child-insert");
  SCM_ASSERT_CONTENT (arg3, SCM_ARG3, "tree-child-insert");

  content in1= scm_to_content (arg1);
  int in2= scm_to_int (arg2);
  content in3= scm_to_content (arg3);

  // SCM_DEFER_INTS;
  tree out= tree_child_insert (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_ip (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-ip");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  path out= obtain_ip (in1);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_tree_activeP (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-active?");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  bool out= tree_active (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_tree_eqP (SCM arg1, SCM arg2) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-eq?");
  SCM_ASSERT_TREE (arg2, SCM_ARG2, "tree-eq?");

  tree in1= scm_to_tree (arg1);
  tree in2= scm_to_tree (arg2);

  // SCM_DEFER_INTS;
  bool out= strong_equal (in1, in2);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_subtree (SCM arg1, SCM arg2) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "subtree");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "subtree");

  tree in1= scm_to_tree (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  tree out= subtree (in1, in2);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_range (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-range");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tree-range");
  SCM_ASSERT_INT (arg3, SCM_ARG3, "tree-range");

  tree in1= scm_to_tree (arg1);
  int in2= scm_to_int (arg2);
  int in3= scm_to_int (arg3);

  // SCM_DEFER_INTS;
  tree out= tree_range (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_copy (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-copy");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  tree out= copy (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_append (SCM arg1, SCM arg2) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-append");
  SCM_ASSERT_TREE (arg2, SCM_ARG2, "tree-append");

  tree in1= scm_to_tree (arg1);
  tree in2= scm_to_tree (arg2);

  // SCM_DEFER_INTS;
  tree out= tree_append (in1, in2);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_right_index (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-right-index");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  int out= right_index (in1);
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_tree_label_extensionP (SCM arg1) {
  SCM_ASSERT_TREE_LABEL (arg1, SCM_ARG1, "tree-label-extension?");

  tree_label in1= scm_to_tree_label (arg1);

  // SCM_DEFER_INTS;
  bool out= is_extension (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_tree_multi_paragraphP (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-multi-paragraph?");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  bool out= is_multi_paragraph (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_tree_simplify (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-simplify");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  tree out= simplify_correct (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_minimal_arity (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-minimal-arity");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  int out= minimal_arity (in1);
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_tree_maximal_arity (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-maximal-arity");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  int out= maximal_arity (in1);
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_tree_possible_arityP (SCM arg1, SCM arg2) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-possible-arity?");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tree-possible-arity?");

  tree in1= scm_to_tree (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  bool out= correct_arity (in1, in2);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_tree_insert_point (SCM arg1, SCM arg2) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-insert_point");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tree-insert_point");

  tree in1= scm_to_tree (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  int out= insert_point (in1, in2);
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_tree_is_dynamicP (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-is-dynamic?");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  bool out= is_dynamic (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_tree_accessible_childP (SCM arg1, SCM arg2) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-accessible-child?");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tree-accessible-child?");

  tree in1= scm_to_tree (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  bool out= is_accessible_child (in1, in2);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_tree_accessible_children (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-accessible-children");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  array_tree out= accessible_children (in1);
  // SCM_ALLOW_INTS;

  return array_tree_to_scm (out);
}

SCM
tmg_tree_all_accessibleP (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "tree-all-accessible?");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  bool out= all_accessible (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_tree_none_accessibleP (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "tree-none-accessible?");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  bool out= none_accessible (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_tree_name (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "tree-name");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  string out= get_name (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tree_long_name (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "tree-long-name");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  string out= get_long_name (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tree_child_name (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "tree-child-name");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tree-child-name");

  content in1= scm_to_content (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  string out= get_child_name (in1, in2);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tree_child_long_name (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "tree-child-long-name");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tree-child-long-name");

  content in1= scm_to_content (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  string out= get_child_long_name (in1, in2);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tree_child_type (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "tree-child-type");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tree-child-type");

  content in1= scm_to_content (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  string out= get_child_type (in1, in2);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tree_load_inclusion (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "tree-load-inclusion");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  tree out= load_inclusion (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_as_string (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "tree-as-string");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  string out= tree_as_string (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tree_extents (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "tree-extents");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  tree out= tree_extents (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_emptyP (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "tree-empty?");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  bool out= is_empty (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_tree_is_bufferP (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-is-buffer?");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  bool out= admits_edit_observer (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_tree_search_sections (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-search-sections");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  array_tree out= search_sections (in1);
  // SCM_ALLOW_INTS;

  return array_tree_to_scm (out);
}

SCM
tmg_tree_assign (SCM arg1, SCM arg2) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-assign");
  SCM_ASSERT_CONTENT (arg2, SCM_ARG2, "tree-assign");

  tree in1= scm_to_tree (arg1);
  content in2= scm_to_content (arg2);

  // SCM_DEFER_INTS;
  tree out= tree_assign (in1, in2);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_var_insert (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-var-insert");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tree-var-insert");
  SCM_ASSERT_CONTENT (arg3, SCM_ARG3, "tree-var-insert");

  tree in1= scm_to_tree (arg1);
  int in2= scm_to_int (arg2);
  content in3= scm_to_content (arg3);

  // SCM_DEFER_INTS;
  tree out= tree_insert (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_remove (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-remove");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tree-remove");
  SCM_ASSERT_INT (arg3, SCM_ARG3, "tree-remove");

  tree in1= scm_to_tree (arg1);
  int in2= scm_to_int (arg2);
  int in3= scm_to_int (arg3);

  // SCM_DEFER_INTS;
  tree out= tree_remove (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_split (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-split");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tree-split");
  SCM_ASSERT_INT (arg3, SCM_ARG3, "tree-split");

  tree in1= scm_to_tree (arg1);
  int in2= scm_to_int (arg2);
  int in3= scm_to_int (arg3);

  // SCM_DEFER_INTS;
  tree out= tree_split (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_join (SCM arg1, SCM arg2) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-join");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tree-join");

  tree in1= scm_to_tree (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  tree out= tree_join (in1, in2);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_assign_node (SCM arg1, SCM arg2) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-assign-node");
  SCM_ASSERT_TREE_LABEL (arg2, SCM_ARG2, "tree-assign-node");

  tree in1= scm_to_tree (arg1);
  tree_label in2= scm_to_tree_label (arg2);

  // SCM_DEFER_INTS;
  tree out= tree_assign_node (in1, in2);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_insert_node (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-insert-node");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tree-insert-node");
  SCM_ASSERT_CONTENT (arg3, SCM_ARG3, "tree-insert-node");

  tree in1= scm_to_tree (arg1);
  int in2= scm_to_int (arg2);
  content in3= scm_to_content (arg3);

  // SCM_DEFER_INTS;
  tree out= tree_insert_node (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_remove_node (SCM arg1, SCM arg2) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-remove-node");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tree-remove-node");

  tree in1= scm_to_tree (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  tree out= tree_remove_node (in1, in2);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_cpp_tree_correct_node (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "cpp-tree-correct-node");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  correct_node (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_cpp_tree_correct_downwards (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "cpp-tree-correct-downwards");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  correct_downwards (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_cpp_tree_correct_upwards (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "cpp-tree-correct-upwards");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  correct_upwards (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_concat_tokenize_math (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "concat-tokenize-math");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  array_tree out= concat_tokenize (in1);
  // SCM_ALLOW_INTS;

  return array_tree_to_scm (out);
}

SCM
tmg_concat_decompose (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "concat-decompose");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  array_tree out= concat_decompose (in1);
  // SCM_ALLOW_INTS;

  return array_tree_to_scm (out);
}

SCM
tmg_concat_recompose (SCM arg1) {
  SCM_ASSERT_ARRAY_TREE (arg1, SCM_ARG1, "concat-recompose");

  array_tree in1= scm_to_array_tree (arg1);

  // SCM_DEFER_INTS;
  tree out= concat_recompose (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_with_likeP (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "with-like?");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  bool out= is_with_like (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_with_same_typeP (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "with-same-type?");
  SCM_ASSERT_CONTENT (arg2, SCM_ARG2, "with-same-type?");

  content in1= scm_to_content (arg1);
  content in2= scm_to_content (arg2);

  // SCM_DEFER_INTS;
  bool out= with_same_type (in1, in2);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_with_similar_typeP (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "with-similar-type?");
  SCM_ASSERT_CONTENT (arg2, SCM_ARG2, "with-similar-type?");

  content in1= scm_to_content (arg1);
  content in2= scm_to_content (arg2);

  // SCM_DEFER_INTS;
  bool out= with_similar_type (in1, in2);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_with_correct (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "with-correct");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  tree out= with_correct (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_with_correct_superfluous (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "with-correct-superfluous");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  tree out= superfluous_with_correct (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_invisible_correct_superfluous (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "invisible-correct-superfluous");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  tree out= superfluous_invisible_correct (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_invisible_correct_missing (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "invisible-correct-missing");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "invisible-correct-missing");

  content in1= scm_to_content (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  tree out= missing_invisible_correct (in1, in2);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_automatic_correct (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "automatic-correct");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "automatic-correct");

  content in1= scm_to_content (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  tree out= automatic_correct (in1, in2);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_manual_correct (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "manual-correct");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  tree out= manual_correct (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_upgrade_brackets (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "tree-upgrade-brackets");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "tree-upgrade-brackets");

  content in1= scm_to_content (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  tree out= upgrade_brackets (in1, in2);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_upgrade_big (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "tree-upgrade-big");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  tree out= upgrade_big (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_downgrade_brackets (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "tree-downgrade-brackets");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "tree-downgrade-brackets");
  SCM_ASSERT_BOOL (arg3, SCM_ARG3, "tree-downgrade-brackets");

  content in1= scm_to_content (arg1);
  bool in2= scm_to_bool (arg2);
  bool in3= scm_to_bool (arg3);

  // SCM_DEFER_INTS;
  tree out= downgrade_brackets (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_tree_downgrade_big (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "tree-downgrade-big");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  tree out= downgrade_big (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_math_status_print () {
  // SCM_DEFER_INTS;
  math_status_print ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_math_status_reset () {
  // SCM_DEFER_INTS;
  math_status_reset ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_path_infP (SCM arg1, SCM arg2) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "path-inf?");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "path-inf?");

  path in1= scm_to_path (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  bool out= path_inf (in1, in2);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_path_inf_eqP (SCM arg1, SCM arg2) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "path-inf-eq?");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "path-inf-eq?");

  path in1= scm_to_path (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  bool out= path_inf_eq (in1, in2);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_path_lessP (SCM arg1, SCM arg2) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "path-less?");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "path-less?");

  path in1= scm_to_path (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  bool out= path_less (in1, in2);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_path_less_eqP (SCM arg1, SCM arg2) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "path-less-eq?");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "path-less-eq?");

  path in1= scm_to_path (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  bool out= path_less_eq (in1, in2);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_path_start (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "path-start");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "path-start");

  content in1= scm_to_content (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  path out= start (in1, in2);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_path_end (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "path-end");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "path-end");

  content in1= scm_to_content (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  path out= end (in1, in2);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_path_next (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "path-next");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "path-next");

  content in1= scm_to_content (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  path out= next_valid (in1, in2);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_path_previous (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "path-previous");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "path-previous");

  content in1= scm_to_content (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  path out= previous_valid (in1, in2);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_path_next_word (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "path-next-word");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "path-next-word");

  content in1= scm_to_content (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  path out= next_word (in1, in2);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_path_previous_word (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "path-previous-word");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "path-previous-word");

  content in1= scm_to_content (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  path out= previous_word (in1, in2);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_path_next_node (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "path-next-node");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "path-next-node");

  content in1= scm_to_content (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  path out= next_node (in1, in2);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_path_previous_node (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "path-previous-node");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "path-previous-node");

  content in1= scm_to_content (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  path out= previous_node (in1, in2);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_path_next_tag (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "path-next-tag");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "path-next-tag");
  SCM_ASSERT_SCHEME_TREE (arg3, SCM_ARG3, "path-next-tag");

  content in1= scm_to_content (arg1);
  path in2= scm_to_path (arg2);
  scheme_tree in3= scm_to_scheme_tree (arg3);

  // SCM_DEFER_INTS;
  path out= next_tag (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_path_previous_tag (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "path-previous-tag");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "path-previous-tag");
  SCM_ASSERT_SCHEME_TREE (arg3, SCM_ARG3, "path-previous-tag");

  content in1= scm_to_content (arg1);
  path in2= scm_to_path (arg2);
  scheme_tree in3= scm_to_scheme_tree (arg3);

  // SCM_DEFER_INTS;
  path out= previous_tag (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_path_next_tag_same_argument (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "path-next-tag-same-argument");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "path-next-tag-same-argument");
  SCM_ASSERT_SCHEME_TREE (arg3, SCM_ARG3, "path-next-tag-same-argument");

  content in1= scm_to_content (arg1);
  path in2= scm_to_path (arg2);
  scheme_tree in3= scm_to_scheme_tree (arg3);

  // SCM_DEFER_INTS;
  path out= next_tag_same_argument (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_path_previous_tag_same_argument (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "path-previous-tag-same-argument");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "path-previous-tag-same-argument");
  SCM_ASSERT_SCHEME_TREE (arg3, SCM_ARG3, "path-previous-tag-same-argument");

  content in1= scm_to_content (arg1);
  path in2= scm_to_path (arg2);
  scheme_tree in3= scm_to_scheme_tree (arg3);

  // SCM_DEFER_INTS;
  path out= previous_tag_same_argument (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_path_next_argument (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "path-next-argument");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "path-next-argument");

  content in1= scm_to_content (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  path out= next_argument (in1, in2);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_path_previous_argument (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "path-previous-argument");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "path-previous-argument");

  content in1= scm_to_content (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  path out= previous_argument (in1, in2);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_path_previous_section (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "path-previous-section");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "path-previous-section");

  content in1= scm_to_content (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  path out= previous_section (in1, in2);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_tree_2ids (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree->ids");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  list_string out= get_ids (in1);
  // SCM_ALLOW_INTS;

  return list_string_to_scm (out);
}

SCM
tmg_id_2trees (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "id->trees");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  list_tree out= get_trees (in1);
  // SCM_ALLOW_INTS;

  return list_tree_to_scm (out);
}

SCM
tmg_vertex_2links (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "vertex->links");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  list_tree out= get_links (in1);
  // SCM_ALLOW_INTS;

  return list_tree_to_scm (out);
}

SCM
tmg_tree_2tree_pointer (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree->tree-pointer");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  observer out= tree_pointer_new (in1);
  // SCM_ALLOW_INTS;

  return observer_to_scm (out);
}

SCM
tmg_tree_pointer_detach (SCM arg1) {
  SCM_ASSERT_OBSERVER (arg1, SCM_ARG1, "tree-pointer-detach");

  observer in1= scm_to_observer (arg1);

  // SCM_DEFER_INTS;
  tree_pointer_delete (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_tree_pointer_2tree (SCM arg1) {
  SCM_ASSERT_OBSERVER (arg1, SCM_ARG1, "tree-pointer->tree");

  observer in1= scm_to_observer (arg1);

  // SCM_DEFER_INTS;
  tree out= obtain_tree (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_current_link_types () {
  // SCM_DEFER_INTS;
  list_string out= all_link_types ();
  // SCM_ALLOW_INTS;

  return list_string_to_scm (out);
}

SCM
tmg_get_locus_rendering (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "get-locus-rendering");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= get_locus_rendering (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_set_locus_rendering (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "set-locus-rendering");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "set-locus-rendering");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  set_locus_rendering (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_declare_visited (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "declare-visited");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  declare_visited (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_has_been_visitedP (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "has-been-visited?");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  bool out= has_been_visited (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_graphics_set (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "graphics-set");
  SCM_ASSERT_CONTENT (arg2, SCM_ARG2, "graphics-set");

  content in1= scm_to_content (arg1);
  content in2= scm_to_content (arg2);

  // SCM_DEFER_INTS;
  set_graphical_value (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_graphics_hasP (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "graphics-has?");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  bool out= has_graphical_value (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_graphics_ref (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "graphics-ref");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  tree out= get_graphical_value (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_graphics_needs_updateP () {
  // SCM_DEFER_INTS;
  bool out= graphics_needs_update ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_graphics_notify_update (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "graphics-notify-update");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  graphics_notify_update (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_string_numberP (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "string-number?");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  bool out= is_double (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_string_occursP (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "string-occurs?");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "string-occurs?");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  bool out= occurs (in1, in2);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_string_search_forwards (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "string-search-forwards");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "string-search-forwards");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "string-search-forwards");

  string in1= scm_to_string (arg1);
  int in2= scm_to_int (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  int out= search_forwards (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_string_search_backwards (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "string-search-backwards");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "string-search-backwards");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "string-search-backwards");

  string in1= scm_to_string (arg1);
  int in2= scm_to_int (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  int out= search_backwards (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_string_replace (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "string-replace");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "string-replace");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "string-replace");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  string out= replace (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_string_alphaP (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "string-alpha?");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  bool out= is_alpha (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_string_locase_alphaP (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "string-locase-alpha?");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  bool out= is_locase_alpha (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_upcase_first (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "upcase-first");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= upcase_first (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_locase_first (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "locase-first");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= locase_first (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_upcase_all (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "upcase-all");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= upcase_all (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_locase_all (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "locase-all");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= locase_all (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_string_union (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "string-union");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "string-union");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  string out= string_union (in1, in2);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_string_minus (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "string-minus");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "string-minus");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  string out= string_minus (in1, in2);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_escape_generic (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "escape-generic");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= escape_generic (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_escape_verbatim (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "escape-verbatim");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= escape_verbatim (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_escape_shell (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "escape-shell");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= escape_sh (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_string_convert (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "string-convert");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "string-convert");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "string-convert");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  string out= convert (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_utf8_2cork (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "utf8->cork");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= utf8_to_cork (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_cork_2utf8 (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "cork->utf8");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= cork_to_utf8 (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_utf8_2html (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "utf8->html");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= utf8_to_html (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tm_2xml_name (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tm->xml-name");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= tm_to_xml_name (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_old_tm_2xml_cdata (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "old-tm->xml-cdata");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= old_tm_to_xml_cdata (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tm_2xml_cdata (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tm->xml-cdata");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  object out= tm_to_xml_cdata (in1);
  // SCM_ALLOW_INTS;

  return object_to_scm (out);
}

SCM
tmg_xml_name_2tm (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "xml-name->tm");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= xml_name_to_tm (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_old_xml_cdata_2tm (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "old-xml-cdata->tm");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= old_xml_cdata_to_tm (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_xml_unspace (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "xml-unspace");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "xml-unspace");
  SCM_ASSERT_BOOL (arg3, SCM_ARG3, "xml-unspace");

  string in1= scm_to_string (arg1);
  bool in2= scm_to_bool (arg2);
  bool in3= scm_to_bool (arg3);

  // SCM_DEFER_INTS;
  string out= xml_unspace (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_integer_2hexadecimal (SCM arg1) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "integer->hexadecimal");

  int in1= scm_to_int (arg1);

  // SCM_DEFER_INTS;
  string out= as_hexadecimal (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_integer_2padded_hexadecimal (SCM arg1, SCM arg2) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "integer->padded-hexadecimal");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "integer->padded-hexadecimal");

  int in1= scm_to_int (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  string out= as_hexadecimal (in1, in2);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_hexadecimal_2integer (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "hexadecimal->integer");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  int out= from_hexadecimal (in1);
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_string_2tmstring (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "string->tmstring");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= tm_encode (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tmstring_2string (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmstring->string");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= tm_decode (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tmstring_length (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmstring-length");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  int out= tm_string_length (in1);
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_tmstring_ref (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmstring-ref");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tmstring-ref");

  string in1= scm_to_string (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  string out= tm_forward_access (in1, in2);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tmstring_reverse_ref (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmstring-reverse-ref");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tmstring-reverse-ref");

  string in1= scm_to_string (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  string out= tm_backward_access (in1, in2);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tmstring_2list (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmstring->list");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  array_string out= tm_tokenize (in1);
  // SCM_ALLOW_INTS;

  return array_string_to_scm (out);
}

SCM
tmg_list_2tmstring (SCM arg1) {
  SCM_ASSERT_ARRAY_STRING (arg1, SCM_ARG1, "list->tmstring");

  array_string in1= scm_to_array_string (arg1);

  // SCM_DEFER_INTS;
  string out= tm_recompose (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_string_next (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "string-next");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "string-next");

  string in1= scm_to_string (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  int out= tm_char_next (in1, in2);
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_string_previous (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "string-previous");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "string-previous");

  string in1= scm_to_string (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  int out= tm_char_previous (in1, in2);
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_packrat_define (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "packrat-define");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "packrat-define");
  SCM_ASSERT_TREE (arg3, SCM_ARG3, "packrat-define");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  tree in3= scm_to_tree (arg3);

  // SCM_DEFER_INTS;
  packrat_define (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_packrat_property (SCM arg1, SCM arg2, SCM arg3, SCM arg4) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "packrat-property");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "packrat-property");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "packrat-property");
  SCM_ASSERT_STRING (arg4, SCM_ARG4, "packrat-property");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  string in3= scm_to_string (arg3);
  string in4= scm_to_string (arg4);

  // SCM_DEFER_INTS;
  packrat_property (in1, in2, in3, in4);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_packrat_inherit (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "packrat-inherit");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "packrat-inherit");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  packrat_inherit (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_packrat_parse (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "packrat-parse");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "packrat-parse");
  SCM_ASSERT_CONTENT (arg3, SCM_ARG3, "packrat-parse");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  content in3= scm_to_content (arg3);

  // SCM_DEFER_INTS;
  path out= packrat_parse (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_packrat_correctP (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "packrat-correct?");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "packrat-correct?");
  SCM_ASSERT_CONTENT (arg3, SCM_ARG3, "packrat-correct?");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  content in3= scm_to_content (arg3);

  // SCM_DEFER_INTS;
  bool out= packrat_correct (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_packrat_context (SCM arg1, SCM arg2, SCM arg3, SCM arg4) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "packrat-context");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "packrat-context");
  SCM_ASSERT_CONTENT (arg3, SCM_ARG3, "packrat-context");
  SCM_ASSERT_PATH (arg4, SCM_ARG4, "packrat-context");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  content in3= scm_to_content (arg3);
  path in4= scm_to_path (arg4);

  // SCM_DEFER_INTS;
  object out= packrat_context (in1, in2, in3, in4);
  // SCM_ALLOW_INTS;

  return object_to_scm (out);
}

SCM
tmg_parse_texmacs (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "parse-texmacs");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tree out= texmacs_document_to_tree (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_serialize_texmacs (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "serialize-texmacs");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  string out= tree_to_texmacs (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_parse_texmacs_snippet (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "parse-texmacs-snippet");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tree out= texmacs_to_tree (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_serialize_texmacs_snippet (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "serialize-texmacs-snippet");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  string out= tree_to_texmacs (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_texmacs_2stm (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "texmacs->stm");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  string out= tree_to_scheme (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_stm_2texmacs (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "stm->texmacs");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tree out= scheme_document_to_tree (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_stm_snippet_2texmacs (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "stm-snippet->texmacs");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tree out= scheme_to_tree (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_cpp_texmacs_2verbatim (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "cpp-texmacs->verbatim");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "cpp-texmacs->verbatim");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "cpp-texmacs->verbatim");

  tree in1= scm_to_tree (arg1);
  bool in2= scm_to_bool (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  string out= tree_to_verbatim (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_cpp_verbatim_snippet_2texmacs (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "cpp-verbatim-snippet->texmacs");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "cpp-verbatim-snippet->texmacs");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "cpp-verbatim-snippet->texmacs");

  string in1= scm_to_string (arg1);
  bool in2= scm_to_bool (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  tree out= verbatim_to_tree (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_cpp_verbatim_2texmacs (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "cpp-verbatim->texmacs");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "cpp-verbatim->texmacs");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "cpp-verbatim->texmacs");

  string in1= scm_to_string (arg1);
  bool in2= scm_to_bool (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  tree out= verbatim_document_to_tree (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_parse_latex (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "parse-latex");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tree out= parse_latex (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_parse_latex_document (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "parse-latex-document");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tree out= parse_latex_document (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_latex_2texmacs (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "latex->texmacs");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  tree out= latex_to_tree (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_latex_document_2texmacs (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "latex-document->texmacs");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tree out= latex_document_to_tree (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_latex_class_document_2texmacs (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "latex-class-document->texmacs");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tree out= latex_class_document_to_tree (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_parse_xml (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "parse-xml");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  scheme_tree out= parse_xml (in1);
  // SCM_ALLOW_INTS;

  return scheme_tree_to_scm (out);
}

SCM
tmg_parse_html (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "parse-html");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  scheme_tree out= parse_html (in1);
  // SCM_ALLOW_INTS;

  return scheme_tree_to_scm (out);
}

SCM
tmg_parse_bib (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "parse-bib");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tree out= parse_bib (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_upgrade_tmml (SCM arg1) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "upgrade-tmml");

  scheme_tree in1= scm_to_scheme_tree (arg1);

  // SCM_DEFER_INTS;
  tree out= tmml_upgrade (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_upgrade_mathml (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "upgrade-mathml");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  tree out= upgrade_mathml (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_string_2url (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "string->url");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  url out= url (in1);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "url");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "url");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  url out= url (in1, in2);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_system (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "url-system");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  url out= url_system (in1);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_none () {
  // SCM_DEFER_INTS;
  url out= url_none ();
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_any () {
  // SCM_DEFER_INTS;
  url out= url_wildcard ();
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_wildcard (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "url-wildcard");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  url out= url_wildcard (in1);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_parent () {
  // SCM_DEFER_INTS;
  url out= url_parent ();
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_ancestor () {
  // SCM_DEFER_INTS;
  url out= url_ancestor ();
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_append (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-append");
  SCM_ASSERT_URL (arg2, SCM_ARG2, "url-append");

  url in1= scm_to_url (arg1);
  url in2= scm_to_url (arg2);

  // SCM_DEFER_INTS;
  url out= url_concat (in1, in2);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_or (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-or");
  SCM_ASSERT_URL (arg2, SCM_ARG2, "url-or");

  url in1= scm_to_url (arg1);
  url in2= scm_to_url (arg2);

  // SCM_DEFER_INTS;
  url out= url_or (in1, in2);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_2string (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url->string");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  string out= as_string (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_url_noneP (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-none?");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  bool out= is_none (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_url_rooted_webP (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-rooted-web?");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  bool out= is_rooted_web (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_url_concatP (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-concat?");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  bool out= is_concat (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_url_orP (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-or?");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  bool out= is_or (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_url_ref (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-ref");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "url-ref");

  url in1= scm_to_url (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  url out= url_ref (in1, in2);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_head (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-head");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  url out= head (in1);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_tail (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-tail");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  url out= tail (in1);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_suffix (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-suffix");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  string out= suffix (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_url_glue (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-glue");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "url-glue");

  url in1= scm_to_url (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  url out= glue (in1, in2);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_unglue (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-unglue");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "url-unglue");

  url in1= scm_to_url (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  url out= unglue (in1, in2);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_relative (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-relative");
  SCM_ASSERT_URL (arg2, SCM_ARG2, "url-relative");

  url in1= scm_to_url (arg1);
  url in2= scm_to_url (arg2);

  // SCM_DEFER_INTS;
  url out= relative (in1, in2);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_expand (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-expand");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  url out= expand (in1);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_factor (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-factor");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  url out= factor (in1);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_delta (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-delta");
  SCM_ASSERT_URL (arg2, SCM_ARG2, "url-delta");

  url in1= scm_to_url (arg1);
  url in2= scm_to_url (arg2);

  // SCM_DEFER_INTS;
  url out= delta (in1, in2);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_secureP (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-secure?");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  bool out= is_secure (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_url_descendsP (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-descends?");
  SCM_ASSERT_URL (arg2, SCM_ARG2, "url-descends?");

  url in1= scm_to_url (arg1);
  url in2= scm_to_url (arg2);

  // SCM_DEFER_INTS;
  bool out= descends (in1, in2);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_url_complete (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-complete");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "url-complete");

  url in1= scm_to_url (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  url out= complete (in1, in2);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_resolve (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-resolve");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "url-resolve");

  url in1= scm_to_url (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  url out= resolve (in1, in2);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_resolve_in_path (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-resolve-in-path");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  url out= resolve_in_path (in1);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_existsP (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-exists?");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  bool out= exists (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_url_exists_in_pathP (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-exists-in-path?");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  bool out= exists_in_path (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_url_exists_in_texP (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-exists-in-tex?");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  bool out= exists_in_tex (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_url_concretize (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-concretize");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  string out= concretize (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_url_materialize (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-materialize");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "url-materialize");

  url in1= scm_to_url (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  string out= materialize (in1, in2);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_url_testP (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-test?");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "url-test?");

  url in1= scm_to_url (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  bool out= is_of_type (in1, in2);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_url_regularP (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-regular?");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  bool out= is_regular (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_url_directoryP (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-directory?");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  bool out= is_directory (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_url_linkP (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-link?");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  bool out= is_symbolic_link (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_url_newerP (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-newer?");
  SCM_ASSERT_URL (arg2, SCM_ARG2, "url-newer?");

  url in1= scm_to_url (arg1);
  url in2= scm_to_url (arg2);

  // SCM_DEFER_INTS;
  bool out= is_newer (in1, in2);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_url_last_modified (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-last-modified");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  int out= last_modified (in1);
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_url_temp () {
  // SCM_DEFER_INTS;
  url out= url_temp ();
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_scratch (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "url-scratch");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "url-scratch");
  SCM_ASSERT_INT (arg3, SCM_ARG3, "url-scratch");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  int in3= scm_to_int (arg3);

  // SCM_DEFER_INTS;
  url out= url_scratch (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_url_scratchP (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "url-scratch?");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  bool out= is_scratch (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_string_save (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "string-save");
  SCM_ASSERT_URL (arg2, SCM_ARG2, "string-save");

  string in1= scm_to_string (arg1);
  url in2= scm_to_url (arg2);

  // SCM_DEFER_INTS;
  string_save (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_string_load (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "string-load");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  string out= string_load (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_system_move (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "system-move");
  SCM_ASSERT_URL (arg2, SCM_ARG2, "system-move");

  url in1= scm_to_url (arg1);
  url in2= scm_to_url (arg2);

  // SCM_DEFER_INTS;
  move (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_system_copy (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "system-copy");
  SCM_ASSERT_URL (arg2, SCM_ARG2, "system-copy");

  url in1= scm_to_url (arg1);
  url in2= scm_to_url (arg2);

  // SCM_DEFER_INTS;
  copy (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_system_remove (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "system-remove");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  remove (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_system_mkdir (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "system-mkdir");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  mkdir (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_system_search_score (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "system-search-score");
  SCM_ASSERT_ARRAY_STRING (arg2, SCM_ARG2, "system-search-score");

  url in1= scm_to_url (arg1);
  array_string in2= scm_to_array_string (arg2);

  // SCM_DEFER_INTS;
  int out= search_score (in1, in2);
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_system_1 (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "system-1");
  SCM_ASSERT_URL (arg2, SCM_ARG2, "system-1");

  string in1= scm_to_string (arg1);
  url in2= scm_to_url (arg2);

  // SCM_DEFER_INTS;
  system (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_system_2 (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "system-2");
  SCM_ASSERT_URL (arg2, SCM_ARG2, "system-2");
  SCM_ASSERT_URL (arg3, SCM_ARG3, "system-2");

  string in1= scm_to_string (arg1);
  url in2= scm_to_url (arg2);
  url in3= scm_to_url (arg3);

  // SCM_DEFER_INTS;
  system (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_tmfs_set (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-set");
  SCM_ASSERT_COLLECTION (arg2, SCM_ARG2, "tmfs-set");

  string in1= scm_to_string (arg1);
  collection in2= scm_to_collection (arg2);

  // SCM_DEFER_INTS;
  tmfs_set (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_tmfs_reset (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-reset");
  SCM_ASSERT_COLLECTION (arg2, SCM_ARG2, "tmfs-reset");

  string in1= scm_to_string (arg1);
  collection in2= scm_to_collection (arg2);

  // SCM_DEFER_INTS;
  tmfs_reset (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_tmfs_get (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-get");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  collection out= tmfs_get (in1);
  // SCM_ALLOW_INTS;

  return collection_to_scm (out);
}

SCM
tmg_tmfs_new_save (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-new-save");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "tmfs-new-save");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  tmfs_save (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_tmfs_new_remove (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-new-remove");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tmfs_remove (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_tmfs_new_load (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-new-load");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= tmfs_load (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tmfs_create_ressource () {
  // SCM_DEFER_INTS;
  string out= tmfs_create_ressource ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tmfs_ressource_head (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-ressource-head");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= tmfs_get_head (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tmfs_ressource_versions (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-ressource-versions");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  collection out= tmfs_get_versions (in1);
  // SCM_ALLOW_INTS;

  return collection_to_scm (out);
}

SCM
tmg_tmfs_save_ressource (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-save-ressource");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "tmfs-save-ressource");
  SCM_ASSERT_PROPERTIES (arg3, SCM_ARG3, "tmfs-save-ressource");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  properties in3= scm_to_properties (arg3);

  // SCM_DEFER_INTS;
  tmfs_save_ressource (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_tmfs_load_ressource_file (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-load-ressource-file");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= tmfs_load_ressource_file (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tmfs_load_ressource_properties (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-load-ressource-properties");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  properties out= tmfs_load_ressource_properties (in1);
  // SCM_ALLOW_INTS;

  return properties_to_scm (out);
}

SCM
tmg_tmfs_create_user (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-create-user");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= tmfs_create_user (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tmfs_search_user (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-search-user");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  collection out= tmfs_search_user (in1);
  // SCM_ALLOW_INTS;

  return collection_to_scm (out);
}

SCM
tmg_tmfs_set_user (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-set-user");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tmfs_set_user (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_tmfs_get_user () {
  // SCM_DEFER_INTS;
  string out= tmfs_get_user ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tmfs_allowsP (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-allows?");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "tmfs-allows?");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  bool out= tmfs_allows (in1, in2);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_tmfs_set_attributes (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-set-attributes");
  SCM_ASSERT_PROPERTIES (arg2, SCM_ARG2, "tmfs-set-attributes");

  string in1= scm_to_string (arg1);
  properties in2= scm_to_properties (arg2);

  // SCM_DEFER_INTS;
  tmfs_set_attributes (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_tmfs_get_attributes (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-get-attributes");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  properties out= tmfs_get_attributes (in1);
  // SCM_ALLOW_INTS;

  return properties_to_scm (out);
}

SCM
tmg_tmfs_add_attributes (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-add-attributes");
  SCM_ASSERT_PROPERTIES (arg2, SCM_ARG2, "tmfs-add-attributes");

  string in1= scm_to_string (arg1);
  properties in2= scm_to_properties (arg2);

  // SCM_DEFER_INTS;
  tmfs_add_attributes (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_tmfs_remove_attributes (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-remove-attributes");
  SCM_ASSERT_PROPERTIES (arg2, SCM_ARG2, "tmfs-remove-attributes");

  string in1= scm_to_string (arg1);
  properties in2= scm_to_properties (arg2);

  // SCM_DEFER_INTS;
  tmfs_remove_attributes (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_tmfs_change_attributes (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-change-attributes");
  SCM_ASSERT_PROPERTIES (arg2, SCM_ARG2, "tmfs-change-attributes");

  string in1= scm_to_string (arg1);
  properties in2= scm_to_properties (arg2);

  // SCM_DEFER_INTS;
  tmfs_change_attributes (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_tmfs_query (SCM arg1) {
  SCM_ASSERT_PROPERTIES (arg1, SCM_ARG1, "tmfs-query");

  properties in1= scm_to_properties (arg1);

  // SCM_DEFER_INTS;
  solutions out= tmfs_query (in1);
  // SCM_ALLOW_INTS;

  return solutions_to_scm (out);
}

SCM
tmg_solutions_2collection (SCM arg1, SCM arg2) {
  SCM_ASSERT_SOLUTIONS (arg1, SCM_ARG1, "solutions->collection");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "solutions->collection");

  solutions in1= scm_to_solutions (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  collection out= as_collection (in1, in2);
  // SCM_ALLOW_INTS;

  return collection_to_scm (out);
}

SCM
tmg_tmfs_create_file (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-create-file");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "tmfs-create-file");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  string out= tmfs_create_file (in1, in2);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tmfs_create_file_in (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-create-file-in");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "tmfs-create-file-in");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "tmfs-create-file-in");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  string out= tmfs_create_file (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tmfs_search_file (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-search-file");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  collection out= tmfs_search_file (in1);
  // SCM_ALLOW_INTS;

  return collection_to_scm (out);
}

SCM
tmg_tmfs_save_file (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-save-file");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "tmfs-save-file");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  tmfs_save_file (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_tmfs_load_file (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-load-file");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= tmfs_load_file (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tmfs_create_project (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-create-project");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= tmfs_create_project (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tmfs_search_project (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-search-project");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  collection out= tmfs_search_project (in1);
  // SCM_ALLOW_INTS;

  return collection_to_scm (out);
}

SCM
tmg_tmfs_get_file_projects (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-get-file-projects");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  collection out= tmfs_get_file_projects (in1);
  // SCM_ALLOW_INTS;

  return collection_to_scm (out);
}

SCM
tmg_tmfs_get_project_files (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-get-project-files");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  collection out= tmfs_get_project_files (in1);
  // SCM_ALLOW_INTS;

  return collection_to_scm (out);
}

SCM
tmg_tmfs_create_branch (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-create-branch");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "tmfs-create-branch");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  string out= tmfs_create_branch (in1, in2);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_tmfs_set_root (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-set-root");
  SCM_ASSERT_URL (arg2, SCM_ARG2, "tmfs-set-root");

  string in1= scm_to_string (arg1);
  url in2= scm_to_url (arg2);

  // SCM_DEFER_INTS;
  tmfs_set_root (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_tmfs_get_root (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tmfs-get-root");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  url out= tmfs_get_root (in1);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_tmfs_import (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "tmfs-import");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  tmfs_import (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_tmfs_export (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "tmfs-export");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  tmfs_export (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_server_start () {
  // SCM_DEFER_INTS;
  server_start ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_server_stop () {
  // SCM_DEFER_INTS;
  server_stop ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_server_read (SCM arg1) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "server-read");

  int in1= scm_to_int (arg1);

  // SCM_DEFER_INTS;
  string out= server_read (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_server_write (SCM arg1, SCM arg2) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "server-write");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "server-write");

  int in1= scm_to_int (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  server_write (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_client_start (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "client-start");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  client_start (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_client_stop () {
  // SCM_DEFER_INTS;
  client_stop ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_client_read () {
  // SCM_DEFER_INTS;
  string out= client_read ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_client_write (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "client-write");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  client_write (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_enter_secure_mode () {
  // SCM_DEFER_INTS;
  enter_secure_mode ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_connection_start (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "connection-start");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "connection-start");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  string out= connection_start (in1, in2);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_connection_status (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "connection-status");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "connection-status");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  int out= connection_status (in1, in2);
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_connection_write_string (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "connection-write-string");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "connection-write-string");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "connection-write-string");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  connection_write (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_connection_write (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "connection-write");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "connection-write");
  SCM_ASSERT_CONTENT (arg3, SCM_ARG3, "connection-write");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  content in3= scm_to_content (arg3);

  // SCM_DEFER_INTS;
  connection_write (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_connection_cmd (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "connection-cmd");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "connection-cmd");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "connection-cmd");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  tree out= connection_cmd (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_connection_eval (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "connection-eval");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "connection-eval");
  SCM_ASSERT_CONTENT (arg3, SCM_ARG3, "connection-eval");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  content in3= scm_to_content (arg3);

  // SCM_DEFER_INTS;
  tree out= connection_eval (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_connection_interrupt (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "connection-interrupt");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "connection-interrupt");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  connection_interrupt (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_connection_stop (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "connection-stop");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "connection-stop");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  connection_stop (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_widget_printer (SCM arg1, SCM arg2) {
  SCM_ASSERT_COMMAND (arg1, SCM_ARG1, "widget-printer");
  SCM_ASSERT_URL (arg2, SCM_ARG2, "widget-printer");

  command in1= scm_to_command (arg1);
  url in2= scm_to_url (arg2);

  // SCM_DEFER_INTS;
  widget out= printer_widget (in1, in2);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_color_picker (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_COMMAND (arg1, SCM_ARG1, "widget-color-picker");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "widget-color-picker");
  SCM_ASSERT_ARRAY_TREE (arg3, SCM_ARG3, "widget-color-picker");

  command in1= scm_to_command (arg1);
  bool in2= scm_to_bool (arg2);
  array_tree in3= scm_to_array_tree (arg3);

  // SCM_DEFER_INTS;
  widget out= color_picker_widget (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_extend (SCM arg1, SCM arg2) {
  SCM_ASSERT_WIDGET (arg1, SCM_ARG1, "widget-extend");
  SCM_ASSERT_ARRAY_WIDGET (arg2, SCM_ARG2, "widget-extend");

  widget in1= scm_to_widget (arg1);
  array_widget in2= scm_to_array_widget (arg2);

  // SCM_DEFER_INTS;
  widget out= extend (in1, in2);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_hmenu (SCM arg1) {
  SCM_ASSERT_ARRAY_WIDGET (arg1, SCM_ARG1, "widget-hmenu");

  array_widget in1= scm_to_array_widget (arg1);

  // SCM_DEFER_INTS;
  widget out= horizontal_menu (in1);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_vmenu (SCM arg1) {
  SCM_ASSERT_ARRAY_WIDGET (arg1, SCM_ARG1, "widget-vmenu");

  array_widget in1= scm_to_array_widget (arg1);

  // SCM_DEFER_INTS;
  widget out= vertical_menu (in1);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_tmenu (SCM arg1, SCM arg2) {
  SCM_ASSERT_ARRAY_WIDGET (arg1, SCM_ARG1, "widget-tmenu");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "widget-tmenu");

  array_widget in1= scm_to_array_widget (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  widget out= tile_menu (in1, in2);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_minibar_menu (SCM arg1) {
  SCM_ASSERT_ARRAY_WIDGET (arg1, SCM_ARG1, "widget-minibar-menu");

  array_widget in1= scm_to_array_widget (arg1);

  // SCM_DEFER_INTS;
  widget out= minibar_menu (in1);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_separator (SCM arg1) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "widget-separator");

  bool in1= scm_to_bool (arg1);

  // SCM_DEFER_INTS;
  widget out= menu_separator (in1);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_menu_group (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "widget-menu-group");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "widget-menu-group");

  string in1= scm_to_string (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  widget out= menu_group (in1, in2);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_pulldown_button (SCM arg1, SCM arg2) {
  SCM_ASSERT_WIDGET (arg1, SCM_ARG1, "widget-pulldown-button");
  SCM_ASSERT_PROMISE_WIDGET (arg2, SCM_ARG2, "widget-pulldown-button");

  widget in1= scm_to_widget (arg1);
  promise_widget in2= scm_to_promise_widget (arg2);

  // SCM_DEFER_INTS;
  widget out= pulldown_button (in1, in2);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_pullright_button (SCM arg1, SCM arg2) {
  SCM_ASSERT_WIDGET (arg1, SCM_ARG1, "widget-pullright-button");
  SCM_ASSERT_PROMISE_WIDGET (arg2, SCM_ARG2, "widget-pullright-button");

  widget in1= scm_to_widget (arg1);
  promise_widget in2= scm_to_promise_widget (arg2);

  // SCM_DEFER_INTS;
  widget out= pullright_button (in1, in2);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_menu_button (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5) {
  SCM_ASSERT_WIDGET (arg1, SCM_ARG1, "widget-menu-button");
  SCM_ASSERT_COMMAND (arg2, SCM_ARG2, "widget-menu-button");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "widget-menu-button");
  SCM_ASSERT_STRING (arg4, SCM_ARG4, "widget-menu-button");
  SCM_ASSERT_INT (arg5, SCM_ARG5, "widget-menu-button");

  widget in1= scm_to_widget (arg1);
  command in2= scm_to_command (arg2);
  string in3= scm_to_string (arg3);
  string in4= scm_to_string (arg4);
  int in5= scm_to_int (arg5);

  // SCM_DEFER_INTS;
  widget out= menu_button (in1, in2, in3, in4, in5);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_toggle (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_COMMAND (arg1, SCM_ARG1, "widget-toggle");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "widget-toggle");
  SCM_ASSERT_INT (arg3, SCM_ARG3, "widget-toggle");

  command in1= scm_to_command (arg1);
  bool in2= scm_to_bool (arg2);
  int in3= scm_to_int (arg3);

  // SCM_DEFER_INTS;
  widget out= toggle_widget (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_balloon (SCM arg1, SCM arg2) {
  SCM_ASSERT_WIDGET (arg1, SCM_ARG1, "widget-balloon");
  SCM_ASSERT_WIDGET (arg2, SCM_ARG2, "widget-balloon");

  widget in1= scm_to_widget (arg1);
  widget in2= scm_to_widget (arg2);

  // SCM_DEFER_INTS;
  widget out= balloon_widget (in1, in2);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_empty () {
  // SCM_DEFER_INTS;
  widget out= empty_widget ();
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_text (SCM arg1, SCM arg2, SCM arg3, SCM arg4) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "widget-text");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "widget-text");
  SCM_ASSERT_INT (arg3, SCM_ARG3, "widget-text");
  SCM_ASSERT_BOOL (arg4, SCM_ARG4, "widget-text");

  string in1= scm_to_string (arg1);
  int in2= scm_to_int (arg2);
  int in3= scm_to_int (arg3);
  bool in4= scm_to_bool (arg4);

  // SCM_DEFER_INTS;
  widget out= text_widget (in1, in2, in3, in4);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_input (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5) {
  SCM_ASSERT_COMMAND (arg1, SCM_ARG1, "widget-input");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "widget-input");
  SCM_ASSERT_ARRAY_STRING (arg3, SCM_ARG3, "widget-input");
  SCM_ASSERT_INT (arg4, SCM_ARG4, "widget-input");
  SCM_ASSERT_STRING (arg5, SCM_ARG5, "widget-input");

  command in1= scm_to_command (arg1);
  string in2= scm_to_string (arg2);
  array_string in3= scm_to_array_string (arg3);
  int in4= scm_to_int (arg4);
  string in5= scm_to_string (arg5);

  // SCM_DEFER_INTS;
  widget out= input_text_widget (in1, in2, in3, in4, in5);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_enum (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5) {
  SCM_ASSERT_COMMAND (arg1, SCM_ARG1, "widget-enum");
  SCM_ASSERT_ARRAY_STRING (arg2, SCM_ARG2, "widget-enum");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "widget-enum");
  SCM_ASSERT_INT (arg4, SCM_ARG4, "widget-enum");
  SCM_ASSERT_STRING (arg5, SCM_ARG5, "widget-enum");

  command in1= scm_to_command (arg1);
  array_string in2= scm_to_array_string (arg2);
  string in3= scm_to_string (arg3);
  int in4= scm_to_int (arg4);
  string in5= scm_to_string (arg5);

  // SCM_DEFER_INTS;
  widget out= enum_widget (in1, in2, in3, in4, in5);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_choice (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_COMMAND (arg1, SCM_ARG1, "widget-choice");
  SCM_ASSERT_ARRAY_STRING (arg2, SCM_ARG2, "widget-choice");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "widget-choice");

  command in1= scm_to_command (arg1);
  array_string in2= scm_to_array_string (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  widget out= choice_widget (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_choices (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_COMMAND (arg1, SCM_ARG1, "widget-choices");
  SCM_ASSERT_ARRAY_STRING (arg2, SCM_ARG2, "widget-choices");
  SCM_ASSERT_ARRAY_STRING (arg3, SCM_ARG3, "widget-choices");

  command in1= scm_to_command (arg1);
  array_string in2= scm_to_array_string (arg2);
  array_string in3= scm_to_array_string (arg3);

  // SCM_DEFER_INTS;
  widget out= choice_widget (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_xpm (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "widget-xpm");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  widget out= xpm_widget (in1);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_box (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "widget-box");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "widget-box");
  SCM_ASSERT_INT (arg3, SCM_ARG3, "widget-box");
  SCM_ASSERT_BOOL (arg4, SCM_ARG4, "widget-box");
  SCM_ASSERT_BOOL (arg5, SCM_ARG5, "widget-box");

  scheme_tree in1= scm_to_scheme_tree (arg1);
  string in2= scm_to_string (arg2);
  int in3= scm_to_int (arg3);
  bool in4= scm_to_bool (arg4);
  bool in5= scm_to_bool (arg5);

  // SCM_DEFER_INTS;
  widget out= box_widget (in1, in2, in3, in4, in5);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_glue (SCM arg1, SCM arg2, SCM arg3, SCM arg4) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "widget-glue");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "widget-glue");
  SCM_ASSERT_INT (arg3, SCM_ARG3, "widget-glue");
  SCM_ASSERT_INT (arg4, SCM_ARG4, "widget-glue");

  bool in1= scm_to_bool (arg1);
  bool in2= scm_to_bool (arg2);
  int in3= scm_to_int (arg3);
  int in4= scm_to_int (arg4);

  // SCM_DEFER_INTS;
  widget out= glue_widget (in1, in2, in3, in4);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_color (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "widget-color");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "widget-color");
  SCM_ASSERT_BOOL (arg3, SCM_ARG3, "widget-color");
  SCM_ASSERT_INT (arg4, SCM_ARG4, "widget-color");
  SCM_ASSERT_INT (arg5, SCM_ARG5, "widget-color");

  content in1= scm_to_content (arg1);
  bool in2= scm_to_bool (arg2);
  bool in3= scm_to_bool (arg3);
  int in4= scm_to_int (arg4);
  int in5= scm_to_int (arg5);

  // SCM_DEFER_INTS;
  widget out= glue_widget (in1, in2, in3, in4, in5);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_hlist (SCM arg1) {
  SCM_ASSERT_ARRAY_WIDGET (arg1, SCM_ARG1, "widget-hlist");

  array_widget in1= scm_to_array_widget (arg1);

  // SCM_DEFER_INTS;
  widget out= horizontal_list (in1);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_vlist (SCM arg1) {
  SCM_ASSERT_ARRAY_WIDGET (arg1, SCM_ARG1, "widget-vlist");

  array_widget in1= scm_to_array_widget (arg1);

  // SCM_DEFER_INTS;
  widget out= vertical_list (in1);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_aligned (SCM arg1, SCM arg2) {
  SCM_ASSERT_ARRAY_WIDGET (arg1, SCM_ARG1, "widget-aligned");
  SCM_ASSERT_ARRAY_WIDGET (arg2, SCM_ARG2, "widget-aligned");

  array_widget in1= scm_to_array_widget (arg1);
  array_widget in2= scm_to_array_widget (arg2);

  // SCM_DEFER_INTS;
  widget out= aligned_widget (in1, in2);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_tabs (SCM arg1, SCM arg2) {
  SCM_ASSERT_ARRAY_WIDGET (arg1, SCM_ARG1, "widget-tabs");
  SCM_ASSERT_ARRAY_WIDGET (arg2, SCM_ARG2, "widget-tabs");

  array_widget in1= scm_to_array_widget (arg1);
  array_widget in2= scm_to_array_widget (arg2);

  // SCM_DEFER_INTS;
  widget out= tabs_widget (in1, in2);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_scrollable (SCM arg1, SCM arg2) {
  SCM_ASSERT_WIDGET (arg1, SCM_ARG1, "widget-scrollable");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "widget-scrollable");

  widget in1= scm_to_widget (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  widget out= user_canvas_widget (in1, in2);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_resize (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7, SCM arg8) {
  SCM_ASSERT_WIDGET (arg1, SCM_ARG1, "widget-resize");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "widget-resize");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "widget-resize");
  SCM_ASSERT_STRING (arg4, SCM_ARG4, "widget-resize");
  SCM_ASSERT_STRING (arg5, SCM_ARG5, "widget-resize");
  SCM_ASSERT_STRING (arg6, SCM_ARG6, "widget-resize");
  SCM_ASSERT_STRING (arg7, SCM_ARG7, "widget-resize");
  SCM_ASSERT_STRING (arg8, SCM_ARG8, "widget-resize");

  widget in1= scm_to_widget (arg1);
  int in2= scm_to_int (arg2);
  string in3= scm_to_string (arg3);
  string in4= scm_to_string (arg4);
  string in5= scm_to_string (arg5);
  string in6= scm_to_string (arg6);
  string in7= scm_to_string (arg7);
  string in8= scm_to_string (arg8);

  // SCM_DEFER_INTS;
  widget out= resize_widget (in1, in2, in3, in4, in5, in6, in7, in8);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_hsplit (SCM arg1, SCM arg2) {
  SCM_ASSERT_WIDGET (arg1, SCM_ARG1, "widget-hsplit");
  SCM_ASSERT_WIDGET (arg2, SCM_ARG2, "widget-hsplit");

  widget in1= scm_to_widget (arg1);
  widget in2= scm_to_widget (arg2);

  // SCM_DEFER_INTS;
  widget out= hsplit_widget (in1, in2);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_vsplit (SCM arg1, SCM arg2) {
  SCM_ASSERT_WIDGET (arg1, SCM_ARG1, "widget-vsplit");
  SCM_ASSERT_WIDGET (arg2, SCM_ARG2, "widget-vsplit");

  widget in1= scm_to_widget (arg1);
  widget in2= scm_to_widget (arg2);

  // SCM_DEFER_INTS;
  widget out= vsplit_widget (in1, in2);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_texmacs_output (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "widget-texmacs-output");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  widget out= texmacs_output_widget (in1);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_texmacs_input (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "widget-texmacs-input");
  SCM_ASSERT_COMMAND (arg2, SCM_ARG2, "widget-texmacs-input");
  SCM_ASSERT_BOOL (arg3, SCM_ARG3, "widget-texmacs-input");

  content in1= scm_to_content (arg1);
  command in2= scm_to_command (arg2);
  bool in3= scm_to_bool (arg3);

  // SCM_DEFER_INTS;
  widget out= texmacs_input_widget (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_ink (SCM arg1) {
  SCM_ASSERT_COMMAND (arg1, SCM_ARG1, "widget-ink");

  command in1= scm_to_command (arg1);

  // SCM_DEFER_INTS;
  widget out= ink_widget (in1);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_refresh (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "widget-refresh");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  widget out= refresh_widget (in1);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_object_2promise_widget (SCM arg1) {
  SCM_ASSERT_OBJECT (arg1, SCM_ARG1, "object->promise-widget");

  object in1= scm_to_object (arg1);

  // SCM_DEFER_INTS;
  promise_widget out= as_promise_widget (in1);
  // SCM_ALLOW_INTS;

  return promise_widget_to_scm (out);
}

SCM
tmg_tree_bounding_rectangle (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-bounding-rectangle");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  array_int out= get_bounding_rectangle (in1);
  // SCM_ALLOW_INTS;

  return array_int_to_scm (out);
}

SCM
tmg_show_balloon (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_WIDGET (arg1, SCM_ARG1, "show-balloon");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "show-balloon");
  SCM_ASSERT_INT (arg3, SCM_ARG3, "show-balloon");

  widget in1= scm_to_widget (arg1);
  int in2= scm_to_int (arg2);
  int in3= scm_to_int (arg3);

  // SCM_DEFER_INTS;
  show_help_balloon (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_window_handle () {
  // SCM_DEFER_INTS;
  int out= window_handle ();
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_window_create (SCM arg1, SCM arg2, SCM arg3, SCM arg4) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "window-create");
  SCM_ASSERT_WIDGET (arg2, SCM_ARG2, "window-create");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "window-create");
  SCM_ASSERT_BOOL (arg4, SCM_ARG4, "window-create");

  int in1= scm_to_int (arg1);
  widget in2= scm_to_widget (arg2);
  string in3= scm_to_string (arg3);
  bool in4= scm_to_bool (arg4);

  // SCM_DEFER_INTS;
  window_create (in1, in2, in3, in4);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_window_create_quit (SCM arg1, SCM arg2, SCM arg3, SCM arg4) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "window-create-quit");
  SCM_ASSERT_WIDGET (arg2, SCM_ARG2, "window-create-quit");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "window-create-quit");
  SCM_ASSERT_COMMAND (arg4, SCM_ARG4, "window-create-quit");

  int in1= scm_to_int (arg1);
  widget in2= scm_to_widget (arg2);
  string in3= scm_to_string (arg3);
  command in4= scm_to_command (arg4);

  // SCM_DEFER_INTS;
  window_create (in1, in2, in3, in4);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_window_delete (SCM arg1) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "window-delete");

  int in1= scm_to_int (arg1);

  // SCM_DEFER_INTS;
  window_delete (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_window_show (SCM arg1) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "window-show");

  int in1= scm_to_int (arg1);

  // SCM_DEFER_INTS;
  window_show (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_window_hide (SCM arg1) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "window-hide");

  int in1= scm_to_int (arg1);

  // SCM_DEFER_INTS;
  window_hide (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_bib_add_period (SCM arg1) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "bib-add-period");

  scheme_tree in1= scm_to_scheme_tree (arg1);

  // SCM_DEFER_INTS;
  scheme_tree out= bib_add_period (in1);
  // SCM_ALLOW_INTS;

  return scheme_tree_to_scm (out);
}

SCM
tmg_bib_upcase_first (SCM arg1) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "bib-upcase-first");

  scheme_tree in1= scm_to_scheme_tree (arg1);

  // SCM_DEFER_INTS;
  scheme_tree out= bib_upcase_first (in1);
  // SCM_ALLOW_INTS;

  return scheme_tree_to_scm (out);
}

SCM
tmg_bib_locase (SCM arg1) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "bib-locase");

  scheme_tree in1= scm_to_scheme_tree (arg1);

  // SCM_DEFER_INTS;
  scheme_tree out= bib_locase (in1);
  // SCM_ALLOW_INTS;

  return scheme_tree_to_scm (out);
}

SCM
tmg_bib_upcase (SCM arg1) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "bib-upcase");

  scheme_tree in1= scm_to_scheme_tree (arg1);

  // SCM_DEFER_INTS;
  scheme_tree out= bib_upcase (in1);
  // SCM_ALLOW_INTS;

  return scheme_tree_to_scm (out);
}

SCM
tmg_bib_default (SCM arg1) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "bib-default");

  scheme_tree in1= scm_to_scheme_tree (arg1);

  // SCM_DEFER_INTS;
  scheme_tree out= bib_default (in1);
  // SCM_ALLOW_INTS;

  return scheme_tree_to_scm (out);
}

SCM
tmg_bib_purify (SCM arg1) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "bib-purify");

  scheme_tree in1= scm_to_scheme_tree (arg1);

  // SCM_DEFER_INTS;
  string out= bib_purify (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_bib_text_length (SCM arg1) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "bib-text-length");

  scheme_tree in1= scm_to_scheme_tree (arg1);

  // SCM_DEFER_INTS;
  int out= bib_text_length (in1);
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_bib_prefix (SCM arg1, SCM arg2) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "bib-prefix");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "bib-prefix");

  scheme_tree in1= scm_to_scheme_tree (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  string out= bib_prefix (in1, in2);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_bib_emptyP (SCM arg1, SCM arg2) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "bib-empty?");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "bib-empty?");

  scheme_tree in1= scm_to_scheme_tree (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  bool out= bib_empty (in1, in2);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_bib_field (SCM arg1, SCM arg2) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "bib-field");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "bib-field");

  scheme_tree in1= scm_to_scheme_tree (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  scheme_tree out= bib_field (in1, in2);
  // SCM_ALLOW_INTS;

  return scheme_tree_to_scm (out);
}

SCM
tmg_bib_abbreviate (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "bib-abbreviate");
  SCM_ASSERT_SCHEME_TREE (arg2, SCM_ARG2, "bib-abbreviate");
  SCM_ASSERT_SCHEME_TREE (arg3, SCM_ARG3, "bib-abbreviate");

  scheme_tree in1= scm_to_scheme_tree (arg1);
  scheme_tree in2= scm_to_scheme_tree (arg2);
  scheme_tree in3= scm_to_scheme_tree (arg3);

  // SCM_DEFER_INTS;
  scheme_tree out= bib_abbreviate (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return scheme_tree_to_scm (out);
}

void
initialize_glue_basic () {
  scm_install_procedure ("texmacs-version-release",  tmg_texmacs_version_release, 1, 0, 0);
  scm_install_procedure ("version-before?",  tmg_version_beforeP, 2, 0, 0);
  scm_install_procedure ("os-win32?",  tmg_os_win32P, 0, 0, 0);
  scm_install_procedure ("os-mingw?",  tmg_os_mingwP, 0, 0, 0);
  scm_install_procedure ("os-macos?",  tmg_os_macosP, 0, 0, 0);
  scm_install_procedure ("x-gui?",  tmg_x_guiP, 0, 0, 0);
  scm_install_procedure ("qt-gui?",  tmg_qt_guiP, 0, 0, 0);
  scm_install_procedure ("default-look-and-feel",  tmg_default_look_and_feel, 0, 0, 0);
  scm_install_procedure ("default-chinese-font",  tmg_default_chinese_font, 0, 0, 0);
  scm_install_procedure ("default-japanese-font",  tmg_default_japanese_font, 0, 0, 0);
  scm_install_procedure ("default-korean-font",  tmg_default_korean_font, 0, 0, 0);
  scm_install_procedure ("tm-output",  tmg_tm_output, 1, 0, 0);
  scm_install_procedure ("tm-errput",  tmg_tm_errput, 1, 0, 0);
  scm_install_procedure ("win32-display",  tmg_win32_display, 1, 0, 0);
  scm_install_procedure ("cpp-error",  tmg_cpp_error, 0, 0, 0);
  scm_install_procedure ("scheme-dialect",  tmg_scheme_dialect, 0, 0, 0);
  scm_install_procedure ("get-texmacs-path",  tmg_get_texmacs_path, 0, 0, 0);
  scm_install_procedure ("plugin-list",  tmg_plugin_list, 0, 0, 0);
  scm_install_procedure ("set-fast-environments",  tmg_set_fast_environments, 1, 0, 0);
  scm_install_procedure ("font-exists-in-tt?",  tmg_font_exists_in_ttP, 1, 0, 0);
  scm_install_procedure ("eval-system",  tmg_eval_system, 1, 0, 0);
  scm_install_procedure ("var-eval-system",  tmg_var_eval_system, 1, 0, 0);
  scm_install_procedure ("get-locale-language",  tmg_get_locale_language, 0, 0, 0);
  scm_install_procedure ("texmacs-time",  tmg_texmacs_time, 0, 0, 0);
  scm_install_procedure ("texmacs-memory",  tmg_texmacs_memory, 0, 0, 0);
  scm_install_procedure ("bench-print",  tmg_bench_print, 1, 0, 0);
  scm_install_procedure ("bench-print-all",  tmg_bench_print_all, 0, 0, 0);
  scm_install_procedure ("system-wait",  tmg_system_wait, 2, 0, 0);
  scm_install_procedure ("set-bibtex-command",  tmg_set_bibtex_command, 1, 0, 0);
  scm_install_procedure ("math-symbol-group",  tmg_math_symbol_group, 1, 0, 0);
  scm_install_procedure ("math-group-members",  tmg_math_group_members, 1, 0, 0);
  scm_install_procedure ("math-symbol-type",  tmg_math_symbol_type, 1, 0, 0);
  scm_install_procedure ("object->command",  tmg_object_2command, 1, 0, 0);
  scm_install_procedure ("exec-delayed",  tmg_exec_delayed, 1, 0, 0);
  scm_install_procedure ("exec-delayed-pause",  tmg_exec_delayed_pause, 1, 0, 0);
  scm_install_procedure ("notify-preferences-loaded",  tmg_notify_preferences_loaded, 0, 0, 0);
  scm_install_procedure ("set-input-language",  tmg_set_input_language, 1, 0, 0);
  scm_install_procedure ("get-input-language",  tmg_get_input_language, 0, 0, 0);
  scm_install_procedure ("set-output-language",  tmg_set_output_language, 1, 0, 0);
  scm_install_procedure ("get-output-language",  tmg_get_output_language, 0, 0, 0);
  scm_install_procedure ("translate",  tmg_translate, 1, 0, 0);
  scm_install_procedure ("translate-from-to",  tmg_translate_from_to, 3, 0, 0);
  scm_install_procedure ("tree-translate",  tmg_tree_translate, 1, 0, 0);
  scm_install_procedure ("tree-translate-from-to",  tmg_tree_translate_from_to, 3, 0, 0);
  scm_install_procedure ("color",  tmg_color, 1, 0, 0);
  scm_install_procedure ("new-author",  tmg_new_author, 0, 0, 0);
  scm_install_procedure ("set-author",  tmg_set_author, 1, 0, 0);
  scm_install_procedure ("get-author",  tmg_get_author, 0, 0, 0);
  scm_install_procedure ("debug-set",  tmg_debug_set, 2, 0, 0);
  scm_install_procedure ("debug-get",  tmg_debug_get, 1, 0, 0);
  scm_install_procedure ("cout-buffer",  tmg_cout_buffer, 0, 0, 0);
  scm_install_procedure ("cout-unbuffer",  tmg_cout_unbuffer, 0, 0, 0);
  scm_install_procedure ("mark-new",  tmg_mark_new, 0, 0, 0);
  scm_install_procedure ("glyph-register",  tmg_glyph_register, 2, 0, 0);
  scm_install_procedure ("glyph-recognize",  tmg_glyph_recognize, 1, 0, 0);
  scm_install_procedure ("image->psdoc",  tmg_image_2psdoc, 1, 0, 0);
  scm_install_procedure ("tree->stree",  tmg_tree_2stree, 1, 0, 0);
  scm_install_procedure ("stree->tree",  tmg_stree_2tree, 1, 0, 0);
  scm_install_procedure ("tree->string",  tmg_tree_2string, 1, 0, 0);
  scm_install_procedure ("string->tree",  tmg_string_2tree, 1, 0, 0);
  scm_install_procedure ("tm->tree",  tmg_tm_2tree, 1, 0, 0);
  scm_install_procedure ("tree-atomic?",  tmg_tree_atomicP, 1, 0, 0);
  scm_install_procedure ("tree-compound?",  tmg_tree_compoundP, 1, 0, 0);
  scm_install_procedure ("tree-label",  tmg_tree_label, 1, 0, 0);
  scm_install_procedure ("tree-children",  tmg_tree_children, 1, 0, 0);
  scm_install_procedure ("tree-arity",  tmg_tree_arity, 1, 0, 0);
  scm_install_procedure ("tree-child-ref",  tmg_tree_child_ref, 2, 0, 0);
  scm_install_procedure ("tree-child-set!",  tmg_tree_child_setS, 3, 0, 0);
  scm_install_procedure ("tree-child-insert",  tmg_tree_child_insert, 3, 0, 0);
  scm_install_procedure ("tree-ip",  tmg_tree_ip, 1, 0, 0);
  scm_install_procedure ("tree-active?",  tmg_tree_activeP, 1, 0, 0);
  scm_install_procedure ("tree-eq?",  tmg_tree_eqP, 2, 0, 0);
  scm_install_procedure ("subtree",  tmg_subtree, 2, 0, 0);
  scm_install_procedure ("tree-range",  tmg_tree_range, 3, 0, 0);
  scm_install_procedure ("tree-copy",  tmg_tree_copy, 1, 0, 0);
  scm_install_procedure ("tree-append",  tmg_tree_append, 2, 0, 0);
  scm_install_procedure ("tree-right-index",  tmg_tree_right_index, 1, 0, 0);
  scm_install_procedure ("tree-label-extension?",  tmg_tree_label_extensionP, 1, 0, 0);
  scm_install_procedure ("tree-multi-paragraph?",  tmg_tree_multi_paragraphP, 1, 0, 0);
  scm_install_procedure ("tree-simplify",  tmg_tree_simplify, 1, 0, 0);
  scm_install_procedure ("tree-minimal-arity",  tmg_tree_minimal_arity, 1, 0, 0);
  scm_install_procedure ("tree-maximal-arity",  tmg_tree_maximal_arity, 1, 0, 0);
  scm_install_procedure ("tree-possible-arity?",  tmg_tree_possible_arityP, 2, 0, 0);
  scm_install_procedure ("tree-insert_point",  tmg_tree_insert_point, 2, 0, 0);
  scm_install_procedure ("tree-is-dynamic?",  tmg_tree_is_dynamicP, 1, 0, 0);
  scm_install_procedure ("tree-accessible-child?",  tmg_tree_accessible_childP, 2, 0, 0);
  scm_install_procedure ("tree-accessible-children",  tmg_tree_accessible_children, 1, 0, 0);
  scm_install_procedure ("tree-all-accessible?",  tmg_tree_all_accessibleP, 1, 0, 0);
  scm_install_procedure ("tree-none-accessible?",  tmg_tree_none_accessibleP, 1, 0, 0);
  scm_install_procedure ("tree-name",  tmg_tree_name, 1, 0, 0);
  scm_install_procedure ("tree-long-name",  tmg_tree_long_name, 1, 0, 0);
  scm_install_procedure ("tree-child-name",  tmg_tree_child_name, 2, 0, 0);
  scm_install_procedure ("tree-child-long-name",  tmg_tree_child_long_name, 2, 0, 0);
  scm_install_procedure ("tree-child-type",  tmg_tree_child_type, 2, 0, 0);
  scm_install_procedure ("tree-load-inclusion",  tmg_tree_load_inclusion, 1, 0, 0);
  scm_install_procedure ("tree-as-string",  tmg_tree_as_string, 1, 0, 0);
  scm_install_procedure ("tree-extents",  tmg_tree_extents, 1, 0, 0);
  scm_install_procedure ("tree-empty?",  tmg_tree_emptyP, 1, 0, 0);
  scm_install_procedure ("tree-is-buffer?",  tmg_tree_is_bufferP, 1, 0, 0);
  scm_install_procedure ("tree-search-sections",  tmg_tree_search_sections, 1, 0, 0);
  scm_install_procedure ("tree-assign",  tmg_tree_assign, 2, 0, 0);
  scm_install_procedure ("tree-var-insert",  tmg_tree_var_insert, 3, 0, 0);
  scm_install_procedure ("tree-remove",  tmg_tree_remove, 3, 0, 0);
  scm_install_procedure ("tree-split",  tmg_tree_split, 3, 0, 0);
  scm_install_procedure ("tree-join",  tmg_tree_join, 2, 0, 0);
  scm_install_procedure ("tree-assign-node",  tmg_tree_assign_node, 2, 0, 0);
  scm_install_procedure ("tree-insert-node",  tmg_tree_insert_node, 3, 0, 0);
  scm_install_procedure ("tree-remove-node",  tmg_tree_remove_node, 2, 0, 0);
  scm_install_procedure ("cpp-tree-correct-node",  tmg_cpp_tree_correct_node, 1, 0, 0);
  scm_install_procedure ("cpp-tree-correct-downwards",  tmg_cpp_tree_correct_downwards, 1, 0, 0);
  scm_install_procedure ("cpp-tree-correct-upwards",  tmg_cpp_tree_correct_upwards, 1, 0, 0);
  scm_install_procedure ("concat-tokenize-math",  tmg_concat_tokenize_math, 1, 0, 0);
  scm_install_procedure ("concat-decompose",  tmg_concat_decompose, 1, 0, 0);
  scm_install_procedure ("concat-recompose",  tmg_concat_recompose, 1, 0, 0);
  scm_install_procedure ("with-like?",  tmg_with_likeP, 1, 0, 0);
  scm_install_procedure ("with-same-type?",  tmg_with_same_typeP, 2, 0, 0);
  scm_install_procedure ("with-similar-type?",  tmg_with_similar_typeP, 2, 0, 0);
  scm_install_procedure ("with-correct",  tmg_with_correct, 1, 0, 0);
  scm_install_procedure ("with-correct-superfluous",  tmg_with_correct_superfluous, 1, 0, 0);
  scm_install_procedure ("invisible-correct-superfluous",  tmg_invisible_correct_superfluous, 1, 0, 0);
  scm_install_procedure ("invisible-correct-missing",  tmg_invisible_correct_missing, 2, 0, 0);
  scm_install_procedure ("automatic-correct",  tmg_automatic_correct, 2, 0, 0);
  scm_install_procedure ("manual-correct",  tmg_manual_correct, 1, 0, 0);
  scm_install_procedure ("tree-upgrade-brackets",  tmg_tree_upgrade_brackets, 2, 0, 0);
  scm_install_procedure ("tree-upgrade-big",  tmg_tree_upgrade_big, 1, 0, 0);
  scm_install_procedure ("tree-downgrade-brackets",  tmg_tree_downgrade_brackets, 3, 0, 0);
  scm_install_procedure ("tree-downgrade-big",  tmg_tree_downgrade_big, 1, 0, 0);
  scm_install_procedure ("math-status-print",  tmg_math_status_print, 0, 0, 0);
  scm_install_procedure ("math-status-reset",  tmg_math_status_reset, 0, 0, 0);
  scm_install_procedure ("path-inf?",  tmg_path_infP, 2, 0, 0);
  scm_install_procedure ("path-inf-eq?",  tmg_path_inf_eqP, 2, 0, 0);
  scm_install_procedure ("path-less?",  tmg_path_lessP, 2, 0, 0);
  scm_install_procedure ("path-less-eq?",  tmg_path_less_eqP, 2, 0, 0);
  scm_install_procedure ("path-start",  tmg_path_start, 2, 0, 0);
  scm_install_procedure ("path-end",  tmg_path_end, 2, 0, 0);
  scm_install_procedure ("path-next",  tmg_path_next, 2, 0, 0);
  scm_install_procedure ("path-previous",  tmg_path_previous, 2, 0, 0);
  scm_install_procedure ("path-next-word",  tmg_path_next_word, 2, 0, 0);
  scm_install_procedure ("path-previous-word",  tmg_path_previous_word, 2, 0, 0);
  scm_install_procedure ("path-next-node",  tmg_path_next_node, 2, 0, 0);
  scm_install_procedure ("path-previous-node",  tmg_path_previous_node, 2, 0, 0);
  scm_install_procedure ("path-next-tag",  tmg_path_next_tag, 3, 0, 0);
  scm_install_procedure ("path-previous-tag",  tmg_path_previous_tag, 3, 0, 0);
  scm_install_procedure ("path-next-tag-same-argument",  tmg_path_next_tag_same_argument, 3, 0, 0);
  scm_install_procedure ("path-previous-tag-same-argument",  tmg_path_previous_tag_same_argument, 3, 0, 0);
  scm_install_procedure ("path-next-argument",  tmg_path_next_argument, 2, 0, 0);
  scm_install_procedure ("path-previous-argument",  tmg_path_previous_argument, 2, 0, 0);
  scm_install_procedure ("path-previous-section",  tmg_path_previous_section, 2, 0, 0);
  scm_install_procedure ("tree->ids",  tmg_tree_2ids, 1, 0, 0);
  scm_install_procedure ("id->trees",  tmg_id_2trees, 1, 0, 0);
  scm_install_procedure ("vertex->links",  tmg_vertex_2links, 1, 0, 0);
  scm_install_procedure ("tree->tree-pointer",  tmg_tree_2tree_pointer, 1, 0, 0);
  scm_install_procedure ("tree-pointer-detach",  tmg_tree_pointer_detach, 1, 0, 0);
  scm_install_procedure ("tree-pointer->tree",  tmg_tree_pointer_2tree, 1, 0, 0);
  scm_install_procedure ("current-link-types",  tmg_current_link_types, 0, 0, 0);
  scm_install_procedure ("get-locus-rendering",  tmg_get_locus_rendering, 1, 0, 0);
  scm_install_procedure ("set-locus-rendering",  tmg_set_locus_rendering, 2, 0, 0);
  scm_install_procedure ("declare-visited",  tmg_declare_visited, 1, 0, 0);
  scm_install_procedure ("has-been-visited?",  tmg_has_been_visitedP, 1, 0, 0);
  scm_install_procedure ("graphics-set",  tmg_graphics_set, 2, 0, 0);
  scm_install_procedure ("graphics-has?",  tmg_graphics_hasP, 1, 0, 0);
  scm_install_procedure ("graphics-ref",  tmg_graphics_ref, 1, 0, 0);
  scm_install_procedure ("graphics-needs-update?",  tmg_graphics_needs_updateP, 0, 0, 0);
  scm_install_procedure ("graphics-notify-update",  tmg_graphics_notify_update, 1, 0, 0);
  scm_install_procedure ("string-number?",  tmg_string_numberP, 1, 0, 0);
  scm_install_procedure ("string-occurs?",  tmg_string_occursP, 2, 0, 0);
  scm_install_procedure ("string-search-forwards",  tmg_string_search_forwards, 3, 0, 0);
  scm_install_procedure ("string-search-backwards",  tmg_string_search_backwards, 3, 0, 0);
  scm_install_procedure ("string-replace",  tmg_string_replace, 3, 0, 0);
  scm_install_procedure ("string-alpha?",  tmg_string_alphaP, 1, 0, 0);
  scm_install_procedure ("string-locase-alpha?",  tmg_string_locase_alphaP, 1, 0, 0);
  scm_install_procedure ("upcase-first",  tmg_upcase_first, 1, 0, 0);
  scm_install_procedure ("locase-first",  tmg_locase_first, 1, 0, 0);
  scm_install_procedure ("upcase-all",  tmg_upcase_all, 1, 0, 0);
  scm_install_procedure ("locase-all",  tmg_locase_all, 1, 0, 0);
  scm_install_procedure ("string-union",  tmg_string_union, 2, 0, 0);
  scm_install_procedure ("string-minus",  tmg_string_minus, 2, 0, 0);
  scm_install_procedure ("escape-generic",  tmg_escape_generic, 1, 0, 0);
  scm_install_procedure ("escape-verbatim",  tmg_escape_verbatim, 1, 0, 0);
  scm_install_procedure ("escape-shell",  tmg_escape_shell, 1, 0, 0);
  scm_install_procedure ("string-convert",  tmg_string_convert, 3, 0, 0);
  scm_install_procedure ("utf8->cork",  tmg_utf8_2cork, 1, 0, 0);
  scm_install_procedure ("cork->utf8",  tmg_cork_2utf8, 1, 0, 0);
  scm_install_procedure ("utf8->html",  tmg_utf8_2html, 1, 0, 0);
  scm_install_procedure ("tm->xml-name",  tmg_tm_2xml_name, 1, 0, 0);
  scm_install_procedure ("old-tm->xml-cdata",  tmg_old_tm_2xml_cdata, 1, 0, 0);
  scm_install_procedure ("tm->xml-cdata",  tmg_tm_2xml_cdata, 1, 0, 0);
  scm_install_procedure ("xml-name->tm",  tmg_xml_name_2tm, 1, 0, 0);
  scm_install_procedure ("old-xml-cdata->tm",  tmg_old_xml_cdata_2tm, 1, 0, 0);
  scm_install_procedure ("xml-unspace",  tmg_xml_unspace, 3, 0, 0);
  scm_install_procedure ("integer->hexadecimal",  tmg_integer_2hexadecimal, 1, 0, 0);
  scm_install_procedure ("integer->padded-hexadecimal",  tmg_integer_2padded_hexadecimal, 2, 0, 0);
  scm_install_procedure ("hexadecimal->integer",  tmg_hexadecimal_2integer, 1, 0, 0);
  scm_install_procedure ("string->tmstring",  tmg_string_2tmstring, 1, 0, 0);
  scm_install_procedure ("tmstring->string",  tmg_tmstring_2string, 1, 0, 0);
  scm_install_procedure ("tmstring-length",  tmg_tmstring_length, 1, 0, 0);
  scm_install_procedure ("tmstring-ref",  tmg_tmstring_ref, 2, 0, 0);
  scm_install_procedure ("tmstring-reverse-ref",  tmg_tmstring_reverse_ref, 2, 0, 0);
  scm_install_procedure ("tmstring->list",  tmg_tmstring_2list, 1, 0, 0);
  scm_install_procedure ("list->tmstring",  tmg_list_2tmstring, 1, 0, 0);
  scm_install_procedure ("string-next",  tmg_string_next, 2, 0, 0);
  scm_install_procedure ("string-previous",  tmg_string_previous, 2, 0, 0);
  scm_install_procedure ("packrat-define",  tmg_packrat_define, 3, 0, 0);
  scm_install_procedure ("packrat-property",  tmg_packrat_property, 4, 0, 0);
  scm_install_procedure ("packrat-inherit",  tmg_packrat_inherit, 2, 0, 0);
  scm_install_procedure ("packrat-parse",  tmg_packrat_parse, 3, 0, 0);
  scm_install_procedure ("packrat-correct?",  tmg_packrat_correctP, 3, 0, 0);
  scm_install_procedure ("packrat-context",  tmg_packrat_context, 4, 0, 0);
  scm_install_procedure ("parse-texmacs",  tmg_parse_texmacs, 1, 0, 0);
  scm_install_procedure ("serialize-texmacs",  tmg_serialize_texmacs, 1, 0, 0);
  scm_install_procedure ("parse-texmacs-snippet",  tmg_parse_texmacs_snippet, 1, 0, 0);
  scm_install_procedure ("serialize-texmacs-snippet",  tmg_serialize_texmacs_snippet, 1, 0, 0);
  scm_install_procedure ("texmacs->stm",  tmg_texmacs_2stm, 1, 0, 0);
  scm_install_procedure ("stm->texmacs",  tmg_stm_2texmacs, 1, 0, 0);
  scm_install_procedure ("stm-snippet->texmacs",  tmg_stm_snippet_2texmacs, 1, 0, 0);
  scm_install_procedure ("cpp-texmacs->verbatim",  tmg_cpp_texmacs_2verbatim, 3, 0, 0);
  scm_install_procedure ("cpp-verbatim-snippet->texmacs",  tmg_cpp_verbatim_snippet_2texmacs, 3, 0, 0);
  scm_install_procedure ("cpp-verbatim->texmacs",  tmg_cpp_verbatim_2texmacs, 3, 0, 0);
  scm_install_procedure ("parse-latex",  tmg_parse_latex, 1, 0, 0);
  scm_install_procedure ("parse-latex-document",  tmg_parse_latex_document, 1, 0, 0);
  scm_install_procedure ("latex->texmacs",  tmg_latex_2texmacs, 1, 0, 0);
  scm_install_procedure ("latex-document->texmacs",  tmg_latex_document_2texmacs, 1, 0, 0);
  scm_install_procedure ("latex-class-document->texmacs",  tmg_latex_class_document_2texmacs, 1, 0, 0);
  scm_install_procedure ("parse-xml",  tmg_parse_xml, 1, 0, 0);
  scm_install_procedure ("parse-html",  tmg_parse_html, 1, 0, 0);
  scm_install_procedure ("parse-bib",  tmg_parse_bib, 1, 0, 0);
  scm_install_procedure ("upgrade-tmml",  tmg_upgrade_tmml, 1, 0, 0);
  scm_install_procedure ("upgrade-mathml",  tmg_upgrade_mathml, 1, 0, 0);
  scm_install_procedure ("string->url",  tmg_string_2url, 1, 0, 0);
  scm_install_procedure ("url",  tmg_url, 2, 0, 0);
  scm_install_procedure ("url-system",  tmg_url_system, 1, 0, 0);
  scm_install_procedure ("url-none",  tmg_url_none, 0, 0, 0);
  scm_install_procedure ("url-any",  tmg_url_any, 0, 0, 0);
  scm_install_procedure ("url-wildcard",  tmg_url_wildcard, 1, 0, 0);
  scm_install_procedure ("url-parent",  tmg_url_parent, 0, 0, 0);
  scm_install_procedure ("url-ancestor",  tmg_url_ancestor, 0, 0, 0);
  scm_install_procedure ("url-append",  tmg_url_append, 2, 0, 0);
  scm_install_procedure ("url-or",  tmg_url_or, 2, 0, 0);
  scm_install_procedure ("url->string",  tmg_url_2string, 1, 0, 0);
  scm_install_procedure ("url-none?",  tmg_url_noneP, 1, 0, 0);
  scm_install_procedure ("url-rooted-web?",  tmg_url_rooted_webP, 1, 0, 0);
  scm_install_procedure ("url-concat?",  tmg_url_concatP, 1, 0, 0);
  scm_install_procedure ("url-or?",  tmg_url_orP, 1, 0, 0);
  scm_install_procedure ("url-ref",  tmg_url_ref, 2, 0, 0);
  scm_install_procedure ("url-head",  tmg_url_head, 1, 0, 0);
  scm_install_procedure ("url-tail",  tmg_url_tail, 1, 0, 0);
  scm_install_procedure ("url-suffix",  tmg_url_suffix, 1, 0, 0);
  scm_install_procedure ("url-glue",  tmg_url_glue, 2, 0, 0);
  scm_install_procedure ("url-unglue",  tmg_url_unglue, 2, 0, 0);
  scm_install_procedure ("url-relative",  tmg_url_relative, 2, 0, 0);
  scm_install_procedure ("url-expand",  tmg_url_expand, 1, 0, 0);
  scm_install_procedure ("url-factor",  tmg_url_factor, 1, 0, 0);
  scm_install_procedure ("url-delta",  tmg_url_delta, 2, 0, 0);
  scm_install_procedure ("url-secure?",  tmg_url_secureP, 1, 0, 0);
  scm_install_procedure ("url-descends?",  tmg_url_descendsP, 2, 0, 0);
  scm_install_procedure ("url-complete",  tmg_url_complete, 2, 0, 0);
  scm_install_procedure ("url-resolve",  tmg_url_resolve, 2, 0, 0);
  scm_install_procedure ("url-resolve-in-path",  tmg_url_resolve_in_path, 1, 0, 0);
  scm_install_procedure ("url-exists?",  tmg_url_existsP, 1, 0, 0);
  scm_install_procedure ("url-exists-in-path?",  tmg_url_exists_in_pathP, 1, 0, 0);
  scm_install_procedure ("url-exists-in-tex?",  tmg_url_exists_in_texP, 1, 0, 0);
  scm_install_procedure ("url-concretize",  tmg_url_concretize, 1, 0, 0);
  scm_install_procedure ("url-materialize",  tmg_url_materialize, 2, 0, 0);
  scm_install_procedure ("url-test?",  tmg_url_testP, 2, 0, 0);
  scm_install_procedure ("url-regular?",  tmg_url_regularP, 1, 0, 0);
  scm_install_procedure ("url-directory?",  tmg_url_directoryP, 1, 0, 0);
  scm_install_procedure ("url-link?",  tmg_url_linkP, 1, 0, 0);
  scm_install_procedure ("url-newer?",  tmg_url_newerP, 2, 0, 0);
  scm_install_procedure ("url-last-modified",  tmg_url_last_modified, 1, 0, 0);
  scm_install_procedure ("url-temp",  tmg_url_temp, 0, 0, 0);
  scm_install_procedure ("url-scratch",  tmg_url_scratch, 3, 0, 0);
  scm_install_procedure ("url-scratch?",  tmg_url_scratchP, 1, 0, 0);
  scm_install_procedure ("string-save",  tmg_string_save, 2, 0, 0);
  scm_install_procedure ("string-load",  tmg_string_load, 1, 0, 0);
  scm_install_procedure ("system-move",  tmg_system_move, 2, 0, 0);
  scm_install_procedure ("system-copy",  tmg_system_copy, 2, 0, 0);
  scm_install_procedure ("system-remove",  tmg_system_remove, 1, 0, 0);
  scm_install_procedure ("system-mkdir",  tmg_system_mkdir, 1, 0, 0);
  scm_install_procedure ("system-search-score",  tmg_system_search_score, 2, 0, 0);
  scm_install_procedure ("system-1",  tmg_system_1, 2, 0, 0);
  scm_install_procedure ("system-2",  tmg_system_2, 3, 0, 0);
  scm_install_procedure ("tmfs-set",  tmg_tmfs_set, 2, 0, 0);
  scm_install_procedure ("tmfs-reset",  tmg_tmfs_reset, 2, 0, 0);
  scm_install_procedure ("tmfs-get",  tmg_tmfs_get, 1, 0, 0);
  scm_install_procedure ("tmfs-new-save",  tmg_tmfs_new_save, 2, 0, 0);
  scm_install_procedure ("tmfs-new-remove",  tmg_tmfs_new_remove, 1, 0, 0);
  scm_install_procedure ("tmfs-new-load",  tmg_tmfs_new_load, 1, 0, 0);
  scm_install_procedure ("tmfs-create-ressource",  tmg_tmfs_create_ressource, 0, 0, 0);
  scm_install_procedure ("tmfs-ressource-head",  tmg_tmfs_ressource_head, 1, 0, 0);
  scm_install_procedure ("tmfs-ressource-versions",  tmg_tmfs_ressource_versions, 1, 0, 0);
  scm_install_procedure ("tmfs-save-ressource",  tmg_tmfs_save_ressource, 3, 0, 0);
  scm_install_procedure ("tmfs-load-ressource-file",  tmg_tmfs_load_ressource_file, 1, 0, 0);
  scm_install_procedure ("tmfs-load-ressource-properties",  tmg_tmfs_load_ressource_properties, 1, 0, 0);
  scm_install_procedure ("tmfs-create-user",  tmg_tmfs_create_user, 1, 0, 0);
  scm_install_procedure ("tmfs-search-user",  tmg_tmfs_search_user, 1, 0, 0);
  scm_install_procedure ("tmfs-set-user",  tmg_tmfs_set_user, 1, 0, 0);
  scm_install_procedure ("tmfs-get-user",  tmg_tmfs_get_user, 0, 0, 0);
  scm_install_procedure ("tmfs-allows?",  tmg_tmfs_allowsP, 2, 0, 0);
  scm_install_procedure ("tmfs-set-attributes",  tmg_tmfs_set_attributes, 2, 0, 0);
  scm_install_procedure ("tmfs-get-attributes",  tmg_tmfs_get_attributes, 1, 0, 0);
  scm_install_procedure ("tmfs-add-attributes",  tmg_tmfs_add_attributes, 2, 0, 0);
  scm_install_procedure ("tmfs-remove-attributes",  tmg_tmfs_remove_attributes, 2, 0, 0);
  scm_install_procedure ("tmfs-change-attributes",  tmg_tmfs_change_attributes, 2, 0, 0);
  scm_install_procedure ("tmfs-query",  tmg_tmfs_query, 1, 0, 0);
  scm_install_procedure ("solutions->collection",  tmg_solutions_2collection, 2, 0, 0);
  scm_install_procedure ("tmfs-create-file",  tmg_tmfs_create_file, 2, 0, 0);
  scm_install_procedure ("tmfs-create-file-in",  tmg_tmfs_create_file_in, 3, 0, 0);
  scm_install_procedure ("tmfs-search-file",  tmg_tmfs_search_file, 1, 0, 0);
  scm_install_procedure ("tmfs-save-file",  tmg_tmfs_save_file, 2, 0, 0);
  scm_install_procedure ("tmfs-load-file",  tmg_tmfs_load_file, 1, 0, 0);
  scm_install_procedure ("tmfs-create-project",  tmg_tmfs_create_project, 1, 0, 0);
  scm_install_procedure ("tmfs-search-project",  tmg_tmfs_search_project, 1, 0, 0);
  scm_install_procedure ("tmfs-get-file-projects",  tmg_tmfs_get_file_projects, 1, 0, 0);
  scm_install_procedure ("tmfs-get-project-files",  tmg_tmfs_get_project_files, 1, 0, 0);
  scm_install_procedure ("tmfs-create-branch",  tmg_tmfs_create_branch, 2, 0, 0);
  scm_install_procedure ("tmfs-set-root",  tmg_tmfs_set_root, 2, 0, 0);
  scm_install_procedure ("tmfs-get-root",  tmg_tmfs_get_root, 1, 0, 0);
  scm_install_procedure ("tmfs-import",  tmg_tmfs_import, 1, 0, 0);
  scm_install_procedure ("tmfs-export",  tmg_tmfs_export, 1, 0, 0);
  scm_install_procedure ("server-start",  tmg_server_start, 0, 0, 0);
  scm_install_procedure ("server-stop",  tmg_server_stop, 0, 0, 0);
  scm_install_procedure ("server-read",  tmg_server_read, 1, 0, 0);
  scm_install_procedure ("server-write",  tmg_server_write, 2, 0, 0);
  scm_install_procedure ("client-start",  tmg_client_start, 1, 0, 0);
  scm_install_procedure ("client-stop",  tmg_client_stop, 0, 0, 0);
  scm_install_procedure ("client-read",  tmg_client_read, 0, 0, 0);
  scm_install_procedure ("client-write",  tmg_client_write, 1, 0, 0);
  scm_install_procedure ("enter-secure-mode",  tmg_enter_secure_mode, 0, 0, 0);
  scm_install_procedure ("connection-start",  tmg_connection_start, 2, 0, 0);
  scm_install_procedure ("connection-status",  tmg_connection_status, 2, 0, 0);
  scm_install_procedure ("connection-write-string",  tmg_connection_write_string, 3, 0, 0);
  scm_install_procedure ("connection-write",  tmg_connection_write, 3, 0, 0);
  scm_install_procedure ("connection-cmd",  tmg_connection_cmd, 3, 0, 0);
  scm_install_procedure ("connection-eval",  tmg_connection_eval, 3, 0, 0);
  scm_install_procedure ("connection-interrupt",  tmg_connection_interrupt, 2, 0, 0);
  scm_install_procedure ("connection-stop",  tmg_connection_stop, 2, 0, 0);
  scm_install_procedure ("widget-printer",  tmg_widget_printer, 2, 0, 0);
  scm_install_procedure ("widget-color-picker",  tmg_widget_color_picker, 3, 0, 0);
  scm_install_procedure ("widget-extend",  tmg_widget_extend, 2, 0, 0);
  scm_install_procedure ("widget-hmenu",  tmg_widget_hmenu, 1, 0, 0);
  scm_install_procedure ("widget-vmenu",  tmg_widget_vmenu, 1, 0, 0);
  scm_install_procedure ("widget-tmenu",  tmg_widget_tmenu, 2, 0, 0);
  scm_install_procedure ("widget-minibar-menu",  tmg_widget_minibar_menu, 1, 0, 0);
  scm_install_procedure ("widget-separator",  tmg_widget_separator, 1, 0, 0);
  scm_install_procedure ("widget-menu-group",  tmg_widget_menu_group, 2, 0, 0);
  scm_install_procedure ("widget-pulldown-button",  tmg_widget_pulldown_button, 2, 0, 0);
  scm_install_procedure ("widget-pullright-button",  tmg_widget_pullright_button, 2, 0, 0);
  scm_install_procedure ("widget-menu-button",  tmg_widget_menu_button, 5, 0, 0);
  scm_install_procedure ("widget-toggle",  tmg_widget_toggle, 3, 0, 0);
  scm_install_procedure ("widget-balloon",  tmg_widget_balloon, 2, 0, 0);
  scm_install_procedure ("widget-empty",  tmg_widget_empty, 0, 0, 0);
  scm_install_procedure ("widget-text",  tmg_widget_text, 4, 0, 0);
  scm_install_procedure ("widget-input",  tmg_widget_input, 5, 0, 0);
  scm_install_procedure ("widget-enum",  tmg_widget_enum, 5, 0, 0);
  scm_install_procedure ("widget-choice",  tmg_widget_choice, 3, 0, 0);
  scm_install_procedure ("widget-choices",  tmg_widget_choices, 3, 0, 0);
  scm_install_procedure ("widget-xpm",  tmg_widget_xpm, 1, 0, 0);
  scm_install_procedure ("widget-box",  tmg_widget_box, 5, 0, 0);
  scm_install_procedure ("widget-glue",  tmg_widget_glue, 4, 0, 0);
  scm_install_procedure ("widget-color",  tmg_widget_color, 5, 0, 0);
  scm_install_procedure ("widget-hlist",  tmg_widget_hlist, 1, 0, 0);
  scm_install_procedure ("widget-vlist",  tmg_widget_vlist, 1, 0, 0);
  scm_install_procedure ("widget-aligned",  tmg_widget_aligned, 2, 0, 0);
  scm_install_procedure ("widget-tabs",  tmg_widget_tabs, 2, 0, 0);
  scm_install_procedure ("widget-scrollable",  tmg_widget_scrollable, 2, 0, 0);
  scm_install_procedure ("widget-resize",  tmg_widget_resize, 8, 0, 0);
  scm_install_procedure ("widget-hsplit",  tmg_widget_hsplit, 2, 0, 0);
  scm_install_procedure ("widget-vsplit",  tmg_widget_vsplit, 2, 0, 0);
  scm_install_procedure ("widget-texmacs-output",  tmg_widget_texmacs_output, 1, 0, 0);
  scm_install_procedure ("widget-texmacs-input",  tmg_widget_texmacs_input, 3, 0, 0);
  scm_install_procedure ("widget-ink",  tmg_widget_ink, 1, 0, 0);
  scm_install_procedure ("widget-refresh",  tmg_widget_refresh, 1, 0, 0);
  scm_install_procedure ("object->promise-widget",  tmg_object_2promise_widget, 1, 0, 0);
  scm_install_procedure ("tree-bounding-rectangle",  tmg_tree_bounding_rectangle, 1, 0, 0);
  scm_install_procedure ("show-balloon",  tmg_show_balloon, 3, 0, 0);
  scm_install_procedure ("window-handle",  tmg_window_handle, 0, 0, 0);
  scm_install_procedure ("window-create",  tmg_window_create, 4, 0, 0);
  scm_install_procedure ("window-create-quit",  tmg_window_create_quit, 4, 0, 0);
  scm_install_procedure ("window-delete",  tmg_window_delete, 1, 0, 0);
  scm_install_procedure ("window-show",  tmg_window_show, 1, 0, 0);
  scm_install_procedure ("window-hide",  tmg_window_hide, 1, 0, 0);
  scm_install_procedure ("bib-add-period",  tmg_bib_add_period, 1, 0, 0);
  scm_install_procedure ("bib-upcase-first",  tmg_bib_upcase_first, 1, 0, 0);
  scm_install_procedure ("bib-locase",  tmg_bib_locase, 1, 0, 0);
  scm_install_procedure ("bib-upcase",  tmg_bib_upcase, 1, 0, 0);
  scm_install_procedure ("bib-default",  tmg_bib_default, 1, 0, 0);
  scm_install_procedure ("bib-purify",  tmg_bib_purify, 1, 0, 0);
  scm_install_procedure ("bib-text-length",  tmg_bib_text_length, 1, 0, 0);
  scm_install_procedure ("bib-prefix",  tmg_bib_prefix, 2, 0, 0);
  scm_install_procedure ("bib-empty?",  tmg_bib_emptyP, 2, 0, 0);
  scm_install_procedure ("bib-field",  tmg_bib_field, 2, 0, 0);
  scm_install_procedure ("bib-abbreviate",  tmg_bib_abbreviate, 3, 0, 0);
}
