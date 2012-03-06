
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

tmscm
tmg_texmacs_version_release (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "texmacs-version-release");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= texmacs_version (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_version_beforeP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "version-before?");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "version-before?");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  bool out= version_inf (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_os_win32P () {
  // TMSCM_DEFER_INTS;
  bool out= os_win32 ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_os_mingwP () {
  // TMSCM_DEFER_INTS;
  bool out= os_mingw ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_os_macosP () {
  // TMSCM_DEFER_INTS;
  bool out= os_macos ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_x_guiP () {
  // TMSCM_DEFER_INTS;
  bool out= gui_is_x ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_qt_guiP () {
  // TMSCM_DEFER_INTS;
  bool out= gui_is_qt ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_default_look_and_feel () {
  // TMSCM_DEFER_INTS;
  string out= default_look_and_feel ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_default_chinese_font () {
  // TMSCM_DEFER_INTS;
  string out= default_chinese_font_name ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_default_japanese_font () {
  // TMSCM_DEFER_INTS;
  string out= default_japanese_font_name ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_default_korean_font () {
  // TMSCM_DEFER_INTS;
  string out= default_korean_font_name ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tm_output (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tm-output");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tm_output (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tm_errput (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tm-errput");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tm_errput (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_win32_display (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "win32-display");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  win32_display (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_error () {
  // TMSCM_DEFER_INTS;
  cpp_error ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_scheme_dialect () {
  // TMSCM_DEFER_INTS;
  string out= scheme_dialect ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_get_texmacs_path () {
  // TMSCM_DEFER_INTS;
  string out= get_texmacs_path ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_plugin_list () {
  // TMSCM_DEFER_INTS;
  scheme_tree out= plugin_list ();
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_set_fast_environments (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "set-fast-environments");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  set_fast_environments (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_font_exists_in_ttP (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "font-exists-in-tt?");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bool out= tt_font_exists (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_eval_system (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "eval-system");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= eval_system (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_var_eval_system (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "var-eval-system");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= var_eval_system (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_get_locale_language () {
  // TMSCM_DEFER_INTS;
  string out= get_locale_language ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_texmacs_time () {
  // TMSCM_DEFER_INTS;
  int out= texmacs_time ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_texmacs_memory () {
  // TMSCM_DEFER_INTS;
  int out= mem_used ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_bench_print (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "bench-print");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bench_print (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_bench_print_all () {
  // TMSCM_DEFER_INTS;
  bench_print ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_system_wait (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "system-wait");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "system-wait");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  system_wait (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_bibtex_command (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "set-bibtex-command");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  set_bibtex_command (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_math_symbol_group (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "math-symbol-group");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= math_symbol_group (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_math_group_members (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "math-group-members");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= math_group_members (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_math_symbol_type (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "math-symbol-type");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= math_symbol_type (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_object_2command (tmscm arg1) {
  TMSCM_ASSERT_OBJECT (arg1, TMSCM_ARG1, "object->command");

  object in1= tmscm_to_object (arg1);

  // TMSCM_DEFER_INTS;
  command out= as_command (in1);
  // TMSCM_ALLOW_INTS;

  return command_to_tmscm (out);
}

tmscm
tmg_exec_delayed (tmscm arg1) {
  TMSCM_ASSERT_OBJECT (arg1, TMSCM_ARG1, "exec-delayed");

  object in1= tmscm_to_object (arg1);

  // TMSCM_DEFER_INTS;
  exec_delayed (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_exec_delayed_pause (tmscm arg1) {
  TMSCM_ASSERT_OBJECT (arg1, TMSCM_ARG1, "exec-delayed-pause");

  object in1= tmscm_to_object (arg1);

  // TMSCM_DEFER_INTS;
  exec_delayed_pause (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_notify_preferences_loaded () {
  // TMSCM_DEFER_INTS;
  notify_preferences_loaded ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_input_language (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "set-input-language");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  set_input_language (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_input_language () {
  // TMSCM_DEFER_INTS;
  string out= get_input_language ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_set_output_language (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "set-output-language");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  gui_set_output_language (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_output_language () {
  // TMSCM_DEFER_INTS;
  string out= get_output_language ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_translate (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "translate");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  string out= translate (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_translate_from_to (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "translate-from-to");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "translate-from-to");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "translate-from-to");

  content in1= tmscm_to_content (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  string out= translate (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tree_translate (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-translate");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= tree_translate (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_translate_from_to (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-translate-from-to");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tree-translate-from-to");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "tree-translate-from-to");

  content in1= tmscm_to_content (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  tree out= tree_translate (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_color (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "color");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  int out= named_color (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_new_author () {
  // TMSCM_DEFER_INTS;
  double out= new_author ();
  // TMSCM_ALLOW_INTS;

  return double_to_tmscm (out);
}

tmscm
tmg_set_author (tmscm arg1) {
  TMSCM_ASSERT_DOUBLE (arg1, TMSCM_ARG1, "set-author");

  double in1= tmscm_to_double (arg1);

  // TMSCM_DEFER_INTS;
  set_author (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_author () {
  // TMSCM_DEFER_INTS;
  double out= get_author ();
  // TMSCM_ALLOW_INTS;

  return double_to_tmscm (out);
}

tmscm
tmg_debug_set (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "debug-set");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "debug-set");

  string in1= tmscm_to_string (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  debug_set (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_debug_get (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "debug-get");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bool out= debug_get (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_cout_buffer () {
  // TMSCM_DEFER_INTS;
  cout_buffer ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cout_unbuffer () {
  // TMSCM_DEFER_INTS;
  string out= cout_unbuffer ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_mark_new () {
  // TMSCM_DEFER_INTS;
  double out= new_marker ();
  // TMSCM_ALLOW_INTS;

  return double_to_tmscm (out);
}

tmscm
tmg_glyph_register (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "glyph-register");
  TMSCM_ASSERT_ARRAY_ARRAY_ARRAY_DOUBLE (arg2, TMSCM_ARG2, "glyph-register");

  string in1= tmscm_to_string (arg1);
  array_array_array_double in2= tmscm_to_array_array_array_double (arg2);

  // TMSCM_DEFER_INTS;
  register_glyph (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_glyph_recognize (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_ARRAY_ARRAY_DOUBLE (arg1, TMSCM_ARG1, "glyph-recognize");

  array_array_array_double in1= tmscm_to_array_array_array_double (arg1);

  // TMSCM_DEFER_INTS;
  string out= recognize_glyph (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_image_2psdoc (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "image->psdoc");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  string out= image_to_psdoc (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tree_2stree (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree->stree");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  scheme_tree out= tree_to_scheme_tree (in1);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_stree_2tree (tmscm arg1) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "stree->tree");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);

  // TMSCM_DEFER_INTS;
  tree out= scheme_tree_to_tree (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_2string (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree->string");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  string out= coerce_tree_string (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_string_2tree (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string->tree");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= coerce_string_tree (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tm_2tree (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tm->tree");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= tree (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_atomicP (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-atomic?");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_atomic (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tree_compoundP (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-compound?");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_compound (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tree_label (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-label");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  tree_label out= L (in1);
  // TMSCM_ALLOW_INTS;

  return tree_label_to_tmscm (out);
}

tmscm
tmg_tree_children (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-children");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  array_tree out= A (in1);
  // TMSCM_ALLOW_INTS;

  return array_tree_to_tmscm (out);
}

tmscm
tmg_tree_arity (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-arity");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  int out= N (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_tree_child_ref (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-child-ref");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-child-ref");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  tree out= tree_ref (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_child_setS (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-child-set!");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-child-set!");
  TMSCM_ASSERT_CONTENT (arg3, TMSCM_ARG3, "tree-child-set!");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);
  content in3= tmscm_to_content (arg3);

  // TMSCM_DEFER_INTS;
  tree out= tree_set (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_child_insert (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-child-insert");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-child-insert");
  TMSCM_ASSERT_CONTENT (arg3, TMSCM_ARG3, "tree-child-insert");

  content in1= tmscm_to_content (arg1);
  int in2= tmscm_to_int (arg2);
  content in3= tmscm_to_content (arg3);

  // TMSCM_DEFER_INTS;
  tree out= tree_child_insert (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_ip (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-ip");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  path out= obtain_ip (in1);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_tree_activeP (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-active?");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  bool out= tree_active (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tree_eqP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-eq?");
  TMSCM_ASSERT_TREE (arg2, TMSCM_ARG2, "tree-eq?");

  tree in1= tmscm_to_tree (arg1);
  tree in2= tmscm_to_tree (arg2);

  // TMSCM_DEFER_INTS;
  bool out= strong_equal (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_subtree (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "subtree");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "subtree");

  tree in1= tmscm_to_tree (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  tree out= subtree (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_range (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-range");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-range");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "tree-range");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);
  int in3= tmscm_to_int (arg3);

  // TMSCM_DEFER_INTS;
  tree out= tree_range (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_copy (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-copy");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  tree out= copy (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_append (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-append");
  TMSCM_ASSERT_TREE (arg2, TMSCM_ARG2, "tree-append");

  tree in1= tmscm_to_tree (arg1);
  tree in2= tmscm_to_tree (arg2);

  // TMSCM_DEFER_INTS;
  tree out= tree_append (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_right_index (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-right-index");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  int out= right_index (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_tree_label_extensionP (tmscm arg1) {
  TMSCM_ASSERT_TREE_LABEL (arg1, TMSCM_ARG1, "tree-label-extension?");

  tree_label in1= tmscm_to_tree_label (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_extension (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tree_multi_paragraphP (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-multi-paragraph?");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_multi_paragraph (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tree_simplify (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-simplify");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  tree out= simplify_correct (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_minimal_arity (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-minimal-arity");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  int out= minimal_arity (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_tree_maximal_arity (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-maximal-arity");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  int out= maximal_arity (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_tree_possible_arityP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-possible-arity?");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-possible-arity?");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  bool out= correct_arity (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tree_insert_point (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-insert_point");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-insert_point");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  int out= insert_point (in1, in2);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_tree_is_dynamicP (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-is-dynamic?");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_dynamic (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tree_accessible_childP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-accessible-child?");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-accessible-child?");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  bool out= is_accessible_child (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tree_accessible_children (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-accessible-children");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  array_tree out= accessible_children (in1);
  // TMSCM_ALLOW_INTS;

  return array_tree_to_tmscm (out);
}

tmscm
tmg_tree_all_accessibleP (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-all-accessible?");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  bool out= all_accessible (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tree_none_accessibleP (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-none-accessible?");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  bool out= none_accessible (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tree_name (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-name");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  string out= get_name (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tree_long_name (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-long-name");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  string out= get_long_name (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tree_child_name (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-child-name");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-child-name");

  content in1= tmscm_to_content (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  string out= get_child_name (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tree_child_long_name (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-child-long-name");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-child-long-name");

  content in1= tmscm_to_content (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  string out= get_child_long_name (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tree_child_type (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-child-type");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-child-type");

  content in1= tmscm_to_content (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  string out= get_child_type (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tree_child_env (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-child-env");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-child-env");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "tree-child-env");
  TMSCM_ASSERT_CONTENT (arg4, TMSCM_ARG4, "tree-child-env");

  content in1= tmscm_to_content (arg1);
  int in2= tmscm_to_int (arg2);
  string in3= tmscm_to_string (arg3);
  content in4= tmscm_to_content (arg4);

  // TMSCM_DEFER_INTS;
  tree out= get_env_child (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_load_inclusion (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tree-load-inclusion");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  tree out= load_inclusion (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_as_string (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-as-string");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  string out= tree_as_string (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tree_extents (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-extents");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= tree_extents (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_emptyP (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-empty?");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_empty (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tree_is_bufferP (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-is-buffer?");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  bool out= admits_edit_observer (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tree_search_sections (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-search-sections");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  array_tree out= search_sections (in1);
  // TMSCM_ALLOW_INTS;

  return array_tree_to_tmscm (out);
}

tmscm
tmg_tree_assign (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-assign");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "tree-assign");

  tree in1= tmscm_to_tree (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  tree out= tree_assign (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_var_insert (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-var-insert");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-var-insert");
  TMSCM_ASSERT_CONTENT (arg3, TMSCM_ARG3, "tree-var-insert");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);
  content in3= tmscm_to_content (arg3);

  // TMSCM_DEFER_INTS;
  tree out= tree_insert (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_remove (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-remove");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-remove");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "tree-remove");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);
  int in3= tmscm_to_int (arg3);

  // TMSCM_DEFER_INTS;
  tree out= tree_remove (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_split (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-split");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-split");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "tree-split");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);
  int in3= tmscm_to_int (arg3);

  // TMSCM_DEFER_INTS;
  tree out= tree_split (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_join (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-join");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-join");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  tree out= tree_join (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_assign_node (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-assign-node");
  TMSCM_ASSERT_TREE_LABEL (arg2, TMSCM_ARG2, "tree-assign-node");

  tree in1= tmscm_to_tree (arg1);
  tree_label in2= tmscm_to_tree_label (arg2);

  // TMSCM_DEFER_INTS;
  tree out= tree_assign_node (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_insert_node (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-insert-node");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-insert-node");
  TMSCM_ASSERT_CONTENT (arg3, TMSCM_ARG3, "tree-insert-node");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);
  content in3= tmscm_to_content (arg3);

  // TMSCM_DEFER_INTS;
  tree out= tree_insert_node (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_remove_node (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-remove-node");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-remove-node");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  tree out= tree_remove_node (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_cpp_tree_correct_node (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "cpp-tree-correct-node");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  correct_node (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_tree_correct_downwards (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "cpp-tree-correct-downwards");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  correct_downwards (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_tree_correct_upwards (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "cpp-tree-correct-upwards");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  correct_upwards (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_concat_tokenize_math (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "concat-tokenize-math");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  array_tree out= concat_tokenize (in1);
  // TMSCM_ALLOW_INTS;

  return array_tree_to_tmscm (out);
}

tmscm
tmg_concat_decompose (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "concat-decompose");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  array_tree out= concat_decompose (in1);
  // TMSCM_ALLOW_INTS;

  return array_tree_to_tmscm (out);
}

tmscm
tmg_concat_recompose (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_TREE (arg1, TMSCM_ARG1, "concat-recompose");

  array_tree in1= tmscm_to_array_tree (arg1);

  // TMSCM_DEFER_INTS;
  tree out= concat_recompose (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_with_likeP (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "with-like?");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_with_like (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_with_same_typeP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "with-same-type?");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "with-same-type?");

  content in1= tmscm_to_content (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  bool out= with_same_type (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_with_similar_typeP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "with-similar-type?");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "with-similar-type?");

  content in1= tmscm_to_content (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  bool out= with_similar_type (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_with_correct (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "with-correct");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= with_correct (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_with_correct_superfluous (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "with-correct-superfluous");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= superfluous_with_correct (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_invisible_correct_superfluous (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "invisible-correct-superfluous");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= superfluous_invisible_correct (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_invisible_correct_missing (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "invisible-correct-missing");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "invisible-correct-missing");

  content in1= tmscm_to_content (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  tree out= missing_invisible_correct (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_automatic_correct (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "automatic-correct");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "automatic-correct");

  content in1= tmscm_to_content (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  tree out= automatic_correct (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_manual_correct (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "manual-correct");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= manual_correct (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_upgrade_brackets (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-upgrade-brackets");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tree-upgrade-brackets");

  content in1= tmscm_to_content (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  tree out= upgrade_brackets (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_upgrade_big (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-upgrade-big");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= upgrade_big (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_downgrade_brackets (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-downgrade-brackets");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "tree-downgrade-brackets");
  TMSCM_ASSERT_BOOL (arg3, TMSCM_ARG3, "tree-downgrade-brackets");

  content in1= tmscm_to_content (arg1);
  bool in2= tmscm_to_bool (arg2);
  bool in3= tmscm_to_bool (arg3);

  // TMSCM_DEFER_INTS;
  tree out= downgrade_brackets (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_downgrade_big (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-downgrade-big");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= downgrade_big (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_math_status_print () {
  // TMSCM_DEFER_INTS;
  math_status_print ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_math_status_reset () {
  // TMSCM_DEFER_INTS;
  math_status_reset ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_path_infP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "path-inf?");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-inf?");

  path in1= tmscm_to_path (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  bool out= path_inf (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_path_inf_eqP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "path-inf-eq?");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-inf-eq?");

  path in1= tmscm_to_path (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  bool out= path_inf_eq (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_path_lessP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "path-less?");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-less?");

  path in1= tmscm_to_path (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  bool out= path_less (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_path_less_eqP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "path-less-eq?");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-less-eq?");

  path in1= tmscm_to_path (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  bool out= path_less_eq (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_path_start (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "path-start");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-start");

  content in1= tmscm_to_content (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  path out= start (in1, in2);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_path_end (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "path-end");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-end");

  content in1= tmscm_to_content (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  path out= end (in1, in2);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_path_next (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "path-next");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-next");

  content in1= tmscm_to_content (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  path out= next_valid (in1, in2);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_path_previous (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "path-previous");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-previous");

  content in1= tmscm_to_content (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  path out= previous_valid (in1, in2);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_path_next_word (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "path-next-word");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-next-word");

  content in1= tmscm_to_content (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  path out= next_word (in1, in2);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_path_previous_word (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "path-previous-word");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-previous-word");

  content in1= tmscm_to_content (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  path out= previous_word (in1, in2);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_path_next_node (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "path-next-node");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-next-node");

  content in1= tmscm_to_content (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  path out= next_node (in1, in2);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_path_previous_node (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "path-previous-node");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-previous-node");

  content in1= tmscm_to_content (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  path out= previous_node (in1, in2);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_path_next_tag (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "path-next-tag");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-next-tag");
  TMSCM_ASSERT_SCHEME_TREE (arg3, TMSCM_ARG3, "path-next-tag");

  content in1= tmscm_to_content (arg1);
  path in2= tmscm_to_path (arg2);
  scheme_tree in3= tmscm_to_scheme_tree (arg3);

  // TMSCM_DEFER_INTS;
  path out= next_tag (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_path_previous_tag (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "path-previous-tag");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-previous-tag");
  TMSCM_ASSERT_SCHEME_TREE (arg3, TMSCM_ARG3, "path-previous-tag");

  content in1= tmscm_to_content (arg1);
  path in2= tmscm_to_path (arg2);
  scheme_tree in3= tmscm_to_scheme_tree (arg3);

  // TMSCM_DEFER_INTS;
  path out= previous_tag (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_path_next_tag_same_argument (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "path-next-tag-same-argument");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-next-tag-same-argument");
  TMSCM_ASSERT_SCHEME_TREE (arg3, TMSCM_ARG3, "path-next-tag-same-argument");

  content in1= tmscm_to_content (arg1);
  path in2= tmscm_to_path (arg2);
  scheme_tree in3= tmscm_to_scheme_tree (arg3);

  // TMSCM_DEFER_INTS;
  path out= next_tag_same_argument (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_path_previous_tag_same_argument (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "path-previous-tag-same-argument");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-previous-tag-same-argument");
  TMSCM_ASSERT_SCHEME_TREE (arg3, TMSCM_ARG3, "path-previous-tag-same-argument");

  content in1= tmscm_to_content (arg1);
  path in2= tmscm_to_path (arg2);
  scheme_tree in3= tmscm_to_scheme_tree (arg3);

  // TMSCM_DEFER_INTS;
  path out= previous_tag_same_argument (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_path_next_argument (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "path-next-argument");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-next-argument");

  content in1= tmscm_to_content (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  path out= next_argument (in1, in2);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_path_previous_argument (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "path-previous-argument");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-previous-argument");

  content in1= tmscm_to_content (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  path out= previous_argument (in1, in2);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_path_previous_section (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "path-previous-section");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-previous-section");

  content in1= tmscm_to_content (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  path out= previous_section (in1, in2);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_tree_2ids (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree->ids");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  list_string out= get_ids (in1);
  // TMSCM_ALLOW_INTS;

  return list_string_to_tmscm (out);
}

tmscm
tmg_id_2trees (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "id->trees");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  list_tree out= get_trees (in1);
  // TMSCM_ALLOW_INTS;

  return list_tree_to_tmscm (out);
}

tmscm
tmg_vertex_2links (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "vertex->links");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  list_tree out= get_links (in1);
  // TMSCM_ALLOW_INTS;

  return list_tree_to_tmscm (out);
}

tmscm
tmg_tree_2tree_pointer (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree->tree-pointer");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  observer out= tree_pointer_new (in1);
  // TMSCM_ALLOW_INTS;

  return observer_to_tmscm (out);
}

tmscm
tmg_tree_pointer_detach (tmscm arg1) {
  TMSCM_ASSERT_OBSERVER (arg1, TMSCM_ARG1, "tree-pointer-detach");

  observer in1= tmscm_to_observer (arg1);

  // TMSCM_DEFER_INTS;
  tree_pointer_delete (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tree_pointer_2tree (tmscm arg1) {
  TMSCM_ASSERT_OBSERVER (arg1, TMSCM_ARG1, "tree-pointer->tree");

  observer in1= tmscm_to_observer (arg1);

  // TMSCM_DEFER_INTS;
  tree out= obtain_tree (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_current_link_types () {
  // TMSCM_DEFER_INTS;
  list_string out= all_link_types ();
  // TMSCM_ALLOW_INTS;

  return list_string_to_tmscm (out);
}

tmscm
tmg_get_locus_rendering (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "get-locus-rendering");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= get_locus_rendering (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_set_locus_rendering (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "set-locus-rendering");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "set-locus-rendering");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  set_locus_rendering (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_declare_visited (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "declare-visited");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  declare_visited (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_has_been_visitedP (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "has-been-visited?");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bool out= has_been_visited (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_graphics_set (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "graphics-set");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "graphics-set");

  content in1= tmscm_to_content (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  set_graphical_value (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_graphics_hasP (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "graphics-has?");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  bool out= has_graphical_value (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_graphics_ref (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "graphics-ref");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_graphical_value (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_graphics_needs_updateP () {
  // TMSCM_DEFER_INTS;
  bool out= graphics_needs_update ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_graphics_notify_update (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "graphics-notify-update");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  graphics_notify_update (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_string_numberP (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-number?");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_double (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_string_occursP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-occurs?");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "string-occurs?");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  bool out= occurs (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_string_search_forwards (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-search-forwards");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "string-search-forwards");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "string-search-forwards");

  string in1= tmscm_to_string (arg1);
  int in2= tmscm_to_int (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  int out= search_forwards (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_string_search_backwards (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-search-backwards");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "string-search-backwards");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "string-search-backwards");

  string in1= tmscm_to_string (arg1);
  int in2= tmscm_to_int (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  int out= search_backwards (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_string_replace (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-replace");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "string-replace");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "string-replace");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  string out= replace (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_string_alphaP (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-alpha?");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_alpha (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_string_locase_alphaP (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-locase-alpha?");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_locase_alpha (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_upcase_first (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "upcase-first");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= upcase_first (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_locase_first (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "locase-first");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= locase_first (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_upcase_all (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "upcase-all");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= upcase_all (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_locase_all (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "locase-all");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= locase_all (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_string_union (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-union");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "string-union");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  string out= string_union (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_string_minus (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-minus");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "string-minus");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  string out= string_minus (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_escape_generic (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "escape-generic");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= escape_generic (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_escape_verbatim (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "escape-verbatim");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= escape_verbatim (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_escape_shell (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "escape-shell");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= escape_sh (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_string_convert (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-convert");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "string-convert");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "string-convert");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  string out= convert (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_utf8_2cork (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "utf8->cork");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= utf8_to_cork (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_cork_2utf8 (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cork->utf8");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= cork_to_utf8 (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_utf8_2html (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "utf8->html");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= utf8_to_html (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tm_2xml_name (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tm->xml-name");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= tm_to_xml_name (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_old_tm_2xml_cdata (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "old-tm->xml-cdata");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= old_tm_to_xml_cdata (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tm_2xml_cdata (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tm->xml-cdata");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  object out= tm_to_xml_cdata (in1);
  // TMSCM_ALLOW_INTS;

  return object_to_tmscm (out);
}

tmscm
tmg_xml_name_2tm (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "xml-name->tm");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= xml_name_to_tm (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_old_xml_cdata_2tm (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "old-xml-cdata->tm");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= old_xml_cdata_to_tm (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_xml_unspace (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "xml-unspace");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "xml-unspace");
  TMSCM_ASSERT_BOOL (arg3, TMSCM_ARG3, "xml-unspace");

  string in1= tmscm_to_string (arg1);
  bool in2= tmscm_to_bool (arg2);
  bool in3= tmscm_to_bool (arg3);

  // TMSCM_DEFER_INTS;
  string out= xml_unspace (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_integer_2hexadecimal (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "integer->hexadecimal");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  string out= as_hexadecimal (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_integer_2padded_hexadecimal (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "integer->padded-hexadecimal");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "integer->padded-hexadecimal");

  int in1= tmscm_to_int (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  string out= as_hexadecimal (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_hexadecimal_2integer (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "hexadecimal->integer");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  int out= from_hexadecimal (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_string_2tmstring (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string->tmstring");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= tm_encode (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmstring_2string (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmstring->string");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= tm_decode (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmstring_length (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmstring-length");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  int out= tm_string_length (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_tmstring_ref (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmstring-ref");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tmstring-ref");

  string in1= tmscm_to_string (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  string out= tm_forward_access (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmstring_reverse_ref (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmstring-reverse-ref");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tmstring-reverse-ref");

  string in1= tmscm_to_string (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  string out= tm_backward_access (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmstring_2list (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmstring->list");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= tm_tokenize (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_list_2tmstring (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_STRING (arg1, TMSCM_ARG1, "list->tmstring");

  array_string in1= tmscm_to_array_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= tm_recompose (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_string_next (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-next");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "string-next");

  string in1= tmscm_to_string (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  int out= tm_char_next (in1, in2);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_string_previous (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-previous");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "string-previous");

  string in1= tmscm_to_string (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  int out= tm_char_previous (in1, in2);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_packrat_define (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "packrat-define");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "packrat-define");
  TMSCM_ASSERT_TREE (arg3, TMSCM_ARG3, "packrat-define");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  tree in3= tmscm_to_tree (arg3);

  // TMSCM_DEFER_INTS;
  packrat_define (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_packrat_property (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "packrat-property");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "packrat-property");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "packrat-property");
  TMSCM_ASSERT_STRING (arg4, TMSCM_ARG4, "packrat-property");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);
  string in4= tmscm_to_string (arg4);

  // TMSCM_DEFER_INTS;
  packrat_property (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_packrat_inherit (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "packrat-inherit");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "packrat-inherit");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  packrat_inherit (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_packrat_parse (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "packrat-parse");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "packrat-parse");
  TMSCM_ASSERT_CONTENT (arg3, TMSCM_ARG3, "packrat-parse");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  content in3= tmscm_to_content (arg3);

  // TMSCM_DEFER_INTS;
  path out= packrat_parse (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_packrat_correctP (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "packrat-correct?");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "packrat-correct?");
  TMSCM_ASSERT_CONTENT (arg3, TMSCM_ARG3, "packrat-correct?");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  content in3= tmscm_to_content (arg3);

  // TMSCM_DEFER_INTS;
  bool out= packrat_correct (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_packrat_context (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "packrat-context");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "packrat-context");
  TMSCM_ASSERT_CONTENT (arg3, TMSCM_ARG3, "packrat-context");
  TMSCM_ASSERT_PATH (arg4, TMSCM_ARG4, "packrat-context");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  content in3= tmscm_to_content (arg3);
  path in4= tmscm_to_path (arg4);

  // TMSCM_DEFER_INTS;
  object out= packrat_context (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return object_to_tmscm (out);
}

tmscm
tmg_parse_texmacs (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "parse-texmacs");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= texmacs_document_to_tree (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_serialize_texmacs (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "serialize-texmacs");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  string out= tree_to_texmacs (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_parse_texmacs_snippet (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "parse-texmacs-snippet");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= texmacs_to_tree (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_serialize_texmacs_snippet (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "serialize-texmacs-snippet");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  string out= tree_to_texmacs (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_texmacs_2stm (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "texmacs->stm");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  string out= tree_to_scheme (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_stm_2texmacs (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "stm->texmacs");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= scheme_document_to_tree (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_stm_snippet_2texmacs (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "stm-snippet->texmacs");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= scheme_to_tree (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_cpp_texmacs_2verbatim (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "cpp-texmacs->verbatim");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "cpp-texmacs->verbatim");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "cpp-texmacs->verbatim");

  tree in1= tmscm_to_tree (arg1);
  bool in2= tmscm_to_bool (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  string out= tree_to_verbatim (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_cpp_verbatim_snippet_2texmacs (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cpp-verbatim-snippet->texmacs");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "cpp-verbatim-snippet->texmacs");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "cpp-verbatim-snippet->texmacs");

  string in1= tmscm_to_string (arg1);
  bool in2= tmscm_to_bool (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  tree out= verbatim_to_tree (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_cpp_verbatim_2texmacs (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cpp-verbatim->texmacs");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "cpp-verbatim->texmacs");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "cpp-verbatim->texmacs");

  string in1= tmscm_to_string (arg1);
  bool in2= tmscm_to_bool (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  tree out= verbatim_document_to_tree (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_parse_latex (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "parse-latex");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= parse_latex (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_parse_latex_document (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "parse-latex-document");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= parse_latex_document (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_latex_2texmacs (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "latex->texmacs");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  tree out= latex_to_tree (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_latex_document_2texmacs (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "latex-document->texmacs");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= latex_document_to_tree (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_latex_class_document_2texmacs (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "latex-class-document->texmacs");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= latex_class_document_to_tree (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_parse_xml (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "parse-xml");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  scheme_tree out= parse_xml (in1);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_parse_html (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "parse-html");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  scheme_tree out= parse_html (in1);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_parse_bib (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "parse-bib");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= parse_bib (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_upgrade_tmml (tmscm arg1) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "upgrade-tmml");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);

  // TMSCM_DEFER_INTS;
  tree out= tmml_upgrade (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_upgrade_mathml (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "upgrade-mathml");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= upgrade_mathml (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_string_2url (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string->url");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  url out= url (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "url");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "url");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  url out= url (in1, in2);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_system (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "url-system");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  url out= url_system (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_none () {
  // TMSCM_DEFER_INTS;
  url out= url_none ();
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_any () {
  // TMSCM_DEFER_INTS;
  url out= url_wildcard ();
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_wildcard (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "url-wildcard");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  url out= url_wildcard (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_parent () {
  // TMSCM_DEFER_INTS;
  url out= url_parent ();
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_ancestor () {
  // TMSCM_DEFER_INTS;
  url out= url_ancestor ();
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_append (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-append");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "url-append");

  url in1= tmscm_to_url (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  url out= url_concat (in1, in2);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_or (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-or");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "url-or");

  url in1= tmscm_to_url (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  url out= url_or (in1, in2);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_2string (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url->string");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  string out= as_string (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_url_noneP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-none?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_none (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_url_rooted_webP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-rooted-web?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_rooted_web (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_url_concatP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-concat?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_concat (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_url_orP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-or?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_or (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_url_ref (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-ref");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "url-ref");

  url in1= tmscm_to_url (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  url out= url_ref (in1, in2);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_head (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-head");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  url out= head (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_tail (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-tail");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  url out= tail (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_suffix (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-suffix");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  string out= suffix (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_url_glue (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-glue");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "url-glue");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  url out= glue (in1, in2);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_unglue (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-unglue");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "url-unglue");

  url in1= tmscm_to_url (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  url out= unglue (in1, in2);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_relative (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-relative");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "url-relative");

  url in1= tmscm_to_url (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  url out= relative (in1, in2);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_expand (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-expand");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  url out= expand (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_factor (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-factor");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  url out= factor (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_delta (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-delta");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "url-delta");

  url in1= tmscm_to_url (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  url out= delta (in1, in2);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_secureP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-secure?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_secure (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_url_descendsP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-descends?");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "url-descends?");

  url in1= tmscm_to_url (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  bool out= descends (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_url_complete (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-complete");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "url-complete");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  url out= complete (in1, in2);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_resolve (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-resolve");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "url-resolve");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  url out= resolve (in1, in2);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_resolve_in_path (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-resolve-in-path");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  url out= resolve_in_path (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_existsP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-exists?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= exists (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_url_exists_in_pathP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-exists-in-path?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= exists_in_path (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_url_exists_in_texP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-exists-in-tex?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= exists_in_tex (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_url_concretize (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-concretize");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  string out= concretize (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_url_materialize (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-materialize");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "url-materialize");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  string out= materialize (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_url_testP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-test?");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "url-test?");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  bool out= is_of_type (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_url_regularP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-regular?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_regular (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_url_directoryP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-directory?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_directory (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_url_linkP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-link?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_symbolic_link (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_url_newerP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-newer?");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "url-newer?");

  url in1= tmscm_to_url (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  bool out= is_newer (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_url_last_modified (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-last-modified");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  int out= last_modified (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_url_temp () {
  // TMSCM_DEFER_INTS;
  url out= url_temp ();
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_scratch (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "url-scratch");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "url-scratch");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "url-scratch");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  int in3= tmscm_to_int (arg3);

  // TMSCM_DEFER_INTS;
  url out= url_scratch (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_scratchP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-scratch?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_scratch (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_string_save (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-save");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "string-save");

  string in1= tmscm_to_string (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  string_save (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_string_load (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "string-load");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  string out= string_load (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_system_move (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "system-move");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "system-move");

  url in1= tmscm_to_url (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  move (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_system_copy (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "system-copy");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "system-copy");

  url in1= tmscm_to_url (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  copy (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_system_remove (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "system-remove");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  remove (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_system_mkdir (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "system-mkdir");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  mkdir (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_system_search_score (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "system-search-score");
  TMSCM_ASSERT_ARRAY_STRING (arg2, TMSCM_ARG2, "system-search-score");

  url in1= tmscm_to_url (arg1);
  array_string in2= tmscm_to_array_string (arg2);

  // TMSCM_DEFER_INTS;
  int out= search_score (in1, in2);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_system_1 (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "system-1");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "system-1");

  string in1= tmscm_to_string (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  system (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_system_2 (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "system-2");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "system-2");
  TMSCM_ASSERT_URL (arg3, TMSCM_ARG3, "system-2");

  string in1= tmscm_to_string (arg1);
  url in2= tmscm_to_url (arg2);
  url in3= tmscm_to_url (arg3);

  // TMSCM_DEFER_INTS;
  system (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmfs_set (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-set");
  TMSCM_ASSERT_COLLECTION (arg2, TMSCM_ARG2, "tmfs-set");

  string in1= tmscm_to_string (arg1);
  collection in2= tmscm_to_collection (arg2);

  // TMSCM_DEFER_INTS;
  tmfs_set (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmfs_reset (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-reset");
  TMSCM_ASSERT_COLLECTION (arg2, TMSCM_ARG2, "tmfs-reset");

  string in1= tmscm_to_string (arg1);
  collection in2= tmscm_to_collection (arg2);

  // TMSCM_DEFER_INTS;
  tmfs_reset (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmfs_get (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-get");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  collection out= tmfs_get (in1);
  // TMSCM_ALLOW_INTS;

  return collection_to_tmscm (out);
}

tmscm
tmg_tmfs_new_save (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-new-save");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tmfs-new-save");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  tmfs_save (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmfs_new_remove (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-new-remove");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tmfs_remove (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmfs_new_load (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-new-load");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= tmfs_load (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmfs_create_ressource () {
  // TMSCM_DEFER_INTS;
  string out= tmfs_create_ressource ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmfs_ressource_head (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-ressource-head");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= tmfs_get_head (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmfs_ressource_versions (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-ressource-versions");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  collection out= tmfs_get_versions (in1);
  // TMSCM_ALLOW_INTS;

  return collection_to_tmscm (out);
}

tmscm
tmg_tmfs_save_ressource (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-save-ressource");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tmfs-save-ressource");
  TMSCM_ASSERT_PROPERTIES (arg3, TMSCM_ARG3, "tmfs-save-ressource");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  properties in3= tmscm_to_properties (arg3);

  // TMSCM_DEFER_INTS;
  tmfs_save_ressource (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmfs_load_ressource_file (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-load-ressource-file");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= tmfs_load_ressource_file (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmfs_load_ressource_properties (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-load-ressource-properties");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  properties out= tmfs_load_ressource_properties (in1);
  // TMSCM_ALLOW_INTS;

  return properties_to_tmscm (out);
}

tmscm
tmg_tmfs_create_user (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-create-user");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= tmfs_create_user (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmfs_search_user (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-search-user");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  collection out= tmfs_search_user (in1);
  // TMSCM_ALLOW_INTS;

  return collection_to_tmscm (out);
}

tmscm
tmg_tmfs_set_user (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-set-user");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tmfs_set_user (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmfs_get_user () {
  // TMSCM_DEFER_INTS;
  string out= tmfs_get_user ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmfs_allowsP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-allows?");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tmfs-allows?");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  bool out= tmfs_allows (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tmfs_set_attributes (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-set-attributes");
  TMSCM_ASSERT_PROPERTIES (arg2, TMSCM_ARG2, "tmfs-set-attributes");

  string in1= tmscm_to_string (arg1);
  properties in2= tmscm_to_properties (arg2);

  // TMSCM_DEFER_INTS;
  tmfs_set_attributes (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmfs_get_attributes (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-get-attributes");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  properties out= tmfs_get_attributes (in1);
  // TMSCM_ALLOW_INTS;

  return properties_to_tmscm (out);
}

tmscm
tmg_tmfs_add_attributes (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-add-attributes");
  TMSCM_ASSERT_PROPERTIES (arg2, TMSCM_ARG2, "tmfs-add-attributes");

  string in1= tmscm_to_string (arg1);
  properties in2= tmscm_to_properties (arg2);

  // TMSCM_DEFER_INTS;
  tmfs_add_attributes (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmfs_remove_attributes (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-remove-attributes");
  TMSCM_ASSERT_PROPERTIES (arg2, TMSCM_ARG2, "tmfs-remove-attributes");

  string in1= tmscm_to_string (arg1);
  properties in2= tmscm_to_properties (arg2);

  // TMSCM_DEFER_INTS;
  tmfs_remove_attributes (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmfs_change_attributes (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-change-attributes");
  TMSCM_ASSERT_PROPERTIES (arg2, TMSCM_ARG2, "tmfs-change-attributes");

  string in1= tmscm_to_string (arg1);
  properties in2= tmscm_to_properties (arg2);

  // TMSCM_DEFER_INTS;
  tmfs_change_attributes (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmfs_query (tmscm arg1) {
  TMSCM_ASSERT_PROPERTIES (arg1, TMSCM_ARG1, "tmfs-query");

  properties in1= tmscm_to_properties (arg1);

  // TMSCM_DEFER_INTS;
  solutions out= tmfs_query (in1);
  // TMSCM_ALLOW_INTS;

  return solutions_to_tmscm (out);
}

tmscm
tmg_solutions_2collection (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_SOLUTIONS (arg1, TMSCM_ARG1, "solutions->collection");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "solutions->collection");

  solutions in1= tmscm_to_solutions (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  collection out= as_collection (in1, in2);
  // TMSCM_ALLOW_INTS;

  return collection_to_tmscm (out);
}

tmscm
tmg_tmfs_create_file (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-create-file");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tmfs-create-file");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  string out= tmfs_create_file (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmfs_create_file_in (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-create-file-in");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tmfs-create-file-in");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "tmfs-create-file-in");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  string out= tmfs_create_file (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmfs_search_file (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-search-file");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  collection out= tmfs_search_file (in1);
  // TMSCM_ALLOW_INTS;

  return collection_to_tmscm (out);
}

tmscm
tmg_tmfs_save_file (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-save-file");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tmfs-save-file");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  tmfs_save_file (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmfs_load_file (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-load-file");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= tmfs_load_file (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmfs_create_project (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-create-project");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= tmfs_create_project (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmfs_search_project (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-search-project");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  collection out= tmfs_search_project (in1);
  // TMSCM_ALLOW_INTS;

  return collection_to_tmscm (out);
}

tmscm
tmg_tmfs_get_file_projects (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-get-file-projects");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  collection out= tmfs_get_file_projects (in1);
  // TMSCM_ALLOW_INTS;

  return collection_to_tmscm (out);
}

tmscm
tmg_tmfs_get_project_files (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-get-project-files");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  collection out= tmfs_get_project_files (in1);
  // TMSCM_ALLOW_INTS;

  return collection_to_tmscm (out);
}

tmscm
tmg_tmfs_create_branch (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-create-branch");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tmfs-create-branch");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  string out= tmfs_create_branch (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmfs_set_root (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-set-root");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "tmfs-set-root");

  string in1= tmscm_to_string (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  tmfs_set_root (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmfs_get_root (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmfs-get-root");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  url out= tmfs_get_root (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_tmfs_import (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tmfs-import");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  tmfs_import (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmfs_export (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tmfs-export");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  tmfs_export (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_server_start () {
  // TMSCM_DEFER_INTS;
  server_start ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_server_stop () {
  // TMSCM_DEFER_INTS;
  server_stop ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_server_read (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "server-read");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  string out= server_read (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_server_write (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "server-write");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "server-write");

  int in1= tmscm_to_int (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  server_write (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_client_start (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "client-start");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  client_start (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_client_stop () {
  // TMSCM_DEFER_INTS;
  client_stop ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_client_read () {
  // TMSCM_DEFER_INTS;
  string out= client_read ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_client_write (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "client-write");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  client_write (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_enter_secure_mode () {
  // TMSCM_DEFER_INTS;
  enter_secure_mode ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_connection_start (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "connection-start");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "connection-start");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  string out= connection_start (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_connection_status (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "connection-status");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "connection-status");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  int out= connection_status (in1, in2);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_connection_write_string (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "connection-write-string");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "connection-write-string");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "connection-write-string");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  connection_write (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_connection_write (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "connection-write");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "connection-write");
  TMSCM_ASSERT_CONTENT (arg3, TMSCM_ARG3, "connection-write");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  content in3= tmscm_to_content (arg3);

  // TMSCM_DEFER_INTS;
  connection_write (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_connection_cmd (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "connection-cmd");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "connection-cmd");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "connection-cmd");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  tree out= connection_cmd (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_connection_eval (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "connection-eval");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "connection-eval");
  TMSCM_ASSERT_CONTENT (arg3, TMSCM_ARG3, "connection-eval");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  content in3= tmscm_to_content (arg3);

  // TMSCM_DEFER_INTS;
  tree out= connection_eval (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_connection_interrupt (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "connection-interrupt");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "connection-interrupt");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  connection_interrupt (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_connection_stop (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "connection-stop");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "connection-stop");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  connection_stop (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_widget_printer (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_COMMAND (arg1, TMSCM_ARG1, "widget-printer");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "widget-printer");

  command in1= tmscm_to_command (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  widget out= printer_widget (in1, in2);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_color_picker (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_COMMAND (arg1, TMSCM_ARG1, "widget-color-picker");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "widget-color-picker");
  TMSCM_ASSERT_ARRAY_TREE (arg3, TMSCM_ARG3, "widget-color-picker");

  command in1= tmscm_to_command (arg1);
  bool in2= tmscm_to_bool (arg2);
  array_tree in3= tmscm_to_array_tree (arg3);

  // TMSCM_DEFER_INTS;
  widget out= color_picker_widget (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_extend (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_WIDGET (arg1, TMSCM_ARG1, "widget-extend");
  TMSCM_ASSERT_ARRAY_WIDGET (arg2, TMSCM_ARG2, "widget-extend");

  widget in1= tmscm_to_widget (arg1);
  array_widget in2= tmscm_to_array_widget (arg2);

  // TMSCM_DEFER_INTS;
  widget out= extend (in1, in2);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_hmenu (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_WIDGET (arg1, TMSCM_ARG1, "widget-hmenu");

  array_widget in1= tmscm_to_array_widget (arg1);

  // TMSCM_DEFER_INTS;
  widget out= horizontal_menu (in1);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_vmenu (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_WIDGET (arg1, TMSCM_ARG1, "widget-vmenu");

  array_widget in1= tmscm_to_array_widget (arg1);

  // TMSCM_DEFER_INTS;
  widget out= vertical_menu (in1);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_tmenu (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_ARRAY_WIDGET (arg1, TMSCM_ARG1, "widget-tmenu");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "widget-tmenu");

  array_widget in1= tmscm_to_array_widget (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  widget out= tile_menu (in1, in2);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_minibar_menu (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_WIDGET (arg1, TMSCM_ARG1, "widget-minibar-menu");

  array_widget in1= tmscm_to_array_widget (arg1);

  // TMSCM_DEFER_INTS;
  widget out= minibar_menu (in1);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_separator (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "widget-separator");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  widget out= menu_separator (in1);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_menu_group (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "widget-menu-group");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "widget-menu-group");

  string in1= tmscm_to_string (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  widget out= menu_group (in1, in2);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_pulldown_button (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_WIDGET (arg1, TMSCM_ARG1, "widget-pulldown-button");
  TMSCM_ASSERT_PROMISE_WIDGET (arg2, TMSCM_ARG2, "widget-pulldown-button");

  widget in1= tmscm_to_widget (arg1);
  promise_widget in2= tmscm_to_promise_widget (arg2);

  // TMSCM_DEFER_INTS;
  widget out= pulldown_button (in1, in2);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_pullright_button (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_WIDGET (arg1, TMSCM_ARG1, "widget-pullright-button");
  TMSCM_ASSERT_PROMISE_WIDGET (arg2, TMSCM_ARG2, "widget-pullright-button");

  widget in1= tmscm_to_widget (arg1);
  promise_widget in2= tmscm_to_promise_widget (arg2);

  // TMSCM_DEFER_INTS;
  widget out= pullright_button (in1, in2);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_menu_button (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5) {
  TMSCM_ASSERT_WIDGET (arg1, TMSCM_ARG1, "widget-menu-button");
  TMSCM_ASSERT_COMMAND (arg2, TMSCM_ARG2, "widget-menu-button");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "widget-menu-button");
  TMSCM_ASSERT_STRING (arg4, TMSCM_ARG4, "widget-menu-button");
  TMSCM_ASSERT_INT (arg5, TMSCM_ARG5, "widget-menu-button");

  widget in1= tmscm_to_widget (arg1);
  command in2= tmscm_to_command (arg2);
  string in3= tmscm_to_string (arg3);
  string in4= tmscm_to_string (arg4);
  int in5= tmscm_to_int (arg5);

  // TMSCM_DEFER_INTS;
  widget out= menu_button (in1, in2, in3, in4, in5);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_toggle (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_COMMAND (arg1, TMSCM_ARG1, "widget-toggle");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "widget-toggle");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "widget-toggle");

  command in1= tmscm_to_command (arg1);
  bool in2= tmscm_to_bool (arg2);
  int in3= tmscm_to_int (arg3);

  // TMSCM_DEFER_INTS;
  widget out= toggle_widget (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_balloon (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_WIDGET (arg1, TMSCM_ARG1, "widget-balloon");
  TMSCM_ASSERT_WIDGET (arg2, TMSCM_ARG2, "widget-balloon");

  widget in1= tmscm_to_widget (arg1);
  widget in2= tmscm_to_widget (arg2);

  // TMSCM_DEFER_INTS;
  widget out= balloon_widget (in1, in2);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_empty () {
  // TMSCM_DEFER_INTS;
  widget out= empty_widget ();
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_text (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "widget-text");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "widget-text");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "widget-text");
  TMSCM_ASSERT_BOOL (arg4, TMSCM_ARG4, "widget-text");

  string in1= tmscm_to_string (arg1);
  int in2= tmscm_to_int (arg2);
  int in3= tmscm_to_int (arg3);
  bool in4= tmscm_to_bool (arg4);

  // TMSCM_DEFER_INTS;
  widget out= text_widget (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_input (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5) {
  TMSCM_ASSERT_COMMAND (arg1, TMSCM_ARG1, "widget-input");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "widget-input");
  TMSCM_ASSERT_ARRAY_STRING (arg3, TMSCM_ARG3, "widget-input");
  TMSCM_ASSERT_INT (arg4, TMSCM_ARG4, "widget-input");
  TMSCM_ASSERT_STRING (arg5, TMSCM_ARG5, "widget-input");

  command in1= tmscm_to_command (arg1);
  string in2= tmscm_to_string (arg2);
  array_string in3= tmscm_to_array_string (arg3);
  int in4= tmscm_to_int (arg4);
  string in5= tmscm_to_string (arg5);

  // TMSCM_DEFER_INTS;
  widget out= input_text_widget (in1, in2, in3, in4, in5);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_enum (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5) {
  TMSCM_ASSERT_COMMAND (arg1, TMSCM_ARG1, "widget-enum");
  TMSCM_ASSERT_ARRAY_STRING (arg2, TMSCM_ARG2, "widget-enum");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "widget-enum");
  TMSCM_ASSERT_INT (arg4, TMSCM_ARG4, "widget-enum");
  TMSCM_ASSERT_STRING (arg5, TMSCM_ARG5, "widget-enum");

  command in1= tmscm_to_command (arg1);
  array_string in2= tmscm_to_array_string (arg2);
  string in3= tmscm_to_string (arg3);
  int in4= tmscm_to_int (arg4);
  string in5= tmscm_to_string (arg5);

  // TMSCM_DEFER_INTS;
  widget out= enum_widget (in1, in2, in3, in4, in5);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_choice (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_COMMAND (arg1, TMSCM_ARG1, "widget-choice");
  TMSCM_ASSERT_ARRAY_STRING (arg2, TMSCM_ARG2, "widget-choice");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "widget-choice");

  command in1= tmscm_to_command (arg1);
  array_string in2= tmscm_to_array_string (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  widget out= choice_widget (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_choices (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_COMMAND (arg1, TMSCM_ARG1, "widget-choices");
  TMSCM_ASSERT_ARRAY_STRING (arg2, TMSCM_ARG2, "widget-choices");
  TMSCM_ASSERT_ARRAY_STRING (arg3, TMSCM_ARG3, "widget-choices");

  command in1= tmscm_to_command (arg1);
  array_string in2= tmscm_to_array_string (arg2);
  array_string in3= tmscm_to_array_string (arg3);

  // TMSCM_DEFER_INTS;
  widget out= choice_widget (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_xpm (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "widget-xpm");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  widget out= xpm_widget (in1);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_box (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "widget-box");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "widget-box");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "widget-box");
  TMSCM_ASSERT_BOOL (arg4, TMSCM_ARG4, "widget-box");
  TMSCM_ASSERT_BOOL (arg5, TMSCM_ARG5, "widget-box");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);
  string in2= tmscm_to_string (arg2);
  int in3= tmscm_to_int (arg3);
  bool in4= tmscm_to_bool (arg4);
  bool in5= tmscm_to_bool (arg5);

  // TMSCM_DEFER_INTS;
  widget out= box_widget (in1, in2, in3, in4, in5);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_glue (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "widget-glue");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "widget-glue");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "widget-glue");
  TMSCM_ASSERT_INT (arg4, TMSCM_ARG4, "widget-glue");

  bool in1= tmscm_to_bool (arg1);
  bool in2= tmscm_to_bool (arg2);
  int in3= tmscm_to_int (arg3);
  int in4= tmscm_to_int (arg4);

  // TMSCM_DEFER_INTS;
  widget out= glue_widget (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_color (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "widget-color");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "widget-color");
  TMSCM_ASSERT_BOOL (arg3, TMSCM_ARG3, "widget-color");
  TMSCM_ASSERT_INT (arg4, TMSCM_ARG4, "widget-color");
  TMSCM_ASSERT_INT (arg5, TMSCM_ARG5, "widget-color");

  content in1= tmscm_to_content (arg1);
  bool in2= tmscm_to_bool (arg2);
  bool in3= tmscm_to_bool (arg3);
  int in4= tmscm_to_int (arg4);
  int in5= tmscm_to_int (arg5);

  // TMSCM_DEFER_INTS;
  widget out= glue_widget (in1, in2, in3, in4, in5);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_hlist (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_WIDGET (arg1, TMSCM_ARG1, "widget-hlist");

  array_widget in1= tmscm_to_array_widget (arg1);

  // TMSCM_DEFER_INTS;
  widget out= horizontal_list (in1);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_vlist (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_WIDGET (arg1, TMSCM_ARG1, "widget-vlist");

  array_widget in1= tmscm_to_array_widget (arg1);

  // TMSCM_DEFER_INTS;
  widget out= vertical_list (in1);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_aligned (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_ARRAY_WIDGET (arg1, TMSCM_ARG1, "widget-aligned");
  TMSCM_ASSERT_ARRAY_WIDGET (arg2, TMSCM_ARG2, "widget-aligned");

  array_widget in1= tmscm_to_array_widget (arg1);
  array_widget in2= tmscm_to_array_widget (arg2);

  // TMSCM_DEFER_INTS;
  widget out= aligned_widget (in1, in2);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_tabs (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_ARRAY_WIDGET (arg1, TMSCM_ARG1, "widget-tabs");
  TMSCM_ASSERT_ARRAY_WIDGET (arg2, TMSCM_ARG2, "widget-tabs");

  array_widget in1= tmscm_to_array_widget (arg1);
  array_widget in2= tmscm_to_array_widget (arg2);

  // TMSCM_DEFER_INTS;
  widget out= tabs_widget (in1, in2);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_scrollable (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_WIDGET (arg1, TMSCM_ARG1, "widget-scrollable");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "widget-scrollable");

  widget in1= tmscm_to_widget (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  widget out= user_canvas_widget (in1, in2);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_resize (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5, tmscm arg6, tmscm arg7, tmscm arg8) {
  TMSCM_ASSERT_WIDGET (arg1, TMSCM_ARG1, "widget-resize");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "widget-resize");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "widget-resize");
  TMSCM_ASSERT_STRING (arg4, TMSCM_ARG4, "widget-resize");
  TMSCM_ASSERT_STRING (arg5, TMSCM_ARG5, "widget-resize");
  TMSCM_ASSERT_STRING (arg6, TMSCM_ARG6, "widget-resize");
  TMSCM_ASSERT_STRING (arg7, TMSCM_ARG7, "widget-resize");
  TMSCM_ASSERT_STRING (arg8, TMSCM_ARG8, "widget-resize");

  widget in1= tmscm_to_widget (arg1);
  int in2= tmscm_to_int (arg2);
  string in3= tmscm_to_string (arg3);
  string in4= tmscm_to_string (arg4);
  string in5= tmscm_to_string (arg5);
  string in6= tmscm_to_string (arg6);
  string in7= tmscm_to_string (arg7);
  string in8= tmscm_to_string (arg8);

  // TMSCM_DEFER_INTS;
  widget out= resize_widget (in1, in2, in3, in4, in5, in6, in7, in8);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_hsplit (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_WIDGET (arg1, TMSCM_ARG1, "widget-hsplit");
  TMSCM_ASSERT_WIDGET (arg2, TMSCM_ARG2, "widget-hsplit");

  widget in1= tmscm_to_widget (arg1);
  widget in2= tmscm_to_widget (arg2);

  // TMSCM_DEFER_INTS;
  widget out= hsplit_widget (in1, in2);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_vsplit (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_WIDGET (arg1, TMSCM_ARG1, "widget-vsplit");
  TMSCM_ASSERT_WIDGET (arg2, TMSCM_ARG2, "widget-vsplit");

  widget in1= tmscm_to_widget (arg1);
  widget in2= tmscm_to_widget (arg2);

  // TMSCM_DEFER_INTS;
  widget out= vsplit_widget (in1, in2);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_texmacs_output (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "widget-texmacs-output");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  widget out= texmacs_output_widget (in1);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_texmacs_input (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "widget-texmacs-input");
  TMSCM_ASSERT_COMMAND (arg2, TMSCM_ARG2, "widget-texmacs-input");
  TMSCM_ASSERT_BOOL (arg3, TMSCM_ARG3, "widget-texmacs-input");

  content in1= tmscm_to_content (arg1);
  command in2= tmscm_to_command (arg2);
  bool in3= tmscm_to_bool (arg3);

  // TMSCM_DEFER_INTS;
  widget out= texmacs_input_widget (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_ink (tmscm arg1) {
  TMSCM_ASSERT_COMMAND (arg1, TMSCM_ARG1, "widget-ink");

  command in1= tmscm_to_command (arg1);

  // TMSCM_DEFER_INTS;
  widget out= ink_widget (in1);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_refresh (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "widget-refresh");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  widget out= refresh_widget (in1);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_object_2promise_widget (tmscm arg1) {
  TMSCM_ASSERT_OBJECT (arg1, TMSCM_ARG1, "object->promise-widget");

  object in1= tmscm_to_object (arg1);

  // TMSCM_DEFER_INTS;
  promise_widget out= as_promise_widget (in1);
  // TMSCM_ALLOW_INTS;

  return promise_widget_to_tmscm (out);
}

tmscm
tmg_tree_bounding_rectangle (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-bounding-rectangle");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  array_int out= get_bounding_rectangle (in1);
  // TMSCM_ALLOW_INTS;

  return array_int_to_tmscm (out);
}

tmscm
tmg_show_balloon (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_WIDGET (arg1, TMSCM_ARG1, "show-balloon");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "show-balloon");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "show-balloon");

  widget in1= tmscm_to_widget (arg1);
  int in2= tmscm_to_int (arg2);
  int in3= tmscm_to_int (arg3);

  // TMSCM_DEFER_INTS;
  show_help_balloon (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_window_handle () {
  // TMSCM_DEFER_INTS;
  int out= window_handle ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_window_create (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "window-create");
  TMSCM_ASSERT_WIDGET (arg2, TMSCM_ARG2, "window-create");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "window-create");
  TMSCM_ASSERT_BOOL (arg4, TMSCM_ARG4, "window-create");

  int in1= tmscm_to_int (arg1);
  widget in2= tmscm_to_widget (arg2);
  string in3= tmscm_to_string (arg3);
  bool in4= tmscm_to_bool (arg4);

  // TMSCM_DEFER_INTS;
  window_create (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_window_create_quit (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "window-create-quit");
  TMSCM_ASSERT_WIDGET (arg2, TMSCM_ARG2, "window-create-quit");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "window-create-quit");
  TMSCM_ASSERT_COMMAND (arg4, TMSCM_ARG4, "window-create-quit");

  int in1= tmscm_to_int (arg1);
  widget in2= tmscm_to_widget (arg2);
  string in3= tmscm_to_string (arg3);
  command in4= tmscm_to_command (arg4);

  // TMSCM_DEFER_INTS;
  window_create (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_window_delete (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "window-delete");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  window_delete (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_window_show (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "window-show");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  window_show (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_window_hide (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "window-hide");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  window_hide (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_bib_add_period (tmscm arg1) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "bib-add-period");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);

  // TMSCM_DEFER_INTS;
  scheme_tree out= bib_add_period (in1);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_bib_upcase_first (tmscm arg1) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "bib-upcase-first");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);

  // TMSCM_DEFER_INTS;
  scheme_tree out= bib_upcase_first (in1);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_bib_locase (tmscm arg1) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "bib-locase");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);

  // TMSCM_DEFER_INTS;
  scheme_tree out= bib_locase (in1);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_bib_upcase (tmscm arg1) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "bib-upcase");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);

  // TMSCM_DEFER_INTS;
  scheme_tree out= bib_upcase (in1);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_bib_default (tmscm arg1) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "bib-default");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);

  // TMSCM_DEFER_INTS;
  scheme_tree out= bib_default (in1);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_bib_purify (tmscm arg1) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "bib-purify");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);

  // TMSCM_DEFER_INTS;
  string out= bib_purify (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_bib_text_length (tmscm arg1) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "bib-text-length");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);

  // TMSCM_DEFER_INTS;
  int out= bib_text_length (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_bib_prefix (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "bib-prefix");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "bib-prefix");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  string out= bib_prefix (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_bib_emptyP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "bib-empty?");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "bib-empty?");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  bool out= bib_empty (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_bib_field (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "bib-field");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "bib-field");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  scheme_tree out= bib_field (in1, in2);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_bib_abbreviate (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "bib-abbreviate");
  TMSCM_ASSERT_SCHEME_TREE (arg2, TMSCM_ARG2, "bib-abbreviate");
  TMSCM_ASSERT_SCHEME_TREE (arg3, TMSCM_ARG3, "bib-abbreviate");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);
  scheme_tree in2= tmscm_to_scheme_tree (arg2);
  scheme_tree in3= tmscm_to_scheme_tree (arg3);

  // TMSCM_DEFER_INTS;
  scheme_tree out= bib_abbreviate (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

void
initialize_glue_basic () {
  tmscm_install_procedure ("texmacs-version-release",  tmg_texmacs_version_release, 1, 0, 0);
  tmscm_install_procedure ("version-before?",  tmg_version_beforeP, 2, 0, 0);
  tmscm_install_procedure ("os-win32?",  tmg_os_win32P, 0, 0, 0);
  tmscm_install_procedure ("os-mingw?",  tmg_os_mingwP, 0, 0, 0);
  tmscm_install_procedure ("os-macos?",  tmg_os_macosP, 0, 0, 0);
  tmscm_install_procedure ("x-gui?",  tmg_x_guiP, 0, 0, 0);
  tmscm_install_procedure ("qt-gui?",  tmg_qt_guiP, 0, 0, 0);
  tmscm_install_procedure ("default-look-and-feel",  tmg_default_look_and_feel, 0, 0, 0);
  tmscm_install_procedure ("default-chinese-font",  tmg_default_chinese_font, 0, 0, 0);
  tmscm_install_procedure ("default-japanese-font",  tmg_default_japanese_font, 0, 0, 0);
  tmscm_install_procedure ("default-korean-font",  tmg_default_korean_font, 0, 0, 0);
  tmscm_install_procedure ("tm-output",  tmg_tm_output, 1, 0, 0);
  tmscm_install_procedure ("tm-errput",  tmg_tm_errput, 1, 0, 0);
  tmscm_install_procedure ("win32-display",  tmg_win32_display, 1, 0, 0);
  tmscm_install_procedure ("cpp-error",  tmg_cpp_error, 0, 0, 0);
  tmscm_install_procedure ("scheme-dialect",  tmg_scheme_dialect, 0, 0, 0);
  tmscm_install_procedure ("get-texmacs-path",  tmg_get_texmacs_path, 0, 0, 0);
  tmscm_install_procedure ("plugin-list",  tmg_plugin_list, 0, 0, 0);
  tmscm_install_procedure ("set-fast-environments",  tmg_set_fast_environments, 1, 0, 0);
  tmscm_install_procedure ("font-exists-in-tt?",  tmg_font_exists_in_ttP, 1, 0, 0);
  tmscm_install_procedure ("eval-system",  tmg_eval_system, 1, 0, 0);
  tmscm_install_procedure ("var-eval-system",  tmg_var_eval_system, 1, 0, 0);
  tmscm_install_procedure ("get-locale-language",  tmg_get_locale_language, 0, 0, 0);
  tmscm_install_procedure ("texmacs-time",  tmg_texmacs_time, 0, 0, 0);
  tmscm_install_procedure ("texmacs-memory",  tmg_texmacs_memory, 0, 0, 0);
  tmscm_install_procedure ("bench-print",  tmg_bench_print, 1, 0, 0);
  tmscm_install_procedure ("bench-print-all",  tmg_bench_print_all, 0, 0, 0);
  tmscm_install_procedure ("system-wait",  tmg_system_wait, 2, 0, 0);
  tmscm_install_procedure ("set-bibtex-command",  tmg_set_bibtex_command, 1, 0, 0);
  tmscm_install_procedure ("math-symbol-group",  tmg_math_symbol_group, 1, 0, 0);
  tmscm_install_procedure ("math-group-members",  tmg_math_group_members, 1, 0, 0);
  tmscm_install_procedure ("math-symbol-type",  tmg_math_symbol_type, 1, 0, 0);
  tmscm_install_procedure ("object->command",  tmg_object_2command, 1, 0, 0);
  tmscm_install_procedure ("exec-delayed",  tmg_exec_delayed, 1, 0, 0);
  tmscm_install_procedure ("exec-delayed-pause",  tmg_exec_delayed_pause, 1, 0, 0);
  tmscm_install_procedure ("notify-preferences-loaded",  tmg_notify_preferences_loaded, 0, 0, 0);
  tmscm_install_procedure ("set-input-language",  tmg_set_input_language, 1, 0, 0);
  tmscm_install_procedure ("get-input-language",  tmg_get_input_language, 0, 0, 0);
  tmscm_install_procedure ("set-output-language",  tmg_set_output_language, 1, 0, 0);
  tmscm_install_procedure ("get-output-language",  tmg_get_output_language, 0, 0, 0);
  tmscm_install_procedure ("translate",  tmg_translate, 1, 0, 0);
  tmscm_install_procedure ("translate-from-to",  tmg_translate_from_to, 3, 0, 0);
  tmscm_install_procedure ("tree-translate",  tmg_tree_translate, 1, 0, 0);
  tmscm_install_procedure ("tree-translate-from-to",  tmg_tree_translate_from_to, 3, 0, 0);
  tmscm_install_procedure ("color",  tmg_color, 1, 0, 0);
  tmscm_install_procedure ("new-author",  tmg_new_author, 0, 0, 0);
  tmscm_install_procedure ("set-author",  tmg_set_author, 1, 0, 0);
  tmscm_install_procedure ("get-author",  tmg_get_author, 0, 0, 0);
  tmscm_install_procedure ("debug-set",  tmg_debug_set, 2, 0, 0);
  tmscm_install_procedure ("debug-get",  tmg_debug_get, 1, 0, 0);
  tmscm_install_procedure ("cout-buffer",  tmg_cout_buffer, 0, 0, 0);
  tmscm_install_procedure ("cout-unbuffer",  tmg_cout_unbuffer, 0, 0, 0);
  tmscm_install_procedure ("mark-new",  tmg_mark_new, 0, 0, 0);
  tmscm_install_procedure ("glyph-register",  tmg_glyph_register, 2, 0, 0);
  tmscm_install_procedure ("glyph-recognize",  tmg_glyph_recognize, 1, 0, 0);
  tmscm_install_procedure ("image->psdoc",  tmg_image_2psdoc, 1, 0, 0);
  tmscm_install_procedure ("tree->stree",  tmg_tree_2stree, 1, 0, 0);
  tmscm_install_procedure ("stree->tree",  tmg_stree_2tree, 1, 0, 0);
  tmscm_install_procedure ("tree->string",  tmg_tree_2string, 1, 0, 0);
  tmscm_install_procedure ("string->tree",  tmg_string_2tree, 1, 0, 0);
  tmscm_install_procedure ("tm->tree",  tmg_tm_2tree, 1, 0, 0);
  tmscm_install_procedure ("tree-atomic?",  tmg_tree_atomicP, 1, 0, 0);
  tmscm_install_procedure ("tree-compound?",  tmg_tree_compoundP, 1, 0, 0);
  tmscm_install_procedure ("tree-label",  tmg_tree_label, 1, 0, 0);
  tmscm_install_procedure ("tree-children",  tmg_tree_children, 1, 0, 0);
  tmscm_install_procedure ("tree-arity",  tmg_tree_arity, 1, 0, 0);
  tmscm_install_procedure ("tree-child-ref",  tmg_tree_child_ref, 2, 0, 0);
  tmscm_install_procedure ("tree-child-set!",  tmg_tree_child_setS, 3, 0, 0);
  tmscm_install_procedure ("tree-child-insert",  tmg_tree_child_insert, 3, 0, 0);
  tmscm_install_procedure ("tree-ip",  tmg_tree_ip, 1, 0, 0);
  tmscm_install_procedure ("tree-active?",  tmg_tree_activeP, 1, 0, 0);
  tmscm_install_procedure ("tree-eq?",  tmg_tree_eqP, 2, 0, 0);
  tmscm_install_procedure ("subtree",  tmg_subtree, 2, 0, 0);
  tmscm_install_procedure ("tree-range",  tmg_tree_range, 3, 0, 0);
  tmscm_install_procedure ("tree-copy",  tmg_tree_copy, 1, 0, 0);
  tmscm_install_procedure ("tree-append",  tmg_tree_append, 2, 0, 0);
  tmscm_install_procedure ("tree-right-index",  tmg_tree_right_index, 1, 0, 0);
  tmscm_install_procedure ("tree-label-extension?",  tmg_tree_label_extensionP, 1, 0, 0);
  tmscm_install_procedure ("tree-multi-paragraph?",  tmg_tree_multi_paragraphP, 1, 0, 0);
  tmscm_install_procedure ("tree-simplify",  tmg_tree_simplify, 1, 0, 0);
  tmscm_install_procedure ("tree-minimal-arity",  tmg_tree_minimal_arity, 1, 0, 0);
  tmscm_install_procedure ("tree-maximal-arity",  tmg_tree_maximal_arity, 1, 0, 0);
  tmscm_install_procedure ("tree-possible-arity?",  tmg_tree_possible_arityP, 2, 0, 0);
  tmscm_install_procedure ("tree-insert_point",  tmg_tree_insert_point, 2, 0, 0);
  tmscm_install_procedure ("tree-is-dynamic?",  tmg_tree_is_dynamicP, 1, 0, 0);
  tmscm_install_procedure ("tree-accessible-child?",  tmg_tree_accessible_childP, 2, 0, 0);
  tmscm_install_procedure ("tree-accessible-children",  tmg_tree_accessible_children, 1, 0, 0);
  tmscm_install_procedure ("tree-all-accessible?",  tmg_tree_all_accessibleP, 1, 0, 0);
  tmscm_install_procedure ("tree-none-accessible?",  tmg_tree_none_accessibleP, 1, 0, 0);
  tmscm_install_procedure ("tree-name",  tmg_tree_name, 1, 0, 0);
  tmscm_install_procedure ("tree-long-name",  tmg_tree_long_name, 1, 0, 0);
  tmscm_install_procedure ("tree-child-name",  tmg_tree_child_name, 2, 0, 0);
  tmscm_install_procedure ("tree-child-long-name",  tmg_tree_child_long_name, 2, 0, 0);
  tmscm_install_procedure ("tree-child-type",  tmg_tree_child_type, 2, 0, 0);
  tmscm_install_procedure ("tree-child-env",  tmg_tree_child_env, 4, 0, 0);
  tmscm_install_procedure ("tree-load-inclusion",  tmg_tree_load_inclusion, 1, 0, 0);
  tmscm_install_procedure ("tree-as-string",  tmg_tree_as_string, 1, 0, 0);
  tmscm_install_procedure ("tree-extents",  tmg_tree_extents, 1, 0, 0);
  tmscm_install_procedure ("tree-empty?",  tmg_tree_emptyP, 1, 0, 0);
  tmscm_install_procedure ("tree-is-buffer?",  tmg_tree_is_bufferP, 1, 0, 0);
  tmscm_install_procedure ("tree-search-sections",  tmg_tree_search_sections, 1, 0, 0);
  tmscm_install_procedure ("tree-assign",  tmg_tree_assign, 2, 0, 0);
  tmscm_install_procedure ("tree-var-insert",  tmg_tree_var_insert, 3, 0, 0);
  tmscm_install_procedure ("tree-remove",  tmg_tree_remove, 3, 0, 0);
  tmscm_install_procedure ("tree-split",  tmg_tree_split, 3, 0, 0);
  tmscm_install_procedure ("tree-join",  tmg_tree_join, 2, 0, 0);
  tmscm_install_procedure ("tree-assign-node",  tmg_tree_assign_node, 2, 0, 0);
  tmscm_install_procedure ("tree-insert-node",  tmg_tree_insert_node, 3, 0, 0);
  tmscm_install_procedure ("tree-remove-node",  tmg_tree_remove_node, 2, 0, 0);
  tmscm_install_procedure ("cpp-tree-correct-node",  tmg_cpp_tree_correct_node, 1, 0, 0);
  tmscm_install_procedure ("cpp-tree-correct-downwards",  tmg_cpp_tree_correct_downwards, 1, 0, 0);
  tmscm_install_procedure ("cpp-tree-correct-upwards",  tmg_cpp_tree_correct_upwards, 1, 0, 0);
  tmscm_install_procedure ("concat-tokenize-math",  tmg_concat_tokenize_math, 1, 0, 0);
  tmscm_install_procedure ("concat-decompose",  tmg_concat_decompose, 1, 0, 0);
  tmscm_install_procedure ("concat-recompose",  tmg_concat_recompose, 1, 0, 0);
  tmscm_install_procedure ("with-like?",  tmg_with_likeP, 1, 0, 0);
  tmscm_install_procedure ("with-same-type?",  tmg_with_same_typeP, 2, 0, 0);
  tmscm_install_procedure ("with-similar-type?",  tmg_with_similar_typeP, 2, 0, 0);
  tmscm_install_procedure ("with-correct",  tmg_with_correct, 1, 0, 0);
  tmscm_install_procedure ("with-correct-superfluous",  tmg_with_correct_superfluous, 1, 0, 0);
  tmscm_install_procedure ("invisible-correct-superfluous",  tmg_invisible_correct_superfluous, 1, 0, 0);
  tmscm_install_procedure ("invisible-correct-missing",  tmg_invisible_correct_missing, 2, 0, 0);
  tmscm_install_procedure ("automatic-correct",  tmg_automatic_correct, 2, 0, 0);
  tmscm_install_procedure ("manual-correct",  tmg_manual_correct, 1, 0, 0);
  tmscm_install_procedure ("tree-upgrade-brackets",  tmg_tree_upgrade_brackets, 2, 0, 0);
  tmscm_install_procedure ("tree-upgrade-big",  tmg_tree_upgrade_big, 1, 0, 0);
  tmscm_install_procedure ("tree-downgrade-brackets",  tmg_tree_downgrade_brackets, 3, 0, 0);
  tmscm_install_procedure ("tree-downgrade-big",  tmg_tree_downgrade_big, 1, 0, 0);
  tmscm_install_procedure ("math-status-print",  tmg_math_status_print, 0, 0, 0);
  tmscm_install_procedure ("math-status-reset",  tmg_math_status_reset, 0, 0, 0);
  tmscm_install_procedure ("path-inf?",  tmg_path_infP, 2, 0, 0);
  tmscm_install_procedure ("path-inf-eq?",  tmg_path_inf_eqP, 2, 0, 0);
  tmscm_install_procedure ("path-less?",  tmg_path_lessP, 2, 0, 0);
  tmscm_install_procedure ("path-less-eq?",  tmg_path_less_eqP, 2, 0, 0);
  tmscm_install_procedure ("path-start",  tmg_path_start, 2, 0, 0);
  tmscm_install_procedure ("path-end",  tmg_path_end, 2, 0, 0);
  tmscm_install_procedure ("path-next",  tmg_path_next, 2, 0, 0);
  tmscm_install_procedure ("path-previous",  tmg_path_previous, 2, 0, 0);
  tmscm_install_procedure ("path-next-word",  tmg_path_next_word, 2, 0, 0);
  tmscm_install_procedure ("path-previous-word",  tmg_path_previous_word, 2, 0, 0);
  tmscm_install_procedure ("path-next-node",  tmg_path_next_node, 2, 0, 0);
  tmscm_install_procedure ("path-previous-node",  tmg_path_previous_node, 2, 0, 0);
  tmscm_install_procedure ("path-next-tag",  tmg_path_next_tag, 3, 0, 0);
  tmscm_install_procedure ("path-previous-tag",  tmg_path_previous_tag, 3, 0, 0);
  tmscm_install_procedure ("path-next-tag-same-argument",  tmg_path_next_tag_same_argument, 3, 0, 0);
  tmscm_install_procedure ("path-previous-tag-same-argument",  tmg_path_previous_tag_same_argument, 3, 0, 0);
  tmscm_install_procedure ("path-next-argument",  tmg_path_next_argument, 2, 0, 0);
  tmscm_install_procedure ("path-previous-argument",  tmg_path_previous_argument, 2, 0, 0);
  tmscm_install_procedure ("path-previous-section",  tmg_path_previous_section, 2, 0, 0);
  tmscm_install_procedure ("tree->ids",  tmg_tree_2ids, 1, 0, 0);
  tmscm_install_procedure ("id->trees",  tmg_id_2trees, 1, 0, 0);
  tmscm_install_procedure ("vertex->links",  tmg_vertex_2links, 1, 0, 0);
  tmscm_install_procedure ("tree->tree-pointer",  tmg_tree_2tree_pointer, 1, 0, 0);
  tmscm_install_procedure ("tree-pointer-detach",  tmg_tree_pointer_detach, 1, 0, 0);
  tmscm_install_procedure ("tree-pointer->tree",  tmg_tree_pointer_2tree, 1, 0, 0);
  tmscm_install_procedure ("current-link-types",  tmg_current_link_types, 0, 0, 0);
  tmscm_install_procedure ("get-locus-rendering",  tmg_get_locus_rendering, 1, 0, 0);
  tmscm_install_procedure ("set-locus-rendering",  tmg_set_locus_rendering, 2, 0, 0);
  tmscm_install_procedure ("declare-visited",  tmg_declare_visited, 1, 0, 0);
  tmscm_install_procedure ("has-been-visited?",  tmg_has_been_visitedP, 1, 0, 0);
  tmscm_install_procedure ("graphics-set",  tmg_graphics_set, 2, 0, 0);
  tmscm_install_procedure ("graphics-has?",  tmg_graphics_hasP, 1, 0, 0);
  tmscm_install_procedure ("graphics-ref",  tmg_graphics_ref, 1, 0, 0);
  tmscm_install_procedure ("graphics-needs-update?",  tmg_graphics_needs_updateP, 0, 0, 0);
  tmscm_install_procedure ("graphics-notify-update",  tmg_graphics_notify_update, 1, 0, 0);
  tmscm_install_procedure ("string-number?",  tmg_string_numberP, 1, 0, 0);
  tmscm_install_procedure ("string-occurs?",  tmg_string_occursP, 2, 0, 0);
  tmscm_install_procedure ("string-search-forwards",  tmg_string_search_forwards, 3, 0, 0);
  tmscm_install_procedure ("string-search-backwards",  tmg_string_search_backwards, 3, 0, 0);
  tmscm_install_procedure ("string-replace",  tmg_string_replace, 3, 0, 0);
  tmscm_install_procedure ("string-alpha?",  tmg_string_alphaP, 1, 0, 0);
  tmscm_install_procedure ("string-locase-alpha?",  tmg_string_locase_alphaP, 1, 0, 0);
  tmscm_install_procedure ("upcase-first",  tmg_upcase_first, 1, 0, 0);
  tmscm_install_procedure ("locase-first",  tmg_locase_first, 1, 0, 0);
  tmscm_install_procedure ("upcase-all",  tmg_upcase_all, 1, 0, 0);
  tmscm_install_procedure ("locase-all",  tmg_locase_all, 1, 0, 0);
  tmscm_install_procedure ("string-union",  tmg_string_union, 2, 0, 0);
  tmscm_install_procedure ("string-minus",  tmg_string_minus, 2, 0, 0);
  tmscm_install_procedure ("escape-generic",  tmg_escape_generic, 1, 0, 0);
  tmscm_install_procedure ("escape-verbatim",  tmg_escape_verbatim, 1, 0, 0);
  tmscm_install_procedure ("escape-shell",  tmg_escape_shell, 1, 0, 0);
  tmscm_install_procedure ("string-convert",  tmg_string_convert, 3, 0, 0);
  tmscm_install_procedure ("utf8->cork",  tmg_utf8_2cork, 1, 0, 0);
  tmscm_install_procedure ("cork->utf8",  tmg_cork_2utf8, 1, 0, 0);
  tmscm_install_procedure ("utf8->html",  tmg_utf8_2html, 1, 0, 0);
  tmscm_install_procedure ("tm->xml-name",  tmg_tm_2xml_name, 1, 0, 0);
  tmscm_install_procedure ("old-tm->xml-cdata",  tmg_old_tm_2xml_cdata, 1, 0, 0);
  tmscm_install_procedure ("tm->xml-cdata",  tmg_tm_2xml_cdata, 1, 0, 0);
  tmscm_install_procedure ("xml-name->tm",  tmg_xml_name_2tm, 1, 0, 0);
  tmscm_install_procedure ("old-xml-cdata->tm",  tmg_old_xml_cdata_2tm, 1, 0, 0);
  tmscm_install_procedure ("xml-unspace",  tmg_xml_unspace, 3, 0, 0);
  tmscm_install_procedure ("integer->hexadecimal",  tmg_integer_2hexadecimal, 1, 0, 0);
  tmscm_install_procedure ("integer->padded-hexadecimal",  tmg_integer_2padded_hexadecimal, 2, 0, 0);
  tmscm_install_procedure ("hexadecimal->integer",  tmg_hexadecimal_2integer, 1, 0, 0);
  tmscm_install_procedure ("string->tmstring",  tmg_string_2tmstring, 1, 0, 0);
  tmscm_install_procedure ("tmstring->string",  tmg_tmstring_2string, 1, 0, 0);
  tmscm_install_procedure ("tmstring-length",  tmg_tmstring_length, 1, 0, 0);
  tmscm_install_procedure ("tmstring-ref",  tmg_tmstring_ref, 2, 0, 0);
  tmscm_install_procedure ("tmstring-reverse-ref",  tmg_tmstring_reverse_ref, 2, 0, 0);
  tmscm_install_procedure ("tmstring->list",  tmg_tmstring_2list, 1, 0, 0);
  tmscm_install_procedure ("list->tmstring",  tmg_list_2tmstring, 1, 0, 0);
  tmscm_install_procedure ("string-next",  tmg_string_next, 2, 0, 0);
  tmscm_install_procedure ("string-previous",  tmg_string_previous, 2, 0, 0);
  tmscm_install_procedure ("packrat-define",  tmg_packrat_define, 3, 0, 0);
  tmscm_install_procedure ("packrat-property",  tmg_packrat_property, 4, 0, 0);
  tmscm_install_procedure ("packrat-inherit",  tmg_packrat_inherit, 2, 0, 0);
  tmscm_install_procedure ("packrat-parse",  tmg_packrat_parse, 3, 0, 0);
  tmscm_install_procedure ("packrat-correct?",  tmg_packrat_correctP, 3, 0, 0);
  tmscm_install_procedure ("packrat-context",  tmg_packrat_context, 4, 0, 0);
  tmscm_install_procedure ("parse-texmacs",  tmg_parse_texmacs, 1, 0, 0);
  tmscm_install_procedure ("serialize-texmacs",  tmg_serialize_texmacs, 1, 0, 0);
  tmscm_install_procedure ("parse-texmacs-snippet",  tmg_parse_texmacs_snippet, 1, 0, 0);
  tmscm_install_procedure ("serialize-texmacs-snippet",  tmg_serialize_texmacs_snippet, 1, 0, 0);
  tmscm_install_procedure ("texmacs->stm",  tmg_texmacs_2stm, 1, 0, 0);
  tmscm_install_procedure ("stm->texmacs",  tmg_stm_2texmacs, 1, 0, 0);
  tmscm_install_procedure ("stm-snippet->texmacs",  tmg_stm_snippet_2texmacs, 1, 0, 0);
  tmscm_install_procedure ("cpp-texmacs->verbatim",  tmg_cpp_texmacs_2verbatim, 3, 0, 0);
  tmscm_install_procedure ("cpp-verbatim-snippet->texmacs",  tmg_cpp_verbatim_snippet_2texmacs, 3, 0, 0);
  tmscm_install_procedure ("cpp-verbatim->texmacs",  tmg_cpp_verbatim_2texmacs, 3, 0, 0);
  tmscm_install_procedure ("parse-latex",  tmg_parse_latex, 1, 0, 0);
  tmscm_install_procedure ("parse-latex-document",  tmg_parse_latex_document, 1, 0, 0);
  tmscm_install_procedure ("latex->texmacs",  tmg_latex_2texmacs, 1, 0, 0);
  tmscm_install_procedure ("latex-document->texmacs",  tmg_latex_document_2texmacs, 1, 0, 0);
  tmscm_install_procedure ("latex-class-document->texmacs",  tmg_latex_class_document_2texmacs, 1, 0, 0);
  tmscm_install_procedure ("parse-xml",  tmg_parse_xml, 1, 0, 0);
  tmscm_install_procedure ("parse-html",  tmg_parse_html, 1, 0, 0);
  tmscm_install_procedure ("parse-bib",  tmg_parse_bib, 1, 0, 0);
  tmscm_install_procedure ("upgrade-tmml",  tmg_upgrade_tmml, 1, 0, 0);
  tmscm_install_procedure ("upgrade-mathml",  tmg_upgrade_mathml, 1, 0, 0);
  tmscm_install_procedure ("string->url",  tmg_string_2url, 1, 0, 0);
  tmscm_install_procedure ("url",  tmg_url, 2, 0, 0);
  tmscm_install_procedure ("url-system",  tmg_url_system, 1, 0, 0);
  tmscm_install_procedure ("url-none",  tmg_url_none, 0, 0, 0);
  tmscm_install_procedure ("url-any",  tmg_url_any, 0, 0, 0);
  tmscm_install_procedure ("url-wildcard",  tmg_url_wildcard, 1, 0, 0);
  tmscm_install_procedure ("url-parent",  tmg_url_parent, 0, 0, 0);
  tmscm_install_procedure ("url-ancestor",  tmg_url_ancestor, 0, 0, 0);
  tmscm_install_procedure ("url-append",  tmg_url_append, 2, 0, 0);
  tmscm_install_procedure ("url-or",  tmg_url_or, 2, 0, 0);
  tmscm_install_procedure ("url->string",  tmg_url_2string, 1, 0, 0);
  tmscm_install_procedure ("url-none?",  tmg_url_noneP, 1, 0, 0);
  tmscm_install_procedure ("url-rooted-web?",  tmg_url_rooted_webP, 1, 0, 0);
  tmscm_install_procedure ("url-concat?",  tmg_url_concatP, 1, 0, 0);
  tmscm_install_procedure ("url-or?",  tmg_url_orP, 1, 0, 0);
  tmscm_install_procedure ("url-ref",  tmg_url_ref, 2, 0, 0);
  tmscm_install_procedure ("url-head",  tmg_url_head, 1, 0, 0);
  tmscm_install_procedure ("url-tail",  tmg_url_tail, 1, 0, 0);
  tmscm_install_procedure ("url-suffix",  tmg_url_suffix, 1, 0, 0);
  tmscm_install_procedure ("url-glue",  tmg_url_glue, 2, 0, 0);
  tmscm_install_procedure ("url-unglue",  tmg_url_unglue, 2, 0, 0);
  tmscm_install_procedure ("url-relative",  tmg_url_relative, 2, 0, 0);
  tmscm_install_procedure ("url-expand",  tmg_url_expand, 1, 0, 0);
  tmscm_install_procedure ("url-factor",  tmg_url_factor, 1, 0, 0);
  tmscm_install_procedure ("url-delta",  tmg_url_delta, 2, 0, 0);
  tmscm_install_procedure ("url-secure?",  tmg_url_secureP, 1, 0, 0);
  tmscm_install_procedure ("url-descends?",  tmg_url_descendsP, 2, 0, 0);
  tmscm_install_procedure ("url-complete",  tmg_url_complete, 2, 0, 0);
  tmscm_install_procedure ("url-resolve",  tmg_url_resolve, 2, 0, 0);
  tmscm_install_procedure ("url-resolve-in-path",  tmg_url_resolve_in_path, 1, 0, 0);
  tmscm_install_procedure ("url-exists?",  tmg_url_existsP, 1, 0, 0);
  tmscm_install_procedure ("url-exists-in-path?",  tmg_url_exists_in_pathP, 1, 0, 0);
  tmscm_install_procedure ("url-exists-in-tex?",  tmg_url_exists_in_texP, 1, 0, 0);
  tmscm_install_procedure ("url-concretize",  tmg_url_concretize, 1, 0, 0);
  tmscm_install_procedure ("url-materialize",  tmg_url_materialize, 2, 0, 0);
  tmscm_install_procedure ("url-test?",  tmg_url_testP, 2, 0, 0);
  tmscm_install_procedure ("url-regular?",  tmg_url_regularP, 1, 0, 0);
  tmscm_install_procedure ("url-directory?",  tmg_url_directoryP, 1, 0, 0);
  tmscm_install_procedure ("url-link?",  tmg_url_linkP, 1, 0, 0);
  tmscm_install_procedure ("url-newer?",  tmg_url_newerP, 2, 0, 0);
  tmscm_install_procedure ("url-last-modified",  tmg_url_last_modified, 1, 0, 0);
  tmscm_install_procedure ("url-temp",  tmg_url_temp, 0, 0, 0);
  tmscm_install_procedure ("url-scratch",  tmg_url_scratch, 3, 0, 0);
  tmscm_install_procedure ("url-scratch?",  tmg_url_scratchP, 1, 0, 0);
  tmscm_install_procedure ("string-save",  tmg_string_save, 2, 0, 0);
  tmscm_install_procedure ("string-load",  tmg_string_load, 1, 0, 0);
  tmscm_install_procedure ("system-move",  tmg_system_move, 2, 0, 0);
  tmscm_install_procedure ("system-copy",  tmg_system_copy, 2, 0, 0);
  tmscm_install_procedure ("system-remove",  tmg_system_remove, 1, 0, 0);
  tmscm_install_procedure ("system-mkdir",  tmg_system_mkdir, 1, 0, 0);
  tmscm_install_procedure ("system-search-score",  tmg_system_search_score, 2, 0, 0);
  tmscm_install_procedure ("system-1",  tmg_system_1, 2, 0, 0);
  tmscm_install_procedure ("system-2",  tmg_system_2, 3, 0, 0);
  tmscm_install_procedure ("tmfs-set",  tmg_tmfs_set, 2, 0, 0);
  tmscm_install_procedure ("tmfs-reset",  tmg_tmfs_reset, 2, 0, 0);
  tmscm_install_procedure ("tmfs-get",  tmg_tmfs_get, 1, 0, 0);
  tmscm_install_procedure ("tmfs-new-save",  tmg_tmfs_new_save, 2, 0, 0);
  tmscm_install_procedure ("tmfs-new-remove",  tmg_tmfs_new_remove, 1, 0, 0);
  tmscm_install_procedure ("tmfs-new-load",  tmg_tmfs_new_load, 1, 0, 0);
  tmscm_install_procedure ("tmfs-create-ressource",  tmg_tmfs_create_ressource, 0, 0, 0);
  tmscm_install_procedure ("tmfs-ressource-head",  tmg_tmfs_ressource_head, 1, 0, 0);
  tmscm_install_procedure ("tmfs-ressource-versions",  tmg_tmfs_ressource_versions, 1, 0, 0);
  tmscm_install_procedure ("tmfs-save-ressource",  tmg_tmfs_save_ressource, 3, 0, 0);
  tmscm_install_procedure ("tmfs-load-ressource-file",  tmg_tmfs_load_ressource_file, 1, 0, 0);
  tmscm_install_procedure ("tmfs-load-ressource-properties",  tmg_tmfs_load_ressource_properties, 1, 0, 0);
  tmscm_install_procedure ("tmfs-create-user",  tmg_tmfs_create_user, 1, 0, 0);
  tmscm_install_procedure ("tmfs-search-user",  tmg_tmfs_search_user, 1, 0, 0);
  tmscm_install_procedure ("tmfs-set-user",  tmg_tmfs_set_user, 1, 0, 0);
  tmscm_install_procedure ("tmfs-get-user",  tmg_tmfs_get_user, 0, 0, 0);
  tmscm_install_procedure ("tmfs-allows?",  tmg_tmfs_allowsP, 2, 0, 0);
  tmscm_install_procedure ("tmfs-set-attributes",  tmg_tmfs_set_attributes, 2, 0, 0);
  tmscm_install_procedure ("tmfs-get-attributes",  tmg_tmfs_get_attributes, 1, 0, 0);
  tmscm_install_procedure ("tmfs-add-attributes",  tmg_tmfs_add_attributes, 2, 0, 0);
  tmscm_install_procedure ("tmfs-remove-attributes",  tmg_tmfs_remove_attributes, 2, 0, 0);
  tmscm_install_procedure ("tmfs-change-attributes",  tmg_tmfs_change_attributes, 2, 0, 0);
  tmscm_install_procedure ("tmfs-query",  tmg_tmfs_query, 1, 0, 0);
  tmscm_install_procedure ("solutions->collection",  tmg_solutions_2collection, 2, 0, 0);
  tmscm_install_procedure ("tmfs-create-file",  tmg_tmfs_create_file, 2, 0, 0);
  tmscm_install_procedure ("tmfs-create-file-in",  tmg_tmfs_create_file_in, 3, 0, 0);
  tmscm_install_procedure ("tmfs-search-file",  tmg_tmfs_search_file, 1, 0, 0);
  tmscm_install_procedure ("tmfs-save-file",  tmg_tmfs_save_file, 2, 0, 0);
  tmscm_install_procedure ("tmfs-load-file",  tmg_tmfs_load_file, 1, 0, 0);
  tmscm_install_procedure ("tmfs-create-project",  tmg_tmfs_create_project, 1, 0, 0);
  tmscm_install_procedure ("tmfs-search-project",  tmg_tmfs_search_project, 1, 0, 0);
  tmscm_install_procedure ("tmfs-get-file-projects",  tmg_tmfs_get_file_projects, 1, 0, 0);
  tmscm_install_procedure ("tmfs-get-project-files",  tmg_tmfs_get_project_files, 1, 0, 0);
  tmscm_install_procedure ("tmfs-create-branch",  tmg_tmfs_create_branch, 2, 0, 0);
  tmscm_install_procedure ("tmfs-set-root",  tmg_tmfs_set_root, 2, 0, 0);
  tmscm_install_procedure ("tmfs-get-root",  tmg_tmfs_get_root, 1, 0, 0);
  tmscm_install_procedure ("tmfs-import",  tmg_tmfs_import, 1, 0, 0);
  tmscm_install_procedure ("tmfs-export",  tmg_tmfs_export, 1, 0, 0);
  tmscm_install_procedure ("server-start",  tmg_server_start, 0, 0, 0);
  tmscm_install_procedure ("server-stop",  tmg_server_stop, 0, 0, 0);
  tmscm_install_procedure ("server-read",  tmg_server_read, 1, 0, 0);
  tmscm_install_procedure ("server-write",  tmg_server_write, 2, 0, 0);
  tmscm_install_procedure ("client-start",  tmg_client_start, 1, 0, 0);
  tmscm_install_procedure ("client-stop",  tmg_client_stop, 0, 0, 0);
  tmscm_install_procedure ("client-read",  tmg_client_read, 0, 0, 0);
  tmscm_install_procedure ("client-write",  tmg_client_write, 1, 0, 0);
  tmscm_install_procedure ("enter-secure-mode",  tmg_enter_secure_mode, 0, 0, 0);
  tmscm_install_procedure ("connection-start",  tmg_connection_start, 2, 0, 0);
  tmscm_install_procedure ("connection-status",  tmg_connection_status, 2, 0, 0);
  tmscm_install_procedure ("connection-write-string",  tmg_connection_write_string, 3, 0, 0);
  tmscm_install_procedure ("connection-write",  tmg_connection_write, 3, 0, 0);
  tmscm_install_procedure ("connection-cmd",  tmg_connection_cmd, 3, 0, 0);
  tmscm_install_procedure ("connection-eval",  tmg_connection_eval, 3, 0, 0);
  tmscm_install_procedure ("connection-interrupt",  tmg_connection_interrupt, 2, 0, 0);
  tmscm_install_procedure ("connection-stop",  tmg_connection_stop, 2, 0, 0);
  tmscm_install_procedure ("widget-printer",  tmg_widget_printer, 2, 0, 0);
  tmscm_install_procedure ("widget-color-picker",  tmg_widget_color_picker, 3, 0, 0);
  tmscm_install_procedure ("widget-extend",  tmg_widget_extend, 2, 0, 0);
  tmscm_install_procedure ("widget-hmenu",  tmg_widget_hmenu, 1, 0, 0);
  tmscm_install_procedure ("widget-vmenu",  tmg_widget_vmenu, 1, 0, 0);
  tmscm_install_procedure ("widget-tmenu",  tmg_widget_tmenu, 2, 0, 0);
  tmscm_install_procedure ("widget-minibar-menu",  tmg_widget_minibar_menu, 1, 0, 0);
  tmscm_install_procedure ("widget-separator",  tmg_widget_separator, 1, 0, 0);
  tmscm_install_procedure ("widget-menu-group",  tmg_widget_menu_group, 2, 0, 0);
  tmscm_install_procedure ("widget-pulldown-button",  tmg_widget_pulldown_button, 2, 0, 0);
  tmscm_install_procedure ("widget-pullright-button",  tmg_widget_pullright_button, 2, 0, 0);
  tmscm_install_procedure ("widget-menu-button",  tmg_widget_menu_button, 5, 0, 0);
  tmscm_install_procedure ("widget-toggle",  tmg_widget_toggle, 3, 0, 0);
  tmscm_install_procedure ("widget-balloon",  tmg_widget_balloon, 2, 0, 0);
  tmscm_install_procedure ("widget-empty",  tmg_widget_empty, 0, 0, 0);
  tmscm_install_procedure ("widget-text",  tmg_widget_text, 4, 0, 0);
  tmscm_install_procedure ("widget-input",  tmg_widget_input, 5, 0, 0);
  tmscm_install_procedure ("widget-enum",  tmg_widget_enum, 5, 0, 0);
  tmscm_install_procedure ("widget-choice",  tmg_widget_choice, 3, 0, 0);
  tmscm_install_procedure ("widget-choices",  tmg_widget_choices, 3, 0, 0);
  tmscm_install_procedure ("widget-xpm",  tmg_widget_xpm, 1, 0, 0);
  tmscm_install_procedure ("widget-box",  tmg_widget_box, 5, 0, 0);
  tmscm_install_procedure ("widget-glue",  tmg_widget_glue, 4, 0, 0);
  tmscm_install_procedure ("widget-color",  tmg_widget_color, 5, 0, 0);
  tmscm_install_procedure ("widget-hlist",  tmg_widget_hlist, 1, 0, 0);
  tmscm_install_procedure ("widget-vlist",  tmg_widget_vlist, 1, 0, 0);
  tmscm_install_procedure ("widget-aligned",  tmg_widget_aligned, 2, 0, 0);
  tmscm_install_procedure ("widget-tabs",  tmg_widget_tabs, 2, 0, 0);
  tmscm_install_procedure ("widget-scrollable",  tmg_widget_scrollable, 2, 0, 0);
  tmscm_install_procedure ("widget-resize",  tmg_widget_resize, 8, 0, 0);
  tmscm_install_procedure ("widget-hsplit",  tmg_widget_hsplit, 2, 0, 0);
  tmscm_install_procedure ("widget-vsplit",  tmg_widget_vsplit, 2, 0, 0);
  tmscm_install_procedure ("widget-texmacs-output",  tmg_widget_texmacs_output, 1, 0, 0);
  tmscm_install_procedure ("widget-texmacs-input",  tmg_widget_texmacs_input, 3, 0, 0);
  tmscm_install_procedure ("widget-ink",  tmg_widget_ink, 1, 0, 0);
  tmscm_install_procedure ("widget-refresh",  tmg_widget_refresh, 1, 0, 0);
  tmscm_install_procedure ("object->promise-widget",  tmg_object_2promise_widget, 1, 0, 0);
  tmscm_install_procedure ("tree-bounding-rectangle",  tmg_tree_bounding_rectangle, 1, 0, 0);
  tmscm_install_procedure ("show-balloon",  tmg_show_balloon, 3, 0, 0);
  tmscm_install_procedure ("window-handle",  tmg_window_handle, 0, 0, 0);
  tmscm_install_procedure ("window-create",  tmg_window_create, 4, 0, 0);
  tmscm_install_procedure ("window-create-quit",  tmg_window_create_quit, 4, 0, 0);
  tmscm_install_procedure ("window-delete",  tmg_window_delete, 1, 0, 0);
  tmscm_install_procedure ("window-show",  tmg_window_show, 1, 0, 0);
  tmscm_install_procedure ("window-hide",  tmg_window_hide, 1, 0, 0);
  tmscm_install_procedure ("bib-add-period",  tmg_bib_add_period, 1, 0, 0);
  tmscm_install_procedure ("bib-upcase-first",  tmg_bib_upcase_first, 1, 0, 0);
  tmscm_install_procedure ("bib-locase",  tmg_bib_locase, 1, 0, 0);
  tmscm_install_procedure ("bib-upcase",  tmg_bib_upcase, 1, 0, 0);
  tmscm_install_procedure ("bib-default",  tmg_bib_default, 1, 0, 0);
  tmscm_install_procedure ("bib-purify",  tmg_bib_purify, 1, 0, 0);
  tmscm_install_procedure ("bib-text-length",  tmg_bib_text_length, 1, 0, 0);
  tmscm_install_procedure ("bib-prefix",  tmg_bib_prefix, 2, 0, 0);
  tmscm_install_procedure ("bib-empty?",  tmg_bib_emptyP, 2, 0, 0);
  tmscm_install_procedure ("bib-field",  tmg_bib_field, 2, 0, 0);
  tmscm_install_procedure ("bib-abbreviate",  tmg_bib_abbreviate, 3, 0, 0);
}
