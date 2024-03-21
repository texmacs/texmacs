
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
tmg_updater_supportedP () {
  // TMSCM_DEFER_INTS;
  bool out= updater_supported ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_updater_runningP () {
  // TMSCM_DEFER_INTS;
  bool out= updater_is_running ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_updater_check_background () {
  // TMSCM_DEFER_INTS;
  bool out= updater_check_background ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_updater_check_foreground () {
  // TMSCM_DEFER_INTS;
  bool out= updater_check_foreground ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_updater_last_check () {
  // TMSCM_DEFER_INTS;
  long out= updater_last_check ();
  // TMSCM_ALLOW_INTS;

  return long_to_tmscm (out);
}

tmscm
tmg_updater_set_interval (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "updater-set-interval");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  bool out= updater_set_interval (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_get_original_path () {
  // TMSCM_DEFER_INTS;
  string out= get_original_path ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
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
tmg_has_printing_cmdP () {
  // TMSCM_DEFER_INTS;
  bool out= has_printing_cmd ();
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
tmg_gui_version () {
  // TMSCM_DEFER_INTS;
  string out= gui_version ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
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
tmg_get_retina_factor () {
  // TMSCM_DEFER_INTS;
  int out= get_retina_factor ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_get_retina_zoom () {
  // TMSCM_DEFER_INTS;
  int out= get_retina_zoom ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_get_retina_icons () {
  // TMSCM_DEFER_INTS;
  int out= get_retina_icons ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_get_retina_scale () {
  // TMSCM_DEFER_INTS;
  double out= get_retina_scale ();
  // TMSCM_ALLOW_INTS;

  return double_to_tmscm (out);
}

tmscm
tmg_set_retina_factor (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "set-retina-factor");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  set_retina_factor (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_retina_zoom (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "set-retina-zoom");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  set_retina_zoom (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_retina_icons (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "set-retina-icons");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  set_retina_icons (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_retina_scale (tmscm arg1) {
  TMSCM_ASSERT_DOUBLE (arg1, TMSCM_ARG1, "set-retina-scale");

  double in1= tmscm_to_double (arg1);

  // TMSCM_DEFER_INTS;
  set_retina_scale (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
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
tmg_supports_native_pdfP () {
  // TMSCM_DEFER_INTS;
  bool out= supports_native_pdf ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_supports_ghostscriptP () {
  // TMSCM_DEFER_INTS;
  bool out= supports_ghostscript ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_rescue_modeP () {
  // TMSCM_DEFER_INTS;
  bool out= in_rescue_mode ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
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
  url out= get_texmacs_path ();
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_get_texmacs_home_path () {
  // TMSCM_DEFER_INTS;
  url out= get_texmacs_home_path ();
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_get_user_login () {
  // TMSCM_DEFER_INTS;
  string out= get_user_login ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_get_user_name () {
  // TMSCM_DEFER_INTS;
  string out= get_user_name ();
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
tmg_evaluate_system (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_ARRAY_STRING (arg1, TMSCM_ARG1, "evaluate-system");
  TMSCM_ASSERT_ARRAY_INT (arg2, TMSCM_ARG2, "evaluate-system");
  TMSCM_ASSERT_ARRAY_STRING (arg3, TMSCM_ARG3, "evaluate-system");
  TMSCM_ASSERT_ARRAY_INT (arg4, TMSCM_ARG4, "evaluate-system");

  array_string in1= tmscm_to_array_string (arg1);
  array_int in2= tmscm_to_array_int (arg2);
  array_string in3= tmscm_to_array_string (arg3);
  array_int in4= tmscm_to_array_int (arg4);

  // TMSCM_DEFER_INTS;
  array_string out= evaluate_system (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_get_locale_language () {
  // TMSCM_DEFER_INTS;
  string out= get_locale_language ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_get_locale_charset () {
  // TMSCM_DEFER_INTS;
  string out= get_locale_charset ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_locale_to_language (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "locale-to-language");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= locale_to_language (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_language_to_locale (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "language-to-locale");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= language_to_locale (in1);
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
tmg_pretty_time (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "pretty-time");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  string out= pretty_time (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
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
tmg_get_show_kbd () {
  // TMSCM_DEFER_INTS;
  bool out= get_show_kbd ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_set_show_kbd (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "set-show-kbd");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  set_show_kbd (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_latex_command (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "set-latex-command");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  set_latex_command (in1);
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
tmg_number_latex_errors (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "number-latex-errors");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  int out= number_latex_errors (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_number_latex_pages (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "number-latex-pages");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  int out= number_latex_pages (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
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
tmg_command_eval (tmscm arg1) {
  TMSCM_ASSERT_COMMAND (arg1, TMSCM_ARG1, "command-eval");

  command in1= tmscm_to_command (arg1);

  // TMSCM_DEFER_INTS;
  eval (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_command_apply (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_COMMAND (arg1, TMSCM_ARG1, "command-apply");
  TMSCM_ASSERT_OBJECT (arg2, TMSCM_ARG2, "command-apply");

  command in1= tmscm_to_command (arg1);
  object in2= tmscm_to_object (arg2);

  // TMSCM_DEFER_INTS;
  apply (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
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
tmg_protected_call (tmscm arg1) {
  TMSCM_ASSERT_OBJECT (arg1, TMSCM_ARG1, "protected-call");

  object in1= tmscm_to_object (arg1);

  // TMSCM_DEFER_INTS;
  protected_call (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_notify_preferences_booted () {
  // TMSCM_DEFER_INTS;
  notify_preferences_booted ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_has_preferenceP (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cpp-has-preference?");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bool out= has_user_preference (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_cpp_get_preference (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cpp-get-preference");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "cpp-get-preference");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  string out= get_user_preference (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_cpp_set_preference (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cpp-set-preference");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "cpp-set-preference");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  set_user_preference (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_reset_preference (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cpp-reset-preference");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  reset_user_preference (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_save_preferences () {
  // TMSCM_DEFER_INTS;
  save_user_preferences ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_default_printing_command () {
  // TMSCM_DEFER_INTS;
  string out= get_printing_default ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
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
tmg_string_translate (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-translate");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= translate_as_is (in1);
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
tmg_force_load_translations (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "force-load-translations");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "force-load-translations");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  force_load_dictionary (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
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
tmg_get_hex_color (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "get-hex-color");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= get_hex_color (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_named_color_2xcolormap (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "named-color->xcolormap");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= named_color_to_xcolormap (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_rgba_2named_color (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_INT (arg1, TMSCM_ARG1, "rgba->named-color");

  array_int in1= tmscm_to_array_int (arg1);

  // TMSCM_DEFER_INTS;
  string out= named_rgb_color (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_named_color_2rgba (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "named-color->rgba");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  array_int out= get_named_rgb_color (in1);
  // TMSCM_ALLOW_INTS;

  return array_int_to_tmscm (out);
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
tmg_debug_message (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "debug-message");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "debug-message");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  debug_message (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_debug_messages (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "get-debug-messages");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "get-debug-messages");

  string in1= tmscm_to_string (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  tree out= get_debug_messages (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_clear_debug_messages () {
  // TMSCM_DEFER_INTS;
  clear_debug_messages ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
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
tmg_set_new_fonts (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "set-new-fonts");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  set_new_fonts (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_new_fontsP () {
  // TMSCM_DEFER_INTS;
  bool out= get_new_fonts ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tmtm_eqnumber_2nonumber (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tmtm-eqnumber->nonumber");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  tree out= eqnumber_to_nonumber (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_busy_versioningP () {
  // TMSCM_DEFER_INTS;
  bool out= is_busy_versioning ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_players_set_elapsed (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "players-set-elapsed");
  TMSCM_ASSERT_DOUBLE (arg2, TMSCM_ARG2, "players-set-elapsed");

  tree in1= tmscm_to_tree (arg1);
  double in2= tmscm_to_double (arg2);

  // TMSCM_DEFER_INTS;
  players_set_elapsed (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_players_set_speed (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "players-set-speed");
  TMSCM_ASSERT_DOUBLE (arg2, TMSCM_ARG2, "players-set-speed");

  tree in1= tmscm_to_tree (arg1);
  double in2= tmscm_to_double (arg2);

  // TMSCM_DEFER_INTS;
  players_set_speed (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_apply_effect (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "apply-effect");
  TMSCM_ASSERT_ARRAY_URL (arg2, TMSCM_ARG2, "apply-effect");
  TMSCM_ASSERT_URL (arg3, TMSCM_ARG3, "apply-effect");
  TMSCM_ASSERT_INT (arg4, TMSCM_ARG4, "apply-effect");
  TMSCM_ASSERT_INT (arg5, TMSCM_ARG5, "apply-effect");

  content in1= tmscm_to_content (arg1);
  array_url in2= tmscm_to_array_url (arg2);
  url in3= tmscm_to_url (arg3);
  int in4= tmscm_to_int (arg4);
  int in5= tmscm_to_int (arg5);

  // TMSCM_DEFER_INTS;
  apply_effect (in1, in2, in3, in4, in5);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tt_existsP (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tt-exists?");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bool out= tt_font_exists (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tt_dump (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tt-dump");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  tt_dump (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tt_font_name (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tt-font-name");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  scheme_tree out= tt_font_name (in1);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_tt_analyze (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tt-analyze");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= tt_analyze (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_font_database_build (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "font-database-build");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  font_database_build (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_font_database_build_local () {
  // TMSCM_DEFER_INTS;
  font_database_build_local ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_font_database_extend_local (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "font-database-extend-local");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  font_database_extend_local (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_font_database_build_global () {
  // TMSCM_DEFER_INTS;
  font_database_build_global ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_font_database_build_characteristics (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "font-database-build-characteristics");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  font_database_build_characteristics (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_font_database_insert_global (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "font-database-insert-global");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  font_database_build_global (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_font_database_save_local_delta () {
  // TMSCM_DEFER_INTS;
  font_database_save_local_delta ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_font_database_load () {
  // TMSCM_DEFER_INTS;
  font_database_load ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_font_database_save () {
  // TMSCM_DEFER_INTS;
  font_database_save ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_font_database_filter () {
  // TMSCM_DEFER_INTS;
  font_database_filter ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_font_database_families () {
  // TMSCM_DEFER_INTS;
  array_string out= font_database_families ();
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_font_database_delta_families () {
  // TMSCM_DEFER_INTS;
  array_string out= font_database_delta_families ();
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_font_database_styles (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "font-database-styles");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= font_database_styles (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_font_database_search (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "font-database-search");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "font-database-search");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  array_string out= font_database_search (in1, in2);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_font_database_characteristics (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "font-database-characteristics");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "font-database-characteristics");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  array_string out= font_database_characteristics (in1, in2);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_font_database_substitutions (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "font-database-substitutions");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  scheme_tree out= font_database_substitutions (in1);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_font_family_2master (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "font-family->master");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= family_to_master (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_font_master_2families (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "font-master->families");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= master_to_families (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_font_master_features (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "font-master-features");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= master_features (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_font_family_features (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "font-family-features");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= family_features (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_font_family_strict_features (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "font-family-strict-features");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= family_strict_features (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_font_style_features (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "font-style-features");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= style_features (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_font_guessed_features (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "font-guessed-features");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "font-guessed-features");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  array_string out= guessed_features (in1, in2);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_font_guessed_distance (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "font-guessed-distance");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "font-guessed-distance");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "font-guessed-distance");
  TMSCM_ASSERT_STRING (arg4, TMSCM_ARG4, "font-guessed-distance");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);
  string in4= tmscm_to_string (arg4);

  // TMSCM_DEFER_INTS;
  double out= guessed_distance (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return double_to_tmscm (out);
}

tmscm
tmg_font_master_guessed_distance (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "font-master-guessed-distance");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "font-master-guessed-distance");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  double out= guessed_distance (in1, in2);
  // TMSCM_ALLOW_INTS;

  return double_to_tmscm (out);
}

tmscm
tmg_font_family_guessed_features (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "font-family-guessed-features");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "font-family-guessed-features");

  string in1= tmscm_to_string (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  array_string out= guessed_features (in1, in2);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_characteristic_distance (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_ARRAY_STRING (arg1, TMSCM_ARG1, "characteristic-distance");
  TMSCM_ASSERT_ARRAY_STRING (arg2, TMSCM_ARG2, "characteristic-distance");

  array_string in1= tmscm_to_array_string (arg1);
  array_string in2= tmscm_to_array_string (arg2);

  // TMSCM_DEFER_INTS;
  double out= characteristic_distance (in1, in2);
  // TMSCM_ALLOW_INTS;

  return double_to_tmscm (out);
}

tmscm
tmg_trace_distance (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "trace-distance");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "trace-distance");
  TMSCM_ASSERT_DOUBLE (arg3, TMSCM_ARG3, "trace-distance");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  double in3= tmscm_to_double (arg3);

  // TMSCM_DEFER_INTS;
  double out= trace_distance (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return double_to_tmscm (out);
}

tmscm
tmg_logical_font_public (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "logical-font-public");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "logical-font-public");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  array_string out= logical_font (in1, in2);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_logical_font_exact (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "logical-font-exact");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "logical-font-exact");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  array_string out= logical_font_exact (in1, in2);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_logical_font_private (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "logical-font-private");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "logical-font-private");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "logical-font-private");
  TMSCM_ASSERT_STRING (arg4, TMSCM_ARG4, "logical-font-private");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);
  string in4= tmscm_to_string (arg4);

  // TMSCM_DEFER_INTS;
  array_string out= logical_font (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_logical_font_family (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_STRING (arg1, TMSCM_ARG1, "logical-font-family");

  array_string in1= tmscm_to_array_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= get_family (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_logical_font_variant (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_STRING (arg1, TMSCM_ARG1, "logical-font-variant");

  array_string in1= tmscm_to_array_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= get_variant (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_logical_font_series (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_STRING (arg1, TMSCM_ARG1, "logical-font-series");

  array_string in1= tmscm_to_array_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= get_series (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_logical_font_shape (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_STRING (arg1, TMSCM_ARG1, "logical-font-shape");

  array_string in1= tmscm_to_array_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= get_shape (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_logical_font_search (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_STRING (arg1, TMSCM_ARG1, "logical-font-search");

  array_string in1= tmscm_to_array_string (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= search_font (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_logical_font_search_exact (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_STRING (arg1, TMSCM_ARG1, "logical-font-search-exact");

  array_string in1= tmscm_to_array_string (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= search_font_exact (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_search_font_families (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_STRING (arg1, TMSCM_ARG1, "search-font-families");

  array_string in1= tmscm_to_array_string (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= search_font_families (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_search_font_styles (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "search-font-styles");
  TMSCM_ASSERT_ARRAY_STRING (arg2, TMSCM_ARG2, "search-font-styles");

  string in1= tmscm_to_string (arg1);
  array_string in2= tmscm_to_array_string (arg2);

  // TMSCM_DEFER_INTS;
  array_string out= search_font_styles (in1, in2);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_logical_font_patch (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_ARRAY_STRING (arg1, TMSCM_ARG1, "logical-font-patch");
  TMSCM_ASSERT_ARRAY_STRING (arg2, TMSCM_ARG2, "logical-font-patch");

  array_string in1= tmscm_to_array_string (arg1);
  array_string in2= tmscm_to_array_string (arg2);

  // TMSCM_DEFER_INTS;
  array_string out= patch_font (in1, in2);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_logical_font_substitute (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_STRING (arg1, TMSCM_ARG1, "logical-font-substitute");

  array_string in1= tmscm_to_array_string (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= apply_substitutions (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_font_family_main (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "font-family-main");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= main_family (in1);
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
tmg_anim_control_times (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "anim-control-times");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  array_double out= get_control_times (in1);
  // TMSCM_ALLOW_INTS;

  return array_double_to_tmscm (out);
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
tmg_tree_label_macroP (tmscm arg1) {
  TMSCM_ASSERT_TREE_LABEL (arg1, TMSCM_ARG1, "tree-label-macro?");

  tree_label in1= tmscm_to_tree_label (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_macro (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tree_label_parameterP (tmscm arg1) {
  TMSCM_ASSERT_TREE_LABEL (arg1, TMSCM_ARG1, "tree-label-parameter?");

  tree_label in1= tmscm_to_tree_label (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_parameter (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tree_label_type (tmscm arg1) {
  TMSCM_ASSERT_TREE_LABEL (arg1, TMSCM_ARG1, "tree-label-type");

  tree_label in1= tmscm_to_tree_label (arg1);

  // TMSCM_DEFER_INTS;
  string out= get_tag_type (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
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
tmg_tree_child_env_dot (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-child-env*");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-child-env*");
  TMSCM_ASSERT_CONTENT (arg3, TMSCM_ARG3, "tree-child-env*");

  content in1= tmscm_to_content (arg1);
  int in2= tmscm_to_int (arg2);
  content in3= tmscm_to_content (arg3);

  // TMSCM_DEFER_INTS;
  tree out= get_env_child (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
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
tmg_tree_descendant_env_dot (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-descendant-env*");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "tree-descendant-env*");
  TMSCM_ASSERT_CONTENT (arg3, TMSCM_ARG3, "tree-descendant-env*");

  content in1= tmscm_to_content (arg1);
  path in2= tmscm_to_path (arg2);
  content in3= tmscm_to_content (arg3);

  // TMSCM_DEFER_INTS;
  tree out= get_env_descendant (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_descendant_env (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-descendant-env");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "tree-descendant-env");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "tree-descendant-env");
  TMSCM_ASSERT_CONTENT (arg4, TMSCM_ARG4, "tree-descendant-env");

  content in1= tmscm_to_content (arg1);
  path in2= tmscm_to_path (arg2);
  string in3= tmscm_to_string (arg3);
  content in4= tmscm_to_content (arg4);

  // TMSCM_DEFER_INTS;
  tree out= get_env_descendant (in1, in2, in3, in4);
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
tmg_tree_multi_lineP (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-multi-line?");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_multi_line (in1);
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
tmg_tree_search_tree (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-search-tree");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "tree-search-tree");
  TMSCM_ASSERT_PATH (arg3, TMSCM_ARG3, "tree-search-tree");
  TMSCM_ASSERT_INT (arg4, TMSCM_ARG4, "tree-search-tree");

  content in1= tmscm_to_content (arg1);
  content in2= tmscm_to_content (arg2);
  path in3= tmscm_to_path (arg3);
  int in4= tmscm_to_int (arg4);

  // TMSCM_DEFER_INTS;
  array_path out= search (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return array_path_to_tmscm (out);
}

tmscm
tmg_tree_search_tree_at (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tree-search-tree-at");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "tree-search-tree-at");
  TMSCM_ASSERT_PATH (arg3, TMSCM_ARG3, "tree-search-tree-at");
  TMSCM_ASSERT_PATH (arg4, TMSCM_ARG4, "tree-search-tree-at");
  TMSCM_ASSERT_INT (arg5, TMSCM_ARG5, "tree-search-tree-at");

  content in1= tmscm_to_content (arg1);
  content in2= tmscm_to_content (arg2);
  path in3= tmscm_to_path (arg3);
  path in4= tmscm_to_path (arg4);
  int in5= tmscm_to_int (arg5);

  // TMSCM_DEFER_INTS;
  array_path out= search (in1, in2, in3, in4, in5);
  // TMSCM_ALLOW_INTS;

  return array_path_to_tmscm (out);
}

tmscm
tmg_tree_spell (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tree-spell");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "tree-spell");
  TMSCM_ASSERT_PATH (arg3, TMSCM_ARG3, "tree-spell");
  TMSCM_ASSERT_INT (arg4, TMSCM_ARG4, "tree-spell");

  string in1= tmscm_to_string (arg1);
  content in2= tmscm_to_content (arg2);
  path in3= tmscm_to_path (arg3);
  int in4= tmscm_to_int (arg4);

  // TMSCM_DEFER_INTS;
  array_path out= spell (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return array_path_to_tmscm (out);
}

tmscm
tmg_tree_spell_at (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tree-spell-at");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "tree-spell-at");
  TMSCM_ASSERT_PATH (arg3, TMSCM_ARG3, "tree-spell-at");
  TMSCM_ASSERT_PATH (arg4, TMSCM_ARG4, "tree-spell-at");
  TMSCM_ASSERT_INT (arg5, TMSCM_ARG5, "tree-spell-at");

  string in1= tmscm_to_string (arg1);
  content in2= tmscm_to_content (arg2);
  path in3= tmscm_to_path (arg3);
  path in4= tmscm_to_path (arg4);
  int in5= tmscm_to_int (arg5);

  // TMSCM_DEFER_INTS;
  array_path out= spell (in1, in2, in3, in4, in5);
  // TMSCM_ALLOW_INTS;

  return array_path_to_tmscm (out);
}

tmscm
tmg_tree_spell_selection (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5, tmscm arg6) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tree-spell-selection");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "tree-spell-selection");
  TMSCM_ASSERT_PATH (arg3, TMSCM_ARG3, "tree-spell-selection");
  TMSCM_ASSERT_PATH (arg4, TMSCM_ARG4, "tree-spell-selection");
  TMSCM_ASSERT_PATH (arg5, TMSCM_ARG5, "tree-spell-selection");
  TMSCM_ASSERT_INT (arg6, TMSCM_ARG6, "tree-spell-selection");

  string in1= tmscm_to_string (arg1);
  content in2= tmscm_to_content (arg2);
  path in3= tmscm_to_path (arg3);
  path in4= tmscm_to_path (arg4);
  path in5= tmscm_to_path (arg5);
  int in6= tmscm_to_int (arg6);

  // TMSCM_DEFER_INTS;
  array_path out= spell (in1, in2, in3, in4, in5, in6);
  // TMSCM_ALLOW_INTS;

  return array_path_to_tmscm (out);
}

tmscm
tmg_previous_search_hit (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_ARRAY_PATH (arg1, TMSCM_ARG1, "previous-search-hit");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "previous-search-hit");
  TMSCM_ASSERT_BOOL (arg3, TMSCM_ARG3, "previous-search-hit");

  array_path in1= tmscm_to_array_path (arg1);
  path in2= tmscm_to_path (arg2);
  bool in3= tmscm_to_bool (arg3);

  // TMSCM_DEFER_INTS;
  array_path out= previous_search_hit (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return array_path_to_tmscm (out);
}

tmscm
tmg_next_search_hit (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_ARRAY_PATH (arg1, TMSCM_ARG1, "next-search-hit");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "next-search-hit");
  TMSCM_ASSERT_BOOL (arg3, TMSCM_ARG3, "next-search-hit");

  array_path in1= tmscm_to_array_path (arg1);
  path in2= tmscm_to_path (arg2);
  bool in3= tmscm_to_bool (arg3);

  // TMSCM_DEFER_INTS;
  array_path out= next_search_hit (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return array_path_to_tmscm (out);
}

tmscm
tmg_navigate_search_hit (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "navigate-search-hit");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "navigate-search-hit");
  TMSCM_ASSERT_BOOL (arg3, TMSCM_ARG3, "navigate-search-hit");
  TMSCM_ASSERT_BOOL (arg4, TMSCM_ARG4, "navigate-search-hit");

  path in1= tmscm_to_path (arg1);
  bool in2= tmscm_to_bool (arg2);
  bool in3= tmscm_to_bool (arg3);
  bool in4= tmscm_to_bool (arg4);

  // TMSCM_DEFER_INTS;
  array_path out= navigate_search_hit (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return array_path_to_tmscm (out);
}

tmscm
tmg_tag_minimal_arity (tmscm arg1) {
  TMSCM_ASSERT_TREE_LABEL (arg1, TMSCM_ARG1, "tag-minimal-arity");

  tree_label in1= tmscm_to_tree_label (arg1);

  // TMSCM_DEFER_INTS;
  int out= minimal_arity (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_tag_maximal_arity (tmscm arg1) {
  TMSCM_ASSERT_TREE_LABEL (arg1, TMSCM_ARG1, "tag-maximal-arity");

  tree_label in1= tmscm_to_tree_label (arg1);

  // TMSCM_DEFER_INTS;
  int out= maximal_arity (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_tag_possible_arityP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE_LABEL (arg1, TMSCM_ARG1, "tag-possible-arity?");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tag-possible-arity?");

  tree_label in1= tmscm_to_tree_label (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  bool out= correct_arity (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_set_access_mode (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "set-access-mode");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  int out= set_access_mode (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_get_access_mode () {
  // TMSCM_DEFER_INTS;
  int out= get_access_mode ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
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
tmg_math_stats_compile (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "math-stats-compile");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "math-stats-compile");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "math-stats-compile");

  string in1= tmscm_to_string (arg1);
  content in2= tmscm_to_content (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  compile_stats (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_math_stats_occurrences (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "math-stats-occurrences");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "math-stats-occurrences");

  string in1= tmscm_to_string (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  int out= number_occurrences (in1, in2);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_math_stats_number_in_role (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "math-stats-number-in-role");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "math-stats-number-in-role");

  string in1= tmscm_to_string (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  int out= number_in_role (in1, in2);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_path_strip (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "path-strip");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "path-strip");

  path in1= tmscm_to_path (arg1);
  path in2= tmscm_to_path (arg2);

  // TMSCM_DEFER_INTS;
  path out= strip (in1, in2);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
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
tmg_make_modification (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "make-modification");
  TMSCM_ASSERT_PATH (arg2, TMSCM_ARG2, "make-modification");
  TMSCM_ASSERT_CONTENT (arg3, TMSCM_ARG3, "make-modification");

  string in1= tmscm_to_string (arg1);
  path in2= tmscm_to_path (arg2);
  content in3= tmscm_to_content (arg3);

  // TMSCM_DEFER_INTS;
  modification out= make_modification (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return modification_to_tmscm (out);
}

tmscm
tmg_modification_assign (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "modification-assign");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "modification-assign");

  path in1= tmscm_to_path (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  modification out= mod_assign (in1, in2);
  // TMSCM_ALLOW_INTS;

  return modification_to_tmscm (out);
}

tmscm
tmg_modification_insert (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "modification-insert");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "modification-insert");
  TMSCM_ASSERT_CONTENT (arg3, TMSCM_ARG3, "modification-insert");

  path in1= tmscm_to_path (arg1);
  int in2= tmscm_to_int (arg2);
  content in3= tmscm_to_content (arg3);

  // TMSCM_DEFER_INTS;
  modification out= mod_insert (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return modification_to_tmscm (out);
}

tmscm
tmg_modification_remove (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "modification-remove");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "modification-remove");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "modification-remove");

  path in1= tmscm_to_path (arg1);
  int in2= tmscm_to_int (arg2);
  int in3= tmscm_to_int (arg3);

  // TMSCM_DEFER_INTS;
  modification out= mod_remove (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return modification_to_tmscm (out);
}

tmscm
tmg_modification_split (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "modification-split");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "modification-split");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "modification-split");

  path in1= tmscm_to_path (arg1);
  int in2= tmscm_to_int (arg2);
  int in3= tmscm_to_int (arg3);

  // TMSCM_DEFER_INTS;
  modification out= mod_split (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return modification_to_tmscm (out);
}

tmscm
tmg_modification_join (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "modification-join");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "modification-join");

  path in1= tmscm_to_path (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  modification out= mod_join (in1, in2);
  // TMSCM_ALLOW_INTS;

  return modification_to_tmscm (out);
}

tmscm
tmg_modification_assign_node (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "modification-assign-node");
  TMSCM_ASSERT_TREE_LABEL (arg2, TMSCM_ARG2, "modification-assign-node");

  path in1= tmscm_to_path (arg1);
  tree_label in2= tmscm_to_tree_label (arg2);

  // TMSCM_DEFER_INTS;
  modification out= mod_assign_node (in1, in2);
  // TMSCM_ALLOW_INTS;

  return modification_to_tmscm (out);
}

tmscm
tmg_modification_insert_node (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "modification-insert-node");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "modification-insert-node");
  TMSCM_ASSERT_CONTENT (arg3, TMSCM_ARG3, "modification-insert-node");

  path in1= tmscm_to_path (arg1);
  int in2= tmscm_to_int (arg2);
  content in3= tmscm_to_content (arg3);

  // TMSCM_DEFER_INTS;
  modification out= mod_insert_node (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return modification_to_tmscm (out);
}

tmscm
tmg_modification_remove_node (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "modification-remove-node");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "modification-remove-node");

  path in1= tmscm_to_path (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  modification out= mod_remove_node (in1, in2);
  // TMSCM_ALLOW_INTS;

  return modification_to_tmscm (out);
}

tmscm
tmg_modification_set_cursor (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "modification-set-cursor");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "modification-set-cursor");
  TMSCM_ASSERT_CONTENT (arg3, TMSCM_ARG3, "modification-set-cursor");

  path in1= tmscm_to_path (arg1);
  int in2= tmscm_to_int (arg2);
  content in3= tmscm_to_content (arg3);

  // TMSCM_DEFER_INTS;
  modification out= mod_set_cursor (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return modification_to_tmscm (out);
}

tmscm
tmg_modification_kind (tmscm arg1) {
  TMSCM_ASSERT_MODIFICATION (arg1, TMSCM_ARG1, "modification-kind");

  modification in1= tmscm_to_modification (arg1);

  // TMSCM_DEFER_INTS;
  string out= get_type (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_modification_path (tmscm arg1) {
  TMSCM_ASSERT_MODIFICATION (arg1, TMSCM_ARG1, "modification-path");

  modification in1= tmscm_to_modification (arg1);

  // TMSCM_DEFER_INTS;
  path out= get_path (in1);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_modification_tree (tmscm arg1) {
  TMSCM_ASSERT_MODIFICATION (arg1, TMSCM_ARG1, "modification-tree");

  modification in1= tmscm_to_modification (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_tree (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_modification_root (tmscm arg1) {
  TMSCM_ASSERT_MODIFICATION (arg1, TMSCM_ARG1, "modification-root");

  modification in1= tmscm_to_modification (arg1);

  // TMSCM_DEFER_INTS;
  path out= root (in1);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_modification_index (tmscm arg1) {
  TMSCM_ASSERT_MODIFICATION (arg1, TMSCM_ARG1, "modification-index");

  modification in1= tmscm_to_modification (arg1);

  // TMSCM_DEFER_INTS;
  int out= index (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_modification_argument (tmscm arg1) {
  TMSCM_ASSERT_MODIFICATION (arg1, TMSCM_ARG1, "modification-argument");

  modification in1= tmscm_to_modification (arg1);

  // TMSCM_DEFER_INTS;
  int out= argument (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_modification_label (tmscm arg1) {
  TMSCM_ASSERT_MODIFICATION (arg1, TMSCM_ARG1, "modification-label");

  modification in1= tmscm_to_modification (arg1);

  // TMSCM_DEFER_INTS;
  tree_label out= L (in1);
  // TMSCM_ALLOW_INTS;

  return tree_label_to_tmscm (out);
}

tmscm
tmg_modification_copy (tmscm arg1) {
  TMSCM_ASSERT_MODIFICATION (arg1, TMSCM_ARG1, "modification-copy");

  modification in1= tmscm_to_modification (arg1);

  // TMSCM_DEFER_INTS;
  modification out= copy (in1);
  // TMSCM_ALLOW_INTS;

  return modification_to_tmscm (out);
}

tmscm
tmg_modification_applicableP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "modification-applicable?");
  TMSCM_ASSERT_MODIFICATION (arg2, TMSCM_ARG2, "modification-applicable?");

  content in1= tmscm_to_content (arg1);
  modification in2= tmscm_to_modification (arg2);

  // TMSCM_DEFER_INTS;
  bool out= is_applicable (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_modification_apply (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "modification-apply");
  TMSCM_ASSERT_MODIFICATION (arg2, TMSCM_ARG2, "modification-apply");

  content in1= tmscm_to_content (arg1);
  modification in2= tmscm_to_modification (arg2);

  // TMSCM_DEFER_INTS;
  tree out= var_clean_apply (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_modification_inplace_apply (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "modification-inplace-apply");
  TMSCM_ASSERT_MODIFICATION (arg2, TMSCM_ARG2, "modification-inplace-apply");

  tree in1= tmscm_to_tree (arg1);
  modification in2= tmscm_to_modification (arg2);

  // TMSCM_DEFER_INTS;
  tree out= var_apply (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_modification_invert (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_MODIFICATION (arg1, TMSCM_ARG1, "modification-invert");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "modification-invert");

  modification in1= tmscm_to_modification (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  modification out= invert (in1, in2);
  // TMSCM_ALLOW_INTS;

  return modification_to_tmscm (out);
}

tmscm
tmg_modification_commuteP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_MODIFICATION (arg1, TMSCM_ARG1, "modification-commute?");
  TMSCM_ASSERT_MODIFICATION (arg2, TMSCM_ARG2, "modification-commute?");

  modification in1= tmscm_to_modification (arg1);
  modification in2= tmscm_to_modification (arg2);

  // TMSCM_DEFER_INTS;
  bool out= commute (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_modification_can_pullP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_MODIFICATION (arg1, TMSCM_ARG1, "modification-can-pull?");
  TMSCM_ASSERT_MODIFICATION (arg2, TMSCM_ARG2, "modification-can-pull?");

  modification in1= tmscm_to_modification (arg1);
  modification in2= tmscm_to_modification (arg2);

  // TMSCM_DEFER_INTS;
  bool out= can_pull (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_modification_pull (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_MODIFICATION (arg1, TMSCM_ARG1, "modification-pull");
  TMSCM_ASSERT_MODIFICATION (arg2, TMSCM_ARG2, "modification-pull");

  modification in1= tmscm_to_modification (arg1);
  modification in2= tmscm_to_modification (arg2);

  // TMSCM_DEFER_INTS;
  modification out= pull (in1, in2);
  // TMSCM_ALLOW_INTS;

  return modification_to_tmscm (out);
}

tmscm
tmg_modification_co_pull (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_MODIFICATION (arg1, TMSCM_ARG1, "modification-co-pull");
  TMSCM_ASSERT_MODIFICATION (arg2, TMSCM_ARG2, "modification-co-pull");

  modification in1= tmscm_to_modification (arg1);
  modification in2= tmscm_to_modification (arg2);

  // TMSCM_DEFER_INTS;
  modification out= co_pull (in1, in2);
  // TMSCM_ALLOW_INTS;

  return modification_to_tmscm (out);
}

tmscm
tmg_patch_pair (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_MODIFICATION (arg1, TMSCM_ARG1, "patch-pair");
  TMSCM_ASSERT_MODIFICATION (arg2, TMSCM_ARG2, "patch-pair");

  modification in1= tmscm_to_modification (arg1);
  modification in2= tmscm_to_modification (arg2);

  // TMSCM_DEFER_INTS;
  patch out= patch (in1, in2);
  // TMSCM_ALLOW_INTS;

  return patch_to_tmscm (out);
}

tmscm
tmg_patch_compound (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_PATCH (arg1, TMSCM_ARG1, "patch-compound");

  array_patch in1= tmscm_to_array_patch (arg1);

  // TMSCM_DEFER_INTS;
  patch out= patch (in1);
  // TMSCM_ALLOW_INTS;

  return patch_to_tmscm (out);
}

tmscm
tmg_patch_branch (tmscm arg1) {
  TMSCM_ASSERT_ARRAY_PATCH (arg1, TMSCM_ARG1, "patch-branch");

  array_patch in1= tmscm_to_array_patch (arg1);

  // TMSCM_DEFER_INTS;
  patch out= branch_patch (in1);
  // TMSCM_ALLOW_INTS;

  return patch_to_tmscm (out);
}

tmscm
tmg_patch_birth (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_DOUBLE (arg1, TMSCM_ARG1, "patch-birth");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "patch-birth");

  double in1= tmscm_to_double (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  patch out= patch (in1, in2);
  // TMSCM_ALLOW_INTS;

  return patch_to_tmscm (out);
}

tmscm
tmg_patch_author (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_DOUBLE (arg1, TMSCM_ARG1, "patch-author");
  TMSCM_ASSERT_PATCH (arg2, TMSCM_ARG2, "patch-author");

  double in1= tmscm_to_double (arg1);
  patch in2= tmscm_to_patch (arg2);

  // TMSCM_DEFER_INTS;
  patch out= patch (in1, in2);
  // TMSCM_ALLOW_INTS;

  return patch_to_tmscm (out);
}

tmscm
tmg_patch_pairP (tmscm arg1) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-pair?");

  patch in1= tmscm_to_patch (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_modification (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_patch_compoundP (tmscm arg1) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-compound?");

  patch in1= tmscm_to_patch (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_compound (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_patch_branchP (tmscm arg1) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-branch?");

  patch in1= tmscm_to_patch (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_branch (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_patch_birthP (tmscm arg1) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-birth?");

  patch in1= tmscm_to_patch (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_birth (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_patch_authorP (tmscm arg1) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-author?");

  patch in1= tmscm_to_patch (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_author (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_patch_arity (tmscm arg1) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-arity");

  patch in1= tmscm_to_patch (arg1);

  // TMSCM_DEFER_INTS;
  int out= N (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_patch_ref (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-ref");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "patch-ref");

  patch in1= tmscm_to_patch (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  patch out= access (in1, in2);
  // TMSCM_ALLOW_INTS;

  return patch_to_tmscm (out);
}

tmscm
tmg_patch_direct (tmscm arg1) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-direct");

  patch in1= tmscm_to_patch (arg1);

  // TMSCM_DEFER_INTS;
  modification out= get_modification (in1);
  // TMSCM_ALLOW_INTS;

  return modification_to_tmscm (out);
}

tmscm
tmg_patch_inverse (tmscm arg1) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-inverse");

  patch in1= tmscm_to_patch (arg1);

  // TMSCM_DEFER_INTS;
  modification out= get_inverse (in1);
  // TMSCM_ALLOW_INTS;

  return modification_to_tmscm (out);
}

tmscm
tmg_patch_get_birth (tmscm arg1) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-get-birth");

  patch in1= tmscm_to_patch (arg1);

  // TMSCM_DEFER_INTS;
  bool out= get_birth (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_patch_get_author (tmscm arg1) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-get-author");

  patch in1= tmscm_to_patch (arg1);

  // TMSCM_DEFER_INTS;
  double out= get_author (in1);
  // TMSCM_ALLOW_INTS;

  return double_to_tmscm (out);
}

tmscm
tmg_patch_copy (tmscm arg1) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-copy");

  patch in1= tmscm_to_patch (arg1);

  // TMSCM_DEFER_INTS;
  patch out= copy (in1);
  // TMSCM_ALLOW_INTS;

  return patch_to_tmscm (out);
}

tmscm
tmg_patch_applicableP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-applicable?");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "patch-applicable?");

  patch in1= tmscm_to_patch (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  bool out= is_applicable (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_patch_apply (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "patch-apply");
  TMSCM_ASSERT_PATCH (arg2, TMSCM_ARG2, "patch-apply");

  content in1= tmscm_to_content (arg1);
  patch in2= tmscm_to_patch (arg2);

  // TMSCM_DEFER_INTS;
  tree out= var_clean_apply (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_patch_inplace_apply (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "patch-inplace-apply");
  TMSCM_ASSERT_PATCH (arg2, TMSCM_ARG2, "patch-inplace-apply");

  tree in1= tmscm_to_tree (arg1);
  patch in2= tmscm_to_patch (arg2);

  // TMSCM_DEFER_INTS;
  tree out= var_apply (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_patch_compactify (tmscm arg1) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-compactify");

  patch in1= tmscm_to_patch (arg1);

  // TMSCM_DEFER_INTS;
  patch out= compactify (in1);
  // TMSCM_ALLOW_INTS;

  return patch_to_tmscm (out);
}

tmscm
tmg_patch_cursor_hint (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-cursor-hint");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "patch-cursor-hint");

  patch in1= tmscm_to_patch (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  path out= cursor_hint (in1, in2);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_patch_invert (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-invert");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "patch-invert");

  patch in1= tmscm_to_patch (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  patch out= invert (in1, in2);
  // TMSCM_ALLOW_INTS;

  return patch_to_tmscm (out);
}

tmscm
tmg_patch_commuteP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-commute?");
  TMSCM_ASSERT_PATCH (arg2, TMSCM_ARG2, "patch-commute?");

  patch in1= tmscm_to_patch (arg1);
  patch in2= tmscm_to_patch (arg2);

  // TMSCM_DEFER_INTS;
  bool out= commute (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_patch_can_pullP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-can-pull?");
  TMSCM_ASSERT_PATCH (arg2, TMSCM_ARG2, "patch-can-pull?");

  patch in1= tmscm_to_patch (arg1);
  patch in2= tmscm_to_patch (arg2);

  // TMSCM_DEFER_INTS;
  bool out= can_pull (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_patch_pull (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-pull");
  TMSCM_ASSERT_PATCH (arg2, TMSCM_ARG2, "patch-pull");

  patch in1= tmscm_to_patch (arg1);
  patch in2= tmscm_to_patch (arg2);

  // TMSCM_DEFER_INTS;
  patch out= pull (in1, in2);
  // TMSCM_ALLOW_INTS;

  return patch_to_tmscm (out);
}

tmscm
tmg_patch_co_pull (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-co-pull");
  TMSCM_ASSERT_PATCH (arg2, TMSCM_ARG2, "patch-co-pull");

  patch in1= tmscm_to_patch (arg1);
  patch in2= tmscm_to_patch (arg2);

  // TMSCM_DEFER_INTS;
  patch out= co_pull (in1, in2);
  // TMSCM_ALLOW_INTS;

  return patch_to_tmscm (out);
}

tmscm
tmg_patch_remove_set_cursor (tmscm arg1) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-remove-set-cursor");

  patch in1= tmscm_to_patch (arg1);

  // TMSCM_DEFER_INTS;
  patch out= remove_set_cursor (in1);
  // TMSCM_ALLOW_INTS;

  return patch_to_tmscm (out);
}

tmscm
tmg_patch_modifiesP (tmscm arg1) {
  TMSCM_ASSERT_PATCH (arg1, TMSCM_ARG1, "patch-modifies?");

  patch in1= tmscm_to_patch (arg1);

  // TMSCM_DEFER_INTS;
  bool out= does_modify (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
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
tmg_cpp_string_numberP (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cpp-string-number?");

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
tmg_string_count_occurrences (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-count-occurrences");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "string-count-occurrences");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  int out= count_occurrences (in1, in2);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
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
tmg_string_overlapping (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-overlapping");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "string-overlapping");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  int out= overlapping (in1, in2);
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
tmg_string_find_non_alpha (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-find-non-alpha");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "string-find-non-alpha");
  TMSCM_ASSERT_BOOL (arg3, TMSCM_ARG3, "string-find-non-alpha");

  string in1= tmscm_to_string (arg1);
  int in2= tmscm_to_int (arg2);
  bool in3= tmscm_to_bool (arg3);

  // TMSCM_DEFER_INTS;
  int out= find_non_alpha (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
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
tmg_escape_to_ascii (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "escape-to-ascii");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= cork_to_ascii (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_unescape_guile (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "unescape-guile");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= unescape_guile (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_string_quote (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-quote");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= scm_quote (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_string_unquote (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-unquote");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= scm_unquote (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_string_trim_spaces_left (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-trim-spaces-left");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= trim_spaces_left (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_string_trim_spaces_right (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-trim-spaces-right");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= trim_spaces_right (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_string_trim_spaces (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-trim-spaces");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= trim_spaces (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_downgrade_math_letters (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "downgrade-math-letters");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= downgrade_math_letters (in1);
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
tmg_encode_base64 (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "encode-base64");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= encode_base64 (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_decode_base64 (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "decode-base64");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= decode_base64 (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_sourcecode_2cork (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "sourcecode->cork");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= sourcecode_to_cork (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_cork_2sourcecode (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cork->sourcecode");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= cork_to_sourcecode (in1);
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
tmg_utf8_2t2a (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "utf8->t2a");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= utf8_to_t2a (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_t2a_2utf8 (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "t2a->utf8");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= t2a_to_utf8 (in1);
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
tmg_guess_wencoding (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "guess-wencoding");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= guess_wencoding (in1);
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
tmg_cpp_string_tokenize (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cpp-string-tokenize");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "cpp-string-tokenize");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  array_string out= tokenize (in1, in2);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_cpp_string_recompose (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_ARRAY_STRING (arg1, TMSCM_ARG1, "cpp-string-recompose");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "cpp-string-recompose");

  array_string in1= tmscm_to_array_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  string out= recompose (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_string_differences (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-differences");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "string-differences");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  array_int out= differences (in1, in2);
  // TMSCM_ALLOW_INTS;

  return array_int_to_tmscm (out);
}

tmscm
tmg_string_distance (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-distance");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "string-distance");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  int out= distance (in1, in2);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_find_left_bracket (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "find-left-bracket");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "find-left-bracket");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "find-left-bracket");

  path in1= tmscm_to_path (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  path out= find_left_bracket (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_find_right_bracket (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "find-right-bracket");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "find-right-bracket");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "find-right-bracket");

  path in1= tmscm_to_path (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  path out= find_right_bracket (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
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
tmg_tmstring_split (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmstring-split");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= tm_string_split (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_tmstring_translit (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmstring-translit");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= uni_translit (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmstring_locase_first (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmstring-locase-first");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= uni_locase_first (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmstring_upcase_first (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmstring-upcase-first");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= uni_upcase_first (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmstring_locase_all (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmstring-locase-all");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= uni_locase_all (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmstring_upcase_all (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmstring-upcase-all");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= uni_upcase_all (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmstring_unaccent_all (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmstring-unaccent-all");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= uni_unaccent_all (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tmstring_letterP (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmstring-letter?");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bool out= uni_is_letter (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tmstring_beforeP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tmstring-before?");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tmstring-before?");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  bool out= uni_before (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_multi_spell_start () {
  // TMSCM_DEFER_INTS;
  spell_start ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_multi_spell_done () {
  // TMSCM_DEFER_INTS;
  spell_done ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_single_spell_start (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "single-spell-start");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= spell_start (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_single_spell_done (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "single-spell-done");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  spell_done (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_spell_check (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "spell-check");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "spell-check");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  tree out= spell_check (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_spell_checkP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "spell-check?");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "spell-check?");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  bool out= check_word (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_spell_accept (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "spell-accept");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "spell-accept");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  spell_accept (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_spell_var_accept (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "spell-var-accept");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "spell-var-accept");
  TMSCM_ASSERT_BOOL (arg3, TMSCM_ARG3, "spell-var-accept");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  bool in3= tmscm_to_bool (arg3);

  // TMSCM_DEFER_INTS;
  spell_accept (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_spell_insert (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "spell-insert");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "spell-insert");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  spell_insert (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
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
tmg_syntax_read_preferences (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "syntax-read-preferences");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  initialize_color_decodings (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
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
tmg_cpp_latex_document_2texmacs (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "cpp-latex-document->texmacs");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "cpp-latex-document->texmacs");

  string in1= tmscm_to_string (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  tree out= latex_document_to_tree (in1, in2);
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
tmg_tracked_latex_2texmacs (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tracked-latex->texmacs");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "tracked-latex->texmacs");

  string in1= tmscm_to_string (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  tree out= tracked_latex_to_texmacs (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_conservative_texmacs_2latex (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "conservative-texmacs->latex");
  TMSCM_ASSERT_OBJECT (arg2, TMSCM_ARG2, "conservative-texmacs->latex");

  content in1= tmscm_to_content (arg1);
  object in2= tmscm_to_object (arg2);

  // TMSCM_DEFER_INTS;
  string out= conservative_texmacs_to_latex (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tracked_texmacs_2latex (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "tracked-texmacs->latex");
  TMSCM_ASSERT_OBJECT (arg2, TMSCM_ARG2, "tracked-texmacs->latex");

  content in1= tmscm_to_content (arg1);
  object in2= tmscm_to_object (arg2);

  // TMSCM_DEFER_INTS;
  string out= tracked_texmacs_to_latex (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_conservative_latex_2texmacs (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "conservative-latex->texmacs");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "conservative-latex->texmacs");

  string in1= tmscm_to_string (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  tree out= conservative_latex_to_texmacs (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_get_line_number (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "get-line-number");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "get-line-number");

  string in1= tmscm_to_string (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  int out= get_line_number (in1, in2);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_get_column_number (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "get-column-number");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "get-column-number");

  string in1= tmscm_to_string (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  int out= get_column_number (in1, in2);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_try_latex_export (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "try-latex-export");
  TMSCM_ASSERT_OBJECT (arg2, TMSCM_ARG2, "try-latex-export");
  TMSCM_ASSERT_URL (arg3, TMSCM_ARG3, "try-latex-export");
  TMSCM_ASSERT_URL (arg4, TMSCM_ARG4, "try-latex-export");

  content in1= tmscm_to_content (arg1);
  object in2= tmscm_to_object (arg2);
  url in3= tmscm_to_url (arg3);
  url in4= tmscm_to_url (arg4);

  // TMSCM_DEFER_INTS;
  tree out= try_latex_export (in1, in2, in3, in4);
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
tmg_conservative_bib_import (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "conservative-bib-import");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "conservative-bib-import");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "conservative-bib-import");

  string in1= tmscm_to_string (arg1);
  content in2= tmscm_to_content (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  tree out= conservative_bib_import (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_conservative_bib_export (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "conservative-bib-export");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "conservative-bib-export");
  TMSCM_ASSERT_CONTENT (arg3, TMSCM_ARG3, "conservative-bib-export");

  content in1= tmscm_to_content (arg1);
  string in2= tmscm_to_string (arg2);
  content in3= tmscm_to_content (arg3);

  // TMSCM_DEFER_INTS;
  string out= conservative_bib_export (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_clean_html (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "clean-html");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  tree out= clean_html (in1);
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
tmg_retrieve_mathjax (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "retrieve-mathjax");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  tree out= retrieve_mathjax (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_vernac_2texmacs (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "vernac->texmacs");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= vernac_to_tree (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_vernac_document_2texmacs (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "vernac-document->texmacs");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= vernac_document_to_tree (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_compute_keys_string (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "compute-keys-string");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "compute-keys-string");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  array_string out= compute_keys (in1, in2);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_compute_keys_tree (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "compute-keys-tree");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "compute-keys-tree");

  content in1= tmscm_to_content (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  array_string out= compute_keys (in1, in2);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_compute_keys_url (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "compute-keys-url");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= compute_keys (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_compute_index_string (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "compute-index-string");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "compute-index-string");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  scheme_tree out= compute_index (in1, in2);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_compute_index_tree (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "compute-index-tree");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "compute-index-tree");

  content in1= tmscm_to_content (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  scheme_tree out= compute_index (in1, in2);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_compute_index_url (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "compute-index-url");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  scheme_tree out= compute_index (in1);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_url_2url (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url->url");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  url out= url (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_root_2url (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "root->url");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  url out= url_root (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
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
tmg_url_2string (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url->string");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  string out= as_string (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_url_2stree (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url->stree");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  scheme_tree out= as_tree (in1);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_system_2url (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "system->url");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  url out= url_system (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_2system (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url->system");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  string out= as_system_string (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_unix_2url (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "unix->url");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  url out= url_unix (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_2unix (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url->unix");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  string out= as_unix_string (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_url_unix (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "url-unix");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "url-unix");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  url out= url (in1, in2);
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
tmg_url_pwd () {
  // TMSCM_DEFER_INTS;
  url out= url_pwd ();
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
tmg_url_noneP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-none?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_none (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_url_rootedP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-rooted?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_rooted (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_url_rooted_protocolP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-rooted-protocol?");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "url-rooted-protocol?");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  bool out= is_rooted (in1, in2);
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
tmg_url_rooted_tmfsP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-rooted-tmfs?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_rooted_tmfs (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_url_rooted_tmfs_protocolP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-rooted-tmfs-protocol?");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "url-rooted-tmfs-protocol?");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  bool out= is_rooted_tmfs (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_url_root (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-root");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  string out= get_root (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_url_unroot (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-unroot");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  url out= unroot (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_atomicP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-atomic?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_atomic (in1);
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
tmg_url_format (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-format");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  string out= file_format (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
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
tmg_url_basename (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-basename");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  string out= basename (in1);
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
tmg_url_resolve_pattern (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-resolve-pattern");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  url out= resolve_pattern (in1);
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
tmg_url_concretize_dot (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-concretize*");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  url out= concretize_url (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
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
tmg_url_sys_concretize (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-sys-concretize");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  string out= sys_concretize (in1);
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
tmg_url_size (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-size");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  int out= file_size (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
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
tmg_url_temp_dir () {
  // TMSCM_DEFER_INTS;
  url out= url_temp_dir ();
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
tmg_url_cache_invalidate (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-cache-invalidate");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  web_cache_invalidate (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
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
tmg_string_append_to_file (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "string-append-to-file");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "string-append-to-file");

  string in1= tmscm_to_string (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  string_append_to_file (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
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
tmg_system_rmdir (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "system-rmdir");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  rmdir (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_system_setenv (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "system-setenv");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "system-setenv");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  set_env (in1, in2);
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
tmg_system_url_2string (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "system-url->string");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  string out= sys_concretize (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_url_grep (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "url-grep");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "url-grep");

  string in1= tmscm_to_string (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  url out= grep (in1, in2);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_url_search_upwards (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "url-search-upwards");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "url-search-upwards");
  TMSCM_ASSERT_ARRAY_STRING (arg3, TMSCM_ARG3, "url-search-upwards");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);
  array_string in3= tmscm_to_array_string (arg3);

  // TMSCM_DEFER_INTS;
  url out= search_file_upwards (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_picture_cache_reset () {
  // TMSCM_DEFER_INTS;
  picture_cache_reset ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_file_focus (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "set-file-focus");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  set_file_focus (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_file_focus () {
  // TMSCM_DEFER_INTS;
  url out= get_file_focus ();
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_persistent_set (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "persistent-set");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "persistent-set");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "persistent-set");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  persistent_set (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_persistent_remove (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "persistent-remove");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "persistent-remove");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  persistent_reset (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_persistent_hasP (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "persistent-has?");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "persistent-has?");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  bool out= persistent_contains (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_persistent_get (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "persistent-get");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "persistent-get");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  string out= persistent_get (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_persistent_file_name (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "persistent-file-name");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "persistent-file-name");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  url out= persistent_file_name (in1, in2);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_tmdb_keep_history (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tmdb-keep-history");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "tmdb-keep-history");

  url in1= tmscm_to_url (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  keep_history (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmdb_set_field (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tmdb-set-field");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tmdb-set-field");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "tmdb-set-field");
  TMSCM_ASSERT_ARRAY_STRING (arg4, TMSCM_ARG4, "tmdb-set-field");
  TMSCM_ASSERT_DOUBLE (arg5, TMSCM_ARG5, "tmdb-set-field");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);
  array_string in4= tmscm_to_array_string (arg4);
  double in5= tmscm_to_double (arg5);

  // TMSCM_DEFER_INTS;
  set_field (in1, in2, in3, in4, in5);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmdb_get_field (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tmdb-get-field");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tmdb-get-field");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "tmdb-get-field");
  TMSCM_ASSERT_DOUBLE (arg4, TMSCM_ARG4, "tmdb-get-field");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);
  double in4= tmscm_to_double (arg4);

  // TMSCM_DEFER_INTS;
  array_string out= get_field (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_tmdb_remove_field (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tmdb-remove-field");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tmdb-remove-field");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "tmdb-remove-field");
  TMSCM_ASSERT_DOUBLE (arg4, TMSCM_ARG4, "tmdb-remove-field");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);
  double in4= tmscm_to_double (arg4);

  // TMSCM_DEFER_INTS;
  remove_field (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmdb_get_attributes (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tmdb-get-attributes");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tmdb-get-attributes");
  TMSCM_ASSERT_DOUBLE (arg3, TMSCM_ARG3, "tmdb-get-attributes");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);
  double in3= tmscm_to_double (arg3);

  // TMSCM_DEFER_INTS;
  array_string out= get_attributes (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_tmdb_set_entry (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tmdb-set-entry");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tmdb-set-entry");
  TMSCM_ASSERT_SCHEME_TREE (arg3, TMSCM_ARG3, "tmdb-set-entry");
  TMSCM_ASSERT_DOUBLE (arg4, TMSCM_ARG4, "tmdb-set-entry");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);
  scheme_tree in3= tmscm_to_scheme_tree (arg3);
  double in4= tmscm_to_double (arg4);

  // TMSCM_DEFER_INTS;
  set_entry (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmdb_get_entry (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tmdb-get-entry");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tmdb-get-entry");
  TMSCM_ASSERT_DOUBLE (arg3, TMSCM_ARG3, "tmdb-get-entry");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);
  double in3= tmscm_to_double (arg3);

  // TMSCM_DEFER_INTS;
  scheme_tree out= get_entry (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_tmdb_remove_entry (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tmdb-remove-entry");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tmdb-remove-entry");
  TMSCM_ASSERT_DOUBLE (arg3, TMSCM_ARG3, "tmdb-remove-entry");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);
  double in3= tmscm_to_double (arg3);

  // TMSCM_DEFER_INTS;
  remove_entry (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmdb_query (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tmdb-query");
  TMSCM_ASSERT_SCHEME_TREE (arg2, TMSCM_ARG2, "tmdb-query");
  TMSCM_ASSERT_DOUBLE (arg3, TMSCM_ARG3, "tmdb-query");
  TMSCM_ASSERT_INT (arg4, TMSCM_ARG4, "tmdb-query");

  url in1= tmscm_to_url (arg1);
  scheme_tree in2= tmscm_to_scheme_tree (arg2);
  double in3= tmscm_to_double (arg3);
  int in4= tmscm_to_int (arg4);

  // TMSCM_DEFER_INTS;
  array_string out= query (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_tmdb_inspect_history (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tmdb-inspect-history");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tmdb-inspect-history");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  inspect_history (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tmdb_get_completions (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tmdb-get-completions");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tmdb-get-completions");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  array_string out= get_completions (in1, in2);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_tmdb_get_name_completions (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tmdb-get-name-completions");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tmdb-get-name-completions");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  array_string out= get_name_completions (in1, in2);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_supports_sqlP () {
  // TMSCM_DEFER_INTS;
  bool out= sqlite3_present ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_sql_exec (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "sql-exec");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "sql-exec");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  scheme_tree out= sql_exec (in1, in2);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_sql_quote (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "sql-quote");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= sql_quote (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
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
tmg_server_startedP () {
  // TMSCM_DEFER_INTS;
  bool out= server_started ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_client_start (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "client-start");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  int out= client_start (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_client_stop (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "client-stop");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  client_stop (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_client_read (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "client-read");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  string out= client_read (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_client_write (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "client-write");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "client-write");

  int in1= tmscm_to_int (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  client_write (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_enter_secure_mode (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "enter-secure-mode");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  enter_secure_mode (in1);
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
  widget out= extend_widget (in1, in2);
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
tmg_widget_filtered_choice (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_COMMAND (arg1, TMSCM_ARG1, "widget-filtered-choice");
  TMSCM_ASSERT_ARRAY_STRING (arg2, TMSCM_ARG2, "widget-filtered-choice");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "widget-filtered-choice");
  TMSCM_ASSERT_STRING (arg4, TMSCM_ARG4, "widget-filtered-choice");

  command in1= tmscm_to_command (arg1);
  array_string in2= tmscm_to_array_string (arg2);
  string in3= tmscm_to_string (arg3);
  string in4= tmscm_to_string (arg4);

  // TMSCM_DEFER_INTS;
  widget out= choice_widget (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_tree_view (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_COMMAND (arg1, TMSCM_ARG1, "widget-tree-view");
  TMSCM_ASSERT_TREE (arg2, TMSCM_ARG2, "widget-tree-view");
  TMSCM_ASSERT_TREE (arg3, TMSCM_ARG3, "widget-tree-view");

  command in1= tmscm_to_command (arg1);
  tree in2= tmscm_to_tree (arg2);
  tree in3= tmscm_to_tree (arg3);

  // TMSCM_DEFER_INTS;
  widget out= tree_view_widget (in1, in2, in3);
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
tmg_widget_division (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "widget-division");
  TMSCM_ASSERT_WIDGET (arg2, TMSCM_ARG2, "widget-division");

  string in1= tmscm_to_string (arg1);
  widget in2= tmscm_to_widget (arg2);

  // TMSCM_DEFER_INTS;
  widget out= division_widget (in1, in2);
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
tmg_widget_icon_tabs (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_ARRAY_URL (arg1, TMSCM_ARG1, "widget-icon-tabs");
  TMSCM_ASSERT_ARRAY_WIDGET (arg2, TMSCM_ARG2, "widget-icon-tabs");
  TMSCM_ASSERT_ARRAY_WIDGET (arg3, TMSCM_ARG3, "widget-icon-tabs");

  array_url in1= tmscm_to_array_url (arg1);
  array_widget in2= tmscm_to_array_widget (arg2);
  array_widget in3= tmscm_to_array_widget (arg3);

  // TMSCM_DEFER_INTS;
  widget out= icon_tabs_widget (in1, in2, in3);
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
tmg_widget_resize (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5, tmscm arg6, tmscm arg7, tmscm arg8, tmscm arg9, tmscm arg10) {
  TMSCM_ASSERT_WIDGET (arg1, TMSCM_ARG1, "widget-resize");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "widget-resize");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "widget-resize");
  TMSCM_ASSERT_STRING (arg4, TMSCM_ARG4, "widget-resize");
  TMSCM_ASSERT_STRING (arg5, TMSCM_ARG5, "widget-resize");
  TMSCM_ASSERT_STRING (arg6, TMSCM_ARG6, "widget-resize");
  TMSCM_ASSERT_STRING (arg7, TMSCM_ARG7, "widget-resize");
  TMSCM_ASSERT_STRING (arg8, TMSCM_ARG8, "widget-resize");
  TMSCM_ASSERT_STRING (arg9, TMSCM_ARG9, "widget-resize");
  TMSCM_ASSERT_STRING (arg10, TMSCM_ARG10, "widget-resize");

  widget in1= tmscm_to_widget (arg1);
  int in2= tmscm_to_int (arg2);
  string in3= tmscm_to_string (arg3);
  string in4= tmscm_to_string (arg4);
  string in5= tmscm_to_string (arg5);
  string in6= tmscm_to_string (arg6);
  string in7= tmscm_to_string (arg7);
  string in8= tmscm_to_string (arg8);
  string in9= tmscm_to_string (arg9);
  string in10= tmscm_to_string (arg10);

  // TMSCM_DEFER_INTS;
  widget out= resize_widget (in1, in2, in3, in4, in5, in6, in7, in8, in9, in10);
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
tmg_widget_texmacs_output (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "widget-texmacs-output");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "widget-texmacs-output");

  content in1= tmscm_to_content (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  widget out= texmacs_output_widget (in1, in2);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_texmacs_input (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "widget-texmacs-input");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "widget-texmacs-input");
  TMSCM_ASSERT_URL (arg3, TMSCM_ARG3, "widget-texmacs-input");

  content in1= tmscm_to_content (arg1);
  content in2= tmscm_to_content (arg2);
  url in3= tmscm_to_url (arg3);

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
tmg_widget_refresh (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "widget-refresh");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "widget-refresh");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  widget out= refresh_widget (in1, in2);
  // TMSCM_ALLOW_INTS;

  return widget_to_tmscm (out);
}

tmscm
tmg_widget_refreshable (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_OBJECT (arg1, TMSCM_ARG1, "widget-refreshable");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "widget-refreshable");

  object in1= tmscm_to_object (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  widget out= refreshable_widget (in1, in2);
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
tmg_widget_size (tmscm arg1) {
  TMSCM_ASSERT_WIDGET (arg1, TMSCM_ARG1, "widget-size");

  widget in1= tmscm_to_widget (arg1);

  // TMSCM_DEFER_INTS;
  array_int out= get_widget_size (in1);
  // TMSCM_ALLOW_INTS;

  return array_int_to_tmscm (out);
}

tmscm
tmg_texmacs_widget_size (tmscm arg1) {
  TMSCM_ASSERT_WIDGET (arg1, TMSCM_ARG1, "texmacs-widget-size");

  widget in1= tmscm_to_widget (arg1);

  // TMSCM_DEFER_INTS;
  array_int out= get_texmacs_widget_size (in1);
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
tmg_get_style_menu () {
  // TMSCM_DEFER_INTS;
  object out= get_style_menu ();
  // TMSCM_ALLOW_INTS;

  return object_to_tmscm (out);
}

tmscm
tmg_hidden_packageP (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "hidden-package?");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bool out= hidden_package (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_get_add_package_menu () {
  // TMSCM_DEFER_INTS;
  object out= get_add_package_menu ();
  // TMSCM_ALLOW_INTS;

  return object_to_tmscm (out);
}

tmscm
tmg_get_remove_package_menu () {
  // TMSCM_DEFER_INTS;
  object out= get_remove_package_menu ();
  // TMSCM_ALLOW_INTS;

  return object_to_tmscm (out);
}

tmscm
tmg_get_toggle_package_menu () {
  // TMSCM_DEFER_INTS;
  object out= get_toggle_package_menu ();
  // TMSCM_ALLOW_INTS;

  return object_to_tmscm (out);
}

tmscm
tmg_refresh_now (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "refresh-now");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  windows_refresh (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_screen_size () {
  // TMSCM_DEFER_INTS;
  array_int out= get_screen_size ();
  // TMSCM_ALLOW_INTS;

  return array_int_to_tmscm (out);
}

tmscm
tmg_buffer_list () {
  // TMSCM_DEFER_INTS;
  array_url out= get_all_buffers ();
  // TMSCM_ALLOW_INTS;

  return array_url_to_tmscm (out);
}

tmscm
tmg_current_buffer_url () {
  // TMSCM_DEFER_INTS;
  url out= get_current_buffer_safe ();
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_path_to_buffer (tmscm arg1) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "path-to-buffer");

  path in1= tmscm_to_path (arg1);

  // TMSCM_DEFER_INTS;
  url out= path_to_buffer (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_buffer_new () {
  // TMSCM_DEFER_INTS;
  url out= make_new_buffer ();
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_buffer_rename (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-rename");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "buffer-rename");

  url in1= tmscm_to_url (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  rename_buffer (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_buffer_set (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-set");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "buffer-set");

  url in1= tmscm_to_url (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  set_buffer_tree (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_buffer_get (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-get");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_buffer_tree (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_buffer_set_body (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-set-body");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "buffer-set-body");

  url in1= tmscm_to_url (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  set_buffer_body (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_buffer_get_body (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-get-body");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_buffer_body (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_buffer_set_master (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-set-master");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "buffer-set-master");

  url in1= tmscm_to_url (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  set_master_buffer (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_buffer_get_master (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-get-master");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  url out= get_master_buffer (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_buffer_set_title (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-set-title");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "buffer-set-title");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  set_title_buffer (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_buffer_get_title (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-get-title");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  string out= get_title_buffer (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_buffer_last_save (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-last-save");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  int out= get_last_save_buffer (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_buffer_last_visited (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-last-visited");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  double out= last_visited (in1);
  // TMSCM_ALLOW_INTS;

  return double_to_tmscm (out);
}

tmscm
tmg_buffer_modifiedP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-modified?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= buffer_modified (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_buffer_modified_since_autosaveP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-modified-since-autosave?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= buffer_modified_since_autosave (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_buffer_pretend_modified (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-pretend-modified");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  pretend_buffer_modified (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_buffer_pretend_saved (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-pretend-saved");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  pretend_buffer_saved (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_buffer_pretend_autosaved (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-pretend-autosaved");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  pretend_buffer_autosaved (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_buffer_attach_notifier (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-attach-notifier");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  attach_buffer_notifier (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_buffer_has_nameP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-has-name?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= buffer_has_name (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_buffer_auxP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-aux?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_aux_buffer (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_buffer_embeddedP (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-embedded?");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= is_embedded_buffer (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_buffer_import (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-import");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "buffer-import");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "buffer-import");

  url in1= tmscm_to_url (arg1);
  url in2= tmscm_to_url (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  bool out= buffer_import (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_buffer_load (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-load");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= buffer_load (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_buffer_export (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-export");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "buffer-export");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "buffer-export");

  url in1= tmscm_to_url (arg1);
  url in2= tmscm_to_url (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  bool out= buffer_export (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_buffer_save (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-save");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= buffer_save (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tree_import_loaded (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tree-import-loaded");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "tree-import-loaded");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "tree-import-loaded");

  string in1= tmscm_to_string (arg1);
  url in2= tmscm_to_url (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  tree out= import_loaded_tree (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_import (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tree-import");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tree-import");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  tree out= import_tree (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_inclusion (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tree-inclusion");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  tree out= load_inclusion (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_tree_export (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-export");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "tree-export");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "tree-export");

  tree in1= tmscm_to_tree (arg1);
  url in2= tmscm_to_url (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  bool out= export_tree (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tree_load_style (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tree-load-style");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= load_style_tree (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_buffer_focus (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-focus");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= focus_on_buffer (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_buffer_focus_dot (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-focus*");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= var_focus_on_buffer (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_view_list () {
  // TMSCM_DEFER_INTS;
  array_url out= get_all_views ();
  // TMSCM_ALLOW_INTS;

  return array_url_to_tmscm (out);
}

tmscm
tmg_buffer_2views (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer->views");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  array_url out= buffer_to_views (in1);
  // TMSCM_ALLOW_INTS;

  return array_url_to_tmscm (out);
}

tmscm
tmg_current_view_url () {
  // TMSCM_DEFER_INTS;
  url out= get_current_view_safe ();
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_window_2view (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "window->view");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  url out= window_to_view (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_view_2buffer (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "view->buffer");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  url out= view_to_buffer (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_view_2window_url (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "view->window-url");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  url out= view_to_window (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_view_new (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "view-new");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  url out= get_new_view (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_view_passive (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "view-passive");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  url out= get_passive_view (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_view_recent (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "view-recent");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  url out= get_recent_view (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_view_delete (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "view-delete");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  delete_view (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_window_set_view (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "window-set-view");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "window-set-view");
  TMSCM_ASSERT_BOOL (arg3, TMSCM_ARG3, "window-set-view");

  url in1= tmscm_to_url (arg1);
  url in2= tmscm_to_url (arg2);
  bool in3= tmscm_to_bool (arg3);

  // TMSCM_DEFER_INTS;
  window_set_view (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_switch_to_buffer (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "switch-to-buffer");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  switch_to_buffer (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_drd (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "set-drd");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  set_current_drd (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_window_list () {
  // TMSCM_DEFER_INTS;
  array_url out= windows_list ();
  // TMSCM_ALLOW_INTS;

  return array_url_to_tmscm (out);
}

tmscm
tmg_windows_number () {
  // TMSCM_DEFER_INTS;
  int out= get_nr_windows ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_current_window () {
  // TMSCM_DEFER_INTS;
  url out= get_current_window ();
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_buffer_2windows (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer->windows");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  array_url out= buffer_to_windows (in1);
  // TMSCM_ALLOW_INTS;

  return array_url_to_tmscm (out);
}

tmscm
tmg_window_to_buffer (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "window-to-buffer");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  url out= window_to_buffer (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_window_set_buffer (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "window-set-buffer");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "window-set-buffer");

  url in1= tmscm_to_url (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  window_set_buffer (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_window_focus (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "window-focus");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  window_focus (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_switch_to_window (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "switch-to-window");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  switch_to_window (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_new_buffer () {
  // TMSCM_DEFER_INTS;
  url out= create_buffer ();
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_open_buffer_in_window (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "open-buffer-in-window");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "open-buffer-in-window");
  TMSCM_ASSERT_CONTENT (arg3, TMSCM_ARG3, "open-buffer-in-window");

  url in1= tmscm_to_url (arg1);
  content in2= tmscm_to_content (arg2);
  content in3= tmscm_to_content (arg3);

  // TMSCM_DEFER_INTS;
  url out= new_buffer_in_new_window (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_open_window () {
  // TMSCM_DEFER_INTS;
  url out= open_window ();
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_open_window_geometry (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "open-window-geometry");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  url out= open_window (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_clone_window () {
  // TMSCM_DEFER_INTS;
  clone_window ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_buffer_close (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "cpp-buffer-close");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  kill_buffer (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_kill_window (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "kill-window");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  kill_window (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_kill_current_window_and_buffer () {
  // TMSCM_DEFER_INTS;
  kill_current_window_and_buffer ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_project_attach (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "project-attach");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  project_attach (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_project_detach () {
  // TMSCM_DEFER_INTS;
  project_attach ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_project_attachedP () {
  // TMSCM_DEFER_INTS;
  bool out= project_attached ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_project_get () {
  // TMSCM_DEFER_INTS;
  url out= project_get ();
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_alt_window_handle () {
  // TMSCM_DEFER_INTS;
  int out= window_handle ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_alt_window_create_quit (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "alt-window-create-quit");
  TMSCM_ASSERT_WIDGET (arg2, TMSCM_ARG2, "alt-window-create-quit");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "alt-window-create-quit");
  TMSCM_ASSERT_COMMAND (arg4, TMSCM_ARG4, "alt-window-create-quit");

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
tmg_alt_window_create_plain (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "alt-window-create-plain");
  TMSCM_ASSERT_WIDGET (arg2, TMSCM_ARG2, "alt-window-create-plain");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "alt-window-create-plain");

  int in1= tmscm_to_int (arg1);
  widget in2= tmscm_to_widget (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  window_create_plain (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_alt_window_create_popup (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "alt-window-create-popup");
  TMSCM_ASSERT_WIDGET (arg2, TMSCM_ARG2, "alt-window-create-popup");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "alt-window-create-popup");

  int in1= tmscm_to_int (arg1);
  widget in2= tmscm_to_widget (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  window_create_popup (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_alt_window_create_tooltip (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "alt-window-create-tooltip");
  TMSCM_ASSERT_WIDGET (arg2, TMSCM_ARG2, "alt-window-create-tooltip");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "alt-window-create-tooltip");

  int in1= tmscm_to_int (arg1);
  widget in2= tmscm_to_widget (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  window_create_tooltip (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_alt_window_delete (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "alt-window-delete");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  window_delete (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_alt_window_show (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "alt-window-show");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  window_show (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_alt_window_hide (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "alt-window-hide");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  window_hide (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_alt_window_get_size (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "alt-window-get-size");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  scheme_tree out= window_get_size (in1);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_alt_window_set_size (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "alt-window-set-size");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "alt-window-set-size");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "alt-window-set-size");

  int in1= tmscm_to_int (arg1);
  int in2= tmscm_to_int (arg2);
  int in3= tmscm_to_int (arg3);

  // TMSCM_DEFER_INTS;
  window_set_size (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_alt_window_get_position (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "alt-window-get-position");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  scheme_tree out= window_get_position (in1);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_alt_window_set_position (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "alt-window-set-position");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "alt-window-set-position");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "alt-window-set-position");

  int in1= tmscm_to_int (arg1);
  int in2= tmscm_to_int (arg2);
  int in3= tmscm_to_int (arg3);

  // TMSCM_DEFER_INTS;
  window_set_position (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_alt_window_search (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "alt-window-search");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  path out= window_search (in1);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_supports_bibtexP () {
  // TMSCM_DEFER_INTS;
  bool out= bibtex_present ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_bibtex_run (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "bibtex-run");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "bibtex-run");
  TMSCM_ASSERT_URL (arg3, TMSCM_ARG3, "bibtex-run");
  TMSCM_ASSERT_ARRAY_STRING (arg4, TMSCM_ARG4, "bibtex-run");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  url in3= tmscm_to_url (arg3);
  array_string in4= tmscm_to_array_string (arg4);

  // TMSCM_DEFER_INTS;
  tree out= bibtex_run (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
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
tmg_bib_locase_first (tmscm arg1) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "bib-locase-first");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);

  // TMSCM_DEFER_INTS;
  scheme_tree out= bib_locase_first (in1);
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
tmg_bib_default_preserve_case (tmscm arg1) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "bib-default-preserve-case");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);

  // TMSCM_DEFER_INTS;
  scheme_tree out= bib_default_preserve_case (in1);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_bib_default_upcase_first (tmscm arg1) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "bib-default-upcase-first");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);

  // TMSCM_DEFER_INTS;
  scheme_tree out= bib_default_upcase_first (in1);
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

tmscm
tmg_extract_attachments (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "extract-attachments");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  bool out= scm_extract_attachments (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_pdf_make_attachments (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "pdf-make-attachments");
  TMSCM_ASSERT_ARRAY_URL (arg2, TMSCM_ARG2, "pdf-make-attachments");
  TMSCM_ASSERT_URL (arg3, TMSCM_ARG3, "pdf-make-attachments");

  url in1= tmscm_to_url (arg1);
  array_url in2= tmscm_to_array_url (arg2);
  url in3= tmscm_to_url (arg3);

  // TMSCM_DEFER_INTS;
  bool out= pdf_hummus_make_attachments (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_pdf_get_linked_file_paths (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "pdf-get-linked-file-paths");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "pdf-get-linked-file-paths");

  tree in1= tmscm_to_tree (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  array_url out= get_linked_file_paths (in1, in2);
  // TMSCM_ALLOW_INTS;

  return array_url_to_tmscm (out);
}

tmscm
tmg_pdf_replace_linked_path (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "pdf-replace-linked-path");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "pdf-replace-linked-path");

  tree in1= tmscm_to_tree (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  tree out= replace_with_relative_path (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_pdf_get_attached_main_tm (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "pdf-get-attached-main-tm");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  url out= get_main_tm (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_array_url_append (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "array-url-append");
  TMSCM_ASSERT_ARRAY_URL (arg2, TMSCM_ARG2, "array-url-append");

  url in1= tmscm_to_url (arg1);
  array_url in2= tmscm_to_array_url (arg2);

  // TMSCM_DEFER_INTS;
  array_url out= append (in1, in2);
  // TMSCM_ALLOW_INTS;

  return array_url_to_tmscm (out);
}

void
initialize_glue_basic () {
  tmscm_install_procedure ("texmacs-version-release",  tmg_texmacs_version_release, 1, 0, 0);
  tmscm_install_procedure ("version-before?",  tmg_version_beforeP, 2, 0, 0);
  tmscm_install_procedure ("updater-supported?",  tmg_updater_supportedP, 0, 0, 0);
  tmscm_install_procedure ("updater-running?",  tmg_updater_runningP, 0, 0, 0);
  tmscm_install_procedure ("updater-check-background",  tmg_updater_check_background, 0, 0, 0);
  tmscm_install_procedure ("updater-check-foreground",  tmg_updater_check_foreground, 0, 0, 0);
  tmscm_install_procedure ("updater-last-check",  tmg_updater_last_check, 0, 0, 0);
  tmscm_install_procedure ("updater-set-interval",  tmg_updater_set_interval, 1, 0, 0);
  tmscm_install_procedure ("get-original-path",  tmg_get_original_path, 0, 0, 0);
  tmscm_install_procedure ("os-win32?",  tmg_os_win32P, 0, 0, 0);
  tmscm_install_procedure ("os-mingw?",  tmg_os_mingwP, 0, 0, 0);
  tmscm_install_procedure ("os-macos?",  tmg_os_macosP, 0, 0, 0);
  tmscm_install_procedure ("has-printing-cmd?",  tmg_has_printing_cmdP, 0, 0, 0);
  tmscm_install_procedure ("x-gui?",  tmg_x_guiP, 0, 0, 0);
  tmscm_install_procedure ("qt-gui?",  tmg_qt_guiP, 0, 0, 0);
  tmscm_install_procedure ("gui-version",  tmg_gui_version, 0, 0, 0);
  tmscm_install_procedure ("default-look-and-feel",  tmg_default_look_and_feel, 0, 0, 0);
  tmscm_install_procedure ("default-chinese-font",  tmg_default_chinese_font, 0, 0, 0);
  tmscm_install_procedure ("default-japanese-font",  tmg_default_japanese_font, 0, 0, 0);
  tmscm_install_procedure ("default-korean-font",  tmg_default_korean_font, 0, 0, 0);
  tmscm_install_procedure ("get-retina-factor",  tmg_get_retina_factor, 0, 0, 0);
  tmscm_install_procedure ("get-retina-zoom",  tmg_get_retina_zoom, 0, 0, 0);
  tmscm_install_procedure ("get-retina-icons",  tmg_get_retina_icons, 0, 0, 0);
  tmscm_install_procedure ("get-retina-scale",  tmg_get_retina_scale, 0, 0, 0);
  tmscm_install_procedure ("set-retina-factor",  tmg_set_retina_factor, 1, 0, 0);
  tmscm_install_procedure ("set-retina-zoom",  tmg_set_retina_zoom, 1, 0, 0);
  tmscm_install_procedure ("set-retina-icons",  tmg_set_retina_icons, 1, 0, 0);
  tmscm_install_procedure ("set-retina-scale",  tmg_set_retina_scale, 1, 0, 0);
  tmscm_install_procedure ("tm-output",  tmg_tm_output, 1, 0, 0);
  tmscm_install_procedure ("tm-errput",  tmg_tm_errput, 1, 0, 0);
  tmscm_install_procedure ("win32-display",  tmg_win32_display, 1, 0, 0);
  tmscm_install_procedure ("cpp-error",  tmg_cpp_error, 0, 0, 0);
  tmscm_install_procedure ("supports-native-pdf?",  tmg_supports_native_pdfP, 0, 0, 0);
  tmscm_install_procedure ("supports-ghostscript?",  tmg_supports_ghostscriptP, 0, 0, 0);
  tmscm_install_procedure ("rescue-mode?",  tmg_rescue_modeP, 0, 0, 0);
  tmscm_install_procedure ("scheme-dialect",  tmg_scheme_dialect, 0, 0, 0);
  tmscm_install_procedure ("get-texmacs-path",  tmg_get_texmacs_path, 0, 0, 0);
  tmscm_install_procedure ("get-texmacs-home-path",  tmg_get_texmacs_home_path, 0, 0, 0);
  tmscm_install_procedure ("get-user-login",  tmg_get_user_login, 0, 0, 0);
  tmscm_install_procedure ("get-user-name",  tmg_get_user_name, 0, 0, 0);
  tmscm_install_procedure ("plugin-list",  tmg_plugin_list, 0, 0, 0);
  tmscm_install_procedure ("set-fast-environments",  tmg_set_fast_environments, 1, 0, 0);
  tmscm_install_procedure ("font-exists-in-tt?",  tmg_font_exists_in_ttP, 1, 0, 0);
  tmscm_install_procedure ("eval-system",  tmg_eval_system, 1, 0, 0);
  tmscm_install_procedure ("var-eval-system",  tmg_var_eval_system, 1, 0, 0);
  tmscm_install_procedure ("evaluate-system",  tmg_evaluate_system, 4, 0, 0);
  tmscm_install_procedure ("get-locale-language",  tmg_get_locale_language, 0, 0, 0);
  tmscm_install_procedure ("get-locale-charset",  tmg_get_locale_charset, 0, 0, 0);
  tmscm_install_procedure ("locale-to-language",  tmg_locale_to_language, 1, 0, 0);
  tmscm_install_procedure ("language-to-locale",  tmg_language_to_locale, 1, 0, 0);
  tmscm_install_procedure ("texmacs-time",  tmg_texmacs_time, 0, 0, 0);
  tmscm_install_procedure ("pretty-time",  tmg_pretty_time, 1, 0, 0);
  tmscm_install_procedure ("texmacs-memory",  tmg_texmacs_memory, 0, 0, 0);
  tmscm_install_procedure ("bench-print",  tmg_bench_print, 1, 0, 0);
  tmscm_install_procedure ("bench-print-all",  tmg_bench_print_all, 0, 0, 0);
  tmscm_install_procedure ("system-wait",  tmg_system_wait, 2, 0, 0);
  tmscm_install_procedure ("get-show-kbd",  tmg_get_show_kbd, 0, 0, 0);
  tmscm_install_procedure ("set-show-kbd",  tmg_set_show_kbd, 1, 0, 0);
  tmscm_install_procedure ("set-latex-command",  tmg_set_latex_command, 1, 0, 0);
  tmscm_install_procedure ("set-bibtex-command",  tmg_set_bibtex_command, 1, 0, 0);
  tmscm_install_procedure ("number-latex-errors",  tmg_number_latex_errors, 1, 0, 0);
  tmscm_install_procedure ("number-latex-pages",  tmg_number_latex_pages, 1, 0, 0);
  tmscm_install_procedure ("math-symbol-group",  tmg_math_symbol_group, 1, 0, 0);
  tmscm_install_procedure ("math-group-members",  tmg_math_group_members, 1, 0, 0);
  tmscm_install_procedure ("math-symbol-type",  tmg_math_symbol_type, 1, 0, 0);
  tmscm_install_procedure ("object->command",  tmg_object_2command, 1, 0, 0);
  tmscm_install_procedure ("command-eval",  tmg_command_eval, 1, 0, 0);
  tmscm_install_procedure ("command-apply",  tmg_command_apply, 2, 0, 0);
  tmscm_install_procedure ("exec-delayed",  tmg_exec_delayed, 1, 0, 0);
  tmscm_install_procedure ("exec-delayed-pause",  tmg_exec_delayed_pause, 1, 0, 0);
  tmscm_install_procedure ("protected-call",  tmg_protected_call, 1, 0, 0);
  tmscm_install_procedure ("notify-preferences-booted",  tmg_notify_preferences_booted, 0, 0, 0);
  tmscm_install_procedure ("cpp-has-preference?",  tmg_cpp_has_preferenceP, 1, 0, 0);
  tmscm_install_procedure ("cpp-get-preference",  tmg_cpp_get_preference, 2, 0, 0);
  tmscm_install_procedure ("cpp-set-preference",  tmg_cpp_set_preference, 2, 0, 0);
  tmscm_install_procedure ("cpp-reset-preference",  tmg_cpp_reset_preference, 1, 0, 0);
  tmscm_install_procedure ("save-preferences",  tmg_save_preferences, 0, 0, 0);
  tmscm_install_procedure ("get-default-printing-command",  tmg_get_default_printing_command, 0, 0, 0);
  tmscm_install_procedure ("set-input-language",  tmg_set_input_language, 1, 0, 0);
  tmscm_install_procedure ("get-input-language",  tmg_get_input_language, 0, 0, 0);
  tmscm_install_procedure ("set-output-language",  tmg_set_output_language, 1, 0, 0);
  tmscm_install_procedure ("get-output-language",  tmg_get_output_language, 0, 0, 0);
  tmscm_install_procedure ("translate",  tmg_translate, 1, 0, 0);
  tmscm_install_procedure ("string-translate",  tmg_string_translate, 1, 0, 0);
  tmscm_install_procedure ("translate-from-to",  tmg_translate_from_to, 3, 0, 0);
  tmscm_install_procedure ("tree-translate",  tmg_tree_translate, 1, 0, 0);
  tmscm_install_procedure ("tree-translate-from-to",  tmg_tree_translate_from_to, 3, 0, 0);
  tmscm_install_procedure ("force-load-translations",  tmg_force_load_translations, 2, 0, 0);
  tmscm_install_procedure ("color",  tmg_color, 1, 0, 0);
  tmscm_install_procedure ("get-hex-color",  tmg_get_hex_color, 1, 0, 0);
  tmscm_install_procedure ("named-color->xcolormap",  tmg_named_color_2xcolormap, 1, 0, 0);
  tmscm_install_procedure ("rgba->named-color",  tmg_rgba_2named_color, 1, 0, 0);
  tmscm_install_procedure ("named-color->rgba",  tmg_named_color_2rgba, 1, 0, 0);
  tmscm_install_procedure ("new-author",  tmg_new_author, 0, 0, 0);
  tmscm_install_procedure ("set-author",  tmg_set_author, 1, 0, 0);
  tmscm_install_procedure ("get-author",  tmg_get_author, 0, 0, 0);
  tmscm_install_procedure ("debug-set",  tmg_debug_set, 2, 0, 0);
  tmscm_install_procedure ("debug-get",  tmg_debug_get, 1, 0, 0);
  tmscm_install_procedure ("debug-message",  tmg_debug_message, 2, 0, 0);
  tmscm_install_procedure ("get-debug-messages",  tmg_get_debug_messages, 2, 0, 0);
  tmscm_install_procedure ("clear-debug-messages",  tmg_clear_debug_messages, 0, 0, 0);
  tmscm_install_procedure ("cout-buffer",  tmg_cout_buffer, 0, 0, 0);
  tmscm_install_procedure ("cout-unbuffer",  tmg_cout_unbuffer, 0, 0, 0);
  tmscm_install_procedure ("mark-new",  tmg_mark_new, 0, 0, 0);
  tmscm_install_procedure ("glyph-register",  tmg_glyph_register, 2, 0, 0);
  tmscm_install_procedure ("glyph-recognize",  tmg_glyph_recognize, 1, 0, 0);
  tmscm_install_procedure ("set-new-fonts",  tmg_set_new_fonts, 1, 0, 0);
  tmscm_install_procedure ("new-fonts?",  tmg_new_fontsP, 0, 0, 0);
  tmscm_install_procedure ("tmtm-eqnumber->nonumber",  tmg_tmtm_eqnumber_2nonumber, 1, 0, 0);
  tmscm_install_procedure ("busy-versioning?",  tmg_busy_versioningP, 0, 0, 0);
  tmscm_install_procedure ("players-set-elapsed",  tmg_players_set_elapsed, 2, 0, 0);
  tmscm_install_procedure ("players-set-speed",  tmg_players_set_speed, 2, 0, 0);
  tmscm_install_procedure ("apply-effect",  tmg_apply_effect, 5, 0, 0);
  tmscm_install_procedure ("tt-exists?",  tmg_tt_existsP, 1, 0, 0);
  tmscm_install_procedure ("tt-dump",  tmg_tt_dump, 1, 0, 0);
  tmscm_install_procedure ("tt-font-name",  tmg_tt_font_name, 1, 0, 0);
  tmscm_install_procedure ("tt-analyze",  tmg_tt_analyze, 1, 0, 0);
  tmscm_install_procedure ("font-database-build",  tmg_font_database_build, 1, 0, 0);
  tmscm_install_procedure ("font-database-build-local",  tmg_font_database_build_local, 0, 0, 0);
  tmscm_install_procedure ("font-database-extend-local",  tmg_font_database_extend_local, 1, 0, 0);
  tmscm_install_procedure ("font-database-build-global",  tmg_font_database_build_global, 0, 0, 0);
  tmscm_install_procedure ("font-database-build-characteristics",  tmg_font_database_build_characteristics, 1, 0, 0);
  tmscm_install_procedure ("font-database-insert-global",  tmg_font_database_insert_global, 1, 0, 0);
  tmscm_install_procedure ("font-database-save-local-delta",  tmg_font_database_save_local_delta, 0, 0, 0);
  tmscm_install_procedure ("font-database-load",  tmg_font_database_load, 0, 0, 0);
  tmscm_install_procedure ("font-database-save",  tmg_font_database_save, 0, 0, 0);
  tmscm_install_procedure ("font-database-filter",  tmg_font_database_filter, 0, 0, 0);
  tmscm_install_procedure ("font-database-families",  tmg_font_database_families, 0, 0, 0);
  tmscm_install_procedure ("font-database-delta-families",  tmg_font_database_delta_families, 0, 0, 0);
  tmscm_install_procedure ("font-database-styles",  tmg_font_database_styles, 1, 0, 0);
  tmscm_install_procedure ("font-database-search",  tmg_font_database_search, 2, 0, 0);
  tmscm_install_procedure ("font-database-characteristics",  tmg_font_database_characteristics, 2, 0, 0);
  tmscm_install_procedure ("font-database-substitutions",  tmg_font_database_substitutions, 1, 0, 0);
  tmscm_install_procedure ("font-family->master",  tmg_font_family_2master, 1, 0, 0);
  tmscm_install_procedure ("font-master->families",  tmg_font_master_2families, 1, 0, 0);
  tmscm_install_procedure ("font-master-features",  tmg_font_master_features, 1, 0, 0);
  tmscm_install_procedure ("font-family-features",  tmg_font_family_features, 1, 0, 0);
  tmscm_install_procedure ("font-family-strict-features",  tmg_font_family_strict_features, 1, 0, 0);
  tmscm_install_procedure ("font-style-features",  tmg_font_style_features, 1, 0, 0);
  tmscm_install_procedure ("font-guessed-features",  tmg_font_guessed_features, 2, 0, 0);
  tmscm_install_procedure ("font-guessed-distance",  tmg_font_guessed_distance, 4, 0, 0);
  tmscm_install_procedure ("font-master-guessed-distance",  tmg_font_master_guessed_distance, 2, 0, 0);
  tmscm_install_procedure ("font-family-guessed-features",  tmg_font_family_guessed_features, 2, 0, 0);
  tmscm_install_procedure ("characteristic-distance",  tmg_characteristic_distance, 2, 0, 0);
  tmscm_install_procedure ("trace-distance",  tmg_trace_distance, 3, 0, 0);
  tmscm_install_procedure ("logical-font-public",  tmg_logical_font_public, 2, 0, 0);
  tmscm_install_procedure ("logical-font-exact",  tmg_logical_font_exact, 2, 0, 0);
  tmscm_install_procedure ("logical-font-private",  tmg_logical_font_private, 4, 0, 0);
  tmscm_install_procedure ("logical-font-family",  tmg_logical_font_family, 1, 0, 0);
  tmscm_install_procedure ("logical-font-variant",  tmg_logical_font_variant, 1, 0, 0);
  tmscm_install_procedure ("logical-font-series",  tmg_logical_font_series, 1, 0, 0);
  tmscm_install_procedure ("logical-font-shape",  tmg_logical_font_shape, 1, 0, 0);
  tmscm_install_procedure ("logical-font-search",  tmg_logical_font_search, 1, 0, 0);
  tmscm_install_procedure ("logical-font-search-exact",  tmg_logical_font_search_exact, 1, 0, 0);
  tmscm_install_procedure ("search-font-families",  tmg_search_font_families, 1, 0, 0);
  tmscm_install_procedure ("search-font-styles",  tmg_search_font_styles, 2, 0, 0);
  tmscm_install_procedure ("logical-font-patch",  tmg_logical_font_patch, 2, 0, 0);
  tmscm_install_procedure ("logical-font-substitute",  tmg_logical_font_substitute, 1, 0, 0);
  tmscm_install_procedure ("font-family-main",  tmg_font_family_main, 1, 0, 0);
  tmscm_install_procedure ("image->psdoc",  tmg_image_2psdoc, 1, 0, 0);
  tmscm_install_procedure ("anim-control-times",  tmg_anim_control_times, 1, 0, 0);
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
  tmscm_install_procedure ("tree-label-macro?",  tmg_tree_label_macroP, 1, 0, 0);
  tmscm_install_procedure ("tree-label-parameter?",  tmg_tree_label_parameterP, 1, 0, 0);
  tmscm_install_procedure ("tree-label-type",  tmg_tree_label_type, 1, 0, 0);
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
  tmscm_install_procedure ("tree-child-env*",  tmg_tree_child_env_dot, 3, 0, 0);
  tmscm_install_procedure ("tree-child-env",  tmg_tree_child_env, 4, 0, 0);
  tmscm_install_procedure ("tree-descendant-env*",  tmg_tree_descendant_env_dot, 3, 0, 0);
  tmscm_install_procedure ("tree-descendant-env",  tmg_tree_descendant_env, 4, 0, 0);
  tmscm_install_procedure ("tree-load-inclusion",  tmg_tree_load_inclusion, 1, 0, 0);
  tmscm_install_procedure ("tree-as-string",  tmg_tree_as_string, 1, 0, 0);
  tmscm_install_procedure ("tree-extents",  tmg_tree_extents, 1, 0, 0);
  tmscm_install_procedure ("tree-empty?",  tmg_tree_emptyP, 1, 0, 0);
  tmscm_install_procedure ("tree-multi-line?",  tmg_tree_multi_lineP, 1, 0, 0);
  tmscm_install_procedure ("tree-is-buffer?",  tmg_tree_is_bufferP, 1, 0, 0);
  tmscm_install_procedure ("tree-search-sections",  tmg_tree_search_sections, 1, 0, 0);
  tmscm_install_procedure ("tree-search-tree",  tmg_tree_search_tree, 4, 0, 0);
  tmscm_install_procedure ("tree-search-tree-at",  tmg_tree_search_tree_at, 5, 0, 0);
  tmscm_install_procedure ("tree-spell",  tmg_tree_spell, 4, 0, 0);
  tmscm_install_procedure ("tree-spell-at",  tmg_tree_spell_at, 5, 0, 0);
  tmscm_install_procedure ("tree-spell-selection",  tmg_tree_spell_selection, 6, 0, 0);
  tmscm_install_procedure ("previous-search-hit",  tmg_previous_search_hit, 3, 0, 0);
  tmscm_install_procedure ("next-search-hit",  tmg_next_search_hit, 3, 0, 0);
  tmscm_install_procedure ("navigate-search-hit",  tmg_navigate_search_hit, 4, 0, 0);
  tmscm_install_procedure ("tag-minimal-arity",  tmg_tag_minimal_arity, 1, 0, 0);
  tmscm_install_procedure ("tag-maximal-arity",  tmg_tag_maximal_arity, 1, 0, 0);
  tmscm_install_procedure ("tag-possible-arity?",  tmg_tag_possible_arityP, 2, 0, 0);
  tmscm_install_procedure ("set-access-mode",  tmg_set_access_mode, 1, 0, 0);
  tmscm_install_procedure ("get-access-mode",  tmg_get_access_mode, 0, 0, 0);
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
  tmscm_install_procedure ("math-stats-compile",  tmg_math_stats_compile, 3, 0, 0);
  tmscm_install_procedure ("math-stats-occurrences",  tmg_math_stats_occurrences, 2, 0, 0);
  tmscm_install_procedure ("math-stats-number-in-role",  tmg_math_stats_number_in_role, 2, 0, 0);
  tmscm_install_procedure ("path-strip",  tmg_path_strip, 2, 0, 0);
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
  tmscm_install_procedure ("make-modification",  tmg_make_modification, 3, 0, 0);
  tmscm_install_procedure ("modification-assign",  tmg_modification_assign, 2, 0, 0);
  tmscm_install_procedure ("modification-insert",  tmg_modification_insert, 3, 0, 0);
  tmscm_install_procedure ("modification-remove",  tmg_modification_remove, 3, 0, 0);
  tmscm_install_procedure ("modification-split",  tmg_modification_split, 3, 0, 0);
  tmscm_install_procedure ("modification-join",  tmg_modification_join, 2, 0, 0);
  tmscm_install_procedure ("modification-assign-node",  tmg_modification_assign_node, 2, 0, 0);
  tmscm_install_procedure ("modification-insert-node",  tmg_modification_insert_node, 3, 0, 0);
  tmscm_install_procedure ("modification-remove-node",  tmg_modification_remove_node, 2, 0, 0);
  tmscm_install_procedure ("modification-set-cursor",  tmg_modification_set_cursor, 3, 0, 0);
  tmscm_install_procedure ("modification-kind",  tmg_modification_kind, 1, 0, 0);
  tmscm_install_procedure ("modification-path",  tmg_modification_path, 1, 0, 0);
  tmscm_install_procedure ("modification-tree",  tmg_modification_tree, 1, 0, 0);
  tmscm_install_procedure ("modification-root",  tmg_modification_root, 1, 0, 0);
  tmscm_install_procedure ("modification-index",  tmg_modification_index, 1, 0, 0);
  tmscm_install_procedure ("modification-argument",  tmg_modification_argument, 1, 0, 0);
  tmscm_install_procedure ("modification-label",  tmg_modification_label, 1, 0, 0);
  tmscm_install_procedure ("modification-copy",  tmg_modification_copy, 1, 0, 0);
  tmscm_install_procedure ("modification-applicable?",  tmg_modification_applicableP, 2, 0, 0);
  tmscm_install_procedure ("modification-apply",  tmg_modification_apply, 2, 0, 0);
  tmscm_install_procedure ("modification-inplace-apply",  tmg_modification_inplace_apply, 2, 0, 0);
  tmscm_install_procedure ("modification-invert",  tmg_modification_invert, 2, 0, 0);
  tmscm_install_procedure ("modification-commute?",  tmg_modification_commuteP, 2, 0, 0);
  tmscm_install_procedure ("modification-can-pull?",  tmg_modification_can_pullP, 2, 0, 0);
  tmscm_install_procedure ("modification-pull",  tmg_modification_pull, 2, 0, 0);
  tmscm_install_procedure ("modification-co-pull",  tmg_modification_co_pull, 2, 0, 0);
  tmscm_install_procedure ("patch-pair",  tmg_patch_pair, 2, 0, 0);
  tmscm_install_procedure ("patch-compound",  tmg_patch_compound, 1, 0, 0);
  tmscm_install_procedure ("patch-branch",  tmg_patch_branch, 1, 0, 0);
  tmscm_install_procedure ("patch-birth",  tmg_patch_birth, 2, 0, 0);
  tmscm_install_procedure ("patch-author",  tmg_patch_author, 2, 0, 0);
  tmscm_install_procedure ("patch-pair?",  tmg_patch_pairP, 1, 0, 0);
  tmscm_install_procedure ("patch-compound?",  tmg_patch_compoundP, 1, 0, 0);
  tmscm_install_procedure ("patch-branch?",  tmg_patch_branchP, 1, 0, 0);
  tmscm_install_procedure ("patch-birth?",  tmg_patch_birthP, 1, 0, 0);
  tmscm_install_procedure ("patch-author?",  tmg_patch_authorP, 1, 0, 0);
  tmscm_install_procedure ("patch-arity",  tmg_patch_arity, 1, 0, 0);
  tmscm_install_procedure ("patch-ref",  tmg_patch_ref, 2, 0, 0);
  tmscm_install_procedure ("patch-direct",  tmg_patch_direct, 1, 0, 0);
  tmscm_install_procedure ("patch-inverse",  tmg_patch_inverse, 1, 0, 0);
  tmscm_install_procedure ("patch-get-birth",  tmg_patch_get_birth, 1, 0, 0);
  tmscm_install_procedure ("patch-get-author",  tmg_patch_get_author, 1, 0, 0);
  tmscm_install_procedure ("patch-copy",  tmg_patch_copy, 1, 0, 0);
  tmscm_install_procedure ("patch-applicable?",  tmg_patch_applicableP, 2, 0, 0);
  tmscm_install_procedure ("patch-apply",  tmg_patch_apply, 2, 0, 0);
  tmscm_install_procedure ("patch-inplace-apply",  tmg_patch_inplace_apply, 2, 0, 0);
  tmscm_install_procedure ("patch-compactify",  tmg_patch_compactify, 1, 0, 0);
  tmscm_install_procedure ("patch-cursor-hint",  tmg_patch_cursor_hint, 2, 0, 0);
  tmscm_install_procedure ("patch-invert",  tmg_patch_invert, 2, 0, 0);
  tmscm_install_procedure ("patch-commute?",  tmg_patch_commuteP, 2, 0, 0);
  tmscm_install_procedure ("patch-can-pull?",  tmg_patch_can_pullP, 2, 0, 0);
  tmscm_install_procedure ("patch-pull",  tmg_patch_pull, 2, 0, 0);
  tmscm_install_procedure ("patch-co-pull",  tmg_patch_co_pull, 2, 0, 0);
  tmscm_install_procedure ("patch-remove-set-cursor",  tmg_patch_remove_set_cursor, 1, 0, 0);
  tmscm_install_procedure ("patch-modifies?",  tmg_patch_modifiesP, 1, 0, 0);
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
  tmscm_install_procedure ("cpp-string-number?",  tmg_cpp_string_numberP, 1, 0, 0);
  tmscm_install_procedure ("string-occurs?",  tmg_string_occursP, 2, 0, 0);
  tmscm_install_procedure ("string-count-occurrences",  tmg_string_count_occurrences, 2, 0, 0);
  tmscm_install_procedure ("string-search-forwards",  tmg_string_search_forwards, 3, 0, 0);
  tmscm_install_procedure ("string-search-backwards",  tmg_string_search_backwards, 3, 0, 0);
  tmscm_install_procedure ("string-overlapping",  tmg_string_overlapping, 2, 0, 0);
  tmscm_install_procedure ("string-replace",  tmg_string_replace, 3, 0, 0);
  tmscm_install_procedure ("string-find-non-alpha",  tmg_string_find_non_alpha, 3, 0, 0);
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
  tmscm_install_procedure ("escape-to-ascii",  tmg_escape_to_ascii, 1, 0, 0);
  tmscm_install_procedure ("unescape-guile",  tmg_unescape_guile, 1, 0, 0);
  tmscm_install_procedure ("string-quote",  tmg_string_quote, 1, 0, 0);
  tmscm_install_procedure ("string-unquote",  tmg_string_unquote, 1, 0, 0);
  tmscm_install_procedure ("string-trim-spaces-left",  tmg_string_trim_spaces_left, 1, 0, 0);
  tmscm_install_procedure ("string-trim-spaces-right",  tmg_string_trim_spaces_right, 1, 0, 0);
  tmscm_install_procedure ("string-trim-spaces",  tmg_string_trim_spaces, 1, 0, 0);
  tmscm_install_procedure ("downgrade-math-letters",  tmg_downgrade_math_letters, 1, 0, 0);
  tmscm_install_procedure ("string-convert",  tmg_string_convert, 3, 0, 0);
  tmscm_install_procedure ("encode-base64",  tmg_encode_base64, 1, 0, 0);
  tmscm_install_procedure ("decode-base64",  tmg_decode_base64, 1, 0, 0);
  tmscm_install_procedure ("sourcecode->cork",  tmg_sourcecode_2cork, 1, 0, 0);
  tmscm_install_procedure ("cork->sourcecode",  tmg_cork_2sourcecode, 1, 0, 0);
  tmscm_install_procedure ("utf8->cork",  tmg_utf8_2cork, 1, 0, 0);
  tmscm_install_procedure ("cork->utf8",  tmg_cork_2utf8, 1, 0, 0);
  tmscm_install_procedure ("utf8->t2a",  tmg_utf8_2t2a, 1, 0, 0);
  tmscm_install_procedure ("t2a->utf8",  tmg_t2a_2utf8, 1, 0, 0);
  tmscm_install_procedure ("utf8->html",  tmg_utf8_2html, 1, 0, 0);
  tmscm_install_procedure ("guess-wencoding",  tmg_guess_wencoding, 1, 0, 0);
  tmscm_install_procedure ("tm->xml-name",  tmg_tm_2xml_name, 1, 0, 0);
  tmscm_install_procedure ("old-tm->xml-cdata",  tmg_old_tm_2xml_cdata, 1, 0, 0);
  tmscm_install_procedure ("tm->xml-cdata",  tmg_tm_2xml_cdata, 1, 0, 0);
  tmscm_install_procedure ("xml-name->tm",  tmg_xml_name_2tm, 1, 0, 0);
  tmscm_install_procedure ("old-xml-cdata->tm",  tmg_old_xml_cdata_2tm, 1, 0, 0);
  tmscm_install_procedure ("xml-unspace",  tmg_xml_unspace, 3, 0, 0);
  tmscm_install_procedure ("integer->hexadecimal",  tmg_integer_2hexadecimal, 1, 0, 0);
  tmscm_install_procedure ("integer->padded-hexadecimal",  tmg_integer_2padded_hexadecimal, 2, 0, 0);
  tmscm_install_procedure ("hexadecimal->integer",  tmg_hexadecimal_2integer, 1, 0, 0);
  tmscm_install_procedure ("cpp-string-tokenize",  tmg_cpp_string_tokenize, 2, 0, 0);
  tmscm_install_procedure ("cpp-string-recompose",  tmg_cpp_string_recompose, 2, 0, 0);
  tmscm_install_procedure ("string-differences",  tmg_string_differences, 2, 0, 0);
  tmscm_install_procedure ("string-distance",  tmg_string_distance, 2, 0, 0);
  tmscm_install_procedure ("find-left-bracket",  tmg_find_left_bracket, 3, 0, 0);
  tmscm_install_procedure ("find-right-bracket",  tmg_find_right_bracket, 3, 0, 0);
  tmscm_install_procedure ("string->tmstring",  tmg_string_2tmstring, 1, 0, 0);
  tmscm_install_procedure ("tmstring->string",  tmg_tmstring_2string, 1, 0, 0);
  tmscm_install_procedure ("tmstring-length",  tmg_tmstring_length, 1, 0, 0);
  tmscm_install_procedure ("tmstring-ref",  tmg_tmstring_ref, 2, 0, 0);
  tmscm_install_procedure ("tmstring-reverse-ref",  tmg_tmstring_reverse_ref, 2, 0, 0);
  tmscm_install_procedure ("tmstring->list",  tmg_tmstring_2list, 1, 0, 0);
  tmscm_install_procedure ("list->tmstring",  tmg_list_2tmstring, 1, 0, 0);
  tmscm_install_procedure ("string-next",  tmg_string_next, 2, 0, 0);
  tmscm_install_procedure ("string-previous",  tmg_string_previous, 2, 0, 0);
  tmscm_install_procedure ("tmstring-split",  tmg_tmstring_split, 1, 0, 0);
  tmscm_install_procedure ("tmstring-translit",  tmg_tmstring_translit, 1, 0, 0);
  tmscm_install_procedure ("tmstring-locase-first",  tmg_tmstring_locase_first, 1, 0, 0);
  tmscm_install_procedure ("tmstring-upcase-first",  tmg_tmstring_upcase_first, 1, 0, 0);
  tmscm_install_procedure ("tmstring-locase-all",  tmg_tmstring_locase_all, 1, 0, 0);
  tmscm_install_procedure ("tmstring-upcase-all",  tmg_tmstring_upcase_all, 1, 0, 0);
  tmscm_install_procedure ("tmstring-unaccent-all",  tmg_tmstring_unaccent_all, 1, 0, 0);
  tmscm_install_procedure ("tmstring-letter?",  tmg_tmstring_letterP, 1, 0, 0);
  tmscm_install_procedure ("tmstring-before?",  tmg_tmstring_beforeP, 2, 0, 0);
  tmscm_install_procedure ("multi-spell-start",  tmg_multi_spell_start, 0, 0, 0);
  tmscm_install_procedure ("multi-spell-done",  tmg_multi_spell_done, 0, 0, 0);
  tmscm_install_procedure ("single-spell-start",  tmg_single_spell_start, 1, 0, 0);
  tmscm_install_procedure ("single-spell-done",  tmg_single_spell_done, 1, 0, 0);
  tmscm_install_procedure ("spell-check",  tmg_spell_check, 2, 0, 0);
  tmscm_install_procedure ("spell-check?",  tmg_spell_checkP, 2, 0, 0);
  tmscm_install_procedure ("spell-accept",  tmg_spell_accept, 2, 0, 0);
  tmscm_install_procedure ("spell-var-accept",  tmg_spell_var_accept, 3, 0, 0);
  tmscm_install_procedure ("spell-insert",  tmg_spell_insert, 2, 0, 0);
  tmscm_install_procedure ("packrat-define",  tmg_packrat_define, 3, 0, 0);
  tmscm_install_procedure ("packrat-property",  tmg_packrat_property, 4, 0, 0);
  tmscm_install_procedure ("packrat-inherit",  tmg_packrat_inherit, 2, 0, 0);
  tmscm_install_procedure ("packrat-parse",  tmg_packrat_parse, 3, 0, 0);
  tmscm_install_procedure ("packrat-correct?",  tmg_packrat_correctP, 3, 0, 0);
  tmscm_install_procedure ("packrat-context",  tmg_packrat_context, 4, 0, 0);
  tmscm_install_procedure ("syntax-read-preferences",  tmg_syntax_read_preferences, 1, 0, 0);
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
  tmscm_install_procedure ("cpp-latex-document->texmacs",  tmg_cpp_latex_document_2texmacs, 2, 0, 0);
  tmscm_install_procedure ("latex-class-document->texmacs",  tmg_latex_class_document_2texmacs, 1, 0, 0);
  tmscm_install_procedure ("tracked-latex->texmacs",  tmg_tracked_latex_2texmacs, 2, 0, 0);
  tmscm_install_procedure ("conservative-texmacs->latex",  tmg_conservative_texmacs_2latex, 2, 0, 0);
  tmscm_install_procedure ("tracked-texmacs->latex",  tmg_tracked_texmacs_2latex, 2, 0, 0);
  tmscm_install_procedure ("conservative-latex->texmacs",  tmg_conservative_latex_2texmacs, 2, 0, 0);
  tmscm_install_procedure ("get-line-number",  tmg_get_line_number, 2, 0, 0);
  tmscm_install_procedure ("get-column-number",  tmg_get_column_number, 2, 0, 0);
  tmscm_install_procedure ("try-latex-export",  tmg_try_latex_export, 4, 0, 0);
  tmscm_install_procedure ("parse-xml",  tmg_parse_xml, 1, 0, 0);
  tmscm_install_procedure ("parse-html",  tmg_parse_html, 1, 0, 0);
  tmscm_install_procedure ("parse-bib",  tmg_parse_bib, 1, 0, 0);
  tmscm_install_procedure ("conservative-bib-import",  tmg_conservative_bib_import, 3, 0, 0);
  tmscm_install_procedure ("conservative-bib-export",  tmg_conservative_bib_export, 3, 0, 0);
  tmscm_install_procedure ("clean-html",  tmg_clean_html, 1, 0, 0);
  tmscm_install_procedure ("upgrade-tmml",  tmg_upgrade_tmml, 1, 0, 0);
  tmscm_install_procedure ("upgrade-mathml",  tmg_upgrade_mathml, 1, 0, 0);
  tmscm_install_procedure ("retrieve-mathjax",  tmg_retrieve_mathjax, 1, 0, 0);
  tmscm_install_procedure ("vernac->texmacs",  tmg_vernac_2texmacs, 1, 0, 0);
  tmscm_install_procedure ("vernac-document->texmacs",  tmg_vernac_document_2texmacs, 1, 0, 0);
  tmscm_install_procedure ("compute-keys-string",  tmg_compute_keys_string, 2, 0, 0);
  tmscm_install_procedure ("compute-keys-tree",  tmg_compute_keys_tree, 2, 0, 0);
  tmscm_install_procedure ("compute-keys-url",  tmg_compute_keys_url, 1, 0, 0);
  tmscm_install_procedure ("compute-index-string",  tmg_compute_index_string, 2, 0, 0);
  tmscm_install_procedure ("compute-index-tree",  tmg_compute_index_tree, 2, 0, 0);
  tmscm_install_procedure ("compute-index-url",  tmg_compute_index_url, 1, 0, 0);
  tmscm_install_procedure ("url->url",  tmg_url_2url, 1, 0, 0);
  tmscm_install_procedure ("root->url",  tmg_root_2url, 1, 0, 0);
  tmscm_install_procedure ("string->url",  tmg_string_2url, 1, 0, 0);
  tmscm_install_procedure ("url->string",  tmg_url_2string, 1, 0, 0);
  tmscm_install_procedure ("url->stree",  tmg_url_2stree, 1, 0, 0);
  tmscm_install_procedure ("system->url",  tmg_system_2url, 1, 0, 0);
  tmscm_install_procedure ("url->system",  tmg_url_2system, 1, 0, 0);
  tmscm_install_procedure ("unix->url",  tmg_unix_2url, 1, 0, 0);
  tmscm_install_procedure ("url->unix",  tmg_url_2unix, 1, 0, 0);
  tmscm_install_procedure ("url-unix",  tmg_url_unix, 2, 0, 0);
  tmscm_install_procedure ("url-none",  tmg_url_none, 0, 0, 0);
  tmscm_install_procedure ("url-any",  tmg_url_any, 0, 0, 0);
  tmscm_install_procedure ("url-wildcard",  tmg_url_wildcard, 1, 0, 0);
  tmscm_install_procedure ("url-pwd",  tmg_url_pwd, 0, 0, 0);
  tmscm_install_procedure ("url-parent",  tmg_url_parent, 0, 0, 0);
  tmscm_install_procedure ("url-ancestor",  tmg_url_ancestor, 0, 0, 0);
  tmscm_install_procedure ("url-append",  tmg_url_append, 2, 0, 0);
  tmscm_install_procedure ("url-or",  tmg_url_or, 2, 0, 0);
  tmscm_install_procedure ("url-none?",  tmg_url_noneP, 1, 0, 0);
  tmscm_install_procedure ("url-rooted?",  tmg_url_rootedP, 1, 0, 0);
  tmscm_install_procedure ("url-rooted-protocol?",  tmg_url_rooted_protocolP, 2, 0, 0);
  tmscm_install_procedure ("url-rooted-web?",  tmg_url_rooted_webP, 1, 0, 0);
  tmscm_install_procedure ("url-rooted-tmfs?",  tmg_url_rooted_tmfsP, 1, 0, 0);
  tmscm_install_procedure ("url-rooted-tmfs-protocol?",  tmg_url_rooted_tmfs_protocolP, 2, 0, 0);
  tmscm_install_procedure ("url-root",  tmg_url_root, 1, 0, 0);
  tmscm_install_procedure ("url-unroot",  tmg_url_unroot, 1, 0, 0);
  tmscm_install_procedure ("url-atomic?",  tmg_url_atomicP, 1, 0, 0);
  tmscm_install_procedure ("url-concat?",  tmg_url_concatP, 1, 0, 0);
  tmscm_install_procedure ("url-or?",  tmg_url_orP, 1, 0, 0);
  tmscm_install_procedure ("url-ref",  tmg_url_ref, 2, 0, 0);
  tmscm_install_procedure ("url-head",  tmg_url_head, 1, 0, 0);
  tmscm_install_procedure ("url-tail",  tmg_url_tail, 1, 0, 0);
  tmscm_install_procedure ("url-format",  tmg_url_format, 1, 0, 0);
  tmscm_install_procedure ("url-suffix",  tmg_url_suffix, 1, 0, 0);
  tmscm_install_procedure ("url-basename",  tmg_url_basename, 1, 0, 0);
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
  tmscm_install_procedure ("url-resolve-pattern",  tmg_url_resolve_pattern, 1, 0, 0);
  tmscm_install_procedure ("url-exists?",  tmg_url_existsP, 1, 0, 0);
  tmscm_install_procedure ("url-exists-in-path?",  tmg_url_exists_in_pathP, 1, 0, 0);
  tmscm_install_procedure ("url-exists-in-tex?",  tmg_url_exists_in_texP, 1, 0, 0);
  tmscm_install_procedure ("url-concretize*",  tmg_url_concretize_dot, 1, 0, 0);
  tmscm_install_procedure ("url-concretize",  tmg_url_concretize, 1, 0, 0);
  tmscm_install_procedure ("url-sys-concretize",  tmg_url_sys_concretize, 1, 0, 0);
  tmscm_install_procedure ("url-materialize",  tmg_url_materialize, 2, 0, 0);
  tmscm_install_procedure ("url-test?",  tmg_url_testP, 2, 0, 0);
  tmscm_install_procedure ("url-regular?",  tmg_url_regularP, 1, 0, 0);
  tmscm_install_procedure ("url-directory?",  tmg_url_directoryP, 1, 0, 0);
  tmscm_install_procedure ("url-link?",  tmg_url_linkP, 1, 0, 0);
  tmscm_install_procedure ("url-newer?",  tmg_url_newerP, 2, 0, 0);
  tmscm_install_procedure ("url-size",  tmg_url_size, 1, 0, 0);
  tmscm_install_procedure ("url-last-modified",  tmg_url_last_modified, 1, 0, 0);
  tmscm_install_procedure ("url-temp",  tmg_url_temp, 0, 0, 0);
  tmscm_install_procedure ("url-temp-dir",  tmg_url_temp_dir, 0, 0, 0);
  tmscm_install_procedure ("url-scratch",  tmg_url_scratch, 3, 0, 0);
  tmscm_install_procedure ("url-scratch?",  tmg_url_scratchP, 1, 0, 0);
  tmscm_install_procedure ("url-cache-invalidate",  tmg_url_cache_invalidate, 1, 0, 0);
  tmscm_install_procedure ("string-save",  tmg_string_save, 2, 0, 0);
  tmscm_install_procedure ("string-load",  tmg_string_load, 1, 0, 0);
  tmscm_install_procedure ("string-append-to-file",  tmg_string_append_to_file, 2, 0, 0);
  tmscm_install_procedure ("system-move",  tmg_system_move, 2, 0, 0);
  tmscm_install_procedure ("system-copy",  tmg_system_copy, 2, 0, 0);
  tmscm_install_procedure ("system-remove",  tmg_system_remove, 1, 0, 0);
  tmscm_install_procedure ("system-mkdir",  tmg_system_mkdir, 1, 0, 0);
  tmscm_install_procedure ("system-rmdir",  tmg_system_rmdir, 1, 0, 0);
  tmscm_install_procedure ("system-setenv",  tmg_system_setenv, 2, 0, 0);
  tmscm_install_procedure ("system-search-score",  tmg_system_search_score, 2, 0, 0);
  tmscm_install_procedure ("system-1",  tmg_system_1, 2, 0, 0);
  tmscm_install_procedure ("system-2",  tmg_system_2, 3, 0, 0);
  tmscm_install_procedure ("system-url->string",  tmg_system_url_2string, 1, 0, 0);
  tmscm_install_procedure ("url-grep",  tmg_url_grep, 2, 0, 0);
  tmscm_install_procedure ("url-search-upwards",  tmg_url_search_upwards, 3, 0, 0);
  tmscm_install_procedure ("picture-cache-reset",  tmg_picture_cache_reset, 0, 0, 0);
  tmscm_install_procedure ("set-file-focus",  tmg_set_file_focus, 1, 0, 0);
  tmscm_install_procedure ("get-file-focus",  tmg_get_file_focus, 0, 0, 0);
  tmscm_install_procedure ("persistent-set",  tmg_persistent_set, 3, 0, 0);
  tmscm_install_procedure ("persistent-remove",  tmg_persistent_remove, 2, 0, 0);
  tmscm_install_procedure ("persistent-has?",  tmg_persistent_hasP, 2, 0, 0);
  tmscm_install_procedure ("persistent-get",  tmg_persistent_get, 2, 0, 0);
  tmscm_install_procedure ("persistent-file-name",  tmg_persistent_file_name, 2, 0, 0);
  tmscm_install_procedure ("tmdb-keep-history",  tmg_tmdb_keep_history, 2, 0, 0);
  tmscm_install_procedure ("tmdb-set-field",  tmg_tmdb_set_field, 5, 0, 0);
  tmscm_install_procedure ("tmdb-get-field",  tmg_tmdb_get_field, 4, 0, 0);
  tmscm_install_procedure ("tmdb-remove-field",  tmg_tmdb_remove_field, 4, 0, 0);
  tmscm_install_procedure ("tmdb-get-attributes",  tmg_tmdb_get_attributes, 3, 0, 0);
  tmscm_install_procedure ("tmdb-set-entry",  tmg_tmdb_set_entry, 4, 0, 0);
  tmscm_install_procedure ("tmdb-get-entry",  tmg_tmdb_get_entry, 3, 0, 0);
  tmscm_install_procedure ("tmdb-remove-entry",  tmg_tmdb_remove_entry, 3, 0, 0);
  tmscm_install_procedure ("tmdb-query",  tmg_tmdb_query, 4, 0, 0);
  tmscm_install_procedure ("tmdb-inspect-history",  tmg_tmdb_inspect_history, 2, 0, 0);
  tmscm_install_procedure ("tmdb-get-completions",  tmg_tmdb_get_completions, 2, 0, 0);
  tmscm_install_procedure ("tmdb-get-name-completions",  tmg_tmdb_get_name_completions, 2, 0, 0);
  tmscm_install_procedure ("supports-sql?",  tmg_supports_sqlP, 0, 0, 0);
  tmscm_install_procedure ("sql-exec",  tmg_sql_exec, 2, 0, 0);
  tmscm_install_procedure ("sql-quote",  tmg_sql_quote, 1, 0, 0);
  tmscm_install_procedure ("server-start",  tmg_server_start, 0, 0, 0);
  tmscm_install_procedure ("server-stop",  tmg_server_stop, 0, 0, 0);
  tmscm_install_procedure ("server-read",  tmg_server_read, 1, 0, 0);
  tmscm_install_procedure ("server-write",  tmg_server_write, 2, 0, 0);
  tmscm_install_procedure ("server-started?",  tmg_server_startedP, 0, 0, 0);
  tmscm_install_procedure ("client-start",  tmg_client_start, 1, 0, 0);
  tmscm_install_procedure ("client-stop",  tmg_client_stop, 1, 0, 0);
  tmscm_install_procedure ("client-read",  tmg_client_read, 1, 0, 0);
  tmscm_install_procedure ("client-write",  tmg_client_write, 2, 0, 0);
  tmscm_install_procedure ("enter-secure-mode",  tmg_enter_secure_mode, 1, 0, 0);
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
  tmscm_install_procedure ("widget-filtered-choice",  tmg_widget_filtered_choice, 4, 0, 0);
  tmscm_install_procedure ("widget-tree-view",  tmg_widget_tree_view, 3, 0, 0);
  tmscm_install_procedure ("widget-xpm",  tmg_widget_xpm, 1, 0, 0);
  tmscm_install_procedure ("widget-box",  tmg_widget_box, 5, 0, 0);
  tmscm_install_procedure ("widget-glue",  tmg_widget_glue, 4, 0, 0);
  tmscm_install_procedure ("widget-color",  tmg_widget_color, 5, 0, 0);
  tmscm_install_procedure ("widget-hlist",  tmg_widget_hlist, 1, 0, 0);
  tmscm_install_procedure ("widget-vlist",  tmg_widget_vlist, 1, 0, 0);
  tmscm_install_procedure ("widget-division",  tmg_widget_division, 2, 0, 0);
  tmscm_install_procedure ("widget-aligned",  tmg_widget_aligned, 2, 0, 0);
  tmscm_install_procedure ("widget-tabs",  tmg_widget_tabs, 2, 0, 0);
  tmscm_install_procedure ("widget-icon-tabs",  tmg_widget_icon_tabs, 3, 0, 0);
  tmscm_install_procedure ("widget-scrollable",  tmg_widget_scrollable, 2, 0, 0);
  tmscm_install_procedure ("widget-resize",  tmg_widget_resize, 10, 0, 0);
  tmscm_install_procedure ("widget-hsplit",  tmg_widget_hsplit, 2, 0, 0);
  tmscm_install_procedure ("widget-vsplit",  tmg_widget_vsplit, 2, 0, 0);
  tmscm_install_procedure ("widget-texmacs-output",  tmg_widget_texmacs_output, 2, 0, 0);
  tmscm_install_procedure ("widget-texmacs-input",  tmg_widget_texmacs_input, 3, 0, 0);
  tmscm_install_procedure ("widget-ink",  tmg_widget_ink, 1, 0, 0);
  tmscm_install_procedure ("widget-refresh",  tmg_widget_refresh, 2, 0, 0);
  tmscm_install_procedure ("widget-refreshable",  tmg_widget_refreshable, 2, 0, 0);
  tmscm_install_procedure ("object->promise-widget",  tmg_object_2promise_widget, 1, 0, 0);
  tmscm_install_procedure ("tree-bounding-rectangle",  tmg_tree_bounding_rectangle, 1, 0, 0);
  tmscm_install_procedure ("widget-size",  tmg_widget_size, 1, 0, 0);
  tmscm_install_procedure ("texmacs-widget-size",  tmg_texmacs_widget_size, 1, 0, 0);
  tmscm_install_procedure ("show-balloon",  tmg_show_balloon, 3, 0, 0);
  tmscm_install_procedure ("get-style-menu",  tmg_get_style_menu, 0, 0, 0);
  tmscm_install_procedure ("hidden-package?",  tmg_hidden_packageP, 1, 0, 0);
  tmscm_install_procedure ("get-add-package-menu",  tmg_get_add_package_menu, 0, 0, 0);
  tmscm_install_procedure ("get-remove-package-menu",  tmg_get_remove_package_menu, 0, 0, 0);
  tmscm_install_procedure ("get-toggle-package-menu",  tmg_get_toggle_package_menu, 0, 0, 0);
  tmscm_install_procedure ("refresh-now",  tmg_refresh_now, 1, 0, 0);
  tmscm_install_procedure ("get-screen-size",  tmg_get_screen_size, 0, 0, 0);
  tmscm_install_procedure ("buffer-list",  tmg_buffer_list, 0, 0, 0);
  tmscm_install_procedure ("current-buffer-url",  tmg_current_buffer_url, 0, 0, 0);
  tmscm_install_procedure ("path-to-buffer",  tmg_path_to_buffer, 1, 0, 0);
  tmscm_install_procedure ("buffer-new",  tmg_buffer_new, 0, 0, 0);
  tmscm_install_procedure ("buffer-rename",  tmg_buffer_rename, 2, 0, 0);
  tmscm_install_procedure ("buffer-set",  tmg_buffer_set, 2, 0, 0);
  tmscm_install_procedure ("buffer-get",  tmg_buffer_get, 1, 0, 0);
  tmscm_install_procedure ("buffer-set-body",  tmg_buffer_set_body, 2, 0, 0);
  tmscm_install_procedure ("buffer-get-body",  tmg_buffer_get_body, 1, 0, 0);
  tmscm_install_procedure ("buffer-set-master",  tmg_buffer_set_master, 2, 0, 0);
  tmscm_install_procedure ("buffer-get-master",  tmg_buffer_get_master, 1, 0, 0);
  tmscm_install_procedure ("buffer-set-title",  tmg_buffer_set_title, 2, 0, 0);
  tmscm_install_procedure ("buffer-get-title",  tmg_buffer_get_title, 1, 0, 0);
  tmscm_install_procedure ("buffer-last-save",  tmg_buffer_last_save, 1, 0, 0);
  tmscm_install_procedure ("buffer-last-visited",  tmg_buffer_last_visited, 1, 0, 0);
  tmscm_install_procedure ("buffer-modified?",  tmg_buffer_modifiedP, 1, 0, 0);
  tmscm_install_procedure ("buffer-modified-since-autosave?",  tmg_buffer_modified_since_autosaveP, 1, 0, 0);
  tmscm_install_procedure ("buffer-pretend-modified",  tmg_buffer_pretend_modified, 1, 0, 0);
  tmscm_install_procedure ("buffer-pretend-saved",  tmg_buffer_pretend_saved, 1, 0, 0);
  tmscm_install_procedure ("buffer-pretend-autosaved",  tmg_buffer_pretend_autosaved, 1, 0, 0);
  tmscm_install_procedure ("buffer-attach-notifier",  tmg_buffer_attach_notifier, 1, 0, 0);
  tmscm_install_procedure ("buffer-has-name?",  tmg_buffer_has_nameP, 1, 0, 0);
  tmscm_install_procedure ("buffer-aux?",  tmg_buffer_auxP, 1, 0, 0);
  tmscm_install_procedure ("buffer-embedded?",  tmg_buffer_embeddedP, 1, 0, 0);
  tmscm_install_procedure ("buffer-import",  tmg_buffer_import, 3, 0, 0);
  tmscm_install_procedure ("buffer-load",  tmg_buffer_load, 1, 0, 0);
  tmscm_install_procedure ("buffer-export",  tmg_buffer_export, 3, 0, 0);
  tmscm_install_procedure ("buffer-save",  tmg_buffer_save, 1, 0, 0);
  tmscm_install_procedure ("tree-import-loaded",  tmg_tree_import_loaded, 3, 0, 0);
  tmscm_install_procedure ("tree-import",  tmg_tree_import, 2, 0, 0);
  tmscm_install_procedure ("tree-inclusion",  tmg_tree_inclusion, 1, 0, 0);
  tmscm_install_procedure ("tree-export",  tmg_tree_export, 3, 0, 0);
  tmscm_install_procedure ("tree-load-style",  tmg_tree_load_style, 1, 0, 0);
  tmscm_install_procedure ("buffer-focus",  tmg_buffer_focus, 1, 0, 0);
  tmscm_install_procedure ("buffer-focus*",  tmg_buffer_focus_dot, 1, 0, 0);
  tmscm_install_procedure ("view-list",  tmg_view_list, 0, 0, 0);
  tmscm_install_procedure ("buffer->views",  tmg_buffer_2views, 1, 0, 0);
  tmscm_install_procedure ("current-view-url",  tmg_current_view_url, 0, 0, 0);
  tmscm_install_procedure ("window->view",  tmg_window_2view, 1, 0, 0);
  tmscm_install_procedure ("view->buffer",  tmg_view_2buffer, 1, 0, 0);
  tmscm_install_procedure ("view->window-url",  tmg_view_2window_url, 1, 0, 0);
  tmscm_install_procedure ("view-new",  tmg_view_new, 1, 0, 0);
  tmscm_install_procedure ("view-passive",  tmg_view_passive, 1, 0, 0);
  tmscm_install_procedure ("view-recent",  tmg_view_recent, 1, 0, 0);
  tmscm_install_procedure ("view-delete",  tmg_view_delete, 1, 0, 0);
  tmscm_install_procedure ("window-set-view",  tmg_window_set_view, 3, 0, 0);
  tmscm_install_procedure ("switch-to-buffer",  tmg_switch_to_buffer, 1, 0, 0);
  tmscm_install_procedure ("set-drd",  tmg_set_drd, 1, 0, 0);
  tmscm_install_procedure ("window-list",  tmg_window_list, 0, 0, 0);
  tmscm_install_procedure ("windows-number",  tmg_windows_number, 0, 0, 0);
  tmscm_install_procedure ("current-window",  tmg_current_window, 0, 0, 0);
  tmscm_install_procedure ("buffer->windows",  tmg_buffer_2windows, 1, 0, 0);
  tmscm_install_procedure ("window-to-buffer",  tmg_window_to_buffer, 1, 0, 0);
  tmscm_install_procedure ("window-set-buffer",  tmg_window_set_buffer, 2, 0, 0);
  tmscm_install_procedure ("window-focus",  tmg_window_focus, 1, 0, 0);
  tmscm_install_procedure ("switch-to-window",  tmg_switch_to_window, 1, 0, 0);
  tmscm_install_procedure ("new-buffer",  tmg_new_buffer, 0, 0, 0);
  tmscm_install_procedure ("open-buffer-in-window",  tmg_open_buffer_in_window, 3, 0, 0);
  tmscm_install_procedure ("open-window",  tmg_open_window, 0, 0, 0);
  tmscm_install_procedure ("open-window-geometry",  tmg_open_window_geometry, 1, 0, 0);
  tmscm_install_procedure ("clone-window",  tmg_clone_window, 0, 0, 0);
  tmscm_install_procedure ("cpp-buffer-close",  tmg_cpp_buffer_close, 1, 0, 0);
  tmscm_install_procedure ("kill-window",  tmg_kill_window, 1, 0, 0);
  tmscm_install_procedure ("kill-current-window-and-buffer",  tmg_kill_current_window_and_buffer, 0, 0, 0);
  tmscm_install_procedure ("project-attach",  tmg_project_attach, 1, 0, 0);
  tmscm_install_procedure ("project-detach",  tmg_project_detach, 0, 0, 0);
  tmscm_install_procedure ("project-attached?",  tmg_project_attachedP, 0, 0, 0);
  tmscm_install_procedure ("project-get",  tmg_project_get, 0, 0, 0);
  tmscm_install_procedure ("alt-window-handle",  tmg_alt_window_handle, 0, 0, 0);
  tmscm_install_procedure ("alt-window-create-quit",  tmg_alt_window_create_quit, 4, 0, 0);
  tmscm_install_procedure ("alt-window-create-plain",  tmg_alt_window_create_plain, 3, 0, 0);
  tmscm_install_procedure ("alt-window-create-popup",  tmg_alt_window_create_popup, 3, 0, 0);
  tmscm_install_procedure ("alt-window-create-tooltip",  tmg_alt_window_create_tooltip, 3, 0, 0);
  tmscm_install_procedure ("alt-window-delete",  tmg_alt_window_delete, 1, 0, 0);
  tmscm_install_procedure ("alt-window-show",  tmg_alt_window_show, 1, 0, 0);
  tmscm_install_procedure ("alt-window-hide",  tmg_alt_window_hide, 1, 0, 0);
  tmscm_install_procedure ("alt-window-get-size",  tmg_alt_window_get_size, 1, 0, 0);
  tmscm_install_procedure ("alt-window-set-size",  tmg_alt_window_set_size, 3, 0, 0);
  tmscm_install_procedure ("alt-window-get-position",  tmg_alt_window_get_position, 1, 0, 0);
  tmscm_install_procedure ("alt-window-set-position",  tmg_alt_window_set_position, 3, 0, 0);
  tmscm_install_procedure ("alt-window-search",  tmg_alt_window_search, 1, 0, 0);
  tmscm_install_procedure ("supports-bibtex?",  tmg_supports_bibtexP, 0, 0, 0);
  tmscm_install_procedure ("bibtex-run",  tmg_bibtex_run, 4, 0, 0);
  tmscm_install_procedure ("bib-add-period",  tmg_bib_add_period, 1, 0, 0);
  tmscm_install_procedure ("bib-locase-first",  tmg_bib_locase_first, 1, 0, 0);
  tmscm_install_procedure ("bib-upcase-first",  tmg_bib_upcase_first, 1, 0, 0);
  tmscm_install_procedure ("bib-locase",  tmg_bib_locase, 1, 0, 0);
  tmscm_install_procedure ("bib-upcase",  tmg_bib_upcase, 1, 0, 0);
  tmscm_install_procedure ("bib-default-preserve-case",  tmg_bib_default_preserve_case, 1, 0, 0);
  tmscm_install_procedure ("bib-default-upcase-first",  tmg_bib_default_upcase_first, 1, 0, 0);
  tmscm_install_procedure ("bib-purify",  tmg_bib_purify, 1, 0, 0);
  tmscm_install_procedure ("bib-text-length",  tmg_bib_text_length, 1, 0, 0);
  tmscm_install_procedure ("bib-prefix",  tmg_bib_prefix, 2, 0, 0);
  tmscm_install_procedure ("bib-empty?",  tmg_bib_emptyP, 2, 0, 0);
  tmscm_install_procedure ("bib-field",  tmg_bib_field, 2, 0, 0);
  tmscm_install_procedure ("bib-abbreviate",  tmg_bib_abbreviate, 3, 0, 0);
  tmscm_install_procedure ("extract-attachments",  tmg_extract_attachments, 1, 0, 0);
  tmscm_install_procedure ("pdf-make-attachments",  tmg_pdf_make_attachments, 3, 0, 0);
  tmscm_install_procedure ("pdf-get-linked-file-paths",  tmg_pdf_get_linked_file_paths, 2, 0, 0);
  tmscm_install_procedure ("pdf-replace-linked-path",  tmg_pdf_replace_linked_path, 2, 0, 0);
  tmscm_install_procedure ("pdf-get-attached-main-tm",  tmg_pdf_get_attached_main_tm, 1, 0, 0);
  tmscm_install_procedure ("array-url-append",  tmg_array_url_append, 2, 0, 0);
}
