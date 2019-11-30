
/******************************************************************************
*
* This file has been generated automatically using build-glue.scm
* from build-glue-server.scm. Please do not edit its contents.
* Copyright (C) 2000 Joris van der Hoeven
*
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*
******************************************************************************/

tmscm
tmg_insert_kbd_wildcard (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "insert-kbd-wildcard");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "insert-kbd-wildcard");
  TMSCM_ASSERT_BOOL (arg3, TMSCM_ARG3, "insert-kbd-wildcard");
  TMSCM_ASSERT_BOOL (arg4, TMSCM_ARG4, "insert-kbd-wildcard");
  TMSCM_ASSERT_BOOL (arg5, TMSCM_ARG5, "insert-kbd-wildcard");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);
  bool in3= tmscm_to_bool (arg3);
  bool in4= tmscm_to_bool (arg4);
  bool in5= tmscm_to_bool (arg5);

  // TMSCM_DEFER_INTS;
  get_server()->insert_kbd_wildcard (in1, in2, in3, in4, in5);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_variant_keys (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "set-variant-keys");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "set-variant-keys");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  get_server()->set_variant_keys (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_kbd_pre_rewrite (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "kbd-pre-rewrite");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= get_server()->kbd_pre_rewrite (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_kbd_post_rewrite (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "kbd-post-rewrite");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "kbd-post-rewrite");

  string in1= tmscm_to_string (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  string out= get_server()->kbd_post_rewrite (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_kbd_system_rewrite (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "kbd-system-rewrite");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_server()->kbd_system_rewrite (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_set_font_rules (tmscm arg1) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "set-font-rules");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);

  // TMSCM_DEFER_INTS;
  get_server()->set_font_rules (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_window_get_serial () {
  // TMSCM_DEFER_INTS;
  int out= get_server()->get_window_serial ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_window_set_property (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "window-set-property");
  TMSCM_ASSERT_SCHEME_TREE (arg2, TMSCM_ARG2, "window-set-property");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);
  scheme_tree in2= tmscm_to_scheme_tree (arg2);

  // TMSCM_DEFER_INTS;
  get_server()->set_window_property (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_window_get_property (tmscm arg1) {
  TMSCM_ASSERT_SCHEME_TREE (arg1, TMSCM_ARG1, "window-get-property");

  scheme_tree in1= tmscm_to_scheme_tree (arg1);

  // TMSCM_DEFER_INTS;
  scheme_tree out= get_server()->get_window_property (in1);
  // TMSCM_ALLOW_INTS;

  return scheme_tree_to_tmscm (out);
}

tmscm
tmg_show_header (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "show-header");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  get_server()->show_header (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_show_icon_bar (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "show-icon-bar");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "show-icon-bar");

  int in1= tmscm_to_int (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  get_server()->show_icon_bar (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_show_side_tools (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "show-side-tools");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "show-side-tools");

  int in1= tmscm_to_int (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  get_server()->show_side_tools (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_show_bottom_tools (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "show-bottom-tools");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "show-bottom-tools");

  int in1= tmscm_to_int (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  get_server()->show_bottom_tools (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_show_footer (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "show-footer");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  get_server()->show_footer (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_visible_headerP () {
  // TMSCM_DEFER_INTS;
  bool out= get_server()->visible_header ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_visible_icon_barP (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "visible-icon-bar?");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  bool out= get_server()->visible_icon_bar (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_visible_side_toolsP (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "visible-side-tools?");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  bool out= get_server()->visible_side_tools (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_visible_bottom_toolsP (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "visible-bottom-tools?");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  bool out= get_server()->visible_bottom_tools (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_visible_footerP () {
  // TMSCM_DEFER_INTS;
  bool out= get_server()->visible_footer ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_full_screen_mode (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "full-screen-mode");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "full-screen-mode");

  bool in1= tmscm_to_bool (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  get_server()->full_screen_mode (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_full_screenP () {
  // TMSCM_DEFER_INTS;
  bool out= get_server()->in_full_screen_mode ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_full_screen_editP () {
  // TMSCM_DEFER_INTS;
  bool out= get_server()->in_full_screen_edit_mode ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_set_window_zoom_factor (tmscm arg1) {
  TMSCM_ASSERT_DOUBLE (arg1, TMSCM_ARG1, "set-window-zoom-factor");

  double in1= tmscm_to_double (arg1);

  // TMSCM_DEFER_INTS;
  get_server()->set_window_zoom_factor (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_window_zoom_factor () {
  // TMSCM_DEFER_INTS;
  double out= get_server()->get_window_zoom_factor ();
  // TMSCM_ALLOW_INTS;

  return double_to_tmscm (out);
}

tmscm
tmg_shell (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "shell");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_server()->shell (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_dialogue_end () {
  // TMSCM_DEFER_INTS;
  get_server()->dialogue_end ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_choose_file (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5) {
  TMSCM_ASSERT_OBJECT (arg1, TMSCM_ARG1, "cpp-choose-file");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "cpp-choose-file");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "cpp-choose-file");
  TMSCM_ASSERT_STRING (arg4, TMSCM_ARG4, "cpp-choose-file");
  TMSCM_ASSERT_URL (arg5, TMSCM_ARG5, "cpp-choose-file");

  object in1= tmscm_to_object (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);
  string in4= tmscm_to_string (arg4);
  url in5= tmscm_to_url (arg5);

  // TMSCM_DEFER_INTS;
  get_server()->choose_file (in1, in2, in3, in4, in5);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tm_interactive (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_OBJECT (arg1, TMSCM_ARG1, "tm-interactive");
  TMSCM_ASSERT_SCHEME_TREE (arg2, TMSCM_ARG2, "tm-interactive");

  object in1= tmscm_to_object (arg1);
  scheme_tree in2= tmscm_to_scheme_tree (arg2);

  // TMSCM_DEFER_INTS;
  get_server()->interactive (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_cpp_style_clear_cache () {
  // TMSCM_DEFER_INTS;
  get_server()->style_clear_cache ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_script_status (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "set-script-status");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  get_server()->set_script_status (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_printing_command (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "set-printing-command");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_server()->set_printing_command (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_printer_paper_type (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "set-printer-paper-type");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_server()->set_printer_page_type (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_printer_paper_type () {
  // TMSCM_DEFER_INTS;
  string out= get_server()->get_printer_page_type ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_set_printer_dpi (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "set-printer-dpi");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_server()->set_printer_dpi (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_default_zoom_factor (tmscm arg1) {
  TMSCM_ASSERT_DOUBLE (arg1, TMSCM_ARG1, "set-default-zoom-factor");

  double in1= tmscm_to_double (arg1);

  // TMSCM_DEFER_INTS;
  get_server()->set_default_zoom_factor (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_default_zoom_factor () {
  // TMSCM_DEFER_INTS;
  double out= get_server()->get_default_zoom_factor ();
  // TMSCM_ALLOW_INTS;

  return double_to_tmscm (out);
}

tmscm
tmg_inclusions_gc () {
  // TMSCM_DEFER_INTS;
  get_server()->inclusions_gc ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_update_all_path (tmscm arg1) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "update-all-path");

  path in1= tmscm_to_path (arg1);

  // TMSCM_DEFER_INTS;
  get_server()->typeset_update (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_update_all_buffers () {
  // TMSCM_DEFER_INTS;
  get_server()->typeset_update_all ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_message (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "set-message");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "set-message");

  content in1= tmscm_to_content (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  get_server()->set_message (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_message_temp (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "set-message-temp");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "set-message-temp");
  TMSCM_ASSERT_BOOL (arg3, TMSCM_ARG3, "set-message-temp");

  content in1= tmscm_to_content (arg1);
  content in2= tmscm_to_content (arg2);
  bool in3= tmscm_to_bool (arg3);

  // TMSCM_DEFER_INTS;
  get_server()->set_message (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_recall_message () {
  // TMSCM_DEFER_INTS;
  get_server()->recall_message ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_yesP (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "yes?");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  bool out= get_server()->is_yes (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_quit_TeXmacs () {
  // TMSCM_DEFER_INTS;
  get_server()->quit ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

void
initialize_glue_server () {
  tmscm_install_procedure ("insert-kbd-wildcard",  tmg_insert_kbd_wildcard, 5, 0, 0);
  tmscm_install_procedure ("set-variant-keys",  tmg_set_variant_keys, 2, 0, 0);
  tmscm_install_procedure ("kbd-pre-rewrite",  tmg_kbd_pre_rewrite, 1, 0, 0);
  tmscm_install_procedure ("kbd-post-rewrite",  tmg_kbd_post_rewrite, 2, 0, 0);
  tmscm_install_procedure ("kbd-system-rewrite",  tmg_kbd_system_rewrite, 1, 0, 0);
  tmscm_install_procedure ("set-font-rules",  tmg_set_font_rules, 1, 0, 0);
  tmscm_install_procedure ("window-get-serial",  tmg_window_get_serial, 0, 0, 0);
  tmscm_install_procedure ("window-set-property",  tmg_window_set_property, 2, 0, 0);
  tmscm_install_procedure ("window-get-property",  tmg_window_get_property, 1, 0, 0);
  tmscm_install_procedure ("show-header",  tmg_show_header, 1, 0, 0);
  tmscm_install_procedure ("show-icon-bar",  tmg_show_icon_bar, 2, 0, 0);
  tmscm_install_procedure ("show-side-tools",  tmg_show_side_tools, 2, 0, 0);
  tmscm_install_procedure ("show-bottom-tools",  tmg_show_bottom_tools, 2, 0, 0);
  tmscm_install_procedure ("show-footer",  tmg_show_footer, 1, 0, 0);
  tmscm_install_procedure ("visible-header?",  tmg_visible_headerP, 0, 0, 0);
  tmscm_install_procedure ("visible-icon-bar?",  tmg_visible_icon_barP, 1, 0, 0);
  tmscm_install_procedure ("visible-side-tools?",  tmg_visible_side_toolsP, 1, 0, 0);
  tmscm_install_procedure ("visible-bottom-tools?",  tmg_visible_bottom_toolsP, 1, 0, 0);
  tmscm_install_procedure ("visible-footer?",  tmg_visible_footerP, 0, 0, 0);
  tmscm_install_procedure ("full-screen-mode",  tmg_full_screen_mode, 2, 0, 0);
  tmscm_install_procedure ("full-screen?",  tmg_full_screenP, 0, 0, 0);
  tmscm_install_procedure ("full-screen-edit?",  tmg_full_screen_editP, 0, 0, 0);
  tmscm_install_procedure ("set-window-zoom-factor",  tmg_set_window_zoom_factor, 1, 0, 0);
  tmscm_install_procedure ("get-window-zoom-factor",  tmg_get_window_zoom_factor, 0, 0, 0);
  tmscm_install_procedure ("shell",  tmg_shell, 1, 0, 0);
  tmscm_install_procedure ("dialogue-end",  tmg_dialogue_end, 0, 0, 0);
  tmscm_install_procedure ("cpp-choose-file",  tmg_cpp_choose_file, 5, 0, 0);
  tmscm_install_procedure ("tm-interactive",  tmg_tm_interactive, 2, 0, 0);
  tmscm_install_procedure ("cpp-style-clear-cache",  tmg_cpp_style_clear_cache, 0, 0, 0);
  tmscm_install_procedure ("set-script-status",  tmg_set_script_status, 1, 0, 0);
  tmscm_install_procedure ("set-printing-command",  tmg_set_printing_command, 1, 0, 0);
  tmscm_install_procedure ("set-printer-paper-type",  tmg_set_printer_paper_type, 1, 0, 0);
  tmscm_install_procedure ("get-printer-paper-type",  tmg_get_printer_paper_type, 0, 0, 0);
  tmscm_install_procedure ("set-printer-dpi",  tmg_set_printer_dpi, 1, 0, 0);
  tmscm_install_procedure ("set-default-zoom-factor",  tmg_set_default_zoom_factor, 1, 0, 0);
  tmscm_install_procedure ("get-default-zoom-factor",  tmg_get_default_zoom_factor, 0, 0, 0);
  tmscm_install_procedure ("inclusions-gc",  tmg_inclusions_gc, 0, 0, 0);
  tmscm_install_procedure ("update-all-path",  tmg_update_all_path, 1, 0, 0);
  tmscm_install_procedure ("update-all-buffers",  tmg_update_all_buffers, 0, 0, 0);
  tmscm_install_procedure ("set-message",  tmg_set_message, 2, 0, 0);
  tmscm_install_procedure ("set-message-temp",  tmg_set_message_temp, 3, 0, 0);
  tmscm_install_procedure ("recall-message",  tmg_recall_message, 0, 0, 0);
  tmscm_install_procedure ("yes?",  tmg_yesP, 1, 0, 0);
  tmscm_install_procedure ("quit-TeXmacs",  tmg_quit_TeXmacs, 0, 0, 0);
}
