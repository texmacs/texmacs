
/******************************************************************************
*
* This file has been generated automatically using build-glue.scm
* from build-glue-server.scm. Please do not edit its contents.
* Copyright (C) 2000 Joris van der Hoeven
*
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*
******************************************************************************/

SCM
tmg_set_input_language (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "set-input-language");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->set_input_language (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_get_input_language () {
  // SCM_DEFER_INTS;
  string out= get_server()->get_input_language ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_set_output_language (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "set-output-language");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->set_output_language (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_get_output_language () {
  // SCM_DEFER_INTS;
  string out= get_server()->get_output_language ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_insert_kbd_wildcard (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "insert-kbd-wildcard");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "insert-kbd-wildcard");
  SCM_ASSERT_BOOL (arg3, SCM_ARG3, "insert-kbd-wildcard");
  SCM_ASSERT_BOOL (arg4, SCM_ARG4, "insert-kbd-wildcard");
  SCM_ASSERT_BOOL (arg5, SCM_ARG5, "insert-kbd-wildcard");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  bool in3= scm_to_bool (arg3);
  bool in4= scm_to_bool (arg4);
  bool in5= scm_to_bool (arg5);

  // SCM_DEFER_INTS;
  get_server()->insert_kbd_wildcard (in1, in2, in3, in4, in5);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_set_variant_keys (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "set-variant-keys");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "set-variant-keys");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  get_server()->set_variant_keys (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_kbd_pre_rewrite (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "kbd-pre-rewrite");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= get_server()->kbd_pre_rewrite (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_kbd_post_rewrite (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "kbd-post-rewrite");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= get_server()->kbd_post_rewrite (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_set_font_rules (SCM arg1) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "set-font-rules");

  scheme_tree in1= scm_to_scheme_tree (arg1);

  // SCM_DEFER_INTS;
  get_server()->set_font_rules (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_window_set_property (SCM arg1, SCM arg2) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "window-set-property");
  SCM_ASSERT_SCHEME_TREE (arg2, SCM_ARG2, "window-set-property");

  scheme_tree in1= scm_to_scheme_tree (arg1);
  scheme_tree in2= scm_to_scheme_tree (arg2);

  // SCM_DEFER_INTS;
  get_server()->set_window_property (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_window_get_property (SCM arg1) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "window-get-property");

  scheme_tree in1= scm_to_scheme_tree (arg1);

  // SCM_DEFER_INTS;
  scheme_tree out= get_server()->get_window_property (in1);
  // SCM_ALLOW_INTS;

  return scheme_tree_to_scm (out);
}

SCM
tmg_show_header (SCM arg1) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "show-header");

  bool in1= scm_to_bool (arg1);

  // SCM_DEFER_INTS;
  get_server()->show_header (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_show_icon_bar (SCM arg1, SCM arg2) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "show-icon-bar");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "show-icon-bar");

  int in1= scm_to_int (arg1);
  bool in2= scm_to_bool (arg2);

  // SCM_DEFER_INTS;
  get_server()->show_icon_bar (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_show_footer (SCM arg1) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "show-footer");

  bool in1= scm_to_bool (arg1);

  // SCM_DEFER_INTS;
  get_server()->show_footer (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_visible_headerP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->visible_header ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_visible_icon_barP (SCM arg1) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "visible-icon-bar?");

  int in1= scm_to_int (arg1);

  // SCM_DEFER_INTS;
  bool out= get_server()->visible_icon_bar (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_visible_footerP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->visible_footer ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_full_screen_mode (SCM arg1, SCM arg2) {
  SCM_ASSERT_BOOL (arg1, SCM_ARG1, "full-screen-mode");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "full-screen-mode");

  bool in1= scm_to_bool (arg1);
  bool in2= scm_to_bool (arg2);

  // SCM_DEFER_INTS;
  get_server()->full_screen_mode (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_full_screenP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->in_full_screen_mode ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_full_screen_editP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->in_full_screen_edit_mode ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_set_shrinking_factor (SCM arg1) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "set-shrinking-factor");

  int in1= scm_to_int (arg1);

  // SCM_DEFER_INTS;
  get_server()->set_shrinking_factor (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_get_shrinking_factor () {
  // SCM_DEFER_INTS;
  int out= get_server()->get_shrinking_factor ();
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_exec_delayed (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "exec-delayed");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->exec_delayed (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_exec_delayed_cmd (SCM arg1) {
  SCM_ASSERT_COMMAND (arg1, SCM_ARG1, "exec-delayed-cmd");

  command in1= scm_to_command (arg1);

  // SCM_DEFER_INTS;
  get_server()->exec_delayed (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_shell (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "shell");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->shell (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_dialogue_end () {
  // SCM_DEFER_INTS;
  get_server()->dialogue_end ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_choose_file (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "choose-file");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "choose-file");
  SCM_ASSERT_SCHEME_TREE (arg3, SCM_ARG3, "choose-file");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  scheme_tree in3= scm_to_scheme_tree (arg3);

  // SCM_DEFER_INTS;
  get_server()->choose_file (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_has_viewP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->has_view ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_buffer_unsavedP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->buffer_unsaved ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_exists_unsaved_bufferP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->exists_unsaved_buffer ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_pretend_save_buffer () {
  // SCM_DEFER_INTS;
  get_server()->pretend_save_buffer ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_get_name_buffer () {
  // SCM_DEFER_INTS;
  url out= get_server()->get_name_buffer ();
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_set_name_buffer (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "set-name-buffer");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  get_server()->set_name_buffer (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_set_abbr_buffer (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "set-abbr-buffer");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->set_abbr_buffer (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_new_buffer () {
  // SCM_DEFER_INTS;
  get_server()->new_buffer ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_switch_to_buffer (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "switch-to-buffer");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  get_server()->switch_to_buffer (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_switch_to_active_buffer (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "switch-to-active-buffer");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  get_server()->switch_to_active_buffer (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_revert_buffer () {
  // SCM_DEFER_INTS;
  get_server()->revert_buffer ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_kill_buffer () {
  // SCM_DEFER_INTS;
  get_server()->kill_buffer ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_open_window () {
  // SCM_DEFER_INTS;
  get_server()->open_window ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_clone_window () {
  // SCM_DEFER_INTS;
  get_server()->clone_window ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_kill_window () {
  // SCM_DEFER_INTS;
  get_server()->kill_window ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_set_maximal_undo_depth (SCM arg1) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "set-maximal-undo-depth");

  int in1= scm_to_int (arg1);

  // SCM_DEFER_INTS;
  get_server()->set_max_undo_depth (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_get_maximal_undo_depth () {
  // SCM_DEFER_INTS;
  int out= get_server()->get_max_undo_depth ();
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_no_nameP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->no_name ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_help_bufferP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->help_buffer ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_set_buffer (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "set-buffer");
  SCM_ASSERT_CONTENT (arg2, SCM_ARG2, "set-buffer");

  url in1= scm_to_url (arg1);
  content in2= scm_to_content (arg2);

  // SCM_DEFER_INTS;
  get_server()->revert_buffer (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_set_aux_buffer (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "set-aux-buffer");
  SCM_ASSERT_URL (arg2, SCM_ARG2, "set-aux-buffer");
  SCM_ASSERT_CONTENT (arg3, SCM_ARG3, "set-aux-buffer");

  string in1= scm_to_string (arg1);
  url in2= scm_to_url (arg2);
  content in3= scm_to_content (arg3);

  // SCM_DEFER_INTS;
  get_server()->set_aux_buffer (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_set_help_buffer (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "set-help-buffer");
  SCM_ASSERT_CONTENT (arg2, SCM_ARG2, "set-help-buffer");

  url in1= scm_to_url (arg1);
  content in2= scm_to_content (arg2);

  // SCM_DEFER_INTS;
  get_server()->set_help_buffer (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_browse_help (SCM arg1) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "browse-help");

  int in1= scm_to_int (arg1);

  // SCM_DEFER_INTS;
  get_server()->browse_help (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_project_attach (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "project-attach");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->project_attach (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_project_detach () {
  // SCM_DEFER_INTS;
  get_server()->project_attach ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_project_attachedP () {
  // SCM_DEFER_INTS;
  bool out= get_server()->project_attached ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_texmacs_load_tree (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "texmacs-load-tree");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "texmacs-load-tree");

  url in1= scm_to_url (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  tree out= get_server()->load_tree (in1, in2);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_texmacs_load_buffer (SCM arg1, SCM arg2, SCM arg3, SCM arg4) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "texmacs-load-buffer");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "texmacs-load-buffer");
  SCM_ASSERT_INT (arg3, SCM_ARG3, "texmacs-load-buffer");
  SCM_ASSERT_BOOL (arg4, SCM_ARG4, "texmacs-load-buffer");

  url in1= scm_to_url (arg1);
  string in2= scm_to_string (arg2);
  int in3= scm_to_int (arg3);
  bool in4= scm_to_bool (arg4);

  // SCM_DEFER_INTS;
  get_server()->load_buffer (in1, in2, in3, in4);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_texmacs_save_buffer (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "texmacs-save-buffer");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "texmacs-save-buffer");

  url in1= scm_to_url (arg1);
  string in2= scm_to_string (arg2);

  // SCM_DEFER_INTS;
  get_server()->save_buffer (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_delayed_autosave () {
  // SCM_DEFER_INTS;
  get_server()->delayed_autosave ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_color (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "color");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  int out= get_server()->get_color (in1);
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_style_clear_cache () {
  // SCM_DEFER_INTS;
  get_server()->style_clear_cache ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_set_script_status (SCM arg1) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "set-script-status");

  int in1= scm_to_int (arg1);

  // SCM_DEFER_INTS;
  get_server()->set_script_status (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_set_printing_command (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "set-printing-command");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->set_printing_command (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_set_printer_paper_type (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "set-printer-paper-type");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->set_printer_page_type (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_get_printer_paper_type () {
  // SCM_DEFER_INTS;
  string out= get_server()->get_printer_page_type ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_set_printer_dpi (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "set-printer-dpi");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->set_printer_dpi (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_set_default_shrinking_factor (SCM arg1) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "set-default-shrinking-factor");

  int in1= scm_to_int (arg1);

  // SCM_DEFER_INTS;
  get_server()->set_default_shrinking_factor (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_get_default_shrinking_factor () {
  // SCM_DEFER_INTS;
  int out= get_server()->get_default_shrinking_factor ();
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_get_nr_windows () {
  // SCM_DEFER_INTS;
  int out= get_server()->get_nr_windows ();
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_postscript_gc () {
  // SCM_DEFER_INTS;
  get_server()->postscript_gc ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_inclusions_gc () {
  // SCM_DEFER_INTS;
  get_server()->inclusions_gc ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_translate (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "translate");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "translate");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "translate");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  string out= get_server()->translate (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_yesP (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "yes?");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  bool out= get_server()->is_yes (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_quit_TeXmacs () {
  // SCM_DEFER_INTS;
  get_server()->quit ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_package_evaluate (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "package-evaluate");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "package-evaluate");
  SCM_ASSERT_CONTENT (arg3, SCM_ARG3, "package-evaluate");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  content in3= scm_to_content (arg3);

  // SCM_DEFER_INTS;
  tree out= get_server()->evaluate (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

void
initialize_glue_server () {
  gh_new_procedure ("set-input-language", (FN) tmg_set_input_language, 1, 0, 0);
  gh_new_procedure ("get-input-language", (FN) tmg_get_input_language, 0, 0, 0);
  gh_new_procedure ("set-output-language", (FN) tmg_set_output_language, 1, 0, 0);
  gh_new_procedure ("get-output-language", (FN) tmg_get_output_language, 0, 0, 0);
  gh_new_procedure ("insert-kbd-wildcard", (FN) tmg_insert_kbd_wildcard, 5, 0, 0);
  gh_new_procedure ("set-variant-keys", (FN) tmg_set_variant_keys, 2, 0, 0);
  gh_new_procedure ("kbd-pre-rewrite", (FN) tmg_kbd_pre_rewrite, 1, 0, 0);
  gh_new_procedure ("kbd-post-rewrite", (FN) tmg_kbd_post_rewrite, 1, 0, 0);
  gh_new_procedure ("set-font-rules", (FN) tmg_set_font_rules, 1, 0, 0);
  gh_new_procedure ("window-set-property", (FN) tmg_window_set_property, 2, 0, 0);
  gh_new_procedure ("window-get-property", (FN) tmg_window_get_property, 1, 0, 0);
  gh_new_procedure ("show-header", (FN) tmg_show_header, 1, 0, 0);
  gh_new_procedure ("show-icon-bar", (FN) tmg_show_icon_bar, 2, 0, 0);
  gh_new_procedure ("show-footer", (FN) tmg_show_footer, 1, 0, 0);
  gh_new_procedure ("visible-header?", (FN) tmg_visible_headerP, 0, 0, 0);
  gh_new_procedure ("visible-icon-bar?", (FN) tmg_visible_icon_barP, 1, 0, 0);
  gh_new_procedure ("visible-footer?", (FN) tmg_visible_footerP, 0, 0, 0);
  gh_new_procedure ("full-screen-mode", (FN) tmg_full_screen_mode, 2, 0, 0);
  gh_new_procedure ("full-screen?", (FN) tmg_full_screenP, 0, 0, 0);
  gh_new_procedure ("full-screen-edit?", (FN) tmg_full_screen_editP, 0, 0, 0);
  gh_new_procedure ("set-shrinking-factor", (FN) tmg_set_shrinking_factor, 1, 0, 0);
  gh_new_procedure ("get-shrinking-factor", (FN) tmg_get_shrinking_factor, 0, 0, 0);
  gh_new_procedure ("exec-delayed", (FN) tmg_exec_delayed, 1, 0, 0);
  gh_new_procedure ("exec-delayed-cmd", (FN) tmg_exec_delayed_cmd, 1, 0, 0);
  gh_new_procedure ("shell", (FN) tmg_shell, 1, 0, 0);
  gh_new_procedure ("dialogue-end", (FN) tmg_dialogue_end, 0, 0, 0);
  gh_new_procedure ("choose-file", (FN) tmg_choose_file, 3, 0, 0);
  gh_new_procedure ("has-view?", (FN) tmg_has_viewP, 0, 0, 0);
  gh_new_procedure ("buffer-unsaved?", (FN) tmg_buffer_unsavedP, 0, 0, 0);
  gh_new_procedure ("exists-unsaved-buffer?", (FN) tmg_exists_unsaved_bufferP, 0, 0, 0);
  gh_new_procedure ("pretend-save-buffer", (FN) tmg_pretend_save_buffer, 0, 0, 0);
  gh_new_procedure ("get-name-buffer", (FN) tmg_get_name_buffer, 0, 0, 0);
  gh_new_procedure ("set-name-buffer", (FN) tmg_set_name_buffer, 1, 0, 0);
  gh_new_procedure ("set-abbr-buffer", (FN) tmg_set_abbr_buffer, 1, 0, 0);
  gh_new_procedure ("new-buffer", (FN) tmg_new_buffer, 0, 0, 0);
  gh_new_procedure ("switch-to-buffer", (FN) tmg_switch_to_buffer, 1, 0, 0);
  gh_new_procedure ("switch-to-active-buffer", (FN) tmg_switch_to_active_buffer, 1, 0, 0);
  gh_new_procedure ("revert-buffer", (FN) tmg_revert_buffer, 0, 0, 0);
  gh_new_procedure ("kill-buffer", (FN) tmg_kill_buffer, 0, 0, 0);
  gh_new_procedure ("open-window", (FN) tmg_open_window, 0, 0, 0);
  gh_new_procedure ("clone-window", (FN) tmg_clone_window, 0, 0, 0);
  gh_new_procedure ("kill-window", (FN) tmg_kill_window, 0, 0, 0);
  gh_new_procedure ("set-maximal-undo-depth", (FN) tmg_set_maximal_undo_depth, 1, 0, 0);
  gh_new_procedure ("get-maximal-undo-depth", (FN) tmg_get_maximal_undo_depth, 0, 0, 0);
  gh_new_procedure ("no-name?", (FN) tmg_no_nameP, 0, 0, 0);
  gh_new_procedure ("help-buffer?", (FN) tmg_help_bufferP, 0, 0, 0);
  gh_new_procedure ("set-buffer", (FN) tmg_set_buffer, 2, 0, 0);
  gh_new_procedure ("set-aux-buffer", (FN) tmg_set_aux_buffer, 3, 0, 0);
  gh_new_procedure ("set-help-buffer", (FN) tmg_set_help_buffer, 2, 0, 0);
  gh_new_procedure ("browse-help", (FN) tmg_browse_help, 1, 0, 0);
  gh_new_procedure ("project-attach", (FN) tmg_project_attach, 1, 0, 0);
  gh_new_procedure ("project-detach", (FN) tmg_project_detach, 0, 0, 0);
  gh_new_procedure ("project-attached?", (FN) tmg_project_attachedP, 0, 0, 0);
  gh_new_procedure ("texmacs-load-tree", (FN) tmg_texmacs_load_tree, 2, 0, 0);
  gh_new_procedure ("texmacs-load-buffer", (FN) tmg_texmacs_load_buffer, 4, 0, 0);
  gh_new_procedure ("texmacs-save-buffer", (FN) tmg_texmacs_save_buffer, 2, 0, 0);
  gh_new_procedure ("delayed-autosave", (FN) tmg_delayed_autosave, 0, 0, 0);
  gh_new_procedure ("color", (FN) tmg_color, 1, 0, 0);
  gh_new_procedure ("style-clear-cache", (FN) tmg_style_clear_cache, 0, 0, 0);
  gh_new_procedure ("set-script-status", (FN) tmg_set_script_status, 1, 0, 0);
  gh_new_procedure ("set-printing-command", (FN) tmg_set_printing_command, 1, 0, 0);
  gh_new_procedure ("set-printer-paper-type", (FN) tmg_set_printer_paper_type, 1, 0, 0);
  gh_new_procedure ("get-printer-paper-type", (FN) tmg_get_printer_paper_type, 0, 0, 0);
  gh_new_procedure ("set-printer-dpi", (FN) tmg_set_printer_dpi, 1, 0, 0);
  gh_new_procedure ("set-default-shrinking-factor", (FN) tmg_set_default_shrinking_factor, 1, 0, 0);
  gh_new_procedure ("get-default-shrinking-factor", (FN) tmg_get_default_shrinking_factor, 0, 0, 0);
  gh_new_procedure ("get-nr-windows", (FN) tmg_get_nr_windows, 0, 0, 0);
  gh_new_procedure ("postscript-gc", (FN) tmg_postscript_gc, 0, 0, 0);
  gh_new_procedure ("inclusions-gc", (FN) tmg_inclusions_gc, 0, 0, 0);
  gh_new_procedure ("translate", (FN) tmg_translate, 3, 0, 0);
  gh_new_procedure ("yes?", (FN) tmg_yesP, 1, 0, 0);
  gh_new_procedure ("quit-TeXmacs", (FN) tmg_quit_TeXmacs, 0, 0, 0);
  gh_new_procedure ("package-evaluate", (FN) tmg_package_evaluate, 3, 0, 0);
}
