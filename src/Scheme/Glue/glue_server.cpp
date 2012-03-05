
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
tmg_window_get_id () {
  // TMSCM_DEFER_INTS;
  int out= get_server()->get_window_id ();
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
tmg_set_shrinking_factor (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "set-shrinking-factor");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  get_server()->set_shrinking_factor (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_shrinking_factor () {
  // TMSCM_DEFER_INTS;
  int out= get_server()->get_shrinking_factor ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
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
tmg_choose_file (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_OBJECT (arg1, TMSCM_ARG1, "choose-file");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "choose-file");
  TMSCM_ASSERT_STRING (arg3, TMSCM_ARG3, "choose-file");

  object in1= tmscm_to_object (arg1);
  string in2= tmscm_to_string (arg2);
  string in3= tmscm_to_string (arg3);

  // TMSCM_DEFER_INTS;
  get_server()->choose_file (in1, in2, in3);
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
tmg_has_viewP () {
  // TMSCM_DEFER_INTS;
  bool out= get_server()->has_view ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_buffer_unsavedP () {
  // TMSCM_DEFER_INTS;
  bool out= get_server()->buffer_unsaved ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_exists_unsaved_bufferP () {
  // TMSCM_DEFER_INTS;
  bool out= get_server()->exists_unsaved_buffer ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_pretend_save_buffer () {
  // TMSCM_DEFER_INTS;
  get_server()->pretend_save_buffer ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_name_buffer (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "set-name-buffer");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  get_server()->set_name_buffer (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_name_buffer () {
  // TMSCM_DEFER_INTS;
  url out= get_server()->get_name_buffer ();
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_get_name_buffer_path (tmscm arg1) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "get-name-buffer-path");

  path in1= tmscm_to_path (arg1);

  // TMSCM_DEFER_INTS;
  url out= get_server()->get_name_buffer (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_set_abbr_buffer (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "set-abbr-buffer");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_server()->set_abbr_buffer (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_abbr_buffer () {
  // TMSCM_DEFER_INTS;
  string out= get_server()->get_abbr_buffer ();
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_new_buffer () {
  // TMSCM_DEFER_INTS;
  url out= get_server()->new_buffer ();
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_switch_to_buffer_path (tmscm arg1) {
  TMSCM_ASSERT_PATH (arg1, TMSCM_ARG1, "switch-to-buffer-path");

  path in1= tmscm_to_path (arg1);

  // TMSCM_DEFER_INTS;
  bool out= get_server()->switch_to_buffer (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_switch_to_buffer (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "switch-to-buffer");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  get_server()->switch_to_buffer (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_switch_to_active_buffer (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "switch-to-active-buffer");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  get_server()->switch_to_active_buffer (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_revert_buffer () {
  // TMSCM_DEFER_INTS;
  get_server()->revert_buffer ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_kill_buffer () {
  // TMSCM_DEFER_INTS;
  get_server()->kill_buffer ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
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
  get_server()->new_buffer_in_new_window (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_open_window () {
  // TMSCM_DEFER_INTS;
  url out= get_server()->open_window ();
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_open_window_geometry (tmscm arg1) {
  TMSCM_ASSERT_CONTENT (arg1, TMSCM_ARG1, "open-window-geometry");

  content in1= tmscm_to_content (arg1);

  // TMSCM_DEFER_INTS;
  url out= get_server()->open_window (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_clone_window () {
  // TMSCM_DEFER_INTS;
  get_server()->clone_window ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_kill_window () {
  // TMSCM_DEFER_INTS;
  get_server()->kill_window ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_kill_window_and_buffer () {
  // TMSCM_DEFER_INTS;
  get_server()->kill_window_and_buffer ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_no_nameP () {
  // TMSCM_DEFER_INTS;
  bool out= get_server()->no_name ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_help_bufferP () {
  // TMSCM_DEFER_INTS;
  bool out= get_server()->help_buffer ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_set_buffer (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "set-buffer");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "set-buffer");

  url in1= tmscm_to_url (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  get_server()->revert_buffer (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_aux (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "set-aux");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "set-aux");

  string in1= tmscm_to_string (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  get_server()->set_aux (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_aux_buffer (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "set-aux-buffer");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "set-aux-buffer");
  TMSCM_ASSERT_CONTENT (arg3, TMSCM_ARG3, "set-aux-buffer");

  string in1= tmscm_to_string (arg1);
  url in2= tmscm_to_url (arg2);
  content in3= tmscm_to_content (arg3);

  // TMSCM_DEFER_INTS;
  get_server()->set_aux_buffer (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_help_buffer (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "set-help-buffer");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "set-help-buffer");

  url in1= tmscm_to_url (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  get_server()->set_help_buffer (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_set_buffer_tree (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "set-buffer-tree");
  TMSCM_ASSERT_CONTENT (arg2, TMSCM_ARG2, "set-buffer-tree");

  url in1= tmscm_to_url (arg1);
  content in2= tmscm_to_content (arg2);

  // TMSCM_DEFER_INTS;
  get_server()->set_buffer_tree (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_buffer_tree (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "get-buffer-tree");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  tree out= get_server()->get_buffer_tree (in1);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_get_all_buffers () {
  // TMSCM_DEFER_INTS;
  url out= get_server()->get_all_buffers ();
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_get_buffer_menu () {
  // TMSCM_DEFER_INTS;
  object out= get_server()->get_buffer_menu ();
  // TMSCM_ALLOW_INTS;

  return object_to_tmscm (out);
}

tmscm
tmg_buffer_in_menu (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer-in-menu");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "buffer-in-menu");

  url in1= tmscm_to_url (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  bool out= get_server()->buffer_in_menu (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_project_attach (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "project-attach");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  get_server()->project_attach (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_project_detach () {
  // TMSCM_DEFER_INTS;
  get_server()->project_attach ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_project_attachedP () {
  // TMSCM_DEFER_INTS;
  bool out= get_server()->project_attached ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_get_project_buffer_menu () {
  // TMSCM_DEFER_INTS;
  object out= get_server()->get_project_buffer_menu ();
  // TMSCM_ALLOW_INTS;

  return object_to_tmscm (out);
}

tmscm
tmg_window_current () {
  // TMSCM_DEFER_INTS;
  int out= get_server()->window_current ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_window_list () {
  // TMSCM_DEFER_INTS;
  path out= get_server()->windows_list ();
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_buffer_2windows (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "buffer->windows");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  path out= get_server()->buffer_to_windows (in1);
  // TMSCM_ALLOW_INTS;

  return path_to_tmscm (out);
}

tmscm
tmg_window_2buffer (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "window->buffer");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  url out= get_server()->window_to_buffer (in1);
  // TMSCM_ALLOW_INTS;

  return url_to_tmscm (out);
}

tmscm
tmg_window_set_buffer (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "window-set-buffer");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "window-set-buffer");

  int in1= tmscm_to_int (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  get_server()->window_set_buffer (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_window_focus (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "window-focus");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  get_server()->window_focus (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_texmacs_load_tree (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "texmacs-load-tree");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "texmacs-load-tree");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  tree out= get_server()->load_tree (in1, in2);
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_texmacs_load_buffer (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "texmacs-load-buffer");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "texmacs-load-buffer");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "texmacs-load-buffer");
  TMSCM_ASSERT_BOOL (arg4, TMSCM_ARG4, "texmacs-load-buffer");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);
  int in3= tmscm_to_int (arg3);
  bool in4= tmscm_to_bool (arg4);

  // TMSCM_DEFER_INTS;
  get_server()->load_buffer (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_texmacs_save_buffer (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "texmacs-save-buffer");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "texmacs-save-buffer");

  url in1= tmscm_to_url (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  get_server()->save_buffer (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_auto_save () {
  // TMSCM_DEFER_INTS;
  get_server()->auto_save ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_style_menu () {
  // TMSCM_DEFER_INTS;
  object out= get_server()->get_style_menu ();
  // TMSCM_ALLOW_INTS;

  return object_to_tmscm (out);
}

tmscm
tmg_get_add_package_menu () {
  // TMSCM_DEFER_INTS;
  object out= get_server()->get_add_package_menu ();
  // TMSCM_ALLOW_INTS;

  return object_to_tmscm (out);
}

tmscm
tmg_get_remove_package_menu () {
  // TMSCM_DEFER_INTS;
  object out= get_server()->get_remove_package_menu ();
  // TMSCM_ALLOW_INTS;

  return object_to_tmscm (out);
}

tmscm
tmg_style_clear_cache () {
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
tmg_set_default_shrinking_factor (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "set-default-shrinking-factor");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  get_server()->set_default_shrinking_factor (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_get_default_shrinking_factor () {
  // TMSCM_DEFER_INTS;
  int out= get_server()->get_default_shrinking_factor ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_get_nr_windows () {
  // TMSCM_DEFER_INTS;
  int out= get_server()->get_nr_windows ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_image_gc () {
  // TMSCM_DEFER_INTS;
  get_server()->image_gc ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
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
  tmscm_install_procedure ("window-get-id",  tmg_window_get_id, 0, 0, 0);
  tmscm_install_procedure ("window-set-property",  tmg_window_set_property, 2, 0, 0);
  tmscm_install_procedure ("window-get-property",  tmg_window_get_property, 1, 0, 0);
  tmscm_install_procedure ("show-header",  tmg_show_header, 1, 0, 0);
  tmscm_install_procedure ("show-icon-bar",  tmg_show_icon_bar, 2, 0, 0);
  tmscm_install_procedure ("show-side-tools",  tmg_show_side_tools, 2, 0, 0);
  tmscm_install_procedure ("show-footer",  tmg_show_footer, 1, 0, 0);
  tmscm_install_procedure ("visible-header?",  tmg_visible_headerP, 0, 0, 0);
  tmscm_install_procedure ("visible-icon-bar?",  tmg_visible_icon_barP, 1, 0, 0);
  tmscm_install_procedure ("visible-side-tools?",  tmg_visible_side_toolsP, 1, 0, 0);
  tmscm_install_procedure ("visible-footer?",  tmg_visible_footerP, 0, 0, 0);
  tmscm_install_procedure ("full-screen-mode",  tmg_full_screen_mode, 2, 0, 0);
  tmscm_install_procedure ("full-screen?",  tmg_full_screenP, 0, 0, 0);
  tmscm_install_procedure ("full-screen-edit?",  tmg_full_screen_editP, 0, 0, 0);
  tmscm_install_procedure ("set-shrinking-factor",  tmg_set_shrinking_factor, 1, 0, 0);
  tmscm_install_procedure ("get-shrinking-factor",  tmg_get_shrinking_factor, 0, 0, 0);
  tmscm_install_procedure ("shell",  tmg_shell, 1, 0, 0);
  tmscm_install_procedure ("dialogue-end",  tmg_dialogue_end, 0, 0, 0);
  tmscm_install_procedure ("choose-file",  tmg_choose_file, 3, 0, 0);
  tmscm_install_procedure ("tm-interactive",  tmg_tm_interactive, 2, 0, 0);
  tmscm_install_procedure ("has-view?",  tmg_has_viewP, 0, 0, 0);
  tmscm_install_procedure ("buffer-unsaved?",  tmg_buffer_unsavedP, 0, 0, 0);
  tmscm_install_procedure ("exists-unsaved-buffer?",  tmg_exists_unsaved_bufferP, 0, 0, 0);
  tmscm_install_procedure ("pretend-save-buffer",  tmg_pretend_save_buffer, 0, 0, 0);
  tmscm_install_procedure ("set-name-buffer",  tmg_set_name_buffer, 1, 0, 0);
  tmscm_install_procedure ("get-name-buffer",  tmg_get_name_buffer, 0, 0, 0);
  tmscm_install_procedure ("get-name-buffer-path",  tmg_get_name_buffer_path, 1, 0, 0);
  tmscm_install_procedure ("set-abbr-buffer",  tmg_set_abbr_buffer, 1, 0, 0);
  tmscm_install_procedure ("get-abbr-buffer",  tmg_get_abbr_buffer, 0, 0, 0);
  tmscm_install_procedure ("new-buffer",  tmg_new_buffer, 0, 0, 0);
  tmscm_install_procedure ("switch-to-buffer-path",  tmg_switch_to_buffer_path, 1, 0, 0);
  tmscm_install_procedure ("switch-to-buffer",  tmg_switch_to_buffer, 1, 0, 0);
  tmscm_install_procedure ("switch-to-active-buffer",  tmg_switch_to_active_buffer, 1, 0, 0);
  tmscm_install_procedure ("revert-buffer",  tmg_revert_buffer, 0, 0, 0);
  tmscm_install_procedure ("kill-buffer",  tmg_kill_buffer, 0, 0, 0);
  tmscm_install_procedure ("open-buffer-in-window",  tmg_open_buffer_in_window, 3, 0, 0);
  tmscm_install_procedure ("open-window",  tmg_open_window, 0, 0, 0);
  tmscm_install_procedure ("open-window-geometry",  tmg_open_window_geometry, 1, 0, 0);
  tmscm_install_procedure ("clone-window",  tmg_clone_window, 0, 0, 0);
  tmscm_install_procedure ("kill-window",  tmg_kill_window, 0, 0, 0);
  tmscm_install_procedure ("kill-window-and-buffer",  tmg_kill_window_and_buffer, 0, 0, 0);
  tmscm_install_procedure ("no-name?",  tmg_no_nameP, 0, 0, 0);
  tmscm_install_procedure ("help-buffer?",  tmg_help_bufferP, 0, 0, 0);
  tmscm_install_procedure ("set-buffer",  tmg_set_buffer, 2, 0, 0);
  tmscm_install_procedure ("set-aux",  tmg_set_aux, 2, 0, 0);
  tmscm_install_procedure ("set-aux-buffer",  tmg_set_aux_buffer, 3, 0, 0);
  tmscm_install_procedure ("set-help-buffer",  tmg_set_help_buffer, 2, 0, 0);
  tmscm_install_procedure ("set-buffer-tree",  tmg_set_buffer_tree, 2, 0, 0);
  tmscm_install_procedure ("get-buffer-tree",  tmg_get_buffer_tree, 1, 0, 0);
  tmscm_install_procedure ("get-all-buffers",  tmg_get_all_buffers, 0, 0, 0);
  tmscm_install_procedure ("get-buffer-menu",  tmg_get_buffer_menu, 0, 0, 0);
  tmscm_install_procedure ("buffer-in-menu",  tmg_buffer_in_menu, 2, 0, 0);
  tmscm_install_procedure ("project-attach",  tmg_project_attach, 1, 0, 0);
  tmscm_install_procedure ("project-detach",  tmg_project_detach, 0, 0, 0);
  tmscm_install_procedure ("project-attached?",  tmg_project_attachedP, 0, 0, 0);
  tmscm_install_procedure ("get-project-buffer-menu",  tmg_get_project_buffer_menu, 0, 0, 0);
  tmscm_install_procedure ("window-current",  tmg_window_current, 0, 0, 0);
  tmscm_install_procedure ("window-list",  tmg_window_list, 0, 0, 0);
  tmscm_install_procedure ("buffer->windows",  tmg_buffer_2windows, 1, 0, 0);
  tmscm_install_procedure ("window->buffer",  tmg_window_2buffer, 1, 0, 0);
  tmscm_install_procedure ("window-set-buffer",  tmg_window_set_buffer, 2, 0, 0);
  tmscm_install_procedure ("window-focus",  tmg_window_focus, 1, 0, 0);
  tmscm_install_procedure ("texmacs-load-tree",  tmg_texmacs_load_tree, 2, 0, 0);
  tmscm_install_procedure ("texmacs-load-buffer",  tmg_texmacs_load_buffer, 4, 0, 0);
  tmscm_install_procedure ("texmacs-save-buffer",  tmg_texmacs_save_buffer, 2, 0, 0);
  tmscm_install_procedure ("auto-save",  tmg_auto_save, 0, 0, 0);
  tmscm_install_procedure ("get-style-menu",  tmg_get_style_menu, 0, 0, 0);
  tmscm_install_procedure ("get-add-package-menu",  tmg_get_add_package_menu, 0, 0, 0);
  tmscm_install_procedure ("get-remove-package-menu",  tmg_get_remove_package_menu, 0, 0, 0);
  tmscm_install_procedure ("style-clear-cache",  tmg_style_clear_cache, 0, 0, 0);
  tmscm_install_procedure ("set-script-status",  tmg_set_script_status, 1, 0, 0);
  tmscm_install_procedure ("set-printing-command",  tmg_set_printing_command, 1, 0, 0);
  tmscm_install_procedure ("set-printer-paper-type",  tmg_set_printer_paper_type, 1, 0, 0);
  tmscm_install_procedure ("get-printer-paper-type",  tmg_get_printer_paper_type, 0, 0, 0);
  tmscm_install_procedure ("set-printer-dpi",  tmg_set_printer_dpi, 1, 0, 0);
  tmscm_install_procedure ("set-default-shrinking-factor",  tmg_set_default_shrinking_factor, 1, 0, 0);
  tmscm_install_procedure ("get-default-shrinking-factor",  tmg_get_default_shrinking_factor, 0, 0, 0);
  tmscm_install_procedure ("get-nr-windows",  tmg_get_nr_windows, 0, 0, 0);
  tmscm_install_procedure ("image-gc",  tmg_image_gc, 0, 0, 0);
  tmscm_install_procedure ("inclusions-gc",  tmg_inclusions_gc, 0, 0, 0);
  tmscm_install_procedure ("update-all-path",  tmg_update_all_path, 1, 0, 0);
  tmscm_install_procedure ("update-all-buffers",  tmg_update_all_buffers, 0, 0, 0);
  tmscm_install_procedure ("set-message",  tmg_set_message, 2, 0, 0);
  tmscm_install_procedure ("set-message-temp",  tmg_set_message_temp, 3, 0, 0);
  tmscm_install_procedure ("recall-message",  tmg_recall_message, 0, 0, 0);
  tmscm_install_procedure ("yes?",  tmg_yesP, 1, 0, 0);
  tmscm_install_procedure ("quit-TeXmacs",  tmg_quit_TeXmacs, 0, 0, 0);
}
