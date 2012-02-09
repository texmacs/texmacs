
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
tmg_kbd_post_rewrite (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "kbd-post-rewrite");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "kbd-post-rewrite");

  string in1= scm_to_string (arg1);
  bool in2= scm_to_bool (arg2);

  // SCM_DEFER_INTS;
  string out= get_server()->kbd_post_rewrite (in1, in2);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_kbd_system_rewrite (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "kbd-system-rewrite");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tree out= get_server()->kbd_system_rewrite (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
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
tmg_window_get_id () {
  // SCM_DEFER_INTS;
  int out= get_server()->get_window_id ();
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
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
tmg_show_side_tools (SCM arg1, SCM arg2) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "show-side-tools");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "show-side-tools");

  int in1= scm_to_int (arg1);
  bool in2= scm_to_bool (arg2);

  // SCM_DEFER_INTS;
  get_server()->show_side_tools (in1, in2);
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
tmg_visible_side_toolsP (SCM arg1) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "visible-side-tools?");

  int in1= scm_to_int (arg1);

  // SCM_DEFER_INTS;
  bool out= get_server()->visible_side_tools (in1);
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
  SCM_ASSERT_OBJECT (arg1, SCM_ARG1, "choose-file");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "choose-file");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "choose-file");

  object in1= scm_to_object (arg1);
  string in2= scm_to_string (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  get_server()->choose_file (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_tm_interactive (SCM arg1, SCM arg2) {
  SCM_ASSERT_OBJECT (arg1, SCM_ARG1, "tm-interactive");
  SCM_ASSERT_SCHEME_TREE (arg2, SCM_ARG2, "tm-interactive");

  object in1= scm_to_object (arg1);
  scheme_tree in2= scm_to_scheme_tree (arg2);

  // SCM_DEFER_INTS;
  get_server()->interactive (in1, in2);
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
tmg_set_name_buffer (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "set-name-buffer");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  get_server()->set_name_buffer (in1);
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
tmg_get_name_buffer_path (SCM arg1) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "get-name-buffer-path");

  path in1= scm_to_path (arg1);

  // SCM_DEFER_INTS;
  url out= get_server()->get_name_buffer (in1);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_set_abbr_buffer (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "set-abbr-buffer");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  get_server()->set_abbr_buffer (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_get_abbr_buffer () {
  // SCM_DEFER_INTS;
  string out= get_server()->get_abbr_buffer ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_new_buffer () {
  // SCM_DEFER_INTS;
  url out= get_server()->new_buffer ();
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_switch_to_buffer_path (SCM arg1) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "switch-to-buffer-path");

  path in1= scm_to_path (arg1);

  // SCM_DEFER_INTS;
  bool out= get_server()->switch_to_buffer (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
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
tmg_open_buffer_in_window (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "open-buffer-in-window");
  SCM_ASSERT_CONTENT (arg2, SCM_ARG2, "open-buffer-in-window");
  SCM_ASSERT_CONTENT (arg3, SCM_ARG3, "open-buffer-in-window");

  url in1= scm_to_url (arg1);
  content in2= scm_to_content (arg2);
  content in3= scm_to_content (arg3);

  // SCM_DEFER_INTS;
  get_server()->new_buffer_in_new_window (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_open_window () {
  // SCM_DEFER_INTS;
  url out= get_server()->open_window ();
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_open_window_geometry (SCM arg1) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "open-window-geometry");

  content in1= scm_to_content (arg1);

  // SCM_DEFER_INTS;
  url out= get_server()->open_window (in1);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
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
tmg_kill_window_and_buffer () {
  // SCM_DEFER_INTS;
  get_server()->kill_window_and_buffer ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
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
tmg_set_aux (SCM arg1, SCM arg2) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "set-aux");
  SCM_ASSERT_URL (arg2, SCM_ARG2, "set-aux");

  string in1= scm_to_string (arg1);
  url in2= scm_to_url (arg2);

  // SCM_DEFER_INTS;
  get_server()->set_aux (in1, in2);
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
tmg_set_buffer_tree (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "set-buffer-tree");
  SCM_ASSERT_CONTENT (arg2, SCM_ARG2, "set-buffer-tree");

  url in1= scm_to_url (arg1);
  content in2= scm_to_content (arg2);

  // SCM_DEFER_INTS;
  get_server()->set_buffer_tree (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_get_buffer_tree (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "get-buffer-tree");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  tree out= get_server()->get_buffer_tree (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_get_all_buffers () {
  // SCM_DEFER_INTS;
  url out= get_server()->get_all_buffers ();
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_get_buffer_menu () {
  // SCM_DEFER_INTS;
  object out= get_server()->get_buffer_menu ();
  // SCM_ALLOW_INTS;

  return object_to_scm (out);
}

SCM
tmg_buffer_in_menu (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "buffer-in-menu");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "buffer-in-menu");

  url in1= scm_to_url (arg1);
  bool in2= scm_to_bool (arg2);

  // SCM_DEFER_INTS;
  bool out= get_server()->buffer_in_menu (in1, in2);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
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
tmg_get_project_buffer_menu () {
  // SCM_DEFER_INTS;
  object out= get_server()->get_project_buffer_menu ();
  // SCM_ALLOW_INTS;

  return object_to_scm (out);
}

SCM
tmg_window_current () {
  // SCM_DEFER_INTS;
  int out= get_server()->window_current ();
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_window_list () {
  // SCM_DEFER_INTS;
  path out= get_server()->windows_list ();
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_buffer_2windows (SCM arg1) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "buffer->windows");

  url in1= scm_to_url (arg1);

  // SCM_DEFER_INTS;
  path out= get_server()->buffer_to_windows (in1);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_window_2buffer (SCM arg1) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "window->buffer");

  int in1= scm_to_int (arg1);

  // SCM_DEFER_INTS;
  url out= get_server()->window_to_buffer (in1);
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
}

SCM
tmg_window_set_buffer (SCM arg1, SCM arg2) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "window-set-buffer");
  SCM_ASSERT_URL (arg2, SCM_ARG2, "window-set-buffer");

  int in1= scm_to_int (arg1);
  url in2= scm_to_url (arg2);

  // SCM_DEFER_INTS;
  get_server()->window_set_buffer (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_window_focus (SCM arg1) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "window-focus");

  int in1= scm_to_int (arg1);

  // SCM_DEFER_INTS;
  get_server()->window_focus (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
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
tmg_auto_save () {
  // SCM_DEFER_INTS;
  get_server()->auto_save ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_get_style_menu () {
  // SCM_DEFER_INTS;
  object out= get_server()->get_style_menu ();
  // SCM_ALLOW_INTS;

  return object_to_scm (out);
}

SCM
tmg_get_add_package_menu () {
  // SCM_DEFER_INTS;
  object out= get_server()->get_add_package_menu ();
  // SCM_ALLOW_INTS;

  return object_to_scm (out);
}

SCM
tmg_get_remove_package_menu () {
  // SCM_DEFER_INTS;
  object out= get_server()->get_remove_package_menu ();
  // SCM_ALLOW_INTS;

  return object_to_scm (out);
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
tmg_image_gc () {
  // SCM_DEFER_INTS;
  get_server()->image_gc ();
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
tmg_update_all_path (SCM arg1) {
  SCM_ASSERT_PATH (arg1, SCM_ARG1, "update-all-path");

  path in1= scm_to_path (arg1);

  // SCM_DEFER_INTS;
  get_server()->typeset_update (in1);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_update_all_buffers () {
  // SCM_DEFER_INTS;
  get_server()->typeset_update_all ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_set_message (SCM arg1, SCM arg2) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "set-message");
  SCM_ASSERT_CONTENT (arg2, SCM_ARG2, "set-message");

  content in1= scm_to_content (arg1);
  content in2= scm_to_content (arg2);

  // SCM_DEFER_INTS;
  get_server()->set_message (in1, in2);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_set_message_temp (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_CONTENT (arg1, SCM_ARG1, "set-message-temp");
  SCM_ASSERT_CONTENT (arg2, SCM_ARG2, "set-message-temp");
  SCM_ASSERT_BOOL (arg3, SCM_ARG3, "set-message-temp");

  content in1= scm_to_content (arg1);
  content in2= scm_to_content (arg2);
  bool in3= scm_to_bool (arg3);

  // SCM_DEFER_INTS;
  get_server()->set_message (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_recall_message () {
  // SCM_DEFER_INTS;
  get_server()->recall_message ();
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
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

void
initialize_glue_server () {
  scm_new_procedure ("insert-kbd-wildcard", (FN) tmg_insert_kbd_wildcard, 5, 0, 0);
  scm_new_procedure ("set-variant-keys", (FN) tmg_set_variant_keys, 2, 0, 0);
  scm_new_procedure ("kbd-pre-rewrite", (FN) tmg_kbd_pre_rewrite, 1, 0, 0);
  scm_new_procedure ("kbd-post-rewrite", (FN) tmg_kbd_post_rewrite, 2, 0, 0);
  scm_new_procedure ("kbd-system-rewrite", (FN) tmg_kbd_system_rewrite, 1, 0, 0);
  scm_new_procedure ("set-font-rules", (FN) tmg_set_font_rules, 1, 0, 0);
  scm_new_procedure ("window-get-id", (FN) tmg_window_get_id, 0, 0, 0);
  scm_new_procedure ("window-set-property", (FN) tmg_window_set_property, 2, 0, 0);
  scm_new_procedure ("window-get-property", (FN) tmg_window_get_property, 1, 0, 0);
  scm_new_procedure ("show-header", (FN) tmg_show_header, 1, 0, 0);
  scm_new_procedure ("show-icon-bar", (FN) tmg_show_icon_bar, 2, 0, 0);
  scm_new_procedure ("show-side-tools", (FN) tmg_show_side_tools, 2, 0, 0);
  scm_new_procedure ("show-footer", (FN) tmg_show_footer, 1, 0, 0);
  scm_new_procedure ("visible-header?", (FN) tmg_visible_headerP, 0, 0, 0);
  scm_new_procedure ("visible-icon-bar?", (FN) tmg_visible_icon_barP, 1, 0, 0);
  scm_new_procedure ("visible-side-tools?", (FN) tmg_visible_side_toolsP, 1, 0, 0);
  scm_new_procedure ("visible-footer?", (FN) tmg_visible_footerP, 0, 0, 0);
  scm_new_procedure ("full-screen-mode", (FN) tmg_full_screen_mode, 2, 0, 0);
  scm_new_procedure ("full-screen?", (FN) tmg_full_screenP, 0, 0, 0);
  scm_new_procedure ("full-screen-edit?", (FN) tmg_full_screen_editP, 0, 0, 0);
  scm_new_procedure ("set-shrinking-factor", (FN) tmg_set_shrinking_factor, 1, 0, 0);
  scm_new_procedure ("get-shrinking-factor", (FN) tmg_get_shrinking_factor, 0, 0, 0);
  scm_new_procedure ("shell", (FN) tmg_shell, 1, 0, 0);
  scm_new_procedure ("dialogue-end", (FN) tmg_dialogue_end, 0, 0, 0);
  scm_new_procedure ("choose-file", (FN) tmg_choose_file, 3, 0, 0);
  scm_new_procedure ("tm-interactive", (FN) tmg_tm_interactive, 2, 0, 0);
  scm_new_procedure ("has-view?", (FN) tmg_has_viewP, 0, 0, 0);
  scm_new_procedure ("buffer-unsaved?", (FN) tmg_buffer_unsavedP, 0, 0, 0);
  scm_new_procedure ("exists-unsaved-buffer?", (FN) tmg_exists_unsaved_bufferP, 0, 0, 0);
  scm_new_procedure ("pretend-save-buffer", (FN) tmg_pretend_save_buffer, 0, 0, 0);
  scm_new_procedure ("set-name-buffer", (FN) tmg_set_name_buffer, 1, 0, 0);
  scm_new_procedure ("get-name-buffer", (FN) tmg_get_name_buffer, 0, 0, 0);
  scm_new_procedure ("get-name-buffer-path", (FN) tmg_get_name_buffer_path, 1, 0, 0);
  scm_new_procedure ("set-abbr-buffer", (FN) tmg_set_abbr_buffer, 1, 0, 0);
  scm_new_procedure ("get-abbr-buffer", (FN) tmg_get_abbr_buffer, 0, 0, 0);
  scm_new_procedure ("new-buffer", (FN) tmg_new_buffer, 0, 0, 0);
  scm_new_procedure ("switch-to-buffer-path", (FN) tmg_switch_to_buffer_path, 1, 0, 0);
  scm_new_procedure ("switch-to-buffer", (FN) tmg_switch_to_buffer, 1, 0, 0);
  scm_new_procedure ("switch-to-active-buffer", (FN) tmg_switch_to_active_buffer, 1, 0, 0);
  scm_new_procedure ("revert-buffer", (FN) tmg_revert_buffer, 0, 0, 0);
  scm_new_procedure ("kill-buffer", (FN) tmg_kill_buffer, 0, 0, 0);
  scm_new_procedure ("open-buffer-in-window", (FN) tmg_open_buffer_in_window, 3, 0, 0);
  scm_new_procedure ("open-window", (FN) tmg_open_window, 0, 0, 0);
  scm_new_procedure ("open-window-geometry", (FN) tmg_open_window_geometry, 1, 0, 0);
  scm_new_procedure ("clone-window", (FN) tmg_clone_window, 0, 0, 0);
  scm_new_procedure ("kill-window", (FN) tmg_kill_window, 0, 0, 0);
  scm_new_procedure ("kill-window-and-buffer", (FN) tmg_kill_window_and_buffer, 0, 0, 0);
  scm_new_procedure ("no-name?", (FN) tmg_no_nameP, 0, 0, 0);
  scm_new_procedure ("help-buffer?", (FN) tmg_help_bufferP, 0, 0, 0);
  scm_new_procedure ("set-buffer", (FN) tmg_set_buffer, 2, 0, 0);
  scm_new_procedure ("set-aux", (FN) tmg_set_aux, 2, 0, 0);
  scm_new_procedure ("set-aux-buffer", (FN) tmg_set_aux_buffer, 3, 0, 0);
  scm_new_procedure ("set-help-buffer", (FN) tmg_set_help_buffer, 2, 0, 0);
  scm_new_procedure ("set-buffer-tree", (FN) tmg_set_buffer_tree, 2, 0, 0);
  scm_new_procedure ("get-buffer-tree", (FN) tmg_get_buffer_tree, 1, 0, 0);
  scm_new_procedure ("get-all-buffers", (FN) tmg_get_all_buffers, 0, 0, 0);
  scm_new_procedure ("get-buffer-menu", (FN) tmg_get_buffer_menu, 0, 0, 0);
  scm_new_procedure ("buffer-in-menu", (FN) tmg_buffer_in_menu, 2, 0, 0);
  scm_new_procedure ("project-attach", (FN) tmg_project_attach, 1, 0, 0);
  scm_new_procedure ("project-detach", (FN) tmg_project_detach, 0, 0, 0);
  scm_new_procedure ("project-attached?", (FN) tmg_project_attachedP, 0, 0, 0);
  scm_new_procedure ("get-project-buffer-menu", (FN) tmg_get_project_buffer_menu, 0, 0, 0);
  scm_new_procedure ("window-current", (FN) tmg_window_current, 0, 0, 0);
  scm_new_procedure ("window-list", (FN) tmg_window_list, 0, 0, 0);
  scm_new_procedure ("buffer->windows", (FN) tmg_buffer_2windows, 1, 0, 0);
  scm_new_procedure ("window->buffer", (FN) tmg_window_2buffer, 1, 0, 0);
  scm_new_procedure ("window-set-buffer", (FN) tmg_window_set_buffer, 2, 0, 0);
  scm_new_procedure ("window-focus", (FN) tmg_window_focus, 1, 0, 0);
  scm_new_procedure ("texmacs-load-tree", (FN) tmg_texmacs_load_tree, 2, 0, 0);
  scm_new_procedure ("texmacs-load-buffer", (FN) tmg_texmacs_load_buffer, 4, 0, 0);
  scm_new_procedure ("texmacs-save-buffer", (FN) tmg_texmacs_save_buffer, 2, 0, 0);
  scm_new_procedure ("auto-save", (FN) tmg_auto_save, 0, 0, 0);
  scm_new_procedure ("get-style-menu", (FN) tmg_get_style_menu, 0, 0, 0);
  scm_new_procedure ("get-add-package-menu", (FN) tmg_get_add_package_menu, 0, 0, 0);
  scm_new_procedure ("get-remove-package-menu", (FN) tmg_get_remove_package_menu, 0, 0, 0);
  scm_new_procedure ("style-clear-cache", (FN) tmg_style_clear_cache, 0, 0, 0);
  scm_new_procedure ("set-script-status", (FN) tmg_set_script_status, 1, 0, 0);
  scm_new_procedure ("set-printing-command", (FN) tmg_set_printing_command, 1, 0, 0);
  scm_new_procedure ("set-printer-paper-type", (FN) tmg_set_printer_paper_type, 1, 0, 0);
  scm_new_procedure ("get-printer-paper-type", (FN) tmg_get_printer_paper_type, 0, 0, 0);
  scm_new_procedure ("set-printer-dpi", (FN) tmg_set_printer_dpi, 1, 0, 0);
  scm_new_procedure ("set-default-shrinking-factor", (FN) tmg_set_default_shrinking_factor, 1, 0, 0);
  scm_new_procedure ("get-default-shrinking-factor", (FN) tmg_get_default_shrinking_factor, 0, 0, 0);
  scm_new_procedure ("get-nr-windows", (FN) tmg_get_nr_windows, 0, 0, 0);
  scm_new_procedure ("image-gc", (FN) tmg_image_gc, 0, 0, 0);
  scm_new_procedure ("inclusions-gc", (FN) tmg_inclusions_gc, 0, 0, 0);
  scm_new_procedure ("update-all-path", (FN) tmg_update_all_path, 1, 0, 0);
  scm_new_procedure ("update-all-buffers", (FN) tmg_update_all_buffers, 0, 0, 0);
  scm_new_procedure ("set-message", (FN) tmg_set_message, 2, 0, 0);
  scm_new_procedure ("set-message-temp", (FN) tmg_set_message_temp, 3, 0, 0);
  scm_new_procedure ("recall-message", (FN) tmg_recall_message, 0, 0, 0);
  scm_new_procedure ("yes?", (FN) tmg_yesP, 1, 0, 0);
  scm_new_procedure ("quit-TeXmacs", (FN) tmg_quit_TeXmacs, 0, 0, 0);
}
