
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : build-glue-server.scm
;; DESCRIPTION : Building basic glue for the server
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(output-copyright "build-glue-server.scm")

(build
  "get_server()->"
  "initialize_glue_server"

  (insert-kbd-wildcard insert_kbd_wildcard (void string string bool bool bool))
  (set-variant-keys set_variant_keys (void string string))
  (kbd-pre-rewrite kbd_pre_rewrite (string string))
  (kbd-post-rewrite kbd_post_rewrite (string string))
  (set-font-rules set_font_rules (void scheme_tree))

  (window-get-id get_window_id (int))
  (window-set-property set_window_property (void scheme_tree scheme_tree))
  (window-get-property get_window_property (scheme_tree scheme_tree))
  (show-header show_header (void bool))
  (show-icon-bar show_icon_bar (void int bool))
  (show-footer show_footer (void bool))
  (visible-header? visible_header (bool))
  (visible-icon-bar? visible_icon_bar (bool int))
  (visible-footer? visible_footer (bool))
  (full-screen-mode full_screen_mode (void bool bool))
  (full-screen? in_full_screen_mode (bool))
  (full-screen-edit? in_full_screen_edit_mode (bool))
  (set-shrinking-factor set_shrinking_factor (void int))
  (get-shrinking-factor get_shrinking_factor (int))

  (shell shell (void string))
  (dialogue-end dialogue_end (void))
  (choose-file choose_file (void object string string))
  (tm-interactive interactive (void object scheme_tree))

  (has-view? has_view (bool))
  (buffer-unsaved? buffer_unsaved (bool))
  (exists-unsaved-buffer? exists_unsaved_buffer (bool))
  (pretend-save-buffer pretend_save_buffer (void))
  (set-name-buffer set_name_buffer (void url))
  (get-name-buffer get_name_buffer (url))
  (get-name-buffer-path get_name_buffer (url path))
  (set-abbr-buffer set_abbr_buffer (void string))
  (get-abbr-buffer get_abbr_buffer (string))
  (new-buffer new_buffer (url))
  (switch-to-buffer-path switch_to_buffer (bool path))
  (switch-to-buffer switch_to_buffer (void url))
  (switch-to-active-buffer switch_to_active_buffer (void url))
  (revert-buffer revert_buffer (void))
  (kill-buffer kill_buffer (void))
  (open-buffer-in-window new_buffer_in_new_window (void url content content))
  (open-window open_window (url))
  (open-window-geometry open_window (url content))
  (clone-window clone_window (void))
  (kill-window kill_window (void))
  (kill-window-and-buffer kill_window_and_buffer (void))
  (no-name? no_name (bool))
  (help-buffer? help_buffer (bool))
  (set-buffer revert_buffer (void url content))
  (set-aux set_aux (void string url))
  (set-aux-buffer set_aux_buffer (void string url content))
  (set-help-buffer set_help_buffer (void url content))
  (set-buffer-tree set_buffer_tree (void url content))
  (get-buffer-tree get_buffer_tree (tree url))
  (get-all-buffers get_all_buffers (url))
  (get-buffer-menu get_buffer_menu (object))
  (buffer-in-menu buffer_in_menu (bool url bool))

  (project-attach project_attach (void string))
  (project-detach project_attach (void))
  (project-attached? project_attached (bool))
  (get-project-buffer-menu get_project_buffer_menu (object))

  (window-current window_current (int))
  (window-list windows_list (path))
  (buffer->windows buffer_to_windows (path url))
  (window->buffer window_to_buffer (url int))
  (window-set-buffer window_set_buffer (void int url))
  (window-focus window_focus (void int))

  (texmacs-load-tree load_tree (tree url string))
  (texmacs-load-buffer load_buffer (void url string int bool))
  (texmacs-save-buffer save_buffer (void url string))
  (auto-save auto_save (void))
  
  (get-style-menu get_style_menu (object))
  (get-add-package-menu get_add_package_menu (object))
  (get-remove-package-menu get_remove_package_menu (object))
  (style-clear-cache style_clear_cache (void))
  (set-script-status set_script_status (void int))
  (set-printing-command set_printing_command (void string))
  (set-printer-paper-type set_printer_page_type (void string))
  (get-printer-paper-type get_printer_page_type (string))
  (set-printer-dpi set_printer_dpi (void string))
  (set-default-shrinking-factor set_default_shrinking_factor (void int))
  (get-default-shrinking-factor get_default_shrinking_factor (int))
  (get-nr-windows get_nr_windows (int))
  (image-gc image_gc (void))
  (inclusions-gc inclusions_gc (void))
  (update-all-path typeset_update (void path))
  (update-all-buffers typeset_update_all (void))
  (set-message set_message (void string string))
  (set-message-temp set_message (void string string bool))
  (recall-message recall_message (void))
  (yes? is_yes (bool string))
  (quit-TeXmacs quit (void)))
