
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
  (kbd-post-rewrite kbd_post_rewrite (string string bool))
  (kbd-system-rewrite kbd_system_rewrite (tree string))
  (set-font-rules set_font_rules (void scheme_tree))

  (window-get-id get_window_id (int))
  (window-set-property set_window_property (void scheme_tree scheme_tree))
  (window-get-property get_window_property (scheme_tree scheme_tree))
  (show-header show_header (void bool))
  (show-icon-bar show_icon_bar (void int bool))
  (show-side-tools show_side_tools (void int bool))
  (show-footer show_footer (void bool))
  (visible-header? visible_header (bool))
  (visible-icon-bar? visible_icon_bar (bool int))
  (visible-side-tools? visible_side_tools (bool int))
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
  (set-message set_message (void content content))
  (set-message-temp set_message (void content content bool))
  (recall-message recall_message (void))
  (yes? is_yes (bool string))
  (quit-TeXmacs quit (void)))
