<TeXmacs|1.99.4>

<style|<tuple|tmdoc|english>>

<\body>
<tmdoc-title|All glue functions>

This document lists all available <scheme> functions that are implemented in
the <c++> code and which, consequently, are neither defined nor documented in the
<scheme> modules. Ideally each of these functions should be documented
elsewhere in the documentation.

This document was generated automatically from the glue code definitions by
the script <verbatim|src/src/Scheme/Glue/make-apidoc-doc.scm> in <TeXmacs>
source code.

\;

\;

  <\explain>
    <scm|(texmacs-version-release <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|texmacs_version> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(version-before? <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|version_inf> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(updater-supported?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|updater_supported> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(updater-running?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|updater_is_running> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(updater-check-background)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|updater_check_background> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(updater-check-foreground)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|updater_check_foreground> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(updater-last-check)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|updater_last_check> which returns
    <scm|long>.
  </explain>

  <\explain>
    <scm|(updater-set-interval <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|updater_set_interval> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(get-original-path)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_original_path> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(os-win32?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|os_win32> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(os-mingw?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|os_mingw> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(os-macos?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|os_macos> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(has-printing-cmd?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|has_printing_cmd> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(x-gui?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|gui_is_x> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(qt-gui?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|gui_is_qt> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(gui-version)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|gui_version> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(default-look-and-feel)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|default_look_and_feel> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(default-chinese-font)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|default_chinese_font_name> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(default-japanese-font)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|default_japanese_font_name> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(default-korean-font)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|default_korean_font_name> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(get-retina-factor)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_retina_factor> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(get-retina-zoom)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_retina_zoom> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(get-retina-icons)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_retina_icons> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(get-retina-scale)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_retina_scale> which returns
    <scm|double>.
  </explain>

  <\explain>
    <scm|(set-retina-factor <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_retina_factor> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(set-retina-zoom <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_retina_zoom> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(set-retina-icons <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_retina_icons> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(set-retina-scale <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_retina_scale> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(tm-output <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tm_output> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(tm-errput <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tm_errput> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(win32-display <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|win32_display> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-error)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|cpp_error> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(supports-native-pdf?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|supports_native_pdf> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(supports-ghostscript?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|supports_ghostscript> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(rescue-mode?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|in_rescue_mode> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(scheme-dialect)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|scheme_dialect> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(get-texmacs-path)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_texmacs_path> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(get-texmacs-home-path)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_texmacs_home_path> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(get-user-login)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_user_login> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(get-user-name)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_user_name> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(plugin-list)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|plugin_list> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(set-fast-environments <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_fast_environments> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(font-exists-in-tt? <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tt_font_exists> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(eval-system <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|eval_system> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(var-eval-system <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|var_eval_system> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(evaluate-system <scm-arg|array_string> <scm-arg|array_int> <scm-arg|array_string> <scm-arg|array_int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|evaluate_system> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(get-locale-language)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_locale_language> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(get-locale-charset)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_locale_charset> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(locale-to-language <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|locale_to_language> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(language-to-locale <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|language_to_locale> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(texmacs-time)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|texmacs_time> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(pretty-time <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|pretty_time> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(texmacs-memory)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|mem_used> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(bench-print <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|bench_print> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(bench-print-all)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|bench_print> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(system-wait <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|system_wait> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(get-show-kbd)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_show_kbd> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(set-show-kbd <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_show_kbd> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(set-latex-command <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_latex_command> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(set-bibtex-command <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_bibtex_command> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(number-latex-errors <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|number_latex_errors> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(number-latex-pages <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|number_latex_pages> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(math-symbol-group <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|math_symbol_group> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(math-group-members <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|math_group_members> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(math-symbol-type <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|math_symbol_type> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(object-\<gtr\>command <scm-arg|object>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|as_command> which returns
    <scm|command>.
  </explain>

  <\explain>
    <scm|(command-eval <scm-arg|command>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|eval> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(command-apply <scm-arg|command> <scm-arg|object>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|apply> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(exec-delayed <scm-arg|object>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|exec_delayed> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(exec-delayed-pause <scm-arg|object>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|exec_delayed_pause> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(protected-call <scm-arg|object>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|protected_call> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(notify-preferences-booted)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|notify_preferences_booted> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-has-preference? <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|has_user_preference> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(cpp-get-preference <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_user_preference> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(cpp-set-preference <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_user_preference> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-reset-preference <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|reset_user_preference> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(save-preferences)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|save_user_preferences> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(get-default-printing-command)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_printing_default> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(set-input-language <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_input_language> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(get-input-language)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_input_language> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(set-output-language <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|gui_set_output_language> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(get-output-language)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_output_language> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(translate <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|translate> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(string-translate <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|translate_as_is> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(translate-from-to <scm-arg|content> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|translate> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tree-translate <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_translate> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-translate-from-to <scm-arg|content> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_translate> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(force-load-translations <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|force_load_dictionary> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(color <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|named_color> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(get-hex-color <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_hex_color> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(named-color-\<gtr\>xcolormap <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|named_color_to_xcolormap> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(rgba-\<gtr\>named-color <scm-arg|array_int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|named_rgb_color> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(named-color-\<gtr\>rgba <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_named_rgb_color> which returns
    <scm|array_int>.
  </explain>

  <\explain>
    <scm|(new-author)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|new_author> which returns
    <scm|double>.
  </explain>

  <\explain>
    <scm|(set-author <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_author> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(get-author)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_author> which returns
    <scm|double>.
  </explain>

  <\explain>
    <scm|(debug-set <scm-arg|string> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|debug_set> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(debug-get <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|debug_get> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(debug-message <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|debug_message> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(get-debug-messages <scm-arg|string> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_debug_messages> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(clear-debug-messages)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|clear_debug_messages> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cout-buffer)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|cout_buffer> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cout-unbuffer)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|cout_unbuffer> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(mark-new)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|new_marker> which returns
    <scm|double>.
  </explain>

  <\explain>
    <scm|(glyph-register <scm-arg|string> <scm-arg|array_array_array_double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|register_glyph> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(glyph-recognize <scm-arg|array_array_array_double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|recognize_glyph> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(set-new-fonts <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_new_fonts> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(new-fonts?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_new_fonts> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tmtm-eqnumber-\<gtr\>nonumber <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|eqnumber_to_nonumber> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(busy-versioning?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_busy_versioning> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(players-set-elapsed <scm-arg|tree> <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|players_set_elapsed> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(players-set-speed <scm-arg|tree> <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|players_set_speed> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(apply-effect <scm-arg|content> <scm-arg|array_url> <scm-arg|url> <scm-arg|int> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|apply_effect> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(tt-exists? <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tt_font_exists> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tt-dump <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tt_dump> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(tt-font-name <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tt_font_name> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(tt-analyze <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tt_analyze> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(font-database-build <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|font_database_build> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(font-database-build-local)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|font_database_build_local> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(font-database-extend-local <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|font_database_extend_local> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(font-database-build-global)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|font_database_build_global> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(font-database-build-characteristics <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|font_database_build_characteristics> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(font-database-insert-global <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|font_database_build_global> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(font-database-save-local-delta)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|font_database_save_local_delta> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(font-database-load)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|font_database_load> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(font-database-save)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|font_database_save> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(font-database-filter)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|font_database_filter> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(font-database-families)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|font_database_families> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(font-database-delta-families)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|font_database_delta_families> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(font-database-styles <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|font_database_styles> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(font-database-search <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|font_database_search> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(font-database-characteristics <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|font_database_characteristics> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(font-database-substitutions <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|font_database_substitutions> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(font-family-\<gtr\>master <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|family_to_master> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(font-master-\<gtr\>families <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|master_to_families> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(font-master-features <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|master_features> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(font-family-features <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|family_features> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(font-family-strict-features <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|family_strict_features> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(font-style-features <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|style_features> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(font-guessed-features <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|guessed_features> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(font-guessed-distance <scm-arg|string> <scm-arg|string> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|guessed_distance> which returns
    <scm|double>.
  </explain>

  <\explain>
    <scm|(font-master-guessed-distance <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|guessed_distance> which returns
    <scm|double>.
  </explain>

  <\explain>
    <scm|(font-family-guessed-features <scm-arg|string> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|guessed_features> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(characteristic-distance <scm-arg|array_string> <scm-arg|array_string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|characteristic_distance> which returns
    <scm|double>.
  </explain>

  <\explain>
    <scm|(trace-distance <scm-arg|string> <scm-arg|string> <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|trace_distance> which returns
    <scm|double>.
  </explain>

  <\explain>
    <scm|(logical-font-public <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|logical_font> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(logical-font-exact <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|logical_font_exact> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(logical-font-private <scm-arg|string> <scm-arg|string> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|logical_font> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(logical-font-family <scm-arg|array_string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_family> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(logical-font-variant <scm-arg|array_string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_variant> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(logical-font-series <scm-arg|array_string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_series> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(logical-font-shape <scm-arg|array_string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_shape> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(logical-font-search <scm-arg|array_string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|search_font> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(logical-font-search-exact <scm-arg|array_string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|search_font_exact> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(search-font-families <scm-arg|array_string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|search_font_families> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(search-font-styles <scm-arg|string> <scm-arg|array_string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|search_font_styles> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(logical-font-patch <scm-arg|array_string> <scm-arg|array_string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|patch_font> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(logical-font-substitute <scm-arg|array_string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|apply_substitutions> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(font-family-main <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|main_family> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(image-\<gtr\>psdoc <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|image_to_psdoc> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(anim-control-times <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_control_times> which returns
    <scm|array_double>.
  </explain>

  <\explain>
    <scm|(tree-\<gtr\>stree <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_to_scheme_tree> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(stree-\<gtr\>tree <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|scheme_tree_to_tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-\<gtr\>string <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|coerce_tree_string> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(string-\<gtr\>tree <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|coerce_string_tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tm-\<gtr\>tree <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-atomic? <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_atomic> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tree-compound? <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_compound> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tree-label <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|L> which returns
    <scm|tree_label>.
  </explain>

  <\explain>
    <scm|(tree-children <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|A> which returns
    <scm|array_tree>.
  </explain>

  <\explain>
    <scm|(tree-arity <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|N> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(tree-child-ref <scm-arg|tree> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_ref> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-child-set! <scm-arg|tree> <scm-arg|int> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_set> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-child-insert <scm-arg|content> <scm-arg|int> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_child_insert> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-ip <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|obtain_ip> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(tree-active? <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_active> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tree-eq? <scm-arg|tree> <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|strong_equal> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(subtree <scm-arg|tree> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|subtree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-range <scm-arg|tree> <scm-arg|int> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_range> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-copy <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|copy> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-append <scm-arg|tree> <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_append> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-right-index <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|right_index> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(tree-label-extension? <scm-arg|tree_label>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_extension> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tree-label-macro? <scm-arg|tree_label>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_macro> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tree-label-parameter? <scm-arg|tree_label>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_parameter> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tree-label-type <scm-arg|tree_label>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_tag_type> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tree-multi-paragraph? <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_multi_paragraph> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tree-simplify <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|simplify_correct> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-minimal-arity <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|minimal_arity> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(tree-maximal-arity <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|maximal_arity> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(tree-possible-arity? <scm-arg|tree> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|correct_arity> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tree-insert_point <scm-arg|tree> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|insert_point> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(tree-is-dynamic? <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_dynamic> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tree-accessible-child? <scm-arg|tree> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_accessible_child> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tree-accessible-children <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|accessible_children> which returns
    <scm|array_tree>.
  </explain>

  <\explain>
    <scm|(tree-all-accessible? <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|all_accessible> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tree-none-accessible? <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|none_accessible> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tree-name <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_name> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tree-long-name <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_long_name> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tree-child-name <scm-arg|content> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_child_name> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tree-child-long-name <scm-arg|content> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_child_long_name> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tree-child-type <scm-arg|content> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_child_type> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tree-child-env* <scm-arg|content> <scm-arg|int> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_env_child> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-child-env <scm-arg|content> <scm-arg|int> <scm-arg|string> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_env_child> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-descendant-env* <scm-arg|content> <scm-arg|path> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_env_descendant> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-descendant-env <scm-arg|content> <scm-arg|path> <scm-arg|string> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_env_descendant> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-load-inclusion <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|load_inclusion> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-as-string <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_as_string> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tree-extents <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_extents> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-empty? <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_empty> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tree-multi-line? <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_multi_line> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tree-is-buffer? <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|admits_edit_observer> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tree-search-sections <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|search_sections> which returns
    <scm|array_tree>.
  </explain>

  <\explain>
    <scm|(tree-search-tree <scm-arg|content> <scm-arg|content> <scm-arg|path> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|search> which returns
    <scm|array_path>.
  </explain>

  <\explain>
    <scm|(tree-search-tree-at <scm-arg|content> <scm-arg|content> <scm-arg|path> <scm-arg|path> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|search> which returns
    <scm|array_path>.
  </explain>

  <\explain>
    <scm|(tree-spell <scm-arg|string> <scm-arg|content> <scm-arg|path> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|spell> which returns
    <scm|array_path>.
  </explain>

  <\explain>
    <scm|(tree-spell-at <scm-arg|string> <scm-arg|content> <scm-arg|path> <scm-arg|path> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|spell> which returns
    <scm|array_path>.
  </explain>

  <\explain>
    <scm|(tree-spell-selection <scm-arg|string> <scm-arg|content> <scm-arg|path> <scm-arg|path> <scm-arg|path> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|spell> which returns
    <scm|array_path>.
  </explain>

  <\explain>
    <scm|(previous-search-hit <scm-arg|array_path> <scm-arg|path> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|previous_search_hit> which returns
    <scm|array_path>.
  </explain>

  <\explain>
    <scm|(next-search-hit <scm-arg|array_path> <scm-arg|path> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|next_search_hit> which returns
    <scm|array_path>.
  </explain>

  <\explain>
    <scm|(navigate-search-hit <scm-arg|path> <scm-arg|bool> <scm-arg|bool> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|navigate_search_hit> which returns
    <scm|array_path>.
  </explain>

  <\explain>
    <scm|(tag-minimal-arity <scm-arg|tree_label>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|minimal_arity> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(tag-maximal-arity <scm-arg|tree_label>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|maximal_arity> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(tag-possible-arity? <scm-arg|tree_label> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|correct_arity> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(set-access-mode <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_access_mode> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(get-access-mode)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_access_mode> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(tree-assign <scm-arg|tree> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_assign> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-var-insert <scm-arg|tree> <scm-arg|int> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_insert> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-remove <scm-arg|tree> <scm-arg|int> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_remove> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-split <scm-arg|tree> <scm-arg|int> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_split> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-join <scm-arg|tree> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_join> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-assign-node <scm-arg|tree> <scm-arg|tree_label>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_assign_node> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-insert-node <scm-arg|tree> <scm-arg|int> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_insert_node> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-remove-node <scm-arg|tree> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_remove_node> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(cpp-tree-correct-node <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|correct_node> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-tree-correct-downwards <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|correct_downwards> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-tree-correct-upwards <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|correct_upwards> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(concat-tokenize-math <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|concat_tokenize> which returns
    <scm|array_tree>.
  </explain>

  <\explain>
    <scm|(concat-decompose <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|concat_decompose> which returns
    <scm|array_tree>.
  </explain>

  <\explain>
    <scm|(concat-recompose <scm-arg|array_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|concat_recompose> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(with-like? <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_with_like> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(with-same-type? <scm-arg|content> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|with_same_type> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(with-similar-type? <scm-arg|content> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|with_similar_type> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(with-correct <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|with_correct> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(with-correct-superfluous <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|superfluous_with_correct> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(invisible-correct-superfluous <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|superfluous_invisible_correct> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(invisible-correct-missing <scm-arg|content> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|missing_invisible_correct> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(automatic-correct <scm-arg|content> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|automatic_correct> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(manual-correct <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|manual_correct> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-upgrade-brackets <scm-arg|content> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|upgrade_brackets> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-upgrade-big <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|upgrade_big> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-downgrade-brackets <scm-arg|content> <scm-arg|bool> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|downgrade_brackets> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-downgrade-big <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|downgrade_big> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(math-status-print)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|math_status_print> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(math-status-reset)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|math_status_reset> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(math-stats-compile <scm-arg|string> <scm-arg|content> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|compile_stats> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(math-stats-occurrences <scm-arg|string> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|number_occurrences> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(math-stats-number-in-role <scm-arg|string> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|number_in_role> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(path-strip <scm-arg|path> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|strip> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(path-inf? <scm-arg|path> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|path_inf> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(path-inf-eq? <scm-arg|path> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|path_inf_eq> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(path-less? <scm-arg|path> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|path_less> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(path-less-eq? <scm-arg|path> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|path_less_eq> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(path-start <scm-arg|content> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|start> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(path-end <scm-arg|content> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|end> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(path-next <scm-arg|content> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|next_valid> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(path-previous <scm-arg|content> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|previous_valid> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(path-next-word <scm-arg|content> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|next_word> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(path-previous-word <scm-arg|content> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|previous_word> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(path-next-node <scm-arg|content> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|next_node> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(path-previous-node <scm-arg|content> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|previous_node> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(path-next-tag <scm-arg|content> <scm-arg|path> <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|next_tag> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(path-previous-tag <scm-arg|content> <scm-arg|path> <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|previous_tag> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(path-next-tag-same-argument <scm-arg|content> <scm-arg|path> <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|next_tag_same_argument> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(path-previous-tag-same-argument <scm-arg|content> <scm-arg|path> <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|previous_tag_same_argument> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(path-next-argument <scm-arg|content> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|next_argument> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(path-previous-argument <scm-arg|content> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|previous_argument> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(path-previous-section <scm-arg|content> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|previous_section> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(make-modification <scm-arg|string> <scm-arg|path> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_modification> which returns
    <scm|modification>.
  </explain>

  <\explain>
    <scm|(modification-assign <scm-arg|path> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|mod_assign> which returns
    <scm|modification>.
  </explain>

  <\explain>
    <scm|(modification-insert <scm-arg|path> <scm-arg|int> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|mod_insert> which returns
    <scm|modification>.
  </explain>

  <\explain>
    <scm|(modification-remove <scm-arg|path> <scm-arg|int> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|mod_remove> which returns
    <scm|modification>.
  </explain>

  <\explain>
    <scm|(modification-split <scm-arg|path> <scm-arg|int> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|mod_split> which returns
    <scm|modification>.
  </explain>

  <\explain>
    <scm|(modification-join <scm-arg|path> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|mod_join> which returns
    <scm|modification>.
  </explain>

  <\explain>
    <scm|(modification-assign-node <scm-arg|path> <scm-arg|tree_label>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|mod_assign_node> which returns
    <scm|modification>.
  </explain>

  <\explain>
    <scm|(modification-insert-node <scm-arg|path> <scm-arg|int> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|mod_insert_node> which returns
    <scm|modification>.
  </explain>

  <\explain>
    <scm|(modification-remove-node <scm-arg|path> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|mod_remove_node> which returns
    <scm|modification>.
  </explain>

  <\explain>
    <scm|(modification-set-cursor <scm-arg|path> <scm-arg|int> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|mod_set_cursor> which returns
    <scm|modification>.
  </explain>

  <\explain>
    <scm|(modification-kind <scm-arg|modification>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_type> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(modification-path <scm-arg|modification>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_path> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(modification-tree <scm-arg|modification>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(modification-root <scm-arg|modification>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|root> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(modification-index <scm-arg|modification>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|index> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(modification-argument <scm-arg|modification>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|argument> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(modification-label <scm-arg|modification>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|L> which returns
    <scm|tree_label>.
  </explain>

  <\explain>
    <scm|(modification-copy <scm-arg|modification>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|copy> which returns
    <scm|modification>.
  </explain>

  <\explain>
    <scm|(modification-applicable? <scm-arg|content> <scm-arg|modification>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_applicable> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(modification-apply <scm-arg|content> <scm-arg|modification>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|var_clean_apply> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(modification-inplace-apply <scm-arg|tree> <scm-arg|modification>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|var_apply> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(modification-invert <scm-arg|modification> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|invert> which returns
    <scm|modification>.
  </explain>

  <\explain>
    <scm|(modification-commute? <scm-arg|modification> <scm-arg|modification>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|commute> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(modification-can-pull? <scm-arg|modification> <scm-arg|modification>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|can_pull> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(modification-pull <scm-arg|modification> <scm-arg|modification>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|pull> which returns
    <scm|modification>.
  </explain>

  <\explain>
    <scm|(modification-co-pull <scm-arg|modification> <scm-arg|modification>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|co_pull> which returns
    <scm|modification>.
  </explain>

  <\explain>
    <scm|(patch-pair <scm-arg|modification> <scm-arg|modification>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|patch> which returns
    <scm|patch>.
  </explain>

  <\explain>
    <scm|(patch-compound <scm-arg|array_patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|patch> which returns
    <scm|patch>.
  </explain>

  <\explain>
    <scm|(patch-branch <scm-arg|array_patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|branch_patch> which returns
    <scm|patch>.
  </explain>

  <\explain>
    <scm|(patch-birth <scm-arg|double> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|patch> which returns
    <scm|patch>.
  </explain>

  <\explain>
    <scm|(patch-author <scm-arg|double> <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|patch> which returns
    <scm|patch>.
  </explain>

  <\explain>
    <scm|(patch-pair? <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_modification> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(patch-compound? <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_compound> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(patch-branch? <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_branch> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(patch-birth? <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_birth> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(patch-author? <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_author> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(patch-arity <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|N> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(patch-ref <scm-arg|patch> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|access> which returns
    <scm|patch>.
  </explain>

  <\explain>
    <scm|(patch-direct <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_modification> which returns
    <scm|modification>.
  </explain>

  <\explain>
    <scm|(patch-inverse <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_inverse> which returns
    <scm|modification>.
  </explain>

  <\explain>
    <scm|(patch-get-birth <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_birth> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(patch-get-author <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_author> which returns
    <scm|double>.
  </explain>

  <\explain>
    <scm|(patch-copy <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|copy> which returns
    <scm|patch>.
  </explain>

  <\explain>
    <scm|(patch-applicable? <scm-arg|patch> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_applicable> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(patch-apply <scm-arg|content> <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|var_clean_apply> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(patch-inplace-apply <scm-arg|tree> <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|var_apply> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(patch-compactify <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|compactify> which returns
    <scm|patch>.
  </explain>

  <\explain>
    <scm|(patch-cursor-hint <scm-arg|patch> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|cursor_hint> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(patch-invert <scm-arg|patch> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|invert> which returns
    <scm|patch>.
  </explain>

  <\explain>
    <scm|(patch-commute? <scm-arg|patch> <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|commute> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(patch-can-pull? <scm-arg|patch> <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|can_pull> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(patch-pull <scm-arg|patch> <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|pull> which returns
    <scm|patch>.
  </explain>

  <\explain>
    <scm|(patch-co-pull <scm-arg|patch> <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|co_pull> which returns
    <scm|patch>.
  </explain>

  <\explain>
    <scm|(patch-remove-set-cursor <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|remove_set_cursor> which returns
    <scm|patch>.
  </explain>

  <\explain>
    <scm|(patch-modifies? <scm-arg|patch>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|does_modify> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tree-\<gtr\>ids <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_ids> which returns
    <scm|list_string>.
  </explain>

  <\explain>
    <scm|(id-\<gtr\>trees <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_trees> which returns
    <scm|list_tree>.
  </explain>

  <\explain>
    <scm|(vertex-\<gtr\>links <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_links> which returns
    <scm|list_tree>.
  </explain>

  <\explain>
    <scm|(tree-\<gtr\>tree-pointer <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_pointer_new> which returns
    <scm|observer>.
  </explain>

  <\explain>
    <scm|(tree-pointer-detach <scm-arg|observer>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_pointer_delete> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(tree-pointer-\<gtr\>tree <scm-arg|observer>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|obtain_tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(current-link-types)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|all_link_types> which returns
    <scm|list_string>.
  </explain>

  <\explain>
    <scm|(get-locus-rendering <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_locus_rendering> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(set-locus-rendering <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_locus_rendering> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(declare-visited <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|declare_visited> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(has-been-visited? <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|has_been_visited> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(graphics-set <scm-arg|content> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_graphical_value> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(graphics-has? <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|has_graphical_value> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(graphics-ref <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_graphical_value> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(graphics-needs-update?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|graphics_needs_update> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(graphics-notify-update <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|graphics_notify_update> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-string-number? <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_double> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(string-occurs? <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|occurs> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(string-count-occurrences <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|count_occurrences> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(string-search-forwards <scm-arg|string> <scm-arg|int> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|search_forwards> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(string-search-backwards <scm-arg|string> <scm-arg|int> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|search_backwards> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(string-overlapping <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|overlapping> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(string-replace <scm-arg|string> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|replace> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(string-find-non-alpha <scm-arg|string> <scm-arg|int> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|find_non_alpha> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(string-alpha? <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_alpha> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(string-locase-alpha? <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_locase_alpha> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(upcase-first <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|upcase_first> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(locase-first <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|locase_first> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(upcase-all <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|upcase_all> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(locase-all <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|locase_all> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(string-union <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|string_union> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(string-minus <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|string_minus> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(escape-generic <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|escape_generic> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(escape-verbatim <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|escape_verbatim> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(escape-shell <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|escape_sh> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(escape-to-ascii <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|cork_to_ascii> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(unescape-guile <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|unescape_guile> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(string-quote <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|scm_quote> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(string-unquote <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|scm_unquote> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(string-trim-spaces-left <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|trim_spaces_left> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(string-trim-spaces-right <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|trim_spaces_right> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(string-trim-spaces <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|trim_spaces> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(downgrade-math-letters <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|downgrade_math_letters> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(string-convert <scm-arg|string> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|convert> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(encode-base64 <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|encode_base64> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(decode-base64 <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|decode_base64> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(sourcecode-\<gtr\>cork <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|sourcecode_to_cork> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(cork-\<gtr\>sourcecode <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|cork_to_sourcecode> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(utf8-\<gtr\>cork <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|utf8_to_cork> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(cork-\<gtr\>utf8 <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|cork_to_utf8> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(utf8-\<gtr\>t2a <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|utf8_to_t2a> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(t2a-\<gtr\>utf8 <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|t2a_to_utf8> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(utf8-\<gtr\>html <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|utf8_to_html> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(guess-wencoding <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|guess_wencoding> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tm-\<gtr\>xml-name <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tm_to_xml_name> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(old-tm-\<gtr\>xml-cdata <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|old_tm_to_xml_cdata> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tm-\<gtr\>xml-cdata <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tm_to_xml_cdata> which returns
    <scm|object>.
  </explain>

  <\explain>
    <scm|(xml-name-\<gtr\>tm <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|xml_name_to_tm> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(old-xml-cdata-\<gtr\>tm <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|old_xml_cdata_to_tm> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(xml-unspace <scm-arg|string> <scm-arg|bool> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|xml_unspace> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(integer-\<gtr\>hexadecimal <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|as_hexadecimal> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(integer-\<gtr\>padded-hexadecimal <scm-arg|int> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|as_hexadecimal> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(hexadecimal-\<gtr\>integer <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|from_hexadecimal> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(cpp-string-tokenize <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tokenize> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(cpp-string-recompose <scm-arg|array_string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|recompose> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(string-differences <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|differences> which returns
    <scm|array_int>.
  </explain>

  <\explain>
    <scm|(string-distance <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|distance> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(find-left-bracket <scm-arg|path> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|find_left_bracket> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(find-right-bracket <scm-arg|path> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|find_right_bracket> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(string-\<gtr\>tmstring <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tm_encode> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tmstring-\<gtr\>string <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tm_decode> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tmstring-length <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tm_string_length> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(tmstring-ref <scm-arg|string> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tm_forward_access> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tmstring-reverse-ref <scm-arg|string> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tm_backward_access> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tmstring-\<gtr\>list <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tm_tokenize> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(list-\<gtr\>tmstring <scm-arg|array_string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tm_recompose> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(string-next <scm-arg|string> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tm_char_next> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(string-previous <scm-arg|string> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tm_char_previous> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(tmstring-split <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tm_string_split> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(tmstring-translit <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|uni_translit> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tmstring-locase-first <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|uni_locase_first> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tmstring-upcase-first <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|uni_upcase_first> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tmstring-locase-all <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|uni_locase_all> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tmstring-upcase-all <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|uni_upcase_all> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tmstring-unaccent-all <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|uni_unaccent_all> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tmstring-letter? <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|uni_is_letter> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tmstring-before? <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|uni_before> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(multi-spell-start)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|spell_start> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(multi-spell-done)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|spell_done> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(single-spell-start <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|spell_start> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(single-spell-done <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|spell_done> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(spell-check <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|spell_check> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(spell-check? <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|check_word> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(spell-accept <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|spell_accept> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(spell-var-accept <scm-arg|string> <scm-arg|string> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|spell_accept> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(spell-insert <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|spell_insert> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(packrat-define <scm-arg|string> <scm-arg|string> <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|packrat_define> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(packrat-property <scm-arg|string> <scm-arg|string> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|packrat_property> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(packrat-inherit <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|packrat_inherit> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(packrat-parse <scm-arg|string> <scm-arg|string> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|packrat_parse> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(packrat-correct? <scm-arg|string> <scm-arg|string> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|packrat_correct> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(packrat-context <scm-arg|string> <scm-arg|string> <scm-arg|content> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|packrat_context> which returns
    <scm|object>.
  </explain>

  <\explain>
    <scm|(syntax-read-preferences <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|initialize_color_decodings> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(parse-texmacs <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|texmacs_document_to_tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(serialize-texmacs <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_to_texmacs> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(parse-texmacs-snippet <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|texmacs_to_tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(serialize-texmacs-snippet <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_to_texmacs> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(texmacs-\<gtr\>stm <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_to_scheme> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(stm-\<gtr\>texmacs <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|scheme_document_to_tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(stm-snippet-\<gtr\>texmacs <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|scheme_to_tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(cpp-texmacs-\<gtr\>verbatim <scm-arg|tree> <scm-arg|bool> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_to_verbatim> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(cpp-verbatim-snippet-\<gtr\>texmacs <scm-arg|string> <scm-arg|bool> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|verbatim_to_tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(cpp-verbatim-\<gtr\>texmacs <scm-arg|string> <scm-arg|bool> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|verbatim_document_to_tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(parse-latex <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|parse_latex> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(parse-latex-document <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|parse_latex_document> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(latex-\<gtr\>texmacs <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|latex_to_tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(cpp-latex-document-\<gtr\>texmacs <scm-arg|string> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|latex_document_to_tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(latex-class-document-\<gtr\>texmacs <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|latex_class_document_to_tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tracked-latex-\<gtr\>texmacs <scm-arg|string> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tracked_latex_to_texmacs> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(conservative-texmacs-\<gtr\>latex <scm-arg|content> <scm-arg|object>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|conservative_texmacs_to_latex> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(tracked-texmacs-\<gtr\>latex <scm-arg|content> <scm-arg|object>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tracked_texmacs_to_latex> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(conservative-latex-\<gtr\>texmacs <scm-arg|string> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|conservative_latex_to_texmacs> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(get-line-number <scm-arg|string> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_line_number> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(get-column-number <scm-arg|string> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_column_number> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(try-latex-export <scm-arg|content> <scm-arg|object> <scm-arg|url> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|try_latex_export> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(parse-xml <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|parse_xml> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(parse-html <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|parse_html> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(parse-bib <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|parse_bib> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(conservative-bib-import <scm-arg|string> <scm-arg|content> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|conservative_bib_import> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(conservative-bib-export <scm-arg|content> <scm-arg|string> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|conservative_bib_export> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(clean-html <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|clean_html> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(upgrade-tmml <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tmml_upgrade> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(upgrade-mathml <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|upgrade_mathml> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(retrieve-mathjax <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|retrieve_mathjax> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(vernac-\<gtr\>texmacs <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|vernac_to_tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(vernac-document-\<gtr\>texmacs <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|vernac_document_to_tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(compute-keys-string <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|compute_keys> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(compute-keys-tree <scm-arg|content> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|compute_keys> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(compute-keys-url <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|compute_keys> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(compute-index-string <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|compute_index> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(compute-index-tree <scm-arg|content> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|compute_index> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(compute-index-url <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|compute_index> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(url-\<gtr\>url <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|url> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(root-\<gtr\>url <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|url_root> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(string-\<gtr\>url <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|url> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-\<gtr\>string <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|as_string> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(url-\<gtr\>stree <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|as_tree> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(system-\<gtr\>url <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|url_system> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-\<gtr\>system <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|as_system_string> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(unix-\<gtr\>url <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|url_unix> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-\<gtr\>unix <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|as_unix_string> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(url-unix <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|url> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-none)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|url_none> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-any)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|url_wildcard> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-wildcard <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|url_wildcard> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-pwd)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|url_pwd> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-parent)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|url_parent> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-ancestor)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|url_ancestor> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-append <scm-arg|url> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|url_concat> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-or <scm-arg|url> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|url_or> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-none? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_none> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-rooted? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_rooted> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-rooted-protocol? <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_rooted> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-rooted-web? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_rooted_web> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-rooted-tmfs? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_rooted_tmfs> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-rooted-tmfs-protocol? <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_rooted_tmfs> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-root <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_root> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(url-unroot <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|unroot> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-atomic? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_atomic> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-concat? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_concat> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-or? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_or> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-ref <scm-arg|url> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|url_ref> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-head <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|head> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-tail <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tail> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-format <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|file_format> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(url-suffix <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|suffix> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(url-basename <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|basename> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(url-glue <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|glue> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-unglue <scm-arg|url> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|unglue> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-relative <scm-arg|url> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|relative> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-expand <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|expand> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-factor <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|factor> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-delta <scm-arg|url> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|delta> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-secure? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_secure> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-descends? <scm-arg|url> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|descends> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-complete <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|complete> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-resolve <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|resolve> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-resolve-in-path <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|resolve_in_path> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-resolve-pattern <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|resolve_pattern> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-exists? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|exists> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-exists-in-path? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|exists_in_path> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-exists-in-tex? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|exists_in_tex> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-concretize* <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|concretize_url> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-concretize <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|concretize> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(url-sys-concretize <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|sys_concretize> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(url-materialize <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|materialize> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(url-test? <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_of_type> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-regular? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_regular> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-directory? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_directory> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-link? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_symbolic_link> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-newer? <scm-arg|url> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_newer> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-size <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|file_size> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(url-last-modified <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|last_modified> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(url-temp)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|url_temp> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-temp-dir)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|url_temp_dir> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-scratch <scm-arg|string> <scm-arg|string> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|url_scratch> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-scratch? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_scratch> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(url-cache-invalidate <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|web_cache_invalidate> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(string-save <scm-arg|string> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|string_save> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(string-load <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|string_load> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(string-append-to-file <scm-arg|string> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|string_append_to_file> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(system-move <scm-arg|url> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|move> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(system-copy <scm-arg|url> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|copy> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(system-remove <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|remove> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(system-mkdir <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|mkdir> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(system-rmdir <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|rmdir> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(system-setenv <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_env> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(system-search-score <scm-arg|url> <scm-arg|array_string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|search_score> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(system-1 <scm-arg|string> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|system> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(system-2 <scm-arg|string> <scm-arg|url> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|system> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(system-url-\<gtr\>string <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|sys_concretize> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(url-grep <scm-arg|string> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|grep> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(url-search-upwards <scm-arg|url> <scm-arg|string> <scm-arg|array_string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|search_file_upwards> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(picture-cache-reset)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|picture_cache_reset> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(set-file-focus <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_file_focus> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(get-file-focus)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_file_focus> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(persistent-set <scm-arg|url> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|persistent_set> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(persistent-remove <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|persistent_reset> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(persistent-has? <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|persistent_contains> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(persistent-get <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|persistent_get> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(persistent-file-name <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|persistent_file_name> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(tmdb-keep-history <scm-arg|url> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|keep_history> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(tmdb-set-field <scm-arg|url> <scm-arg|string> <scm-arg|string> <scm-arg|array_string> <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_field> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(tmdb-get-field <scm-arg|url> <scm-arg|string> <scm-arg|string> <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_field> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(tmdb-remove-field <scm-arg|url> <scm-arg|string> <scm-arg|string> <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|remove_field> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(tmdb-get-attributes <scm-arg|url> <scm-arg|string> <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_attributes> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(tmdb-set-entry <scm-arg|url> <scm-arg|string> <scm-arg|scheme_tree> <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_entry> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(tmdb-get-entry <scm-arg|url> <scm-arg|string> <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_entry> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(tmdb-remove-entry <scm-arg|url> <scm-arg|string> <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|remove_entry> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(tmdb-query <scm-arg|url> <scm-arg|scheme_tree> <scm-arg|double> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|query> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(tmdb-inspect-history <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|inspect_history> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(tmdb-get-completions <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_completions> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(tmdb-get-name-completions <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_name_completions> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(supports-sql?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|sqlite3_present> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(sql-exec <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|sql_exec> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(sql-quote <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|sql_quote> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(server-start)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|server_start> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(server-stop)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|server_stop> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(server-read <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|server_read> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(server-write <scm-arg|int> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|server_write> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(server-started?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|server_started> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(client-start <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|client_start> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(client-stop <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|client_stop> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(client-read <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|client_read> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(client-write <scm-arg|int> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|client_write> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(enter-secure-mode <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|enter_secure_mode> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(connection-start <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|connection_start> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(connection-status <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|connection_status> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(connection-write-string <scm-arg|string> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|connection_write> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(connection-write <scm-arg|string> <scm-arg|string> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|connection_write> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(connection-cmd <scm-arg|string> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|connection_cmd> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(connection-eval <scm-arg|string> <scm-arg|string> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|connection_eval> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(connection-interrupt <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|connection_interrupt> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(connection-stop <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|connection_stop> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(widget-printer <scm-arg|command> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|printer_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-color-picker <scm-arg|command> <scm-arg|bool> <scm-arg|array_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|color_picker_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-extend <scm-arg|widget> <scm-arg|array_widget>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|extend_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-hmenu <scm-arg|array_widget>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|horizontal_menu> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-vmenu <scm-arg|array_widget>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|vertical_menu> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-tmenu <scm-arg|array_widget> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tile_menu> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-minibar-menu <scm-arg|array_widget>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|minibar_menu> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-separator <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|menu_separator> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-menu-group <scm-arg|string> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|menu_group> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-pulldown-button <scm-arg|widget> <scm-arg|promise_widget>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|pulldown_button> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-pullright-button <scm-arg|widget> <scm-arg|promise_widget>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|pullright_button> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-menu-button <scm-arg|widget> <scm-arg|command> <scm-arg|string> <scm-arg|string> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|menu_button> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-toggle <scm-arg|command> <scm-arg|bool> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|toggle_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-balloon <scm-arg|widget> <scm-arg|widget>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|balloon_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-empty)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|empty_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-text <scm-arg|string> <scm-arg|int> <scm-arg|int> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|text_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-input <scm-arg|command> <scm-arg|string> <scm-arg|array_string> <scm-arg|int> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|input_text_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-enum <scm-arg|command> <scm-arg|array_string> <scm-arg|string> <scm-arg|int> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|enum_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-choice <scm-arg|command> <scm-arg|array_string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|choice_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-choices <scm-arg|command> <scm-arg|array_string> <scm-arg|array_string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|choice_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-filtered-choice <scm-arg|command> <scm-arg|array_string> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|choice_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-tree-view <scm-arg|command> <scm-arg|tree> <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tree_view_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-xpm <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|xpm_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-box <scm-arg|scheme_tree> <scm-arg|string> <scm-arg|int> <scm-arg|bool> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|box_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-glue <scm-arg|bool> <scm-arg|bool> <scm-arg|int> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|glue_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-color <scm-arg|content> <scm-arg|bool> <scm-arg|bool> <scm-arg|int> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|glue_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-hlist <scm-arg|array_widget>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|horizontal_list> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-vlist <scm-arg|array_widget>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|vertical_list> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-division <scm-arg|string> <scm-arg|widget>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|division_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-aligned <scm-arg|array_widget> <scm-arg|array_widget>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|aligned_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-tabs <scm-arg|array_widget> <scm-arg|array_widget>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tabs_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-icon-tabs <scm-arg|array_url> <scm-arg|array_widget> <scm-arg|array_widget>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|icon_tabs_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-scrollable <scm-arg|widget> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|user_canvas_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-resize <scm-arg|widget> <scm-arg|int> <scm-arg|string> <scm-arg|string> <scm-arg|string> <scm-arg|string> <scm-arg|string> <scm-arg|string> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|resize_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-hsplit <scm-arg|widget> <scm-arg|widget>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|hsplit_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-vsplit <scm-arg|widget> <scm-arg|widget>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|vsplit_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-texmacs-output <scm-arg|content> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|texmacs_output_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-texmacs-input <scm-arg|content> <scm-arg|content> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|texmacs_input_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-ink <scm-arg|command>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|ink_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-refresh <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|refresh_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(widget-refreshable <scm-arg|object> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|refreshable_widget> which returns
    <scm|widget>.
  </explain>

  <\explain>
    <scm|(object-\<gtr\>promise-widget <scm-arg|object>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|as_promise_widget> which returns
    <scm|promise_widget>.
  </explain>

  <\explain>
    <scm|(tree-bounding-rectangle <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_bounding_rectangle> which returns
    <scm|array_int>.
  </explain>

  <\explain>
    <scm|(widget-size <scm-arg|widget>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_widget_size> which returns
    <scm|array_int>.
  </explain>

  <\explain>
    <scm|(texmacs-widget-size <scm-arg|widget>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_texmacs_widget_size> which returns
    <scm|array_int>.
  </explain>

  <\explain>
    <scm|(show-balloon <scm-arg|widget> <scm-arg|int> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|show_help_balloon> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(get-style-menu)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_style_menu> which returns
    <scm|object>.
  </explain>

  <\explain>
    <scm|(hidden-package? <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|hidden_package> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(get-add-package-menu)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_add_package_menu> which returns
    <scm|object>.
  </explain>

  <\explain>
    <scm|(get-remove-package-menu)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_remove_package_menu> which returns
    <scm|object>.
  </explain>

  <\explain>
    <scm|(get-toggle-package-menu)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_toggle_package_menu> which returns
    <scm|object>.
  </explain>

  <\explain>
    <scm|(refresh-now <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|windows_refresh> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(get-screen-size)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_screen_size> which returns
    <scm|array_int>.
  </explain>

  <\explain>
    <scm|(buffer-list)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_all_buffers> which returns
    <scm|array_url>.
  </explain>

  <\explain>
    <scm|(current-buffer-url)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_current_buffer_safe> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(path-to-buffer <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|path_to_buffer> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(buffer-new)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_new_buffer> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(buffer-rename <scm-arg|url> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|rename_buffer> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(buffer-set <scm-arg|url> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_buffer_tree> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(buffer-get <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_buffer_tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(buffer-set-body <scm-arg|url> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_buffer_body> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(buffer-get-body <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_buffer_body> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(buffer-set-master <scm-arg|url> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_master_buffer> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(buffer-get-master <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_master_buffer> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(buffer-set-title <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_title_buffer> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(buffer-get-title <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_title_buffer> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(buffer-last-save <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_last_save_buffer> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(buffer-last-visited <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|last_visited> which returns
    <scm|double>.
  </explain>

  <\explain>
    <scm|(buffer-modified? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|buffer_modified> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(buffer-modified-since-autosave? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|buffer_modified_since_autosave> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(buffer-pretend-modified <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|pretend_buffer_modified> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(buffer-pretend-saved <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|pretend_buffer_saved> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(buffer-pretend-autosaved <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|pretend_buffer_autosaved> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(buffer-attach-notifier <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|attach_buffer_notifier> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(buffer-has-name? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|buffer_has_name> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(buffer-aux? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_aux_buffer> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(buffer-embedded? <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_embedded_buffer> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(buffer-import <scm-arg|url> <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|buffer_import> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(buffer-load <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|buffer_load> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(buffer-export <scm-arg|url> <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|buffer_export> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(buffer-save <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|buffer_save> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tree-import-loaded <scm-arg|string> <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|import_loaded_tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-import <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|import_tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-inclusion <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|load_inclusion> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(tree-export <scm-arg|tree> <scm-arg|url> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|export_tree> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(tree-load-style <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|load_style_tree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(buffer-focus <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|focus_on_buffer> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(buffer-focus* <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|var_focus_on_buffer> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(view-list)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_all_views> which returns
    <scm|array_url>.
  </explain>

  <\explain>
    <scm|(buffer-\<gtr\>views <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|buffer_to_views> which returns
    <scm|array_url>.
  </explain>

  <\explain>
    <scm|(current-view-url)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_current_view_safe> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(window-\<gtr\>view <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|window_to_view> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(view-\<gtr\>buffer <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|view_to_buffer> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(view-\<gtr\>window-url <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|view_to_window> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(view-new <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_new_view> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(view-passive <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_passive_view> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(view-recent <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_recent_view> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(view-delete <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|delete_view> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(window-set-view <scm-arg|url> <scm-arg|url> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|window_set_view> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(switch-to-buffer <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|switch_to_buffer> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(set-drd <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_current_drd> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(window-list)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|windows_list> which returns
    <scm|array_url>.
  </explain>

  <\explain>
    <scm|(windows-number)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_nr_windows> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(current-window)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_current_window> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(buffer-\<gtr\>windows <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|buffer_to_windows> which returns
    <scm|array_url>.
  </explain>

  <\explain>
    <scm|(window-to-buffer <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|window_to_buffer> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(window-set-buffer <scm-arg|url> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|window_set_buffer> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(window-focus <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|window_focus> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(switch-to-window <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|switch_to_window> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(new-buffer)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|create_buffer> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(open-buffer-in-window <scm-arg|url> <scm-arg|content> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|new_buffer_in_new_window> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(open-window)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|open_window> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(open-window-geometry <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|open_window> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(clone-window)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|clone_window> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-buffer-close <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|kill_buffer> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(kill-window <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|kill_window> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(kill-current-window-and-buffer)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|kill_current_window_and_buffer> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(project-attach <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|project_attach> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(project-detach)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|project_attach> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(project-attached?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|project_attached> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(project-get)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|project_get> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(alt-window-handle)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|window_handle> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(alt-window-create-quit <scm-arg|int> <scm-arg|widget> <scm-arg|string> <scm-arg|command>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|window_create> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(alt-window-create-plain <scm-arg|int> <scm-arg|widget> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|window_create_plain> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(alt-window-create-popup <scm-arg|int> <scm-arg|widget> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|window_create_popup> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(alt-window-create-tooltip <scm-arg|int> <scm-arg|widget> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|window_create_tooltip> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(alt-window-delete <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|window_delete> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(alt-window-show <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|window_show> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(alt-window-hide <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|window_hide> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(alt-window-get-size <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|window_get_size> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(alt-window-set-size <scm-arg|int> <scm-arg|int> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|window_set_size> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(alt-window-get-position <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|window_get_position> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(alt-window-set-position <scm-arg|int> <scm-arg|int> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|window_set_position> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(alt-window-search <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|window_search> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(supports-bibtex?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|bibtex_present> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(bibtex-run <scm-arg|string> <scm-arg|string> <scm-arg|url> <scm-arg|array_string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|bibtex_run> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(bib-add-period <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|bib_add_period> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(bib-locase-first <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|bib_locase_first> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(bib-upcase-first <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|bib_upcase_first> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(bib-locase <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|bib_locase> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(bib-upcase <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|bib_upcase> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(bib-default-preserve-case <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|bib_default_preserve_case> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(bib-default-upcase-first <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|bib_default_upcase_first> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(bib-purify <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|bib_purify> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(bib-text-length <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|bib_text_length> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(bib-prefix <scm-arg|scheme_tree> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|bib_prefix> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(bib-empty? <scm-arg|scheme_tree> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|bib_empty> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(bib-field <scm-arg|scheme_tree> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|bib_field> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(bib-abbreviate <scm-arg|scheme_tree> <scm-arg|scheme_tree> <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|bib_abbreviate> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(extract-attachments <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|scm_extract_attachments> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(pdf-make-attachments <scm-arg|url> <scm-arg|array_url> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|pdf_hummus_make_attachments> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(pdf-get-linked-file-paths <scm-arg|tree> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_linked_file_paths> which returns
    <scm|array_url>.
  </explain>

  <\explain>
    <scm|(pdf-replace-linked-path <scm-arg|tree> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|replace_with_relative_path> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(pdf-get-attached-main-tm <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_main_tm> which returns
    <scm|url>.
  </explain>

  <\explain>
    <scm|(array-url-append <scm-arg|url> <scm-arg|array_url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|append> which returns
    <scm|array_url>.
  </explain>

  <\explain>
    <scm|(insert-kbd-wildcard <scm-arg|string> <scm-arg|string> <scm-arg|bool> <scm-arg|bool> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|insert_kbd_wildcard> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(set-variant-keys <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_variant_keys> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(kbd-pre-rewrite <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|kbd_pre_rewrite> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(kbd-post-rewrite <scm-arg|string> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|kbd_post_rewrite> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(kbd-system-rewrite <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|kbd_system_rewrite> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(set-font-rules <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_font_rules> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(window-get-serial)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_window_serial> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(window-set-property <scm-arg|scheme_tree> <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_window_property> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(window-get-property <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_window_property> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(show-header <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|show_header> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(show-icon-bar <scm-arg|int> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|show_icon_bar> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(show-side-tools <scm-arg|int> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|show_side_tools> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(show-bottom-tools <scm-arg|int> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|show_bottom_tools> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(show-footer <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|show_footer> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(visible-header?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|visible_header> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(visible-icon-bar? <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|visible_icon_bar> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(visible-side-tools? <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|visible_side_tools> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(visible-bottom-tools? <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|visible_bottom_tools> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(visible-footer?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|visible_footer> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(full-screen-mode <scm-arg|bool> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|full_screen_mode> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(full-screen?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|in_full_screen_mode> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(full-screen-edit?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|in_full_screen_edit_mode> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(set-window-zoom-factor <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_window_zoom_factor> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(get-window-zoom-factor)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_window_zoom_factor> which returns
    <scm|double>.
  </explain>

  <\explain>
    <scm|(shell <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|shell> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(dialogue-end)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|dialogue_end> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-choose-file <scm-arg|object> <scm-arg|string> <scm-arg|string> <scm-arg|string> <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|choose_file> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(tm-interactive <scm-arg|object> <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|interactive> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-style-clear-cache)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|style_clear_cache> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(set-script-status <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_script_status> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(set-printing-command <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_printing_command> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(set-printer-paper-type <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_printer_page_type> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(get-printer-paper-type)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_printer_page_type> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(set-printer-dpi <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_printer_dpi> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(set-default-zoom-factor <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_default_zoom_factor> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(get-default-zoom-factor)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_default_zoom_factor> which returns
    <scm|double>.
  </explain>

  <\explain>
    <scm|(inclusions-gc)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|inclusions_gc> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(update-all-path <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|typeset_update> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(update-all-buffers)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|typeset_update_all> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(set-message <scm-arg|content> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_message> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(set-message-temp <scm-arg|content> <scm-arg|content> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_message> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(recall-message)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|recall_message> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(yes? <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_yes> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(quit-TeXmacs)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|quit> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(root-tree)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|the_root> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(buffer-path)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|the_buffer_path> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(buffer-tree)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|the_buffer> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(paragraph-tree)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|the_line> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(cursor-path)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|the_path> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(cursor-path*)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|the_shifted_path> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(selection-tree)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_get> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(path-exists? <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|test_subtree> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(cpp-path-\<gtr\>tree <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|the_subtree> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(path-correct-old <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|correct> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(path-insert-with <scm-arg|path> <scm-arg|string> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|insert_with> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(path-remove-with <scm-arg|path> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|remove_with> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(position-new-path <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|position_new> which returns
    <scm|observer>.
  </explain>

  <\explain>
    <scm|(position-delete <scm-arg|observer>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|position_delete> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(position-set <scm-arg|observer> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|position_set> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(position-get <scm-arg|observer>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|position_get> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(inside? <scm-arg|tree_label>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|inside> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(cpp-insert <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|insert_tree> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-insert-go-to <scm-arg|content> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|var_insert_tree> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(insert-raw-go-to <scm-arg|content> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|insert_tree> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(insert-raw-return)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|insert_return> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(remove-text <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|remove_text> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(remove-structure <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|remove_structure> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(remove-structure-upwards)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|remove_structure_upwards> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-make <scm-arg|tree_label>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_compound> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-make-arity <scm-arg|tree_label> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_compound> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(activate)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|activate> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(insert-argument <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|insert_argument> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(remove-argument <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|remove_argument> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(insert-argument-at <scm-arg|path> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|insert_argument> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(remove-argument-at <scm-arg|path> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|remove_argument> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-make-with <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_with> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(make-mod-active <scm-arg|tree_label>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_mod_active> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(make-style-with <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_style_with> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-make-hybrid)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_hybrid> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(activate-latex)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|activate_latex> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(activate-hybrid <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|activate_hybrid> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(activate-symbol)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|activate_symbol> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(make-return-before)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_return_before> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(make-return-after)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_return_after> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(temp-proof-fix)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|temp_proof_fix> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(get-full-env)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_full_env> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(get-all-inits)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_init_all> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(init-default-one <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|init_default> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(init-env <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|init_env> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(init-env-tree <scm-arg|string> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|init_env> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(init-style <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|init_style> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(get-style-tree)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_style> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(set-style-tree <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|change_style> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(get-env <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_env_string> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(get-env-tree <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_env_value> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(get-env-tree-at <scm-arg|string> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_env_value> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(get-init <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_init_string> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(get-init-tree <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_init_value> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(context-has? <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|defined_at_cursor> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(style-has? <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|defined_at_init> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(init-has? <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|defined_in_init> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(get-page-count)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_page_count> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(get-page-width <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_page_width> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(get-pages-width <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_pages_width> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(get-page-height <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_page_height> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(get-total-width <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_total_width> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(get-total-height <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_total_height> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(get-reference <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_ref> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(set-reference <scm-arg|string> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_ref> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(reset-reference <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|reset_ref> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(find-references <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|find_refs> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(list-references)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|list_refs> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(list-references* <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|list_refs> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(get-auxiliary <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_aux> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(set-auxiliary <scm-arg|string> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_aux> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(reset-auxiliary <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|reset_aux> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(list-auxiliaries)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|list_auxs> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(list-auxiliaries* <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|list_auxs> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(get-attachment <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_att> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(set-attachment <scm-arg|string> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_att> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(reset-attachment <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|reset_att> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(list-attachments)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|list_atts> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(list-attachments* <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|list_atts> which returns
    <scm|array_string>.
  </explain>

  <\explain>
    <scm|(make-htab <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_htab> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(make-space <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_space> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(make-var-space <scm-arg|string> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_space> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(make-hspace <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_hspace> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(make-var-hspace <scm-arg|string> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_hspace> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(make-vspace-before <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_vspace_before> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(make-var-vspace-before <scm-arg|string> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_vspace_before> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(make-vspace-after <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_vspace_after> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(make-var-vspace-after <scm-arg|string> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_vspace_after> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(make-image <scm-arg|string> <scm-arg|bool> <scm-arg|string> <scm-arg|string> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_image> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(length-decode <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|as_length> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(length-add <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|add_lengths> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(length-sub <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|sub_lengths> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(length-max <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|max_lengths> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(length-min <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|min_lengths> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(length-mult <scm-arg|double> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|multiply_length> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(length? <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|is_length> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(length-divide <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|divide_lengths> which returns
    <scm|double>.
  </explain>

  <\explain>
    <scm|(cpp-make-rigid)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_rigid> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-make-lprime <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_lprime> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-make-rprime <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_rprime> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-make-below)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_below> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-make-above)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_above> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-make-script <scm-arg|bool> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_script> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-make-fraction)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_fraction> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-make-sqrt)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_sqrt> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-make-wide <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_wide> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-make-wide-under <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_wide_under> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-make-var-sqrt)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_var_sqrt> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-make-neg)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_neg> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-make-tree)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_tree> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(make-subtable)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|make_subtable> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(table-deactivate)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_deactivate> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(table-extract-format)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_extract_format> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(table-insert-row <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_insert_row> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(table-insert-column <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_insert_column> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(table-remove-row <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_remove_row> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(table-remove-column <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_remove_column> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(table-nr-rows)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_nr_rows> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(table-nr-columns)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_nr_columns> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(table-get-extents)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_get_extents> which returns
    <scm|array_int>.
  </explain>

  <\explain>
    <scm|(table-set-extents <scm-arg|int> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_set_extents> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(table-which-row)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_which_row> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(table-which-column)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_which_column> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(table-which-cells)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_which_cells> which returns
    <scm|array_int>.
  </explain>

  <\explain>
    <scm|(table-cell-path <scm-arg|int> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_search_cell> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(table-go-to <scm-arg|int> <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_go_to> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(table-set-format <scm-arg|string> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_set_format> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(table-get-format-all)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_get_format> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(table-get-format <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_get_format> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(table-del-format <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_del_format> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(table-row-decoration <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_row_decoration> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(table-column-decoration <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_column_decoration> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(table-format-center)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_format_center> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(table-correct-block-content)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_correct_block_content> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(set-cell-mode <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_cell_mode> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(get-cell-mode)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_cell_mode> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(cell-set-format <scm-arg|string> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|cell_set_format> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cell-get-format <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|cell_get_format> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(cell-del-format <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|cell_del_format> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(table-test)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|table_test> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(key-press <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|key_press> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(raw-emulate-keyboard <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|emulate_keyboard> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(complete-try?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|complete_try> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(get-input-mode)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_input_mode> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(key-press-search <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|search_keypress> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(key-press-replace <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|replace_keypress> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(key-press-spell <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|spell_keypress> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(key-press-complete <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|complete_keypress> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(mouse-any <scm-arg|string> <scm-arg|int> <scm-arg|int> <scm-arg|int> <scm-arg|double> <scm-arg|array_double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|mouse_any> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(get-mouse-position)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_mouse_position> which returns
    <scm|array_int>.
  </explain>

  <\explain>
    <scm|(set-mouse-pointer <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_pointer> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(set-predef-mouse-pointer <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_pointer> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(go-to-path <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|go_to> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(go-left)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|go_left> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(go-right)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|go_right> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(go-up)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|go_up> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(go-down)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|go_down> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(go-start)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|go_start> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(go-end)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|go_end> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(go-start-of <scm-arg|tree_label>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|go_start_of> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(go-end-of <scm-arg|tree_label>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|go_end_of> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(go-start-with <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|go_start_with> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(go-end-with <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|go_end_with> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(go-start-line)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|go_start_line> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(go-end-line)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|go_end_line> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(go-page-up)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|go_page_up> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(go-page-down)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|go_page_down> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(go-start-paragraph)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|go_start_paragraph> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(go-end-paragraph)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|go_end_paragraph> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(label-\<gtr\>path <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|search_label> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(go-to-label <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|go_to_label> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cursor-accessible?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|cursor_is_accessible> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(cursor-show-if-hidden)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|show_cursor_if_hidden> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(select-all)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|select_all> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(select-line)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|select_line> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(select-from-cursor)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|select_from_cursor> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(select-from-cursor-if-active)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|select_from_cursor_if_active> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(select-from-keyboard <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|select_from_keyboard> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(select-from-shift-keyboard)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|select_from_shift_keyboard> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(select-enlarge)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|select_enlarge> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(select-enlarge-environmental)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|select_enlarge_environmental> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(selection-active-any?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_active_any> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(selection-active-normal?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_active_normal> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(selection-active-table?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_active_table> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(selection-active-small?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_active_small> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(selection-active-enlarging?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_active_enlarging> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(selection-set-start)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_set_start> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(selection-set-end)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_set_end> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(selection-get-start)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_get_start> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(selection-get-end)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_get_end> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(selection-get-start*)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_var_get_start> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(selection-get-end*)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_var_get_end> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(selection-path)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_get_path> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(selection-set <scm-arg|path> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_set_paths> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(selection-set-range-set <scm-arg|array_path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_set_range_set> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(clipboard-set <scm-arg|string> <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_set> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(clipboard-get <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_get> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(cpp-clipboard-copy <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_copy> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-clipboard-cut <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_cut> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(clipboard-cut-at <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|cut> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(clipboard-cut-between <scm-arg|path> <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|cut> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cpp-clipboard-paste <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_paste> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(selection-move)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_move> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(clipboard-clear <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_clear> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(selection-cancel)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_cancel> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(clipboard-set-import <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_set_import> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(clipboard-set-export <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_set_export> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(clipboard-get-import)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_get_import> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(clipboard-get-export)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|selection_get_export> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(set-manual-focus-path <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|manual_focus_set> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(get-manual-focus-path)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|manual_focus_get> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(get-focus-path)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|focus_get> which returns
    <scm|path>.
  </explain>

  <\explain>
    <scm|(set-alt-selection <scm-arg|string> <scm-arg|array_path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_alt_selection> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(get-alt-selection <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_alt_selection> which returns
    <scm|array_path>.
  </explain>

  <\explain>
    <scm|(cancel-alt-selection <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|cancel_alt_selection> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cancel-alt-selections)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|cancel_alt_selections> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(clear-undo-history)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|clear_undo_history> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(commit-changes)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|end_editing> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(start-slave <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|start_slave> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(mark-start <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|mark_start> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(mark-end <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|mark_end> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(mark-cancel <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|mark_cancel> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(remove-undo-mark)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|remove_undo_mark> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(add-undo-mark)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|add_undo_mark> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(unredoable-undo)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|unredoable_undo> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(undo-possibilities)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|undo_possibilities> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(undo <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|undo> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(redo-possibilities)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|redo_possibilities> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(redo <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|redo> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(show-history)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|show_history> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(archive-state)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|archive_state> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(start-editing)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|start_editing> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(end-editing)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|end_editing> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(cancel-editing)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|cancel_editing> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(in-graphics?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|inside_graphics> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(get-graphical-x)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_x> which returns
    <scm|double>.
  </explain>

  <\explain>
    <scm|(get-graphical-y)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_y> which returns
    <scm|double>.
  </explain>

  <\explain>
    <scm|(get-graphical-pixel)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_pixel> which returns
    <scm|double>.
  </explain>

  <\explain>
    <scm|(get-graphical-object)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_graphical_object> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(set-graphical-object <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_graphical_object> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(invalidate-graphical-object)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|invalidate_graphical_object> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(graphical-select <scm-arg|double> <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|graphical_select> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(graphical-select-area <scm-arg|double> <scm-arg|double> <scm-arg|double> <scm-arg|double>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|graphical_select> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(in-normal-mode?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|in_normal_mode> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(in-search-mode?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|in_search_mode> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(in-replace-mode?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|in_replace_mode> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(in-spell-mode?)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|in_spell_mode> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(search-start <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|search_start> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(search-button-next)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|search_button_next> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(replace-start <scm-arg|string> <scm-arg|string> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|replace_start> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(spell-start)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|spell_start> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(spell-replace <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|spell_replace> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(session-complete-command <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|session_complete_command> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(custom-complete <scm-arg|tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|custom_complete> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(keyboard-focus-on <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|keyboard_focus_on> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(view-set-property <scm-arg|scheme_tree> <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|set_property> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(view-get-property <scm-arg|scheme_tree>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_property> which returns
    <scm|scheme_tree>.
  </explain>

  <\explain>
    <scm|(get-window-width)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_window_width> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(get-window-height)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_window_height> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(get-window-x)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_window_x> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(get-window-y)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_window_y> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(get-canvas-x)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_canvas_x> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(get-canvas-y)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_canvas_y> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(get-scroll-x)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_scroll_x> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(get-scroll-y)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_scroll_y> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(clear-buffer)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|clear_buffer> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(tex-buffer)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|tex_buffer> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(clear-local-info)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|clear_local_info> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(refresh-window)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|invalidate_all> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(update-forced)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|typeset_forced> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(update-path <scm-arg|path>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|typeset_invalidate> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(update-current-buffer)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|typeset_invalidate_all> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(update-players <scm-arg|path> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|typeset_invalidate_players> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(generate-all-aux)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|generate_aux> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(generate-aux <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|generate_aux> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(notify-page-change)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|notify_page_change> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(notify-change <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|notify_change> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(notified-change? <scm-arg|int>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|has_changed> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(get-metadata <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|get_metadata> which returns
    <scm|string>.
  </explain>

  <\explain>
    <scm|(cpp-nr-pages)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|nr_pages> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(print-to-file <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|print_to_file> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(print-pages-to-file <scm-arg|url> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|print_to_file> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(print)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|print_buffer> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(print-pages <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|print_buffer> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(print-snippet <scm-arg|url> <scm-arg|content> <scm-arg|bool>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|print_snippet> which returns
    <scm|array_int>.
  </explain>

  <\explain>
    <scm|(graphics-file-to-clipboard <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|graphics_file_to_clipboard> which returns
    <scm|bool>.
  </explain>

  <\explain>
    <scm|(export-postscript <scm-arg|url>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|export_ps> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(export-pages-postscript <scm-arg|url> <scm-arg|string> <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|export_ps> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(footer-eval <scm-arg|string>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|footer_eval> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(texmacs-exec <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|texmacs_exec> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(texmacs-exec* <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|var_texmacs_exec> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(texmacs-expand <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|exec_texmacs> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(verbatim-expand <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|exec_verbatim> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(latex-expand <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|exec_latex> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(html-expand <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|exec_html> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(animate-checkout <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|checkout_animation> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(animate-commit <scm-arg|content>)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|commit_animation> which returns
    <scm|tree>.
  </explain>

  <\explain>
    <scm|(idle-time)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|idle_time> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(change-time)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|change_time> which returns
    <scm|int>.
  </explain>

  <\explain>
    <scm|(menu-before-action)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|before_menu_action> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(menu-after-action)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|after_menu_action> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(update-menus)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|update_menus> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(show-tree)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|show_tree> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(show-box)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|show_box> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(show-env)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|show_env> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(show-path)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|show_path> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(show-cursor)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|show_cursor> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(show-selection)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|show_selection> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(show-meminfo)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|show_meminfo> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(edit-special)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|edit_special> which returns
    <scm|void>.
  </explain>

  <\explain>
    <scm|(edit-test)>
<explain-synopsis|no synopsis>
  <|explain>
    Calls the <c++> function <cpp|edit_test> which returns
    <scm|void>.
  </explain>


  <tmdoc-copyright|2016|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>