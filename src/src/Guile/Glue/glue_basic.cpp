
/******************************************************************************
*
* This file has been generated automatically using build-glue.scm
* from build-glue-basic.scm. Please do not edit its contents.
* Copyright (C) 2000 Joris van der Hoeven
*
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
tmg_os_win32P () {
  // SCM_DEFER_INTS;
  bool out= os_win32 ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
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
tmg_tree_2object (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree->object");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  scheme_tree out= tree_to_scheme_tree (in1);
  // SCM_ALLOW_INTS;

  return scheme_tree_to_scm (out);
}

SCM
tmg_object_2tree (SCM arg1) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "object->tree");

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
tmg_tree_get_label (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-get-label");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  tree_label out= L (in1);
  // SCM_ALLOW_INTS;

  return tree_label_to_scm (out);
}

SCM
tmg_tree_get_children (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "tree-get-children");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  array_tree out= A (in1);
  // SCM_ALLOW_INTS;

  return array_tree_to_scm (out);
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
tmg_tree0 (SCM arg1) {
  SCM_ASSERT_TREE_LABEL (arg1, SCM_ARG1, "tree0");

  tree_label in1= scm_to_tree_label (arg1);

  // SCM_DEFER_INTS;
  texmacs_tree out= tree (in1);
  // SCM_ALLOW_INTS;

  return texmacs_tree_to_scm (out);
}

SCM
tmg_tree1 (SCM arg1, SCM arg2) {
  SCM_ASSERT_TREE_LABEL (arg1, SCM_ARG1, "tree1");
  SCM_ASSERT_TEXMACS_TREE (arg2, SCM_ARG2, "tree1");

  tree_label in1= scm_to_tree_label (arg1);
  texmacs_tree in2= scm_to_texmacs_tree (arg2);

  // SCM_DEFER_INTS;
  texmacs_tree out= tree (in1, in2);
  // SCM_ALLOW_INTS;

  return texmacs_tree_to_scm (out);
}

SCM
tmg_tree2 (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_TREE_LABEL (arg1, SCM_ARG1, "tree2");
  SCM_ASSERT_TEXMACS_TREE (arg2, SCM_ARG2, "tree2");
  SCM_ASSERT_TEXMACS_TREE (arg3, SCM_ARG3, "tree2");

  tree_label in1= scm_to_tree_label (arg1);
  texmacs_tree in2= scm_to_texmacs_tree (arg2);
  texmacs_tree in3= scm_to_texmacs_tree (arg3);

  // SCM_DEFER_INTS;
  texmacs_tree out= tree (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return texmacs_tree_to_scm (out);
}

SCM
tmg_tree3 (SCM arg1, SCM arg2, SCM arg3, SCM arg4) {
  SCM_ASSERT_TREE_LABEL (arg1, SCM_ARG1, "tree3");
  SCM_ASSERT_TEXMACS_TREE (arg2, SCM_ARG2, "tree3");
  SCM_ASSERT_TEXMACS_TREE (arg3, SCM_ARG3, "tree3");
  SCM_ASSERT_TEXMACS_TREE (arg4, SCM_ARG4, "tree3");

  tree_label in1= scm_to_tree_label (arg1);
  texmacs_tree in2= scm_to_texmacs_tree (arg2);
  texmacs_tree in3= scm_to_texmacs_tree (arg3);
  texmacs_tree in4= scm_to_texmacs_tree (arg4);

  // SCM_DEFER_INTS;
  texmacs_tree out= tree (in1, in2, in3, in4);
  // SCM_ALLOW_INTS;

  return texmacs_tree_to_scm (out);
}

SCM
tmg_tree_atomicP (SCM arg1) {
  SCM_ASSERT_TEXMACS_TREE (arg1, SCM_ARG1, "tree-atomic?");

  texmacs_tree in1= scm_to_texmacs_tree (arg1);

  // SCM_DEFER_INTS;
  bool out= is_atomic (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_tree_compoundP (SCM arg1) {
  SCM_ASSERT_TEXMACS_TREE (arg1, SCM_ARG1, "tree-compound?");

  texmacs_tree in1= scm_to_texmacs_tree (arg1);

  // SCM_DEFER_INTS;
  bool out= is_compound (in1);
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_tree_arity (SCM arg1) {
  SCM_ASSERT_TEXMACS_TREE (arg1, SCM_ARG1, "tree-arity");

  texmacs_tree in1= scm_to_texmacs_tree (arg1);

  // SCM_DEFER_INTS;
  int out= N (in1);
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_tree_ref (SCM arg1, SCM arg2) {
  SCM_ASSERT_TEXMACS_TREE (arg1, SCM_ARG1, "tree-ref");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tree-ref");

  texmacs_tree in1= scm_to_texmacs_tree (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  texmacs_tree out= tree_ref (in1, in2);
  // SCM_ALLOW_INTS;

  return texmacs_tree_to_scm (out);
}

SCM
tmg_tree_setS (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_TEXMACS_TREE (arg1, SCM_ARG1, "tree-set!");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "tree-set!");
  SCM_ASSERT_TEXMACS_TREE (arg3, SCM_ARG3, "tree-set!");

  texmacs_tree in1= scm_to_texmacs_tree (arg1);
  int in2= scm_to_int (arg2);
  texmacs_tree in3= scm_to_texmacs_tree (arg3);

  // SCM_DEFER_INTS;
  tree_set (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM
tmg_subtree (SCM arg1, SCM arg2) {
  SCM_ASSERT_TEXMACS_TREE (arg1, SCM_ARG1, "subtree");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "subtree");

  texmacs_tree in1= scm_to_texmacs_tree (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  texmacs_tree out= subtree (in1, in2);
  // SCM_ALLOW_INTS;

  return texmacs_tree_to_scm (out);
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
tmg_texmacs_2scheme (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "texmacs->scheme");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  string out= tree_to_scheme (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_scheme_2texmacs (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "scheme->texmacs");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tree out= scheme_document_to_tree (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_scheme_snippet_2texmacs (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "scheme-snippet->texmacs");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tree out= scheme_to_tree (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_texmacs_2verbatim (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "texmacs->verbatim");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  string out= tree_to_verbatim (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_verbatim_snippet_2texmacs (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "verbatim-snippet->texmacs");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tree out= verbatim_to_tree (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_verbatim_2texmacs (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "verbatim->texmacs");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  tree out= verbatim_document_to_tree (in1);
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
tmg_upgrade_tmml (SCM arg1) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "upgrade-tmml");

  scheme_tree in1= scm_to_scheme_tree (arg1);

  // SCM_DEFER_INTS;
  tree out= tmml_upgrade (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_get_texmacs_path () {
  // SCM_DEFER_INTS;
  string out= get_texmacs_path ();
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
tmg_object_2scheme_tree (SCM arg1) {
  SCM_ASSERT_SCHEME_TREE (arg1, SCM_ARG1, "object->scheme-tree");

  scheme_tree in1= scm_to_scheme_tree (arg1);

  // SCM_DEFER_INTS;
  tree out= copy (in1);
  // SCM_ALLOW_INTS;

  return tree_to_scm (out);
}

SCM
tmg_scheme_tree_2object (SCM arg1) {
  SCM_ASSERT_TREE (arg1, SCM_ARG1, "scheme-tree->object");

  tree in1= scm_to_tree (arg1);

  // SCM_DEFER_INTS;
  scheme_tree out= copy (in1);
  // SCM_ALLOW_INTS;

  return scheme_tree_to_scm (out);
}

SCM
tmg_scheme_dialect () {
  // SCM_DEFER_INTS;
  string out= scheme_dialect ();
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_cursor_start (SCM arg1, SCM arg2) {
  SCM_ASSERT_TEXMACS_TREE (arg1, SCM_ARG1, "cursor-start");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "cursor-start");

  texmacs_tree in1= scm_to_texmacs_tree (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  path out= start (in1, in2);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_cursor_end (SCM arg1, SCM arg2) {
  SCM_ASSERT_TEXMACS_TREE (arg1, SCM_ARG1, "cursor-end");
  SCM_ASSERT_PATH (arg2, SCM_ARG2, "cursor-end");

  texmacs_tree in1= scm_to_texmacs_tree (arg1);
  path in2= scm_to_path (arg2);

  // SCM_DEFER_INTS;
  path out= end (in1, in2);
  // SCM_ALLOW_INTS;

  return path_to_scm (out);
}

SCM
tmg_support_ec_fontsP () {
  // SCM_DEFER_INTS;
  bool out= support_ec_fonts ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_support_tt_fontsP () {
  // SCM_DEFER_INTS;
  bool out= ft_present ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_use_ec_fontsP () {
  // SCM_DEFER_INTS;
  bool out= use_ec_fonts ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_use_tt_fontsP () {
  // SCM_DEFER_INTS;
  bool out= use_tt_fonts ();
  // SCM_ALLOW_INTS;

  return bool_to_scm (out);
}

SCM
tmg_set_font_type (SCM arg1) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "set-font-type");

  int in1= scm_to_int (arg1);

  // SCM_DEFER_INTS;
  set_font_type (in1);
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
tmg_texmacs_time () {
  // SCM_DEFER_INTS;
  int out= texmacs_time ();
  // SCM_ALLOW_INTS;

  return int_to_scm (out);
}

SCM
tmg_plugin_list () {
  // SCM_DEFER_INTS;
  scheme_tree out= plugin_list ();
  // SCM_ALLOW_INTS;

  return scheme_tree_to_scm (out);
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
tmg_escape_quotes (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "escape-quotes");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= escape_quotes (in1);
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
tmg_url_temp () {
  // SCM_DEFER_INTS;
  url out= url_temp ();
  // SCM_ALLOW_INTS;

  return url_to_scm (out);
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
tmg_system_append (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "system-append");
  SCM_ASSERT_URL (arg2, SCM_ARG2, "system-append");

  url in1= scm_to_url (arg1);
  url in2= scm_to_url (arg2);

  // SCM_DEFER_INTS;
  append (in1, in2);
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
tmg_string_slash (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "string-slash");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= slash (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
}

SCM
tmg_string_unslash (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "string-unslash");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= unslash (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
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
tmg_tm_2xml_cdata (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "tm->xml-cdata");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= tm_to_xml_cdata (in1);
  // SCM_ALLOW_INTS;

  return string_to_scm (out);
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
tmg_xml_cdata_2tm (SCM arg1) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "xml-cdata->tm");

  string in1= scm_to_string (arg1);

  // SCM_DEFER_INTS;
  string out= xml_cdata_to_tm (in1);
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
tmg_connection_eval (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "connection-eval");
  SCM_ASSERT_STRING (arg2, SCM_ARG2, "connection-eval");
  SCM_ASSERT_TEXMACS_TREE (arg3, SCM_ARG3, "connection-eval");

  string in1= scm_to_string (arg1);
  string in2= scm_to_string (arg2);
  texmacs_tree in3= scm_to_texmacs_tree (arg3);

  // SCM_DEFER_INTS;
  texmacs_tree out= connection_eval (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return texmacs_tree_to_scm (out);
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
  texmacs_tree out= connection_cmd (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return texmacs_tree_to_scm (out);
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
tmg_widget_hlist (SCM arg1) {
  SCM_ASSERT_ARRAY_WIDGET (arg1, SCM_ARG1, "widget-hlist");

  array_widget in1= scm_to_array_widget (arg1);

  // SCM_DEFER_INTS;
  widget out= horizontal_list (in1);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_named_hlist (SCM arg1, SCM arg2) {
  SCM_ASSERT_ARRAY_WIDGET (arg1, SCM_ARG1, "widget-named-hlist");
  SCM_ASSERT_ARRAY_STRING (arg2, SCM_ARG2, "widget-named-hlist");

  array_widget in1= scm_to_array_widget (arg1);
  array_string in2= scm_to_array_string (arg2);

  // SCM_DEFER_INTS;
  widget out= horizontal_list (in1, in2);
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
tmg_widget_named_vlist (SCM arg1, SCM arg2) {
  SCM_ASSERT_ARRAY_WIDGET (arg1, SCM_ARG1, "widget-named-vlist");
  SCM_ASSERT_ARRAY_STRING (arg2, SCM_ARG2, "widget-named-vlist");

  array_widget in1= scm_to_array_widget (arg1);
  array_string in2= scm_to_array_string (arg2);

  // SCM_DEFER_INTS;
  widget out= vertical_list (in1, in2);
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
tmg_widget_tile (SCM arg1, SCM arg2) {
  SCM_ASSERT_ARRAY_WIDGET (arg1, SCM_ARG1, "widget-tile");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "widget-tile");

  array_widget in1= scm_to_array_widget (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  widget out= tile (in1, in2);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_named_tile (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_ARRAY_WIDGET (arg1, SCM_ARG1, "widget-named-tile");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "widget-named-tile");
  SCM_ASSERT_ARRAY_STRING (arg3, SCM_ARG3, "widget-named-tile");

  array_widget in1= scm_to_array_widget (arg1);
  int in2= scm_to_int (arg2);
  array_string in3= scm_to_array_string (arg3);

  // SCM_DEFER_INTS;
  widget out= tile (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_harray (SCM arg1, SCM arg2) {
  SCM_ASSERT_ARRAY_WIDGET (arg1, SCM_ARG1, "widget-harray");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "widget-harray");

  array_widget in1= scm_to_array_widget (arg1);
  int in2= scm_to_int (arg2);

  // SCM_DEFER_INTS;
  widget out= horizontal_array (in1, in2);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_named_harray (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_ARRAY_WIDGET (arg1, SCM_ARG1, "widget-named-harray");
  SCM_ASSERT_ARRAY_STRING (arg2, SCM_ARG2, "widget-named-harray");
  SCM_ASSERT_INT (arg3, SCM_ARG3, "widget-named-harray");

  array_widget in1= scm_to_array_widget (arg1);
  array_string in2= scm_to_array_string (arg2);
  int in3= scm_to_int (arg3);

  // SCM_DEFER_INTS;
  widget out= horizontal_array (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_switch (SCM arg1, SCM arg2) {
  SCM_ASSERT_ARRAY_WIDGET (arg1, SCM_ARG1, "widget-switch");
  SCM_ASSERT_ARRAY_STRING (arg2, SCM_ARG2, "widget-switch");

  array_widget in1= scm_to_array_widget (arg1);
  array_string in2= scm_to_array_string (arg2);

  // SCM_DEFER_INTS;
  widget out= switch_widget (in1, in2);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_switch_init (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_ARRAY_WIDGET (arg1, SCM_ARG1, "widget-switch-init");
  SCM_ASSERT_ARRAY_STRING (arg2, SCM_ARG2, "widget-switch-init");
  SCM_ASSERT_INT (arg3, SCM_ARG3, "widget-switch-init");

  array_widget in1= scm_to_array_widget (arg1);
  array_string in2= scm_to_array_string (arg2);
  int in3= scm_to_int (arg3);

  // SCM_DEFER_INTS;
  widget out= switch_widget (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_optional (SCM arg1) {
  SCM_ASSERT_WIDGET (arg1, SCM_ARG1, "widget-optional");

  widget in1= scm_to_widget (arg1);

  // SCM_DEFER_INTS;
  widget out= optional_widget (in1);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_optional_init (SCM arg1, SCM arg2) {
  SCM_ASSERT_WIDGET (arg1, SCM_ARG1, "widget-optional-init");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "widget-optional-init");

  widget in1= scm_to_widget (arg1);
  bool in2= scm_to_bool (arg2);

  // SCM_DEFER_INTS;
  widget out= optional_widget (in1, in2);
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
tmg_widget_separator (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "widget-separator");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "widget-separator");
  SCM_ASSERT_BOOL (arg3, SCM_ARG3, "widget-separator");

  int in1= scm_to_int (arg1);
  int in2= scm_to_int (arg2);
  bool in3= scm_to_bool (arg3);

  // SCM_DEFER_INTS;
  widget out= separator_widget (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_text (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "widget-text");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "widget-text");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "widget-text");

  string in1= scm_to_string (arg1);
  bool in2= scm_to_bool (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  widget out= text_widget (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_xpm (SCM arg1, SCM arg2) {
  SCM_ASSERT_URL (arg1, SCM_ARG1, "widget-xpm");
  SCM_ASSERT_BOOL (arg2, SCM_ARG2, "widget-xpm");

  url in1= scm_to_url (arg1);
  bool in2= scm_to_bool (arg2);

  // SCM_DEFER_INTS;
  widget out= xpm_widget (in1, in2);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_menu_text (SCM arg1, SCM arg2, SCM arg3, SCM arg4) {
  SCM_ASSERT_STRING (arg1, SCM_ARG1, "widget-menu-text");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "widget-menu-text");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "widget-menu-text");
  SCM_ASSERT_BOOL (arg4, SCM_ARG4, "widget-menu-text");

  string in1= scm_to_string (arg1);
  int in2= scm_to_int (arg2);
  string in3= scm_to_string (arg3);
  bool in4= scm_to_bool (arg4);

  // SCM_DEFER_INTS;
  widget out= menu_text_widget (in1, in2, in3, in4);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_command_button_1 (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_WIDGET (arg1, SCM_ARG1, "widget-command-button-1");
  SCM_ASSERT_COMMAND (arg2, SCM_ARG2, "widget-command-button-1");
  SCM_ASSERT_BOOL (arg3, SCM_ARG3, "widget-command-button-1");

  widget in1= scm_to_widget (arg1);
  command in2= scm_to_command (arg2);
  bool in3= scm_to_bool (arg3);

  // SCM_DEFER_INTS;
  widget out= command_button (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_command_button_2 (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_WIDGET (arg1, SCM_ARG1, "widget-command-button-2");
  SCM_ASSERT_WIDGET (arg2, SCM_ARG2, "widget-command-button-2");
  SCM_ASSERT_COMMAND (arg3, SCM_ARG3, "widget-command-button-2");

  widget in1= scm_to_widget (arg1);
  widget in2= scm_to_widget (arg2);
  command in3= scm_to_command (arg3);

  // SCM_DEFER_INTS;
  widget out= command_button (in1, in2, in3);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_command_button_3 (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6) {
  SCM_ASSERT_WIDGET (arg1, SCM_ARG1, "widget-command-button-3");
  SCM_ASSERT_WIDGET (arg2, SCM_ARG2, "widget-command-button-3");
  SCM_ASSERT_WIDGET (arg3, SCM_ARG3, "widget-command-button-3");
  SCM_ASSERT_COMMAND (arg4, SCM_ARG4, "widget-command-button-3");
  SCM_ASSERT_BOOL (arg5, SCM_ARG5, "widget-command-button-3");
  SCM_ASSERT_BOOL (arg6, SCM_ARG6, "widget-command-button-3");

  widget in1= scm_to_widget (arg1);
  widget in2= scm_to_widget (arg2);
  widget in3= scm_to_widget (arg3);
  command in4= scm_to_command (arg4);
  bool in5= scm_to_bool (arg5);
  bool in6= scm_to_bool (arg6);

  // SCM_DEFER_INTS;
  widget out= command_button (in1, in2, in3, in4, in5, in6);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_pulldown_button (SCM arg1, SCM arg2) {
  SCM_ASSERT_WIDGET (arg1, SCM_ARG1, "widget-pulldown-button");
  SCM_ASSERT_WIDGET (arg2, SCM_ARG2, "widget-pulldown-button");

  widget in1= scm_to_widget (arg1);
  widget in2= scm_to_widget (arg2);

  // SCM_DEFER_INTS;
  widget out= pulldown_button (in1, in2);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_pullright_button (SCM arg1, SCM arg2) {
  SCM_ASSERT_WIDGET (arg1, SCM_ARG1, "widget-pullright-button");
  SCM_ASSERT_WIDGET (arg2, SCM_ARG2, "widget-pullright-button");

  widget in1= scm_to_widget (arg1);
  widget in2= scm_to_widget (arg2);

  // SCM_DEFER_INTS;
  widget out= pullright_button (in1, in2);
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
tmg_widget_pulldown_button_lazy (SCM arg1, SCM arg2) {
  SCM_ASSERT_WIDGET (arg1, SCM_ARG1, "widget-pulldown-button-lazy");
  SCM_ASSERT_MAKE_WIDGET (arg2, SCM_ARG2, "widget-pulldown-button-lazy");

  widget in1= scm_to_widget (arg1);
  make_widget in2= scm_to_make_widget (arg2);

  // SCM_DEFER_INTS;
  widget out= pulldown_button (in1, in2);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_pullright_button_lazy (SCM arg1, SCM arg2) {
  SCM_ASSERT_WIDGET (arg1, SCM_ARG1, "widget-pullright-button-lazy");
  SCM_ASSERT_MAKE_WIDGET (arg2, SCM_ARG2, "widget-pullright-button-lazy");

  widget in1= scm_to_widget (arg1);
  make_widget in2= scm_to_make_widget (arg2);

  // SCM_DEFER_INTS;
  widget out= pullright_button (in1, in2);
  // SCM_ALLOW_INTS;

  return widget_to_scm (out);
}

SCM
tmg_widget_wait (SCM arg1, SCM arg2, SCM arg3) {
  SCM_ASSERT_INT (arg1, SCM_ARG1, "widget-wait");
  SCM_ASSERT_INT (arg2, SCM_ARG2, "widget-wait");
  SCM_ASSERT_STRING (arg3, SCM_ARG3, "widget-wait");

  int in1= scm_to_int (arg1);
  int in2= scm_to_int (arg2);
  string in3= scm_to_string (arg3);

  // SCM_DEFER_INTS;
  widget out= wait_widget (in1, in2, in3);
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
tmg_object_2make_widget (SCM arg1) {
  SCM_ASSERT_OBJECT (arg1, SCM_ARG1, "object->make-widget");

  object in1= scm_to_object (arg1);

  // SCM_DEFER_INTS;
  make_widget out= as_make_widget (in1);
  // SCM_ALLOW_INTS;

  return make_widget_to_scm (out);
}

void
initialize_glue_basic () {
  gh_new_procedure ("texmacs-version-release", (FN) tmg_texmacs_version_release, 1, 0, 0);
  gh_new_procedure ("os-win32?", (FN) tmg_os_win32P, 0, 0, 0);
  gh_new_procedure ("win32-display", (FN) tmg_win32_display, 1, 0, 0);
  gh_new_procedure ("tree->object", (FN) tmg_tree_2object, 1, 0, 0);
  gh_new_procedure ("object->tree", (FN) tmg_object_2tree, 1, 0, 0);
  gh_new_procedure ("tree->string", (FN) tmg_tree_2string, 1, 0, 0);
  gh_new_procedure ("tree-get-label", (FN) tmg_tree_get_label, 1, 0, 0);
  gh_new_procedure ("tree-get-children", (FN) tmg_tree_get_children, 1, 0, 0);
  gh_new_procedure ("string->tree", (FN) tmg_string_2tree, 1, 0, 0);
  gh_new_procedure ("tree0", (FN) tmg_tree0, 1, 0, 0);
  gh_new_procedure ("tree1", (FN) tmg_tree1, 2, 0, 0);
  gh_new_procedure ("tree2", (FN) tmg_tree2, 3, 0, 0);
  gh_new_procedure ("tree3", (FN) tmg_tree3, 4, 0, 0);
  gh_new_procedure ("tree-atomic?", (FN) tmg_tree_atomicP, 1, 0, 0);
  gh_new_procedure ("tree-compound?", (FN) tmg_tree_compoundP, 1, 0, 0);
  gh_new_procedure ("tree-arity", (FN) tmg_tree_arity, 1, 0, 0);
  gh_new_procedure ("tree-ref", (FN) tmg_tree_ref, 2, 0, 0);
  gh_new_procedure ("tree-set!", (FN) tmg_tree_setS, 3, 0, 0);
  gh_new_procedure ("subtree", (FN) tmg_subtree, 2, 0, 0);
  gh_new_procedure ("tree-copy", (FN) tmg_tree_copy, 1, 0, 0);
  gh_new_procedure ("tree-label-extension?", (FN) tmg_tree_label_extensionP, 1, 0, 0);
  gh_new_procedure ("tree-multi-paragraph?", (FN) tmg_tree_multi_paragraphP, 1, 0, 0);
  gh_new_procedure ("tree-simplify", (FN) tmg_tree_simplify, 1, 0, 0);
  gh_new_procedure ("parse-texmacs", (FN) tmg_parse_texmacs, 1, 0, 0);
  gh_new_procedure ("serialize-texmacs", (FN) tmg_serialize_texmacs, 1, 0, 0);
  gh_new_procedure ("parse-texmacs-snippet", (FN) tmg_parse_texmacs_snippet, 1, 0, 0);
  gh_new_procedure ("serialize-texmacs-snippet", (FN) tmg_serialize_texmacs_snippet, 1, 0, 0);
  gh_new_procedure ("texmacs->scheme", (FN) tmg_texmacs_2scheme, 1, 0, 0);
  gh_new_procedure ("scheme->texmacs", (FN) tmg_scheme_2texmacs, 1, 0, 0);
  gh_new_procedure ("scheme-snippet->texmacs", (FN) tmg_scheme_snippet_2texmacs, 1, 0, 0);
  gh_new_procedure ("texmacs->verbatim", (FN) tmg_texmacs_2verbatim, 1, 0, 0);
  gh_new_procedure ("verbatim-snippet->texmacs", (FN) tmg_verbatim_snippet_2texmacs, 1, 0, 0);
  gh_new_procedure ("verbatim->texmacs", (FN) tmg_verbatim_2texmacs, 1, 0, 0);
  gh_new_procedure ("parse-latex", (FN) tmg_parse_latex, 1, 0, 0);
  gh_new_procedure ("parse-latex-document", (FN) tmg_parse_latex_document, 1, 0, 0);
  gh_new_procedure ("latex->texmacs", (FN) tmg_latex_2texmacs, 1, 0, 0);
  gh_new_procedure ("parse-xml", (FN) tmg_parse_xml, 1, 0, 0);
  gh_new_procedure ("parse-html", (FN) tmg_parse_html, 1, 0, 0);
  gh_new_procedure ("upgrade-tmml", (FN) tmg_upgrade_tmml, 1, 0, 0);
  gh_new_procedure ("get-texmacs-path", (FN) tmg_get_texmacs_path, 0, 0, 0);
  gh_new_procedure ("object->command", (FN) tmg_object_2command, 1, 0, 0);
  gh_new_procedure ("object->scheme-tree", (FN) tmg_object_2scheme_tree, 1, 0, 0);
  gh_new_procedure ("scheme-tree->object", (FN) tmg_scheme_tree_2object, 1, 0, 0);
  gh_new_procedure ("scheme-dialect", (FN) tmg_scheme_dialect, 0, 0, 0);
  gh_new_procedure ("cursor-start", (FN) tmg_cursor_start, 2, 0, 0);
  gh_new_procedure ("cursor-end", (FN) tmg_cursor_end, 2, 0, 0);
  gh_new_procedure ("support-ec-fonts?", (FN) tmg_support_ec_fontsP, 0, 0, 0);
  gh_new_procedure ("support-tt-fonts?", (FN) tmg_support_tt_fontsP, 0, 0, 0);
  gh_new_procedure ("use-ec-fonts?", (FN) tmg_use_ec_fontsP, 0, 0, 0);
  gh_new_procedure ("use-tt-fonts?", (FN) tmg_use_tt_fontsP, 0, 0, 0);
  gh_new_procedure ("set-font-type", (FN) tmg_set_font_type, 1, 0, 0);
  gh_new_procedure ("font-exists-in-tt?", (FN) tmg_font_exists_in_ttP, 1, 0, 0);
  gh_new_procedure ("texmacs-time", (FN) tmg_texmacs_time, 0, 0, 0);
  gh_new_procedure ("plugin-list", (FN) tmg_plugin_list, 0, 0, 0);
  gh_new_procedure ("eval-system", (FN) tmg_eval_system, 1, 0, 0);
  gh_new_procedure ("var-eval-system", (FN) tmg_var_eval_system, 1, 0, 0);
  gh_new_procedure ("get-locale-language", (FN) tmg_get_locale_language, 0, 0, 0);
  gh_new_procedure ("escape-quotes", (FN) tmg_escape_quotes, 1, 0, 0);
  gh_new_procedure ("escape-generic", (FN) tmg_escape_generic, 1, 0, 0);
  gh_new_procedure ("escape-verbatim", (FN) tmg_escape_verbatim, 1, 0, 0);
  gh_new_procedure ("system-wait", (FN) tmg_system_wait, 2, 0, 0);
  gh_new_procedure ("string->url", (FN) tmg_string_2url, 1, 0, 0);
  gh_new_procedure ("url", (FN) tmg_url, 2, 0, 0);
  gh_new_procedure ("url-system", (FN) tmg_url_system, 1, 0, 0);
  gh_new_procedure ("url-none", (FN) tmg_url_none, 0, 0, 0);
  gh_new_procedure ("url-any", (FN) tmg_url_any, 0, 0, 0);
  gh_new_procedure ("url-wildcard", (FN) tmg_url_wildcard, 1, 0, 0);
  gh_new_procedure ("url-parent", (FN) tmg_url_parent, 0, 0, 0);
  gh_new_procedure ("url-append", (FN) tmg_url_append, 2, 0, 0);
  gh_new_procedure ("url-or", (FN) tmg_url_or, 2, 0, 0);
  gh_new_procedure ("url->string", (FN) tmg_url_2string, 1, 0, 0);
  gh_new_procedure ("url-none?", (FN) tmg_url_noneP, 1, 0, 0);
  gh_new_procedure ("url-rooted-web?", (FN) tmg_url_rooted_webP, 1, 0, 0);
  gh_new_procedure ("url-concat?", (FN) tmg_url_concatP, 1, 0, 0);
  gh_new_procedure ("url-or?", (FN) tmg_url_orP, 1, 0, 0);
  gh_new_procedure ("url-ref", (FN) tmg_url_ref, 2, 0, 0);
  gh_new_procedure ("url-tail", (FN) tmg_url_tail, 1, 0, 0);
  gh_new_procedure ("url-suffix", (FN) tmg_url_suffix, 1, 0, 0);
  gh_new_procedure ("url-glue", (FN) tmg_url_glue, 2, 0, 0);
  gh_new_procedure ("url-unglue", (FN) tmg_url_unglue, 2, 0, 0);
  gh_new_procedure ("url-relative", (FN) tmg_url_relative, 2, 0, 0);
  gh_new_procedure ("url-expand", (FN) tmg_url_expand, 1, 0, 0);
  gh_new_procedure ("url-factor", (FN) tmg_url_factor, 1, 0, 0);
  gh_new_procedure ("url-delta", (FN) tmg_url_delta, 2, 0, 0);
  gh_new_procedure ("url-complete", (FN) tmg_url_complete, 2, 0, 0);
  gh_new_procedure ("url-resolve", (FN) tmg_url_resolve, 2, 0, 0);
  gh_new_procedure ("url-resolve-in-path", (FN) tmg_url_resolve_in_path, 1, 0, 0);
  gh_new_procedure ("url-exists?", (FN) tmg_url_existsP, 1, 0, 0);
  gh_new_procedure ("url-exists-in-path?", (FN) tmg_url_exists_in_pathP, 1, 0, 0);
  gh_new_procedure ("url-exists-in-tex?", (FN) tmg_url_exists_in_texP, 1, 0, 0);
  gh_new_procedure ("url-concretize", (FN) tmg_url_concretize, 1, 0, 0);
  gh_new_procedure ("url-materialize", (FN) tmg_url_materialize, 2, 0, 0);
  gh_new_procedure ("url-test?", (FN) tmg_url_testP, 2, 0, 0);
  gh_new_procedure ("url-regular?", (FN) tmg_url_regularP, 1, 0, 0);
  gh_new_procedure ("url-directory?", (FN) tmg_url_directoryP, 1, 0, 0);
  gh_new_procedure ("url-link?", (FN) tmg_url_linkP, 1, 0, 0);
  gh_new_procedure ("url-newer?", (FN) tmg_url_newerP, 2, 0, 0);
  gh_new_procedure ("url-temp", (FN) tmg_url_temp, 0, 0, 0);
  gh_new_procedure ("string-save", (FN) tmg_string_save, 2, 0, 0);
  gh_new_procedure ("string-load", (FN) tmg_string_load, 1, 0, 0);
  gh_new_procedure ("system-move", (FN) tmg_system_move, 2, 0, 0);
  gh_new_procedure ("system-copy", (FN) tmg_system_copy, 2, 0, 0);
  gh_new_procedure ("system-append", (FN) tmg_system_append, 2, 0, 0);
  gh_new_procedure ("system-remove", (FN) tmg_system_remove, 1, 0, 0);
  gh_new_procedure ("system-mkdir", (FN) tmg_system_mkdir, 1, 0, 0);
  gh_new_procedure ("string-search-forwards", (FN) tmg_string_search_forwards, 3, 0, 0);
  gh_new_procedure ("string-search-backwards", (FN) tmg_string_search_backwards, 3, 0, 0);
  gh_new_procedure ("string-replace", (FN) tmg_string_replace, 3, 0, 0);
  gh_new_procedure ("string-slash", (FN) tmg_string_slash, 1, 0, 0);
  gh_new_procedure ("string-unslash", (FN) tmg_string_unslash, 1, 0, 0);
  gh_new_procedure ("upcase-first", (FN) tmg_upcase_first, 1, 0, 0);
  gh_new_procedure ("locase-first", (FN) tmg_locase_first, 1, 0, 0);
  gh_new_procedure ("upcase-all", (FN) tmg_upcase_all, 1, 0, 0);
  gh_new_procedure ("locase-all", (FN) tmg_locase_all, 1, 0, 0);
  gh_new_procedure ("utf8->cork", (FN) tmg_utf8_2cork, 1, 0, 0);
  gh_new_procedure ("cork->utf8", (FN) tmg_cork_2utf8, 1, 0, 0);
  gh_new_procedure ("utf8->html", (FN) tmg_utf8_2html, 1, 0, 0);
  gh_new_procedure ("tm->xml-name", (FN) tmg_tm_2xml_name, 1, 0, 0);
  gh_new_procedure ("tm->xml-cdata", (FN) tmg_tm_2xml_cdata, 1, 0, 0);
  gh_new_procedure ("xml-name->tm", (FN) tmg_xml_name_2tm, 1, 0, 0);
  gh_new_procedure ("xml-cdata->tm", (FN) tmg_xml_cdata_2tm, 1, 0, 0);
  gh_new_procedure ("xml-unspace", (FN) tmg_xml_unspace, 3, 0, 0);
  gh_new_procedure ("connection-eval", (FN) tmg_connection_eval, 3, 0, 0);
  gh_new_procedure ("connection-cmd", (FN) tmg_connection_cmd, 3, 0, 0);
  gh_new_procedure ("path-inf?", (FN) tmg_path_infP, 2, 0, 0);
  gh_new_procedure ("path-inf-eq?", (FN) tmg_path_inf_eqP, 2, 0, 0);
  gh_new_procedure ("path-less?", (FN) tmg_path_lessP, 2, 0, 0);
  gh_new_procedure ("path-less-eq?", (FN) tmg_path_less_eqP, 2, 0, 0);
  gh_new_procedure ("widget-hlist", (FN) tmg_widget_hlist, 1, 0, 0);
  gh_new_procedure ("widget-named-hlist", (FN) tmg_widget_named_hlist, 2, 0, 0);
  gh_new_procedure ("widget-vlist", (FN) tmg_widget_vlist, 1, 0, 0);
  gh_new_procedure ("widget-named-vlist", (FN) tmg_widget_named_vlist, 2, 0, 0);
  gh_new_procedure ("widget-vmenu", (FN) tmg_widget_vmenu, 1, 0, 0);
  gh_new_procedure ("widget-tile", (FN) tmg_widget_tile, 2, 0, 0);
  gh_new_procedure ("widget-named-tile", (FN) tmg_widget_named_tile, 3, 0, 0);
  gh_new_procedure ("widget-harray", (FN) tmg_widget_harray, 2, 0, 0);
  gh_new_procedure ("widget-named-harray", (FN) tmg_widget_named_harray, 3, 0, 0);
  gh_new_procedure ("widget-switch", (FN) tmg_widget_switch, 2, 0, 0);
  gh_new_procedure ("widget-switch-init", (FN) tmg_widget_switch_init, 3, 0, 0);
  gh_new_procedure ("widget-optional", (FN) tmg_widget_optional, 1, 0, 0);
  gh_new_procedure ("widget-optional-init", (FN) tmg_widget_optional_init, 2, 0, 0);
  gh_new_procedure ("widget-glue", (FN) tmg_widget_glue, 4, 0, 0);
  gh_new_procedure ("widget-separator", (FN) tmg_widget_separator, 3, 0, 0);
  gh_new_procedure ("widget-text", (FN) tmg_widget_text, 3, 0, 0);
  gh_new_procedure ("widget-xpm", (FN) tmg_widget_xpm, 2, 0, 0);
  gh_new_procedure ("widget-menu-text", (FN) tmg_widget_menu_text, 4, 0, 0);
  gh_new_procedure ("widget-command-button-1", (FN) tmg_widget_command_button_1, 3, 0, 0);
  gh_new_procedure ("widget-command-button-2", (FN) tmg_widget_command_button_2, 3, 0, 0);
  gh_new_procedure ("widget-command-button-3", (FN) tmg_widget_command_button_3, 6, 0, 0);
  gh_new_procedure ("widget-pulldown-button", (FN) tmg_widget_pulldown_button, 2, 0, 0);
  gh_new_procedure ("widget-pullright-button", (FN) tmg_widget_pullright_button, 2, 0, 0);
  gh_new_procedure ("widget-balloon", (FN) tmg_widget_balloon, 2, 0, 0);
  gh_new_procedure ("widget-pulldown-button-lazy", (FN) tmg_widget_pulldown_button_lazy, 2, 0, 0);
  gh_new_procedure ("widget-pullright-button-lazy", (FN) tmg_widget_pullright_button_lazy, 2, 0, 0);
  gh_new_procedure ("widget-wait", (FN) tmg_widget_wait, 3, 0, 0);
  gh_new_procedure ("widget-box", (FN) tmg_widget_box, 5, 0, 0);
  gh_new_procedure ("object->make-widget", (FN) tmg_object_2make_widget, 1, 0, 0);
}
