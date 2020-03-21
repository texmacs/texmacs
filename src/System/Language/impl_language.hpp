
/******************************************************************************
* MODULE     : impl_language.hpp
* COPYRIGHT  : (C) 1999-2020  Joris van der Hoeven, Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef IMPL_LANGUAGE_H
#define IMPL_LANGUAGE_H

#include "language.hpp"
#include "number_parser.hpp"
#include "inline_comment_parser.hpp"
#include "blanks_parser.hpp"
#include "escaped_char_parser.hpp"
#include "keyword_parser.hpp"
#include "operator_parser.hpp"
#include "identifier_parser.hpp"
#include "string_parser.hpp"

extern text_property_rep tp_normal_rep;
extern text_property_rep tp_hyph_rep;
extern text_property_rep tp_thin_space_rep;
extern text_property_rep tp_space_rep;
extern text_property_rep tp_dspace_rep;
extern text_property_rep tp_nb_thin_space_rep;
extern text_property_rep tp_nb_space_rep;
extern text_property_rep tp_nb_dspace_rep;
extern text_property_rep tp_period_rep;
extern text_property_rep tp_cjk_normal_rep;
extern text_property_rep tp_cjk_no_break_rep;
extern text_property_rep tp_cjk_period_rep;
extern text_property_rep tp_cjk_wide_period_rep;
extern text_property_rep tp_cjk_no_break_period_rep;
extern text_property_rep tp_half_rep;
extern text_property_rep tp_operator_rep;
extern text_property_rep tp_short_apply_rep;
extern text_property_rep tp_apply_rep;

int line_number (tree t);
int number_of_lines (tree t);
tree line_inc (tree t, int i);

struct abstract_language_rep: language_rep {
  hashmap<string,string> colored;
  blanks_parser_rep blanks_parser;
  inline_comment_parser_rep inline_comment_parser;
  number_parser_rep number_parser;
  escaped_char_parser_rep escaped_char_parser;
  keyword_parser_rep keyword_parser;
  operator_parser_rep operator_parser;
  identifier_parser_rep identifier_parser;
  string_parser_rep string_parser;

  abstract_language_rep (string s): language_rep(s) {};
  virtual bool belongs_to_identifier (char c);
  void parse_identifier (hashmap<string, string>& t, string s, int& pos);
  void parse_type (hashmap<string,string>& t, string s, int& pos);
  void parse_keyword (hashmap<string,string>& t, string s, int& pos);
  void parse_constant (hashmap<string,string>& t, string s, int& pos);
};

struct verb_language_rep: language_rep {
  verb_language_rep (string name);
  text_property advance (tree t, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
  string get_color (tree t, int start, int end);
};

struct scheme_language_rep: language_rep {
  hashmap<string,string> colored;
  scheme_language_rep (string name);
  text_property advance (tree t, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
  string get_color (tree t, int start, int end);
};

struct mathemagix_language_rep: abstract_language_rep {
  mathemagix_language_rep (string name);
  text_property advance (tree t, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
  string get_color (tree t, int start, int end);

  bool belongs_to_identifier (char c);
  void parse_identifier_or_markup (hashmap<string, string>& t,
    string s, int& pos, bool postfix, bool& is_markup);
  void parse_keyword (hashmap<string,string>& t, string s, int& pos);
  void parse_modifier (hashmap<string,string>& t, string s, int& pos);
  void parse_class (hashmap<string,string>& t, string s, int& pos);
  void parse_postfix (hashmap<string,string>& t, string s, int& pos);
  void parse_constant (hashmap<string,string>& t, string s, int& pos);
};

struct r_language_rep: abstract_language_rep {
  r_language_rep (string name);
  text_property advance (tree t, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
  string get_color (tree t, int start, int end);

  bool belongs_to_identifier (char c);
  void parse_identifier (hashmap<string, string>& t, string s, int& pos);
  void parse_identifier_or_markup (hashmap<string, string>& t,
    string s, int& pos, bool postfix, bool& is_markup);
  void parse_keyword (hashmap<string,string>& t, string s, int& pos);
  void parse_modifier (hashmap<string,string>& t, string s, int& pos);
  void parse_class (hashmap<string,string>& t, string s, int& pos);
  void parse_postfix (hashmap<string,string>& t, string s, int& pos);
  void parse_constant (hashmap<string,string>& t, string s, int& pos);
};


struct cpp_language_rep: abstract_language_rep {
  cpp_language_rep (string name);
  text_property advance (tree t, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
  string get_color (tree t, int start, int end);

  void parse_preprocessing (string s, int & pos);
};

struct dot_language_rep: abstract_language_rep {
  dot_language_rep (string name);
  text_property advance (tree t, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
  string get_color (tree t, int start, int end);
};

struct scilab_language_rep: abstract_language_rep {
  scilab_language_rep (string name);
  text_property advance (tree t, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
  string get_color (tree t, int start, int end);

  bool belongs_to_identifier (char c);
  string parse_keywords (hashmap<string,string>& t, string s, int& pos);
  string parse_operators (hashmap<string,string>& t, string s, int& pos);
};

struct python_language_rep: abstract_language_rep {
  python_language_rep (string name);
  text_property advance (tree t, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
  string get_color (tree t, int start, int end);

  string parse_keywords (hashmap<string,string>& t, string s, int& pos);
  string parse_operators (hashmap<string,string>& t, string s, int& pos);
};

struct java_language_rep: abstract_language_rep {
  java_language_rep (string name);
  text_property advance (tree t, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
  string get_color (tree t, int start, int end);

  string parse_keywords (hashmap<string,string>& t, string s, int& pos);
  string parse_operators (hashmap<string,string>& t, string s, int& pos);
};

struct scala_language_rep: abstract_language_rep {
  scala_language_rep (string name);
  text_property advance (tree t, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
  string get_color (tree t, int start, int end);

  string parse_keywords (hashmap<string,string>& t, string s, int& pos);
  string parse_operators (hashmap<string,string>& t, string s, int& pos);
};

struct fortran_language_rep: abstract_language_rep {
  fortran_language_rep (string name);
  text_property advance (tree t, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
  string get_color (tree t, int start, int end);

  string parse_keywords (hashmap<string,string>& t, string s, int& pos);
  string parse_operators (hashmap<string,string>& t, string s, int& pos);
};

#endif // defined IMPL_LANGUAGE_H
