
/******************************************************************************
* MODULE     : common_language.cpp
* DESCRIPTION: the DOT language
* COPYRIGHT  : (C) 2020  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
******************************************************************************/

#include "analyze.hpp"
#include "impl_language.hpp"
#include "scheme.hpp"
#include "tree.hpp"
#include "iterator.hpp"

common_language_rep::common_language_rep (string name):
  abstract_language_rep (name)
{
  debug_packrat << "Building the " * name * " language parser" << LF;

  string use_modules= "(use-modules (prog " * name * "-lang))";
  eval (use_modules);

  // Load (<name>-keywords)
  string get_list_of_keywords_tree= "(map tm->tree (" * name * "-keywords))";
  list<tree> l= as_list_tree (eval (get_list_of_keywords_tree));
  for (int i=0; i<N(l); i++) {
    tree group_words= l[i];
    string group= get_label (group_words);
    for (int j=0; j<N(group_words); j++) {
      string word= get_label (group_words[j]);
      keyword_parser.put (word, group);
    }
  }

  // Load (<name>-operators)
  string get_list_of_operators_tree= "(map tm->tree (" * name * "-operators))";
  list<tree> l_oper= as_list_tree (eval (get_list_of_operators_tree));
  for (int i=0; i<N(l_oper); i++) {
    tree group_words= l_oper[i];
    string group= get_label (group_words);
    for (int j=0; j<N(group_words); j++) {
      string word= get_label (group_words[j]);
      operator_parser.put (word, group);
    }
  }

  // Load (<name>-numbers)
  string get_list_of_numbers_tree= "(map tm->tree (" * name * "-numbers))";
  list<tree> l_num= as_list_tree (eval (get_list_of_numbers_tree));
  for (int i=0; i<N(l_num); i++) {
    tree feature= l_num[i];
    string name= get_label (feature);
    if (name == "bool_features") {
      for (int j=0; j<N(feature); j++) {
        string key= get_label (feature[j]);
        number_parser.insert_bool_feature (key);
      }
    } else if (name == "separator" && N(feature) == 1) {
      string key= get_label (feature[0]);
      number_parser.support_separator (key);
    }
  }

  // Load (<name>-inline-comment-starts)
  list<string> inline_comment_starts_list=
    as_list_string (eval ("(" * name * "-inline-comment-starts)"));
  array<string> inline_comment_starts;
  for (int i=0; i<N(inline_comment_starts_list); i++) {
    inline_comment_starts << inline_comment_starts_list[i];
  }
  inline_comment_parser.set_starts (inline_comment_starts);

  // Load (<name>-escape-sequences)
  list<string> get_list_of_escapes=
    as_list_string (eval ("(" * name * "-escape-sequences)"));
  list<tree> l_escape= as_list_tree (eval (get_list_of_numbers_tree));
  for (int i=0; i<N(l_escape); i++) {
    tree feature= l_escape[i];
    string name= get_label (feature);
    if (name == "bool_features") {
      for (int j=0; j<N(feature); j++) {
        string key= get_label (feature[j]);
        escaped_char_parser.insert_bool_feature (key);
      }
    } else if (name == "sequences") {
      array<string> escape_seq;
      for (int j=0; j<N(feature); j++) {
        string key= get_label (feature[j]);
        escape_seq << key;
      }
      escaped_char_parser.set_sequences (escape_seq);
    }
  }

  string_parser.set_escaped_char_parser (escaped_char_parser);
  hashmap<string, string> pairs;
  pairs("\"") = "\"";
  pairs("\'")= "\'";
  string_parser.set_pairs(pairs);
}

text_property
common_language_rep::advance (tree t, int& pos) {
  string s= t->label;
  if (pos>=N(s)) return &tp_normal_rep;

  if (string_parser.unfinished ()) {
    if (string_parser.escaped () && string_parser.parse_escaped (s, pos)) {
      current_parser= escaped_char_parser.get_parser_name ();
      return &tp_normal_rep;
    }
    if (string_parser.parse (s, pos)) {
      current_parser= string_parser.get_parser_name ();
      return &tp_normal_rep;
    }
  }

  if (blanks_parser.parse (s, pos)) {
    current_parser= blanks_parser.get_parser_name ();
    return &tp_space_rep;
  }
  if (string_parser.parse (s, pos)) {
    current_parser= string_parser.get_parser_name ();
    return &tp_normal_rep;
  }
  if (number_parser.parse (s, pos)) {
    current_parser= number_parser.get_parser_name ();
    return &tp_normal_rep;
  }
  if (operator_parser.parse (s, pos)) {
    current_parser= operator_parser.get_parser_name ();
    return &tp_normal_rep;
  }
  if (keyword_parser.parse (s, pos)) {
    current_parser= keyword_parser.get_parser_name ();
    return &tp_normal_rep;
  }
  if (identifier_parser.parse (s, pos)) {
    current_parser= identifier_parser.get_parser_name ();
    return &tp_normal_rep;
  }

  tm_char_forwards (s, pos);
  current_parser= "";

  return &tp_normal_rep;
}

array<int>
common_language_rep::get_hyphens (string s) {
  int i;
  array<int> penalty (N(s)+1);
  penalty[0]= HYPH_INVALID;
  for (i=1; i<N(s); i++)
    if (s[i-1] == '-' && is_alpha (s[i]))
      penalty[i]= HYPH_STD;
    else penalty[i]= HYPH_INVALID;
  penalty[i]= HYPH_INVALID;
  return penalty;
}

void
common_language_rep::hyphenate (
  string s, int after, string& left, string& right)
{
  left = s (0, after);
  right= s (after, N(s));
}

string
common_language_rep::get_color (tree t, int start, int end) {
  static string none= "";
  if (start >= end) return none;


  // Coloring as multi-line comment
  if (in_comment (start, t))
    return decode_color (lan_name, encode_color ("comment"));

  string type= none;
  string s= t->label;
  
  // Coloring as inline comment
  int pos= 0;
  while (pos <= start) {
    if (inline_comment_parser.can_parse (s, pos)) {
      return decode_color (lan_name, encode_color ("comment"));
    }
    pos ++;
  }

  if (current_parser == "string_parser") {
    type= "constant_string";
  } else if (current_parser == "escaped_char_parser") {
    type= "constant_char";
  } else if (current_parser == "number_parser") {
    type= "constant_number";
  } else if (current_parser == "operator_parser") {
    string oper= s(start, end);
    type= operator_parser.get (oper);
  } else if (current_parser == "keyword_parser") {
    string keyword= s(start, end);
    type= keyword_parser.get (keyword);
  } else {
    type= none;
  }

  if (type == none) return none;
  return decode_color (lan_name, encode_color (type));
}

