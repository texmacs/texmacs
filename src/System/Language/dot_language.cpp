
/******************************************************************************
* MODULE     : dot_language.cpp
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

dot_language_rep::dot_language_rep (string name):
  abstract_language_rep (name)
{
  array<string> starts;
  starts << string("//");
  inline_comment_parser.set_starts (starts);

  array<char> escape_chars;
  escape_chars << '\\' << '\'' << '\"'
    << 'b' << 'f' << 'n' << 'r' << 't';
  escaped_char_parser.set_chars (escape_chars);

  string_parser.set_escaped_char_parser (escaped_char_parser);
  hashmap<string, string> pairs;
  pairs("\"") = "\"";
  pairs("\'")= "\'";
  string_parser.set_pairs(pairs);

  keyword_parser.use_keywords_of_lang (name);
  operator_parser.use_operators_of_lang (name);
}

text_property
dot_language_rep::advance (tree t, int& pos) {
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
  if (identifier_parser.parse (s, pos)) {
    current_parser= identifier_parser.get_parser_name ();
    return &tp_normal_rep;
  }

  tm_char_forwards (s, pos);
  current_parser= "";

  return &tp_normal_rep;
}

array<int>
dot_language_rep::get_hyphens (string s) {
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
dot_language_rep::hyphenate (
  string s, int after, string& left, string& right)
{
  left = s (0, after);
  right= s (after, N(s));
}

string
dot_language_rep::get_color (tree t, int start, int end) {
  static string none= "";
  if (start >= end) return none;


  // Coloring as multi-line comment
  if (in_comment (start, t))
    return decode_color ("dot", encode_color ("comment"));

  // Coloring as inline comment
  int pos= 0;
  int opos= 0;
  string type= none;
  string s= t->label;
  while (pos <= start) {
    if (inline_comment_parser.can_parse (s, pos)) {
      return decode_color ("cpp", encode_color ("comment"));
    }
    pos ++;
  }

  pos= start;
  if (current_parser == "string_parser") {
    type= "constant_string";
  } else if (current_parser == "escaped_char_parser") {
    type= "constant_char";
  } else if (current_parser == "number_parser") {
    type= "constant_number";
  } else if (current_parser == "identifier_parser") {
    if (keyword_parser.parse (s, pos)) {
      string keyword= s(opos, pos);
      type= keyword_parser.get (keyword);
    }
    if (operator_parser.parse (s, pos)) {
      string oper= s(opos, pos);
      type= operator_parser.get (oper);
    }
  } else {
    type= none;
  }

  if (type == none) return none;
  return decode_color ("dot", encode_color (type));
}
