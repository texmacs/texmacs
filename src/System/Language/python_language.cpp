
/******************************************************************************
* MODULE     : python_language.cpp
* DESCRIPTION: the Python language
* COPYRIGHT  : (C) 2014-2020  François Poulain, Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
******************************************************************************/

#include "analyze.hpp"
#include "impl_language.hpp"

python_language_rep::python_language_rep (string name):
  abstract_language_rep (name)
{
  number_parser.use_python_style ();

  array<string> starts;
  starts << string("#");
  inline_comment_parser.set_starts (starts);

  array<char> escape_chars;
  escape_chars << '\\' << '\'' << '\"'
    << 'a' << 'b' << 'f' << 'n' << 'r' << 't' << 'v';
  escaped_char_parser.set_chars (escape_chars);

  array<string> escape_strings;
  escape_strings << string("newline");
  escaped_char_parser.set_strings (escape_strings);

  escaped_char_parser.support_hex_with_8_bits (true);
  escaped_char_parser.support_hex_with_16_bits (true);
  escaped_char_parser.support_hex_with_32_bits (true);
  escaped_char_parser.support_octal_upto_3_digits (true);

  keyword_parser.use_keywords_of_lang (name);
  operator_parser.use_operators_of_lang (name);
}

text_property
python_language_rep::advance (tree t, int& pos) {
  int opos= pos;
  string s= t->label;
  if (pos==N(s))
    return &tp_normal_rep;
  char c= s[pos];
  if (c == ' ') {
    pos++;
    return &tp_space_rep;
  }
  if (escaped_char_parser.parse (s, pos)) {
    return &tp_normal_rep;
  }
  if (number_parser.parse (s, pos)) {
    return &tp_normal_rep;
  }
  if (belongs_to_identifier (c)) {
    parse_alpha (s, pos);
    return &tp_normal_rep;
  }
  tm_char_forwards (s, pos);
  if (opos == pos) {
    pos= pos + 1;
    cerr << "Python syntax parsing failed to advance" << LF;
    cerr << pos << ":" << s << LF;
  }
  return &tp_normal_rep;
}

array<int>
python_language_rep::get_hyphens (string s) {
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
python_language_rep::hyphenate (
  string s, int after, string& left, string& right)
{
  left = s (0, after);
  right= s (after, N(s));
}

static bool
parse_string (string s, int& pos, bool force) {
  int n= N(s);
  static string delim;
  if (pos >= n) return false;
  if (test (s, pos, "\"\"\"") || test (s, pos, "\'\'\'")) {
    delim= s(pos, pos+3);
    pos+= N(delim);
  }
  else if (s[pos] == '\"' || s[pos] == '\'') {
    delim= s(pos, pos+1);
    pos+= N(delim);
  }
  else if (!force)
    return false;
  while (pos<n && !test (s, pos, delim)) {
    if (s[pos] == '\\') {
      return true;
    }
    else
      pos++;
  }
  if (test (s, pos, delim))
    pos+= N(delim);
  return false;
}
 
string
python_language_rep::get_color (tree t, int start, int end) {
  static string none= "";
  if (start >= end) return none;
  string s= t->label;
  int pos= 0;
  int opos=0;
  string type;
  bool in_str= false;
  bool in_esc= false;
  do {
    type= none;
    do {
      opos= pos;
      if (in_str) {
        in_esc= parse_string (s, pos, true);
        in_str= false;
        if (opos < pos) {
          type= "constant_string";
          break;
        }
      }
      else if (in_esc) {
        in_esc= false;
        in_str= true;
        if (escaped_char_parser.parse (s, pos)) {
          type= "constant_char";
          break;
        }
      }
      else {
        if (blanks_parser.parse (s, pos)) break;
        if (inline_comment_parser.parse (s, pos)) {
          type= "comment";
          break;
        }
        in_esc= parse_string (s, pos, false);
        if (opos < pos) {
          type= "constant_string";
          break;
        }
        if (number_parser.parse(s, pos)) {
          type= "constant_number";
          break;
        }
        if (keyword_parser.parse (s, pos)) {
          string keyword= s(opos, pos);
          type= keyword_parser.get (keyword);
          break;
        }
        if (operator_parser.parse (s, pos)) {
          string oper= s(opos, pos);
          type= operator_parser.get (oper);
          break;
        }
        parse_identifier (colored, s, pos);
        if (opos < pos) {
          type= none;
          break;
        }
      }
      pos= opos;
      pos++;
    }
    while (false);
  }
  while (pos <= start);
  if (type == none) return none;
  return decode_color ("python", encode_color (type));
}
