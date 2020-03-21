/******************************************************************************
* MODULE     : java_language.cpp
* DESCRIPTION: the Java language
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
******************************************************************************/

#include "analyze.hpp"
#include "impl_language.hpp"
#include "scheme.hpp"

java_language_rep::java_language_rep (string name):
  abstract_language_rep (name)
{
  number_parser.use_java_style ();

  array<string> starts;
  starts << string("//");
  inline_comment_parser.set_starts (starts);

  array<char> escape_chars;
  escape_chars << '\\' << '\'' << '\"'
    << 'b' << 'f' << 'n' << 'r' << 't';
  escaped_char_parser.set_chars (escape_chars);
  escaped_char_parser.support_octal_upto_3_digits (true);
  escaped_char_parser.support_hex_with_16_bits (true);

  string_parser.set_escaped_char_parser (escaped_char_parser);
  hashmap<string, string> pairs;
  pairs("\"") = "\"";
  pairs("\'")= "\'";
  string_parser.set_pairs(pairs);
}

text_property
java_language_rep::advance (tree t, int& pos) {
  string s= t->label;
  if (pos==N(s))
    return &tp_normal_rep;

  if (blanks_parser.parse (s, pos))
    return &tp_space_rep;
  if (escaped_char_parser.parse (s, pos))
    return &tp_normal_rep;
  if (number_parser.parse (s, pos))
    return &tp_normal_rep;
  if (identifier_parser.parse (s, pos))
    return &tp_normal_rep;

  tm_char_forwards (s, pos);
  return &tp_normal_rep;
}

array<int>
java_language_rep::get_hyphens (string s) {
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
java_language_rep::hyphenate (
  string s, int after, string& left, string& right)
{
  left = s (0, after);
  right= s (after, N(s));
}

static void
java_color_setup_operator_openclose (hashmap<string, string> & t) {
  string c= "operator_openclose";
  t ("{")= c;
  t ("[")= c;
  t ("(")= c;
  t (")")= c;
  t ("]")= c;
  t ("}")= c;
}

static void
java_color_setup_constants (hashmap<string, string> & t) {
  string c= "constant";
  t ("false")= c;
  t ("true")= c;
  t ("null")= c;
  
  // type
  t ("boolean")= c;
  t ("byte")= c;
  t ("char")= c;
  t ("const")= c;
  t ("double")= c;
  t ("final")= c;
  t ("float")= c;
  t ("int")= c;
  t ("long")= c;
  t ("short")= c;
  t ("static")= c;
  t ("void")= c;
}

static void
java_color_setup_constant_exceptions (hashmap<string, string> & t) {
  string c= "constant";
  t ("IllegalArgumentException")= c;
  t ("NullPointerException")= c;
  t ("Exception")= c;
  t ("RuntimeException")= c;
}

static void
java_color_setup_declare_class (hashmap<string, string> & t) {
  string c= "declare_type";
  t ("class")= c;
  t ("interface")= c;
}

static void
java_color_setup_declare_function (hashmap<string, string> & t) {
  string c= "declare_function";
  t ("def")= c;
}

static void
java_color_setup_keywords (hashmap<string, string> & t) {
  string c= "keyword";
  t ("abstract")= c;
  t ("case")= c;
  t ("default")= c;
  t ("enum")= c;
  t ("extends")= c;
  t ("import")= c;
  t ("implements")= c;
  t ("instanceof")= c;
  t ("native")= c;
  t ("new")= c;
  t ("override")= c;
  t ("package")= c;
  t ("private")= c;
  t ("protected")= c;
  t ("public")= c;
  t ("super")= c;
  t ("synchronized")= c;
  t ("this")= c;
  t ("transient")= c;
  t ("with")= c;
  t ("volatile")= c;
}

static void
java_color_setup_keywords_conditional (hashmap<string, string> & t) {
  string c= "keyword_conditional";
  t ("do")= c;
  t ("else")= c;
  t ("for")= c;
  t ("goto")= c;
  t ("if")= c;
  t ("switch")= c;
  t ("while")= c;
}

static void
java_color_setup_keywords_control (hashmap<string, string> & t) {
  string c= "keyword_control";
  t ("break")= c;
  t ("continue")= c;
  t ("catch")= c;
  t ("final")= c;
  t ("finally")= c;
  t ("return")= c;
  t ("throw")= c;
  t ("throws")= c;
  t ("try")= c;
  t ("yield")= c;
}

static void
java_color_setup_operator (hashmap<string, string>& t) {
  string c= "operator";
  t ("&&")= c;
  t ("||")= c;
  t ("!")= c;

  t ("+")= c;
  t ("-")= c;
  t ("/")= c;
  t ("*")= c;
  t ("%")= c;
  
  t ("|")= c;
  t ("&")= c;
  t ("^")= c;
  
  t ("<less><less>")= c;
  t ("<gtr><gtr>")= c;
  t ("==")= c;
  t ("!=")= c;
  t ("<less>")= c;
  t ("<gtr>")= c;
  t ("<less>=")= c;
  t ("<gtr>=")= c;

  t ("=")= c;

  t ("+=")= c;
  t ("-=")= c;
  t ("/=")= c;
  t ("*=")= c;
  t ("%=")= c;
  t ("|=")= c;
  t ("&=")= c;
  t ("^=")= c;
  t (":")= c;
}

static void
java_color_setup_operator_special (hashmap<string, string> & t) {
  string c= "operator_special";
  t ("-<gtr>")= c;
}

static void
java_color_setup_operator_decoration (hashmap<string, string> & t) {
  string c= "operator_decoration";
  t ("@")= c;
}

static void
java_color_setup_operator_field (hashmap<string, string> & t) {
  string c= "operator_field";
  t (".")= c;
  t ("::")= c;
}

string
java_language_rep::parse_keywords (hashmap<string,string>& t, string s, int& pos) {
  int i= pos;
  if (pos>=N(s)) return "";
  if (is_digit (s[i])) return "";
  while ((i<N(s)) && belongs_to_identifier (s[i])) i++;
  string r= s (pos, i);
  if (t->contains (r)) {
    string tr= t(r);
    if (tr == "keyword_conditional" ||
        tr == "keyword_control"      ||
        tr == "keyword"              ||
        tr == "declare_type"         ||
        tr == "declare_function"     ||
        tr == "constant") {
      pos=i;
      return tr;
    }
  }
  return "";
}

string
java_language_rep::parse_operators (hashmap<string,string>& t, string s, int& pos) {
  int i;
  for (i=12; i>=1; i--) {
    string r=s(pos,pos+i);
    if (t->contains (r)) {
      string tr= t(r);
      if (tr == "operator"          ||
          tr == "operator_field"    ||
          tr == "operator_special"  ||
          tr == "operator_openclose") {
        pos=pos+i;
        return tr;
      }
      else if (t(r) == "operator_decoration") {
        pos=pos+i;
        while ((pos<N(s)) && belongs_to_identifier (s[pos])) pos++;
        return "operator_special";
      }
    }
  }
  return "";
}

string
java_language_rep::get_color (tree t, int start, int end) {
  static bool setup_done= false;
  if (!setup_done) {
    java_color_setup_constants (colored);
    java_color_setup_constant_exceptions (colored);
    java_color_setup_declare_class (colored);
    java_color_setup_declare_function (colored);
    java_color_setup_keywords (colored);
    java_color_setup_keywords_conditional (colored);
    java_color_setup_keywords_control (colored);
    java_color_setup_operator (colored);
    java_color_setup_operator_special (colored);
    java_color_setup_operator_decoration (colored);
    java_color_setup_operator_openclose (colored);
    java_color_setup_operator_field (colored);
    setup_done= true;
  }

  static string none= "";
  if (start >= end) return none;
  if (in_comment (start, t))
    return decode_color ("java", encode_color ("comment"));

  int pos= 0;
  int opos=0;
  string s= t->label;
  string type;
  string_parser.reset ();

  do {
    type= none;
    do {
      opos= pos;

      if (string_parser.unfinished ()) {
        if (string_parser.escaped () && string_parser.parse_escaped (s, pos)) {
          type= "constant_char";
          break;
        }
        if (string_parser.parse (s, pos)) {
          type= "constant_string";
          break;
        }
      }
      if (blanks_parser.parse (s, pos)) break;
      if (inline_comment_parser.parse (s, pos)) {
        type= "comment";
        break;
      }
      if (string_parser.parse (s, pos)) {
        type= "constant_string";
        break;
      }
      type= parse_keywords (colored, s, pos);
      if (opos < pos) {
        break;
      }
      if (number_parser.parse (s, pos)) {
        type= "constant_number";
        break;
      }
      type= parse_operators (colored, s, pos);
      if (opos < pos) {
        break;
      }
      parse_identifier (colored, s, pos);
      if (opos < pos) {
        type= none;
        break;
      }

      pos= opos;
      pos++;
    } while (false);
  } while (pos <= start);
  if (type == none) return none;
  return decode_color ("java", encode_color (type));
}
