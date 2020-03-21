
/******************************************************************************
* MODULE     : scala_language.cpp
* DESCRIPTION: the Scala language
* COPYRIGHT  : (C) 2014-2020  François Poulain, Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
******************************************************************************/

#include "analyze.hpp"
#include "impl_language.hpp"
#include "scheme.hpp"

scala_language_rep::scala_language_rep (string name):
  abstract_language_rep (name)
{
  number_parser.use_scala_style ();

  array<string> starts;
  starts << string("//");
  inline_comment_parser.set_starts (starts);

  array<char> escape_chars;
  escape_chars << '\\' << '\'' << '\"'
    << 'b' << 'f' << 'n' << 'r' << 't';
  escaped_char_parser.set_chars (escape_chars);
  escaped_char_parser.support_hex_with_16_bits (true);

  string_parser.set_escaped_char_parser (escaped_char_parser);
  hashmap<string, string> pairs;
  pairs("\"") = "\"";
  pairs("\"\"\"") = "\"\"\"";
  pairs("\'")= "\'";
  string_parser.set_pairs(pairs);

  array<char> start_chars, extra_chars;
  // The ‘$’ character is reserved for compiler-synthesized identifiers.
  start_chars << '_' << '$';
  extra_chars << '_';
  identifier_parser.set_start_chars (start_chars);
  identifier_parser.set_extra_chars (extra_chars);
}

text_property
scala_language_rep::advance (tree t, int& pos) {
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
scala_language_rep::get_hyphens (string s) {
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
scala_language_rep::hyphenate (
  string s, int after, string& left, string& right)
{
  left = s (0, after);
  right= s (after, N(s));
}

static void
scala_color_setup_operator_openclose (hashmap<string, string> & t) {
  string c= "operator_openclose";
  t ("{")= c;
  t ("[")= c;
  t ("(")= c;
  t (")")= c;
  t ("]")= c;
  t ("}")= c;
}

static void
scala_color_setup_constants (hashmap<string, string> & t) {
  string c= "constant";
  t ("false")= c;
  t ("true")= c;
  t ("null")= c;
  
  // type
  t ("Byte")= c;
  t ("Short")= c;
  t ("Int")= c;
  t ("Long")= c;
  t ("Char")= c;
  t ("String")= c;
  t ("Float")= c;
  t ("Double")= c;
  t ("Boolean")= c;
  
  // collection
  t ("Array")= c;
  t ("List")= c;
  t ("Map")= c;
  t ("Set")= c;
  t ("Function")= c;
  t ("Class")= c;
  
  // functional operator
  t ("aggregate")= c;
  t ("collect")= c;
  t ("map")= c;
  t ("filter")= c;
  t ("filterNot")= c;
  t ("foreach")= c;
  t ("forall")= c;
  t ("fold")= c;
  t ("foldLeft")= c;
  t ("foldRight")= c;
  t ("reduce")= c;
  t ("reduceLeft")= c;
  t ("reduceRight")= c;
  t ("scan")= c;
  t ("scanLeft")= c;
  t ("scanRight")= c;
  t ("zip")= c;
  t ("unzip")= c;
  t ("flatMap")= c;
  t ("grouped")= c;
  t ("groupBy")= c;
}

static void
scala_color_setup_constant_exceptions (hashmap<string, string> & t) {
  string c= "constant";
  t ("IllegalArgumentException")= c;
  t ("NullPointerException")= c;
  t ("Exception")= c;
  t ("RuntimeException")= c;
}

static void
scala_color_setup_declare_class (hashmap<string, string> & t) {
  string c= "declare_type";
  t ("class")= c;
  t ("object")= c;
  t ("trait")= c;
}

static void
scala_color_setup_declare_function (hashmap<string, string> & t) {
  string c= "declare_function";
  t ("def")= c;
}

static void
scala_color_setup_keywords (hashmap<string, string> & t) {
  string c= "keyword";
  t ("abstract")= c;
  t ("case")= c;
  t ("extends")= c;
  t ("implicit")= c;
  t ("import")= c;
  t ("lazy")= c;
  t ("match")= c;
  t ("new")= c;
  t ("override")= c;
  t ("package")= c;
  t ("private")= c;
  t ("protected")= c;
  t ("requires")= c;
  t ("sealed")= c;
  t ("super")= c;
  t ("this")= c;
  t ("throw")= c;
  t ("type")= c;
  t ("val") = c;
  t ("var") = c;
  t ("with")= c;
}

static void
scala_color_setup_keywords_conditional (hashmap<string, string> & t) {
  string c= "keyword_conditional";
  t ("break")= c;
  t ("do")= c;
  t ("else")= c;
  t ("for")= c;
  t ("if")= c;
  t ("while")= c;
}

static void
scala_color_setup_keywords_control (hashmap<string, string> & t) {
  string c= "keyword_control";
  t ("catch")= c;
  t ("final")= c;
  t ("finally")= c;
  t ("return")= c;
  t ("try")= c;
  t ("yield")= c;
}

static void
scala_color_setup_operator (hashmap<string, string>& t) {
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
  t ("<gtr><gtr><gtr>")= c;
  
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
  t ("<gtr><gtr>=")= c;
  t ("<less><less>=")= c;
}

static void
scala_color_setup_operator_special (hashmap<string, string> & t) {
  string c= "operator_special";
  t (":")= c;
  t ("=<gtr>")= c;
  t ("::")= c;
  t (":::")= c;
  t ("++")= c;
  t ("+:")= c;
  t (":+")= c;
  t ("++:")= c;
  t ("/:")= c;
  t (":\\")= c;
  t ("<less>-")= c;
}

static void
scala_color_setup_operator_decoration (hashmap<string, string> & t) {
  string c= "operator_decoration";
  t ("@")= c;
}

static void
scala_color_setup_operator_field (hashmap<string, string> & t) {
  t (".")= "operator_field";
}

string
scala_language_rep::parse_keywords (hashmap<string,string>& t, string s, int& pos) {
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
scala_language_rep::parse_operators (hashmap<string,string>& t, string s, int& pos) {
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
scala_language_rep::get_color (tree t, int start, int end) {
  static bool setup_done= false;
  if (!setup_done) {
    scala_color_setup_constants (colored);
    scala_color_setup_constant_exceptions (colored);
    scala_color_setup_declare_class (colored);
    scala_color_setup_declare_function (colored);
    scala_color_setup_keywords (colored);
    scala_color_setup_keywords_conditional (colored);
    scala_color_setup_keywords_control (colored);
    scala_color_setup_operator (colored);
    scala_color_setup_operator_special (colored);
    scala_color_setup_operator_decoration (colored);
    scala_color_setup_operator_openclose (colored);
    scala_color_setup_operator_field (colored);
    setup_done= true;
  }

  static string none= "";
  if (start >= end) return none;
  if (in_comment (start, t))
    return decode_color ("scala", encode_color ("comment"));

  string s= t->label;
  int pos= 0;
  int opos= 0;
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
  return decode_color ("scala", encode_color (type));
}
