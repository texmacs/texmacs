
/******************************************************************************
* MODULE     : cpp_language.cpp
* DESCRIPTION: the "cpp" language
* COPYRIGHT  : (C) 2008  Francis Jamet
*              (C) 2019-2020  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
******************************************************************************/

#include "analyze.hpp"
#include "impl_language.hpp"
#include "scheme.hpp"

static void parse_string (string s, int& pos);

cpp_language_rep::cpp_language_rep (string name):
  abstract_language_rep (name)
{
  number_parser.use_cpp_style ();
  array<string> starts;
  starts << string ("//");
  inline_comment_parser.set_starts (starts);

  array<char> escape_chars;
  escape_chars << '\\' << '\'' << '\"'
    << 'b' << 'f' << 'n' << 'r' << 't';
  escaped_char_parser.set_chars (escape_chars);
  // TODO: hex/octal escaped sequence

  string_parser.set_escaped_char_parser (escaped_char_parser);
  hashmap<string, string> pairs;
  pairs("\"") = "\"";
  pairs("\'")= "\'";
  string_parser.set_pairs(pairs);
}

text_property
cpp_language_rep::advance (tree t, int& pos) {
  string s= t->label;
  if (pos == N(s)) return &tp_normal_rep;

  if (blanks_parser.parse (s, pos))
    return &tp_space_rep;
  if (number_parser.parse (s, pos))
    return &tp_normal_rep;
  if (identifier_parser.parse (s, pos))
    return &tp_normal_rep;

  tm_char_forwards (s, pos);
  return &tp_normal_rep;
}

array<int>
cpp_language_rep::get_hyphens (string s) {
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
cpp_language_rep::hyphenate (string s, int after, string& left, string& right) {
  left = s (0, after);
  right= s (after, N(s));
}

static void
cpp_color_setup_constants (hashmap<string, string> & t) {
  string c= "constant";
  t ("true")= c;
  t ("false")= c;
  t ("cout")= c;
  t ("cin")= c;
  t ("cerr")= c;
  t ("NULL")= c;
  t ("null")= c;
  /*
  t ("Q_OBJECT")= c;
  t ("PIXEL")= c;
  t ("ASSERT")= c;
   */
}

static void
cpp_color_setup_keywords (hashmap<string, string> & t)  {
  string c= "keyword";
  t ("asm")= c;
  t ("auto")= c;
  t ("break")= c;
  t ("calloc")= c;
  t ("case")= c;
  t ("catch")= c;
  t ("class")= c;
  t ("concrete")= c;
  t ("constant")= c;
  t ("const")= c;
  t ("const_cast")= c;
  t ("continue")= c;
  t ("default")= c;
  t ("delete")= c;
  t ("do")= c;
  t ("dynamic_cast")= c;
  t ("free")= c;
  t ("else")= c;
  t ("enum")= c;
  t ("extern")= c;
  t ("explicit")= c;
  t ("export")= c;
  t ("for")= c;
  t ("friend")= c;
  t ("goto")= c;
  t ("if")= c;
  t ("inline")= c;
  t ("malloc")= c;
  t ("mutable")= c;
  t ("new")= c;
  t ("operator")= c;
  t ("private")= c;
  t ("protected")= c;
  t ("public")= c;
  t ("realloc")= c;
  t ("register")= c;
  t ("reinterpret_cast")= c;
  t ("return")= c ;
  t ("sizeof")= c;
  t ("static")= c;
  t ("static_cast")= c;
  t ("struct")= c;
  t ("switch")= c;
  t ("template")= c;
  t ("this")= c;
  t ("throw")= c;
  t ("to")= c;
  t ("try")= c;
  t ("typedef")= c;
  t ("typeid")= c;
  t ("typename")= c;
  t ("union")= c;
  t ("using")= c;
  t ("virtual")= c;
  t ("volatile")= c;
  t ("while")= c;
}

static void
cpp_color_setup_types (hashmap<string, string> & t) {
  string c= "constant_type";
  t ("bool")= c;
  t ("char")= c;
  t ("double")= c;
  t ("float")= c;
  t ("int")= c;
  t ("long")= c;
  t ("short")= c;
  t ("signed")= c;
  t ("unsigned")= c;
  t ("void")= c;
  t ("wchar_t")= c;
    // Just a few common ones in TeXmacs
  /* Update: Don't color non-basic types to avoid confusion
   t ("time_t")= c;
   t ("array")= c;
   t ("blackbox")= c;
   t ("box")= c;
   t ("command")= c;
   t ("event")= c;
   t ("hashmap")= c;
   t ("iterator")= c;
   t ("language")= c;
   t ("list")= c;
   t ("path")= c;
   t ("object")= c;
   t ("observer")= c;
   t ("rectangle")= c;
   t ("scheme_tree")= c;
   t ("selection")= c;
   t ("SI")= c;
   t ("string")= c;
   t ("tmscm")= c;
   t ("tree")= c;
   t ("url")= c;
   t ("widget")= c;
   */
}

static void
cpp_color_setup_otherlexeme (hashmap<string, string>& t) {
  string c= "black";
  t ("+")= c;
  t ("-")= c;
  t ("/")= c;
  t ("=")= c;
  t (".")= c;
  t ("<less>")= c;
  t ("gtr")= c;
  t ("|")= c;
  t ("!")= c;
  t ("...")= c;
  t (",")= c;
  t ("+=")= c;
  t ("-=")= c;
  t ("*=")= c;
  t ("/=")= c;
  t (",")= c;
  t (";")= c;
  t ("(")= c;
  t (")")= c;
  t ("[")= c;
  t ("]")= c;
  t ("{")= c;
  t ("}")= c;
  t ("<less><less>")= c;
  t ("<gtr><gtr>")= c;
  t ("<less>=")= c;
  t ("==")= c;
  t ("<gtr>=")= c;
  t ("&&")= c;
  t ("||")= c;
  t ("!=")= c;
  
}

static void
parse_other_lexeme (hashmap<string,string>& t, string s, int& pos) {
  int i;
  for (i=12; i>=1; i--)
    if (t->contains (s (pos, pos+i)))
      pos+= i;
}

static void
parse_diese (string s, int& pos) {
  if (s[pos] == '#') pos++;
}

void
cpp_language_rep::parse_preprocessing (string s, int & pos) {
  int i= pos;
  if (pos >= N(s) || is_digit (s[i])) return;
  while (i < N(s) && belongs_to_identifier (s[i])) i++;
  string r= s (pos, i);
  if (r == "include" ||
      r == "if" ||
      r == "ifdef" ||
      r == "ifndef" ||
      r == "else" ||
      r == "elif" ||
      r == "endif" ||
      r == "define" ||
      r == "undef" ||
      r == "pragma" ||
      r == "error") { pos=i; return; }
}

static bool end_preprocessing(string s) {
  int pos= N(s)-1;
  if (N(s) == 0) return false;
  while (s[pos] == ' ' && pos > 0) --pos;
  if (s[pos] == '/') return true;
  return false;
}

static bool begin_preprocessing(string s) {
  if (N(s)>0 && s[0]=='#') return true;
  return false;
}

static bool in_preprocessing (string s, tree t) {
  if (begin_preprocessing(s)) return true;
  tree t2= t;
  string s2= s;
  while (line_number(t2) > 0) {
    t2= line_inc(t2,-1);
      // line_inc return tree(ERROR) upon error
    if (!is_atomic(t2)) return false;
    s2= t2->label;
    if (!end_preprocessing(s2)) return false;
    if (begin_preprocessing(s2)) return true;
  }
  return false;
}

string
cpp_language_rep::get_color (tree t, int start, int end) {
  static bool setup_done= false;
  if (!setup_done) {
    cpp_color_setup_constants (colored);
    cpp_color_setup_keywords (colored);
    cpp_color_setup_types (colored);
    cpp_color_setup_otherlexeme (colored);
    setup_done= true;
  }
  
  static string none= "";
  if (start >= end) return none;
  if (in_comment (start, t))
    return decode_color ("cpp", encode_color ("comment"));
  string s= t->label;
  int  pos= 0;
  int opos= 0;
  string type;
  if (in_preprocessing(s, t)) {
    do {
      do {
        opos= pos;
        type= none;
        if (blanks_parser.parse (s, pos)) break;
        parse_diese (s, pos);
        if (opos < pos) {
          type= "preprocessor_directive";
          break;
        }
        parse_preprocessing (s, pos);
        if (opos < pos) {
          type= "preprocessor_directive";
          break;
        }
        pos++;
      } while (false);
    } while (pos <= start);
    if (type == "preprocessor_directive")
      decode_color("cpp", encode_color (type));
    return decode_color("cpp", encode_color("preprocessor"));
  }
  pos= 0;
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
      if (string_parser.parse (s, pos)) {
        type= "constant_string";
        break;
      }
      if (inline_comment_parser.parse (s, pos)) {
        type= "comment";
        break;
      }
      parse_keyword (colored, s, pos);
      if (opos < pos) {
        type= "keyword";
        break;
      }
      parse_type (colored, s, pos);
      if (opos < pos) {
        type= "constant_type";
        break;
      }
      parse_other_lexeme (colored, s, pos);  //not left parenthesis
      if (opos < pos)
        break;
      parse_constant (colored, s, pos);
      if (opos < pos) {
        type= "constant";
        break;
      }
      if (number_parser.parse (s, pos)) {
        type= "constant_number";
        break;
      }
      if (identifier_parser.parse (s, pos)) {
        type= none;
        break;
      }
      pos= opos + 1;
    } while (false);
  } while (pos <= start);
  if (type == none) return none;
  return decode_color ("cpp", encode_color (type));
}
