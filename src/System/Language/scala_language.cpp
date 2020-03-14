
/******************************************************************************
* MODULE     : scala_language.cpp
* DESCRIPTION: the Scala language
* COPYRIGHT  : (C) 2014-2019  François Poulain, Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
******************************************************************************/

#include "analyze.hpp"
#include "impl_language.hpp"
#include "scheme.hpp"

extern tree the_et;

static int
line_number (tree t) {
  path p= obtain_ip (t);
  if (is_nil (p) || last_item (p) < 0) return -1;
  tree pt= subtree (the_et, reverse (p->next));
  if (!is_func (pt, DOCUMENT)) return -1;
  return p->item;
}

static int
number_of_lines (tree t) {
  path p= obtain_ip (t);
  if (is_nil (p) || last_item (p) < 0) return -1;
  tree pt= subtree (the_et, reverse (p->next));
  if (!is_func (pt, DOCUMENT)) return -1;
  return N(pt);
}

static tree
line_inc (tree t, int i) {
  if (i == 0) return t;
  path p= obtain_ip (t);
  if (is_nil (p) || last_item (p) < 0) return tree (ERROR);
  tree pt= subtree (the_et, reverse (p->next));
  if (!is_func (pt, DOCUMENT)) return tree (ERROR);
  if ((p->item + i < 0) || (p->item + i >= N(pt))) return tree (ERROR);
  return pt[p->item + i];
}

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

static bool
parse_string (string s, int& pos, bool force) {
  int n= N(s);
  static string delim;
  if (pos >= n) return false;
  if (s[pos] == '\"' || s[pos] == '\'') {
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

static void
parse_comment_multi_lines (string s, int& pos) {
  if (pos+1 < N(s) && s[pos] == '/' && s[pos+1] == '*')
    pos += 2;
}

static void
parse_end_comment (string s, int& pos) {
  if (pos+1 < N(s) && s[pos] == '*' && s[pos+1] == '/')
    pos += 2;
}

static bool
begin_comment (string s, int i) {
  bool comment= false;
  int opos, pos= 0;
  do {
    do {
      opos= pos;
      parse_string (s, pos, false);
      if (opos < pos) break;
      parse_comment_multi_lines (s, pos);
      if (opos < pos) {
        comment = true;
        break;
      }
      pos++;
    } while (false);
  } while (pos <= i);
  return comment;
}

static bool
end_comment (string s, int i) {
  int opos, pos= 0;
  do {
    do {
      opos= pos;
      parse_string (s, pos, false);
      if (opos < pos) break;
      parse_end_comment (s, pos);
      if (opos < pos && pos>i) return true;
      pos++;
    } while (false);
  } while (pos < N(s));
  return false;
}

static int
after_begin_comment (int i, tree t) {
  tree   t2= t;
  string s2= t->label;
  int  line= line_number (t2);
  do {
    if (begin_comment (s2, i)) return line;
    t2= line_inc (t2, -1);
    --line;
      // line_inc returns tree(ERROR) upon error
    if (!is_atomic (t2)) return -1; // FIXME
    s2= t2->label;
    i = N(s2) - 1;
  } while (line > -1);
  return -1;
}

static int
before_end_comment (int i, tree t) {
  int   end= number_of_lines (t);
  tree   t2= t;
  string s2= t2->label;
  int  line= line_number (t2);
  do {
    if (end_comment (s2, i)) return line;
    t2= line_inc (t2, 1);
    ++line;
      // line_inc returns tree(ERROR) upon error
    if (!is_atomic (t2)) return -1; // FIXME
    s2= t2->label;
    i = 0;
  } while (line <= end);
  return -1;
}

static bool
in_comment (int pos, tree t) {
  int beg= after_begin_comment (pos, t);
  if (beg >= 0) {
    int cur= line_number (t);
    int end= before_end_comment (pos, line_inc (t, beg - cur));
    return end >= beg && cur <= end;
  }
  return false;
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
      }
      pos= opos;
      pos++;
    }
    while (false);
  }
  while (pos <= start);
  if (type == none) return none;
  return decode_color ("scala", encode_color (type));
}
