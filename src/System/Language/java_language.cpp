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

static void parse_escaped_char (string s, int& pos);

java_language_rep::java_language_rep (string name):
  abstract_language_rep (name), colored ("")
{
  number_parser.use_java_style ();
}

text_property
java_language_rep::advance (tree t, int& pos) {
  string s= t->label;
  if (pos==N(s))
    return &tp_normal_rep;
  char c= s[pos];
  if (c == ' ') {
    pos++;
    return &tp_space_rep;
  }
  if (c == '\\') {
    parse_escaped_char (s, pos);
    return &tp_normal_rep;
  }
  if (pos+2 < N(s) && s[pos] == '0' &&
       (s[pos+1] == 'x' || s[pos+1] == 'X' )) {
    number_parser.parse (s, pos);
    return &tp_normal_rep;
  }
  if (is_digit (c) ||
      (c == '.' && pos+1 < N(s) && is_digit (s[pos+1]))) {
    number_parser.parse (s, pos);
    return &tp_normal_rep;
  }
  if (belongs_to_identifier (c)) {
    parse_alpha (s, pos);
    return &tp_normal_rep;
  }
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

static void
parse_escaped_char (string s, int& pos) {
  int n= N(s), i= pos++;
  if (i+2 >= n) return;
  if (s[i] != '\\')
    return;
  i++;
  if (s[i] == '\\' || s[i] == '\'' || s[i] == '\"' ||
           s[i] == 'b'  || s[i] == 'f'  ||
           s[i] == 'n'  || s[i] == 'r'  || s[i] == 't')
    pos+= 1;
  else if (s[i] == 'u')
    pos+= 5;
  return;
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

static void
parse_comment_single_line (string s, int& pos) {
  if (pos+1>=N(s)) return;
  if (s[pos]!='/' || s[pos+1]!='/') return;
  pos=N(s);
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
        parse_escaped_char (s, pos);
        in_esc= false;
        in_str= true;
        if (opos < pos) {
          type= "constant_char";
          break;
        }
      }
      else {
        parse_blanks (s, pos);
        if (opos < pos){
          break;
        }
        parse_comment_single_line (s, pos);
        if (opos < pos) {
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
        number_parser.parse (s, pos);
        if (opos < pos) {
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
  return decode_color ("java", encode_color (type));
}
