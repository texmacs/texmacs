
/******************************************************************************
* MODULE     : python_language.cpp
* DESCRIPTION: the python language
* COPYRIGHT  : (C) 2014  François Poulain
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
******************************************************************************/

#include "analyze.hpp"
#include "impl_language.hpp"
#include "scheme.hpp"

static void parse_escaped_char (string s, int& pos);
static void parse_number (string s, int& pos);
static void parse_various_number (string s, int& pos);
static void parse_alpha (string s, int& pos);
static inline bool belongs_to_identifier (char c);

python_language_rep::python_language_rep (string name):
  language_rep (name), colored ("") {}

text_property
python_language_rep::advance (tree t, int& pos) {
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
       (s[pos+1] == 'x' || s[pos+1] == 'X' ||
        s[pos+1] == 'o' || s[pos+1] == 'O' ||
        s[pos+1] == 'b' || s[pos+1] == 'B')) {
    parse_various_number (s, pos);
    return &tp_normal_rep;
  }
  if ((c >= '0' && c <= '9') ||
      (c == '.' && pos+1 < N(s) && s[pos+1] >= '0' && s[pos+1] <= '9')) {
    parse_number (s, pos);
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

static void
python_color_setup_operator_openclose (hashmap<string, string> & t) {
  string c= "operator_openclose";
  t ("{")= c;
  t ("[")= c;
  t ("(")= c;
  t (")")= c;
  t ("]")= c;
  t ("}")= c;
}

static void
python_color_setup_constants (hashmap<string, string> & t) {
  string c= "constant";
  t ("Ellipsis")= c;
  t ("False")= c;
  t ("None")= c;
  t ("NotImplemented")= c;
  t ("True")= c;
  t ("__debug__")= c;
  t ("__import__")= c;
  t ("abs")= c;
  t ("all")= c;
  t ("any")= c;
  t ("apply")= c;
  t ("ascii")= c;
  t ("basestring")= c;
  t ("bin")= c;
  t ("bool")= c;
  t ("buffer")= c;
  t ("bytearray")= c;
  t ("bytes")= c;
  t ("callable")= c;
  t ("chr")= c;
  t ("classmethod")= c;
  t ("cmp")= c;
  t ("coerce")= c;
  t ("compile")= c;
  t ("complex")= c;
  t ("delattr")= c;
  t ("dict")= c;
  t ("dir")= c;
  t ("divmod")= c;
  t ("enumerate")= c;
  t ("eval")= c;
  t ("execfile")= c;
  t ("file")= c;
  t ("filter")= c;
  t ("float")= c;
  t ("format")= c;
  t ("frozenset")= c;
  t ("getattr")= c;
  t ("globals")= c;
  t ("hasattr")= c;
  t ("hash")= c;
  t ("help")= c;
  t ("hex")= c;
  t ("id")= c;
  t ("input")= c;
  t ("int")= c;
  t ("intern")= c;
  t ("isinstance")= c;
  t ("issubclass")= c;
  t ("iter")= c;
  t ("len")= c;
  t ("list")= c;
  t ("locals")= c;
  t ("long")= c;
  t ("map")= c;
  t ("max")= c;
  t ("memoryview")= c;
  t ("min")= c;
  t ("next")= c;
  t ("nonlocal")= c;
  t ("object")= c;
  t ("oct")= c;
  t ("open")= c;
  t ("ord")= c;
  t ("pow")= c;
  t ("property")= c;
  t ("range")= c;
  t ("raw_input")= c;
  t ("reduce")= c;
  t ("reload")= c;
  t ("repr")= c;
  t ("reversed")= c;
  t ("round")= c;
  t ("set")= c;
  t ("setattr")= c;
  t ("slice")= c;
  t ("sorted")= c;
  t ("staticmethod")= c;
  t ("str")= c;
  t ("sum")= c;
  t ("super")= c;
  t ("tuple")= c;
  t ("type")= c;
  t ("unichr")= c;
  t ("unicode")= c;
  t ("vars")= c;
  t ("xrange")= c;
  t ("zip")= c;
}

static void
python_color_setup_constant_exceptions (hashmap<string, string> & t) {
  string c= "constant";
  t ("BaseException")= c;
  t ("Exception")= c;
  t ("ArithmeticError")= c;
  t ("EnvironmentError")= c;
  t ("LookupError")= c;
  t ("StandardError")= c;
  t ("AssertionError")= c;
  t ("AttributeError")= c;
  t ("BufferError")= c;
  t ("EOFError")= c;
  t ("FloatingPointError")= c;
  t ("GeneratorExit")= c;
  t ("IOError")= c;
  t ("ImportError")= c;
  t ("IndentationError")= c;
  t ("IndexError")= c;
  t ("KeyError")= c;
  t ("KeyboardInterrupt")= c;
  t ("MemoryError")= c;
  t ("NameError")= c;
  t ("NotImplementedError")= c;
  t ("OSError")= c;
  t ("OverflowError")= c;
  t ("ReferenceError")= c;
  t ("RuntimeError")= c;
  t ("StopIteration")= c;
  t ("SyntaxError")= c;
  t ("SystemError")= c;
  t ("SystemExit")= c;
  t ("TabError")= c;
  t ("TypeError")= c;
  t ("UnboundLocalError")= c;
  t ("UnicodeError")= c;
  t ("UnicodeDecodeError")= c;
  t ("UnicodeEncodeError")= c;
  t ("UnicodeTranslateError")= c;
  t ("ValueError")= c;
  t ("VMSError")= c;
  t ("WindowsError")= c;
  t ("ZeroDivisionError")= c;
  t ("BytesWarning")= c;
  t ("DeprecationWarning")= c;
  t ("FutureWarning")= c;
  t ("ImportWarning")= c;
  t ("PendingDeprecationWarning")= c;
  t ("RuntimeWarning")= c;
  t ("SyntaxWarning")= c;
  t ("UnicodeWarning")= c;
  t ("UserWarning")= c;
  t ("Warning")= c;
}

static void
python_color_setup_declare_class (hashmap<string, string> & t) {
  string c= "declare_type";
  t ("class")= c;
}

static void
python_color_setup_declare_function (hashmap<string, string> & t) {
  string c= "declare_function";
  t ("def")= c;
  t ("lambda")= c;
}

static void
python_color_setup_keywords (hashmap<string, string> & t) {
  string c= "keyword";
  t ("as")= c;
  t ("del")= c;
  t ("finally")= c;
  t ("from")= c;
  t ("global")= c;
  t ("import")= c;
  t ("in")= c;
  t ("is")= c;
  t ("with")= c;
}

static void
python_color_setup_keywords_conditional (hashmap<string, string> & t) {
  string c= "keyword_conditional";
  t ("break")= c;
  t ("continue")= c;
  t ("elif")= c;
  t ("else")= c;
  t ("for")= c;
  t ("if")= c;
  t ("while")= c;
}

static void
python_color_setup_keywords_control (hashmap<string, string> & t) {
  string c= "keyword_control";
  t ("assert")= c;
  t ("except")= c;
  t ("exec")= c;
  t ("pass")= c;
  t ("print")= c;
  t ("raise")= c;
  t ("return")= c;
  t ("try")= c;
  t ("yield")= c;
}

static void
python_color_setup_operator (hashmap<string, string>& t) {
  string c= "operator";
  t ("and")= c;
  t ("not")= c;
  t ("or")= c;

  t ("+")= c;
  t ("-")= c;
  t ("/")= c;
  t ("*")= c;
  t ("**")= c;
  t ("//")= c;
  t ("%")= c;
  t ("|")= c;
  t ("&")= c;
  t ("^")= c;
  t ("<less><less>")= c;
  t ("<gtr><gtr>")= c;

  t ("==")= c;
  t ("!=")= c;
  t ("<less><gtr>")= c;
  t ("<less>")= c;
  t ("<gtr>")= c;
  t ("<less>=")= c;
  t ("<gtr>=")= c;

  t ("=")= c;

  t ("+=")= c;
  t ("-=")= c;
  t ("/=")= c;
  t ("*=")= c;
  t ("**=")= c;
  t ("//=")= c;
  t ("%=")= c;
  t ("|=")= c;
  t ("&=")= c;
  t ("^=")= c;
  t ("<less><less>=")= c;
  t ("<gtr><gtr>=")= c;

  t ("~")= c;
}

static void
python_color_setup_operator_special (hashmap<string, string> & t) {
  string c= "operator_special";
  t (":")= c;
}

static void
python_color_setup_operator_decoration (hashmap<string, string> & t) {
  string c= "operator_decoration";
  t ("@")= c;
}

static void
python_color_setup_operator_field (hashmap<string, string> & t) {
  t (".")= "operator_field";
}

static inline bool
belongs_to_identifier (char c) {
  return ((c<='9' && c>='0') ||
          (c<='Z' && c>='A') ||
	  (c<='z' && c>='a') ||
          (c=='_'));
}

static void
parse_identifier (hashmap<string, string>& t, string s, int& pos) {
  int i=pos;
  if (pos >= N(s)) return;
  if (is_digit (s[i])) return;
  while (i<N(s) && belongs_to_identifier (s[i])) i++;
  if (!(t->contains (s (pos, i)))) pos= i;
}

static void
parse_alpha (string s, int& pos) {
  static hashmap<string,string> empty;
  parse_identifier (empty, s, pos);
}

static void
parse_blanks (string s, int& pos) {
  while (pos<N(s) && (s[pos] == ' ' || s[pos] == '\t')) pos++;
}

static void
parse_escaped_char (string s, int& pos) {
  int n= N(s), i= pos++;
  if (i+2 >= n) return;
  if (s[i] != '\\')
    return;
  i++;
  if (test (s, i, "newline"))
    pos+= 7;
  else if (s[i] == '\\' || s[i] == '\'' || s[i] == '\"' ||
           s[i] == 'a'  || s[i] == 'b'  || s[i] == 'f'  ||
           s[i] == 'n'  || s[i] == 'r'  || s[i] == 't'  ||
           s[i] == 'N'  || s[i] == 'v')
    pos+= 1;
  else if (s[i] == 'o'  || s[i] == 'x')
    pos+= 3;
  else if (s[i] == 'u')
    pos+= 5;
  else if (s[i] == 'U')
    pos+= 9;
  return;
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
 
static string
parse_keywords (hashmap<string,string>& t, string s, int& pos) {
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

static string
parse_operators (hashmap<string,string>& t, string s, int& pos) {
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
parse_various_number (string s, int& pos) {
  if (!(pos+2 < N(s) && s[pos] == '0' &&
       (s[pos+1] == 'x' || s[pos+1] == 'X' ||
        s[pos+1] == 'o' || s[pos+1] == 'O' ||
        s[pos+1] == 'b' || s[pos+1] == 'B')))
    return;
  pos+= 2;
  while (pos<N(s) && is_hex_digit (s[pos])) pos++;
  if (pos<N(s) && (s[pos] == 'l' || s[pos] == 'L')) pos++;
}

static void
parse_number (string s, int& pos) {
  int i= pos;
  if (pos>=N(s)) return;
  if (!is_digit (s[i]) &&
      !(s[i] == '.' && pos+1 < N(s) && is_digit (s[pos+1])))
    return;
  i++;
  while (i<N(s) && (is_digit (s[i]) || s[i] == '.'))
    i++;
  if (i == pos) return;
  if (i<N(s) && (s[i] == 'e' || s[i] == 'E')) {
    i++;
    if (i<N(s) && s[i] == '-') i++;
    while (i<N(s) && (is_digit (s[i]) || s[i] == '.')) i++;
    if (i<N(s) && (s[i] == 'j')) i++;
  }
  else if (i<N(s) && (s[i] == 'l' || s[i] == 'L')) i++;
  else if (i<N(s) && (s[i] == 'j')) i++;
  pos= i;
}

static void
parse_comment_single_line (string s, int& pos) {
  if (pos>=N(s)) return;
  if (s[pos]!='#') return;
  pos=N(s);
}

string
python_language_rep::get_color (tree t, int start, int end) {
  static bool setup_done= false;
  if (!setup_done) {
    /*
     * NOTE: it seems there is no way to take into account multiline
     * dependencies. Then such weird syntax like
     *
     * str= """some string beginning ...
     * some string end"""
     *
     * will not be correctly typeset.
     *
     */

    python_color_setup_constants (colored);
    python_color_setup_constant_exceptions (colored);
    python_color_setup_declare_class (colored);
    python_color_setup_declare_function (colored);
    python_color_setup_keywords (colored);
    python_color_setup_keywords_conditional (colored);
    python_color_setup_keywords_control (colored);
    python_color_setup_operator (colored);
    python_color_setup_operator_special (colored);
    python_color_setup_operator_decoration (colored);
    python_color_setup_operator_openclose (colored);
    python_color_setup_operator_field (colored);
    setup_done= true;
  }

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
        parse_various_number (s, pos);
        if (opos < pos) {
          type= "constant_number";
          break;
        }
        parse_number (s, pos);
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
  return decode_color ("python", encode_color (type));
}
