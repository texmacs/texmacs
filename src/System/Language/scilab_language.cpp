
/******************************************************************************
* MODULE     : scilab_language.cpp
* DESCRIPTION: the "scilab" language
* COPYRIGHT  : (C) 2008  Francis Jamet
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
******************************************************************************/

#include "analyze.hpp"
#include "impl_language.hpp"
#include "scheme.hpp"

/*
extern tree the_et;

static bool
is_line (tree t) {
 path p= obtain_ip (t);
  if (is_nil (p) || last_item (p) < 0) return false;
  tree pt= subtree (the_et, reverse (p->next));
  if (!is_func (pt, DOCUMENT)) return false;
  return true;
}

static int
line_number (tree t) {
  path p= obtain_ip (t);
  if (is_nil (p) || last_item (p) < 0) return -1;
  tree pt= subtree (the_et, reverse (p->next));
  if (!is_func (pt, DOCUMENT)) return -1;
  return p->item;
}

static int
number_of_line (tree t) {
  path p= obtain_ip (t);
  if (is_nil (p) || last_item (p) < 0) return -1;
  tree pt= subtree (the_et, reverse (p->next));
  if (!is_func (pt, DOCUMENT)) return -1;
  return N(pt);
}

static tree
line_inc (tree t, int i) {
  path p= obtain_ip (t);
  if (is_nil (p) || last_item (p) < 0) return tree (ERROR);
  tree pt= subtree (the_et, reverse (p->next));
  if (!is_func (pt, DOCUMENT)) return tree (ERROR);
  if ((p->item + i < 0) || (p->item + i >= N(pt))) return tree (ERROR);
  return pt[p->item + i];
}
*/

static void parse_number (string s, int& pos);
static void parse_alpha (string s, int& pos);
static inline bool belongs_to_identifier (char c);

scilab_language_rep::scilab_language_rep (string name):
  language_rep (name), colored ("")
{}

text_property
scilab_language_rep::advance (tree t, int& pos) {
  string s= t->label;
  if (pos==N(s)) return &tp_normal_rep;
  char c= s[pos];
  if (c == ' ') {
    pos++; return &tp_space_rep; }
  if (is_digit (c)) {
    parse_number (s, pos); return &tp_normal_rep; }
  if (belongs_to_identifier (c)) {
    parse_alpha (s, pos); return &tp_normal_rep; }
  tm_char_forwards (s, pos);
  return &tp_normal_rep;
}

array<int>
scilab_language_rep::get_hyphens (string s) {
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
scilab_language_rep::hyphenate (
  string s, int after, string& left, string& right)
{ 
  left = s (0, after);
  right= s (after, N(s));
}

static void
scilab_color_setup_operator_openclose (hashmap<string, string> & t) {
  string c= "operator_openclose";
  t ("{")= c;
  t ("[")= c;
  t ("(")= c;
  t (")")= c;
  t ("]")= c;
  t ("}")= c;
}

static void
scilab_color_setup_constants (hashmap<string, string> & t) {
  string c= "constant";
  t ("ans")= c;
  t ("SCI")= c;
  t ("WSCI")= c;
  t ("SCIHOME")= c;
  t ("TMPDIR")= c;
  t ("%e")= c;
  t ("%eps")= c;
  t ("%f")= c;
  t ("%F")= c;
  t ("%i")= c;
  t ("%inf")= c;
  t ("%io")= c;
  t ("%j")= c;
  t ("%nan")= c;
  t ("%pi")= c;
  t ("%s")= c;
  t ("%t")= c;
  t ("%T")= c;
  t ("%z")= c;
}

static void
scilab_color_setup_declare_function (hashmap<string, string> & t) {
  string c= "declare_function";
  t ("function")= c;
  t ("endfunction")= c;
}

static void
scilab_color_setup_keywords_conditional (hashmap<string, string> & t) {
  string c= "keyword_conditional"; 
  t ("case")= c;
  t ("catch")= c;
  t ("do")= c;
  t ("else")= c;
  t ("elseif")= c;
  t ("end")= c;
  t ("for")= c;
  t ("if")= c;
  t ("select")= c;
  t ("then")= c;
  t ("try")= c;
  t ("while")= c;
}

static void
scilab_color_setup_keywords_control (hashmap<string, string> & t) {
  string c= "keyword_control"; 
  t ("abort")= c;
  t ("break")= c;
  t ("continue")= c;
  t ("exit")= c;
  t ("pause")= c;
  t ("quit")= c;
  t ("resume")= c;
  t ("return")= c;
}

static void
scilab_color_setup_operator (hashmap<string, string>& t) {
  string c= "operator";
  t ("\'")= c;
  t ("@")= c;
  t ("~")= c;
  t ("&")= c;
  t ("|")= c;
  t (".'")= c;
  t (".*")= c;
  t ("./")= c;
  t (".\\\\")= c;
  t (".^")= c;
  t (".**")= c;
  t ("+")= c;
  t ("-")= c;
  t ("/")= c;
  t ("\\\\")= c;
  t ("*")= c;
  t ("^")= c;
  t ("**")= c;
  t ("==")= c;
  t ("~=")= c;
  t ("@=")= c;
  t ("<less><gtr>")= c;
  t ("<less>")= c;
  t ("<gtr>")= c;
  t ("<less>=")= c;
  t ("<gtr>=")= c;
  t (".*.")= c;
  t ("./.")= c;
  t (".\\\\.")= c;
  t ("=")= c;
}

static void
scilab_color_setup_operator_special (hashmap<string, string> & t) {
  string c= "operator_special";
  t ("$")= c;
  t (":")= c;
  t ("..")= c;
}

static void
scilab_color_setup_operator_field (hashmap<string, string> & t) {
  t (".")= "operator_field";
}

static inline bool
belongs_to_identifier (char c) {
  return is_digit (c) || is_alpha(c) ||
         (c=='_' || c=='%' || c=='#' || c=='$' || c=='?' || c=='!');
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

static bool
string_end (string s, int pos) {
  while (pos<N(s) && (s[pos] == ' ' || s[pos] == '\t' || s[pos] == '.')) pos++;
  if (pos == N(s) || (pos + 1 < N(s) && s[pos] == '/' && s[pos+1] == '/'))
    return true;
  return false;
}

static bool
parse_string (string s, int& pos) {
  int start= pos;
  if (pos >= N(s)) return false;
  if (s[pos] == '\"' || s[pos] == '\'') {
    pos++;
    while (pos < N(s)) {
      if (s[pos] == '\"' || s[pos] == '\'') {
        pos++;
        if (pos < N(s) && !(s[pos] == '\"' || s[pos] == '\'')) {
          return false;
        }
      }
      else if (pos + 1 < N(s) && s[pos] == '.' && s[pos+1] == '.') {
        if (string_end (s, pos)) {
          pos--;
          return true;
        }
      }
      pos++;
    }
    if (pos >= N(s) && s[start] == '\'') {
      // A non ended quote means a transpose operator 
      pos= start;
      return false;
    }
  }
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
    if (t(r) == "keyword_conditional") {
      pos=i;
      return "keyword_conditional";
    }
    else if (t(r) == "keyword_control") {
      pos=i;
      return "keyword_control";
    }
    else if (t(r) == "declare_function") {
      pos=i;
      return "declare_function";
    }
    else if (t(r) == "constant") {
      pos=i;
      return "constant";
    }
  }
  return "";
}

static string
parse_operators (hashmap<string,string>& t, string s, int& pos) {
  int i;
  for (i=11; i>=1; i--) {
    string r=s(pos,pos+i);
    if (t->contains (r)) {
      if (t(r) == "operator") {
        pos=pos+i;
        return "operator";
      }
      else if (t(r) == "operator_special") {
        if (s[pos] == '.')
          do pos++; while (s[pos] == '.');
        else
          pos=pos+i;
        return "operator_special";
      }
      else if (t(r) == "operator_openclose") {
        pos=pos+i;
        return "operator_openclose";
      }
      else if (t(r) == "operator_field") {
        pos=pos+i;
        while ((pos<N(s)) && belongs_to_identifier (s[pos])) pos++;
        return "operator_field";
      }
    }
  }
  return "";
}

static void
parse_number (string s, int& pos) {
  int i= pos;
  if (pos>=N(s)) return;
  if (s[i] == '.') return;
  while (i<N(s) && 
	 (is_digit (s[i]) ||
	  (s[i] == '.' && (i+1<N(s)) &&
	   (is_digit (s[i+1]) ||
	    s[i+1] == 'e' || s[i+1] == 'E' ||
            s[i+1] == 'd' || s[i+1] == 'D')))) i++;
  if (i == pos) return;
  if (i<N(s) && (s[i] == 'e' || s[i] == 'E' || s[i] == 'd' || s[i] == 'D' )) {
    i++;
    if (i<N(s) && s[i] == '-') i++;
    while (i<N(s) && (is_digit (s[i]))) i++;
  }
  pos= i;
}

static void
parse_comment_single_line (string s, int& pos) {
  if (pos>=N(s)) return;
  if (s[pos]!='/') return;
  if (pos+1<N(s) && s[pos+1]=='/') {pos=N(s);return;}
}

string
scilab_language_rep::get_color (tree t, int start, int end) {
  static bool setup_done= false;
  if (!setup_done) {
    /* 
     * NOTE: it seems there is no way to take into account multiline
     * dependencies. Then such weird syntax like
     *
     * str= "some string beginning ... // some comment
     * some string end"
     *
     * will not be correctly typeset. For the same reasons, i/o function args
     * are not highlighted.
     *
     */

    scilab_color_setup_constants (colored);
    scilab_color_setup_declare_function (colored);
    scilab_color_setup_keywords_conditional (colored);
    scilab_color_setup_keywords_control (colored);
    scilab_color_setup_operator (colored);
    scilab_color_setup_operator_special (colored);
    scilab_color_setup_operator_openclose (colored);
    scilab_color_setup_operator_field (colored);
    setup_done= true;
  }

  static string none= "";
  if (start >= end) return none;
  string s= t->label;
  int pos= 0;
  int opos=0;
  string type;
  bool cut_str= false;
  do {
    type= none;
    do {
      opos= pos;
      if (cut_str) {
        while (s[pos] == '.') pos++;
        if (opos < pos) {
          type= "operator_special";
          break;
        }
      }
      parse_blanks (s, pos);
      if (opos < pos){
        break;
      }
      parse_comment_single_line (s, pos);
      if (opos < pos) {
        type= "comment";
        break;
      }
      cut_str= parse_string (s, pos);
      if (opos < pos) {
        type= "constant_string";
        break;
      }

      if (!cut_str) {
        type= parse_keywords (colored, s, pos);
        if (opos < pos) {
          break;
        }
        type= parse_operators (colored, s, pos);
        if (opos < pos) {
          break;
        }
        parse_number (s, pos);
        if (opos < pos) {
          type= "constant_number";
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
  return decode_color ("scilab", encode_color (type));
}
