
/******************************************************************************
* MODULE     : cpp_language.cpp
* DESCRIPTION: the "cpp" language
* COPYRIGHT  : (C) 2008  Francis Jamet
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "analyze.hpp"
#include "impl_language.hpp"
#include "Scheme/object.hpp"

static void parse_number (string s, int& pos);
static void parse_string (string s, int& pos);
static void parse_alpha (string s, int& pos);

cpp_language_rep::cpp_language_rep (string name):
  language_rep (name), colored ("")
{ 
  eval ("(use-modules (utils misc tm-keywords))");
  list<string> l= as_list_string (eval ("(map symbol->string highlight-any)"));
  while (!is_nil (l)) {
    colored (l->item)= "blue";
    l= l->next;
  }
}

text_property
cpp_language_rep::advance (string s, int& pos) {
  if (pos==N(s)) return &tp_normal_rep;
  char c= s[pos];
  if (c == ' ') {
    pos++; return &tp_space_rep; }
  if (c >= '0' && c <= '9') {
    parse_number (s, pos); return &tp_normal_rep; }
  if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
      (c == '_')) {
    parse_alpha (s, pos); return &tp_normal_rep; }
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
cpp_language_rep::hyphenate (
  string s, int after, string& left, string& right)
{ 
  left = s (0, after);
  right= s (after, N(s));
}

static void
cpp_color_setup_constants (hashmap<string, string> & t) {
  string c= "#2060c0";
  t ("true")= c;
  t ("false")= c;
  t ("cout")= c;
  t ("cin")= c;
  t ("cerr")= c;
}

static void
cpp_color_setup_keywords (hashmap<string, string> & t)  {
  string c= "#8020c0";
  t ("break")= c;
  t ("catch")= c;
  t ("class")= c;
  t ("concrete")= c;
  t ("constant")= c;
  t ("continue")= c;
  t ("do")= c;
  t ("else")= c;
  t ("extern")= c;
  t ("for")= c;
  t ("if")= c;
  t ("inline")= c;
  t ("operator")= c;
  t ("private")= c;
  t ("protected")= c;
  t ("public")= c;
  t ("return")= c ;
  t ("struct")= c;
  t ("this")= c;
  t ("throw")= c;
  t ("to")= c;
  t ("try")= c;
  t ("virtual")= c;
  t ("while")= c;
}

static void
cpp_color_setup_otherlexeme (hashmap<string, string>& t) {
  string c= "black";
  t ("+=")= c;
  t ("-=")= c; 
  t ("*=")= c;
  t ("/=")= c;
  t (",")= c;
  t (";")= c;
  t (")")= c;
  t ("[")= c;
  t ("]")= c;
  t ("{")= c;
  t ("}")= c;
  t ("<less><less>")= c;
  t ("<gtr><gtr>")= c;
}

static inline bool
belongs_to_identifier (char c) {
  return ((c<='9' && c>='0') ||
         (c<='Z' && c>='A') ||
	 (c<='z' && c>='a') ||
          c=='_');
}

static inline bool
is_number (char c) {
  return (c>='0' && c<='9');
}

static void
parse_identifier (hashmap<string, string>& t, string s, int& pos) {
  int i=pos;
  if (pos >= N(s)) return;
  if (is_number (s[i])) return;
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
  while (pos<N(s) && (s[pos]==' ' || s[pos]=='\t')) pos++;
}

static void
parse_string (string s, int& pos) {
  if (pos>=N(s)) return;
  switch (s[pos])  {
  case '\042':
    do pos++;
    while((pos<N(s)) &&
	  (s[pos-1]=='\\' && s[pos]=='\042' || s[pos]!='\042'));
    if (s[pos]=='\042') pos++;
    return;
  case '/':
    if (pos+1<N(s) && s[pos+1]=='\042') {
      pos=pos+2;
      do {
	if (pos+1<N(s) && s[pos]=='\042' && s[pos+1]=='/') {
	  pos=pos+2; return; }
	pos++;
      } while (pos<N(s));
    }
  }
}
  
static void
parse_keyword (hashmap<string,string>& t, string s, int& pos) {
  int i= pos;
  if (pos>=N(s)) return;
  if (is_number (s[i])) return;
  while ((i<N(s)) && belongs_to_identifier (s[i])) i++;
  string r= s (pos, i);
  if (t->contains (r) && t(r)=="#8020c0") { pos=i; return; }
}

static void
parse_constant (hashmap<string,string>& t, string s, int& pos) {
  int i=pos;
  if (pos>=N(s)) return;
  if (is_number (s[i])) return;
  while ((i<N(s)) && belongs_to_identifier (s[i])) i++;
  string r= s (pos, i);
  if (t->contains (r) && t(r)=="#2060c0") { pos=i; return; }
}

static void
parse_other_lexeme (hashmap<string,string>& t, string s, int& pos) {
  int i;
  for (i=12; i>=1; i--) {
    string r=s(pos,pos+i);
    if (t->contains(r) && t(r)=="black") {
      pos=pos+i; return; }
  }
}

static void
parse_number (string s, int& pos) {
  int i= pos;
  if (pos>=N(s)) return;
  if (s[i] == '.') return;
  while (i<N(s) && 
	 (is_number (s[i]) ||
	  (s[i] == '.' && (i+1<N(s)) &&
	   (is_number (s[i+1]) ||
	    s[i+1] == 'e' || s[i+1] == 'E')))) i++;
  if (i == pos) return;
  if (i<N(s) && (s[i] == 'e' || s[i] == 'E')) {
    i++;
    if (i<N(s) && s[i] == '-') i++;
    while (i<N(s) && (is_number (s[i]))) i++;
  }
  pos= i;
}

static void
parse_comment (string s, int& pos) {
  if (pos>=N(s)) return;
  if (s[pos]!='/') return;
  if (pos+1<N(s) && s[pos+1]=='/') {pos=N(s);return;}
  if (pos+1<N(s) && s[pos+1]=='*') {
    pos= pos+2;
    while ((pos<N(s) && s[pos]!='*') || (pos+1<N(s) && s[pos+1]!='/')) pos++;
    pos= min(pos+2,N(s));
  }
}

static void
parse_end_comment (string s, int& pos) {
  if (pos+1<N(s) && s[pos]=='*' && s[pos+1]=='/') pos=pos+2; 
}

string
cpp_language_rep::get_color (tree t, int start, int end) {
  static bool setup_done= false;
  if (!setup_done) {
    cpp_color_setup_constants (colored);
    cpp_color_setup_keywords (colored);
    cpp_color_setup_otherlexeme (colored);
    setup_done= true;
  }

  static string none= "";
  if (start >= end) return none;
  string s= t->label;
  int pos= 0;
  int opos;
  string type;
  do {
    do {
      opos=pos;
      parse_string (s, pos);
      if (opos < pos) break;
      parse_comment (s, pos);
      if (opos < pos) break;
      parse_end_comment (s, pos);
      if (opos < pos) { 
	if (pos > start) return "brown";
	else break;
      }
      pos++;
    }
    while(false);
  }
  while (pos < N(s));
  pos= 0;
  do {
    type= none;
    do {
      opos= pos;
      parse_blanks (s, pos);
      if (opos < pos) break;
      parse_string (s, pos);
      if (opos < pos) {
	type= "string";
	break;
      }
      parse_comment (s, pos);
      if (opos < pos) {
	type= "comment";
	break;
      }
      parse_keyword (colored, s, pos);
      if (opos < pos) {
	type= "keyword";
	break;
      }
      parse_other_lexeme (colored, s, pos);  //not left parenthesis
      if (opos < pos) {
	type= "other_lexeme";
	break;
      }
      parse_constant (colored, s, pos);
      if (opos < pos) {
	type= "constant";
	break;
      }
      parse_number (s, pos);
      if (opos < pos) {
	type= "number";
	break;
      }
      parse_identifier (colored, s, pos);
      if (opos < pos) {
	type="identifier";
	break;
      }
      pos= opos;
      pos++;
    }
    while (false);
  }
  while (pos <= start);
  if (type=="string") return "#a06040";
  if (type=="comment") return "brown";
  if (type=="keyword") return "#8020c0";
  if (type=="constant") return "#2060c0";
  if (type=="number") return "#2060c0";
  return none;
}
