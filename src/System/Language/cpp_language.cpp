
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

extern tree the_et;

/*
static bool
is_line (tree t) {
 path p= obtain_ip (t);
  if (is_nil (p) || last_item (p) < 0) return false;
  tree pt= subtree (the_et, reverse (p->next));
  if (!is_func (pt, DOCUMENT)) return false;
  return true;
}
*/

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
  t ("asm")= c;
  t ("auto")= c;
  t ("break")= c;
  t ("case")= c;
  t ("catch")= c;
  t ("class")= c;
  t ("concrete")= c;
  t ("constant")= c;
  t ("continue")= c;
  t ("default")= c;
  t ("delete")= c;
  t ("do")= c;
  t ("else")= c;
  t ("enum")= c;
  t ("extern")= c;
  t ("explicit")= c;
  t ("for")= c;
  t ("friend")= c;
  t ("goto")= c;
  t ("if")= c;
  t ("inline")= c;
  t ("mutable")= c;
  t ("new")= c;
  t ("operator")= c;
  t ("private")= c;
  t ("protected")= c;
  t ("public")= c;
  t ("register")= c;
  t ("return")= c ;
  t ("sizeof")= c;
  t ("static")= c;
  t ("struct")= c;
  t ("switch")= c;
  t ("template")= c;
  t ("this")= c;
  t ("throw")= c;
  t ("to")= c;
  t ("try")= c;
  t ("typedef")= c;
  t ("union")= c;
  t ("virtual")= c;
  t ("volatile")= c;
  t ("while")= c;
  t ("malloc")= c;
  t ("realloc")= c;
  t ("calloc")= c;
  t ("free")= c;
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
parse_comment_multi_lines (string s, int& pos) {
  if (pos>=N(s)) return;
  if (s[pos]!='/') return;
  if (pos+1<N(s) && s[pos+1]=='*') {
    pos= pos+2;
    while ((pos<N(s) && s[pos]!='*') || (pos+1<N(s) && s[pos+1]!='/')) pos++;
    pos= min(pos+2,N(s));
  }
}

static void
parse_comment_single_line (string s, int& pos) {
  if (pos>=N(s)) return;
  if (s[pos]!='/') return;
  if (pos+1<N(s) && s[pos+1]=='/') {pos=N(s);return;}
}

static void
parse_end_comment (string s, int& pos) {
  if (pos+1<N(s) && s[pos]=='*' && s[pos+1]=='/') pos=pos+2; 
}

static void parse_diese (string s, int& pos) {
  if (s[pos] == '#') pos++;
  }

static void parse_preprocessing (string s, int & pos) {
  int i= pos;
  if (pos>=N(s)) return;
  if (is_number (s[i])) return;
  while ((i<N(s)) && belongs_to_identifier (s[i])) i++;
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
  
  
  
static bool begin_comment (string s, int i) {
  bool comment;
  int pos= 0;
  int opos; 
  do {
    do {
    opos= pos;
    comment= false;
    parse_string (s, pos);
    if (opos < pos) break;
    parse_comment_multi_lines (s, pos);
    if (opos < pos) {comment= true; break;}
    pos++;
    }
  while (false);
  }
  while (pos<=i);
  return comment;  
}

static bool end_comment (string s, int i) {
  bool comment;
  int pos= 0; int opos;
  do {
    do {
    opos= pos;
    comment= false;
    parse_string (s, pos);
	if (opos < pos) break;
    parse_end_comment (s, pos);
    if (opos < pos && pos>i) return true;
    pos ++;
    }
  while (false);
  }
  while (pos<N(s));
  return false;
}

static bool after_begin_comment (string s, int i, tree t) {
  if (begin_comment(s, i)) return true;
  tree t2= t;
  string s2= s;
  if (N(s2)==0) return false;
  int pos=0;
  parse_blanks(s2,pos);
  if (s2[pos]!='*') return false;
  while (line_number(t2) > 0) {
    t2= line_inc(t2,-1);
    s2= t2->label;
    if (N(s2)>0 && begin_comment (s2, N(s2)-1)) return true;
    if (N(s2)==0) return false;
    int pos=0;
    parse_blanks(s2,pos);
    if (s2[pos]!='*') return false;
    } 
  return false;
}

static bool before_end_comment (string s, int i, tree t) {
  int number= number_of_line(t);
  tree t2= t;
  string s2=s;
  if (N(s2)==0) return false;
  int pos=0;
  if (!begin_comment(s,i)) {
    parse_blanks(s2,pos);
    if (s2[pos]!='*') return false;
  }
  if (end_comment(s, i)) return true;
  while (line_number(t2) < number-1) {
    t2= line_inc(t2,1);
    s2= t2->label;
    if (N(s2)==0) return false;
    pos=0;
    parse_blanks(s2, pos);
    if (s2[pos]!='*') return false;
    if (N(s2)>0 && end_comment (s2, 0)) return true;
  } 
  return false;
}

static bool in_comment(string s, int pos, tree t) {
  if (after_begin_comment(s, pos, t) && before_end_comment(s, pos, t)) return true;
  return false;
}  

static bool end_preprocessing( string s) {
  int pos=N(s)-1;
  if (N(s)==0) return false;
  while (s[pos]==' ' && pos>0) {pos--;}
  if (s[pos]=='/') return true;
  return false;
}  
  
static bool begin_preprocessing( string s) { 
  if (N(s)>0 && s[0]=='#') return true;
  return false;
}

static bool in_preprocessing (string s, tree t) {
  if (begin_preprocessing(s)) return true;
  tree t2= t;
  string s2= s;
  while (line_number(t2) > 0) {
    t2= line_inc(t2,-1);
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
    cpp_color_setup_otherlexeme (colored);
    setup_done= true;
  }
  static string none= "";
  if (start >= end) return none;
  string s= t->label;
  if (in_comment(s,start,t)) return "brown";
  int pos= 0;
  int opos=0;
  string type;
  if (in_preprocessing(s, t)){
  do {
    do {
    opos= pos;
    type=none;
    parse_blanks (s, pos);
    if (opos < pos) break;
    parse_diese(s, pos);
    if (opos < pos) {type="preprocessing"; break;}
    parse_preprocessing (s, pos);
    if (opos < pos) {type= "preprocessing"; break; }
    pos++;
  	}
  while (false);}
  while (pos <= start);
  if (type == "preprocessing") return "#20a000";
  return "#004000";
  }
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
      parse_comment_single_line (s, pos);
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
