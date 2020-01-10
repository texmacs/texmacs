
/******************************************************************************
* MODULE     : r_language.cpp
* DESCRIPTION: the "r" language
* COPYRIGHT  : (C) 2008-2019  Francis Jamet, Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "analyze.hpp"
#include "hyphenate.hpp"
#include "impl_language.hpp"
#include "scheme.hpp"

#define COLOR_MARKUP "#500d04"

static void parse_string (string s, int& pos);
static bool is_in_str( char c, const char *str )  ;
static bool is_number_start( char c ) ;
//static inline bool is_identifier_start( char c ) ;
static void advance_till( string s, int & pos, char c) ;

r_language_rep::r_language_rep (string name):
  abstract_language_rep (name)
{ 
  eval ("(use-modules (utils misc tm-keywords))");
  list<string> l= as_list_string (eval ("(map symbol->string highlight-any)"));
  while (!is_nil (l)) {
    colored (l->item)= "blue";
    l= l->next;
  }
  number_parser.use_r_style ();
}

text_property
r_language_rep::advance (tree t, int& pos) {
  string s= t->label;

  if (pos==N(s)) {  return &tp_normal_rep; }

  char c= s[pos];

  if (c == ' ') {
    pos++; return &tp_space_rep; 
  } 

  if (is_number_start(c)) {
    if (number_parser.parse (s, pos)) {
      return &tp_normal_rep;
    }
  }

  if (is_alpha (c) || is_in_str (c, "_.")
      //|| (c == '$') // For some reason, when this is uncommented, TeXmacs gets stuck on entering $.
      ) {
    parse_alpha (s, pos); 
    return &tp_normal_rep; 
  } 

  tm_char_forwards (s, pos);
  return &tp_normal_rep;
}

array<int>
r_language_rep::get_hyphens (string s) {
  int i;
  array<int> penalty (N(s)+1);
  penalty[0]= HYPH_INVALID;
  for (i=1; i < N(s); i++)
//    if ( (s[i-1] == '-' && is_alpha (s[i])) || N(s)>40   )
      penalty[i]= HYPH_STD;
//    else penalty[i]= HYPH_INVALID;
  penalty[i]= HYPH_INVALID;
  return penalty;
}

void
r_language_rep::hyphenate (
			   string s, int after, string& left, string& right)
{ 
  array<int> penalty= get_hyphens (s);
  std_hyphenate (s, after, left, right, penalty[after]);
}

static void
r_color_setup_constants (hashmap<string, string> & t) {
  string c= "#2060c0";
  t ("NULL")= c;
  t ("TRUE")= c;
  t ("FALSE")= c;
  t ("NA")= c;
  t ("Inf")= c;
  t ("NaN")= c;
  t ("LETTERS")= c;
  t ("letters")= c;
  t ("months.abb")= c;
  t ("months.name")= c;
  t ("pi")= c;
}

static void
r_color_setup_keywords (hashmap<string, string> & t)  {
  string c= "keyword"; string d= "modifier"; string e= "class";
  t ("for")= c;
  t ("while")= c;
  t ("if")= c;
  t ("else")= c;
  t ("break")= c;
  t ("repeat")= c;
  t ("next")= c;
  t ("switch")= c;
  t ("function")= c;
  t ("in")= c;
  t ("return")= c;
  t("...")=c ;
}

static void
r_color_setup_otherlexeme (hashmap<string, string>& t) {
  string c= "operator"; string d="op assign" ; string e="op index" ;
  t ("-<gtr>")= d;
  t ("=") = d ;
  t ("<less>-")= d; 
  t ("-<gtr><gtr>")= d; 
  t ("<less><less>-")= d; 
  t ("==")= c;
  t ("!=")= c;
  t ("<gtr>=" ) = c;
  t ("<less>=" ) = c;
  t ("<gtr>" ) = c;
  t ("<less>" ) = c;
  t ("~")= c;
  t ("*")= c;
  t ("/")= c;
  t ("+")= c;
  t ("-")= c;
  t ("^")= c;
  t (",")= c;
  t (";")= c;
  t (":")= c;
  t ("!")= c;

  t ("(")= e; // wasn't supposed to be left paren
  t (")")= e;
  t ("[")= e ;
  t ("]")= e;
  t ("[[")= e ;
  t ("]]")= e;
  t("$") = e ;

  //  t ("{")= c;
  //  t ("}")= c;
  t ("|")= c;
  t ("&")= c;
  t ("||")= c;
  t ("&&")= c;
}

bool
r_language_rep::belongs_to_identifier (char c) {
  return ( is_digit(c) ||
	   is_alpha(c) ||
	   is_in_str( c, "_." ) ) ;
}

static inline bool 
is_number_start( char c ) {
  return( is_digit(c) || is_in_str( c, "." ) ) ;
}

/*
static inline bool 
is_identifier_start( char c ) {
  return( is_alpha(c) || is_in_str( c, "._$`" ) ) ;
}
*/

void
r_language_rep::parse_identifier (hashmap<string, string>& t, string s, int& pos) {
  int i=pos;
  if (pos >= N(s)) return;

  if (s[pos] == '`') {
    pos++;
    advance_till ( s, pos, '`' );
    return ;
  }
  while (i<N(s) && belongs_to_identifier (s[i])) i++;

  if (!(t->contains (s (pos, i)))) pos= i;
}

void
r_language_rep::parse_identifier_or_markup (hashmap<string, string>& t,
    string s, int& pos, bool postfix, bool& is_markup) {
  int i=pos;
  is_markup= false;
  if (pos>=N(s)) return;
  if (postfix && s[i]=='.') i++;
  if( s[pos] == '`' ) {
    pos++ ;
    advance_till( s, pos, '`' ) ;
    return ;
  }
  while (i<N(s) && belongs_to_identifier (s[i])) {
    if (s[i]=='$') is_markup= true;
    i++;
  }
  if (!(t->contains (s (pos, i)))) pos= i;
}

static void
advance_till_unescaped( string s, int & pos, char c) {
  bool escaped = false ;
  if (pos>=N(s)) return;

  for( ; pos < N(s) ; pos++ ) {
    if( !escaped ) {
      if( s[pos] == c ) {
	if( pos+1 < N(s) ) pos++ ;
	return ;
      }
      if( s[pos] == '\\') escaped = true ;
    } else 
      escaped = false ;
  }
}

static void
advance_till( string s, int & pos, char c) {
  if (pos>=N(s)) return;

  for( ; pos < N(s) ; pos++ ) {
    if( s[pos] == c ) {
      if( pos+1 < N(s) ) pos++ ;
      return ;
    }
  }
}


static void
parse_string (string s, int& pos) {
  if (pos>=N(s)) return;
  switch (s[pos])  {
  case '\042': 
    {
      pos++ ;
      advance_till_unescaped( s, pos, '\042' ) ;
      return;
    } 
  case '\'': 
    {
      pos++ ;
      advance_till_unescaped( s, pos, '\'' ) ;
      return;
    }
  }
}
  
void
r_language_rep::parse_keyword (hashmap<string,string>& t, string s, int& pos) {
  int i= pos;
  if (pos>=N(s)) return;
  while ((i<N(s)) && belongs_to_identifier (s[i])) i++;
  string r= s (pos, i);
  if (t->contains (r) && t(r)=="keyword") { pos=i; return; }
}

void
r_language_rep::parse_modifier (hashmap<string,string>& t, string s, int& pos) {
  int i= pos;
  if ( pos >= N(s) ) return;
  while ((i<N(s)) && belongs_to_identifier (s[i])) i++;
  string r= s (pos, i);
  if (t->contains (r) && t(r)=="modifier") { pos=i; return; }
}

void
r_language_rep::parse_class (hashmap<string,string>& t, string s, int& pos) {
  int i= pos;
  if (pos>=N(s)) return;
  while ((i<N(s)) && belongs_to_identifier (s[i])) i++;
  string r= s (pos, i);
  if (t->contains (r) && t(r)=="class") { pos=i; return; }
}


void
r_language_rep::parse_postfix (hashmap<string,string>& t, string s, int& pos) {
  int i= pos;
  if (pos>=N(s)) return;
  while ((i<N(s)) && belongs_to_identifier (s[i])) i++;
  string r= s (pos, i);
  if (t->contains (r) && t(r)=="postfix") { pos=i; return; }
}

void
r_language_rep::parse_constant (hashmap<string,string>& t, string s, int& pos) {
  int i=pos;
  if (pos>=N(s)) return;
  while ((i<N(s)) && belongs_to_identifier (s[i])) i++;
  string r= s (pos, i);
  if (t->contains (r) && t(r)=="#2060c0") { pos=i; return; }
}

static void
parse_other_op_assign (hashmap<string,string>& t, string s, int& pos) {
  int i;
  i=13 ;
  if( i > N(s) ) i=N(s) ;
  for (i=13; i >= 1; i--) {
    string r=s(pos,pos+i);
    if (t->contains(r) && (t(r)=="op assign")) {
      pos=pos+i; return; }
    if (t->contains(r) && (t(r)=="operator")) {
      return; }
    if (t->contains(r) && (t(r)=="op index")) {
      return; }
  }
}

static void
parse_other_op_index (hashmap<string,string>& t, string s, int& pos) {
  int i;
  i=13 ;
  if( i > N(s) ) i=N(s) ;
  for (i=13; i >= 1; i--) {
    string r=s(pos,pos+i);
    if (t->contains(r) && (t(r)=="op index")) {
      pos=pos+i; return; }
    if (t->contains(r) && (t(r)=="operator")) {
      return; }
    if (t->contains(r) && (t(r)=="op assign")) {
      return; }
  }
}


static void
parse_other_lexeme (hashmap<string,string>& t, string s, int& pos) {
  int i;
  if( s[pos] == '%' )  {
    pos++ ;
    advance_till( s, pos, '%' ) ;
    return ;
  }
  for (i=12; i>=1; i--) {
    string r=s(pos,pos+i);
    if (t->contains(r) && ( (t(r)=="operator") || (t(r)=="op assign") || (t(r)=="op index") )) {
      pos=pos+i; return; }
  }
}

static bool 
is_in_str( char c, const char *str ) {
  while( *str != 0 ) {
    if( c == *str ) return true ;
    else str++ ;
  }
  return false ;
}

static void
parse_parenthesized (string s, int& pos) {
  int i=pos;
  if (pos>=N(s)) return;
  if (s[i]!='(') return;
  int nbpar=0;
  while(i<N(s)) {
    switch (s[i]) {
    case '(':
      nbpar++;break;
    case ')':
      if (nbpar>0) nbpar--;
      if (nbpar==0) {i++;pos=i;return;}
      break;
    case '/':
      if (i+1<N(s) && 
          (s[i+1]=='\042' || s[i+1]=='{' || s[i+1]=='/')) {
        pos= i;
        return;
      }
      break;
    case '\042':
      pos=i;
      return;
    }
    i++;
  }
  pos=i;
}

static void
parse_backquote (string s, int & pos) {
  if (pos>=N(s)) return;
  if (s[pos]=='\047') pos++;
}


string
r_language_rep::get_color (tree t, int start, int end) {
  static bool setup_done= false;
  if (!setup_done) {
    r_color_setup_constants (colored);
    r_color_setup_keywords (colored);
    r_color_setup_otherlexeme (colored);
    setup_done= true;
  }

  static string none= "";
  if (start >= end) return none;
  string s= t->label;
  string r1=s(0,start) ;

  int pos=0;int opos;
  bool backquote= false;
  bool after_backquote;
  bool postfix= false;
  bool possible_function= true;
  bool possible_type= false;
  bool possible_class= false;
  bool possible_future_type= false;
  bool possible_future_function= true;
  bool possible_future_class= false;
  string type;
  bool is_markup;

#if 0
  // There isn't much point to the following, because its only effet is pos and type, and both are reset below.
  do {
    do {
      opos=pos;

      parse_string (s, pos);
      if (opos<pos) break;

      if (inline_comment_parser.parse (s, pos)) {
        type= "comment";
        break;
      }

      pos++;
    } while(false);
  } while( pos<N(s) );
#endif 

  pos=0;
  do {
    type= none;
    do {
      after_backquote= backquote;
      possible_function= possible_future_function;
      possible_type= possible_future_type;
      possible_class= possible_future_class;
      opos= pos;

      if (blanks_parser.parse (s, pos)) break;

      parse_string (s, pos);
      if (opos<pos) {
        type= "string";
        backquote= false;
        postfix= false;
        possible_future_function= false;
        possible_future_type= false;
        possible_future_class= false;
        possible_type= false;
        break;
      }

      if (number_parser.parse (s, pos)) {
        type= "number";
        backquote= false;
        postfix= false;
        possible_future_function= false;
        possible_future_class= false;
        break;
      }

      if (inline_comment_parser.parse (s, pos)) {
        type= "comment";
        backquote= false;
        postfix= false;
        possible_future_type= false;
        possible_type= false;
        break;
      }

      parse_modifier (colored, s, pos);
      if (opos<pos) {
        type="keyword";
        backquote= false;
        postfix= false;
        possible_future_type= false;
        possible_type= false;
        possible_function= false;
        break;
      }

      parse_postfix (colored, s, pos);
      if (opos<pos) {
        type="keyword";
        backquote= false;
        postfix= true;
        possible_future_type= false;
        possible_future_class= false;
        possible_type= false;
        possible_function= false;
        possible_future_class= false;
        break;
      }

      parse_class (colored, s, pos);
      if (opos<pos) {
        type= "keyword";
        backquote=false;
        postfix=false;
        possible_future_type= false;
        possible_type= false;
        possible_future_class=true;
        possible_future_function= false;
        break;
      }

      parse_keyword (colored, s, pos);
      if (opos<pos) {
        type= "keyword";
        backquote= false;
        postfix= false;
        possible_future_type= false;
        possible_type= false;
        possible_function= false;
        possible_future_function= false;
        possible_future_class= false;
        break;
      }

      parse_other_op_assign (colored, s, pos);  //not left parenthesis
      if (opos<pos) {
        type= "other_lexeme";// was
        type = "op assign" ;
        backquote= false;
        postfix= false;
        possible_function= false;
        possible_future_function= true;
        possible_future_type= false;
        possible_future_class= false;
        possible_type= false;
        break;
      }

      parse_other_op_index (colored, s, pos);  //not left parenthesis
      if (opos<pos) {
        type= "other_lexeme";// was
        type = "op index" ;
        backquote= false;
        postfix= false;
        possible_function= false;
        possible_future_function= true;
        possible_future_type= false;
        possible_future_class= false;
        possible_type= false;
        break;
      }

      parse_other_lexeme (colored, s, pos);  //not left parenthesis
      if (opos<pos) {
        type= "other_lexeme";// was
        type = "operator" ;
        backquote= false;
        postfix= false;
        possible_function= false;
        possible_future_function= true;
        possible_future_type= false;
        possible_future_class= false;
        possible_type= false;
        break;
      }

      parse_constant (colored, s, pos);
      if (opos<pos) {
        type= "constant";
        backquote= false;
        postfix= false;
        possible_future_function= false;
        possible_future_class= false;
        break;
      }


      parse_backquote (s, pos);
      if (opos<pos) {
        backquote= true;
        postfix= false;
        possible_future_function= false;
        possible_future_class= false;
        break;
      }

      parse_identifier_or_markup (colored, s, pos, postfix, is_markup);
      if (opos<pos) {
        if (is_markup) {type= "identifier_markup";} else type= "identifier";
        backquote= false;
        postfix= false;
        possible_future_function=false;
        possible_future_class= false;
        break;
      }

      parse_parenthesized (s, pos);
      // stops after well parenthesized ) or before  // or /{ or " or /"
      if (opos<pos && pos<=start) {
        type="left_parenthesis";
        backquote= false;
        postfix= false;
        possible_function= false;
        possible_future_function= true;
        possible_future_class= false;
        break;
      }
      if (opos<pos && possible_type==true)
        return "dark green";
      if (opos<pos && after_backquote)  
        return none;

      backquote= false;
      postfix= false;
      pos= opos;
      pos++;
    } while (false); // This while() is so that we can easily escape with break.
  } while (pos<=start);

  if (possible_type) return "dark green";
  if (type=="string") return "#a06040";
  if (type=="operator") return "red";
  if (type=="op assign") return "dark green";
  if (type=="op index") return "dark blue";
  if (type=="comment") return "brown";
  if (type=="keyword" && !after_backquote) return "#8020c0";
  if (type=="other_lexeme") return none;
  if (type=="constant") return "#2060c0";
  if (type=="number") return "#2060c0";
  if (type=="left_parenthesis") return none;
  if (type=="identifier" && !possible_function && !possible_class )  return none;
  if (type=="identifier_markup" && !possible_function && !possible_class ) return COLOR_MARKUP;

  if ( (type=="identifier" || type=="identifier_markup") && possible_function) {
    possible_function= false;
    do {
      do {
        opos=pos;
        if (blanks_parser.parse (s, pos)) break;

        parse_identifier (colored, s, pos);
        if (opos<pos) { possible_function= true; break; }

        if (number_parser.parse (s, pos)) {
          possible_function= true;
          break;
        }

        parse_constant (colored, s, pos);
        if (opos<pos) { possible_function= true; break; }

        if (inline_comment_parser.parse (s, pos)) {
          break;
        }

        parse_parenthesized (s, pos);
        if (opos<pos) { possible_function= true; break; }
      } while (false);
    } while (opos != pos);

    if (!possible_function) {
      if (type=="identifier") {return none;} 
      else return COLOR_MARKUP; // type=="identifier_markup"
    } else do {
      do {
        opos=pos;
        if (blanks_parser.parse (s, pos)) break;
        parse_identifier (colored, s, pos);
        if (opos<pos) break;
        number_parser.parse (s, pos);
        if (opos<pos) break;
        parse_constant (colored, s, pos);
        if (opos<pos) break;
        if (inline_comment_parser.parse (s, pos)) {
          break;
        }
        parse_parenthesized (s, pos);
        if (opos<pos) break;

        if (type=="identifier") {return none;} 
        else return COLOR_MARKUP;

      } while (false);
    } while (pos<N(s));
  } // type==identifier || type==identifier_markup && possible function

  if ( (type=="identifier" || type=="identifier_markup") && possible_class) {
    do {
      do {
        opos=pos;
        if (blanks_parser.parse (s, pos)) break;

        parse_identifier (colored, s, pos);
        if (opos<pos) break;

        if (number_parser.parse (s, pos)) {
          break;
        }

        parse_constant (colored, s, pos);
        if (opos<pos) break;

        if (inline_comment_parser.parse (s, pos)) {
          break;
        }

        parse_parenthesized (s, pos);
        if (opos<pos) break;

        if (type=="identifier") {return none;} 
        else return COLOR_MARKUP;
      } while (false);
    }
    while (pos<N(s));
  }
  if (type=="identifier_markup") {return COLOR_MARKUP;}
  else return none;
}
