
/******************************************************************************
* MODULE     : math_language.cpp
* DESCRIPTION: mathematical languages
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "language.hpp"
#include "analyze.hpp"
#include "Languages/hyphenate.hpp"
#include "Languages/impl_language.hpp"
#include "file.hpp"

/******************************************************************************
* Mathematical languages
******************************************************************************/

struct math_language_rep: language_rep {
  hashmap<string,text_property_rep> tpr_class;
  hashmap<string,text_property_rep> tpr_member;
  string class_name;            // current type name
  bool   class_def;             // in class definition phase

  math_language_rep (string name, string s);
  string get_string (string s, int& i);
  void get_class (string s, int& i);
  void get_members (string s, int& i);
  void get_type (string s, int& i);
  void get_precedence (string s, int& i);
  void get_lpenalty (string s, int& i);
  void get_rpenalty (string s, int& i);
  void get_spacing (string s, int& i);
  void get_limits (string s, int& i);

  void skip_spaces (string s, int& pos, space fn_spc, space& spc);
  string next_word (string s, int& pos);
  text_property advance (string s, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
};

/******************************************************************************
* Load a mathematical language
******************************************************************************/

string
math_language_rep::get_string (string s, int& i) {
  while ((i<N(s)) && (s[i]!=' ') && (s[i]!='\t') && (s[i]!='\n')) i++;
  while ((i<N(s)) && ((s[i]==' ') || (s[i]=='\t') || (s[i]=='='))) i++;
  int start= i;
  while ((i<N(s)) && (s[i]!='\n')) i++;
  int end= i;
  while ((end>start) && ((s[end-1]==' ') || (s[end-1]=='\t'))) end--;
  // cout << "String= " << s (start, end) << "\n";
  return s (start, end);
}

void
math_language_rep::get_class (string s, int& i) {
  class_name= get_string (s, i);
  class_def = true;
}

void
math_language_rep::get_members (string s, int& i) {
  class_name= get_string (s, i);
  class_def = false;
  // cout << "Members " << class_name << "\n";
}

void
math_language_rep::get_type (string s, int& i) {
  if (class_def) {
    string r= get_string (s, i);
    int &ot= tpr_class(class_name).op_type;
    if (r == "Symbol") ot= OP_SYMBOL;
    else if (r == "Prefix") ot= OP_PREFIX;
    else if (r == "Postfix") ot= OP_POSTFIX;
    else if (r == "Infix") ot= OP_INFIX;
    else if (r == "Left Associative Infix") ot= OP_LEFT_ASS_INFIX;
    else if (r == "Right Associative Infix") ot= OP_RIGHT_ASS_INFIX;
    else if (r == "Associative Infix") ot= OP_ASS_INFIX;
    else if (r == "Opening Bracket") ot= OP_OPENING_BRACKET;
    else if (r == "Separator") ot= OP_SEPARATOR;
    else if (r == "Closing Bracket") ot= OP_CLOSING_BRACKET;
    else {
      cerr << "Attempt to associate type " << r
	   << " to " << class_name << "\n";
      fatal_error ("Unknown type", "math_language_rep::get_type");
    }
  }
  else fatal_error ("Type declaration outside class definition",
		    "math_language_rep::get_type");
}

void
math_language_rep::get_precedence (string s, int& i) {
  if (class_def) tpr_class(class_name).priority= as_int (get_string (s, i));
  else fatal_error ("Precedence declaration outside class definition",
		    "math_language_rep::get_precedence");
}

void
math_language_rep::get_lpenalty (string s, int& i) {
  if (class_def) {
    string r= get_string (s, i);
    if (r == "Panic")   tpr_class(class_name).pen_before= HYPH_PANIC;
    if (r == "Invalid") tpr_class(class_name).pen_before= HYPH_INVALID;
    else tpr_class(class_name).pen_before= as_int (r);
  }
  else fatal_error ("Left Penalty declaration outside class definition",
		    "math_language_rep::get_lpenalty");
}

void
math_language_rep::get_rpenalty (string s, int& i) {
  if (class_def) {
    string r= get_string (s, i);
    if (r == "Panic") tpr_class(class_name).pen_after= HYPH_PANIC;
    if (r == "Invalid") tpr_class(class_name).pen_after= HYPH_INVALID;
    else tpr_class(class_name).pen_after= as_int (r);
  }
  else fatal_error ("Right Penalty declaration outside class definition",
		    "math_language_rep::get_rpenalty");
}

void
math_language_rep::get_spacing (string s, int& i) {
  if (class_def) {
    int j;
    string both= get_string (s, i);
    for (j=0; j<N(both); j++)
      if (both[j]==',') {
	string l= both (0, j);
	string r= both (j+1, N(both));
	while ((N(l)>0) && ((l[N(l)-1] == ' ') || (l[N(l)-1] == '\t')))
	  l= l(0, N(l)-1);
	while ((N(r)>0) && ((r[0]==' ') || (r[0]=='\t'))) r= r(1, N(r));
	if (l == "None")
	  tpr_class(class_name).spc_before= SPC_NONE;
	else if (l == "Default")
	  tpr_class(class_name).spc_before= SPC_OPERATOR;
	else if (l == "Big")
	  tpr_class(class_name).spc_before= SPC_BIGOP;
	else {
	  cerr << "Attempt to associate space " << l
	       << " to " << class_name << "\n";
	  fatal_error ("Unknown space", "math_language_rep::get_type");
	}
	if (r == "None")
	  tpr_class(class_name).spc_after= SPC_NONE;
	else if (r == "Default")
	  tpr_class(class_name).spc_after= SPC_OPERATOR;
	else if (r == "Big")
	  tpr_class(class_name).spc_after= SPC_BIGOP;
	else {
	  cerr << "Attempt to associate space " << r
	       << " to " << class_name << "\n";
	  fatal_error ("Unknown space", "math_language_rep::get_type");
	}
	return;
      }
    fatal_error ("Missing comma in Spacing declaration",
		 "math_language_rep::get_spacing");
  }
  else fatal_error ("Spacing declaration outside class definition",
		    "math_language_rep::get_spacing");
}

void
math_language_rep::get_limits (string s, int& i) {
  if (class_def) {
    string l= get_string (s, i);
    if (l == "Display") tpr_class(class_name).limits= LIMITS_DISPLAY;
    else if (l == "Always") tpr_class(class_name).limits= LIMITS_ALWAYS;
    else tpr_class(class_name).limits= LIMITS_NONE;
  }
  else fatal_error ("Limits declaration outside class definition",
		    "math_language_rep::get_limits");
}

math_language_rep::math_language_rep (string name, string s):
  language_rep (name, math_enc),
  tpr_class (text_property_rep ()),
  tpr_member (text_property_rep ()),
  class_name ("symbol"), class_def (true)
{
  language::instances (name)= (pointer) this;
  tpr_class("symbol").op_type = OP_SYMBOL;
  tpr_class("symbol").priority= 1000;

  int i;
  for (i=0; i<N(s); i++) {
    while ((i<N(s)) && ((s[i]==' ') || (s[i]=='\t'))) i++;
    if ((i+5<N(s)) && (s(i,i+5)=="Class")) get_class (s, i);
    else if ((i+7<N(s)) && (s(i,i+7)=="Members")) get_members (s, i);
    else if ((i+4<N(s)) && (s(i,i+4)=="Type")) get_type (s, i);
    else if ((i+10<N(s)) && (s(i,i+10)=="Precedence")) get_precedence (s, i);
    else if ((i+12<N(s)) && (s(i,i+12)=="Left-Penalty")) get_lpenalty (s, i);
    else if ((i+7<N(s)) && (s(i,i+7)=="Penalty")) get_rpenalty (s, i);
    else if ((i+13<N(s)) && (s(i,i+13)=="Right-Penalty")) get_rpenalty (s, i);
    else if ((i+7<N(s)) && (s(i,i+7)=="Spacing")) get_spacing (s, i);
    else if ((i+6<N(s)) && (s(i,i+6)=="Limits")) get_limits (s, i);
    else if ((i+2<N(s)) && (s(i,i+2)=="##")) (void) get_string (s, i);
    else {
      while ((i<N(s)) && (s[i]!='\n')) {
	while ((i<N(s)) && ((s[i]==' ') || (s[i]=='\t'))) i++;
	int start= i;
	while ((i<N(s)) && (s[i]!=' ') && (s[i]!='\t') && (s[i]!='\n')) i++;
	string symbol= s (start, i);
	if ((N(symbol)>1) &&
	    (class_def || (class_name != "operator-with-limits")))
	  symbol= "<" * symbol * ">";
	// cout << "  Member: " << symbol << "\n";
	if ((!class_def) && (tpr_class->contains (class_name)))
	  tpr_member (symbol)= tpr_class [class_name];
	else {
	  cerr << "Attempt to insert " << symbol
	       << " to class " << class_name << "\n";
	  if (class_def)
	    fatal_error ("Member declaration inside class definition",
			 "math_language_rep::get_members");
	  else
	    fatal_error ("Unknown class",
			 "math_language_rep::get_members");
	}
      }
    }
  }
}

/******************************************************************************
* Advancing a word
******************************************************************************/

string
math_language_rep::next_word (string s, int& pos) {
  int start= pos;

  if (pos>=N(s)) return string ("");

  if ((s[pos]>='0') && (s[pos]<='9')) {
    while ((pos<N(s)) && is_numeric (s[pos])) pos++;
    while (s[pos-1]=='.') pos--;
    return s (start, pos);
  }

  if (is_alpha (s[pos])) {
    while ((pos<N(s)) && (is_alpha (s[pos]))) pos++;
    return s (start, pos);
  }

  if (s[pos]=='<') {
    while ((pos<N(s)) && (s[pos]!='>')) pos++;
    if (pos<N(s)) pos++;
    return s (start, pos);
  }

  pos++;
  return s (start, pos);
}

text_property
math_language_rep::advance (string s, int& pos) {
  bool op_flag1=
    (pos==0) ||
    ((pos>=2) && is_alpha (s[pos-2]) && is_alpha (s[pos-1]));
  string r= next_word (s, pos);
  if (r == " ") {
    bool op_flag2=
      (pos==N(s)) ||
      (((pos+2)<N(s)) && is_alpha (s[pos]) && is_alpha (s[pos+1]));
    if (op_flag1 || op_flag2) return &tp_operator_rep;
    else return &tp_shortop_rep;
  }
  return &tpr_member(r);

  /****************************** variant *******************************
  string r= next_word (s, pos);
  if (r == " ") return &tp_operator_rep;
  else return &tpr_member(r);
  **********************************************************************/
}

/******************************************************************************
* Hyphenation
******************************************************************************/

array<int>
math_language_rep::get_hyphens (string s) {
  if (N(s)==0)
    fatal_error ("hyphenation of empty string",
		 "math_language_rep::get_hyphens");

  int i, n= N(s)-1;
  bool flag= is_numeric (s);
  array<int> penalty (n);
  for (i=0; i<n; i++) penalty[i]= (flag? HYPH_PANIC: HYPH_INVALID);
  if (n>0) penalty[0]= penalty[n-1]= HYPH_INVALID;
  if (n>2) penalty[1]= penalty[n-2]= HYPH_INVALID;
  if (n>4) penalty[2]= penalty[n-3]= HYPH_INVALID;
  return penalty;
}

void
math_language_rep::hyphenate (string s, int after, string& left, string& right)
{
  left = s (0, after+1) * string ("\\");
  right= s (after+1, N(s));
}

/******************************************************************************
* Interface
******************************************************************************/

language
math_language (string name) {
  if (language::instances -> contains (name)) return language (name);
  string s, fname= name * ".syx";
  if (DEBUG_VERBOSE) cout << "TeXmacs] Loading " << fname << "\n";
  load_string (url ("$TEXMACS_SYNTAX_PATH", fname), s, true);
  return new math_language_rep (name, s);
}
