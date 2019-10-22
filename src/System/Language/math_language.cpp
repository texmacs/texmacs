
/******************************************************************************
* MODULE     : math_language.cpp
* DESCRIPTION: mathematical languages
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "analyze.hpp"
#include "hyphenate.hpp"
#include "impl_language.hpp"
#include "file.hpp"
#include "iterator.hpp"
#include "packrat_grammar.hpp"

/******************************************************************************
* Mathematical languages
******************************************************************************/

struct math_language_rep: language_rep {
  hashmap<string,string>            group;
  hashmap<string,text_property_rep> tpr_class;
  hashmap<string,text_property_rep> tpr_member;

  math_language_rep (string name);
  void set_type (string cl, string s);
  void set_left_penalty (string cl, string s);
  void set_right_penalty (string cl, string s);
  void set_left_spacing (string cl, string s);
  void set_right_spacing (string cl, string s);
  void set_limits (string cl, string s);
  void set_macro (string sym, string s);

  void skip_spaces (string s, int& pos, space fn_spc, space& spc);
  string next_word (string s, int& pos);
  text_property advance (tree t, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
  string get_group (string s);
  array<string> get_members (string s);
};

/******************************************************************************
* Load a mathematical language
******************************************************************************/

void
math_language_rep::set_type (string cl, string s) {
  int &ot= tpr_class(cl).op_type;
  if (s == "symbol") ot= OP_SYMBOL;
  else if (s == "unary") ot= OP_UNARY;
  else if (s == "binary") ot= OP_BINARY;
  else if (s == "n-ary") ot= OP_N_ARY;
  else if (s == "prefix") ot= OP_PREFIX;
  else if (s == "postfix") ot= OP_POSTFIX;
  else if (s == "infix") ot= OP_INFIX;
  else if (s == "prefix-infix") ot= OP_PREFIX_INFIX;
  else if (s == "separator") ot= OP_SEPARATOR;
  else if (s == "opening-bracket") ot= OP_OPENING_BRACKET;
  else if (s == "middle-bracket") ot= OP_MIDDLE_BRACKET;
  else if (s == "closing-bracket") ot= OP_CLOSING_BRACKET;
  else {
    failed_error << "Attempt to associate type " << s << " to " << cl << "\n";
    FAILED ("invalid type");
  }
}

void
math_language_rep::set_left_penalty (string cl, string s) {
  if (is_int (s)) tpr_class(cl).pen_before= as_int (s);
  else if (s == "panic")   tpr_class(cl).pen_before= HYPH_PANIC;
  else if (s == "invalid") tpr_class(cl).pen_before= HYPH_INVALID;
  else {
    failed_error << "Attempt to associate left penalty " << s
                 << " to " << cl << "\n";
    FAILED ("invalid penalty");
  }
}

void
math_language_rep::set_right_penalty (string cl, string s) {
  if (is_int (s)) tpr_class(cl).pen_after= as_int (s);
  else if (s == "panic")   tpr_class(cl).pen_after= HYPH_PANIC;
  else if (s == "invalid") tpr_class(cl).pen_after= HYPH_INVALID;
  else {
    failed_error << "Attempt to associate right penalty " << s
                 << " to " << cl << "\n";
    FAILED ("invalid penalty");
  }
}

void
math_language_rep::set_left_spacing (string cl, string s) {
  if      (s == "none")     tpr_class(cl).spc_before= SPC_NONE;
  else if (s == "half")     tpr_class(cl).spc_before= SPC_HALF;
  else if (s == "default")  tpr_class(cl).spc_before= SPC_OPERATOR;
  else if (s == "wide")     tpr_class(cl).spc_before= SPC_WIDEOP;
  else if (s == "big")      tpr_class(cl).spc_before= SPC_BIGOP;
  else if (s == "multiply") tpr_class(cl).spc_before= SPC_MULTIPLY;
  else if (s == "middle")   tpr_class(cl).spc_before= SPC_MIDDLE;
  else {
    failed_error << "Attempt to associate left spacing " << s
                 << " to " << cl << "\n";
    FAILED ("invalid spacing");
  }
}

void
math_language_rep::set_right_spacing (string cl, string s) {
  if      (s == "none")     tpr_class(cl).spc_after= SPC_NONE;
  else if (s == "half")     tpr_class(cl).spc_after= SPC_HALF;
  else if (s == "default")  tpr_class(cl).spc_after= SPC_OPERATOR;
  else if (s == "wide")     tpr_class(cl).spc_after= SPC_WIDEOP;
  else if (s == "big")      tpr_class(cl).spc_after= SPC_BIGOP;
  else if (s == "multiply") tpr_class(cl).spc_after= SPC_MULTIPLY;
  else if (s == "middle")   tpr_class(cl).spc_after= SPC_MIDDLE;
  else {
    failed_error << "Attempt to associate right spacing " << s
                 << " to " << cl << "\n";
    FAILED ("invalid spacing");
  }
}

void
math_language_rep::set_limits (string cl, string s) {
  if      (s == "none")    tpr_class(cl).limits= LIMITS_NONE;
  else if (s == "display") tpr_class(cl).limits= LIMITS_DISPLAY;
  else if (s == "always")  tpr_class(cl).limits= LIMITS_ALWAYS;
  else {
    failed_error << "Attempt to associate limits " << s
                 << " to " << cl << "\n";
    FAILED ("invalid limits");
  }
}

void
math_language_rep::set_macro (string sym, string s) {
  tpr_member(sym)= copy (tpr_member(sym));
  tpr_member(sym).macro= make_tree_label (s);
}

//math_language_rep::math_language_rep (string name, string s):
math_language_rep::math_language_rep (string name):
  language_rep (name),
  group ("symbol"),
  tpr_class (text_property_rep ()),
  tpr_member (text_property_rep ())
{
  language::instances (name)= (pointer) this;
  tpr_class("symbol").op_type = OP_SYMBOL;

  packrat_grammar gr= find_packrat_grammar (name);
  hashmap<D,string> props= gr->properties;
  hashmap<string,bool> cls (false);
  iterator<D> it= iterate (props);
  while (it->busy ()) {
    D key = it->next ();
    C prop= ((C) (key >> 32));
    C sym = ((C) (key & 0xffffffff)) ^ prop;
    ASSERT (is_compound (packrat_decode[sym], "symbol", 1) &&
            is_compound (packrat_decode[prop], "property", 1),
            "invalid symbol or property");
    string cl = packrat_decode[sym ][0]->label;
    string var= packrat_decode[prop][0]->label;
    string val= props[key];
    //cout << cl << ", " << var << " -> " << val << "\n";
    if (var == "type") { set_type (cl, val); cls (cl)= true; }
    else if (var == "left-penalty") set_left_penalty (cl, val);
    else if (var == "right-penalty") set_right_penalty (cl, val);
    else if (var == "left-spacing") set_left_spacing (cl, val);
    else if (var == "right-spacing") set_right_spacing (cl, val);
    else if (var == "limits") set_limits (cl, val);
  }

  iterator<string> it2= iterate (cls);
  while (it2->busy ()) {
    string cl= it2->next ();
    array<string> a= gr->members (cl);
    for (int i=0; i<N(a); i++) {
      group (a[i])= cl;
      tpr_member (a[i])= tpr_class [cl];
    }
  }

  it= iterate (props);
  while (it->busy ()) {
    D key = it->next ();
    C prop= ((C) (key >> 32));
    C sym = ((C) (key & 0xffffffff)) ^ prop;
    string smb= packrat_decode[sym ][0]->label;
    string var= packrat_decode[prop][0]->label;
    string val= props[key];
    if (var == "macro") set_macro (smb, val);
  }
}

/******************************************************************************
* Advancing a word
******************************************************************************/

string
math_language_rep::next_word (string s, int& pos) {
  int start= pos;

  if (pos>=N(s)) return string ("");

  if (is_digit (s[pos])) {
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
math_language_rep::advance (tree t, int& pos) {
  string s= t->label;
  bool op_flag1=
    (pos==0) ||
    ((pos>=2) && is_alpha (s[pos-2]) && is_alpha (s[pos-1]));
  string r= next_word (s, pos);
  if (r == " ") {
    bool op_flag2=
      (pos==N(s)) ||
      (((pos+2)<N(s)) && is_alpha (s[pos]) && is_alpha (s[pos+1]));
    if (op_flag1 || op_flag2) return &tp_apply_rep;
    else return &tp_short_apply_rep;
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
  ASSERT (N(s) != 0, "hyphenation of empty string");
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
* Get the group (class) of a symbol
******************************************************************************/

string
math_language_rep::get_group (string s) {
  return group[s];
}

array<string>
math_language_rep::get_members (string g) {
  array<string> r;
  iterator<string> it= iterate (group);
  while (it->busy ()) {
    string s= it->next ();
    if (group[s] == g) r << s;
  }
  return r;
}

/******************************************************************************
* Interface
******************************************************************************/

language
math_language (string name) {
  if (language::instances -> contains (name)) return language (name);
  return tm_new<math_language_rep> (name);
}

string
math_symbol_group (string sym, string lang) {
  language lan= math_language (lang);
  return lan->get_group (sym);
}

array<string>
math_group_members (string gr, string lang) {
  language lan= math_language (lang);
  return lan->get_members (gr);
}

string
math_symbol_type (string sym, string lang) {
  int pos= 0;
  language lan= math_language (lang);
  text_property prop= lan->advance (tree (sym), pos);
  switch (prop->op_type) {
  case OP_UNKNOWN:
    return "unknown";
  case OP_SYMBOL:
    return "symbol";
  case OP_UNARY:
    return "unary";
  case OP_BINARY:
    return "binary";
  case OP_N_ARY:
    return "n-ary";
  case OP_PREFIX:
    return "prefix";
  case OP_POSTFIX:
    return "postfix";
  case OP_INFIX:
    return "infix";
  case OP_PREFIX_INFIX:
    return "prefix-infix";
  case OP_SEPARATOR:
    return "separator";
  case OP_OPENING_BRACKET:
    return "opening-bracket";
  case OP_MIDDLE_BRACKET:
    return "middle-bracket";
  case OP_CLOSING_BRACKET:
    return "closing-bracket";
  }
  return "unknown";
}
