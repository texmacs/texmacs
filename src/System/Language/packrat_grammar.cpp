
/******************************************************************************
* MODULE     : packrat_grammar.cpp
* DESCRIPTION: packrat grammars
* COPYRIGHT  : (C) 2010  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "packrat_grammar.hpp"
#include "analyze.hpp"
#include "iterator.hpp"

int               packrat_nr_tokens= 256;
int               packrat_nr_symbols= 0;
hashmap<string,C> packrat_tokens;
hashmap<tree,C>   packrat_symbols;
tree              packrat_uninit (UNINIT);

RESOURCE_CODE(packrat_grammar);

/******************************************************************************
* Encoding tokens and symbols
******************************************************************************/

C
new_token (string s) {
  if (N(s) == 1)
    return (C) (unsigned char) s[0];
  else {
    C ret= packrat_nr_tokens++;
    //cout << "Encode " << s << " -> " << ret << LF;
    return ret;
  }
}

C
encode_token (string s) {
  if (!packrat_tokens->contains (s)) {
    int pos= 0;
    tm_char_forwards (s, pos);
    if (pos == 0 || pos != N(s)) return -1;
    packrat_tokens (s)= new_token (s);
  }
  return packrat_tokens[s];
}

array<C>
encode_tokens (string s) {
  int pos= 0;
  array<C> ret;
  while (pos < N(s)) {
    int old= pos;
    tm_char_forwards (s, pos);
    ret << encode_token (s (old, pos));
  }
  return ret;
}

C
new_symbol (tree t) {
  if (is_atomic (t)) {
    C ret= encode_token (t->label);
    if (ret != -1) return ret;
  }
  C ret= (packrat_nr_symbols++) + PACKRAT_SYMBOLS;
  //cout << "Encode " << t << " -> " << ret << LF;
  return ret;
}

C
encode_symbol (tree t) {
  if (!packrat_symbols->contains (t))
    packrat_symbols (t)= new_symbol (t);
  return packrat_symbols[t];
}

/******************************************************************************
* Left recursion
******************************************************************************/

bool
left_recursive (string s, tree t) {
  if (is_atomic (t))
    return false;
  else if (is_compound (t, "symbol", 1) && is_atomic (t[0]))
    return t[0]->label == s;
  else if (is_compound (t, "or")) {
    for (int i=0; i<N(t); i++)
      if (left_recursive (s, t[i]))
	return true;
    return false;
  }
  else if (is_compound (t, "concat"))
    return N(t) > 0 && left_recursive (s, t[0]);
  else return false;
}

tree
left_head (string s, tree t) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "symbol", 1) && is_atomic (t[0])) {
    if (t[0]->label == s) return compound ("or");
    else return t;
  }
  else if (is_compound (t, "or")) {
    tree r= compound ("or");
    for (int i=0; i<N(t); i++) {
      tree h= left_head (s, t[i]);
      if (is_compound (h, "or")) r << A(h);
      else r << h;
    }
    return r;
  }
  else if (is_compound (t, "concat")) {
    if (N(t) == 0) return t;
    else return left_head (s, t[0]);
  }
  else return t;
}

tree
left_tail (string s, tree t) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "symbol", 1) && is_atomic (t[0])) {
    if (t[0]->label == s) return compound ("concat");
    else return compound ("or");
  }
  else if (is_compound (t, "or")) {
    tree r= compound ("or");
    for (int i=0; i<N(t); i++) {
      tree u= left_tail (s, t[i]);
      if (is_compound (u, "or")) r << A(u);
      else r << u;
    }
    return r;
  }
  else if (is_compound (t, "concat")) {
    if (N(t) == 0) return compound ("or");
    else {
      tree r= compound ("concat");
      tree u= left_tail (s, t[0]);
      if (u == compound ("or")) return u;
      else if (is_compound (u, "concat")) r << A(u);
      else r << u;
      r << A (t (1, N(t)));
      return r;
    }
  }
  else compound ("or");
}

/******************************************************************************
* Constructor
******************************************************************************/

array<C>
empty_rule () {
  array<C> ret;
  ret << PACKRAT_OR;
  return ret;
}

packrat_grammar_rep::packrat_grammar_rep (string s):
  rep<packrat_grammar> (s),
  grammar (empty_rule ()),
  productions (packrat_uninit) {}

packrat_grammar
make_packrat_grammar (string s) {
  if (packrat_grammar::instances -> contains (s)) return packrat_grammar (s);
  return make (packrat_grammar, s, tm_new<packrat_grammar_rep> (s));
}

/******************************************************************************
* Definition of grammars
******************************************************************************/

array<C>
packrat_grammar_rep::define (tree t) {
  //cout << "Define " << t << INDENT << LF;
  array<C> def;
  if (t == "")
    def << PACKRAT_CONCAT;
  else if (is_atomic (t)) {
    int pos= 0;
    string s= t->label;
    tm_char_forwards (s, pos);
    if (pos > 0 && pos == N(s)) def << encode_symbol (s);
    else def << PACKRAT_CONCAT << encode_tokens (s);
  }
  else if (is_compound (t, "symbol", 1))
    def << encode_symbol (t);
  else if (is_compound (t, "range", 2) &&
	   is_atomic (t[0]) && is_atomic (t[1]) &&
	   N(t[0]->label) == 1 && N(t[1]->label) == 1)
    def << PACKRAT_RANGE
	<< encode_token (t[0]->label)
	<< encode_token (t[1]->label);
  else if (is_compound (t, "or", 1))
    def << define (t[0]);
  else {
    if (is_compound (t, "or")) def << PACKRAT_OR;
    else if (is_compound (t, "concat")) def << PACKRAT_CONCAT;
    else if (is_compound (t, "while")) def << PACKRAT_WHILE;
    else if (is_compound (t, "repeat")) def << PACKRAT_REPEAT;
    else if (is_compound (t, "not")) def << PACKRAT_NOT;
    else def << PACKRAT_UNKNOWN;
    for (int i=0; i<N(t); i++) {
      (void) define (t[i]);
      def << encode_symbol (t[i]);
    }
  }
  if (N (def) != 1 || def[0] != encode_symbol (t)) {
    C sym= encode_symbol (t);
    grammar (sym)= def;
  }
  //cout << UNINDENT << "Defined " << t << " -> " << def << LF;
  return def;
}

void
packrat_grammar_rep::define (string s, tree t) {
  //cout << "Define " << s << " := " << t << "\n";
  if (left_recursive (s, t)) {
    string s1= s * "-head";
    string s2= s * "-tail";
    tree   t1= left_head (s, t);
    tree   t2= left_tail (s, t);
    define (s1, t1);
    define (s2, t2);
    tree   u1= compound ("symbol", s1);
    tree   u2= compound ("while", compound ("symbol", s2));
    define (s, compound ("concat", u1, u2));
  }
  else {
    C        sym= encode_symbol (compound ("symbol", s));
    array<C> def= define (t);
    grammar (sym)= def;
  }
}

/******************************************************************************
* Interface
******************************************************************************/

void
packrat_define (string lan, string s, tree t) {
  packrat_grammar gr= make_packrat_grammar (lan);
  gr->define (s, t);
}

void
packrat_inherit (string lan, string from) {
  packrat_grammar gr = make_packrat_grammar (lan);
  packrat_grammar inh= make_packrat_grammar (from);
  iterator<C>     it = iterate (inh->grammar);
  while (it->busy ()) {
    C sym= it->next ();
    //cout << "Inherit " << sym << " -> " << inh->grammar (sym) << LF;
    gr->grammar (sym)= inh->grammar (sym);
    gr->productions (sym)= inh->productions (sym);
  }
}
