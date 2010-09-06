
/******************************************************************************
* MODULE     : packrat.cpp
* DESCRIPTION: efficient packrat parsing
* COPYRIGHT  : (C) 2010  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "packrat.hpp"
#include "array.hpp"
#include "hashmap.hpp"
#include "analyze.hpp"
#include <stdint.h>
#define C int32_t
#define D int64_t

/******************************************************************************
* Important constants
******************************************************************************/

#define PACKRAT_TOKENS    ((C)         0)
#define PACKRAT_OR        ((C) 100000000)
#define PACKRAT_CONCAT    ((C) 100000001)
#define PACKRAT_WHILE     ((C) 100000002)
#define PACKRAT_REPEAT    ((C) 100000003)
#define PACKRAT_RANGE     ((C) 100000004)
#define PACKRAT_NOT       ((C) 100000005)
#define PACKRAT_UNKNOWN   ((C) 100000006)
#define PACKRAT_SYMBOLS   ((C) 100000007)

#define PACKRAT_UNDEFINED ((C) (-2))
#define PACKRAT_FAILED    ((C) (-1))

/******************************************************************************
* Global variables
******************************************************************************/

array<array<C> >  packrat_grammar;
array<tree>       packrat_production;
int               packrat_nr_tokens= 256;
int               packrat_nr_symbols= 0;
hashmap<string,C> packrat_tokens;
hashmap<string,C> packrat_symbols;

array<C>          current_input;
hashmap<D,C>      current_cache (PACKRAT_UNDEFINED);
tree              tree_uninit (UNINIT);
hashmap<D,tree>   current_production (tree_uninit);

/******************************************************************************
* Forward definitions
******************************************************************************/

array<C> encode_tokens (string s);
array<C> packrat_define (tree t);

/******************************************************************************
* Encoding tokens and symbols
******************************************************************************/

C
new_token (string s) {
  if (N(s) == 1)
    return (C) (unsigned char) s[0];
  else {
    C ret= packrat_nr_tokens++;
    return ret;
  }
}

C
new_symbol () {
  C ret= packrat_nr_symbols++;
  packrat_grammar->resize (packrat_nr_symbols);
  return ret;
}

C
encode_token (string s) {
  if (!packrat_tokens->contains (s)) {
    int pos= 0;
    tm_char_forwards (s, pos);
    if (pos > 0 && pos == N(s))
      packrat_tokens (s)= new_token (s);
    else {
      C aux= new_symbol ();
      array<C> ret;
      ret << PACKRAT_CONCAT << encode_tokens (s);
      packrat_grammar[aux]= ret;
      packrat_tokens (s)  = aux + PACKRAT_SYMBOLS;
    }
    //cout << "Encode " << s << " -> " << packrat_tokens[s] << LF;
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
encode_symbol (string s) {
  if (!packrat_symbols->contains (s)) {
    packrat_symbols (s)= new_symbol () + PACKRAT_SYMBOLS;
    //cout << "Encode " << s
    //<< " -> " << packrat_symbols[s] + PACKRAT_SYMBOLS << LF;
  }
  return packrat_symbols[s];
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
      r << t (1, N(t));
      return r;
    }
  }
  else compound ("or");
}

/******************************************************************************
* Definition of grammars
******************************************************************************/

C
packrat_define_one (tree t) {
  //cout << "Define one " << t << LF;
  if (is_atomic (t))
    return encode_token (t->label);
  else if (is_compound (t, "symbol", 1) && is_atomic (t[0]))
    return encode_symbol (t[0]->label);
  else {
    C aux= new_symbol ();
    packrat_grammar[aux]= packrat_define (t);
    return aux + PACKRAT_SYMBOLS;
  }
}

array<C>
packrat_define (tree t) {
  //cout << "Define " << t << INDENT << LF;
  array<C> ret;
  if (t == "")
    ret << PACKRAT_CONCAT;
  else if (is_atomic (t))
    ret << encode_token (t->label);
  else if (is_compound (t, "symbol", 1) && is_atomic (t[0]))
    ret << encode_symbol (t[0]->label);
  else if (is_compound (t, "range", 2) &&
	   is_atomic (t[0]) && is_atomic (t[1]) &&
	   N(t[0]->label) == 1 && N(t[1]->label) == 1)
    ret << PACKRAT_RANGE
	<< encode_token (t[0]->label)
	<< encode_token (t[1]->label);
  else if (is_compound (t, "or", 1))
    ret << packrat_define (t[0]);
  else if (is_compound (t, "concat", 1))
    ret << packrat_define (t[0]);
  else {
    if (is_compound (t, "or")) ret << PACKRAT_OR;
    else if (is_compound (t, "concat")) ret << PACKRAT_CONCAT;
    else if (is_compound (t, "while")) ret << PACKRAT_WHILE;
    else if (is_compound (t, "repeat")) ret << PACKRAT_REPEAT;
    else if (is_compound (t, "not")) ret << PACKRAT_NOT;
    else ret << PACKRAT_UNKNOWN;
    for (int i=0; i<N(t); i++)
      ret << packrat_define_one (t[i]);
  }
  //cout << UNINDENT << "Defined " << t << " -> " << ret << LF;
  return ret;
}

void
packrat_define (string s, tree t) {
  if (left_recursive (s, t)) {
    string s1= s * "-head";
    string s2= s * "-tail";
    tree   t1= left_head (s, t);
    tree   t2= left_tail (s, t);
    packrat_define (s1, t1);
    packrat_define (s2, t2);
    tree   u1= compound ("symbol", s1);
    tree   u2= compound ("repeat", compound ("symbol", s2));
    packrat_define (s, compound ("concat", u1, u2));
  }
  else {
    C sym= encode_symbol (s);
    packrat_grammar[sym - PACKRAT_SYMBOLS]= packrat_define (t);
  }
}

/******************************************************************************
* Packrat parsing
******************************************************************************/

void
packrat_set_input (const array<C>& in) {
  current_input     = in;
  current_cache     = hashmap<D,C> (PACKRAT_UNDEFINED);
  current_production= hashmap<D,tree> (tree_uninit);
}

C
packrat_parse (C sym, C pos) {
  D key= (((D) sym) << 32) + ((D) (sym^pos));
  C im = current_cache [key];
  if (im != PACKRAT_UNDEFINED) {
    //cout << "Cached " << sym << " at " << pos << " -> " << im << LF;
    return im;
  }
  current_cache (key)= PACKRAT_FAILED;
  //cout << "Parse " << sym << " at " << pos << INDENT << LF;
  if (sym >= PACKRAT_SYMBOLS) {
    array<C> inst= packrat_grammar[sym - PACKRAT_SYMBOLS];
    //cout << "Parse " << inst << " at " << pos << LF;
    switch (inst[0]) {
    case PACKRAT_OR:
      im= PACKRAT_FAILED;
      for (int i=1; i<N(inst); i++) {
	im= packrat_parse (inst[i], pos);
	if (im != PACKRAT_FAILED) break;
      }
      break;
    case PACKRAT_CONCAT:
      im= pos;
      for (int i=1; i<N(inst); i++) {
	im= packrat_parse (inst[i], im);
	if (im == PACKRAT_FAILED) break;
      }
      break;
    case PACKRAT_WHILE:
      im= pos;
      while (true) {
	C next= packrat_parse (inst[1], im);
	if (next == PACKRAT_FAILED || (next >= 0 && next <= im)) break;
	im= next;
      }
      break;
    case PACKRAT_REPEAT:
      im= packrat_parse (inst[1], pos);
      if (im != PACKRAT_FAILED)
	while (true) {
	  C next= packrat_parse (inst[1], im);
	  if (next == PACKRAT_FAILED || (next >= 0 && next <= im)) break;
	  im= next;
	}
      break;
    case PACKRAT_RANGE:
      if (pos < N (current_input) &&
	  current_input [pos] >= inst[1] &&
	  current_input [pos] <= inst[2])
	im= pos + 1;
      else im= PACKRAT_FAILED;
      break;
    case PACKRAT_NOT:
      if (packrat_parse (inst[1], pos) == PACKRAT_FAILED) im= pos;
      else im= PACKRAT_FAILED;
    case PACKRAT_UNKNOWN:
      im= PACKRAT_FAILED;
    default:
      im= packrat_parse (inst[0], pos);
      break;
    }
  }
  else {
    if (pos < N (current_input) && current_input[pos] == sym) im= pos + 1;
    else im= PACKRAT_FAILED;
  }
  current_cache (key)= im;
  //cout << UNINDENT << "Parsed " << sym << " at " << pos << " -> " << im << LF;
  //cout << "cache= " << current_cache << LF;
  return im;
}

int
packrat_parse (string s, string in) {
  packrat_set_input (encode_tokens (in));
  //cout << "Input= " << current_input << LF;
  C pos= packrat_parse (encode_symbol (s), 0);
  if (pos == PACKRAT_FAILED) return -1;
  int i=0, k=0;
  while (i<N(in) && k<pos) {
    tm_char_forwards (in, i);
    k++;
  }
  return k;
}
