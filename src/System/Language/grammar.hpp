
/******************************************************************************
* MODULE     : grammar.hpp
* DESCRIPTION: packrat parsing
* COPYRIGHT  : (C) 2009  Francis Jamet, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef GRAMMAR_H
#define GRAMMAR_H

#include "ntuple.hpp"
#include "hashmap.hpp"
#include "analyze.hpp"
#include "impl_language.hpp"

class parser_rep: concrete_struct {
public:
  hashmap<tree,tree> grammar;
  string xstring;
  hashmap<triple<string,int,int>,int> evaluated_triple;
  hashmap<pair<tree, int>, int> evaluated_pair;
  hashmap<string,bool> can_be_empty_table;
  hashmap<pair<string,string>,bool> dependance;
  hashmap<pair<string,string>,bool> closure;
  hashmap<pair<string,string>,bool> dag;

  parser_rep (hashmap<tree,tree> g, string s);
  hashmap<pair<string,string>,bool> set_closure(hashmap<pair<string,string>,bool> r);
  void set_dependance();
  void set_dependance(string var, tree rule);
  void set_emptyness();
  bool can_be_empty(tree rule);
  void set_dag();
  int parse_level(bool not_decomposition, string calling_letter, int level, tree parsing_tree, int pos);
  int parse (tree parsing_tree, int pos);
  friend class parser;
};

class parser {
CONCRETE(parser);
  parser (hashmap<tree,tree> g, string s);
};
CONCRETE_CODE(parser);

void define_grammar_rule (tree var, tree gram);
int grammar_parse (tree var, string s);

#endif // GRAMMAR_H
