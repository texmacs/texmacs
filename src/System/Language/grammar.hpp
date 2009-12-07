
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
  hashmap<pair<tree,int>,int> evaluated_pair;
  hashmap<pair<tree,int>,bool> wanted_pair;

  parser_rep (hashmap<tree,tree> g, string s);
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
