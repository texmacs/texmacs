
/******************************************************************************
* MODULE     : packrat_parser.hpp
* DESCRIPTION: packrat parsers
* COPYRIGHT  : (C) 2010  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PACKRAT_PARSER_H
#define PACKRAT_PARSER_H
#include "packrat_grammar.hpp"

#define PACKRAT_UNDEFINED ((C) (-2))
#define PACKRAT_FAILED    ((C) (-1))

class packrat_parser_rep: concrete_struct {
public:
  hashmap<C,array<C> >      grammar;
  hashmap<C,tree>           productions;
  hashmap<D,string>         properties;

  tree                      current_tree;
  string                    current_string;
  hashmap<path,int>         current_start;
  hashmap<path,int>         current_end;
  C                         current_cursor;
  int                       current_hl_lan;

  array<C>                  current_input;
  hashmap<D,C>              current_cache;
  hashmap<D,tree>           current_production;

protected:
  void serialize (tree t, path p);
  void set_input (tree t);
  void set_cursor (path t_pos);
  path decode_path (tree t, path p, int pos);
  int  encode_path (tree t, path p, path pos);

public:
  packrat_parser_rep (packrat_grammar gr);

  int  decode_string_position (C pos);
  C    encode_string_position (int i);
  path decode_tree_position (C pos);
  C    encode_tree_position (path p);
  C    parse (C sym, C pos);

  void inspect (C sym, C pos, array<C>& syms, array<C>& poss);
  bool is_left_recursive (C sym);
  bool is_associative (C sym);
  bool is_anti_associative (C sym);
  bool is_list_like (C sym);
  bool is_selectable (C sym);
  void context (C sym, C pos, C left, C right, int mode,
		array<C>& kind, array<C>& begin, array<C>& end);
  void compress (array<C>& kind, array<C>& begin, array<C>& end);
  void highlight (tree t, path tp, path p1, path p2, int col);
  void highlight (C sym, C pos);

  friend class packrat_parser;
};

class packrat_parser {
  CONCRETE_NULL (packrat_parser);
  inline packrat_parser (packrat_grammar gr, tree t, path t_pos= path ());
};
CONCRETE_NULL_CODE (packrat_parser);

inline packrat_parser::packrat_parser
  (packrat_grammar gr, tree t, path t_pos):
    rep (tm_new<packrat_parser_rep> (gr)) {
      rep->set_input (t);
      rep->set_cursor (t_pos); }

#endif // PACKRAT_PARSER_H
