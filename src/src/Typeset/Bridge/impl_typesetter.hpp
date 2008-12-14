
/******************************************************************************
* MODULE     : impl_typesetter.hpp
* DESCRIPTION: Implementation of the main TeXmacs typesetting routines
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef IMPL_TYPESETTER_H
#define IMPL_TYPESETTER_H
#include "Bridge/bridge.hpp"

class typesetter_rep {
public:
  edit_env&   env;
  bridge      br;
  rectangles  change_log;

  array<page_item> l;      // current lines
  stack_border     sb;     // border properties
  array<line_item> a;      // left surroundings
  array<line_item> b;      // right surroundings

  SI x1, y1, x2, y2;
  hashmap<string,tree> old_patch;
  bool paper;

public:
  typesetter_rep (edit_env& env, tree et, path ip);

  void insert_stack     (array<page_item> l, stack_border sb);
  void insert_parunit   (tree t, path ip);
  void insert_paragraph (tree t, path ip);
  void insert_surround  (array<line_item> a, array<line_item> b);
  void insert_marker    (tree st, path ip);

  void local_start   (array<page_item>& l, stack_border& sb);
  void local_end     (array<page_item>& l, stack_border& sb);

  void determine_page_references (box b);
  box  typeset ();
  box  typeset (SI& x1, SI& y1, SI& x2, SI& y2);
};

#endif // defined IMPL_TYPESETTER_H
