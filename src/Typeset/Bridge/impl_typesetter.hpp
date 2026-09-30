
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
  edit_env&    env;
  bridge       br;
  rectangles   change_log;
  array<brush> old_bgs;

  array<page_item> l;      // current lines
  stack_border     sb;     // border properties
  array<line_item> a;      // left surroundings
  array<line_item> b;      // right surroundings

  SI x1, y1, x2, y2;
  hashmap<string,tree> old_patch;
  bool paper;

  // Moving the pixels of unchanged lines outside paper mode (set by the
  // editor before typesetting; see find_shift)
  SI   snap_pixel;          // screen pixel to which line distances are rounded
  bool shift_allowed;       // may the editor move pixels on the screen?
  array<box> body_lines;    // lines of the body at the last pass
  array<SI>  body_ys;       // and the absolute ordinates of their origins
  array<rectangle> body_others; // vertical extents of the rest of the page
  SI   shift_y1, shift_y2;  // after typesetting: band of unchanged lines
  SI   shift_dy;            // moved by shift_dy (0: nothing to move)
  rectangles shift_rects;   // then the areas to repaint (not their union)
  box  last_body;           // body stack made by the pager at this pass

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
  void find_shift (box b, box body, bool plain);
  box  typeset ();
  box  typeset (SI& x1, SI& y1, SI& x2, SI& y2);
};

#endif // defined IMPL_TYPESETTER_H
