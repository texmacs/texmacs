
/******************************************************************************
* MODULE     : new_breaker.hpp
* DESCRIPTION: Page breaking
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef NEW_BREAKER_H
#define NEW_BREAKER_H
#include "Line/lazy_vstream.hpp"
#include "vpenalty.hpp"
#include "skeleton.hpp"
#include "iterator.hpp"

struct new_breaker_rep {
  array<page_item> l;
  int   papyrus_mode;
  space height;
  space fn_sep;            // inter-footnote separation
  space fnote_sep;         // separation between footnote and main text
  space float_sep;         // separation between text or floats and floats
  font  fn;
  int   first_page;
  int   quality;

  bool last_page_flag; // FIXME
  
  array<space> body_ht;     // the heights of these page_items
  array<space> body_cor;    // top and bottom corrections of page_items
  array<space> body_tot;    // total heights up to a certain index
  array<space> foot_ht;     // the height of all footnotes for one page_item,
  array<space> foot_tot;    // the cumulated footnote height until here
  array<space> float_ht;    // the height of all floats for one page_item,
  array<space> float_tot;   // the cumulated float height until here
  array<space> wide_ht;     // height of wide floats/footnotes for page_item,
  array<space> wide_tot;    // the cumulated wide float/footnote height
  array<space> break_ht;    // extra height in case of page break
  array<int>   col_number;  // number of columns
  array<int>   col_same;    // last line with same number of columns
  array<bool>  must_new;    // compulsory new pages
  array<bool>  must_break;  // compulsory new pages or page breaks
  
  array<array<insertion> > ins_list;   // all page insertions

  hashmap<path,path>       best_prev;  // best previous break points
  hashmap<path,vpenalty>   best_pens;  // corresponding penalties
  hashmap<path, bool>      todo_list;
  hashmap<path, bool>      done_list;
 
  hashmap<path,array<path> > cache_uniform;
  hashmap<path,array<path> > cache_colbreaks;
 
  new_breaker_rep (array<page_item> l, space ph, int quality,
                   space fn_sep, space fnote_sep, space float_sep,
                   font fn, int fp);

  insertion make_insertion (lazy_vstream lvs, path p);
  space compute_space (path b1, path b2);
  bool last_break (path b);
  void find_page_breaks (path i1);
  void find_page_breaks ();
  vpenalty format_insertion (insertion& ins, double stretch);
  vpenalty format_pagelet (pagelet& pg, double stretch);
  vpenalty format_pagelet (pagelet& pg, space ht, bool last_page);
  insertion make_insertion (int i1, int i2);
  bool here_floats (path p);
  void lengthen_previous (pagelet& pg, int pos, space done);
  pagelet assemble (path start, path end);
  void assemble_skeleton (skeleton& sk, path end, int& offset);

  // Pages with multi-column content
  bool has_columns (path b1, path b2, int nr);
  int number_columns (path b1, path b2);
  bool is_uniform (path b1, path b2);
  array<path> break_uniform (path b1, path b2);
  int compute_penalty (path b);
  path postpone_floats (path b1, path b2);
  path break_columns_ansatz (path b1, path b2, path ba, path bb, double fr);
  path search_leftwards (path b1, path b2, path b, double fr);
  path search_rightwards (path b1, path b2, path b, double fr);
  path break_columns_at (path b1, path b2, double fr);
  array<path> break_columns (path b1, path b2);
  void compute_space (array<space> spcs, array<vpenalty> pens,
                      space& spc, vpenalty& pen);
  space compute_space (path b1, path b2, vpenalty& pen);
  insertion make_multi_column (skeleton sk, int real_nr_cols);
  insertion make_multi_column (path b1, path b2);
  pagelet assemble_multi_columns (path b1, path b2);
};

vpenalty as_vpenalty (SI diff);
bool float_has (tree t, char c);
bool float_here (tree t);

#endif // NEW_BREAKER_H
