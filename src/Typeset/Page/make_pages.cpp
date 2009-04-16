
/******************************************************************************
* MODULE     : make_pages.cpp
* DESCRIPTION: Control routines for typesetting paragraphs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Format/page_item.hpp"
#include "Format/stack_border.hpp"
#include "pager.hpp"
box format_stack (path ip, array<box> bx, array<space> ht, SI height,
		  bool may_stretch);
#include "Boxes/construct.hpp"
array<page_item> sub (array<page_item> l, path p, path q);
SI stretch_space (space spc, double stretch);
page_item access (array<page_item> l, path p);
skeleton break_pages (array<page_item> l, space ph, int qual,
		      space fn_sep, space fnote_sep, space float_sep, font fn);
box page_box (path ip, box b, tree page, SI width, SI height, SI left, SI top,
	      SI bot, box header, box footer, SI head_sep, SI foot_sep);

box
pager_rep::pages_format (array<page_item> l, SI ht, SI tcor, SI bcor) {
  // cout << "Formatting insertion of height " << ht << LF;
  ht -= (tcor + bcor);
  int i, n= N(l);
  array<box>   bs;
  array<space> spc;
  for (i=0; i<n; i++) {
    page_item item= l[i];
    if (item->type == PAGE_CONTROL_ITEM) {
      if (is_tuple (item->t, "env_page")) {
	if (((item->t[1] == PAGE_THIS_HEADER) ||
	     (item->t[1] == PAGE_THIS_FOOTER)) &&
	    (item->t[2] == "")) style (item->t[1]->label)= " ";
	else if (item->t[1] == PAGE_NR)
	  page_offset= as_int (item->t[2]->label)- N(pages)- 1;
	else style (item->t[1]->label)= copy (item->t[2]);
      }
    }
    else {
      bs  << item->b;
      spc << item->spc;
    }
  }
  ASSERT (N(bs) != 0, "zero lines in insertion");
  box b= format_stack (ip, bs, spc, ht, true);
  return vcorrect_box (b->ip, b, tcor, bcor);
}

box
pager_rep::pages_format (insertion ins) {
  if (is_tuple (ins->type, "multi-column")) {
    // cout << "Format multicolumn " << ins->sk << LF << LF << INDENT;
    int col, nr_cols= N (ins->sk), real_nr_cols= as_int (ins->type[1]);
    SI w= (text_width + col_sep) / real_nr_cols;
    array<box> bs (nr_cols);
    array<SI>  x  (nr_cols);
    array<SI>  y  (nr_cols);
    for (col=0; col<nr_cols; col++) {
      bs[col]= pages_format (ins->sk[col]);
      x [col]= col * w;
      y [col]= 0;
    }
    // cout << UNINDENT << "Formatted multicolumn" << LF;
    return scatter_box (ip, bs, x, y);
  }
  else {
    array<page_item> sub_l= sub (l, ins->begin, ins->end);
    SI ht= stretch_space (ins->ht, ins->stretch);
    return pages_format (sub_l, ht, ins->top_cor, ins->bot_cor);
  }
}

box
pager_rep::pages_format (pagelet pg) {
  // cout << "Formatting pagelet " << (N(pages)+1)
  //      << " stretch " << pg->stretch
  //      << " height " << stretch_space (pg->ht, pg->stretch) << LF << INDENT;
  if (N (pg->ins) == 0) {
    if (N(pages) == 0) return empty_box (ip);
    return empty_box (pages[N(pages)-1]->find_rip());
  }
  else if (N (pg->ins) == 1) {
    insertion ins= pg->ins[0];
    // cout << ins << " stretch " << ins->stretch
    //      << " height " << stretch_space (ins->ht, ins->stretch) << LF;
    //      << UNINDENT << "Formatted pagelet " << (N(pages)+1) << LF << LF;
    return pages_format (ins);
  }
  else {
    int i, n= N(pg->ins);
    array<box> bs (n);
    array<SI>  bx (n);
    array<SI>  by (n);
    SI y= 0, fnote_y= MAX_SI;
    for (i=0; i<n; i++) {
      insertion ins= pg->ins[i];
      // cout << ins << " at " << y << " stretch " << ins->stretch
      //      << " height " << stretch_space (ins->ht, ins->stretch) << LF;
      bs[i]= pages_format (ins);
      bx[i]= 0;
      by[i]= y;
      y -= bs[i]->h();
      if (i < n-1) {
	insertion next= pg->ins[i+1];
	if (ins->type != next->type) {
	  if (is_tuple (next->type, "footnote")) {
	    // cout << "sep " << stretch_space (fnote_sep, ins->stretch) << LF;
	    y -= stretch_space (fnote_sep, ins->stretch);
	    fnote_y= y;
	  }
	  else if (is_tuple (ins->type, "float")) {
	    if (!is_tuple (next->type, "float")) {
	      // cout << "sep " << stretch_space(float_sep, ins->stretch) << LF;
	      y -= stretch_space (float_sep, ins->stretch);
	    }
	  }
	  else if (is_tuple (ins->type, "multi-column") ||
		   is_tuple (next->type, "multi-column")) {
	    page_item item= access (l, path_dec (ins->end));
	    // cout << "sep " << stretch_space (item->spc, ins->stretch) << LF;
	    y -= stretch_space (item->spc, ins->stretch);
	  }
	}
	if (is_tuple (ins->type, "footnote"))
	  if (is_tuple (next->type, "footnote")) {
	    page_item item= access (l, path_dec (ins->end));
	    SI sep= stretch_space (item->spc + fn_sep, ins->stretch);
	    // cout << "sep " << sep << LF;
	    y -= sep;
	  }
	if (is_tuple (next->type, "float")) {
	  // cout << "sep " << stretch_space (float_sep, ins->stretch) << LF;
	  y -= stretch_space (float_sep, ins->stretch);
	}
      }
    }
    if (fnote_y != MAX_SI) {
      bs << line_box (decorate(), 0, 0, fnote_bl, 0, env->fn->wline, env->col);
      bx << 0;
      by << (fnote_y + env->fn->sep);
    }
    // cout << UNINDENT << "Formatted pagelet " << (N(pages)+1) << LF << LF;
    return scatter_box (ip, bs, bx, by);
  }
}

box
pager_rep::pages_make_page (pagelet pg) {
  box sb= pages_format (pg);
  box lb= move_box (ip, sb, 0, 0);
  SI  left= (N(pages)&1)==0? odd: even;
  env->write (PAGE_NR, as_string (N(pages)+1+page_offset));
  env->write (PAGE_THE_PAGE, style[PAGE_THE_PAGE]);
  tree page_t= env->exec (compound (PAGE_THE_PAGE));
  box header= make_header (N (pg->ins) == 0);
  box footer= make_footer (N (pg->ins) == 0);
  return page_box (ip, lb, page_t,
		   width, height, left, top, top+ text_height,
		   header, footer, head_sep, foot_sep);
}

void
pager_rep::pages_make () {
  space ht (text_height- may_shrink, text_height, text_height+ may_extend);
  skeleton sk=
    break_pages (l, ht, quality, fn_sep, fnote_sep, float_sep, env->fn);
  int i, n= N(sk);
  for (i=0; i<n; i++)
    pages << pages_make_page (sk[i]);
}

void
pager_rep::papyrus_make () {
  space ht (MAX_SI >> 1);
  skeleton sk=
    break_pages (l, ht, quality, fn_sep, fnote_sep, float_sep, env->fn);
  if (N(sk) != 1) {
    cerr << "Number of pages: " << N(sk) << "\n";
    FAILED ("unexpected situation");
  }

  box sb= pages_format (sk[0]);
  box b = move_box (ip, sb, 0, 0);
  SI left  = (odd+even) >> 1;
  SI height= top + bot + b->h();
  array<box> bs   (1); bs   [0]= b;
  array<SI>  bs_x (1); bs_x [0]= left;
  array<SI>  bs_y (1); bs_y [0]= -top;
  box pb= page_box (ip, "?", width, height,
		    bs, bs_x, bs_y, 0, 0, 0);
  pages << pb;
}
