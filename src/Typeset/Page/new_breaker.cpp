
/******************************************************************************
* MODULE     : new_breaker.cpp
* DESCRIPTION: Page breaking
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Line/lazy_vstream.hpp"
#include "vpenalty.hpp"
#include "skeleton.hpp"

vpenalty as_vpenalty (SI diff);

#define INVALID_BREAK  0
#define BAD_BREAK      1
#define VALID_BREAK    2

/******************************************************************************
* The new_breaker class
******************************************************************************/

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
  
  array<space> body_ht;    // the heights of these page_items
  array<space> body_cor;   // top and bottom corrections of page_items
  array<space> body_tot;   // total heights up to a certain index
  array<space> foot_ht;    // the height of all footnotes for one page_ite,
  array<space> foot_tot;   // the cumulated footnote height until here

  array<array<insertion> > ins_list;   // all page insertions

  hashmap<path,path>       best_prev;  // best previous break points
  hashmap<path,vpenalty>   best_pens;  // corresponding penalties

  new_breaker_rep (array<page_item> l, space ph, int quality,
                   space fn_sep, space fnote_sep, space float_sep,
                   font fn, int fp);

  insertion make_insertion (lazy_vstream lvs, path p);
  space compute_space (path b1, path b2);
  void find_page_breaks (path i1);
  void find_page_breaks ();
  vpenalty format_insertion (insertion& ins, double stretch);
  vpenalty format_pagelet (pagelet& pg, double stretch);
  vpenalty format_pagelet (pagelet& pg, space ht, bool last_page);
  insertion make_insertion (int i1, int i2, bool last_page);
  void assemble_skeleton (skeleton& sk, int end);
};

/******************************************************************************
* Constructor
******************************************************************************/

new_breaker_rep::new_breaker_rep (
  array<page_item> l2, space ph, int quality2,
  space fn_sep2, space fnote_sep2, space float_sep2,
  font fn2, int fp2):
    l (l2), papyrus_mode (ph == (MAX_SI >> 1)), height (ph),
    fn_sep (fn_sep2), fnote_sep (fnote_sep2), float_sep (float_sep2),
    fn (fn2), first_page (fp2), quality (quality2), last_page_flag (true),
    body_ht (), body_cor (), foot_ht (), foot_tot (), ins_list (),
    best_prev (path (-1)), best_pens (MAX_SI)
{
  for (int i=0; i<N(l); i++) {
    SI   bot_cor= max (0, l[i]->b->y1- fn->y1);
    SI   bod_cor= l[i]->b->h ();
    SI   top_cor= max (0, fn->y2- l[i]->b->y2);
    body_ht  << (space (l[i]->b->h()) + l[i]->spc);
    body_cor << space (bot_cor, bod_cor, top_cor);
    body_tot << (i==0? space(0): body_tot[i-1]) + body_ht[i];
    if ((i==N(l)-1) || (l[i]->nr_cols!=l[i+1]->nr_cols)) l[i]->penalty=0;

    int k= N (l[i]->fl);
    space foot_spc (0);
    array<insertion> ins_here;
    for (int j=0; j<k; j++) {
      lazy_vstream lvs= (lazy_vstream) l[i]->fl[j];
      insertion ins= make_insertion (lvs, path (i, j));
      ins_here << ins;
      if (is_tuple (lvs->channel, "footnote"))
        foot_spc += ins->ht + fn_sep;
      else if (is_tuple (lvs->channel, "float")) {
      }
    }
    ins_list << ins_here;
    foot_ht  << foot_spc;
    foot_tot << (i==0? space(0): foot_tot[i-1] + foot_ht[i]);
  }

  best_prev (path (0))= path (-2); 
  best_pens (path (0))= 0;
}

/******************************************************************************
* Trivial layout of footnote and floating page insertions
******************************************************************************/

insertion
new_breaker_rep::make_insertion (lazy_vstream lvs, path p) {
  // FIXME: Very long insertions should themselves be page-broken
  
  path p1= p * 0;
  path p2= p * N(lvs->l);
  insertion ins (lvs->channel, p1, p2);
  
  array<page_item> l= lvs->l;
  array<space> ins_ht;
  array<space> ins_cor;
  array<space> ins_tot;
  for (int i=0; i<N(l); i++) {
    SI   bot_cor= max (0, l[i]->b->y1- fn->y1);
    SI   bod_cor= l[i]->b->h ();
    SI   top_cor= max (0, fn->y2- l[i]->b->y2);
    ins_ht  << (space (l[i]->b->h()) + l[i]->spc);
    ins_cor << space (bot_cor, bod_cor, top_cor);
    ins_tot << (i==0? space(0): ins_tot[i-1]) + ins_ht[i];
  }
  
  space spc;
  if (N(l) > 1) spc= copy (ins_tot[N(l)-2]);
  SI top_cor= ins_cor[0]->max;
  SI bot_cor= ins_cor[N(l)-1]->min;
  spc += space (top_cor + ins_cor[N(l)-1]->def + bot_cor);
  ins->ht     = spc;
  ins->top_cor= top_cor;
  ins->bot_cor= bot_cor;
  ins->pen    = 0;
  return ins;
}

/******************************************************************************
* Determination of the min and max spaces master routine
******************************************************************************/

space
new_breaker_rep::compute_space (path b1, path b2) {
  //cout << "    Compute space " << b1 << ", " << b2 << LF;
  int i1= b1[0], i2= b2[0];
  space spc;
  if (i1 == 0) { if (i2 > 1) spc= copy (body_tot[i2-2]); }
  else spc= body_tot[i2-2] - body_tot[i1-1];
  SI top_cor= body_cor[i1]->max;
  SI bot_cor= body_cor[i2-1]->min;
  spc += space (top_cor + body_cor[i2-1]->def + bot_cor);

  if (foot_tot[i2-1]->max > (i1==0? 0: foot_tot[i1-1]->min)) {
    space foot_spc= foot_tot[i2-1] - (i1==0? space(0): foot_tot[i1-1]);
    foot_spc += fnote_sep - fn_sep;
    spc += foot_spc;
  }

  //cout << "    Computed space " << i1 << ", " << i2 << " ~> " << spc << LF;
  return spc;
}

/******************************************************************************
* Find page breaks for a given start
******************************************************************************/

void
new_breaker_rep::find_page_breaks (path b1) {
  //cout << "Find page breaks " << b1 << LF;
  bool ok= false;
  vpenalty prev_pen= best_pens [b1];
  path b2= path (b1->item + 1);
  int n= N(l);
  while (true) {
    space spc;
    int bpen= l[b2->item - 1]->penalty;
    if (b2->item >= n) bpen= 0;
    if (bpen < HYPH_INVALID) {
      spc= compute_space (b1, b2);
      ok= true;
      vpenalty pen= prev_pen + vpenalty (bpen);
      if ((b2->item < n) || (!last_page_flag))
	pen += as_vpenalty (spc->def - height->def);
      if (((b2->item < n) || (!last_page_flag)) && (spc->max < height->def)) {
	if (spc->max >= height->min) pen += EXTEND_PAGE_PENALTY;
	else {
	  double factor=
	    ((double) max (spc->def, 1))/((double) max (height->def, 1));
	  if (factor < 0.0 ) factor= 0.0;
	  if (factor > 0.99) factor= 0.99;
	  pen= vpenalty ((int) ((1.0 - factor) * TOO_SHORT_PENALTY));
	}
      }
      else if (spc->min > height->def) {
	if (spc->min <= height->max) pen += REDUCE_PAGE_PENALTY;
	else {
	  double factor=
	    ((double) max (spc->def, 1))/((double) max (height->def, 1));
	  if (factor < 1.0  ) factor= 1.0;
	  if (factor > 100.0) factor= 100.0;
	  pen= vpenalty ((int) (factor * TOO_LONG_PENALTY));
	}
      }
      if (pen < best_pens [b2]) {
        //cout << b1 << ", " << b2 << " ~> " << pen << "\n";
	best_prev (b2)= b1;
	best_pens (b2)= pen;
      }
    }
    if ((b2->item >= n) || (ok && (spc->min > height->max))) break;
    b2= path (b2->item + 1);
  }
}

/******************************************************************************
* Master routine for finding all page breaks
******************************************************************************/

void
new_breaker_rep::find_page_breaks () {
  //cout << "Find page breaks" << LF;
  //for (int i=0; i<N(l); i++)
  //  cout << "  " << i << ": \t" << l[i]
  //       << ", " << body_ht[i]
  //       << ", " << body_cor[i] << ", " << body_tot[i] << LF;
  for (int i=0; i<N(l); i++)
    if (best_prev [path (i)] != path (-1))
      find_page_breaks (path (i));
  //cout << "Found page breaks" << LF;
}

/******************************************************************************
* Formatting pagelets
******************************************************************************/

vpenalty
new_breaker_rep::format_insertion (insertion& ins, double stretch) {
  // cout << "Stretch " << ins << ": " << stretch << LF;
  ins->stretch= stretch;
  skeleton sk = ins->sk;
  if (N(sk) == 0) return vpenalty ();

  int i, k=N(sk);
  vpenalty pen;
  SI ht= stretch_space (ins->ht, stretch);
  // cout << "Formatting multicolumn " << ins->ht
  //      << " stretch " << stretch
  //      << " -> height " << ht << LF << INDENT;
  for (i=0; i<k; i++) {
    pagelet& pg= sk[i];
    // cout << i << ": " << pg->ht;
    double pg_stretch= 0.0;
    if (ht > pg->ht->max) pg_stretch= 1.0;
    else if (ht < pg->ht->min) pg_stretch= -1.0;
    else if ((ht > pg->ht->def) && (pg->ht->max > pg->ht->def))
      pg_stretch=
	((double) (ht - pg->ht->def)) /
	((double) (pg->ht->max - pg->ht->def));
    else if ((ht < pg->ht->def) && (pg->ht->def > pg->ht->min))
      pg_stretch=
	((double) (ht - pg->ht->def)) /
	((double) (pg->ht->def - pg->ht->min));
    // cout << " -> " << pg_stretch << LF;
    pen += format_pagelet (pg, pg_stretch);
    pen += pg->pen + as_vpenalty (pg->ht->def - ht);
  }
  // cout << UNINDENT << "Formatted multicolumn, penalty= " << pen << LF;
  return pen;
}

vpenalty
new_breaker_rep::format_pagelet (pagelet& pg, double stretch) {
  // cout << "Stretch " << pg << ": " << stretch << LF;
  int i;
  vpenalty pen;
  pg->stretch= stretch;
  for (i=0; i<N(pg->ins); i++)
    pen += format_insertion (pg->ins[i], stretch);
  return pen;
}

vpenalty
new_breaker_rep::format_pagelet (pagelet& pg, space ht, bool last_page) {
  // cout << "Formatting " << pg << ", " << ht << LF << INDENT;
  float stretch= 0.0;
  vpenalty pen;

  if (last_page && (pg->ht->def <= ht->def)) {
    // cout << "Eject last page" << LF;
    stretch= 0.0;
  }
  else if ((ht->def >= pg->ht->min) && (ht->def <= pg->ht->max)) {
    if (ht->def > pg->ht->def) {
      // cout << "Stretch" << LF;
      stretch=
	((double) (ht->def - pg->ht->def)) /
	((double) (pg->ht->max - pg->ht->def));
    }
    else if (ht->def < pg->ht->def) {
      // cout << "Shrink" << LF;
      stretch=
	((double) (ht->def - pg->ht->def)) /
	((double) (pg->ht->def - pg->ht->min));
    }
    pen= as_vpenalty (ht->def- pg->ht->def);
  }
  else if ((ht->def < pg->ht->min) && (ht->max >= pg->ht->min)) {
    // cout << "Extend page" << LF;
    stretch= -1.0;
    pen= vpenalty (EXTEND_PAGE_PENALTY) + as_vpenalty (ht->def- pg->ht->def);
  }
  else if ((ht->def > pg->ht->max) && (ht->min <= pg->ht->max)) {
    // cout << "Reduce page" << LF;
    stretch= 1.0;
    pen= vpenalty (REDUCE_PAGE_PENALTY) + as_vpenalty (ht->def- pg->ht->def);
  }
  else if (ht->max < pg->ht->min) {
    // cout << "Overfull page" << LF;
    stretch= -1.0;
    double factor= ((double) max (pg->ht->def, 1))/((double) max (ht->def, 1));
    if (factor < 1.0  ) factor= 1.0;
    if (factor > 100.0) factor= 100.0;
    pen= vpenalty ((int) (factor * TOO_LONG_PENALTY));
  }
  else {
    // cout << "Underfull page" << LF;
    stretch= 1.0;
    double factor= ((double) max (pg->ht->def, 1))/((double) max (ht->def, 1));
    if (factor < 0.0 ) factor= 0.0;
    if (factor > 0.99) factor= 0.99;
    pen= vpenalty ((int) ((1.0 - factor) * TOO_SHORT_PENALTY));
  }
  pen += format_pagelet (pg, stretch);
  // cout << UNINDENT << "Formatted [ stretch= " << stretch
  //      << ", penalty= " << (pg->pen + pen) << " ]" << LF << LF;
  return pg->pen + pen;
}

/******************************************************************************
* Assembling the skeleton
******************************************************************************/

insertion
new_breaker_rep::make_insertion (int i1, int i2, bool last_page) {
  //cout << "Make insertion " << i1 << ", " << i2 << ", " << last_page << LF;
  path p1= i1;
  path p2= i2;
  insertion ins ("", p1, p2);  
  space spc;
  if (i1 == 0) { if (N(l) > 1) spc= copy (body_tot[i2-2]); }
  else { spc= body_tot[i2-2] - body_tot[i1-1]; }
  SI top_cor= body_cor[i1]->max;
  SI bot_cor= body_cor[i2-1]->min;
  spc += space (top_cor + body_cor[i2-1]->def + bot_cor);
  ins->ht     = spc;
  ins->top_cor= top_cor;
  ins->bot_cor= bot_cor;
  ins->pen    = best_pens [path (i2)];
  return ins;
}

void
new_breaker_rep::assemble_skeleton (skeleton& sk, int end) {
  int start= best_prev [path (end)] -> item, n= N(l);
  //cout << "Assemble skeleton " << end << " previous " << start << LF;
  if (start < 0) return;
  assemble_skeleton (sk, start);
  insertion ins= make_insertion (start, end, end == n);
  pagelet pg (0);
  pg << ins;
  for (int i=start; i<end; i++)
    for (int j=0; j<N(ins_list[i]); j++)
      if (is_tuple (ins_list[i][j]->type, "footnote"))
        pg << ins_list[i][j];
  bool last_page= last_page_flag && (end == n);
  format_pagelet (pg, height, last_page);
  sk << pg;
}

/******************************************************************************
* The exported page breaking routine
******************************************************************************/

skeleton
new_break_pages (array<page_item> l, space ph, int qual,
                 space fn_sep, space fnote_sep, space float_sep,
                 font fn, int first_page)
{
  new_breaker_rep* H=
    tm_new<new_breaker_rep> (l, ph, qual, fn_sep, fnote_sep, float_sep,
                             fn, first_page);
  //cout << HRULE << LF;
  H->find_page_breaks ();
  //cout << HRULE << LF;
  skeleton sk;
  H->assemble_skeleton (sk, N(l));
  //cout << HRULE << LF;
  tm_delete (H);
  return sk;
}
