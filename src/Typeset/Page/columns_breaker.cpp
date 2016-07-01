
/******************************************************************************
* MODULE     : columns_breaker.cpp
* DESCRIPTION: Breaking of multi-column content
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "new_breaker.hpp"

/******************************************************************************
* Checking for multi-column content
******************************************************************************/

bool
new_breaker_rep::has_columns (path b1, path b2, int nr) {
  // FIXME: also check multi-column floats and footnotes
  int i1= b1->item, i2= b2->item;
  if (!is_nil (b1->next)) {
    path floats= b1->next;
    if (i1 == i2) floats= head (floats, N(b1) - N(b2));
    while (!is_nil (floats)) {
      int i= floats->item, j= floats->next->item;
      floats= floats->next->next;
      if (ins_list[i][j]->nr_cols != nr) return false;
    }
  }
  if (i1 >= i2) return true;
  if (col_same[i2] > i1) return false;
  return col_number[i1] == nr;
}

int
new_breaker_rep::number_columns (path b1, path b2) {
  int i1= b1->item, i2= b2->item;
  if (i1 < i2) return col_number[i2-1];
  if (is_nil (b1->next) || b1 == b2) return 1;
  int i= b1->next->item, j= b1->next->next->item;
  return ins_list[i][j]->nr_cols;
}

/******************************************************************************
* Breaking into portions with a uniform number of columns
******************************************************************************/

bool
new_breaker_rep::is_uniform (path b1, path b2) {
  int nr= number_columns (b1, b2);
  return has_columns (b1, b2, nr);
}

array<path>
new_breaker_rep::break_uniform (path b1, path b2) {
  if (is_uniform (b1, b2)) {
    array<path> r;
    r << b1 << b2;
    return r;
  }
  else {
    path key= b1 * path (-1, b2);
    array<path> r= cache_uniform[key];
    if (N(r) != 0) return r;
    int i1= b1->item, i2= b2->item;
    path bm;
    if (i1 < i2 && i1 < col_same[i2])
      bm= postpone_floats (path (col_same[i2]), b2);
    else {
      bm= b1;
      while (!is_nil (bm->next) &&
             is_uniform (b1, path (bm->item, bm->next->next->next)))
        bm= path (bm->item, bm->next->next->next);
    }
    r= break_uniform (b1, bm);
    r->resize (N(r) - 1);
    r << break_uniform (bm, b2);
    cache_uniform (key)= r;
    return r;
  }
}

/******************************************************************************
* Finding column breaks on a given page
******************************************************************************/

static inline int iabs (int i) { return i>=0? i: -i; }

int
new_breaker_rep::compute_penalty (path b) {
  if (!is_nil (b->next) || b->item == 0) return 0;
  return l[b->item - 1]->penalty;
}

path
new_breaker_rep::postpone_floats (path b1, path b2) {
  path floats= b2->next;
  if (!is_nil (b1->next)) return b1;
  while (!is_nil (floats)) {
    int i= floats->item, j= floats->next->item;
    floats= floats->next->next;
    if (i < b1->item) b1= b1 * path (i, j);
  }
  return b1;
}

path
new_breaker_rep::break_columns_ansatz (path b0, path b1, path b2, SI h) {
  if (b1 == b2) return b1;
  path bm;
  if (!is_nil (b1->next))
    bm= path (b1->item, b1->next->next->next);
  else if (!is_nil (b2->next))
    bm= path (b2->item, b2->next->next->next);
  else {
    bm= path ((b1->item + b2->item) >> 1);
    bm= postpone_floats (bm, b2);
  }
  if (bm == b1 || bm == b2) {
    space spc1= compute_space (b0, b1);
    space spc2= compute_space (b0, b2);
    if (iabs (spc1->def - h) < iabs (spc2->def - h)) return b1;
    else return b2;
  }
  else {
    space spc= compute_space (b0, bm);
    if (spc->def > h) return break_columns_ansatz (b0, b1, bm, h);
    else return break_columns_ansatz (b0, bm, b1, h);
  }
}

path
new_breaker_rep::search_leftwards (path b1, path b2, path b, SI h) {
  int pen= compute_penalty (b);
  if (pen == 0 || b == b1) return b;
  space spc= compute_space (b1, b);
  if (pen < HYPH_INVALID && (h < spc->min || h > spc->max)) return b;
  path next;
  if (b1->item == b->item) {
    int i= (N(b1->item) - N(b->item) + 1) - 2;
    next= path (b1->item, tail (b1, i));
  }
  else {
    next= path (b->item - 1);
    next= postpone_floats (next, b);
  }
  path bx= search_leftwards (b1, b2, next, h);
  int penx= compute_penalty (bx);
  if (pen < penx) return b;
  space spcx= compute_space (b1, bx);
  if (h < spcx->min || h > spcx->max) return b;
  if (penx < pen) return bx;
  return b;
}

path
new_breaker_rep::search_rightwards (path b1, path b2, path b, SI h) {
  int pen= compute_penalty (b);
  if (pen == 0 || b == b2) return b;
  space spc= compute_space (b1, b);
  if (pen < HYPH_INVALID && (h < spc->min || h > spc->max)) return b;
  path next;
  if (b->item == b2->item) {
    int i= (N(b->item) - N(b2->item) + 1) - 2;
    next= path (b->item, tail (b, i));
  }
  else {
    next= path (b->item + 1);
    next= postpone_floats (next, b2);
  }
  path bx= search_rightwards (b1, b2, next, h);
  int penx= compute_penalty (bx);
  if (pen < penx) return b;
  space spcx= compute_space (b1, bx);
  if (h < spcx->min || h > spcx->max) return b;
  if (penx < pen) return bx;
  return b;
}

path
new_breaker_rep::break_columns_at (path b1, path b2, SI h) {
  if (b1 == b2) return b1;
  path bm= break_columns_ansatz (b1, b1, b2, h);
  path bl= search_leftwards (b1, b2, bm, h);
  path br= search_rightwards (b1, b2, bm, h);
  int penl= compute_penalty (bl);
  int penr= compute_penalty (br);
  if (penl == HYPH_INVALID) return br;
  if (penr == HYPH_INVALID) return bl;
  space spcl= compute_space (b1, bl);
  space spcr= compute_space (b1, br);
  bool okl= h >= spcl->min || h <= spcl->max;
  bool okr= h >= spcr->min || h <= spcr->max;
  if (!okl &&  okr) return br;
  if ( okl && !okr) return bl;
  if (penr <= penl) return br;
  return bl;
}

array<path>
new_breaker_rep::break_columns (path b1, path b2) {
  int cols= number_columns (b1, b2);
  ASSERT (has_columns (b1, b2, cols), "non uniform number of columns");
  if (cols == 1) {
    array<path> r;
    r << b1 << b2;
    return r;
  }
  else {
    path key= b1 * path (-1, b2);
    array<path> r= cache_colbreaks[key];
    if (N(r) != 0) return r;
    space tot= compute_space (b1, b2);
    r << b1;
    for (int k=1; k<cols; k++)
      r << break_columns_at (b1, b2, (k * tot->def) / cols);
    r << b2;
    cache_colbreaks (key)= r;
    return r;
  }
}

/******************************************************************************
* Computing the space and penalty for column breaks
******************************************************************************/

space
new_breaker_rep::compute_space (path b1, path b2, vpenalty& pen) {
  space spc= 0;
  pen= 0;
  array<path> a= break_uniform (b1, b2);
  for (int i=1; i<N(a); i++) {
    path sb1= a[i-1], sb2= a[i];
    array<path> sa= break_columns (b1, b2);
    int cols= N(sa) - 1;
    if (cols == 1)
      spc += compute_space (sb1, sb2);
    else {
      space tot= compute_space (sb1, sb2);
      SI min_inc= 0, def_sum= 0, max_inc= MAX_SI;
      array<SI> def_spcs;
      for (int si=1; si<=cols; si++) {
        path ssb1= sa[si-1], ssb2= sa[si];
        space col_spc= compute_space (ssb1, ssb2);
        min_inc= max (min_inc, col_spc->min);
        def_sum += col_spc->def;
        max_inc= min (max_inc, col_spc->max);
        pen += (si == cols? 0: l[ssb2->item - 1]->penalty);
        def_spcs << col_spc->def;
      }
      if (max_inc < min_inc) {
        max_inc= min_inc;
        pen += vpenalty (UNBALANCED_COLUMNS);
      }
      SI def_inc= def_sum / cols;
      def_inc= max (def_inc, min_inc);
      def_inc= min (def_inc, max_inc);
      spc += space (min_inc, def_inc, max_inc);
      for (int j=0; j<N(def_spcs); j++)
	pen += as_vpenalty (def_spcs[j] - def_inc);
    }
  }
  return spc;
}
