
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
* Breaking pages with multicolumn text
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
new_breaker_rep::break_multicol_ansatz (path b0, path b1, path b2, SI h) {
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
    if (spc->def > h) return break_multicol_ansatz (b0, b1, bm, h);
    else return break_multicol_ansatz (b0, bm, b1, h);
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
new_breaker_rep::break_multicol_at (path b1, path b2, SI h) {
  if (b1 == b2) return b1;
  path bm= break_multicol_ansatz (b1, b1, b2, h);
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
new_breaker_rep::break_multicol (path b1, path b2) {
  int i1= b1->item, i2= b2->item;
  if (col_same[i2] >= i1)
    if (col_same[i2] > i1 || !is_nil (b1->next)) {
      path bm= path (col_same[i2]);
      array<path> r= break_multicol (b1, bm);
      r->resize (N(r) - 1);
      r << break_multicol (bm, b2);
      return r;
    }
  int cols= col_number[i2];
  space tot= compute_space (b1, b2);
  array<path> r;
  r << b1;
  for (int k=1; k<cols; k++)
    r << break_multicol_at (b1, b2, (k * tot->def) / cols);
  r << b2;
  return r;
}
