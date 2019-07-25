
/******************************************************************************
* MODULE     : new_breaker.cpp
* DESCRIPTION: Page breaking
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "new_breaker.hpp"

/******************************************************************************
* Float placement subroutines
******************************************************************************/

bool
float_has (tree t, char c) {
  if (N(t) < 2 || t[0] != "float") return false;
  string s= as_string (t[1]);
  for (int i=0; i<N(s); i++)
    if (s[i] == c) return true;
  return false;
}

bool
float_here (tree t) {
  return !float_has (t, 't') && !float_has (t, 'b');
}

space
as_space (tree t) {
  if (is_func (t, TMLEN, 1))
    return space ((SI) (as_double (t[0]->label)));
  else if (is_func (t, TMLEN, 3)) {
    SI _min= (SI) as_double (t[0]->label);
    SI _def= (SI) as_double (t[1]->label);
    SI _max= (SI) as_double (t[2]->label);
    return space (_min, _def, _max);
  }
  else return space (0);
}

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
    body_ht (), body_cor (), foot_ht (), foot_tot (),
    float_ht (), float_tot (), ins_list (),
    best_prev (path (-1)), best_pens (MAX_SI),
    todo_list (false), done_list (false),
    cache_uniform (array<path> ()),
    cache_colbreaks (array<path> ())
{
  // HACK: migrate double column footnotes in single column text
  for (int i=0; i+1<N(l); i++)
    if (l[i]->nr_cols == 1 && N(l[i]->fl)>0) {
      array<lazy> ofl, nfl;
      for (int j=0; j<N(l[i]->fl); j++) {
        lazy_vstream lvs= (lazy_vstream) l[i]->fl[j];
        if (is_tuple (lvs->channel, "footnote") &&
            N(lvs->l) > 0 && lvs->l[0]->nr_cols == 2)
          nfl << l[i]->fl[j];
        else ofl << l[i]->fl[j];
      }
      if (N(nfl) != 0)
        for (int j=i+1; j<N(l); j++)
          if (l[j]->nr_cols == 2) {
            nfl << l[j]->fl;
            l[i]= copy (l[i]);
            l[j]= copy (l[j]);
            l[i]->fl= ofl;
            l[j]->fl= nfl;
            break;
          }
    }
  // END HACK

  int same= 0;
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
    space float_spc (0);
    space wide_spc (0);
    space break_spc (0);
    array<insertion> ins_here;
    for (int j=0; j<k; j++) {
      lazy_vstream lvs= (lazy_vstream) l[i]->fl[j];
      insertion ins= make_insertion (lvs, path (i, j));
      ins_here << ins;
      //cout << i << ", " << j << ", " << ins->type
      //     << "; " << ins->nr_cols << ", " << l[i]->nr_cols << LF;
      if (ins->ht->def <= 0) continue;
      if (ins->nr_cols == 1 && l[i]->nr_cols > 1) {
        if (is_tuple (lvs->channel, "footnote"))
          wide_spc += ins->ht + fn_sep;
        else if (is_tuple (lvs->channel, "float"))
          wide_spc += ins->ht + float_sep;
        else if (is_tuple (lvs->channel, "if-page-break"))
          break_spc += ins->ht + as_space (lvs->channel[2]);
      }
      else {
        if (is_tuple (lvs->channel, "footnote"))
          foot_spc += ins->ht + fn_sep;
        else if (is_tuple (lvs->channel, "float")) {
          if (float_here (lvs->channel)) float_spc += ins->ht + 2*float_sep;
          else float_spc += ins->ht + float_sep;
        }
        else if (is_tuple (lvs->channel, "if-page-break"))
          break_spc += ins->ht + as_space (lvs->channel[2]);
      }
    }
    ins_list  << ins_here;
    foot_ht   << foot_spc;
    foot_tot  << (i==0? space(0): foot_tot[i-1] + foot_ht[i]);
    float_ht  << float_spc;
    float_tot << (i==0? space(0): float_tot[i-1] + float_ht[i]);
    wide_ht   << wide_spc;
    wide_tot  << (i==0? space(0): wide_tot[i-1] + wide_ht[i]);
    break_ht  << break_spc;

    if (i>0 && l[i]->nr_cols != l[i-1]->nr_cols) same= i;
    col_number << l[i]->nr_cols;
    col_same   << same;

    bool np= false, pb= false;
    if (!papyrus_mode && l[i]->type == PAGE_CONTROL_ITEM) {
      if (l[i]->t == PAGE_BREAK) pb= true;
      if (l[i]->t == NEW_PAGE || l[i]->t == NEW_DPAGE) np= pb= true;
    }
    must_new   << np;
    must_break << pb;
  }
  col_same   << same;
  must_new   << false;
  must_break << false;

  best_prev (path (0))= path (-2); 
  best_pens (path (0))= 0;
  //cout << HRULE;
}

/******************************************************************************
* Trivial layout of footnote and floating page insertions
******************************************************************************/

insertion
new_breaker_rep::make_insertion (lazy_vstream lvs, path p) {
  // FIXME: Very long insertions should themselves be page-broken
  
  path p1= p * 0;
  path p2= p * N(lvs->l);
  insertion ins (copy (lvs->channel), p1, p2);
  
  array<page_item> l= lvs->l;
  if (N(l) == 0) {
    ins->ht     = space (0);
    ins->top_cor= 0;
    ins->bot_cor= 0;
    ins->pen    = 0;
    ins->nr_cols= 1;
    return ins;
  }
  
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
  ins->nr_cols= l[0]->nr_cols;
  return ins;
}

/******************************************************************************
* Determination of the min and max spaces master routine
******************************************************************************/

space
new_breaker_rep::compute_space (path b1, path b2) {
  //cout << "    Compute space " << b1 << ", " << b2 << LF;
  int i1= b1->item, i2= b2->item;
  if (!is_nil (b1->next)) {
    if (b2 == b1) return space (0);
    path nx= b1->next;
    path q1= path (b1->item, nx->next->next);
    int  i0= nx->item;
    int  j0= nx->next->item;
    insertion fl= ins_list[i0][j0];
    space sep= float_sep;
    if (fl->ht->def <= 0) return compute_space (q1, b2);
    if (float_here (fl->type)) sep= 2*sep;
    return fl->ht + sep + compute_space (q1, b2);
  }
  if (!is_nil (b2->next))
    return compute_space (b1, path (b2->item)) -
           compute_space (b2, path (b2->item));
  if (i1 == i2) return space (0);
  
  space spc;
  if (i1 == 0) { if (i2 > 1) spc= copy (body_tot[i2-2]); }
  else spc= body_tot[i2-2] - body_tot[i1-1];
  SI top_cor= body_cor[i1]->max;
  SI bot_cor= body_cor[i2-1]->min;
  spc += break_ht[i1];
  spc += space (top_cor + body_cor[i2-1]->def + bot_cor);

  if (foot_tot[i2-1]->def > (i1==0? 0: foot_tot[i1-1]->def)) {
    space foot_spc= foot_tot[i2-1] - (i1==0? space(0): foot_tot[i1-1]);
    foot_spc += fnote_sep - fn_sep;
    spc += foot_spc;
  }

  if (float_tot[i2-1]->def > (i1==0? 0: float_tot[i1-1]->def))
    spc += (float_tot[i2-1] - (i1==0? space(0): float_tot[i1-1]));

  //cout << "    Computed space " << b1 << ", " << b2 << " ~> " << spc << LF;
  return spc;
}

/******************************************************************************
* Find page breaks for a given start
******************************************************************************/

bool
new_breaker_rep::last_break (path b) {
  return last_page_flag && (b == path (N(l)) ||
                            (must_new[b->item] && is_nil (b->next)));
}

void
new_breaker_rep::find_page_breaks (path b1) {
  path b1x= b1;
  if (must_break[b1x->item] && b1x->item < N(l))
    b1x= path (b1x->item + 1, b1x->next);
  //cout << "Find page breaks " << b1 << LF;
  bool ok= false, found_one= false;
  vpenalty prev_pen= best_pens [b1];
  int n= N(l);
  int float_status= 0;
  path floats;
  path b2= b1;
  while (true) {
    if (height->def >= (1 << 28) && b2->item < n)
      b2= path (n);
    else if (!is_nil (b2->next))
      b2= path (b2->item, b2->next->next->next);
    else if (b2->item >= n)
      break;
    else {
      int i= b2->item;
      if (N(ins_list[i]) != 0 &&
          float_tot[i]->def > (i==0? 0: float_tot[i-1]->def)) {
        for (int j=0; j<N(ins_list[i]); j++) {
          insertion ins= ins_list[i][j];
          if (is_tuple (ins->type, "float")) {
            if (ins->nr_cols == 1 && l[i]->nr_cols > 1);
            else if (float_has (ins->type, 'f')) {
              while (!is_nil (floats)) {
                int i2= floats->item;
                int j2= floats->next->item;
                floats= floats->next->next;
                insertion ins2= ins_list[i2][j2];
                if (float_status == 0 && !float_has (ins2->type, 't'))
                  float_status= 1;
                if (float_status == 1 && !float_has (ins2->type, 'h'))
                  float_status= 2;
                if (float_status == 2 && !float_has (ins2->type, 'b'))
                  float_status= 3;
              }
            }
            else floats= floats * path (i, j);
          }
        }
      }
      b2= path (i+1, floats);
    }
    if (b2->item > n) break;
    bool break_page= must_break[b2->item];
    if (must_new[b2->item]) b2= path (b2->item);
    
    space spc;
    int bpen= l[b2->item - 1]->penalty;
    if (b2->item == n && is_nil (b2->next)) bpen= 0;
    if (b2->item == b1->item) bpen= 0;
    if (break_page) bpen= 0;
    if (float_status == 3) {
      if (ok) break;
      else bpen += BAD_FLOATS_PENALTY;
    }
    if (bpen < HYPH_INVALID) {
      ok= true;
      vpenalty pen= prev_pen + vpenalty (bpen);
      if (has_columns (b1x, b2, 1))
        spc= compute_space (b1x, b2);
      else {
        vpenalty mcpen;
        spc= compute_space (b1x, b2, mcpen);
        //cout << "Space " << b1x << ", " << b2
        //     << " ~> " << spc << ", " << mcpen << LF;
        pen += mcpen;
      }
      if (!last_break (b2))
	pen += as_vpenalty (spc->def - height->def);
      if (!last_break (b2) && !break_page && spc->max < height->def) {
	if (spc->max >= height->min) pen += EXTEND_PAGE_PENALTY;
	else {
	  double factor=
	    ((double) max (spc->def, 1))/((double) max (height->def, 1));
	  if (factor < 0.0 ) factor= 0.0;
	  if (factor > 0.99) factor= 0.99;
	  pen += vpenalty ((int) ((1.0 - factor) * TOO_SHORT_PENALTY));
	}
      }
      else if (spc->min > height->def) {
	if (spc->min <= height->max) pen += REDUCE_PAGE_PENALTY;
	else {
	  double factor=
	    ((double) max (spc->def, 1))/((double) max (height->def, 1));
	  if (factor < 1.0  ) factor= 1.0;
	  if (factor > 100.0) factor= 100.0;
	  pen += vpenalty ((int) (factor * TOO_LONG_PENALTY));
	}
      }
      if (!best_pens->contains (b2) && !done_list->contains (b2))
        todo_list (b2)= true;
      if (pen < best_pens [b2]) {
        //cout << b1 << ", " << b2 << " ~> " << pen << "\n";
	best_prev (b2)= b1;
	best_pens (b2)= pen;
      }
      if (best_prev->contains (b2)) found_one= true;
    }
    if (!found_one) continue;
    if (ok && spc->min > height->max && is_nil (b2->next)) break;
    if (break_page && is_nil (b2->next)) break;
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
  todo_list (path (0))= true;  
  while (N(todo_list) != 0) {
    hashmap<path,bool> temp_list= todo_list;
    todo_list= hashmap<path,bool> (false);
    done_list->join (temp_list);
    if (quality>1) {
      for (iterator<path> it= iterate (temp_list); it->busy (); )
        find_page_breaks (it->next ());
    }
    else {
      path best_start;
      vpenalty best_pen= HYPH_INVALID;
      for (iterator<path> it= iterate (temp_list); it->busy (); ) {
        path here= it->next ();
        if (is_nil (best_start) || best_pens[here] < best_pen) {
          best_start= here;
          best_pen= best_pens[here];
        }
      }
      if (best_start == path (N(l))) break;
      find_page_breaks (best_start);
    }
  }
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
new_breaker_rep::make_insertion (int i1, int i2) {
  //cout << "Make insertion " << i1 << ", " << i2 << LF;
  path p1= i1;
  path p2= i2;
  insertion ins ("", p1, p2);  
  space spc;
  if (i1 == 0) { if (i2 >= 2) spc= copy (body_tot[i2-2]); }
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

bool
new_breaker_rep::here_floats (path p) {
  bool here_flag= true;
  while (!is_nil (p)) {
    int i= p->item, j= p->next->item;
    p= p->next->next;
    if (!float_has (ins_list[i][j]->type, 'h')) here_flag= false;
  }
  return here_flag;
}

static path
filter_subsequent (path floats, path avoid) {
  if (is_nil (floats) || is_nil (avoid)) return floats;
  if (floats->item == avoid->item && floats->next->item == avoid->next->item)
    return filter_subsequent (floats->next->next, avoid->next->next);
  return path (floats->item, floats->next->item) *
         filter_subsequent (floats->next->next, avoid);
}

void
new_breaker_rep::lengthen_previous (pagelet& pg, int pos, space done) {
  if (pos == 0 || N(pg->ins) == 0) return;
  space spc= body_ht[pos-1] - l[pos-1]->b->h();
  spc= max (spc - done, space (0));
  pg->ins[N(pg->ins)-1]->xh += spc;
  pg->ht += spc;
}

pagelet
new_breaker_rep::assemble (path start, path end) {
  //cout << "Assemble " << start << ", " << end << LF;
  // Position the floats
  path floats, avoid= end->next;
  for (int i=start->item; i<end->item; i++)
    for (int j=0; j<N(ins_list[i]); j++)
      if (is_tuple (ins_list[i][j]->type, "float")) {
        if (ins_list[i][j]->nr_cols == 1 && l[i]->nr_cols > 1);
        else if (!is_nil (avoid) && avoid->item == i && avoid->next->item == j)
          avoid= avoid->next->next;
        else floats= floats * path (i, j);
      }
  path top= filter_subsequent (start->next, avoid), here, bottom;
  while (!is_nil (floats)) {
    bool ok= false;
    int i= floats->item, j= floats->next->item;
    if (float_has (ins_list[i][j]->type, 't')) {
      top= top * path (i, j);
      floats= floats->next->next;
      if (is_nil (floats)) break;
      ok= true;
    }
    if (N(top) + N(bottom) >= (2 * N(floats)) && here_floats (floats)) {
      here= floats;
      break;
    }
    int num= N(floats);
    i= floats[num-2]; j= floats[num-1];
    if (float_has (ins_list[i][j]->type, 'b')) {
      bottom= path (i, j) * bottom;
      floats= path_up (floats, 2);
      if (is_nil (floats)) break;
      ok= true;
    }
    if (N(top) + N(bottom) >= (2 * N(floats)) && here_floats (floats)) {
      here= floats;
      break;
    }
    if (!ok) {
      here= floats;
      break;
    }
  }

  // Add the page
  pagelet pg (0);
  while (!is_nil (top)) {
    int i= top->item, j= top->next->item;
    top= top->next->next;
    if (ins_list[i][j]->ht->def > 0) {
      pg << ins_list[i][j];
      pg << float_sep;
    }
  }
  int pos= start->item;
  for (int j=0; j<N(ins_list[pos]); j++)
    if (is_tuple (ins_list[pos][j]->type, "if-page-break")) {
      pg << ins_list[pos][j];
      pg << as_space (ins_list[pos][j]->type[2]);
    }
  bool several= false;
  space done= space (0);
  while (!is_nil (here)) {
    int i= here->item, j= here->next->item;
    here= here->next->next;
    if (i >= end->item && i > pos) {
      if (several) lengthen_previous (pg, pos, done);
      insertion ins= make_insertion (pos, i);
      pg << ins;
      if (ins_list[i][j]->ht->def > 0) pg << float_sep;
      pos= i;
      several= true;
    }
    else if (i+1 > pos) {
      if (several) lengthen_previous (pg, pos, done);
      insertion ins= make_insertion (pos, i+1);
      pg << ins;
      if (ins_list[i][j]->ht->def > 0) pg << float_sep;
      pos= i+1;
      several= true;
    }
    if (ins_list[i][j]->ht->def > 0) {
      pg << ins_list[i][j];
      pg << float_sep;
      done= float_sep;
    }
    else done= space (0);
  }
  if (end->item > pos) {
    if (several) lengthen_previous (pg, pos, done);
    insertion ins= make_insertion (pos, end->item);
    pg << ins;
  }
  while (!is_nil (bottom)) {
    int i= bottom->item, j= bottom->next->item;
    bottom= bottom->next->next;
    if (ins_list[i][j]->ht->def > 0) {
      pg << ins_list[i][j];
      pg << float_sep;
    }
  }
  bool has_footnotes= false;
  for (int i=start->item; i<end->item; i++)
    for (int j=0; j<N(ins_list[i]); j++)
      if (is_tuple (ins_list[i][j]->type, "footnote"))
        if (ins_list[i][j]->nr_cols != 1 || l[i]->nr_cols == 1) {
          if (ins_list[i][j]->ht->def > 0) {
            pg << ins_list[i][j];
            if (has_footnotes) pg << fn_sep;
            else pg << fnote_sep;
            has_footnotes= true;
          }
        }
  return pg;
}

void
new_breaker_rep::assemble_skeleton (skeleton& sk, path end, int& offset) {
  path start= best_prev [end];
  //cout << "Assemble skeleton " << start << " -- " << end << LF;
  if (start->item < 0) return;
  assemble_skeleton (sk, start, offset);
  path begin= start;
  if (must_break[begin->item] && begin->item < end->item) {
    if (l[begin->item]->t == NEW_DPAGE)
      if (((N(sk) + offset) & 1) == 1)
        sk << pagelet (space (0));
    begin= path (begin->item + 1, begin->next);
  }
  //cout << "Assemble page " << begin << ", " << end << "; " << offset << LF;
  if (has_columns (begin, end, 1)) {
    pagelet pg= assemble (begin, end);
    bool last_page= last_break (end);
    format_pagelet (pg, height, last_page);
    sk << pg;
  }
  else {
    pagelet pg= assemble_multi_columns (begin, end);
    bool last_page= last_break (end);
    format_pagelet (pg, height, last_page);
    sk << pg;
  }
  for (int i=begin->item; i<end->item; i++)
    if (is_tuple (l[i]->t, "env_page") && l[i]->t[1] == PAGE_NR)
      offset= as_int (l[i]->t[2]->label) - N(sk);
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
  int offset= first_page - 1;
  H->assemble_skeleton (sk, path (N(l)), offset);
  //cout << HRULE << LF;
  tm_delete (H);
  return sk;
}
