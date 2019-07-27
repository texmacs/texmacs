
/******************************************************************************
* MODULE     : page_breaker.cpp
* DESCRIPTION: Page breaking
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Line/lazy_vstream.hpp"
#include "vpenalty.hpp"
#include "skeleton.hpp"
#include "boot.hpp"

#include "merge_sort.hpp"
void sort (pagelet& pg);
vpenalty as_vpenalty (SI diff);

typedef array<int>               vbreak;
typedef array<int>               ladder;
#define MAIN_FLOW    0
#define FNOTE_FLOW   1
#define FLOAT_FLOW   2
#define NR_FLOWS     3

#define INVALID_BREAK  0
#define BAD_BREAK      1
#define VALID_BREAK    2

/******************************************************************************
* The page_breaker class
******************************************************************************/

struct page_breaker_rep {
  array<page_item> l;
  int   papyrus_mode;
  int   sub_start;
  int   sub_end;
  space height;
  space fn_sep;
  space fnote_sep;
  space float_sep;
  font  fn;
  int   first_page;
  bool  last_page_flag;

  int                 nr_flows;   // number of flows;
  hashmap<path,int>   flow_id;    // unique number for each flow
  array<path>         flow_fl;    // flow associated to flow identifier
  array<array<path> >  flow;       // for each flow the paths to all page_items
  array<array<space> > flow_ht;    // the heights of these page_items
  array<array<space> > flow_cor;   // top and bottom corrections of page_items
  array<array<space> > flow_tot;   // total heights up to a certain index
  array<array<int> >   flow_cont;  // number of contiguous blocks until here

  array<vbreak>       brk;        // possible break points
  array<space>        brk_tot;    // (estimated) total height until breaks
  array<vbreak>       tmp;        // temporary space during sort
  array<space>        tmp_tot;    // temporary space during sort
  hashmap<vbreak,int> brk_nr;     // find number of break from break
  array<ladder>       brk_prev;   // maximal previous incomparable breaks
  array<ladder>       brk_next;   // minimal next incomparable breaks
  int                 brk_first;  // first break
  int                 brk_last;   // last break

  int                 tc_start;   // two column start
  int                 tc_middle;  // two column middle
  int                 tc_end;     // two column end
  int                 tc_ref;     // two column reference
  ladder              tc_ld;      // two column ladder
  int                 tc_bmid;    // best middle
  vpenalty            tc_bpen;    // corresponding penalty
  pagelet             tc_bpg1;    // & left column
  pagelet             tc_bpg2;    // & right column

  int                 quality;    // quality of page breaking
  int                 cur_start;  // current start of page
  int                 cur_end;    // current end of page
  int                 cur_ref;    // current approximation of best end
  ladder              cur_ld;     // current ladder for untreated tries
  int                 best_end;   // best end of page for given start
  vpenalty            best_pen;   // corresponding penalty
  pagelet             best_pg;    // & pagelet
  array<int>          best_prev;  // best previous break points
  array<vpenalty>     best_pens;  // corresponding penalties
  array<pagelet>      best_pgs;   // & pagelets

  page_breaker_rep (array<page_item> l, space ph, int quality,
                    space fn_sep, space fnote_sep, space float_sep,
                    font fn, int fp);

  void init_flows (int start, int end);
  void init_flows (array<page_item> l, int start, int end, path p, path flb);
  void show_penalties ();

  bool correct_break (vbreak br, int id);
  bool correct_break (vbreak br);
  void spool_break (vbreak& br, path flb, path start);
  void rewind_break (vbreak& br, path flb, int& id, path& p);
  bool float_property (path p, char c);
  array<vbreak> generate_breaks (vbreak br, int id);
  array<vbreak> generate_breaks (vbreak br, int id, path flb);
  space compute_total_height (vbreak br);
  void init_breaks ();
  void sort_breaks (int start, int end);
  void sort_breaks ();

  void init_ladders ();
  ladder inc_merge (ladder ld1, ladder ld2);
  ladder dec_merge (ladder ld1, ladder ld2);

  bool correct_pagelet (int start, int end);
  bool last_break (int start, int end, int id);
  insertion make_insertion (int id, int ch, int i1, int i2, bool flag);
  pagelet make_pagelet (int start, int end, path flb, int nr_cols);
  vpenalty format_insertion (insertion& ins, double stretch);
  vpenalty format_pagelet (pagelet& pg, double stretch);
  vpenalty format_pagelet (pagelet& pg, space ht, bool last_page);

  void search_mcol_breaks (vbreak br1, vbreak br2, array<array<int> > part,
			   path p1, path p2, int& i1, int& i2);
  insertion make_multi_column (skeleton sk, int real_nr_cols);
  insertion make_multi_column (int start, int end, path flb, int nr_cols);
  int tc_propose_break (path flb);
  insertion make_two_column (int start, int end, path flb);

  void fast_break_page (int i1, int& first_end);
  void fast_assemble_skeleton (skeleton& sk, int end);
  void fast_assemble_skeleton (skeleton& sk);

  int propose_break ();
  void find_next_breaks ();
  void assemble_skeleton (skeleton& sk, int last);
  void assemble_skeleton (skeleton& sk);
  void assemble_skeleton (skeleton& sk, int start, int end);
  skeleton make_skeleton ();
};

/******************************************************************************
* Constructor
******************************************************************************/

page_breaker_rep::page_breaker_rep (
  array<page_item> l2, space ph, int quality2,
  space fn_sep2, space fnote_sep2, space float_sep2,
  font fn2, int fp2):
    l (l2), papyrus_mode (ph == (MAX_SI >> 1)), height (ph),
    fn_sep (fn_sep2), fnote_sep (fnote_sep2), float_sep (float_sep2),
    fn (fn2), first_page (fp2),
    flow_id (-1), brk_nr (-1), quality (quality2)
{}

/******************************************************************************
* Subroutines
******************************************************************************/

bool
var_path_inf_eq (path p1, path p2) {
  if (is_nil (p1) || is_nil (p2)) return is_nil (p1);
  if (p1->item<p2->item) return true;
  if (p1->item>p2->item) return false;
  return var_path_inf_eq (p1->next, p2->next);
}

static int
fast_find (array<path> a, path p) {
  int n= N(a), step= n, i= n>>1;
  while (step>1) {
    step= (step+1)>>1;
    if (var_path_inf_eq (p, a[i])) i= max (0, i-step);
    else i= min (n-1, i+step);
  }
  if (!var_path_inf_eq (p, a[i])) i= i+1;
  return i;
}

static int
find_length (array<space> a, int start, SI ht) {
  int n= N(a)- start, step= n, i= (start + N(a))>>1;
  while (step>1) {
    step= (step+1)>>1;
    if (ht <= a[i]->def) i= max (start, i-step);
    else i= min (N(a)-1, i+step);
  }
  if (ht > a[i]->def) i= min (N(a)-1, i+1);
  return i;
}

static int
find_end_block (array<path> a, int start, int end) {
  if (a[end-1] == path_add (a[start], end-1-start)) return end;
  int n= end-start, step= n, i= (start+end+1)>>1;
  while (step>1) {
    step= (step+1)>>1;
    if ((i>start) && (a[i-1] != path_add (a[start], i-1-start)))
      i= max (start, i-step);
    else i= min (end-1, i+step);
  }
  if ((i>start) && (a[i-1] != path_add (a[start], i-1-start))) i--;
  return i;
}

/*static*/ page_item
access (array<page_item> l, path p) {
  page_item item= l[p->item];
  if (is_nil (p->next)) return item;
  else {
    lazy_vstream ins= (lazy_vstream) item->fl[p->next->item];
    return access (ins->l, p->next->next);
  }
}

/*static*/ array<page_item>
sub (array<page_item> l, path p, path q) {
  if (is_atom (p) && is_atom (q)) {
    int i= p->item, j= q->item, k;
    array<page_item> r (j-i);
    for (k=i; k<j; k++) r[k-i]= l[k];
    return r;
  }
  else {
    if ((N(p) <= 2) || (N(q) <= 2)) {
      failed_error << "The paths were " << p << " and " << q << "\n";
      FAILED ("paths to short");
    }
    if ((p->item != q->item) || (p->next->item != q->next->item)) {
      failed_error << "The paths were " << p << " and " << q << "\n";
      FAILED ("paths don't match");
    }
    page_item item= l[p->item];
    lazy_vstream ins= (lazy_vstream) item->fl[p->next->item];
    return sub (ins->l, p->next->next, q->next->next);
  }
}

static inline bool
starts (path p, path q) {
  return (N(p) >= N(q)) && (head (p, N(q)) == q);
}

/******************************************************************************
* Initialize flows
******************************************************************************/

void
page_breaker_rep::init_flows (int start, int end) {
  sub_start= start;
  sub_end  = end;
  nr_flows = 0;
  flow_id  = hashmap<path,int> (-1);
  flow_fl  = array<path> (0);
  flow     = array<array<path> > (0);
  flow_ht  = array<array<space> > (0);
  flow_cor = array<array<space> > (0);
  flow_tot = array<array<space> > (0);
  flow_cont= array<array<int> > (0);
  init_flows (l, start, end, path (), path ());
}

void
page_breaker_rep::init_flows (
  array<page_item> l, int start, int end, path p, path flb)
{
  int i;
  for (i=start; i<end; i++) {
    path fl= flb * l[i]->nr_cols;
    if (!flow_id->contains (fl)) {
      flow_id (fl)= nr_flows;
      flow_fl   << fl;
      flow      << array<path> (0);
      flow_ht   << array<space> (0);
      flow_cor  << array<space> (0);
      flow_tot  << array<space> (0);
      flow_cont << array<int> (0);
      nr_flows++;
    }
    int  id= flow_id (fl);
    int  nr= N (flow_tot [id]);
    SI   bot_cor= max (0, l[i]->b->y1- fn->y1);
    SI   bod_cor= l[i]->b->h ();
    SI   top_cor= max (0, fn->y2- l[i]->b->y2);
    bool cont= (nr>0) && (path_up (flow[id][nr-1]) == p);
    flow     [id] << (p * i);
    flow_ht  [id] << (space (l[i]->b->h()) + l[i]->spc);
    flow_cor [id] << space (bot_cor, bod_cor, top_cor);
    flow_tot [id] << (nr==0? space(0): flow_tot[id][nr-1]) + flow_ht[id][nr];
    flow_cont[id] << (nr==0? 1: flow_cont[id][nr-1] + (cont? 0: 1));
    if ((i==end-1) || (l[i]->nr_cols!=l[i+1]->nr_cols)) l[i]->penalty=0;
  }

  for (i=start; i<end; i++) {
    path fl= flb * l[i]->nr_cols;
    int j, k= N (l[i]->fl);
    for (j=0; j<k; j++) {
      int ch= -1;
      lazy_vstream ins= (lazy_vstream) l[i]->fl[j];
      array<page_item> sub_l= ins->l;
      if (is_tuple (ins->channel, "footnote")) ch= 0;
      else if (is_tuple (ins->channel, "float")) ch= 1;
      init_flows (sub_l, 0, N(sub_l), p * path (i, j), fl * ch);
    }
  }
}

void
page_breaker_rep::show_penalties () {
  int id, i;
  for (id=0; id<nr_flows; id++)
    for (i=0; i<N(flow[id]); i++)
      cout << id << ", " << i << ":\t" << flow[id][i] << " -> "
	   << access (l, flow[id][i])->penalty << "\n";
}

/******************************************************************************
* Initialize possible breaks
******************************************************************************/

bool
page_breaker_rep::correct_break (vbreak br, int id) {
  if ((br[id] > 0) && (br[id] < N(flow[id]))) {
    path p= flow[id][br[id]-1];
    if (path_inc (p) != flow[id][br[id]]) return true;
    if (access (l, p)->penalty >= HYPH_INVALID) return false;
  }
  return true;
}


bool
page_breaker_rep::correct_break (vbreak br) {
  int id;
  for (id=0; id<nr_flows; id++)
    if (!correct_break (br, id)) return false;
  return true;
}

void
page_breaker_rep::spool_break (vbreak& br, path flb, path start) {
  int id;
  for (id=0; id<nr_flows; id++)
    if (path_up (flow_fl[id]) == flb)
      br[id]= fast_find (flow[id], start);
}

void
page_breaker_rep::rewind_break (
  vbreak& br, path flb, int& best_id, path& best_p)
{
  int id;
  best_id= -1;
  best_p = path ();
  for (id=0; id<nr_flows; id++)
    if (path_up (flow_fl[id]) == flb)
      if ((br[id]>0) && var_path_inf_eq (best_p, flow[id][br[id]-1])) {
	best_id= id;
	best_p = flow[id][br[id]-1];
      }
  if (best_id != -1) br[best_id]--;
}

bool
page_breaker_rep::float_property (path p, char c) {
  int i;
  page_item item= access (l, path_up (p, 2));
  lazy_vstream ins= (lazy_vstream) item->fl [last_item (path_up (p))];
  string s= as_string (ins->channel[1]);
  for (i=0; i<N(s); i++)
    if (s[i] == c) return true;
  return false;
}

array<vbreak>
page_breaker_rep::generate_breaks (vbreak br, int id)
  // generate all breaks by filling in the (-1) entries in 'br'
  // by break points for subflows of the flow with identifier 'id'
{
  // cout << "Generate breaks " << br << ", " << id << LF;

  path flb= path_up (flow_fl[id]);
  int sid;
  for (sid=0; sid<nr_flows; sid++) 
    if (br[sid] == -1) {
      path sflb= path_up (flow_fl[sid]);
      if ((N(sflb) == (N(flb)+2)) && starts (sflb, flb)) break;
    }
  if (sid == nr_flows) {
    array<vbreak> brk (1);
    brk[0]= br;
    return brk;
  }
  else {
    int j;
    array<vbreak> brk;
    array<vbreak> ref=
      generate_breaks (copy (br), id, path_up (flow_fl[sid]));
    for (j=0; j<N(ref); j++)
      brk << generate_breaks (copy (ref[j]), id);
    return brk;
  }
}

array<vbreak>
page_breaker_rep::generate_breaks (vbreak br, int id, path flb)
  // generate all breaks by filling in the (-1) entries in 'br'
  // by break points for subflows of the form 'flb * nr_cols' and
  // only those break points which are relevant for the given break 'br[id]'
  // for the flow with identifier 'id' (which may be -1 for the top flow)
{
  // cout << "Generate breaks " << br << ", " << id << ", " << flb << LF;

  int sid;
  path halt;
  if (id == -1) {
    halt= path (sub_end);
    for (sid=0; sid<nr_flows; sid++)
      if (is_atom (flow_fl[sid]))
	br[sid]= 0;
  }
  else if (br[id]==0) {
    halt= path (0);
    for (sid=0; sid<nr_flows; sid++)
      if (path_up (flow_fl[sid]) == flb)
	br[sid]=0;
  }
  else {
    halt= path_inc (flow[id][br[id]-1]);
    path start= flow[id][br[id]-1];
    while (last_item (start) != 0) {
      if (access (l, path_dec (start))->penalty < HYPH_INVALID) break;
      start= path_dec (start);
    }
    if ((!is_nil (flb)) && (last_item (flb) == 1)) {
      int credit= 3;
      spool_break (br, flb, halt);
      while (true) {
	path p;
	vbreak old= copy (br);
	rewind_break (br, flb, sid, p);
	if ((sid == -1) || (br == old)) break;
	if (float_property (flow[sid][br[sid]], 'f')) {
	  br= old;
	  break;
	}
	if (var_path_inf_eq (p, start)) {
	  if (correct_break (br)) credit--;
	  if (credit == 0) break;
	}
      }
    }
    else spool_break (br, flb, start);
  }

  array<vbreak> brk (0);
  if (correct_break (br)) {
    int  best_sid= -1;
    path best_p (MAX_SI);
    for (sid=0; sid<nr_flows; sid++)
      if (path_up (flow_fl[sid]) == flb) {
	int pos= br[sid];
	if (best_sid == -1) best_sid= sid;
	if ((pos > 0) && path_inf (flow[sid][pos-1], best_p)) {
	  best_sid= sid;
	  best_p  = flow[sid][pos-1];
	}
      }
    ASSERT (best_sid != -1, "flow not found");
    brk << generate_breaks (copy (br), best_sid);
  }

  while (true) {
    int  best_sid= -1;
    path best_p= halt;
    for (sid=0; sid<nr_flows; sid++)
      if (path_up (flow_fl[sid]) == flb) {
	int pos= br[sid];
	if ((pos < N(flow[sid])) && path_inf (flow[sid][pos], best_p)) {
	  best_sid= sid;
	  best_p  = flow[sid][pos];
	}
      }
    if (best_sid == -1) break;
    br[best_sid]++;
    if (correct_break (br))
      brk << generate_breaks (copy (br), best_sid);
  }
  return brk;
}

space
page_breaker_rep::compute_total_height (vbreak br) {
  int id;
  space tot;
  // cout << "Total height of break " << br << LF;
  for (id=0; id<nr_flows; id++) {
    path fl= flow_fl[id];
    space stot= br[id]==0? space (0): copy (flow_tot[id][br[id]-1]);
    if (!is_atom (fl)) {
      int ch  = last_item (path_up (fl));
      int cont= br[id]==0? 0: flow_cont[id][br[id]-1];
      if (ch == 0)
	stot += space (0, cont * fnote_sep->def / 2, cont * fnote_sep->max);
      if (ch == 1)
	stot += space (cont * float_sep->min,
		       (3 * cont * float_sep->def) / 2,
		       2 * cont * float_sep->max);
    }
    int nr_cols= last_item (fl);
    // cout << "  " << id << ", " << br[id] << ":\t" << (stot/nr_cols) << LF;
    tot += stot / nr_cols;
  }
  return tot;
}

void
page_breaker_rep::init_breaks () {
  int id;
  vbreak br (nr_flows);
  for (id=0; id<nr_flows; id++) br[id]= -1;
  brk= generate_breaks (copy (br), -1, path ());

  int i, brn= N(brk);
  brk_tot= array<space> (brn);
  for (i=0; i<brn; i++)
    brk_tot[i]= compute_total_height (brk[i]);
}

/******************************************************************************
* Sort the breaks on expected default total height
******************************************************************************/

void
page_breaker_rep::sort_breaks (int start, int end) {
  if (end-start<=1) return;
  if (end-start==2) {
    if (!(brk_tot[start]->def <= brk_tot[start+1]->def)) {
      tmp    [start]  = brk    [start];
      tmp_tot[start]  = brk_tot[start];
      brk    [start]  = brk    [start+1];
      brk_tot[start]  = brk_tot[start+1];
      brk    [start+1]= tmp    [start];
      brk_tot[start+1]= tmp_tot[start];
    }
    return;
  }
  int middle= (start+end) >> 1; 
  sort_breaks (start, middle);
  sort_breaks (middle, end);
  int i, j, k;
  for (i= start, j= middle, k= start; (i<middle) && (j<end); )
    if (brk_tot[i]->def <= brk_tot[j]->def) {
      tmp    [k]= brk    [i];
      tmp_tot[k]= brk_tot[i];
      k++; i++;
    }
    else {
      tmp    [k]= brk    [j];
      tmp_tot[k]= brk_tot[j];
      k++; j++;
    }
  j= k;
  while (i!=middle) {
    brk    [k]= brk    [i];
    brk_tot[k]= brk_tot[i];
    k++; i++;
  }
  for (i=start; i<j; i++) {
    brk    [i]= tmp    [i];
    brk_tot[i]= tmp_tot[i];
  }
}

void
page_breaker_rep::sort_breaks () {
  int i, nrb= N(brk);
  tmp= array<vbreak> (nrb);
  tmp_tot= array<space> (nrb);
  sort_breaks (0, nrb);
  for (i=0; i<nrb; i++)
    brk_nr (brk[i])= i;
}

/******************************************************************************
* Initialization and manipulation of ladders
******************************************************************************/

bool
inf_eq (vbreak br1, vbreak br2) {
  int i, n= N(br1);
  for (i=0; i<n; i++)
    if (br1[i] > br2[i]) return false;
  return true;
}

void
page_breaker_rep::init_ladders () {
  int i, j, nrb= N(brk);
  brk_prev= array<ladder> (nrb);
  brk_next= array<ladder> (nrb);

  ladder ld;
  for (i=0; i<nrb; i++) {
    ladder new_ld (1);
    new_ld[0]= i;
    for (j=0; j<N(ld); j++)
      if (!inf_eq (brk[ld[j]], brk[i]))
	new_ld << ld[j];
    ld= new_ld;
    brk_prev[i]= ld;
  }

  ld= ladder ();
  for (i=nrb-1; i>=0; i--) {
    ladder new_ld (1);
    new_ld[0]= i;
    for (j=0; j<N(ld); j++)
      if (!inf_eq (brk[i], brk[ld[j]]))
	new_ld << ld[j];
    ld= new_ld;
    brk_next[i]= ld;
  }

  for (i=0; i<nrb; i++) {
    bool flag= true;
    for (j=0; j<nr_flows; j++)
      flag= flag && (brk[i][j] == 0);
    if (flag) { brk_first= i; break; }
  }

  for (i=nrb-1; i>=0; i--) {
    bool flag= true;
    for (j=0; j<nr_flows; j++)
      flag= flag && (brk[i][j] == N(flow[j]));
    if (flag) { brk_last= i; break; }
  }
}

static ladder
sub (ladder ld, int start, int end) {
  // cout << "sub " << ld << ", " << start << ", " << end;
  int i, n= end-start;
  ladder ret (n);
  for (i=0; i<n; i++)
    ret[i]= ld[start+i];
  // cout << " -> " << ret << LF;
  return ret;
}

ladder
page_breaker_rep::dec_merge (ladder ld1, ladder ld2) {
  // cout << "Decreasing merge " << ld1 << ", " << ld2;
  int i, j, k;
  ladder ld;
  for (i=0, j=0; (i<N(ld1)) || (j<N(ld2)); ) {
    if ((j == N(ld2)) || ((i < N(ld1)) && (ld1[i] > ld2[j]))) {
      for (k=0; k<N(ld2); k++)
	if (inf_eq (brk[ld1[i]], brk[ld2[k]]))
	  break;
      if (k == N(ld2)) ld << ld1[i];
      i++;
      continue;
    }
    if ((i == N(ld1)) || ((j < N(ld2)) && (ld2[j] > ld1[i]))) {
      for (k=0; k<N(ld1); k++)
	if (inf_eq (brk[ld2[j]], brk[ld1[k]]))
	  break;
      if (k == N(ld1)) ld << ld2[j];
      j++;
      continue;
    }
    ld << ld1[i];
    i++, j++;
  }
  // cout << " -> " << ld << LF;
  return ld;
}

ladder
page_breaker_rep::inc_merge (ladder ld1, ladder ld2) {
  // cout << "Increasing merge " << ld1 << ", " << ld2;
  int i, j, k;
  ladder ld;
  for (i=0, j=0; (i<N(ld1)) || (j<N(ld2)); ) {
    if ((j == N(ld2)) || ((i < N(ld1)) && (ld1[i] < ld2[j]))) {
      for (k=0; k<N(ld2); k++)
	if (inf_eq (brk[ld2[k]], brk[ld1[i]]))
	  break;
      if (k == N(ld2)) ld << ld1[i];
      i++;
      continue;
    }
    if ((i == N(ld1)) || ((j < N(ld2)) && (ld2[j] < ld1[i]))) {
      for (k=0; k<N(ld1); k++)
	if (inf_eq (brk[ld1[k]], brk[ld2[j]]))
	  break;
      if (k == N(ld1)) ld << ld2[j];
      j++;
      continue;
    }
    ld << ld1[i];
    i++, j++;
  }
  // cout << " -> " << ld << LF;
  return ld;
}

/******************************************************************************
* Format pagelets
******************************************************************************/

bool
page_breaker_rep::correct_pagelet (int start, int end) {
  int id, mid;
  for (id=0; id<nr_flows; id++)
    if (brk[start][id] > brk[end][id])
      return false;
  for (id=0; id<nr_flows; id++)
    if ((!is_atom (flow_fl[id])) && (last_item (path_up (flow_fl[id])) == 1))
      for (mid= 0; mid<nr_flows; mid++)
	if ((brk[start][mid] < brk[end][mid]) &&
	    (flow_fl[mid] == path_up (flow_fl[id], 2)))
	  {
	    int idb= brk[end][id], midb= brk[start][mid];
	    if ((idb == N(flow[id])) || (midb == N(flow[mid]))) {
	      if (idb < N(flow[id])) return false;
	    }
	    else if (var_path_inf_eq (flow[id][idb], flow[mid][midb]))
	      return false;
	  }
  return true;
}

bool
page_breaker_rep::last_break (int start, int end, int id1) {
  int id2;
  vbreak br1= brk[start], br2= brk[end];
  for (id2=0; id2<nr_flows; id2++)
    if ((id2 != id1) &&
	(br1[id2] < br2[id2]) &&
	(path_up (flow_fl[id2]) == path_up (flow_fl[id1])) &&
	var_path_inf_eq (flow[id1][br2[id1]-1], flow[id2][br2[id2]-1]))
      return false;
  return true;
}

insertion
page_breaker_rep::make_insertion (int id, int ch, int i1, int i2, bool flag) {
  path p1= flow[id][i1];
  path p2= path_inc (flow[id][i2-1]);
  space spc;
  if (i1 == 0) { if (i2 > 1) spc= copy (flow_tot[id][i2-2]); }
  else spc= flow_tot[id][i2-2] - flow_tot[id][i1-1];
  SI top_cor= flow_cor[id][i1]->max;
  SI bot_cor= flow_cor[id][i2-1]->min;
  spc += space (top_cor + flow_cor[id][i2-1]->def + bot_cor);

  tree type= "";
  if (ch == 0) type= tuple ("footnote");
  else if (ch == 1) {
    // -- Why did we perform the first test before David's correction?
    // if ((i1>1) && (p1 != path_inc (flow[id][i1-1])))
    // type= tuple ("float", "t");
    // else if (float_property (p1, 'h')) type= tuple ("float", "h");
    if (float_property (p1, 'h')) type= tuple ("float", "h");
    else if (float_property (p1, 'b')) type= tuple ("float", "b");
    else type= tuple ("float", "t");
  }

  insertion ins (type, p1, p2);
  ins->ht     = spc;
  ins->top_cor= top_cor;
  ins->bot_cor= bot_cor;
  if (flag) ins->pen= access (l, flow[id][i2-1])->penalty;
  return ins;
}

pagelet
page_breaker_rep::make_pagelet (int start, int end, path flb, int nr_cols) {
  // cout << "Make pagelet "
  //      << start << " " << brk[start] << ", "
  //      << end   << " " << brk[end  ] << ", " << nr_cols << LF << INDENT;

  // break flows into consecutive blocks
  int id, sid;
  array<array<int> > part (nr_flows);
  for (id=0; id<nr_flows; id++)
    if (brk[start][id] < brk[end][id])
      if (starts (flow_fl[id], flb)) {
	int i, i1= brk[start][id], i2= brk[end][id];
	part[id] << i1;
	for (i=i1; i<i2; ) {
	  i= find_end_block (flow[id], i, i2);
	  part[id] << i;
	}
      }

  // handle floats which may be placed inside text
  for (sid=0; sid<nr_flows; sid++)
    if (N(part[sid]) != 0) {
      path sfl= flow_fl[sid];
      if ((!is_atom (sfl)) && (last_item (path_up (sfl)) == 1) &&
	  (last_item (path_up (sfl, 2)) == nr_cols))
	{
	  int i, j;
	  array<int> extra (0);
	  id= flow_id (path_up (sfl, 2));
	  for (i=0; i<N(part[sid])-1; i++) {
	    int spos= part[sid][i];
	    bool flag= float_property (flow[sid][spos], 'h');
	    if (flag) {
	      int pos= fast_find (flow[id], flow[sid][spos]);
	      for (j=0; j<N(part[id])-1; j++)
		if ((pos >= part[id][j]+3) && (pos <= part[id][j+1]-3))
		  extra << pos;
	    }
	  }
	  merge_sort (extra);
	  array<int> merge (0);
	  for (i=0, j=0; i<N(part[id]); ) {
	    if ((j == N(extra)) || (part[id][i] <= extra[j]))
	      merge << part[id][i++];
	    else {
	      if ((N(merge) == 0) || (merge[N(merge)-1] != extra[j]))
		merge << extra[j];
	      j++;
	    }
	  }
	  part[id]= merge;
	}
    }

  // fill pagelet with insertions of consecutive blocks
  pagelet pg (0);
  for (id=0; id<nr_flows; id++)
    if (brk[start][id] < brk[end][id])
      if (starts (flow_fl[id], flb)) {
	int i, ch=-1;
	path fl= flow_fl[id];
	if (!is_atom (fl)) ch= last_item (path_up (fl));
	for (i=1; i<N(part[id]); i++) {
	  insertion ins;
	  if (last_item (flow_fl[id]) == nr_cols) {
	    bool flag= (i == N(part[id])-1) && last_break (start, end, id);
	    ins= make_insertion (id, ch, part[id][i-1], part[id][i], flag);
	    // cout << "flow " << id << " : "
	    //      << part[id][i-1] << " -- " << part[id][i] << " " << ins->ht
	    //      << ", cor= " << ins->bot_cor << ", " << ins->top_cor
	    //      << ", penalty= " << ins->pen << LF;
	    pg << ins;
	  }
	  else if ((nr_cols == 1) && (path_up (flow_fl[id]) == flb)) {
	    path p1= flow[id][part[id][i-1]];
	    path p2= path_inc (flow[id][part[id][i]-1]);
	    int i1, i2;
	    search_mcol_breaks (brk[start], brk[end], part, p1, p2, i1, i2);
	    ins= make_multi_column (i1, i2, flb, last_item (flow_fl[id]));
	    // cout << "flow " << id << " : "
	    //      << part[id][i-1] << " -- " << part[id][i] << " " << ins->ht
	    //      << ", cor= " << ins->bot_cor << ", " << ins->top_cor
	    //      << ", penalty= " << ins->pen << LF;
	    pg << ins;
	  }
	}
      }

  // sort and compute height
  sort (pg);
  int i, n= N(pg->ins);
  for (i=0; i<n-1; i++) {
    insertion ins= pg->ins[i];
    insertion next= pg->ins[i+1];
    if (ins->type != next->type) {
      if (is_tuple (next->type, "footnote")) {
	// cout << "add    : footnote " << fnote_sep << LF;
	pg << fnote_sep;
      }
      else if (is_tuple (ins->type, "float")) {
	if (!is_tuple (next->type, "float")) {
	  // cout << "add    : float " << float_sep << LF;
	  pg << float_sep;
	}
      }
      else if (is_tuple (ins->type, "multi-column") ||
	       is_tuple (next->type, "multi-column")) {
	page_item item= access (l, path_dec (ins->end));
	// cout << "add    : multi-column " << item->spc << LF;
	pg << item->spc;
      }
    }
    if (is_tuple (ins->type, "footnote"))
      if (is_tuple (next->type, "footnote")) {
	page_item item= access (l, path_dec (ins->end));
	// cout << "add    : inter-footnote " << (item->spc+fn_sep) << LF;
	pg << (item->spc + fn_sep);
      }
    if (is_tuple (next->type, "float")) {
      // cout << "add    : float " << float_sep << LF;
      pg << float_sep;
    }
  }

  // cout << "height : " << pg->ht << LF;
  // cout << "penalty: " << pg->pen << LF;
  // cout << UNINDENT << "Pagelet: " << pg << LF << LF;
  return pg;
}

vpenalty
page_breaker_rep::format_insertion (insertion& ins, double stretch) {
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
page_breaker_rep::format_pagelet (pagelet& pg, double stretch) {
  // cout << "Stretch " << pg << ": " << stretch << LF;
  int i;
  vpenalty pen;
  pg->stretch= stretch;
  for (i=0; i<N(pg->ins); i++)
    pen += format_insertion (pg->ins[i], stretch);
  return pen;
}

vpenalty
page_breaker_rep::format_pagelet (pagelet& pg, space ht, bool last_page) {
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
* Multi column breaking routines
******************************************************************************/

void
page_breaker_rep::search_mcol_breaks (
  vbreak br1, vbreak br2, array<array<int> > part,
  path p1, path p2, int& i1, int& i2)
{
  // cout << "Search " << p1 << " -- " << p2 << " among parts " << part << LF;
  int id, i;
  br1= copy (br1);
  br2= copy (br2);
  for (id=0; id<nr_flows; id++) {
    /*
    for (i=0; i<N(part[id])-1; i++)
      cout << "  Flow " << id << ": "
	   << flow[id][part[id][i]] << " -- "
	   << path_inc (flow[id][part[id][i+1]-1]) << LF;
    */
    for (i=0; i<N(part[id])-1; i++)
      if (var_path_inf_eq (path_inc (flow[id][part[id][i+1]-1]), p1))
	br1[id]= part[id][i+1];
    for (i=N(part[id])-2; i>=0; i--)
      if (var_path_inf_eq (p2, flow[id][part[id][i]]))
	br2[id]= part[id][i];
  }

  // cout << "Search breaks " << br1 << " -- " << br2 << LF;
  ASSERT (brk_nr->contains (br1) && brk_nr->contains (br2),
	  "break not found");
  i1= brk_nr[br1];
  i2= brk_nr[br2];
}

insertion
page_breaker_rep::make_multi_column (skeleton sk, int real_nr_cols) {
  int i, nr_cols= N(sk);
  space    ht = copy (sk[0]->ht);
  vpenalty pen= sk[0]->pen;
  for (i=1; i<nr_cols; i++) {
    ht->min= max (ht->min, sk[i]->ht->min);
    ht->def += sk[i]->ht->def;
    ht->max= min (ht->max, sk[i]->ht->max);
    pen += sk[i]->pen;
  }
  ht->def /= nr_cols;
  if (ht->max < ht->min) {
    ht->def= ht->max= ht->min;
    pen += UNBALANCED_COLUMNS;
    for (i=1; i<nr_cols; i++)
      if (sk[i-1]->ht->min < sk[i]->ht->min)
	pen += LONGER_LATTER_COLUMN;
  }
  else {
    if (ht->def < ht->min) ht->def= ht->min;
    if (ht->def > ht->max) ht->def= ht->max;
  }
  insertion ins (tuple ("multi-column", as_string (real_nr_cols)), sk);
  ins->ht     = ht;
  ins->pen    = pen;
  ins->top_cor= 0;
  ins->bot_cor= 0;
  return ins;
}

insertion
page_breaker_rep::make_multi_column (
  int start, int end, path flb, int nr_cols)
{
  if ((quality>1) && (nr_cols == 2))
    return make_two_column (start, end, flb);
  // cout << "Make multicolumn "
  //      << start << " " << brk[start] << ", "
  //      << end   << " " << brk[end  ] << LF << LF << INDENT;

  skeleton sk;
  int col, col_start= start, col_end= start;
  int mcid= flow_id[flb * nr_cols];
  page_item item= access (l, flow[mcid][brk[end][mcid]-1]);
  SI col_ht= brk_tot[end]->def - brk_tot[start]->def;
  col_ht -= item->spc->def/(nr_cols-1);
  // already divided by nr_cols
  // cout << "Column height= " << col_ht << LF;
  for (col=0; col<nr_cols; col++) {
    col_end= col_start;
    // avoids bug: a bizar change in col_end occurs between end of loop and
    //             the start of the loop at the next iteration
    if (col_end >= end) break;
    SI tot= brk_tot[col_start]->def + (col_ht / nr_cols);
    int col_end= find_length (brk_tot, col_start, tot);
    col_end= max (col_start+1, col_end-2);
    if (col == nr_cols-1) col_end= end;
    while (col_end < end) {
      if (correct_pagelet (col_start, col_end) &&
	  correct_pagelet (col_end, end))
	{
	  pagelet pg= make_pagelet (col_start, col_end, flb, nr_cols);
	  if ((pg->pen < vpenalty (HYPH_INVALID)) &&
	      (pg->ht->def >= col_ht))
	    {
	      if (N(pg->ins) != 0) sk << pg;
	      break;
	    }
	}
      col_end++;
    }
    if (col_end == end) {
      pagelet pg= make_pagelet (col_start, col_end, flb, nr_cols);
      if (N(pg->ins) != 0) sk << pg;
    }
    col_start= col_end;
  }

  // cout << UNINDENT << "Multicolumn: " << sk << LF;
  return make_multi_column (sk, nr_cols);
}

int
page_breaker_rep::tc_propose_break (path flb) {
  if (!correct_pagelet (tc_start, tc_middle)) return INVALID_BREAK;
  if (!correct_pagelet (tc_middle, tc_end)) return INVALID_BREAK;
  pagelet pg1= make_pagelet (tc_start, tc_middle, flb, 2);
  pagelet pg2= make_pagelet (tc_middle, tc_end, flb, 2);
  bool first_longer = (pg1->ht->min > pg2->ht->max);
  bool second_longer= (pg2->ht->min > pg1->ht->max);

  vpenalty pen= pg1->pen + pg2->pen + as_vpenalty (pg2->ht->def - pg1->ht->def);
  if (first_longer || second_longer) pen += UNBALANCED_COLUMNS;
  if (second_longer) pen += LONGER_LATTER_COLUMN;
  if (is_nil (tc_bpg1) || (pen < tc_bpen)) {
    tc_bmid= tc_middle;
    tc_bpen= pen;
    tc_bpg1= pg1;
    tc_bpg2= pg2;
  }
  if (first_longer && (tc_middle >= tc_ref)) return BAD_BREAK;
  if (second_longer && (tc_middle < tc_ref)) return BAD_BREAK;
  return VALID_BREAK;
}

insertion
page_breaker_rep::make_two_column (int start, int end, path flb) {
  // cout << "Make two column "
  //      << start << " " << brk[start] << ", "
  //      << end   << " " << brk[end  ] << LF << LF << INDENT;

  skeleton sk;
  int mcid= flow_id[flb * 2];
  page_item item= access (l, flow[mcid][brk[end][mcid]-1]);
  SI col_ht= brk_tot[end]->def - brk_tot[start]->def - item->spc->def;
  // already divided by 2
  // cout << "Column height= " << col_ht << LF;
  SI tot= brk_tot[start]->def + (col_ht / 2);
  tc_ref= find_length (brk_tot, start, tot);

  tc_start= start;
  tc_end  = end;
  tc_bmid = end;
  tc_bpen = vpenalty (MAX_SI, MAX_SI);
  tc_bpg1 = pagelet ();
  tc_bpg2 = pagelet ();

  tc_ld   = ladder ();
  if ((tc_ref > tc_start) && (tc_ref < tc_end)) tc_ld << tc_ref;
  while (N(tc_ld) != 0) {
    // cout << "Two column ladder= " << tc_ld << LF;
    tc_middle= tc_ld[0];
    int status= tc_propose_break (flb);
    if ((status == BAD_BREAK) && (tc_bpen < vpenalty (HYPH_INVALID)))
      tc_ld= sub (tc_ld, 1, N(tc_ld));
    else {
      if (tc_middle+1 >= tc_end) tc_ld= ladder ();
      else tc_ld= inc_merge (brk_next[tc_middle+1], sub (tc_ld, 1, N(tc_ld)));
    }
  }

  tc_ld= ladder ();
  if (((tc_ref-1) > tc_start) && ((tc_ref-1) < tc_end)) tc_ld << (tc_ref-1);
  while (N(tc_ld) != 0) {
    // cout << "Two column ladder= " << tc_ld << LF;
    tc_middle= tc_ld[0];
    int status= tc_propose_break (flb);
    if ((status == BAD_BREAK) && (tc_bpen < vpenalty (HYPH_INVALID)))
      tc_ld= sub (tc_ld, 1, N(tc_ld));
    else {
      if (tc_middle-1 <= tc_start) tc_ld= ladder ();
      else tc_ld= dec_merge (brk_prev[tc_middle-1], sub (tc_ld, 1, N(tc_ld)));
    }
  }

  if (is_nil (tc_bpg1))
    sk << make_pagelet (tc_start, tc_end, flb, 2);
  else {
    sk << tc_bpg1;
    sk << tc_bpg2;
  }

  // cout << UNINDENT << "Two column: " << sk << LF;
  return make_multi_column (sk, 2);
}

/******************************************************************************
* Fast page breaking routines
******************************************************************************/

void
page_breaker_rep::fast_break_page (int i1, int& first_end) {
  first_end= max (i1+1, first_end);
  bool ok= false;
  int i2= first_end, n= N(flow[0]);
  while (true) {
    space spc;
    if (i1 == 0) { if (i2 > 1) spc= copy (flow_tot[0][i2-2]); }
    else spc= flow_tot[0][i2-2] - flow_tot[0][i1-1];
    SI top_cor= flow_cor[0][i1]->max;
    SI bot_cor= flow_cor[0][i2-1]->min;
    spc += space (top_cor + flow_cor[0][i2-1]->def + bot_cor);
    
    int bpen= access (l, flow[0][i2-1])->penalty;
    if (i2 >= n) bpen= 0;
    if (bpen < HYPH_INVALID) {
      ok= true;
      if (spc->max < height->min) first_end= i2;
      vpenalty pen= best_pens[i1] + vpenalty (bpen);
      if ((i2 < n) || (!last_page_flag))
	pen += as_vpenalty (spc->def - height->def);
      if (((i2 < n) || (!last_page_flag)) && (spc->max < height->def)) {
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
      if (pen < best_pens[i2]) {
	best_prev[i2]= i1;
	best_pens[i2]= pen;
      }
    }
    if ((i2 >= n) || (ok && (spc->min > height->max))) break;
    i2++;
  }
}

void
page_breaker_rep::fast_assemble_skeleton (skeleton& sk, int end) {
  int start= best_prev[end], n= N(flow[0]);
  if (start < 0) return;
  fast_assemble_skeleton (sk, start);
  insertion ins= make_insertion (0, -1, start, end, end == n);
  pagelet pg (0);
  pg << ins;
  bool last_page= last_page_flag && (end == n);
  format_pagelet (pg, height, last_page);
  sk << pg;
}

void
page_breaker_rep::fast_assemble_skeleton (skeleton& sk) {
  int i, n= N(flow[0]);
  best_prev= array<int> (n+1);
  best_pens= array<vpenalty> (n+1);
  for (i=0; i<=n; i++) {
    best_prev [i]= -1;
    best_pens [i]= HYPH_INVALID;
  }
  best_prev[0]= -2;
  best_pens[0]= vpenalty (0, 0);
  
  int first_end= 0;
  for (i=0; i<n; i++)
    if (best_prev[i] != -1)
      fast_break_page (i, first_end);
  fast_assemble_skeleton (sk, n);
}

/******************************************************************************
* Page breaking routines
******************************************************************************/

int
page_breaker_rep::propose_break () {
  if (!correct_pagelet (cur_start, cur_end)) return INVALID_BREAK;
  pagelet pg= make_pagelet (cur_start, cur_end, path (), 1);

  /*
    bool newpage=
    (l[ins->end->item-1]->type == PAGE_CONTROL_ITEM) &&
    (l[ins->end->item-1]->t == NEW_PAGE);
  */
  bool last_page= last_page_flag && (cur_end == brk_last);
  vpenalty pen= format_pagelet (pg, height, last_page);
  if (is_nil (best_pg) || (pen < best_pen)) {
    best_end= cur_end;
    best_pen= pen;
    best_pg = pg;
  }

  if (quality>0) {
    vpenalty tot_pen= pen + best_pens[cur_start];
    if (is_nil (best_pgs[cur_end]) || (tot_pen < best_pens[cur_end])) {
      best_prev[cur_end]= cur_start;
      best_pens[cur_end]= tot_pen;
      best_pgs [cur_end]= pg;
    }
  }

  if (last_page && (pg->ht->def <= height->def)) return VALID_BREAK;
  if ((height->max < pg->ht->min) && (cur_end >= cur_ref)) return BAD_BREAK;
  if ((height->min > pg->ht->max) && (cur_end <  cur_ref)) return BAD_BREAK;
  return VALID_BREAK;
}

void
page_breaker_rep::find_next_breaks () {
  SI tot  = brk_tot[cur_start]->def + height->def;
  cur_ref = find_length (brk_tot, cur_start, tot);
  best_end= brk_last;
  best_pen= vpenalty (MAX_SI, MAX_SI);
  best_pg = pagelet ();

  // int credit= 10;
  cur_ld= ladder ();
  cur_ld << cur_ref;
  while (N(cur_ld) != 0) {
    // if ((--credit) <= 0) break;
    // cout << "Current ladder= " << cur_ld << LF;
    cur_end= cur_ld[0];
    int status= propose_break ();
    if ((status == BAD_BREAK) && (best_pen < vpenalty (HYPH_INVALID)))
      cur_ld= sub (cur_ld, 1, N(cur_ld));
    else {
      if (cur_end+1 >= N(brk)) cur_ld= ladder ();
      else cur_ld= inc_merge (brk_next[cur_end+1], sub (cur_ld, 1, N(cur_ld)));
    }
  }

  // credit= 10;
  cur_ld= ladder ();
  if (cur_ref-1 > cur_start) cur_ld << (cur_ref-1);
  while (N(cur_ld) != 0) {
    // if ((--credit) <= 0) break;
    // cout << "Current ladder= " << cur_ld << LF;
    cur_end= cur_ld[0];
    int status= propose_break ();
    if ((status == BAD_BREAK) && (best_pen < vpenalty (HYPH_INVALID)))
      cur_ld= sub (cur_ld, 1, N(cur_ld));
    else {
      if (cur_end-1 <= cur_start) cur_ld= ladder ();
      else cur_ld= dec_merge (brk_prev[cur_end-1], sub (cur_ld, 1, N(cur_ld)));
    }
  }
}

void
page_breaker_rep::assemble_skeleton (skeleton& sk, int last) {
  // cout << "Assemble until " << last << LF;
  if (last == brk_first) return;
  ASSERT (best_prev[last] != -1, "unfinished skeleton");
  assemble_skeleton (sk, best_prev[last]);
  sk << best_pgs[last];
}

void
page_breaker_rep::assemble_skeleton (skeleton& sk) {
  int i, nrb= N(brk), nrinit= (quality>0? nrb: 0);
  best_prev= array<int>      (nrinit);
  best_pens= array<vpenalty> (nrinit);
  best_pgs = array<pagelet>  (nrinit);
  for (i=0; i<nrinit; i++) {
    best_prev[i]= -1;
    best_pens[i]= vpenalty (MAX_SI, MAX_SI);
    best_pgs [i]= pagelet ();
  }
  if (quality>0) {
    best_prev[brk_first]= -2;
    best_pens[brk_first]= vpenalty (0, 0);
  }

  cur_start= brk_first;
  if (quality>0) {
    while (cur_start != brk_last) {
      if (best_prev[cur_start] != -1)
	find_next_breaks ();
      // cout << HRULE << LF << LF;
      cur_start++;
    }
    assemble_skeleton (sk, brk_last);
  }
  else {
    while (cur_start != brk_last) {
      find_next_breaks ();
      // cout << HRULE << LF;
      // cout << "Eject " << best_pg << LF;
      // cout << HRULE << LF << LF;
      sk << best_pg;
      cur_start= best_end;
    }
  }
}

void
page_breaker_rep::assemble_skeleton (skeleton& sk, int start, int end) {
  // cout << "Building skeleton " << start << " -- " << end << "\n";
  init_flows (start, end);
  // cout << "Flows done" << LF;
  // cout << "nr_flows = " << nr_flows << LF;
  // cout << "flow_id  = " << flow_id << LF;
  // cout << "flow_fl  = " << flow_fl << LF;
  // cout << "flow     = " << flow << LF;
  // cout << "flow_ht  = " << flow_ht << LF;
  // cout << "flow_cor = " << flow_cor << LF;
  // cout << "flow_tot = " << flow_tot << LF;
  // cout << "flow_cont= " << flow_cont << LF;
  // show_penalties ();
  if ((nr_flows == 1) && (flow_fl[0] == path (1))) {
    fast_assemble_skeleton (sk);
    // cout << "Skeleton done" << LF;
    // cout << "sk= " << sk << LF;
    return;
  }
  init_breaks ();
  // cout << "Breaks done" << LF;
  // cout << "brk      = " << brk << LF;
  // cout << "brk_tot  = " << brk_tot << LF;
  sort_breaks ();
  // cout << "Sorting done" << LF;
  // cout << "brk      = " << brk << LF;
  // cout << "brk_tot  = " << brk_tot << LF;
  init_ladders ();
  // cout << "Ladders done" << LF;
  // cout << "brk_prev = " << brk_prev << LF;
  // cout << "brk_next = " << brk_next << LF;
  // cout << "brk_first= " << brk_first << LF;
  // cout << "brk_last = " << brk_last << LF;
  assemble_skeleton (sk);
  // cout << "Skeleton done" << LF;
  // cout << "sk= " << sk << LF;
  // cout << HRULE << LF << LF;
}

skeleton
page_breaker_rep::make_skeleton () {
  skeleton sk;
  int i, j, n= N(l);
  bool dpage_flag= false;
  int page_offset= first_page - 1;
  for (i=0, j=0; j<n; j++) {
    if ((!papyrus_mode) && (l[j]->type == PAGE_CONTROL_ITEM)) {
      if ((l[j]->t == PAGE_BREAK) ||
	  (l[j]->t == NEW_PAGE) || (l[j]->t == NEW_DPAGE))
	{
	  if (dpage_flag && ((N(sk) + page_offset) & 1) == 1)
	    sk << pagelet (space (0));
	  dpage_flag= (l[j]->t == NEW_DPAGE);
	  last_page_flag= (l[j]->t != PAGE_BREAK);
	  if (i<j) assemble_skeleton (sk, i, j);
	  i=j+1;
	}
      else if (is_tuple (l[j]->t, "env_page") && l[j]->t[1] == PAGE_NR)
        page_offset= as_int (l[j]->t[2]->label) - N(sk) - 1;
    }
  }
  if (i<j) {
    if (dpage_flag && ((N(sk)&1) == 1))
      sk << pagelet (space (0));
    last_page_flag= true;
    assemble_skeleton (sk, i, j);
  }
  return sk;
}

/******************************************************************************
* The exported page breaking routine
******************************************************************************/

skeleton new_break_pages (array<page_item> l, space ph, int qual,
                          space fn_sep, space fnote_sep, space float_sep,
                          font fn, int first_page);

skeleton
break_pages (array<page_item> l, space ph, int qual,
	     space fn_sep, space fnote_sep, space float_sep,
             font fn, int first_page)
{
  if (get_user_preference ("new style page breaking") != "off")
    return new_break_pages (l, ph, qual, fn_sep, fnote_sep, float_sep,
                            fn, first_page);
  else {
    page_breaker_rep* H=
      tm_new<page_breaker_rep> (l, ph, qual, fn_sep, fnote_sep, float_sep,
                                fn, first_page);
    // cout << HRULE << LF;
    skeleton sk= H->make_skeleton ();
    tm_delete (H);
    return sk;
  }
}
