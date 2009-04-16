
/******************************************************************************
* MODULE     : bridge_docrange.cpp
* DESCRIPTION: Bridge between logical and physically typesetted document
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This file contains a VERY HACKY way to accelerate the editing of
* document nodes with many (> 32) children. It should be noticed that
* the notification call-back methods do not behave as usual: the st tree
* remains the st tree of the entire document tree and brs stands for
* a reference to the corresponding bridges. The bridge_docrange structure
* only behaves as usual for the my_exec_until, my_typeset_will_be_complete
* and my_typeset methods.
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bridge.hpp"

#define ACC_THRESHOLD 32

/******************************************************************************
* The docrange bridge structure
******************************************************************************/

class bridge_docrange_rep: public bridge_rep {
protected:
  array<bridge> acc;
  array<bridge>& brs;
  array<int> mid;
  int begin, end;
  bool divide;

public:
  bridge_docrange_rep (typesetter ttt, tree st, path ip, array<bridge>& brs,
		       int begin, int end, bool divide);
  void rebalance ();

  void notify_assign (path p, tree u);
  void notify_insert (path p, tree u);
  void notify_remove (path p, int nr);
  bool notify_macro  (int type, string var, int l, path p, tree u);
  void notify_change ();

  void my_exec_until (path p);
  bool my_typeset_will_be_complete ();
  void my_typeset (int desired_status);
};

bridge
bridge_docrange (typesetter ttt, tree st, path ip,
		 array<bridge>& brs, int begin, int end, bool divide= false)
{
  // cout << "Make range: " << begin << " -- " << end << "\n";
  return tm_new<bridge_docrange_rep> (ttt, st, ip, brs, begin, end, divide);
}

bridge_docrange_rep::bridge_docrange_rep (
  typesetter ttt, tree st, path ip, array<bridge>& brs2,
  int begin2, int end2, bool divide2):
    bridge_rep (ttt, st, ip), brs (brs2),
    begin (begin2), end (end2), divide (divide2)
{
  if (divide) {
    int i, n= ((end - begin - 1) / ACC_THRESHOLD) + 1;
    acc= array<bridge> (n);
    mid= array<int> (n+1);
    for (i=0; i<n; i++) {
      mid[i]  = begin + i * ACC_THRESHOLD;
      mid[i+1]= min (mid[i] + ACC_THRESHOLD, end);
      acc[i]  = bridge_docrange (ttt, st, ip, brs, mid[i], mid[i+1]);
    }
  }
}

void
bridge_docrange_rep::rebalance () {
  int i, n= N(acc);
  array<bridge> acc2;
  array<int>    mid2;

  for (i=0; i<n; i++) {
    // Compactify?
    if ((i < n-1) && ((mid[i+2] == mid[i+1]) ||
		      (mid[i+2] - mid[i] <= ACC_THRESHOLD)))
      {
	int start= i;
	while ((i < n-1) && ((mid[i+2] == mid[start+1]) ||
			     (mid[i+2] - mid[start] <= ACC_THRESHOLD))) i++;
	// cout << "  Compactify " << i-start << " at " << start << ", ";
	// if (mid[i+1] == mid[start+1]) cout << "suppress\n";
	// else cout << "compress\n";
	if (mid[i+1] == mid[start+1]) acc2 << acc[start];
	else acc2 << bridge_docrange (ttt, st, ip, brs, mid[start], mid[i+1]);
	mid2 << mid[start];
      }
    
    // Expand?
    else if (mid[i+1] - mid[i] > (7 * ACC_THRESHOLD / 4)) {
      int j, k= (mid[i+1] - mid[i] - 1) / ACC_THRESHOLD + 1;
      // cout << "  Expand " << k << " at " << i << "\n";
      for (j=0; j<k; j++) {
	int b= mid[i] + j * ACC_THRESHOLD;
	int e= min (b + ACC_THRESHOLD, mid[i+1]);
	acc2 << bridge_docrange (ttt, st, ip, brs, b, e);
	mid2 << b;
      }
    }
    
    // OK
    else {
      acc2 << acc[i];
      mid2 << mid[i];
    }
  }

  mid2 << end;
  // if (mid2 != mid) cout << mid << " -> " << mid2 << "\n";
  acc= acc2;
  mid= mid2;
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_docrange_rep::notify_assign (path p, tree u) {
  ASSERT (!is_nil (p), "erroneous nil path");
  if (divide) {
    int i, n= N(acc);
    for (i=0; i<n; i++)
      if ((p->item >= mid[i]) && (p->item < mid[i+1]))
	acc[i]->notify_assign (p, u);
  }
  status= CORRUPTED;
}

void
bridge_docrange_rep::notify_insert (path p, tree u) {
  // cout << "Notify insert " << p << ", " << N(u)
  //      << " [ " << begin << "--" << end << " ]\n";
  ASSERT (!is_nil (p), "erroneous nil path");
  if (p->item > end) {
    cerr << "\nNotify insert " << u << " at " << p << "\n";
    FAILED ("out of range");
  }
  if (p->item >= begin) status= CORRUPTED;
  else begin += N(u);
  end += N(u);

  if (divide) {
    int i, n= N(acc);
    for (i=0; i<n; i++)
      if (p->item < mid[i+1])
	break;
    if (i==n) i--;
    for (; i<n; i++) {
      acc[i]->notify_insert (p, u);
      mid[i+1] += N(u);
    }
    // cout << "mid[ins,0]= " << mid << "\n";
    rebalance ();
    // cout << "mid[ins,1]= " << mid << "\n";
  }
}

void
bridge_docrange_rep::notify_remove (path p, int nr) {
  // cout << "Notify insert " << p << ", " << nr
  //      << " [ " << begin << "--" << end << " ]\n";
  ASSERT (!is_nil (p), "erroneous nil path");
  ASSERT (p->item < end, "out of range");
  if (p->item + nr > begin) {
    status= CORRUPTED;
    begin= min (begin , p->item);
    end  = max (end-nr, p->item);
  }
  else {
    begin -= nr;
    end   -= nr;
  }

  if (divide) {
    int i, n= N(acc);
    for (i=0; i<n; i++)
      if (p->item < mid[i+1])
	break;
    for (; i<n; i++) {
      acc[i]->notify_remove (p, nr);
      mid[i+1]= max (mid[i+1]-nr, p->item);
    }
    // cout << "mid[rem,0]= " << mid << "\n";
    rebalance ();
    // cout << "mid[rem,1]= " << mid << "\n";
  }
}

bool
bridge_docrange_rep::notify_macro (int type, string v, int l, path p, tree u) {
  (void) type; (void) v; (void) l; (void) p; (void) u;
  FAILED ("method should never be called");
  return false;
}

void
bridge_docrange_rep::notify_change () {
  status= CORRUPTED;
  if (divide) {
    acc[0]->notify_change ();
    if (N(acc)>1) acc[N(acc)-1]->notify_change ();
  }
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_docrange_rep::my_exec_until (path p) {
  if (p->item < begin);
  else if ((p->item >= end) && ((status & VALID_MASK) == PROCESSED))
    env->patch_env (changes);
  else if (divide) {
    int i, n= N(acc);
    for (i=0; i<n; i++)
      acc[i]->my_exec_until (p);
  }
  else {
    int i;
    for (i=begin; i<p->item; i++)
      brs[i]->exec_until (path (right_index (brs[i]->st)), true);
    if (p->item < end) brs[i]->exec_until (p->next);
  }
}

bool
bridge_docrange_rep::my_typeset_will_be_complete () {
  int i, n= N(acc);
  if (divide)
    for (i=0; i<n; i++)
      if (!acc[i]->my_typeset_will_be_complete ()) return false;
  else
    for (i=begin; i<end; i++)
      if (!brs[i]->my_typeset_will_be_complete ()) return false;
  return true;
}

// static bool top_level= true;

void
bridge_docrange_rep::my_typeset (int desired_status) {
  int i, n= N(acc);
  array<line_item> a= ttt->a;
  array<line_item> b= ttt->b;
  if (divide) {
    for (i=0; i<n; i++) {
      int wanted= (i==n-1? desired_status & WANTED_MASK: WANTED_PARAGRAPH);
      ttt->a= (i==0  ? a: array<line_item> ());
      ttt->b= (i==n-1? b: array<line_item> ());
      acc[i]->typeset (PROCESSED+ wanted);
    }
  }
  else {
    //bool show= top_level;
    //bool old_top_level= top_level;
    //top_level= false;
    //if (show) cout << "Typeset range: " << begin << " -- " << end << "\n  ";
    for (i=begin; i<end; i++) {
      int wanted= (i==end-1? desired_status & WANTED_MASK: WANTED_PARAGRAPH);
      ttt->a= (i==0    ? a: array<line_item> ());
      ttt->b= (i==end-1? b: array<line_item> ());
      //if (show) cout << (brs[i]->status == PROCESSED + wanted);
      //if (show) cout.flush ();
      brs[i]->typeset (PROCESSED+ wanted);
    }
    //if (show) cout << "\n";
    //top_level= old_top_level;
  }
}
