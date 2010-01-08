
/******************************************************************************
* MODULE     : skeleton.hpp
* DESCRIPTION: Line breaking facility for paragraphs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SKELETON_H
#define SKELETON_H
#include "Page/vpenalty.hpp"
#include "tree.hpp"
#include "space.hpp"
#include "path.hpp"

/******************************************************************************
* Abstract definitions of skeletons, pagelets and insertions
******************************************************************************/

struct pagelet;
struct insertion_rep;
struct insertion {
  CONCRETE(insertion);
  inline insertion ();
  inline insertion (tree type, path begin, path end);
  inline insertion (tree type, array<pagelet> sk);
  friend bool operator == (insertion ins1, insertion ins2);
  friend bool operator != (insertion ins1, insertion ins2);
  friend tm_ostream& operator << (tm_ostream& out, insertion ins);
};
struct pagelet_rep: concrete_struct {
  array<insertion> ins;
  space            ht;
  vpenalty         pen;
  double           stretch;

  inline pagelet_rep (space ht2);
};

struct pagelet {
  CONCRETE_NULL(pagelet);
  inline pagelet (space ht);
  void operator << (insertion ins);
  void operator << (space ht);
  friend bool operator == (pagelet pg1, pagelet pg2);
  friend bool operator != (pagelet pg1, pagelet pg2);
  friend tm_ostream& operator << (tm_ostream& out, pagelet pg);
};
typedef array<pagelet> skeleton;

/******************************************************************************
* Code for insertions
******************************************************************************/

struct insertion_rep: concrete_struct {
  tree      type;     // type of insertion
  path      begin;    // begin location in array of page_items
  path      end;      // end location in array of page_items
  skeleton  sk;       // or possible subpagelets (used for multicolumns)
  space     ht;       // height of pagelet
  vpenalty  pen;      // penalty associated to pagelet
  double    stretch;  // between -1 and 1 for determining final height
  SI        top_cor;  // top correction
  SI        bot_cor;  // bottom correction

  inline insertion_rep () {}
  inline insertion_rep (tree type2, path begin2, path end2):
    type (type2), begin (begin2), end (end2) {}
  insertion_rep (tree type, skeleton sk);
};
CONCRETE_CODE(insertion);

inline
insertion::insertion () {
  rep= tm_new<insertion_rep> ();
}

inline
insertion::insertion (tree type, path begin, path end) {
  rep= tm_new<insertion_rep> (type, begin, end);
}

inline
insertion::insertion (tree type, skeleton sk) {
  rep= tm_new<insertion_rep> (type, sk);
}

/******************************************************************************
* Code for pagelets
******************************************************************************/

CONCRETE_NULL_CODE(pagelet);

inline pagelet_rep::pagelet_rep (space ht2): ht (ht2) {}

inline
pagelet::pagelet (space ht) {
  rep= tm_new<pagelet_rep> (ht);
}

inline void
pagelet::operator << (insertion ins) {
  rep->ht  += ins->ht;
  rep->pen += ins->pen;
  rep->ins << ins;
}

inline void
pagelet::operator << (space spc) {
  rep->ht += spc;
}

bool operator == (pagelet pg1, pagelet pg2);
bool operator != (pagelet pg1, pagelet pg2);
tm_ostream& operator << (tm_ostream& out, pagelet pg);

#endif // defined SKELETON_H
