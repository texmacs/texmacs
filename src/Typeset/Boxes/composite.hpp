
/******************************************************************************
* MODULE     : composite.hpp
* DESCRIPTION: composite boxes
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef COMPOSITE_H
#define COMPOSITE_H
#include "boxes.hpp"
#include "array.hpp"

/******************************************************************************
* Composite boxes
******************************************************************************/

struct composite_box_rep: public box_rep {
  array<box> bs;  // the children
  path lip, rip;  // left-most and right-most inverse paths

  composite_box_rep (path ip);
  composite_box_rep (path ip, array<box> bs);
  composite_box_rep (path ip, array<box> bs, bool init_sx_sy);
  composite_box_rep (path ip, array<box> bs, array<SI> x, array<SI> y);
  ~composite_box_rep ();

  void    insert (box b, SI x, SI y);
  void    position ();
  void    left_justify ();
  void    finalize ();

  int     subnr ();
  box     subbox (int i);
  void    display (renderer ren);

  virtual int             find_child (SI x, SI y, SI delta, bool force);
  virtual path            find_box_path (SI x, SI y, SI delta,
                                         bool force, bool & found);
  virtual path            find_lip ();
  virtual path            find_rip ();
  virtual path            find_box_path (path p, bool& found);
  virtual path            find_tree_path (path bp);
  virtual cursor          find_cursor (path bp);
  virtual selection       find_selection (path lbp, path rbp);
  virtual gr_selections   graphical_select (SI x, SI y, SI dist);
  virtual gr_selections   graphical_select (SI x1, SI y1, SI x2, SI y2);

  virtual tree message (tree t, SI x, SI y, rectangles& rs);
  virtual void loci (SI x, SI y, SI delta, list<string>& ids, rectangles& rs);
  virtual bool access_allowed ();
  virtual void collect_page_numbers (hashmap<string,tree>& h, tree page);
  virtual path find_tag (string name);

  virtual box  transform (frame fr);
};

struct concrete_composite_box_rep: public composite_box_rep {
  bool border_flag;
  concrete_composite_box_rep (
    path ip, array<box> bs, array<SI> x, array<SI> y, bool bfl):
      composite_box_rep (ip, bs, x, y), border_flag (bfl) { finalize (); }
  concrete_composite_box_rep (path ip, array<box> bs, bool bfl):
    composite_box_rep (ip, bs, true), border_flag (bfl) { finalize (); }
  operator tree () { return tree ("composite"); }
  int find_child (SI x, SI y, SI delta, bool force);
};

#endif // defined COMPOSITE_H
