
/******************************************************************************
* MODULE     : modifier.hpp
* DESCRIPTION: a modifier box modifies the behaviour of another box
*              as to cursor movements etc. , but displays in the same way
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef MODIFIER_H
#define MODIFIER_H
#include "boxes.hpp"

class modifier_box_rep: public box_rep {
public:
  box b;

  modifier_box_rep (path ip, box b);
  ~modifier_box_rep ();

  int       subnr ();
  box       subbox (int i);
  void      display (renderer ren);  
  operator  tree ();
  tree      action (tree t, SI x, SI y, SI delta);
  void      loci (SI x, SI y, SI delta, list<string>& ids, rectangles& rs);
  void      collect_page_numbers (hashmap<string,tree>& h, tree page);
  path      find_tag (string name);

  virtual path            find_box_path (SI x, SI y, SI delta,
                                         bool force, bool & found);
  virtual path            find_lip ();
  virtual path            find_rip ();
  virtual path            find_left_box_path ();
  virtual path            find_right_box_path ();
  virtual path            find_box_path (path p, bool& found);
  virtual path            find_tree_path (path bp);
  virtual cursor          find_cursor (path bp);
  virtual selection       find_selection (path lbp, path rbp);
  virtual gr_selections   graphical_select (SI x, SI y, SI dist);
  virtual gr_selections   graphical_select (SI x1, SI y1, SI x2, SI y2);

  double    left_slope ();
  double    right_slope ();
  SI        left_correction ();
  SI        right_correction ();
  SI        lsub_correction ();
  SI        lsup_correction ();
  SI        rsub_correction ();
  SI        rsup_correction ();
  SI        sub_lo_base (int level);
  SI        sub_hi_lim  (int level);
  SI        sup_lo_lim  (int level);
  SI        sup_lo_base (int level);
  SI        sup_hi_lim  (int level);
  SI        wide_correction (int mode);
  void      get_bracket_extents (SI& lo, SI& hi);

  virtual player     anim_player ();
  virtual double     anim_delay ();
  virtual double     anim_duration ();
  virtual void       anim_position (double delay);
  virtual double     anim_next ();
  virtual rectangles anim_invalid ();
};

#endif // defined MODIFIER_H
