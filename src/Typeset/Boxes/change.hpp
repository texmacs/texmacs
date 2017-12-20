
/******************************************************************************
* MODULE     : change.hpp
* DESCRIPTION: boxes with changed behaviour
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef CHANGE_H
#define CHANGE_H
#include "Boxes/composite.hpp"

struct change_box_rep: public composite_box_rep {
  bool child_flag, big_flag;
  change_box_rep (path ip, bool child_flag, bool big_flag= false);
  operator tree ();
  int find_child (SI x, SI y, SI delta, bool force);
  path find_left_box_path ();
  path find_right_box_path ();

  double left_slope ();
  double right_slope ();
  SI left_correction ();
  SI right_correction ();
  SI lsub_correction ();
  SI lsup_correction ();
  SI rsub_correction ();
  SI rsup_correction ();
  SI sub_lo_base (int l);
  SI sub_hi_lim  (int l);
  SI sup_lo_lim  (int l);
  SI sup_lo_base (int l);
  SI sup_hi_lim  (int l);
  SI wide_correction (int mode);
  void get_bracket_extents (SI& lo, SI& hi);
  SI get_leaf_offset (string search);
  gr_selections graphical_select (SI x, SI y, SI dist);
  gr_selections graphical_select (SI x1, SI y1, SI x2, SI y2);
};

#endif // defined CHANGE_H
