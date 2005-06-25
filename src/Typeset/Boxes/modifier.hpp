
/******************************************************************************
* MODULE     : modifier.hpp
* DESCRIPTION: a modifier box modifies the behaviour of another box
*              as to cursor movements etc. , but displays in the same way
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
  void      display (ps_device dev);  
  operator  tree ();
  tree      action (tree t, SI x, SI y, SI delta);
  void      collect_page_numbers (hashmap<string,tree>& h, tree page);
  path      find_tag (string name);

  virtual path            find_box_path (SI x, SI y, SI delta, bool force);
  virtual path            find_lip ();
  virtual path            find_rip ();
  virtual path            find_left_box_path ();
  virtual path            find_right_box_path ();
  virtual path            find_box_path (path p, bool& found);
  virtual path            find_tree_path (path bp);
  virtual cursor          find_cursor (path bp);
  virtual selection       find_selection (path lbp, path rbp);
  virtual gr_selections   graphical_select (SI x, SI y, SI dist);

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

  virtual int   anim_length ();
  virtual bool  anim_started ();
  virtual bool  anim_finished ();
  virtual void  anim_start_at (time_t at);
  virtual void  anim_finish_now ();
  virtual void  anim_get_invalid (bool& flag, time_t& at, rectangles& rs);
};

#endif // defined MODIFIER_H
