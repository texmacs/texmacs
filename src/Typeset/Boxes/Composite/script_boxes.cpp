
/******************************************************************************
* MODULE     : script.cpp
* DESCRIPTION: Script and limit boxes
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Boxes/composite.hpp"
#include "Boxes/Composite/italic_correct.hpp"
#include "Boxes/construct.hpp"

/******************************************************************************
* subroutine for scripts
******************************************************************************/

static void
finalize_left (path& lip, box sb) {
  if (is_accessible (sb->ip)) {
    path new_lip= descend (sb->ip->next, 0);
    if (path_less (reverse (new_lip), reverse (lip))) lip= new_lip;
  }
}

static void
finalize_right (path& rip, box sb) {
  if (is_accessible (sb->ip)) {
    path new_rip= descend (sb->ip->next, 1);
    if (path_less (reverse (rip), reverse (new_rip))) rip= new_rip;
  }
}

static path
descend_script (path ip1, int side) {
  if (is_accessible (ip1)) ip1= ip1->next;
  return descend_decode (ip1, side);
}

static path
descend_script (path ip1, path ip2, int side) {
  int i=1;
  if (is_accessible (ip2)) {
    if (is_decoration (ip1)) i=2;
    else {
      if ((side == 0) && path_inf (reverse (ip2), reverse (ip1))) i=2;
      if ((side == 1) && path_inf (reverse (ip1), reverse (ip2))) i=2;
    }
  }
  if (i==1) return descend_script (ip1, side);
  else return descend_script (ip2, side);
}

static bool
test_script_border (path p, box sb) {
  return
    is_accessible (sb->ip) &&
    (path_up (p) == path_up (reverse (sb->ip)));
}

/******************************************************************************
* limits
******************************************************************************/

struct lim_box_rep: public composite_box_rep {
  box  ref;
  font fn;
  bool glued;
  int  type;
  lim_box_rep (path ip, box ref, box lo, box hi, font fn, bool glued);
  operator tree () { return tree (TUPLE, "lim", bs[0]); }
  void finalize ();
  box adjust_kerning (int mode, double factor);
  box expand_glyphs (int mode, double factor);

  path find_box_path (path p, bool& found);
  path find_tree_path (path bp);
};

lim_box_rep::lim_box_rep (path ip, box r2, box lo, box hi, font fn2, bool gl):
  composite_box_rep (ip), ref (r2), fn (fn2), glued (gl)
{
  SI sep_lo= fn->sep + fn->yshift;
  SI sep_hi= fn->sep + (fn->yshift >> 1);
  SI X, Y;
  insert (ref, 0, 0);
  type= 0;
  if (!is_nil (lo)) type += 1;
  if (!is_nil (hi)) type += 2;
  if (!is_nil (lo)) {
    SI top= max (lo->y2, fn->y2 * script (fn->size, 1) / fn->size) + sep_lo;
    Y= ref->y1;
    X= ((SI) (ref->right_slope ()* (Y+top-lo->y1))) + ((ref->x1+ref->x2)>>1);
    insert (lo, X- (lo->x2 >> 1), Y-top);
    italic_correct (lo);
  }
  if (!is_nil (hi)) {
    SI bot= min (hi->y1, fn->y1 * script (fn->size, 1) / fn->size) - sep_hi;
    Y= ref->y2;
    X= ((SI) (ref->right_slope ()*(Y+hi->y2-bot))) + ((ref->x1+ref->x2)>>1);
    insert (hi, X- (hi->x2 >> 1), Y-bot);
    italic_correct (hi);
  }
  italic_correct (ref);
  position ();
  italic_restore (ref);
  if (!is_nil (lo)) italic_restore (lo);
  if (!is_nil (hi)) italic_restore (hi);
  left_justify ();
  finalize ();
}

void
lim_box_rep::finalize () {
  composite_box_rep::finalize ();
  if (glued) {
    int i, n= subnr ()- 1;
    for (i=1; i<=n; i++) {
      finalize_left  (lip, bs[i]);
      finalize_right (rip, bs[i]);
    }
  }
}

box
lim_box_rep::adjust_kerning (int mode, double factor) {
  box body= bs[0]->adjust_kerning (mode, factor);
  box sub, sup;
  if ((type & 1) != 0) sub= bs[1]->adjust_kerning (mode, factor/2);
  if ((type & 2) != 0) sup= bs[N(bs)-1]->adjust_kerning (mode, factor/2);
  return limit_box (ip, body, sub, sup, fn, glued);
}

box
lim_box_rep::expand_glyphs (int mode, double factor) {
  (void) mode;
  box body= bs[0]->expand_glyphs (0, factor);
  box sub, sup;
  if ((type & 1) != 0) sub= bs[1]->expand_glyphs (0, factor);
  if ((type & 2) != 0) sup= bs[N(bs)-1]->expand_glyphs (0, factor);
  return limit_box (ip, body, sub, sup, fn, glued);
}

path
lim_box_rep::find_box_path (path p, bool& found) {
  if (glued) {
    int nr= subnr () - 1;
    if (((nr >= 1) && test_script_border (p, bs[1])) ||
	((nr == 2) && test_script_border (p, bs[2])))
      {
	found= true;
	if (last_item (p) == 0) return path (0);
	else return path (1);
      }
  }
  return composite_box_rep::find_box_path (p, found);
}

path
lim_box_rep::find_tree_path (path bp) {
  if (glued && is_atom (bp)) {
    int nr= subnr()- 1;
    if (bp->item == 0) {
      if (is_decoration (bs[0]->ip))
	return reverse (descend_script (bs[0]->ip, 0));
      else return bs[0]->find_tree_path (bs[0]->find_left_box_path ());
    }
    else {
      if (nr == 1) return reverse (descend_script (bs[1]->ip, 1));
      else return reverse (descend_script (bs[1]->ip, bs[2]->ip, 1));
    }
  }
  else return composite_box_rep::find_tree_path (bp);
}

/******************************************************************************
* dummy script boxes
******************************************************************************/

struct dummy_script_box_rep: public composite_box_rep {
  font fn;
  int  type;
  dummy_script_box_rep (path ip, box b1, box b2, font fn);
  operator tree () { return "dummy script"; }
  void finalize ();
  box adjust_kerning (int mode, double factor);
  box expand_glyphs (int mode, double factor);

  path      find_box_path (path p, bool& found);
  path      find_tree_path (path bp);
};

dummy_script_box_rep::dummy_script_box_rep (path ip, box b1, box b2, font fn2):
  composite_box_rep (ip), fn (fn2)
{
  SI sep  = fn->sep;
  SI lo_y = fn->ysub_lo_base;
  SI hi_y = fn->ysup_lo_base;
  SI miny2= (fn->y2 - fn->yshift) * script (fn->size, 1) / fn->size;

  type= 0;
  if (!is_nil (b1)) type += 1;
  if (!is_nil (b2)) type += 2;

  if ((!is_nil (b1)) && (!is_nil (b2))) {
    SI y= max (b1->y2, miny2);
    SI d= lo_y + y + sep - hi_y - b2->y1;
    if (d > 0) {
      lo_y -= (d>>1);
      hi_y += (d>>1);
    }
  }
  if (!is_nil (b1)) {
    insert (b1, 0, lo_y);
    italic_correct (b1);
  }
  if (!is_nil (b2)) {
    insert (b2, 0, hi_y);
    italic_correct (b2);
  }
  position ();
  if (!is_nil (b1)) italic_restore (b1);
  if (!is_nil (b2)) italic_restore (b2);
  left_justify ();
  y1= min (y1, fn->ysub_lo_base);
  y2= max (y2, fn->ysup_lo_base + fn->yx);
  finalize ();
}

void
dummy_script_box_rep::finalize () {
  int i, n= subnr ();
  composite_box_rep::finalize ();
  for (i=0; i<n; i++) {
    finalize_left  (lip, bs[i]);
    finalize_right (rip, bs[i]);
  }
}

box
dummy_script_box_rep::adjust_kerning (int mode, double factor) {
  box sub, sup;
  if ((type & 1) != 0) sub= bs[0]->adjust_kerning (mode, factor/2);
  if ((type & 2) != 0) sup= bs[N(bs)-1]->adjust_kerning (mode, factor/2);
  return script_box (ip, sub, sup, fn);
}

box
dummy_script_box_rep::expand_glyphs (int mode, double factor) {
  (void) mode;
  box sub, sup;
  if ((type & 1) != 0) sub= bs[0]->expand_glyphs (0, factor);
  if ((type & 2) != 0) sup= bs[N(bs)-1]->expand_glyphs (0, factor);
  return script_box (ip, sub, sup, fn);
}

path
dummy_script_box_rep::find_box_path (path p, bool& found) {
  //cout << "Find " << p << " in " << operator tree () << "\n";
  int nr= subnr ();
  if (((nr >= 1) && test_script_border (p, bs[0])) ||
      ((nr == 2) && test_script_border (p, bs[1])))
    {
      found= true;
      if (last_item (p) == 0) return path (0);
      else return path (1);
    }
  return composite_box_rep::find_box_path (p, found);
}

path
dummy_script_box_rep::find_tree_path (path bp) {
  if (is_atom (bp)) {
    int nr= subnr();
    if (bp->item == 0) {
      if (nr == 1) return reverse (descend_script (bs[0]->ip, 0));
      else return reverse (descend_script (bs[0]->ip, bs[1]->ip, 0));
    }
    else {
      if (nr == 1) return reverse (descend_script (bs[0]->ip, 1));
      else return reverse (descend_script (bs[0]->ip, bs[1]->ip, 1));
    }
  }
  else return composite_box_rep::find_tree_path (bp);
}

/******************************************************************************
* side boxes
******************************************************************************/

struct side_box_rep: public composite_box_rep {
  short nr_left, nr_right;
  short id_left, id_right;
  font  fn;
  short level, type;
  side_box_rep (path ip, box r, box l1, box l2, box r1, box r2, font f, int l);
  operator tree () {
    int i, n= N(bs);
    tree t (TUPLE, "side");
    for (i=0; i<n; i++) t << ((tree) bs[i]);
    return t;
  } 
  void finalize ();
  box adjust_kerning (int mode, double factor);
  box expand_glyphs (int mode, double factor);

  int       find_child (SI x, SI y, SI delta, bool force);
  path      find_box_path (path p, bool& found);
  path      find_left_box_path ();
  path      find_right_box_path ();
  path      find_tree_path (path bp);
  cursor    find_cursor (path bp);
  selection find_selection (path lbp, path rbp);

  double left_slope () {
    return bs[id_left]->left_slope (); }
  double right_slope () {
    return bs[id_right]->right_slope (); }
  SI left_correction () {
    return max (0, x1 - sx1(id_left) + bs[id_left]->left_correction ()); }
  SI right_correction () {
    return max (0, sx2(id_right) + bs[id_right]->right_correction () - x2); }
  SI lsub_correction () {
    return nr_left==0? bs[0]->lsub_correction (): left_correction (); }
  SI lsup_correction () {
    return nr_left==0? bs[0]->lsup_correction (): left_correction (); }
  SI rsub_correction () {
    return nr_right==0? bs[0]->rsub_correction (): right_correction (); }
  SI rsup_correction () {
    return nr_right==0? bs[0]->rsup_correction (): right_correction (); }
  void get_bracket_extents (SI& lo, SI& hi);
};

side_box_rep::side_box_rep (
  path ip, box ref, box l1, box l2, box r1, box r2, font fn2, int level2):
  composite_box_rep (ip), fn (fn2), level (level2)
{
  insert (ref, 0, 0);

  SI sep= fn->sep;
  SI sub_lo_base= ref->sub_lo_base (level);
  SI sub_hi_lim = ref->sub_hi_lim  (level);
  SI sup_lo_lim = ref->sup_lo_lim  (level);
  SI sup_lo_base= ref->sup_lo_base (level);
  SI sup_hi_lim = ref->sup_hi_lim  (level);
  SI shift      = fn->yshift;
  SI miny2      = (fn->y2 - fn->yshift) * script (fn->size, 1) / fn->size;
  SI lsub= sub_lo_base, lsup= sup_lo_base;
  SI rsub= sub_lo_base, rsup= sup_lo_base;

  type= 0;
  if (!is_nil (l1)) type += 1;
  if (!is_nil (l2)) type += 2;
  if (!is_nil (r1)) type += 4;
  if (!is_nil (r2)) type += 8;

  if (is_nil (l1)) {
    if (is_nil (l2)) nr_left= 0;
    else {
      nr_left= 1;
      lsup= max (sup_hi_lim, ref->y2- (shift<<1)) - l2->y2;
      if (lsup < sup_lo_base) lsup= sup_lo_base;
      if (lsup+ l2->y1 < sup_lo_lim) lsup= sup_lo_lim- l2->y1;
    }
  }
  else {
    SI y= max (l1->y2, miny2);
    if (lsub + y > sub_hi_lim) lsub= sub_hi_lim- y;
    if (is_nil (l2)) nr_left= 1;
    else {
      nr_left= 2;
      lsup= max (sup_hi_lim, ref->y2- (shift<<1)) - l2->y2;
      if (lsup < sup_lo_base) lsup= sup_lo_base;
      if (lsup+ l2->y1 < sup_lo_lim) lsup= sup_lo_lim- l2->y1;
      SI d= lsub + y + sep - lsup - l2->y1;
      if (d > 0) {
	lsub -= (d>>1);
	lsup += (d>>1);
      }
    }
  }

  if (is_nil (r1)) {
    if (is_nil (r2)) nr_right= 0;
    else {
      nr_right= 1;
      rsup= max (sup_hi_lim, ref->y2- (shift<<1)) - r2->y2;
      if (rsup < sup_lo_base) rsup= sup_lo_base;
      if (rsup+ r2->y1 < sup_lo_lim) rsup= sup_lo_lim- r2->y1;
    }
  }
  else {
    SI y= max (r1->y2, miny2);
    if (rsub + y > sub_hi_lim) rsub= sub_hi_lim- y;
    if (is_nil (r2)) nr_right= 1;
    else {
      nr_right= 2;
      rsup= max (sup_hi_lim, ref->y2- (shift<<1)) - r2->y2;
      if (rsup < sup_lo_base) rsup= sup_lo_base;
      if (rsup+ r2->y1 < sup_lo_lim) rsup= sup_lo_lim- r2->y1;
      SI d= rsub + y + sep - rsup - r2->y1;
      if (d > 0) {
	rsub -= (d>>1);
	rsup += (d>>1);
      }
    }
  }

  if (!is_nil (l1)) {
    SI dx= l1->right_correction () + ref->lsub_correction ();
    insert (l1, -l1->x2- dx, lsub);
  }
  if (!is_nil (l2)) {
    SI dx= l2->right_correction () - ref->lsup_correction ();
    insert (l2, -l2->x2- dx, lsup);
  }
  if (!is_nil (r1)) {
    SI dx= r1->left_correction () + ref->rsub_correction ();
    insert (r1, ref->x2+ dx, rsub);
  }
  if (!is_nil (r2)) {
    SI dx= r2->left_correction () + ref->rsup_correction ();
    insert (r2, ref->x2+ dx, rsup);
  }

  position ();
  left_justify ();

  int i;
  id_left= id_right= 0;
  SI lcorr= x1 - sx1(0) + bs[0]->left_correction ();
  SI rcorr= sx2(0) + bs[0]->right_correction () - x2;
  for (i=1; i<=nr_left; i++) {
    SI ltest= x1 - sx1(i) + bs[i]->left_correction ();
    if (ltest > lcorr) { id_left= i; lcorr= ltest; }
  }
  for (; i<=nr_left+nr_right; i++) {
    SI rtest= sx2(i) + bs[i]->right_correction () - x2;
    if (rtest > rcorr) { id_right= i; rcorr= rtest; }
  }

  finalize ();
}

void
side_box_rep::finalize () {
  int i;
  composite_box_rep::finalize ();
  for (i=1; i<=nr_left+nr_right; i++) {
    finalize_left (lip, bs[i]);
    finalize_right (rip, bs[i]);
  }
}

box
side_box_rep::adjust_kerning (int mode, double factor) {
  int smode= mode;
  if (nr_left  > 0) smode= smode & (~START_OF_LINE);
  if (nr_right > 0) smode= smode & (~END_OF_LINE);
  box body= bs[0]->adjust_kerning (smode, factor);
  box lsub, lsup, rsub, rsup;
  if ((type & 1) != 0)
    lsub= bs[1]->adjust_kerning (mode & (~END_OF_LINE), factor/2);
  if ((type & 2) != 0)
    lsup= bs[nr_left]->adjust_kerning (mode & (~END_OF_LINE), factor/2);
  if ((type & 4) != 0)
    rsub= bs[1+nr_left]->adjust_kerning (mode & (~START_OF_LINE), factor/2);
  if ((type & 8) != 0)
    rsup= bs[N(bs)-1]->adjust_kerning (mode & (~START_OF_LINE), factor/2);
  return side_box (ip, body, lsub, lsup, rsub, rsup, fn, level);
}

box
side_box_rep::expand_glyphs (int mode, double factor) {
  (void) mode;
  box body= bs[0]->expand_glyphs (0, factor);
  box lsub, lsup, rsub, rsup;
  if ((type & 1) != 0)
    lsub= bs[1]->expand_glyphs (0, factor);
  if ((type & 2) != 0)
    lsup= bs[nr_left]->expand_glyphs (0, factor);
  if ((type & 4) != 0)
    rsub= bs[1+nr_left]->expand_glyphs (0, factor);
  if ((type & 8) != 0)
    rsup= bs[N(bs)-1]->expand_glyphs (0, factor);
  return side_box (ip, body, lsub, lsup, rsub, rsup, fn, level);
}

int
side_box_rep::find_child (SI x, SI y, SI delta, bool force) {
  if (outside (x, delta, x1, x2)) {
    bool found;
    int side= box_rep::find_box_path (x, y, delta, force, found)->item;
    if (bs[0]->accessible () || force) {
      if ((side == 0) && (nr_left == 0)) return 0;
      if ((side == 1) && (nr_right == 0)) return 0;
    }
    if (is_accessible (ip) || force) return -1;
  }
  
  int i= 0;
  int k= nr_left+ 1;
  SI  xx, yy;

  if (nr_left>0) {
    if (nr_left == 1) xx= sx2(1);
    else xx= max (sx2(1), sx2(2));
    if ((x<xx) || ((x==xx) && (delta<0))) {
      if (bs[1]->accessible () || force) i= 1;
      if (nr_left == 2) {
	yy= (sy2(1) + sy1(2)) >> 1;
	if ((y >= yy) || (bs[1]->decoration () && (!force)))
	  if (bs[2]->accessible () || force) i= 2;
      }
      if (i != 0) return i;
    }
  }

  if (nr_right>0) {
    if (nr_right == 1) xx= sx1(k);
    else xx= min (sx1(k), sx1(k+1));
    if ((x>xx) || ((x==xx) && (delta>=0))) {
      if (bs[k]->accessible () || force) i= k;
      if (nr_right == 2) {
	yy= (sy2(k) + sy1(k+1)) >> 1;
	if ((y >= yy) || (bs[k]->decoration () && (!force)))
	  if (bs[k+1]->accessible () || force) i= k+1;
      }
      if (i != 0) return i;
    }
  }

  if (bs[0]->decoration () && (!force)) {
    for (i=0; i<(nr_left+nr_right+1); i++)
      if (bs[i]->accessible () || force) return i;
    return -1;
  }

  return 0;
}

path
side_box_rep::find_box_path (path p, bool& found) {
  /*
  cout << "Search " << p << " in " << box (this) << " " << ip << "\n";
  cout << "  l:\t" << reverse (lip) << "\n";
  cout << "  r:\t" << reverse (rip) << "\n";
  int i, n= subnr ();
  for (i=0; i<n; i++)
    cout << "  " << i << ":\t" << reverse (bs[i]->ip) << "\n";
  */

  if (((nr_left >= 1) && test_script_border (p, bs[1])) ||
      ((nr_left == 2) && test_script_border (p, bs[2])))
    {
      found= true;
      if (last_item (p) == 0) return path (0);
      else return path (2);
    }
  if (((nr_right >= 1) && test_script_border (p, bs[nr_left+ 1])) ||
      ((nr_right == 2) && test_script_border (p, bs[nr_left+ 2])))
    {
      found= true;
      if (last_item (p) == 1) return path (1);
      else return path (3);
    }
  return composite_box_rep::find_box_path (p, found);
}

path
side_box_rep::find_left_box_path () {
  if (nr_left == 0) return path (0, bs[0]->find_left_box_path ());
  else return path (0);
}

path
side_box_rep::find_right_box_path () {
  if (nr_right == 0) return path (0, bs[0]->find_right_box_path ());
  else return path (1);
}

path
side_box_rep::find_tree_path (path bp) {
  if (is_atom (bp)) {
    switch (bp->item) {
    case 0:
      if (nr_left == 0) {
	if (is_decoration (bs[0]->ip))
	  return reverse (descend_script (bs[0]->ip, 0));
	else return bs[0]->find_tree_path (bs[0]->find_left_box_path ());
      }
      if (nr_left == 1) return reverse (descend_script (bs[1]->ip, 0));
      return reverse (descend_script (bs[1]->ip, bs[2]->ip, 0));
    case 1:
      if (nr_right == 0) {
	if (is_decoration (bs[0]->ip))
	  return reverse (descend_script (bs[0]->ip, 1));
	else return bs[0]->find_tree_path (bs[0]->find_right_box_path ());
      }
      if (nr_right == 1) return reverse (descend_script (bs[nr_left+1]->ip,1));
      return reverse (descend_script (bs[nr_left+1]->ip, bs[nr_left+2]->ip,1));
    case 2:
      if (nr_left == 1) return reverse (descend_script (bs[1]->ip, 1));
      return reverse (descend_script (bs[1]->ip, bs[2]->ip, 1));
    case 3:
      if (nr_right == 1) return reverse (descend_script (bs[nr_left+1]->ip,0));
      return reverse (descend_script (bs[nr_left+1]->ip, bs[nr_left+2]->ip,0));
    }
    FAILED ("bad leaf");
    return composite_box_rep::find_tree_path (bp);
  }
  else return composite_box_rep::find_tree_path (bp);
}

cursor
side_box_rep::find_cursor (path bp) {
  if (is_atom (bp) && (bp->item == 2)) {
    cursor cu (sx2 (1), 0);
    cu->y1= y1; cu->y2= y2;
    if (nr_left == 2) cu->ox= max (cu->ox, sx2 (2));
    return cu;
  }
  else if (is_atom (bp) && (bp->item == 3)) {
    cursor cu (sx1 (nr_left+ 1), 0);
    cu->y1= y1; cu->y2= y2;
    if (nr_right == 2) cu->ox= min (cu->ox, sx1 (nr_left+ 2));
    return cu;
  }
  else {
    cursor cu= composite_box_rep::find_cursor (bp);
    if (is_atom (bp) && (bp->item == 0) && (nr_left != 0)) cu->slope= 0.0;
    if (is_atom (bp) && (bp->item == 1) && (nr_right != 0)) cu->slope= 0.0;
    return cu;
  }
}

selection
side_box_rep::find_selection (path lbp, path rbp) {
  if ((lbp == path (2)) && (!is_atom (rbp)) && (rbp->item == 0))
    lbp= path (0, bs[0]->find_left_box_path ());
  if ((rbp == path (3)) && (!is_atom (lbp)) && (lbp->item == 0))
    rbp= path (0, bs[0]->find_right_box_path ());
  return composite_box_rep::find_selection (lbp, rbp);
}


void
side_box_rep::get_bracket_extents (SI& lo, SI& hi) {
  int i;
  SI ex= fn->yx;
  SI dd= ex / 4;
  lo= sy1 (0);
  hi= sy2 (0);
  for (i=1; i<N(bs); i++) {
    lo= min (lo, sy1 (i) + dd);
    hi= max (hi, sy2 (i) - dd);
  }
}

/******************************************************************************
* box construction routines
******************************************************************************/

box
limit_box (path ip, box ref, box lo, box hi, font fn, bool glued) {
  return tm_new<lim_box_rep> (ip, ref, lo, hi, fn, glued);
}

box
script_box (path ip, box b1, box b2, font fn) {
  return tm_new<dummy_script_box_rep> (ip, b1, b2, fn);
}

box
left_script_box (path ip, box ref, box b1, box b2, font fn, int level) {
  return tm_new<side_box_rep> (ip, ref, b1, b2, box (), box (), fn, level);
}

box
right_script_box (path ip, box ref, box b1, box b2, font fn, int level) {
  return tm_new<side_box_rep> (ip, ref, box (), box (), b1, b2, fn, level);
}

box
side_box (path ip, box ref, box l1, box l2, box r1, box r2, font fn, int l) {
  return tm_new<side_box_rep> (ip, ref, l1, l2, r1, r2, fn, l);
}
