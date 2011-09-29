
/******************************************************************************
* MODULE     : stack.cpp
* DESCRIPTION: Boxes stacked one above another
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Boxes/composite.hpp"

/******************************************************************************
* The stack_box representation
******************************************************************************/

struct stack_box_rep: public composite_box_rep {
  stack_box_rep (path ip, array<box> bs, array<SI> spc); 
  int get_type () { return STACK_BOX; }
  operator tree () {
    int i, n= N(bs);
    tree t (TUPLE, "stack");
    for (i=0; i<n; i++) t << ((tree) bs[i]);
    return t;
  }

  void      position (array<SI> spc);
  void      finalize ();
  void      display (renderer ren);
  void      clear_incomplete (rectangles& rs, SI pixel, int i, int i1, int i2);
  bool      access_allowed ();

  int       find_child (SI x, SI y, SI delta, bool force);
  path      find_tree_path (path bp);
  cursor    find_cursor (path bp);
  selection find_selection (path lbp, path rbp);
  gr_selections graphical_select (SI x1, SI y1, SI x2, SI y2);
};

/******************************************************************************
* Stack boxes
******************************************************************************/

void
stack_box_rep::position (array<SI> spc) {
  int i;
  y1 = y2 = 0;
  for (i=0; i<N(bs); i++) {
    sx(i)= 0;
    sy(i)= y1- bs[i]->y2;
    y1 -= (bs[i]->h()+ spc[i]);
  }
  composite_box_rep::position ();
}

stack_box_rep::stack_box_rep (path ip, array<box> bs2, array<SI> spc):
  composite_box_rep (ip)
{
  bs= bs2;
  if (N(bs) != 0)
    position (spc);
  finalize ();
}

void
stack_box_rep::finalize () {
  path old_ip= ip;
  ip= decorate_middle (ip);
  composite_box_rep::finalize ();
  ip= old_ip;
}

void
stack_box_rep::display (renderer ren) {
  ren->apply_shadow (x1, y1, x2, y2);
}

bool
stack_box_rep::access_allowed () {
  return false;
}

void
stack_box_rep::clear_incomplete (
  rectangles& rs, SI pixel, int which, int i1, int i2)
{
  if (N(bs) == 0) return;
  if ((i1 <= i2) && (!is_nil (rs))) {
    //cout << "Stack " << which << " ( " << i1 << " - " << i2 << " )" << LF;
    //cout << "  in: " << (rs/256) << LF;
    
    int i, n= N(bs);
    rectangle bound= least_upper_bound (rs);
    SI left = bound->x1, right= bound->x2;
    for (i=0; i<n; i++) {
      left = min (left , sx3 (i));
      right= max (right, sx4 (i));
    }
    if ((which >= 0) && (which < n)) {
      rectangle& r= access_last (rs);
      if (r->x2 >= sx4 (which)) r->x2= right;
    }

    /*
    SI Min_y= min_y, Max_y= max_y;
    if (i2+1<n) Min_y= sy4 (i2+1) + 2*pixel;
    if (i1  >0) Max_y= sy3 (i1-1) - 2*pixel;
    // cout << "  ys : " << Min_y/256 << ", " << min_y/256 << ", " << max_y/256 << ", " << Max_y/256 << "\n";
    */

    SI min_y= sy4 (i2) + 2*pixel, max_y= sy3 (i1) - 2*pixel;
    if ((min_y < max_y) && (bound->y1 < min_y) && (max_y < bound->y2)) {
      rectangles new_rs;
      rectangles count= rs;
      while (!is_nil (count)) {
	rectangle& r= count->item;
	if ((r->y1 <= min_y) || (r->y2 >= max_y))
	  new_rs= rectangles (r, new_rs);
	count= count->next;
      }
      new_rs= rectangles (rectangle (left, min_y, right, max_y), new_rs);
      rs= reverse (new_rs);
    }

    // cout << "  out: " << (rs/256) << "\n\n";
  }
}

/******************************************************************************
* Cursor routines
******************************************************************************/

int
stack_box_rep::find_child (SI x, SI y, SI delta, bool force) {
  int i, h, n=N(bs);
  if (n==0) return -1;
  else if (n<4) i=0;
  else {
    i= h= n>>1;
    while (h != 0) {
      SI yhi= sy1(i);
      SI ylo= sy2(i+1);
      if (h==1) h= 0;
      else h= (h+1) >> 1;
      if ((y < yhi) && (y < ylo)) i += h;
      else i -= max (h, 1);
      if (i < 0  ) i= 0;
      if (i > n-2) i= n-2;
    }
  }

  for (; i<n; i++) {
    if (i<n-1) {
      box sb1= bs[i];
      box sb2= bs[i+1];
      SI  yhi= sy1(i);
      SI  ylo= sy2(i+1);
      // cout << "Test [" << i << "] " << sb1 << ", " << sb2 << "\n";
      // cout << "  y  = " << y   << "\n";
      // cout << "  yhi= " << yhi << "\n";
      // cout << "  ylo= " << ylo << "\n";
      if ((y < yhi) && (y < ylo)) continue;
      if (ylo <= yhi) {
	if (y < ((ylo+yhi) >> 1)) continue;
      }
      else {
	int j;
	SI m= ylo, M= yhi;
	SI ox1= sx(i), ox2= sx(i+1);
	SI oy1= sy(i), oy2= sy(i+1);
	while ((N (sb1) == 1) && (N (sb2) == 1)) {
	  ox1 += sb1->sx(0); ox2 += sb2->sx(0);
	  oy1 += sb1->sy(0); oy2 += sb2->sy(0);
	  sb1= sb1[0];
	  sb2= sb2[0];
	}
	if ((N(sb1) > 0 && N(sb1[0]) == 0) ||
	    (N(sb2) > 0 && N(sb2[0]) == 0))
	  if (y < ((ylo+yhi) >> 1)) continue;
	for (j=0; j<N(sb1); j++)
	  if (!outside (x- ox1- sb1->sx(j), delta, sb1[j]->x1, sb1[j]->x2))
	    m= min (m, oy1+ sb1->sy1(j));
	for (j=0; j<N(sb2); j++)
	  if (!outside (x- ox2- sb2->sx(j), delta, sb2[j]->x1, sb2[j]->x2))
	    M= max (M, oy2+ sb2->sy2(j));
	if (y < ((m+M) >> 1)) continue;
      }
    }
    // cout << "Done [" << i << "]\n";

    if (bs[i]->decoration () && (!force)) {
      int j, k;
      for (j=i-1; j>=0; j--)
	if (bs[j]->accessible () || force) break;
      for (k=i+1; k< n; k++)
	if (bs[k]->accessible () || force) break;
      if ((j< 0) && (k>=n)) return -1;
      if ((j>=0) && (k>=n)) return j;
      if ((j< 0) && (k< n)) return k;
      if (y >= ((sy1(j) + sy2(k)) >> 1)) return j;
      return k;
    }

    return i;
  }

  return -1;
}

path
stack_box_rep::find_tree_path (path bp) {
  if (is_atom (bp)) {
    if (bp->item == 0) {
      if (is_accessible (lip)) return reverse (lip);
      else return reverse (descend_decode (lip, 0));
    }
    else {
      if (is_accessible (rip)) return reverse (rip);
      else return reverse (descend_decode (rip, 1));
    }
  }
  else return composite_box_rep::find_tree_path (bp);
}

cursor
stack_box_rep::find_cursor (path bp) {
  cursor cu= composite_box_rep::find_cursor (bp);
  int i= bp->item, j1, j2, n= N(bs);
  if (is_atom (bp)) i= (bp == 0? 0: N(bs)-1);
  if (bs[i]->h() != 0) return cu;
  for (j1= i-1; j1>=0; j1--)
    if (bs[j1]->h () != 0) break;
  for (j2= i+1; j2<n; j2++)
    if (bs[j2]->h () != 0) break;
  if (j2 < n) cu->oy += sy (j2) - sy (i);
  else if (j1 >= 0) cu->oy += sy (j1) - sy (i);
  return cu;
}

/******************************************************************************
* Selections
******************************************************************************/

static rectangles
descend (rectangles l, SI y) {
  if (is_nil (l)) return l;
  rectangle& r= l->item;
  return rectangles (rectangle (r->x1, min (r->y1, y), r->x2, r->y2),
		     descend (l->next, y));
}

static rectangles
ascend (rectangles l, SI y) {
  if (is_nil (l)) return l;
  rectangle& r= l->item;
  return rectangles (rectangle (r->x1, r->y1, r->x2, max (r->y2, y)),
		     ascend (l->next, y));
}

static rectangles
extend_left (rectangles l, SI x) {
  if (is_nil (l)) return l;
  rectangle& r= l->item;
  return rectangles (rectangle (min (r->x1, x), r->y1, r->x2, r->y2),
		     extend_left (l->next, x));
}

static rectangles
extend_right (rectangles l, SI x) {
  if (is_nil (l)) return l;
  rectangle& r= l->item;
  return rectangles (rectangle (r->x1, r->y1, max (r->x2, x), r->y2),
		     extend_right (l->next, x));
}

static rectangles
truncate_top (rectangles l, SI y) {
  if (is_nil (l)) return l;
  rectangle& r= l->item;
  return rectangles (rectangle (r->x1, r->y1, r->x2, min (r->y2, y)),
		     truncate_top (l->next, y));
}

static rectangles
truncate_bottom (rectangles l, SI y) {
  if (is_nil (l)) return l;
  rectangle& r= l->item;
  return rectangles (rectangle (r->x1, max (r->y1, y), r->x2, r->y2),
		     truncate_bottom (l->next, y));
}

selection
stack_box_rep::find_selection (path lbp, path rbp) {
  if ((N(bs) == 0) ||
      ((!is_atom (lbp)) && (!is_atom (rbp)) && (lbp->item == rbp->item)))
    return composite_box_rep::find_selection (lbp, rbp);

  int  i1  = is_atom (lbp)? 0      : lbp->item;
  int  i2  = is_atom (rbp)? N(bs)-1: rbp->item;
  path lbp1= is_atom (lbp)? path (i1, bs[i1]->find_left_box_path ()) : lbp;
  path rbp1= path (i1, bs[i1]->find_right_box_path ());
  path lbp2= path (i2, bs[i2]->find_left_box_path ());
  path rbp2= is_atom (rbp)? path (i2, bs[i2]->find_right_box_path ()): rbp;

  if (i1 == i2) {
    path lp= find_tree_path (lbp);
    path rp= find_tree_path (rbp);
    return selection (find_selection (lbp1, rbp2)->rs, lp, rp);
  }
  else if (i1 < i2) {
    selection sel1= find_selection (lbp1, rbp1);
    selection sel2= find_selection (lbp2, rbp2);
    path lp= sel1->start;
    path rp= sel2->end;
    SI midy1= sy1(i2-1);
    SI midy2= sy2(i1+1);
    rectangles rs;
    if (i2 == i1+1) {
      rs << extend_right (sel1->rs, x2);
      rs << extend_left  (sel2->rs, x1);
      if ((!is_nil (sel1->rs)) && (!is_nil (sel2->rs))) {
	rectangle r1= least_upper_bound (sel1->rs);
	rectangle r2= least_upper_bound (sel2->rs);
	if ((r1->x1 < r2->x2) && (r2->y2 < r1->y1))
	  rs << rectangle (r1->x1, r2->y2, r2->x2, r1->y1);
      }
    }
    else {
      rs << extend_right (descend (sel1->rs, midy2), x2);
      rs << extend_left  (ascend  (sel2->rs, midy1), x1);
      if (midy1 < midy2) rs << rectangle (x1, midy1, x2, midy2);
    }

    /* This hack produces nicer selections in case of a hidden top/bottom */
    int j1= i1, j2= i2;
    while (j1 < i2 && bs[j1]->h() == 0) j1++;
    while (j2 > j1 && bs[j2]->h() == 0) j2--;
    if (j1 != i1) rs= truncate_top    (rs, sy2 (j1));
    if (j2 != i2) rs= truncate_bottom (rs, sy1 (j2));
    /* End hack */

    return selection (rs, lp, rp);
  }
  else return box_rep::find_selection (lbp, rbp);
}

gr_selections
stack_box_rep::graphical_select (SI x1, SI y1, SI x2, SI y2) {
  gr_selections res;
  int i, n= subnr();
  for (i=n-1; i>=0; i--)
    res << bs[i]->graphical_select (x1- sx(i), y1- sy(i),
				    x2- sx(i), y2- sy(i));
  return res;
}

/******************************************************************************
* box construction routines
******************************************************************************/

box
stack_box (path ip, array<box> bs, array<SI> spc) {
  return tm_new<stack_box_rep> (ip, bs, spc);
}
