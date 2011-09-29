
/******************************************************************************
* MODULE     : concat.cpp
* DESCRIPTION: Concatenations of arrays of boxes
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Boxes/composite.hpp"

/******************************************************************************
* The concat_box representation
******************************************************************************/

struct concat_box_rep: public composite_box_rep {
  concat_box_rep (path ip, array<box> bs, array<SI> spc);
  operator tree ();

  void      finalize ();
  void      clear_incomplete (rectangles& rs, SI pixel, int i, int i1, int i2);
  bool      access_allowed ();
  void      position (array<SI> spc);
  int       count_left (int i);
  int       count_right (int i);

  int       get_first ();
  int       get_last ();
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

  int       find_any_child (SI x, SI y, SI delta, SI& delta_out);
  int       find_accessible_child (SI x, SI y, SI delta, SI& delta_out);
  int       find_child (SI x, SI y, SI delta, bool force);
  path      find_box_path (SI x, SI y, SI delta, bool force);
  path      find_tree_path (path bp);
  cursor    find_cursor (path bp);
  selection find_selection (path lbp, path rbp);

  tree      action (tree t, SI x, SI y, SI delta);
  void      loci (SI x, SI y, SI delta, list<string>& ids, rectangles& rs);
  SI        get_leaf_offset (string search);

  box       transform (frame fr);
  gr_selections graphical_select (SI x1, SI y1, SI x2, SI y2);
};

concat_box_rep::operator tree () {
  int i, n= N(bs);
  tree t (TUPLE, n+1);
  t[0]= "concat";
  for (i=0; i<n; i++) t[i+1]= (tree) bs[i];
  return t;
}

/******************************************************************************
* Routines for concatenation boxes
******************************************************************************/

void
concat_box_rep::position (array<SI> spc) {
  int i;
  ASSERT (N(bs) != 0, "concat of zero boxes");
  x1 = bs[0]->x1;
  x2 = 0;
  for (i=0; i<N(bs); i++) {
    x2 += spc[i];
    sx(i)= x2;
    sy(i)= 0;
    x2 += bs[i]->x2;
  }
  composite_box_rep::position ();
}

concat_box_rep::concat_box_rep (path ip, array<box> bs2, array<SI> spc):
  composite_box_rep (ip)
{
  bs = bs2;
  position (spc);
  finalize ();
}

void
concat_box_rep::finalize () {
  path old_ip= ip;
  ip= decorate_middle (ip);
  composite_box_rep::finalize ();
  ip= old_ip;
}

bool
concat_box_rep::access_allowed () {
  return false;
}

box
concat_box_rep::transform (frame fr) {
  return composite_box_rep::transform (fr);
}

void
concat_box_rep::clear_incomplete (
  rectangles& rs, SI pixel, int i, int i1, int i2)
{
  (void) pixel; (void) i;
  if (i1 < i2) {
    // cout << "Concat " << i << " ( " << i1 << " - " << i2 << " )\n";
    // cout << "  in : " << rs << "\n";

    SI left = sx4 (i1);
    SI right= sx3 (i2);
    bool lbusy= (i+i) >= (i1+i2);
    bool rbusy= (i+i) <= (i1+i2);
    rectangles new_rs;
    rectangles mid_rs;
    rectangles count= rs;
    while (!is_nil (count)) {
      rectangle& r= count->item;
      if ((lbusy && (r->x1 < left)) || (rbusy && (r->x2 > right))) new_rs << r;
      else mid_rs << r;
      count= count->next;
    }
    rs= new_rs;
    if (!is_nil (mid_rs)) rs= rs * least_upper_bound (mid_rs);

    // cout << "  out: " << rs << "\n\n";
  }
}

/******************************************************************************
* Layout routines
******************************************************************************/

int
concat_box_rep::get_first () {
  int i=0, n=N(bs);
  while ((i<n) && (sx2(i)<=x1)) i++;
  return i;
}

int
concat_box_rep::get_last () {
  int n=N(bs), i=n-1;
  while ((i>=0) && (sx1(i)>=x2)) i--;
  return i;
}

double
concat_box_rep::left_slope () {
  int i= get_first ();
  if (i<N(bs)) return bs[i]->left_slope ();
  return 0.0;
}

double
concat_box_rep::right_slope () {
  int i= get_last ();
  if (i>=0) return bs[i]->right_slope ();
  return 0.0;
}

SI
concat_box_rep::left_correction () {
  int i= get_first ();
  if (i<N(bs)) return bs[i]->left_correction ();
  return 0;
}

SI
concat_box_rep::right_correction () {
  int i= get_last ();
  if (i>=0) return bs[i]->right_correction ();
  return 0;
}

SI
concat_box_rep::lsub_correction () {
  int i= get_first ();
  if (i<N(bs)) return bs[i]->lsub_correction ();
  return 0;
}

SI
concat_box_rep::lsup_correction () {
  int i= get_first ();
  if (i<N(bs)) return bs[i]->lsup_correction ();
  return 0;
}

SI
concat_box_rep::rsub_correction () {
  int i= get_last ();
  if (i>=0) return bs[i]->rsub_correction ();
  return 0;
}

SI
concat_box_rep::rsup_correction () {
  int i= get_last ();
  if (i>=0) return bs[i]->rsup_correction ();
  return 0;
}

SI
concat_box_rep::sub_lo_base (int level) {
  int i=0, n=N(bs);
  SI  y=y1;
  for (i=0; i<n; i++)
    y= min (y, bs[i]->sub_lo_base (level));
  return y;
}

SI
concat_box_rep::sub_hi_lim  (int level) {
  int i=0, n=N(bs);
  SI  y= y1 + (y2-y1)/4;
  for (i=0; i<n; i++)
    y= max (y, bs[i]->sub_hi_lim (level));
  return y;
}

SI
concat_box_rep::sup_lo_lim  (int level) {
  int i=0, n=N(bs);
  SI  y=y2 - (y2-y1)/4;
  for (i=0; i<n; i++)
    y= min (y, bs[i]->sup_lo_lim (level));
  return y;
}

SI
concat_box_rep::sup_lo_base (int level) {
  int i=0, n=N(bs);
  SI  y=y2 - (y2-y1)/4;
  for (i=0; i<n; i++)
    y= min (y, bs[i]->sup_lo_base (level));
  return y;
}

SI
concat_box_rep::sup_hi_lim  (int level) {
  int i=0, n=N(bs);
  SI  y=y2;
  for (i=0; i<n; i++)
    y= max (y, bs[i]->sup_hi_lim (level));
  return y;
}

/******************************************************************************
* Cursor routines
******************************************************************************/

int
concat_box_rep::count_left (int i) {
  int n;
  for (n=1; (i+n<N(bs)) && (i+n>0); n++)
    if (sx1(i+n) > sx1(i)) return n;
  return n;
}

int
concat_box_rep::count_right (int i) {
  int n;
  for (n=1; (i-n<N(bs)) && (i-n>0); n++)
    if (sx2(i) > sx2(i-n)) return n;
  return n;
}

int
concat_box_rep::find_any_child (SI x, SI y, SI delta, SI& delta_out) {
  (void) y;
  int i, n;
  bool flag;
  delta_out= delta;

  SI x_1= sx2(N(bs)-1);
  if (x >= x_1) {
    n= count_right (N(bs)-1);
    if ((delta >= 0) || (x > x_1)) i=N(bs)-1; else i=N(bs)+delta;
    if ((N(bs)-1)-i<n) {
      delta_out= ((i==N(bs)-1)? delta: 0);
      return i;
    }
    delta_out= delta+ (n-1);
    return N(bs)-n;
  }

  for (i=0; i<N(bs); i++) {
    SI x_i= sx1(i);
    if ((x < x_i) || ((x == x_i) && (delta < 0))) return i;

    n= count_left (i);
    if (x == sx1(i)) {
      // if ((i>0) && (sx2(i-1) < x))
      //   return i;
      if (delta<0) return i-1; // i can not be zero here
      if (delta<n) {
	delta_out= 0;
	return i + delta;
      }
      delta_out= delta- (n-1);
      return i + (n-1);
    }

    i += n-1;
    if (i+1 == N(bs)) flag=true;
    else {
      int ex1= x- sx2(i);
      int ex2= sx1(i+1)- x;
      flag= ((ex2>ex1+1) || ((ex2>=ex1) && (delta<0)));
    }
    if (flag) return i;
  }

  return -1;
}

int
concat_box_rep::find_accessible_child (SI x, SI y, SI delta, SI& delta_out) {
  int i= find_any_child (x, y, delta, delta_out);

  if (bs[i]->decoration ()) {
    int j, k, n= N(bs);
    for (j=i-1; j>=0; j--)
      if (bs[j]->accessible ()) break;
    for (k=i+1; k< n; k++)
      if (bs[k]->accessible ()) break;
    if ((j< 0) && (k>=n)) return -1;
    if ((j>=0) && (k>=n)) return j;
    if ((j< 0) && (k< n)) return k;
    SI m= (sx2(j) + sx1(k)) >> 1;
    if (sx2(j) == m) return i <= ((j+k)>>1)? j: k;
    if ((x<m) || ((x==m) && (delta<0))) return j;
    return k;
  }

  return i;
}

int
concat_box_rep::find_child (SI x, SI y, SI delta, bool force) {
  int delta_out;
  if (force) return find_any_child (x, y, delta, delta_out);
  else return find_accessible_child (x, y, delta, delta_out);
}

path
concat_box_rep::find_box_path (SI x, SI y, SI delta, bool force) {
  int delta_out, m;
  if (force) m= find_any_child (x, y, delta, delta_out);
  else m= find_accessible_child (x, y, delta, delta_out);
  if (m==-1) return box_rep::find_box_path (x, y, delta, force);
  else {
    SI xx= x- sx(m), yy= y- sy(m);
    SI dd= delta_out + get_delta (xx, bs[m]->x1, bs[m]->x2);
    return path (m, bs[m]->find_box_path (xx, yy, dd, force));
  }
}

path
concat_box_rep::find_tree_path (path bp) {
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
concat_box_rep::find_cursor (path bp) {
  if (is_atom (bp)) return box_rep::find_cursor (bp);
  else {
    int i= bp->item, j, n;
    cursor cu= bs[i]->find_cursor (bp->next);
    cu->delta -= get_delta (cu->ox, bs[i]->x1, bs[i]->x2);
    cu->ox    += sx(i);
    cu->oy    += sy(i);

    SI x_1= sx2(N(bs)-1);
    if (cu->ox >= x_1) {
      n= count_right (N(bs)-1);
      if ((i==N(bs)-1) || (cu->ox > x_1));
      else if (i==N(bs)-n) cu->delta -= n-1;
      else cu->delta= i-N(bs);
      return cu;
    }

    if (cu->ox == sx1(i)) {
      // for (j=0; j<i; j++) if (sx1(j) >= sx1 (i)) break;
      for (j=i; j>0; j--) if (sx1(j) > sx1 (j-1)) break;
      n= count_left (j);
      if ((i==j) || (cu->ox != sx1(j)));
      else if (i==j+(n-1)) cu->delta += n-1;
      else cu->delta= i-j;
    }

    return cu;
  }
}

selection
concat_box_rep::find_selection (path lbp, path rbp) {
  if ((N(bs) == 0) ||
      ((!is_atom (lbp)) && (!is_atom (rbp)) && (lbp->item == rbp->item)))
    return composite_box_rep::find_selection (lbp, rbp);

  int  i;
  int  i1  = is_atom (lbp)? 0      : lbp->item;
  int  i2  = is_atom (rbp)? N(bs)-1: rbp->item;
  path lbp1= is_atom (lbp)? path (i1, bs[i1]->find_left_box_path ()) : lbp;
  path rbp1= path (i1, bs[i1]->find_right_box_path ());
  path lbp2= path (i2, bs[i2]->find_left_box_path ());
  path rbp2= is_atom (rbp)? path (i2, bs[i2]->find_right_box_path ()): rbp;

  /*
  cout << "Find selection " << lbp << " --- " << rbp << "\n"
       << "     in concat " << box (this) << "\n";
  cout << "  i1  =" << i1 << "\n";
  cout << "  i2  =" << i2 << "\n";
  cout << "  lbp1=" << lbp1 << "\n";
  cout << "  rbp1=" << rbp1 << "\n";
  cout << "  lbp2=" << lbp2 << "\n";
  cout << "  rbp2=" << rbp2 << "\n";
  */

  if (i1 == i2) {
    path lp= find_tree_path (lbp);
    path rp= find_tree_path (rbp);
    return selection (find_selection (lbp1, rbp2)->rs, lp, rp);
  }
  else {
    selection sel1= find_selection (lbp1, rbp1);
    selection sel2= find_selection (lbp2, rbp2);
    path lp= sel1->start;
    path rp= sel2->end;
    rectangles rs; rs << sel1->rs << sel2->rs;
    for (i=i1+1; i<i2; i++) {
      path lbpi= path (i, bs[i]->find_left_box_path ());
      path rbpi= path (i, bs[i]->find_right_box_path ());
      rs << find_selection (lbpi, rbpi)->rs;
    }
    if (is_nil (rs)) return selection (rectangles (), lp, rp);
    rectangle r= least_upper_bound (rs);
    return selection (r, lp, rp);
  }
}

tree
concat_box_rep::action (tree t, SI x, SI y, SI delta) {
  int delta_out, m= find_any_child (x, y, delta, delta_out);
  if (m == -1) return "";
  SI xx= x- sx(m), yy= y- sy(m);
  SI dd= delta_out + get_delta (xx, bs[m]->x1, bs[m]->x2);
  return bs[m]->action (t, xx, yy, dd);
}

void
concat_box_rep::loci (SI x, SI y, SI delta,
		      list<string>& ids, rectangles& rs)
{
  int delta_out, m= find_any_child (x, y, delta, delta_out);
  if (m == -1) box_rep::loci (x, y, delta, ids, rs);
  else {
    SI xx= x- sx(m), yy= y- sy(m);
    SI dd= delta_out + get_delta (xx, bs[m]->x1, bs[m]->x2);
    bs[m]->loci (xx, yy, dd, ids, rs);
    rs= translate (rs, sx(m), sy(m));
  }
}

SI
concat_box_rep::get_leaf_offset (string search) {
  int i, n=N(bs);
  for (i=0; i<n; i++) {
    SI offset= bs[i]->get_leaf_offset (search);
    if (offset != bs[i]->w()) return sx1(i) + offset;
  }
  return w();
}

gr_selections
concat_box_rep::graphical_select (SI x1, SI y1, SI x2, SI y2) {
  gr_selections res;
  int i, n= subnr();
  for (i=n-1; i>=0; i--)
    res << bs[i]->graphical_select (x1- sx(i), y1- sy(i),
                                    x2- sx(i), y2- sy(i));
  return res;
}

/******************************************************************************
* Phrase boxes
******************************************************************************/

class phrase_box_rep: public concat_box_rep {
public:
  rectangles* logs_ptr;
  SI          ox, oy;
  phrase_box_rep (path ip, array<box> bs, array<SI> spc);
  ~phrase_box_rep ();
  void position_at (SI x, SI y, rectangles& change_log_ptr);
  void display (renderer ren);
};

phrase_box_rep::phrase_box_rep (path ip, array<box> bs, array<SI> spc):
  concat_box_rep (ip, bs, spc), logs_ptr (NULL) {}

phrase_box_rep::~phrase_box_rep () {
  if (logs_ptr != NULL) {
    rectangles& logs= *logs_ptr;
    logs= rectangles (rectangle (ox+x3, oy+y3, ox+x4, oy+y4), logs);
    logs= rectangles (rectangle (0, 0, 0, 0), logs);
    // cout << "  8=X " << rectangle (ox+x3, oy+y3, ox+x4, oy+y4) << "\n";
  }
}

void
phrase_box_rep::position_at (SI x, SI y, rectangles& logs) {
  x += x0; y += y0;
  if (logs_ptr == NULL) logs= rectangles (rectangle (0, 0, 0, 0), logs);
  else logs= rectangles (rectangle (ox+x3, oy+y3, ox+x4, oy+y4), logs);
  ox= x; oy= y;
  logs= rectangles (rectangle (ox+x3, oy+y3, ox+x4, oy+y4), logs);
  logs_ptr= &logs;
}

void
phrase_box_rep::display (renderer ren) {
  ren->apply_shadow (x1, y1, x2, y2);
}

/******************************************************************************
* box construction routines
******************************************************************************/

box
concat_box (path ip, array<box> bs, array<SI> spc) {
  return tm_new<concat_box_rep> (ip, bs, spc);
}

box
phrase_box (path ip, array<box> bs, array<SI> spc) {
  return tm_new<phrase_box_rep> (ip, bs, spc);
}
