
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
#include "Boxes/construct.hpp"

/******************************************************************************
* The concat_box representation
******************************************************************************/

struct concat_box_rep: public composite_box_rep {
  array<SI> spc;
  bool indent;
  concat_box_rep (path ip, array<box> bs, array<SI> spc, bool indent);
  operator tree ();
  box adjust_kerning (int mode, double factor);
  box expand_glyphs (int mode, double factor);

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
  SI        wide_correction (int mode);
  void      get_bracket_extents (SI& lo, SI& hi);

  int       find_any_child (SI x, SI y, SI delta, SI& delta_out);
  int       find_accessible_child (SI x, SI y, SI delta, SI& delta_out);
  int       find_child (SI x, SI y, SI delta, bool force);
  path      find_box_path (SI x, SI y, SI delta, bool force, bool& found);
  path      find_tree_path (path bp);
  cursor    find_cursor (path bp);
  selection find_selection (path lbp, path rbp);

  tree      action (tree t, SI x, SI y, SI delta);
  void      loci (SI x, SI y, SI delta, list<string>& ids, rectangles& rs);
  SI        get_leaf_offset (string search);

  box       transform (frame fr);
  gr_selections graphical_select (SI x, SI y, SI dist);
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

  bool ok= !indent;
  x1= y1= x3= y3= MAX_SI;
  x2= y2= x4= y4= -MAX_SI;
  for (i=0; i<N(bs); i++)
    if (!indent || sx2(i) > sx1(i)) {
      ok= true;
      x1= min (x1, sx1(i));
      y1= min (y1, sy1(i));
      x2= max (x2, sx2(i));
      y2= max (y2, sy2(i));
      x3= min (x3, sx3(i));
      y3= min (y3, sy3(i));
      x4= max (x4, sx4(i));
      y4= max (y4, sy4(i));
    }
  if (!ok) composite_box_rep::position ();
}

concat_box_rep::concat_box_rep
  (path ip, array<box> bs2, array<SI> spc2, bool indent2):
    composite_box_rep (ip), spc (spc2), indent (indent2)
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


box
concat_box_rep::adjust_kerning (int mode, double factor) {
  int n= N(bs);
  array<box> adj (n);
  array<SI > spa (n);
  for (int i=0; i<n; i++) {
    int smode= mode;
    if (sx1(i) > x1) smode= smode & (~START_OF_LINE);
    if (sx2(i) < x2) smode= smode & (~END_OF_LINE);
    adj[i]= bs[i]->adjust_kerning (smode, factor);
    spa[i]= (SI) tm_round ((1 + 4*factor) * spc[i]);
  }
  return concat_box (ip, adj, spa, indent);
}

box
concat_box_rep::expand_glyphs (int mode, double factor) {
  int n= N(bs);
  array<box> adj (n);
  array<SI > spa (n);
  for (int i=0; i<n; i++) {
    adj[i]= bs[i]->expand_glyphs (mode, factor);
    spa[i]= (SI) tm_round ((1 + factor) * spc[i]);
  }
  return concat_box (ip, adj, spa, indent);
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

SI
concat_box_rep::wide_correction (int mode) {
  SI current= 0;
  bool done= false;
  int i, n= N(bs);
  for (i=0; i<n; i++)
    if (bs[i]->w() != 0) {
      if (done) return 0;
      current= bs[i]->wide_correction (mode);
      done= true;
    }
  if (!done && mode == 0) return 1;
  return current;
}

void
concat_box_rep::get_bracket_extents (SI& lo, SI& hi) {
  int i, n= N(bs);
  if (n == 0) box_rep::get_bracket_extents (lo, hi);
  else bs[0]->get_bracket_extents (lo, hi);
  for (i=1; i<N(bs); i++) {
    SI lo2, hi2;
    bs[i]->get_bracket_extents (lo2, hi2);
    lo= min (lo, lo2);
    hi= max (hi, hi2);
  }
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
concat_box_rep::find_box_path (SI x, SI y, SI delta, bool force, bool& found) {
  int delta_out, m;
  if (force) m= find_any_child (x, y, delta, delta_out);
  else m= find_accessible_child (x, y, delta, delta_out);
  int i, n= subnr();
  if (m != -1)
    for (i=0; i<=n; i++) {
      int c= (m+i) % n;
      SI xx= x- sx(c), yy= y- sy(c);
      SI dd= delta_out + get_delta (xx, bs[c]->x1, bs[c]->x2);
      path r= path (c, bs[c]->find_box_path (xx, yy, dd, force, found));
      if (found || i == n) return r;
    }
  return box_rep::find_box_path (x, y, delta, force, found);
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
    if (indent) {
      r->x1= max (r->x1, x1);
      r->y1= max (r->y1, y1);
      r->x2= min (r->x2, x2);
      r->y2= min (r->y2, y2);
    }
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
concat_box_rep::graphical_select (SI x, SI y, SI dist) {
  gr_selections res;
  int i, n= subnr();
  for (i=n-1; i>=0; i--)
    res << bs[i]->graphical_select (x- sx(i), y- sy(i), dist);
  return res;
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
  concat_box_rep (ip, bs, spc, false), logs_ptr (NULL) {}

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
concat_box (path ip, array<box> bs, array<SI> spc, bool indent) {
  return tm_new<concat_box_rep> (ip, bs, spc, indent);
}

box
phrase_box (path ip, array<box> bs, array<SI> spc) {
  return tm_new<phrase_box_rep> (ip, bs, spc);
}
