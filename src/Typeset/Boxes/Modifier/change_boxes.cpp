
/******************************************************************************
* MODULE     : change.cpp
* DESCRIPTION: Boxes whose behaviour is changed
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Boxes/composite.hpp"
#include "Boxes/construct.hpp"
#include "scheme.hpp"

/******************************************************************************
* changing the behaviour of a box
******************************************************************************/

struct change_box_rep: public composite_box_rep {
  bool child_flag, big_flag;
  change_box_rep (path ip, bool child_flag, bool big_flag= false);
  operator tree () { return tree (TUPLE, "change", (tree) bs[0]); }
  int find_child (SI x, SI y, SI delta, bool force);
  path find_left_box_path () {
    return child_flag?
      composite_box_rep::find_left_box_path ():
      path (0, bs[0]->find_left_box_path ()); }
  path find_right_box_path () {
    return child_flag?
      composite_box_rep::find_right_box_path ():
      path (0, bs[0]->find_right_box_path ()); }

  double left_slope () {
    return big_flag? bs[0]->left_slope(): box_rep::left_slope(); }
  double right_slope () { 
    return big_flag? bs[0]->right_slope(): box_rep::right_slope(); }
  SI left_correction () {
    return big_flag? bs[0]->left_correction():box_rep::left_correction(); }
  SI right_correction () {
    return big_flag? bs[0]->right_correction():box_rep::right_correction(); }
  SI lsub_correction () {
    return big_flag? bs[0]->lsub_correction(): box_rep::lsub_correction(); }
  SI lsup_correction () {
    return big_flag? bs[0]->lsup_correction(): box_rep::lsup_correction(); }
  SI rsub_correction () {
    return big_flag? bs[0]->rsub_correction(): box_rep::rsub_correction(); }
  SI rsup_correction () {
    return big_flag? bs[0]->rsup_correction(): box_rep::rsup_correction(); }
  SI sub_lo_base (int l) {
    return big_flag? bs[0]->sub_lo_base (l): box_rep::sub_lo_base (l); }
  SI sub_hi_lim  (int l) {
    return big_flag? bs[0]->sub_hi_lim (l): box_rep::sub_hi_lim (l); }
  SI sup_lo_lim  (int l) {
    return big_flag? bs[0]->sup_lo_lim (l): box_rep::sup_lo_lim (l); }
  SI sup_lo_base (int l) {
    return big_flag? bs[0]->sup_lo_base (l): box_rep::sup_lo_base (l); }
  SI sup_hi_lim  (int l) {
    return big_flag? bs[0]->sup_hi_lim (l): box_rep::sup_hi_lim (l); }

  SI get_leaf_offset (string search) {
    return sx1(0) + bs[0]->get_leaf_offset (search); }

  gr_selections graphical_select (SI x, SI y, SI dist);
};

change_box_rep::change_box_rep (path ip, bool fl1, bool fl2):
  composite_box_rep (ip), child_flag (fl1), big_flag (fl2) {}

int
change_box_rep::find_child (SI x, SI y, SI delta, bool force) {
  if (child_flag) return composite_box_rep::find_child (x, y, delta, force);
  return 0;
}

gr_selections
change_box_rep::graphical_select (SI x, SI y, SI dist) { 
//TODO : Check if it is correct
  if (child_flag)
    return composite_box_rep::graphical_select (x, y, dist);
  else
    return bs[0]->graphical_select (x- sx(0), y- sy(0), dist);
}

/******************************************************************************
* moving and resizing boxes
******************************************************************************/

struct move_box_rep: public change_box_rep {
  move_box_rep (path ip, box b, SI x, SI y, bool fl1, bool fl2);
  int get_type () { return MOVE_BOX; }
  operator tree () { return tree (TUPLE, "move", (tree) bs[0]); }
};

move_box_rep::move_box_rep (path ip, box b, SI x, SI y, bool fl1, bool fl2):
  change_box_rep (ip, fl1, fl2)
{
  insert (b, x, y);
  position ();
  finalize ();
}

struct resize_box_rep: public change_box_rep {
  resize_box_rep (path ip, box b, SI x1, SI y1, SI x2, SI y2,
		  bool child_flag, bool adjust);
  operator tree () { return tree (TUPLE, "resize", (tree) bs[0]); }
};

resize_box_rep::resize_box_rep (
  path ip, box b, SI X1, SI Y1, SI X2, SI Y2, bool child_flag, bool adjust):
    change_box_rep (ip, child_flag)
{
  insert (b, 0, 0);
  position ();
  x1= X1; y1= Y1;
  x2= X2; y2= Y2;
  if (adjust) left_justify ();
  finalize ();
}

struct vcorrect_box_rep: public change_box_rep {
  vcorrect_box_rep (path ip, box b, SI top_cor, SI bot_cor);
  operator tree () { return tree (TUPLE, "vcorrect", (tree) bs[0]); }
};

vcorrect_box_rep::vcorrect_box_rep (path ip, box b, SI top_cor, SI bot_cor):
  change_box_rep (ip, false, false)
{
  insert (b, 0, -top_cor);
  position ();
  y1 -= bot_cor;
  y2 += top_cor;
  finalize ();
}

/******************************************************************************
* horizontally repetition of one box inside another one
******************************************************************************/

struct repeat_box_rep: public change_box_rep {
  repeat_box_rep (path ip, box b, box repeat, SI xoff);
  operator tree () { return tree (TUPLE, "repeat", (tree) bs[0]); }
};

repeat_box_rep::repeat_box_rep (path ip, box b, box repeat, SI xoff):
  change_box_rep (ip, false)
{
  insert (b, 0, 0);
  position ();

  SI width= repeat->w ();
  if (width >= PIXEL) {
    int i;
    int i1= ((xoff+b->x1)/width)-1;
    int i2= ((xoff+b->x2)/width)+1;
    while (i1*width < (xoff+b->x1)) i1++;
    while (i2*width > (xoff+b->x2)) i2--;
    for (i=i1; i<i2; i++) {
      box bb= move_box (decorate_right (ip), repeat, 0, 0);
      insert (bb, i*width-xoff, 0);
    }
  }

  finalize ();
  x1= b->x1; y1= b->y1;
  x2= b->x2; y2= b->y2;
}

/******************************************************************************
* cell boxes for tables
******************************************************************************/

struct cell_box_rep: public change_box_rep {
  SI    bl, br, bb, bt;
  color fg, bg, old_bg;
  bool  transp;
  cell_box_rep (path ip, box b, SI x0, SI y0, SI x1, SI y1, SI x2, SI y2,
		SI bl, SI br, SI bb, SI bt, color fg, color bg, bool transp);
  operator tree () { return tree (TUPLE, "cell", (tree) bs[0]); }
  void pre_display (ps_device &dev);
  void post_display (ps_device &dev);
  void display (ps_device dev);
};

cell_box_rep::cell_box_rep (
  path ip, box b, SI X0, SI Y0, SI X1, SI Y1, SI X2, SI Y2,
  SI Bl, SI Br, SI Bb, SI Bt, color Fg, color Bg, bool Transp):
  change_box_rep (ip, false),
  bl (Bl<<1), br (Br<<1), bb (Bb<<1), bt (Bt<<1),
  fg (Fg), bg (Bg), transp (Transp)
{
  insert (b, X0, Y0);
  position ();
  x1= X1; y1= Y1;
  x2= X2; y2= Y2;
  if ((!transp) || (bl>0) || (br>0) || (bb>0) || (bt>0)) {
    // the 4*PIXEL extra space is sufficient for (shrinking factor) <= 8
    x3= min (x3, x1 - (bl>>1) - (bl>0? PIXEL<<2: 0));
    y3= min (y3, y1 - (bb>>1) - (bb>0? PIXEL<<2: 0));
    x4= max (x4, x2 + (br>>1) + (br>0? PIXEL<<2: 0));
    y4= max (y4, y2 + (bt>>1) + (bt>0? PIXEL<<2: 0));
  }
  finalize ();
}

void
cell_box_rep::pre_display (ps_device& dev) {
  if (transp) return;
  old_bg= dev->get_background ();
  dev->set_background (bg);
  dev->clear (x1, y1, x2, y2);
}

void
cell_box_rep::post_display (ps_device &dev) {
  if (transp) return;
  dev->set_background (old_bg);
}

void
cell_box_rep::display (ps_device dev) {
  if ((bl>0) || (br>0) || (bb>0) || (bt>0)) {
    SI l= bl, r= br, b= bb, t= bt;
    if (dev->sfactor > 1) { // correction for screen display only
      SI  pixel= dev->pixel;
      l= ((l + (pixel - 1)) / pixel) * pixel;
      r= ((r + (pixel - 1)) / pixel) * pixel;
      b= ((b + (pixel - 1)) / pixel) * pixel;
      t= ((t + (pixel - 1)) / pixel) * pixel;
    }
    SI X1= x1-(l>>1), X2= x2+(r>>1);
    SI Y1= y1-(b>>1), Y2= y2+(t>>1);
    dev->set_color (fg);
    dev->fill (X1  , Y1  , X1+l, Y2  );
    dev->fill (X2-r, Y1  , X2  , Y2  );
    dev->fill (X1  , Y1  , X2  , Y1+b);
    dev->fill (X1  , Y2-t, X2  , Y2  );
  }
}

/******************************************************************************
* action boxes
******************************************************************************/

struct action_box_rep: public change_box_rep {
  tree    filter; // which events are accepted ?
  command cmd;    // command to be executed
  path    vip;    // store this location before execution
  action_box_rep (path ip, box b, tree f, command c, bool ch, path vip);
  operator tree () { return tree (TUPLE, "action", bs[0]); }
  tree action (tree t, SI x, SI y, SI delta);
};

action_box_rep::action_box_rep (
  path ip, box b, tree filter2, command cmd2, bool child_flag, path vip2):
  change_box_rep (ip, child_flag), filter (filter2), cmd (cmd2), vip (vip2)
{
  insert (b, 0, 0);
  position ();
  left_justify ();
  finalize ();
}

tree
action_box_rep::action (tree t, SI x, SI y, SI delta) {
  if (t == filter) {
    call ("set-action-path", reverse (vip));
    cmd ();
    return "done";
  }
  return change_box_rep::action (t, x, y, delta);
}

/******************************************************************************
* tag boxes
******************************************************************************/

struct tag_box_rep: public change_box_rep {
  string name;
  tag_box_rep (path ip, box b, string name);
  operator tree () { return tree (TUPLE, "tag", bs[0]); }
  tree tag (tree t, SI x, SI y, SI delta);
  void collect_page_numbers (hashmap<string,tree>& h, tree page);
  path find_tag (string name);
};

tag_box_rep::tag_box_rep (path ip, box b, string name2):
  change_box_rep (ip, false), name (name2)
{
  insert (b, 0, 0);
  position ();
  left_justify ();
  finalize ();
}

void
tag_box_rep::collect_page_numbers (hashmap<string,tree>& h, tree page) {
  h (name)= page;
  bs[0]->collect_page_numbers (h, page);
}

path
tag_box_rep::find_tag (string search) {
  if (name == search)
    return reverse (descend_decode (ip, 1));
  return path ();
}

/******************************************************************************
* box construction routines
******************************************************************************/

box
move_box (path ip, box b, SI x, SI y, bool child_flag, bool big_flag) {
  return new move_box_rep (ip, b, x, y, child_flag, big_flag);
}

box
resize_box (path ip, box b, SI x1, SI y1, SI x2, SI y2,
	    bool child_flag, bool adjust) {
  return new resize_box_rep (ip, b, x1, y1, x2, y2, child_flag, adjust);
}

box
vcorrect_box (path ip, box b, SI top_cor, SI bot_cor) {
  return new vcorrect_box_rep (ip, b, top_cor, bot_cor);
}

box
repeat_box (path ip, box ref, box repeat, SI xoff) {
  return new repeat_box_rep (ip, ref, repeat, xoff);
}

box
cell_box (path ip, box b, SI x0, SI y0, SI x1, SI y1, SI x2, SI y2,
	  SI bl, SI br, SI bb, SI bt, color fg, color bg, bool trp)
{
  box cb= new cell_box_rep (ip, b, x0, y0, x1, y1, x2, y2,
			    bl, br, bb, bt, fg, bg, trp);
  return cb;
}

box
action_box (path ip, box b, tree filter, command cmd, bool ch, path vip) {
  return new action_box_rep (ip, b, filter, cmd, ch, vip);
}

box
action_box (path ip, box b, tree filter, command cmd, bool ch) {
  return new action_box_rep (ip, b, filter, cmd, ch, decorate ());
}

box
tag_box (path ip, box b, string name) {
  return new tag_box_rep (ip, b, name);
}
