
/******************************************************************************
* MODULE     : modifier.cpp
* DESCRIPTION: a modifier box modifies the behaviour of another box
*              as to cursor movements etc. , but displays in the same way
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Boxes/modifier.hpp"
#include "Boxes/composite.hpp"

/******************************************************************************
* Modifier boxes
******************************************************************************/

modifier_box_rep::modifier_box_rep (path ip, box b2):
  box_rep (ip), b (b2)
{
  x1= b->x1; y1= b->y1;
  x2= b->x2; y2= b->y2;
  x3= b->x3; y3= b->y3;
  x4= b->x4; y4= b->y4;
}

modifier_box_rep::~modifier_box_rep () {}

int
modifier_box_rep::subnr () {
  return 1;
}

box
modifier_box_rep::subbox (int i) { (void) i;
  return b;
}

void
modifier_box_rep::display (ps_device dev) {
  (void) dev;
}

tree
modifier_box_rep::action (tree t, SI x, SI y, SI delta) {
  return b->action (t, x, y, delta);
}

void
modifier_box_rep::loci (SI x, SI y, SI delta,
			list<string>& ids, rectangles& rs)
{
  return b->loci (x, y, delta, ids, rs);
}

void
modifier_box_rep::collect_page_numbers (hashmap<string,tree>& h, tree page) {
  b->collect_page_numbers (h, page);
}

path
modifier_box_rep::find_tag (string name) {
  return b->find_tag (name);
}

modifier_box_rep::operator tree () {
  return (tree) b;
}

double modifier_box_rep::left_slope () { return b->left_slope (); }
double modifier_box_rep::right_slope () { return b->right_slope (); }
SI modifier_box_rep::left_correction () { return b->left_correction (); }
SI modifier_box_rep::right_correction () { return b->right_correction (); }
SI modifier_box_rep::lsub_correction () {
  return b->lsub_correction (); }
SI modifier_box_rep::lsup_correction () {
  return b->lsup_correction (); }
SI modifier_box_rep::rsub_correction () {
  return b->rsub_correction (); }
SI modifier_box_rep::rsup_correction () {
  return b->rsup_correction (); }
SI modifier_box_rep::sub_lo_base (int level) {
  return b->sub_lo_base (level); }
SI modifier_box_rep::sub_hi_lim  (int level) {
  return b->sub_hi_lim (level); }
SI modifier_box_rep::sup_lo_lim  (int level) {
  return b->sup_lo_lim (level); }
SI modifier_box_rep::sup_lo_base (int level) {
  return b->sup_lo_base (level); }
SI modifier_box_rep::sup_hi_lim  (int level) {
  return b->sup_hi_lim (level); }

/******************************************************************************
* New routines concerning the cursor
******************************************************************************/

path
modifier_box_rep::find_box_path (SI x, SI y, SI delta, bool force) {
  return path (0, b->find_box_path (x, y, delta, force));
}

path
modifier_box_rep::find_lip () {
  return b->find_lip ();
}

path
modifier_box_rep::find_rip () {
  return b->find_rip ();
}

path
modifier_box_rep::find_left_box_path () {
  return path (0, b->find_left_box_path ());
}

path
modifier_box_rep::find_right_box_path () {
  return path (0, b->find_right_box_path ());
}

path
modifier_box_rep::find_box_path (path p, bool& found) {
  return path (0, b->find_box_path (p, found));
}

path
modifier_box_rep::find_tree_path (path bp) {
  return b->find_tree_path (bp->next);
}

cursor
modifier_box_rep::find_cursor (path bp) {
  return b->find_cursor (bp->next);
}

selection
modifier_box_rep::find_selection (path lbp, path rbp) {
  return b->find_selection (lbp->next, rbp->next);
}

gr_selections
modifier_box_rep::graphical_select (SI x, SI y, SI dist) {
  return b->graphical_select (x- sx(0), y- sy(0), dist);
}

gr_selections
modifier_box_rep::graphical_select (SI x1, SI y1, SI x2, SI y2) {
  return b->graphical_select (x1- sx(0), y1- sy(0), x2- sx(0), y2- sy(0));
}

/******************************************************************************
* Animations
******************************************************************************/

int modifier_box_rep::anim_length () { return b->anim_length (); }
bool modifier_box_rep::anim_started () { return b->anim_started (); }
bool modifier_box_rep::anim_finished () { return b->anim_finished (); }
void modifier_box_rep::anim_finish_now () { b->anim_finish_now (); }
void modifier_box_rep::anim_start_at (time_t at) { b->anim_start_at (at); }
void modifier_box_rep::anim_get_invalid (bool& f, time_t& at, rectangles& rs) {
  b->anim_get_invalid (f, at, rs); }

/******************************************************************************
* Symbol boxes
******************************************************************************/

class symbol_box_rep: public modifier_box_rep {
  int n;
public:
  symbol_box_rep (path ip, box b, int n);
  operator tree () { return tree (TUPLE, "symbol", subbox(0)); }
  path find_box_path (SI x, SI y, SI delta, bool force);
};

symbol_box_rep::symbol_box_rep (path ip, box b2, int n2):
  modifier_box_rep (ip, b2), n (n2) {}

static box
subbox (box b, path p) {
  if (nil (p)) return b;
  return subbox (b[p->item], p->next);
}

path
symbol_box_rep::find_box_path (SI x, SI y, SI delta, bool force) {
  path p= modifier_box_rep::find_box_path (x, y, delta, force);
  box leaf= ::subbox (box (this), path_up (p));
  if (is_accessible (leaf->ip) || force) {
    if (last_item (p) <= (n>>1)) return path_up (p) * 0;
    else return path_up (p) * n;
  }
  else return p;
}

/******************************************************************************
* Shorter boxes are used for hyphenation
******************************************************************************/

class shorter_box_rep: public modifier_box_rep {
  int pos, len;
public:
  shorter_box_rep (path ip, box b, int len);
  operator tree () { return tuple ("shorter", subbox(0)); }
  path   find_box_path (SI x, SI y, SI delta, bool force);
  path   find_rip ();
  path   find_right_box_path ();
  int    get_leaf_left_pos ();
  int    get_leaf_right_pos ();
  string get_leaf_string ();
  font   get_leaf_font ();
  color  get_leaf_color ();
  SI     get_leaf_offset (string search);
};

shorter_box_rep::shorter_box_rep (path ip, box b2, int len2):
  modifier_box_rep (ip, b2), pos (b->get_leaf_left_pos ()), len (len2) {}

path
shorter_box_rep::find_box_path (SI x, SI y, SI delta, bool force) {
  path p= modifier_box_rep::find_box_path (x, y, delta, force);
  box leaf= ::subbox (box (this), path_up (p));
  if ((is_accessible (leaf->ip) || force) && (last_item (p) > len))
    return path_up (p) * len;
  else return p;
}

path
shorter_box_rep::find_rip () {
  path p= modifier_box_rep::find_rip ();
  if (is_accessible (ip) && (!nil(p)) && (p->item > (pos+ len)))
    return descend (p->next, pos+ len);
  else return p;  
}

path
shorter_box_rep::find_right_box_path () {
  path bp= b->find_right_box_path ();
  return path (0, path_up (bp) * min (last_item (bp), len));
}

int
shorter_box_rep::get_leaf_left_pos () {
  return pos;
}

int
shorter_box_rep::get_leaf_right_pos () {
  return pos+ len;
}

string
shorter_box_rep::get_leaf_string () {
  return b->get_leaf_string () (0, len);
}

font
shorter_box_rep::get_leaf_font () {
  return b->get_leaf_font ();
}

color
shorter_box_rep::get_leaf_color () {
  return b->get_leaf_color ();
}

SI
shorter_box_rep::get_leaf_offset (string search) {
  return b->get_leaf_offset (search);
}

/******************************************************************************
* Frozen boxes
******************************************************************************/

class frozen_box_rep: public modifier_box_rep {
public:
  frozen_box_rep (path ip, box b);
  operator tree () { return tree (TUPLE, "frozen", subbox(0)); }
  path find_lip ();
  path find_rip ();
};

frozen_box_rep::frozen_box_rep (path ip, box b2):
  modifier_box_rep (ip, b2) {}

path
frozen_box_rep::find_lip () {
  return box_rep::find_lip ();
}

path
frozen_box_rep::find_rip () {
  return box_rep::find_rip ();
}

/******************************************************************************
* macro expansions
******************************************************************************/

struct macro_box_rep: public composite_box_rep {
  font big_fn; // big character font if non nil
  macro_box_rep (path ip, box b, font big_fn);
  operator tree () { return tree (TUPLE, "macro", (tree) bs[0]); }

  int       find_child (SI x, SI y, SI delta, bool force);
  path      find_box_path (SI x, SI y, SI delta, bool force);
  path      find_lip ();
  path      find_rip ();
  path      find_box_path (path p, bool& found);
  path      find_tree_path (path bp);
  cursor    find_cursor (path bp);
  selection find_selection (path lbp, path rbp);
  string    get_leaf_string ();
  font      get_leaf_font ();
  color     get_leaf_color ();
  SI        get_leaf_offset (string search);

  double left_slope () { return bs[0]->left_slope(); }
  double right_slope () { return bs[0]->right_slope(); }
  SI left_correction () { return bs[0]->left_correction(); }
  SI right_correction () { return bs[0]->right_correction(); }
  SI lsub_correction () { return bs[0]->lsub_correction(); }
  SI lsup_correction () { return bs[0]->lsup_correction(); }
  SI rsub_correction () { return bs[0]->rsub_correction(); }
  SI rsup_correction () { return bs[0]->rsup_correction(); }
  SI sub_lo_base (int l) {
    // second test separates small and large big operators
    return (!nil (big_fn)) && ((y2-y1) <= 3*big_fn->yx)?
      y1 - (l>0? 0: big_fn->yshift): box_rep::sub_lo_base (l); }
  SI sub_hi_lim (int l) {
    // second test separates small and large size big operators
    return (!nil (big_fn)) && ((y2-y1) <= 3*big_fn->yx)?
      y1 - (l>0? 0: big_fn->yshift) +
        bs[0]->sub_hi_lim (l) - bs[0]->sub_lo_base (l):
      box_rep::sub_hi_lim (l); }
  SI sup_lo_base (int l) {
    if (nil (big_fn)) return box_rep::sup_lo_base (l);
    SI syx= big_fn->yx * script (big_fn->size, 1) / big_fn->size;
    if ((y2-y1) <= 3*big_fn->yx) syx -= (l<0? 0: big_fn->yshift);
    return y2- syx; }
};

macro_box_rep::macro_box_rep (path ip, box b, font fn):
  composite_box_rep (ip), big_fn (fn) {
    insert (b, 0, 0); position (); finalize (); }
int macro_box_rep::find_child (SI x, SI y, SI delta, bool force) {
  (void) x; (void) y; (void) delta; (void) force; return -1; }
path macro_box_rep::find_box_path (SI x, SI y, SI delta, bool force) {
  return box_rep::find_box_path (x, y, delta, force); }
path macro_box_rep::find_lip () {
  return box_rep::find_lip (); }
path macro_box_rep::find_rip () {
  return box_rep::find_rip (); }
path macro_box_rep::find_box_path (path p, bool& found) {
  return box_rep::find_box_path (p, found); }
path macro_box_rep::find_tree_path (path bp) {
  return box_rep::find_tree_path (bp); }
cursor macro_box_rep::find_cursor (path bp) {
  return box_rep::find_cursor (bp); }
selection macro_box_rep::find_selection (path lbp, path rbp) {
  return box_rep::find_selection (lbp, rbp); }
string macro_box_rep::get_leaf_string () {
  return bs[0]->get_leaf_string (); }
font macro_box_rep::get_leaf_font () {
  return bs[0]->get_leaf_font (); }
color macro_box_rep::get_leaf_color () {
  return bs[0]->get_leaf_color (); }
SI macro_box_rep::get_leaf_offset (string search) {
  return bs[0]->get_leaf_offset (search); }

/******************************************************************************
* box construction routines
******************************************************************************/

box
symbol_box (path ip, box b, int n) {
  return new symbol_box_rep (ip, b, n);
}

box
shorter_box (path ip, box b, int len) {
  return new shorter_box_rep (ip, b, len);
}

box
frozen_box (path ip, box b) {
  return new frozen_box_rep (ip, b);
}

box
macro_box (path ip, box b, font big_fn) {
  return new macro_box_rep (ip, b, big_fn);
}
