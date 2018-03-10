
/******************************************************************************
* MODULE     : modifier.cpp
* DESCRIPTION: a modifier box modifies the behaviour of another box
*              as to cursor movements etc. , but displays in the same way
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Boxes/modifier.hpp"
#include "Boxes/composite.hpp"
#include "Boxes/construct.hpp"

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
modifier_box_rep::display (renderer ren) {
  (void) ren;
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
SI modifier_box_rep::wide_correction (int mode) {
  return b->wide_correction (mode); }
void modifier_box_rep::get_bracket_extents (SI& lo, SI& hi) {
  b->get_bracket_extents (lo, hi); }

/******************************************************************************
* New routines concerning the cursor
******************************************************************************/

path
modifier_box_rep::find_box_path (SI x, SI y, SI delta,
                                 bool force, bool& found) {
  return path (0, b->find_box_path (x, y, delta, force, found));
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

player modifier_box_rep::anim_player () { return b->anim_player (); }
double modifier_box_rep::anim_delay () { return b->anim_delay (); }
double modifier_box_rep::anim_duration () { return b->anim_duration (); }
void   modifier_box_rep::anim_position (double t) { b->anim_position (t); }
double modifier_box_rep::anim_next () { return b->anim_next (); }
rectangles modifier_box_rep::anim_invalid () { return b->anim_invalid (); }

/******************************************************************************
* Symbol boxes
******************************************************************************/

class symbol_box_rep: public modifier_box_rep {
  int n;
public:
  symbol_box_rep (path ip, box b, int n);
  operator tree () { return tree (TUPLE, "symbol", subbox(0)); }
  box    adjust_kerning (int mode, double factor);
  box    expand_glyphs (int mode, double factor);
  path   find_box_path (SI x, SI y, SI delta, bool force, bool& found);
};

symbol_box_rep::symbol_box_rep (path ip, box b2, int n2):
  modifier_box_rep (ip, b2), n (n2) {}

box
symbol_box_rep::adjust_kerning (int mode, double factor) {
  return symbol_box (ip, b->adjust_kerning (mode, factor), n);
}

box
symbol_box_rep::expand_glyphs (int mode, double factor) {
  return symbol_box (ip, b->expand_glyphs (mode, factor), n);
}

static box
subbox (box b, path p) {
  if (is_nil (p)) return b;
  return subbox (b[p->item], p->next);
}

path
symbol_box_rep::find_box_path (SI x, SI y, SI delta, bool force, bool& found) {
  path p= modifier_box_rep::find_box_path (x, y, delta, force, found);
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
  box    adjust_kerning (int mode, double factor);
  box    expand_glyphs (int mode, double factor);
  path   find_box_path (SI x, SI y, SI delta, bool force, bool& found);
  path   find_rip ();
  path   find_right_box_path ();
  int    get_type ();
  int    get_leaf_left_pos ();
  int    get_leaf_right_pos ();
  string get_leaf_string ();
  font   get_leaf_font ();
  pencil get_leaf_pencil ();
  SI     get_leaf_offset (string search);
};

shorter_box_rep::shorter_box_rep (path ip, box b2, int len2):
  modifier_box_rep (ip, b2), pos (b->get_leaf_left_pos ()), len (len2) {}

box
shorter_box_rep::adjust_kerning (int mode, double factor) {
  return shorter_box (ip, b->adjust_kerning (mode, factor), len);
}

box
shorter_box_rep::expand_glyphs (int mode, double factor) {
  return shorter_box (ip, b->expand_glyphs (mode, factor), len);
}

path
shorter_box_rep::find_box_path (SI x, SI y, SI delta, bool force, bool& found) {
  path p= modifier_box_rep::find_box_path (x, y, delta, force, found);
  box leaf= ::subbox (box (this), path_up (p));
  if ((is_accessible (leaf->ip) || force) && (last_item (p) > len))
    return path_up (p) * len;
  else return p;
}

path
shorter_box_rep::find_rip () {
  path p= modifier_box_rep::find_rip ();
  if (is_accessible (ip) && (!is_nil(p)) && (p->item > (pos+ len)))
    return descend (p->next, pos+ len);
  else return p;  
}

path
shorter_box_rep::find_right_box_path () {
  path bp= b->find_right_box_path ();
  return path (0, path_up (bp) * min (last_item (bp), len));
}

int
shorter_box_rep::get_type () {
  return SHORTER_BOX;
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

pencil
shorter_box_rep::get_leaf_pencil () {
  return b->get_leaf_pencil ();
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
  box  adjust_kerning (int mode, double factor);
  box  expand_glyphs (int mode, double factor);
  path find_lip ();
  path find_rip ();
};

frozen_box_rep::frozen_box_rep (path ip, box b2):
  modifier_box_rep (ip, b2) {}

box
frozen_box_rep::adjust_kerning (int mode, double factor) {
  return frozen_box (ip, b->adjust_kerning (mode, factor));
}

box
frozen_box_rep::expand_glyphs (int mode, double factor) {
  return frozen_box (ip, b->expand_glyphs (mode, factor));
}

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
  int  btype;
  macro_box_rep (path ip, box b, font big_fn, int btype);
  operator tree () { return tree (TUPLE, "macro", (tree) bs[0]); }
  box adjust_kerning (int mode, double factor);
  box expand_glyphs (int mode, double factor);

  int       find_child (SI x, SI y, SI delta, bool force);
  path      find_box_path (SI x, SI y, SI delta, bool force, bool& found);
  path      find_lip ();
  path      find_rip ();
  path      find_box_path (path p, bool& found);
  path      find_tree_path (path bp);
  cursor    find_cursor (path bp);
  selection find_selection (path lbp, path rbp);
  int       get_type ();
  string    get_leaf_string ();
  font      get_leaf_font ();
  pencil    get_leaf_pencil ();
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
    return (!is_nil (big_fn)) && ((y2-y1) <= 3*big_fn->yx)?
      y1 - (l>0? 0: big_fn->yshift): box_rep::sub_lo_base (l); }
  SI sub_hi_lim (int l) {
    // second test separates small and large size big operators
    return (!is_nil (big_fn)) && ((y2-y1) <= 3*big_fn->yx)?
      y1 - (l>0? 0: big_fn->yshift) +
        bs[0]->sub_hi_lim (l) - bs[0]->sub_lo_base (l):
      box_rep::sub_hi_lim (l); }
  SI sup_lo_base (int l) {
    if (is_nil (big_fn)) return box_rep::sup_lo_base (l);
    SI syx= big_fn->yx * script (big_fn->size, 1) / big_fn->size;
    if ((y2-y1) <= 3*big_fn->yx) syx -= (l<0? 0: big_fn->yshift);
    return y2- syx; }
  SI wide_correction (int mode) { return bs[0]->wide_correction (mode); }
};

macro_box_rep::macro_box_rep (path ip, box b, font fn, int bt):
  composite_box_rep (ip), big_fn (fn), btype (bt) {
    insert (b, 0, 0); position (); finalize (); }
box macro_box_rep::adjust_kerning (int mode, double factor) {
  return macro_box (ip, bs[0]->adjust_kerning (mode, factor), big_fn); }
box macro_box_rep::expand_glyphs (int mode, double factor) {
  return macro_box (ip, bs[0]->expand_glyphs (mode, factor), big_fn); }
int macro_box_rep::find_child (SI x, SI y, SI delta, bool force) {
  (void) x; (void) y; (void) delta; (void) force; return -1; }
path macro_box_rep::find_box_path (SI x, SI y, SI delta, bool force, bool& f) {
  return box_rep::find_box_path (x, y, delta, force, f); }
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
int macro_box_rep::get_type () {
  return btype; }
string macro_box_rep::get_leaf_string () {
  return bs[0]->get_leaf_string (); }
font macro_box_rep::get_leaf_font () {
  return bs[0]->get_leaf_font (); }
pencil macro_box_rep::get_leaf_pencil () {
  return bs[0]->get_leaf_pencil (); }
SI macro_box_rep::get_leaf_offset (string search) {
  return bs[0]->get_leaf_offset (search); }

/******************************************************************************
* box construction routines
******************************************************************************/

box
symbol_box (path ip, box b, int n) {
  return tm_new<symbol_box_rep> (ip, b, n);
}

box
shorter_box (path ip, box b, int len) {
  return tm_new<shorter_box_rep> (ip, b, len);
}

box
frozen_box (path ip, box b) {
  return tm_new<frozen_box_rep> (ip, b);
}

box
macro_box (path ip, box b, font big_fn, int btype) {
  return tm_new<macro_box_rep> (ip, b, big_fn, btype);
}
