
/******************************************************************************
* MODULE     : boxes.hpp
* DESCRIPTION: the low level box structure
*              and formatting routines
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef BOXES_H
#define BOXES_H
#include "basic.hpp"
#include "rectangles.hpp"
#include "path.hpp"
#include "renderer.hpp"
#include "tm_timer.hpp"
#include "font.hpp"
#include "language.hpp"
#include "hashmap.hpp"
#include "frame.hpp"
#include "grid.hpp"

#define STD_BOX       0
#define STACK_BOX     1
#define CONTROL_BOX   2
#define MOVE_BOX      3
#define SCROLL_BOX    4
#define TEXT_BOX      5
#define SHORTER_BOX   6
#define BIG_OP_BOX    7

class player;

/******************************************************************************
* The cursor class
******************************************************************************/

struct cursor_rep: concrete_struct {
  SI ox, oy;    // main cursor position
  SI delta;     // infinitesimal shift to the right
  SI y1;        // under base line
  SI y2;        // upper base line
  double slope; // slope of cursor
  bool valid;   // the cursor is valid
};

struct cursor {
  CONCRETE(cursor);
  cursor (SI x=0, SI y=0, SI delta=0, SI y1=0, SI y2=0,
	  double slope=0.0, bool valid=true);
};
CONCRETE_CODE(cursor);

cursor copy (cursor cu);
bool operator == (cursor cu1, cursor cu2);
bool operator != (cursor cu1, cursor cu2);
tm_ostream& operator << (tm_ostream& out, cursor cu);

/******************************************************************************
* The selection class
******************************************************************************/

struct selection_rep: concrete_struct {
  rectangles rs;
  path start;
  path end;
  bool valid;
};

struct selection {
  CONCRETE(selection);
  selection (rectangles rs= rectangles(),
	     path start= path(), path end= path (),
	     bool valid= true);
};
CONCRETE_CODE(selection);

bool operator == (selection sel1, selection sel2);
bool operator != (selection sel1, selection sel2);
tm_ostream& operator << (tm_ostream& out, selection sel);

/******************************************************************************
* The graphical selection class
******************************************************************************/

struct gr_selection_rep;
struct gr_selection {
  CONCRETE(gr_selection);
  gr_selection (array<path> cp= array<path> (), SI dist= 0);
};

tm_ostream& operator << (tm_ostream& out, gr_selection sel);

typedef array<gr_selection> gr_selections;
void sort (gr_selections& sels);
tree as_tree (gr_selections sels);

/******************************************************************************
* The box class
******************************************************************************/

class box_rep;
struct lazy;
typedef array<double> point;

class box {
  ABSTRACT_NULL(box);
  inline box operator [] (int i);
  box operator [] (path p);
  operator tree ();
  bool operator == (box b2);
  bool operator != (box b2);
  friend inline int N (box b);
};

class box_rep: public abstract_struct {
private:
  SI x0, y0;    // offset w.r.t. parent box

public:
  SI x1, y1;    // under left corner (logical)
  SI x2, y2;    // upper right corner (logical)
  SI x3, y3;    // under left corner (ink)
  SI x4, y4;    // upper right corner (ink)

  path ip;      // corresponding inverse path in source tree

  /****************************** main routines ******************************/

  inline            box_rep (path ip);
  inline            virtual ~box_rep ();
  void              relocate (path p, bool force= false);
  virtual box	    transform (frame fr);
  virtual operator  tree () = 0;
  virtual void      pre_display (renderer& ren);
  virtual void      post_display (renderer& ren);
  virtual void      display (renderer ren) = 0;
  virtual void      display_background (renderer ren);
  virtual void      clear_incomplete (rectangles& rs, SI pixel,
				      int i, int i1, int i2);
  virtual int  subnr ();
  virtual box  subbox (int i);
  virtual tree action (tree t, SI x, SI y, SI delta);
  virtual void loci (SI x, SI y, SI d, list<string>& ids, rectangles& rs);
  virtual void display_links (renderer ren);
  virtual void position_at (SI x, SI y, rectangles& change_log);
  virtual void collect_page_numbers (hashmap<string,tree>& h, tree page);
  virtual void collect_page_colors (array<brush>& bs, array<rectangle>& rs);
  virtual path find_tag (string name);

  virtual int  reindex (int i, int item, int n);
  virtual void redraw (renderer ren, path p, rectangles& l);
  virtual void redraw_background (renderer ren);
  void redraw (renderer ren, path p, rectangles& l, SI x, SI y);
  void clear (renderer ren, SI x1, SI y1, SI x2, SI y2);

  /*************************** positioning routines **************************/

  inline SI   w ();
  inline SI   h ();
  inline SI&  sx (int i);
  inline SI&  sy (int i);
  inline SI   sx1 (int i);
  inline SI   sy1 (int i);
  inline SI   sx2 (int i);
  inline SI   sy2 (int i);
  inline SI   sx3 (int i);
  inline SI   sy3 (int i);
  inline SI   sx4 (int i);
  inline SI   sy4 (int i);
  inline bool test_in (SI x, SI y);

  inline bool accessible ();
  inline bool decoration ();

  SI distance (int i, SI x, SI y, SI delta);
  bool in_rectangle (SI x1, SI y1, SI x2, SI y2);
  bool contains_rectangle (SI x1, SI y1, SI x2, SI y2);

  virtual box  adjust_kerning (int mode, double factor);
  virtual box  expand_glyphs (int mode, double factor);
  virtual void get_cell_extents (SI& l, SI& r);
  virtual box  adjust_cell_geometry (SI dx, SI dl, SI dr);

  /******************* path conversions and cursor routines ******************/

  virtual path      find_lip ();
  virtual path      find_rip ();
  virtual path      find_left_box_path ();
  virtual path      find_right_box_path ();
  virtual path      find_box_path (SI x, SI y, SI d, bool force, bool& found);
  virtual cursor    find_cursor (path bp);
  virtual selection find_selection (path lbp, path rbp);
  virtual path      find_tree_path (path bp);
  virtual path      find_box_path (path p, bool& found);
       
  path      find_tree_path (SI x, SI y, SI delta);
  cursor    find_check_cursor (path p);
  selection find_check_selection (path lp, path rp);

  /************************ fine typesetting routines ************************/

  virtual double    left_slope ();
  virtual double    right_slope ();
  virtual SI        left_correction ();
  virtual SI        right_correction ();
  virtual SI        lsub_correction ();
  virtual SI        lsup_correction ();
  virtual SI        rsub_correction ();
  virtual SI        rsup_correction ();
  virtual SI        sub_lo_base (int level);
  virtual SI        sub_hi_lim  (int level);
  virtual SI        sup_lo_lim  (int level);
  virtual SI        sup_lo_base (int level);
  virtual SI        sup_hi_lim  (int level);
  virtual SI        wide_correction (int mode);
  virtual void      get_bracket_extents (SI& lo, SI& hi);

  /*************************** for graphical boxes ***************************/

  virtual frame     get_frame ();
  virtual grid      get_grid ();
  virtual void      get_limits (point& lim1, point& lim2);

  frame     find_frame (path bp, bool last= false);
  grid      find_grid (path bp);
  void      find_limits (path bp, point& lim1, point& lim2);

  virtual SI             graphical_distance (SI x, SI y);
  virtual gr_selections  graphical_select (SI x, SI y, SI dist);
  virtual gr_selections  graphical_select (SI x1, SI y1, SI x2, SI y2);

  /************************** retrieving information *************************/

  virtual int       get_type ();
  virtual tree      get_info (tree in);
  virtual int       get_leaf_left_pos ();
  virtual int       get_leaf_right_pos ();
  virtual string    get_leaf_string ();
  virtual font      get_leaf_font ();
  virtual pencil    get_leaf_pencil ();
  virtual language  get_leaf_language ();
  virtual tree      get_leaf_tree ();
  virtual box       get_leaf_box ();
  virtual lazy      get_leaf_lazy ();
  virtual SI        get_leaf_offset (string search);

  /******************************** animations *******************************/

  virtual player     anim_player ();
  virtual double     anim_delay ();
  virtual double     anim_duration ();
  virtual void       anim_position (double delay);
  virtual double     anim_next ();
  virtual rectangles anim_invalid ();

  /********************************* obsolete ********************************/

  friend struct page_box_rep; // temporary friends for accessing x0 and y0
  friend struct lazy_paragraph_rep;
  friend class  phrase_box_rep;
  friend class  remember_box_rep;
  friend struct effect_box_rep;
  friend void make_eps (url dest, box b, int dpi);
  friend void make_raster_image (url dest, box b, double zoom);
};
ABSTRACT_NULL_CODE(box);

extern int box_count;
inline box_rep::box_rep (path ip2):
  x0(0), y0(0), x1(0), y1(0), x2(0), y2(0), x3(0), y3(0), x4(0), y4(0),
  ip (ip2) { TM_DEBUG(box_count++); }
inline box_rep::~box_rep () { TM_DEBUG(box_count--); }
inline bool box_rep::test_in (SI x, SI y) {
  return (x>=x1) && (x<x2) && (y>=y1) && (y<y2); }
inline SI box_rep::w () { return x2-x1; }
inline SI box_rep::h () { return y2-y1; }
inline SI& box_rep::sx (int i) { return subbox(i)->x0; }
inline SI& box_rep::sy (int i) { return subbox(i)->y0; }
inline SI box_rep::sx1 (int i) { box b= subbox(i); return b->x0+ b->x1; }
inline SI box_rep::sy1 (int i) { box b= subbox(i); return b->y0+ b->y1; }
inline SI box_rep::sx2 (int i) { box b= subbox(i); return b->x0+ b->x2; }
inline SI box_rep::sy2 (int i) { box b= subbox(i); return b->y0+ b->y2; }
inline SI box_rep::sx3 (int i) { box b= subbox(i); return b->x0+ b->x3; }
inline SI box_rep::sy3 (int i) { box b= subbox(i); return b->y0+ b->y3; }
inline SI box_rep::sx4 (int i) { box b= subbox(i); return b->x0+ b->x4; }
inline SI box_rep::sy4 (int i) { box b= subbox(i); return b->y0+ b->y4; }

inline box box::operator [] (int i) { return rep->subbox(i); }
inline int N (box b) { return b.rep->subnr(); }
tm_ostream& operator << (tm_ostream& out, box b);
SI   get_delta (SI x, SI x1, SI x2);
bool outside (SI x, SI delta, SI x1, SI x2);
void make_eps (url dest, box b, int dpi= 600);
void make_raster_image (url dest, box b, double zoom);
path find_innermost_scroll (box b, path p);
path find_scrolled_tree_path (box b, path sp, SI x, SI y, SI delta);
void find_canvas_info (box b, path sp, SI& x, SI& y, SI& sx, SI& sy,
		       rectangle& outer, rectangle& inner);

#define DECORATION        (-1)
#define DECORATION_LEFT   (-2)
#define DECORATION_MIDDLE (-3)
#define DECORATION_RIGHT  (-4)
#define DETACHED          (-5)
#define is_accessible(p) ((is_nil (p)) || ((p)->item >= 0))
#define is_decoration(p) ((!is_nil (p)) && ((p)->item < 0))
inline path descend (path ip, int i) {
  return (is_nil (ip) || (ip->item >= 0))? path (i, ip): ip; }
inline path decorate () {
  return path (DECORATION); }
inline path decorate (path ip) {
  return (is_nil (ip) || (ip->item >= 0))? path (DECORATION, ip): ip; }
inline path decorate_left (path ip) {
  return (is_nil (ip) || (ip->item >= 0))? path (DECORATION_LEFT, ip): ip; }
inline path decorate_middle (path ip) {
  return (is_nil (ip) || (ip->item >= 0))? path (DECORATION_MIDDLE, ip): ip; }
inline path decorate_right (path ip) {
  return (is_nil (ip) || (ip->item >= 0))? path (DECORATION_RIGHT, ip): ip; }
path descend_decode (path ip, int side);

inline bool box_rep::accessible () { return is_accessible (find_lip ()); }
inline bool box_rep::decoration () { return is_decoration (find_lip ()); }

tree attach_dip (tree ref, path ip);
#define attach_here(t,ip) attach_dip(t,ip),ip
#define attach_deco(t,ip) attach_dip(t,decorate(ip)),decorate(ip)
#define attach_left(t,ip) attach_dip(t,decorate_left(ip)),decorate_left(ip)
#define attach_middle(t,ip) \
  attach_dip(t,decorate_middle(ip)),decorate_middle(ip)
#define attach_right(t,ip) attach_dip(t,decorate_right(ip)),decorate_right(ip)

/******************************************************************************
* The graphical selection class (continued)
******************************************************************************/

struct gr_selection_rep: concrete_struct {
  string type;
  array<path> cp;
  array<point> pts;
  point p;
  SI dist;
  curve c;
};
CONCRETE_CODE(gr_selection);

#endif // defined BOXES_H
