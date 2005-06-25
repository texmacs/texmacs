
/******************************************************************************
* MODULE     : animate_boxes.cpp
* DESCRIPTION: animations
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Boxes/composite.hpp"
#include "Boxes/construct.hpp"
#include "timer.hpp"

/******************************************************************************
* Animations which remain constant for a fixed duration
******************************************************************************/

struct anim_constant_box_rep: public composite_box_rep {
  bool   started;
  time_t started_at;
  bool   finished;
  int    length;

  anim_constant_box_rep (path ip, box b, int length);
  operator tree () { return tree (TUPLE, "anim_constant", (tree) bs[0]); }

  void pre_display (ps_device& dev);
  int  anim_length () { return length; }
  bool anim_started () { return started; }
  bool anim_finished () { return finished; }
  void anim_start_at (time_t at);
  void anim_finish_now ();
  void anim_get_invalid (time_t& at, rectangles& rs);
};

anim_constant_box_rep::anim_constant_box_rep (path ip, box b, int length2):
  composite_box_rep (ip), length (length2)
{
  insert (b, 0, 0);
  position ();
  finalize ();
  started= finished= false;
}

void
anim_constant_box_rep::pre_display (ps_device& dev) {
  if (!started) anim_start_at (texmacs_time ());
  else if (!finished)
    finished= (texmacs_time () - (started_at+length) >= 0);
}

void
anim_constant_box_rep::anim_start_at (time_t at) {
  started   = true;
  started_at= at;
  finished  = (length == 0);
  bs[0]->anim_start_at (at);
}

void
anim_constant_box_rep::anim_finish_now () {
  bs[0]->anim_finish_now ();
  started= finished= true;
}

void
anim_constant_box_rep::anim_get_invalid (time_t& at, rectangles& rs) {
  if (started && !finished) {
    bs[0]->anim_get_invalid (at, rs);
    time_t finish_at= started_at + length;
    if (at - texmacs_time () < 0 || finish_at - (at - 3) < 0) {
      at= finish_at;
      rs= rectangle (x1, y1, x2, y2);
    }
    else if (finish_at - (at + 3) <= 0)
      rs << rectangle (x1, y1, x2, y2);
  }
}

/******************************************************************************
* Compositions of animations
******************************************************************************/

class anim_compose_box_rep: public box_rep {
public:
  bool       started;
  time_t     started_at;
  int        current;
  bool       finished;
  array<box> bs;
  array<int> cum_len;

  anim_compose_box_rep (path ip, array<box> bs);
  ~anim_compose_box_rep ();

  int       subnr () { return 1; }
  box       subbox (int i) { (void) i; return bs[current]; }
  void      display (ps_device dev) { (void) dev; }
  operator  tree () { return tree ("composed animation"); }
  tree      action (tree t, SI x, SI y, SI delta);
  void      collect_page_numbers (hashmap<string,tree>& h, tree page);
  path      find_tag (string name);

  void      pre_display (ps_device& dev);
  int       anim_length () { return cum_len[N(bs)-1]; }
  bool      anim_started () { return started; }
  bool      anim_finished () { return finished; }
  void      anim_start_at (time_t at);
  void      anim_finish_now ();
  void      anim_get_invalid (time_t& at, rectangles& rs);

  path          find_box_path (SI x, SI y, SI delta, bool force);
  path          find_box_path (path p, bool& found);
  path          find_tree_path (path bp);
  cursor        find_cursor (path bp);
  selection     find_selection (path lbp, path rbp);
  gr_selections graphical_select (SI x, SI y, SI dist);
};

/******************************************************************************
* Composition of animations / basic routines
******************************************************************************/

anim_compose_box_rep::anim_compose_box_rep (path ip, array<box> bs2):
  box_rep (ip), bs (bs2), cum_len (N(bs))
{
  if (N(bs) == 0)
    fatal_error ("Empty animation",
		 "anim_compose_box_rep::anim_compose_box_rep");

  started = false;
  finished= false;
  current = 0;

  int i, n= N(bs);
  x1= y1= x3= y3= MAX_SI;
  x2= y2= x4= y4= -MAX_SI;
  for (i=0; i<n; i++) {
    x1= min (x1, sx1(i));
    y1= min (y1, sy1(i));
    x2= max (x2, sx2(i));
    y2= max (y2, sy2(i));
    x3= min (x3, sx3(i));
    y3= min (y3, sy3(i));
    x4= max (x4, sx4(i));
    y4= max (y4, sy4(i));
  }

  int len= 0;
  for (i=0; i<n; i++) {
    int sl= bs[i]->anim_length ();
    if (sl == -1) len= -1;
    if (len != -1) len += sl;
    cum_len[i]= len;
  }
}

anim_compose_box_rep::~anim_compose_box_rep () {}

tree
anim_compose_box_rep::action (tree t, SI x, SI y, SI delta) {
  return bs[current]->action (t, x, y, delta);
}

void
anim_compose_box_rep::collect_page_numbers (
  hashmap<string,tree>& h, tree page)
{
  bs[current]->collect_page_numbers (h, page);
}

path
anim_compose_box_rep::find_tag (string name) {
  return bs[current]->find_tag (name);
}

/******************************************************************************
* Compositions of animations / animation routines
******************************************************************************/

void
anim_compose_box_rep::pre_display (ps_device& dev) {
  if (!started) anim_start_at (texmacs_time ());
  else if (!finished) {
    int    cur= current;
    time_t now= texmacs_time ();
    while (current < N(bs) && now - (started_at+cum_len[current]) >= 0) {
      bs[current]->anim_finish_now ();
      current++;
    }
    if (current == N(bs)) {
      finished= true;
      current--;
    }
    else if (current != cur)
      bs[current]->anim_start_at (started_at + cum_len[current-1]);
  }
}

void
anim_compose_box_rep::anim_start_at (time_t at) {
  started   = true;
  started_at= at;
  finished  = (anim_length () == 0);
  current   = 0;
  bs[current]->anim_start_at (at);
}

void
anim_compose_box_rep::anim_finish_now () {
  int i, n= N(bs);
  for (i=current; i<n; i++)
    bs[i]->anim_finish_now ();
  current= n-1;
  started= finished= true;
}

void
anim_compose_box_rep::anim_get_invalid (time_t& at, rectangles& rs) {
  if (started && !finished) {
    bs[current]->anim_get_invalid (at, rs);
    time_t finish_at= started_at + cum_len[current];
    if (at - texmacs_time () < 0 || finish_at - (at - 3) < 0) {
      at= finish_at;
      rs= rectangle (x1, y1, x2, y2);
    }
    else if (finish_at - (at + 3) <= 0)
      rs << rectangle (x1, y1, x2, y2);
  }

}

/******************************************************************************
* Compositions of animations / cursor routines
******************************************************************************/

path
anim_compose_box_rep::find_box_path (SI x, SI y, SI delta, bool force) {
  if (outside (x, delta, x1, x2) && (is_accessible (ip) || force))
    return box_rep::find_box_path (x, y, delta, force);
  return path (0, bs[current]->find_box_path (x, y, delta, force));
}

path
anim_compose_box_rep::find_box_path (path p, bool& found) {
  path r= path (0, bs[current]->find_box_path (p, found));
  if (found) return r;
  return box_rep::find_box_path (p, found);
}

path
anim_compose_box_rep::find_tree_path (path bp) {
  if (atom (bp)) return box_rep::find_tree_path (bp);
  return bs[current]->find_tree_path (bp->next);
}

cursor
anim_compose_box_rep::find_cursor (path bp) {
  if (atom (bp)) return box_rep::find_cursor (bp);
  else return bs[current]->find_cursor (bp->next);
}

selection
anim_compose_box_rep::find_selection (path lbp, path rbp) {
  if (!atom (lbp) && !atom (rbp))
    return bs[current]->find_selection (lbp->next, rbp->next);
  else return box_rep::find_selection (lbp, rbp);
}

gr_selections
anim_compose_box_rep::graphical_select (SI x, SI y, SI dist) {
  return bs[current]->graphical_select (x- sx(0), y- sy(0), dist);
}

/******************************************************************************
* Animations which are repeated ad infinam
******************************************************************************/

struct anim_repeat_box_rep: public composite_box_rep {
  bool   started;
  time_t started_at;
  int    length;

  anim_repeat_box_rep (path ip, box b);
  operator tree () { return tree (TUPLE, "anim_repeat", (tree) bs[0]); }

  void pre_display (ps_device& dev);
  int  anim_length () { return -1; }
  bool anim_started () { return started; }
  bool anim_finished () { return false; }
  void anim_start_at (time_t at);
  void anim_finish_now () {}
  void anim_get_invalid (time_t& at, rectangles& rs);
};

anim_repeat_box_rep::anim_repeat_box_rep (path ip, box b):
  composite_box_rep (ip)
{
  insert (b, 0, 0);
  position ();
  finalize ();
  started = false;
  length  = b->anim_length ();
}

void
anim_repeat_box_rep::pre_display (ps_device& dev) {
  if (!started) anim_start_at (texmacs_time ());
  else if (length > 0) {
    time_t now= texmacs_time ();
    if (now - (started_at+length) >= 0) {
      bs[0]->anim_finish_now ();
      while (now - (started_at+length) >= 0)
	started_at += length;
      bs[0]->anim_start_at (started_at);
    }
  }
}

void
anim_repeat_box_rep::anim_start_at (time_t at) {
  started= true;
  started_at= at;
  bs[0]->anim_start_at (at);
}

void
anim_repeat_box_rep::anim_get_invalid (time_t& at, rectangles& rs) {
  if (started) bs[0]->anim_get_invalid (at, rs);
}

/******************************************************************************
* box construction routines
******************************************************************************/

box
anim_constant_box (path ip, box b, int len) {
  return new anim_constant_box_rep (ip, b, len);
}

box
anim_compose_box (path ip, array<box> bs) {
  return new anim_compose_box_rep (ip, bs);
}

box
anim_repeat_box (path ip, box b) {
  return new anim_repeat_box_rep (ip, b);
}
