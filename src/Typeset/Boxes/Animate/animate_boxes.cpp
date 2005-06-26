
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
#include "../Plugins/Mplayer/mplayer.hpp"

/******************************************************************************
* Global animation tracking
******************************************************************************/

bool   refresh_needed= false;
time_t refresh_next  = 0;

void
refresh_at (time_t t) {
  time_t now= texmacs_time ();
  if (t - now < 0) t= now;
  if (refresh_needed) {
    if (refresh_next - now < 0) refresh_next= now;
    if (t - refresh_next < 0) refresh_next= t;
  }
  else {
    refresh_needed= true;
    refresh_next  = t;
  }
  //cout << "Refresh at " << t << " -> " << refresh_next << "\n";
}

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

  void   pre_display (ps_device& dev);
  int    anim_length () { return length; }
  bool   anim_started () { return started; }
  bool   anim_finished () { return finished; }
  void   anim_start_at (time_t at);
  void   anim_finish_now ();
  time_t anim_next_update () { return started_at + length; }
  void   anim_get_invalid (bool& flag, time_t& at, rectangles& rs);
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
  else if (!finished) {
    finished= (texmacs_time () - (started_at+length) >= 0);
  }
  if (!finished) refresh_at (anim_next_update ());
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
anim_constant_box_rep::anim_get_invalid (bool& f, time_t& at, rectangles& rs) {
  if (started && !finished) {
    bs[0]->anim_get_invalid (f, at, rs);
    anim_check_invalid (f, at, rs);
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

  void   pre_display (ps_device& dev);
  int    anim_length () { return cum_len[N(bs)-1]; }
  bool   anim_started () { return started; }
  bool   anim_finished () { return finished; }
  void   anim_start_at (time_t at);
  void   anim_finish_now ();
  time_t anim_next_update () { return started_at + cum_len[current]; }
  void   anim_get_invalid (bool& flag, time_t& at, rectangles& rs);

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
    x1= min (x1, bs[i]->x1);
    y1= min (y1, bs[i]->y1);
    x2= max (x2, bs[i]->x2);
    y2= max (y2, bs[i]->y2);
    x3= min (x3, bs[i]->x3);
    y3= min (y3, bs[i]->y3);
    x4= max (x4, bs[i]->x4);
    y4= max (y4, bs[i]->y4);
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
    time_t now= texmacs_time ();
    while (current < N(bs) && now - (started_at+cum_len[current]) >= 0) {
      bs[current]->anim_finish_now ();
      current++;
      if (current<N(bs))
	bs[current]->anim_start_at (started_at + cum_len[current-1]);
    }
    if (current == N(bs)) {
      finished= true;
      current--;
    }
  }
  if (!finished) refresh_at (anim_next_update ());
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
  for (i=current; i<n; i++) {
    bs[i]->anim_finish_now ();
    if (i+1 < n)
      bs[i+1]->anim_start_at (started_at + cum_len[i]);
  }
  current= n-1;
  started= finished= true;
}

void
anim_compose_box_rep::anim_get_invalid (bool& f, time_t& at, rectangles& rs) {
  if (started && !finished) {
    bs[current]->anim_get_invalid (f, at, rs);
    anim_check_invalid (f, at, rs);
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
  void anim_get_invalid (bool& flag, time_t& at, rectangles& rs);
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
anim_repeat_box_rep::anim_get_invalid (bool& f, time_t& at, rectangles& rs) {
  if (started) bs[0]->anim_get_invalid (f, at, rs);
}

/******************************************************************************
* Sound boxes
******************************************************************************/

struct sound_box_rep: public box_rep {
  url    u;
  bool   started;

  sound_box_rep (path ip, url u2, SI h):
    box_rep (ip), u (u2), started (false) { y2= h; }
  operator tree () { return tree (TUPLE, "sound", u->t); }
  void display (ps_device dev) { (void) dev; }

  void play_sound () {
    if (supports_mplayer ()) mplayer_play_sound (u);
    started= true; }
  void pre_display (ps_device& dev) {
    if (!started) anim_start_at (texmacs_time ()); }
  int  anim_length () { return 0; }
  bool anim_started () { return started; }
  bool anim_finished () { return anim_started (); }
  void anim_start_at (time_t at) { (void) at; play_sound (); }
  void anim_finish_now () { if (!started) play_sound (); }
};

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

box
sound_box (path ip, url u, SI h) {
  return new sound_box_rep (ip, u, h);
}
