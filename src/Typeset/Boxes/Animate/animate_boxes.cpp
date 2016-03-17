
/******************************************************************************
* MODULE     : animate_boxes.cpp
* DESCRIPTION: animations
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Boxes/composite.hpp"
#include "Boxes/construct.hpp"
#include "file.hpp"
#include "player.hpp"

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

struct anim_rep {
  player pl;
  double delay;
  double duration;

  anim_rep (player pl2, double duration2):
    pl (pl2), delay (0.0), duration (duration2) {}

  player anim_player () { return pl; }
  double anim_delay () { return delay; }
  double anim_duration () { return duration; }
  void   anim_position (double pos) { delay= pos; }
  double anim_time () { return pl->get_elapsed () - delay; }
};

/******************************************************************************
* Animations which remain constant for a fixed duration
******************************************************************************/

struct anim_constant_box_rep: public composite_box_rep, public anim_rep {
  bool   started;
  time_t started_at;
  bool   finished;
  int    length;

  anim_constant_box_rep (path ip, box b, player pl, int length);
  operator tree () { return tree (TUPLE, "anim_constant", (tree) bs[0]); }

  void   pre_display (renderer& ren);
  int    anim_length () { return length; }
  bool   anim_started () { return started; }
  bool   anim_finished () { return finished; }
  void   anim_start_at (time_t at);
  void   anim_finish_now ();
  time_t anim_next_update () { return started_at + length; }
  void   anim_get_invalid (bool& flag, time_t& at, rectangles& rs);
};

anim_constant_box_rep::anim_constant_box_rep (path ip, box b, player pl,
                                              int length2):
  composite_box_rep (ip), anim_rep (pl, (double) length2), length (length2)
{
  insert (b, 0, 0);
  position ();
  finalize ();
  started= finished= false;
}

void
anim_constant_box_rep::pre_display (renderer& ren) {
  (void) ren;
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

class anim_compose_box_rep: public box_rep, public anim_rep {
public:
  array<box> bs;
  array<int> cum_len;
  array<double> offsets;
  bool       started;
  time_t     started_at;
  int        current;
  bool       finished;

  anim_compose_box_rep (path ip, array<box> bs, player pl);
  ~anim_compose_box_rep ();

  int       subnr () { return 1; }
  box       subbox (int i) { (void) i; return bs[current]; }
  void      display (renderer ren) { (void) ren; }
  operator  tree () { return tree ("composed animation"); }
  tree      action (tree t, SI x, SI y, SI delta);
  void      loci (SI x, SI y, SI delta, list<string>& ids, rectangles& rs);
  void      collect_page_numbers (hashmap<string,tree>& h, tree page);
  path      find_tag (string name);

  double get_index (double t);
  rectangles anim_invalid ();

  void   pre_display (renderer& ren);
  int    anim_length () { return cum_len[N(bs)-1]; }
  bool   anim_started () { return started; }
  bool   anim_finished () { return finished; }
  void   anim_start_at (time_t at);
  void   anim_finish_now ();
  time_t anim_next_update () { return started_at + cum_len[current]; }
  void   anim_get_invalid (bool& flag, time_t& at, rectangles& rs);

  path          find_box_path (SI x, SI y, SI delta, bool force, bool& found);
  path          find_box_path (path p, bool& found);
  path          find_tree_path (path bp);
  cursor        find_cursor (path bp);
  selection     find_selection (path lbp, path rbp);
  gr_selections graphical_select (SI x, SI y, SI dist);
};

/******************************************************************************
* Composition of animations / basic routines
******************************************************************************/

anim_compose_box_rep::anim_compose_box_rep (path ip, array<box> b2, player pl):
  box_rep (ip), anim_rep (pl, 0.0),
  bs (b2), cum_len (N(bs)), offsets (N(bs))
{
  ASSERT (N(bs) != 0, "empty animation");

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
    offsets[i]= (double) len;
  }
  duration= offsets[n-1];
}

anim_compose_box_rep::~anim_compose_box_rep () {}

tree
anim_compose_box_rep::action (tree t, SI x, SI y, SI delta) {
  return bs[current]->action (t, x, y, delta);
}

void
anim_compose_box_rep::loci (SI x, SI y, SI delta,
			    list<string>& ids, rectangles& rs)
{
  bs[current]->loci (x, y, delta, ids, rs);
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

double
anim_compose_box_rep::get_index (double t) {
  for (int i=0; i+1 < N(bs); i++)
    if (t < offsets[i+1]) return i;
  return N(bs) - 1;
}

rectangles
anim_compose_box_rep::anim_invalid () {
  rectangles rs= box_rep::anim_invalid ();
  double t = anim_time ();
  int    ix= get_index (t);
  if (ix != current) rs << rectangle (x1, y1, x2, y2);
  return rs;
}

void
anim_compose_box_rep::pre_display (renderer& ren) {
  double t = anim_time ();
  int    ix= get_index (t);
  if (ix != current) {
    bs[ix]->anim_position (delay + offsets[ix]);
    if (pl->speed > 0 && ix != N(bs) -1)
      pl->request_refresh (offsets[ix+1] - t);
    if (pl->speed < 0 && ix != 0)
      pl->request_refresh (t - offsets[ix]);
    // current= ix;
  }

  (void) ren;
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
anim_compose_box_rep::find_box_path (SI x, SI y, SI delta,
                                     bool force, bool& found) {
  if (outside (x, delta, x1, x2) && (is_accessible (ip) || force))
    return box_rep::find_box_path (x, y, delta, force, found);
  return path (0, bs[current]->find_box_path (x, y, delta, force, found));
}

path
anim_compose_box_rep::find_box_path (path p, bool& found) {
  path r= path (0, bs[current]->find_box_path (p, found));
  if (found) return r;
  return box_rep::find_box_path (p, found);
}

path
anim_compose_box_rep::find_tree_path (path bp) {
  if (is_atom (bp)) return box_rep::find_tree_path (bp);
  return bs[current]->find_tree_path (bp->next);
}

cursor
anim_compose_box_rep::find_cursor (path bp) {
  if (is_atom (bp)) return box_rep::find_cursor (bp);
  else return bs[current]->find_cursor (bp->next);
}

selection
anim_compose_box_rep::find_selection (path lbp, path rbp) {
  if (!is_atom (lbp) && !is_atom (rbp))
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

struct anim_repeat_box_rep: public composite_box_rep, public anim_rep {
  bool   started;
  time_t started_at;
  int    length;

  anim_repeat_box_rep (path ip, box b, player pl);
  operator tree () { return tree (TUPLE, "anim_repeat", (tree) bs[0]); }

  void pre_display (renderer& ren);
  int  anim_length () { return -1; }
  bool anim_started () { return started; }
  bool anim_finished () { return false; }
  void anim_start_at (time_t at);
  void anim_finish_now () {}
  void anim_get_invalid (bool& flag, time_t& at, rectangles& rs);
};

anim_repeat_box_rep::anim_repeat_box_rep (path ip, box b, player pl):
  composite_box_rep (ip), anim_rep (pl, 1000000000.0)
{
  insert (b, 0, 0);
  position ();
  finalize ();
  started = false;
  length  = b->anim_length ();
}

void
anim_repeat_box_rep::pre_display (renderer& ren) {
  double t = anim_time ();
  double it= floor (t / duration);
  bs[0]->anim_position (delay + it * duration);
  if (pl->speed > 0) pl->request_refresh ((it + 1.0) * duration - t);
  else pl->request_refresh (t - it * duration);

  (void) ren;
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
* Special content effects
******************************************************************************/

struct anim_effect_box_rep: public composite_box_rep, public anim_rep {
  box    b;
  bool   started;
  bool   finished;
  time_t started_at;
  time_t last_update;
  int    length;
  SI     old_clip_x1, old_clip_x2, old_clip_y1, old_clip_y2;

  anim_effect_box_rep (path ip, box b, player pl, int len);
  operator tree () { return tree (TUPLE, "anim_effect", (tree) b); }

  void pre_display (renderer& ren);
  void post_display (renderer& ren);
  virtual void set_position (double t) = 0;
  virtual void set_clipping (renderer& ren, double t) = 0;

  int    anim_length () { return length; }
  bool   anim_started () { return started; }
  bool   anim_finished () { return finished; }
  void   anim_start_at (time_t at);
  void   anim_finish_now ();
  time_t anim_next_update ();
  void   anim_get_invalid (bool& flag, time_t& at, rectangles& rs);
  rectangles anim_invalid ();
};

anim_effect_box_rep::anim_effect_box_rep (path ip, box b2, player pl, int len):
  composite_box_rep (ip), anim_rep (pl, (double) len), b (b2)
{
  insert (b, 0, 0);
  position ();
  finalize ();
  started = false;
  finished= false;
  length  = len;
}

void
anim_effect_box_rep::pre_display (renderer& ren) {
  pl->request_refresh (0.0);

  double t= 1.0;
  if (!started) anim_start_at (texmacs_time ());

  if (!finished) {
    time_t now= texmacs_time ();
    if (now - (started_at + length) >= 0) {
      t= 1.0;
      anim_finish_now ();
    }
    else {
      t= ((double) (now - started_at)) / ((double) length);
      set_position (t);
    }
    last_update= now;
    refresh_at (anim_next_update ());
  }

  ren->get_clipping (old_clip_x1, old_clip_y1, old_clip_x2, old_clip_y2);
  set_clipping (ren, t);
}

void
anim_effect_box_rep::post_display (renderer &ren) {
  ren->set_clipping
    (old_clip_x1, old_clip_y1, old_clip_x2, old_clip_y2, true);
}

void
anim_effect_box_rep::anim_start_at (time_t at) {
  started   = true;
  started_at= at;
  finished  = false;
  bs[0]->anim_start_at (at);
  set_position (0.0);
}

void
anim_effect_box_rep::anim_finish_now () {
  started = true;
  finished= true;
  bs[0]->anim_finish_now ();
  set_position (1.0);
}

time_t
anim_effect_box_rep::anim_next_update () {
  int elapsed= last_update - started_at;
  return started_at + min (40 * ((elapsed / 40) + 1), length);
}

void
anim_effect_box_rep::anim_get_invalid
  (bool& f, time_t& at, rectangles& rs)
{
  if (started) {
    bs[0]->anim_get_invalid (f, at, rs);
    if (!finished) anim_check_invalid (f, at, rs);
  }
}

rectangles
anim_effect_box_rep::anim_invalid () {
  rectangles rs= bs[0]->anim_invalid ();
  rs << rectangle (x1, y1, x2, y2);
  return rs;
}

struct anim_translate_box_rep: public anim_effect_box_rep {
  SI start_x, start_y, end_x, end_y;

  anim_translate_box_rep (path ip, box b, player pl,
                          int len, SI sx, SI sy, SI ex, SI ey):
    anim_effect_box_rep (ip, b, pl, len),
    start_x (sx), start_y (sy), end_x (ex), end_y (ey) {}
  operator tree () { return tree (TUPLE, "anim_translate", (tree) b); }

  void set_position (double t) {
    sx (0)= start_x - x1 + as_int (t * (end_x - start_x));
    sy (0)= start_y - y1 + as_int (t * (end_y - start_y)); }
  void set_clipping (renderer& ren, double t) {
    if (t != 1.0 || end_x != x1 || end_y != y1)
      ren->extra_clipping (x1, y1, x2, y2); }
};

struct anim_progressive_box_rep: public anim_effect_box_rep {
  rectangle start_r, end_r;
  anim_progressive_box_rep (path ip, box b, player pl,
                            int l, rectangle r1, rectangle r2):
    anim_effect_box_rep (ip, b, pl, l),
    start_r (r1), end_r (r2) {}
  operator tree () { return tree (TUPLE, "anim_progressive", (tree) b); }

  void set_position (double t) { (void) t; }
  void set_clipping (renderer& ren, double t) {
    SI X1= start_r->x1 + as_int (t * (end_r->x1 - start_r->x1));
    SI Y1= start_r->y1 + as_int (t * (end_r->y1 - start_r->y1));
    SI X2= start_r->x2 + as_int (t * (end_r->x2 - start_r->x2));
    SI Y2= start_r->y2 + as_int (t * (end_r->y2 - start_r->y2));
    if (t != 1.0 || X1 != x1 || Y1 != y1 || X2 != x2 || Y2 != y2)
      ren->extra_clipping (X1, Y1, X2, Y2); }
};

/******************************************************************************
* Sound boxes
******************************************************************************/

struct sound_box_rep: public box_rep, public anim_rep {
  url    u;
  bool   started;

  sound_box_rep (path ip, player pl, url u2, SI h):
    box_rep (ip), anim_rep (pl, 0.0), u (u2), started (false) { y2= h; }
  operator tree () { return tree (TUPLE, "sound", u->t); }
  void display (renderer ren) { (void) ren; }

  void play_sound () {
    if (exists_in_path ("play"))
      system ("play", u, "&");
    started= true; }
  void pre_display (renderer& ren) {
    (void) ren;
    if (!started) anim_start_at (texmacs_time ()); }
  int  anim_length () { return 0; }
  bool anim_started () { return started; }
  bool anim_finished () { return anim_started (); }
  void anim_start_at (time_t at) { (void) at; play_sound (); }
  void anim_finish_now () { if (!started) play_sound (); }
};

/******************************************************************************
* Animated gifs
******************************************************************************/

static hashmap<tree,tree> decomposed_gif ("");

static url
decompose_gif (url u) {
  if (!exists_in_path ("convert"))
    return url_none ();
  if (!decomposed_gif->contains (u->t)) {
    url tmp= url_temp ("");
    url res= glue (tmp, "_%04d.gif");
    system ("convert +adjoin -coalesce", u, res);
    decomposed_gif (u->t)= tmp->t;
  }
  url tmp= as_url (decomposed_gif [u->t]);
  url dir= head (tmp);
  url nam= tail (tmp);
  return expand (complete (dir * url_wildcard (as_string (nam) * "_*.gif")));
}

static void
add_frames (array<box>& v, path ip, player pl, url u, int w, int h, int a,
            int msecs, int px) {
  if (is_none (u)) return;
  else if (is_or (u)) {
    add_frames (v, ip, pl, u[1], w, h, a, msecs, px);
    add_frames (v, ip, pl, u[2], w, h, a, msecs, px);
  }
  else {
    box imb= image_box (ip, u, w, h, a, px);
    v << anim_constant_box (ip, imb, pl, msecs);
  }
}

/******************************************************************************
* box construction routines
******************************************************************************/

box
anim_constant_box (path ip, box b, player pl, int len) {
  return tm_new<anim_constant_box_rep> (ip, b, pl, len);
}

box
anim_compose_box (path ip, array<box> bs, player pl) {
  return tm_new<anim_compose_box_rep> (ip, bs, pl);
}

box
anim_repeat_box (path ip, box b, player pl) {
  return tm_new<anim_repeat_box_rep> (ip, b, pl);
}

box
anim_translate_box (path ip, box b, player pl,
                    int len, SI sx, SI sy, SI ex, SI ey) {
  return tm_new<anim_translate_box_rep> (ip, b, pl, len, sx, sy, ex, ey);
}

box
anim_progressive_box (path ip, box b, player pl,
                      int len, rectangle r1, rectangle r2) {
  return tm_new<anim_progressive_box_rep> (ip, b, pl, len, r1, r2);
}

box
sound_box (path ip, player pl, url u, SI h) {
  return tm_new<sound_box_rep> (ip, pl, u, h);
}

box
video_box (path ip, player pl, url u,
           SI w, SI h, int alpha, int ms, bool rep, int px) {
  url frames= decompose_gif (u);
  if (is_none (frames)) return empty_box (ip, 0, 0, w, h);
  array<box> bs;
  add_frames (bs, decorate (ip), pl, frames, w, h, alpha, ms, px);
  box b= anim_compose_box (rep? decorate (ip): ip, bs, pl);
  if (rep) return anim_repeat_box (ip, b, pl);
  else return b;
}
