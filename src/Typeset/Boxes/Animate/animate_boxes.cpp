
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
#include "Files/image_files.hpp"

/******************************************************************************
* Base classes for animation boxes
******************************************************************************/

bool animated_flag= false;

struct anim_box_rep: public box_rep {
  player pl;
  double delay;
  double duration;

  anim_box_rep (path ip, player pl2, double duration2):
    box_rep (ip), pl (pl2), delay (0.0), duration (duration2) {}

  player anim_player () { return pl; }
  double anim_delay () { return delay; }
  double anim_duration () { return duration; }
  void   anim_position (double pos) { delay= pos; }
  double anim_time () { return pl->get_elapsed () - delay; }
  void   pre_display (renderer& ren) { (void) ren; animated_flag= true; }
};

struct composite_anim_box_rep: public composite_box_rep {
  player pl;
  double delay;
  double duration;

  composite_anim_box_rep (path ip, player pl2, double duration2):
    composite_box_rep (ip), pl (pl2), delay (0.0), duration (duration2) {}

  player anim_player () { return pl; }
  double anim_delay () { return delay; }
  double anim_duration () { return duration; }
  void   anim_position (double pos) { delay= pos; }
  double anim_time () { return pl->get_elapsed () - delay; }
  void   pre_display (renderer& ren) { (void) ren; animated_flag= true; }
};

/******************************************************************************
* Animations which remain constant for a fixed duration
******************************************************************************/

struct anim_constant_box_rep: public composite_anim_box_rep {
  anim_constant_box_rep (path ip, box b, player pl, int length);
  operator tree () { return tree (TUPLE, "anim_constant", (tree) bs[0]); }
};

anim_constant_box_rep::anim_constant_box_rep (path ip, box b, player pl,
                                              int length2):
  composite_anim_box_rep (ip, pl, (double) length2)
{
  insert (b, 0, 0);
  position ();
  finalize ();
}

/******************************************************************************
* Compositions of animations
******************************************************************************/

class anim_compose_box_rep: public anim_box_rep {
public:
  array<box> bs;
  array<int> cum_len;
  array<double> offsets;
  int        current;
  double     cur_delay;
  int        last_index;
  double     last_delay;

  anim_compose_box_rep (path ip, array<box> bs, player pl);
  ~anim_compose_box_rep ();

  int        subnr () { return 1; }
  box        subbox (int i) { (void) i; return bs[current]; }
  void       display (renderer ren) { (void) ren; }
  operator   tree () { return tree ("composed animation"); }
  tree       message (tree t, SI x, SI y, rectangles& rs);
  void       loci (SI x, SI y, SI delta, list<string>& ids, rectangles& rs);
  void       collect_page_numbers (hashmap<string,tree>& h, tree page);
  path       find_tag (string name);

  int        get_index (double t);
  void       anim_resync ();
  double     anim_next ();
  rectangles anim_invalid ();
  void       pre_display (renderer& ren);

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
  anim_box_rep (ip, pl, 0.0),
  bs (b2), offsets (N(bs))
{
  ASSERT (N(bs) != 0, "empty animation");

  current   = 0;
  cur_delay = 0.0;
  last_index= 0;
  last_delay= 0.0;

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

  double len= 0.0;
  for (i=0; i<n; i++) {
    len += bs[i]->anim_duration ();
    offsets[i]= len;
  }
  duration= offsets[n-1];
}

anim_compose_box_rep::~anim_compose_box_rep () {}

tree
anim_compose_box_rep::message (tree t, SI x, SI y, rectangles& rs) {
  return bs[current]->message (t, x, y, rs);
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

int
anim_compose_box_rep::get_index (double t) {
  for (int i=0; i<N(bs); i++)
    if (t < offsets[i]) return i;
  return N(bs) - 1;
}

void
anim_compose_box_rep::anim_resync () {
  double t = anim_time ();
  int    ix= get_index (t);
  if (ix != current || delay != cur_delay) {
    if (ix == 0) bs[ix]->anim_position (delay);
    else bs[ix]->anim_position (delay + offsets[ix-1]);
    current  = ix;
    cur_delay= delay;
  }
}

double
anim_compose_box_rep::anim_next () {
  anim_resync ();
  double r= bs[current]->anim_next ();
  if (pl->is_progressing () && current != N(bs)-1)
    return min (r, pl->get_refresh_time (offsets[current] - anim_time ()));
  if (!pl->is_progressing () && current != 0)
    return min (r, pl->get_refresh_time (anim_time () - offsets[current-1]));
  return r;
}

rectangles
anim_compose_box_rep::anim_invalid () {
  rectangles rs= box_rep::anim_invalid ();
  anim_resync ();
  if (current != last_index) rs << box_rep::anim_invalid ();
  if (current != last_index || delay != last_delay)
    rs << rectangle (min (x1, x3), min (y1, y3), max (x2, x4), max (y2, y4));
  return rs;
}

void
anim_compose_box_rep::pre_display (renderer& ren) {
  (void) ren;
  animated_flag= true;
  anim_resync ();
  last_index= current;
  last_delay= delay;
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

struct anim_repeat_box_rep: public composite_anim_box_rep {
  double current_it;
  double last_it;

  anim_repeat_box_rep (path ip, box b, player pl);
  operator tree () { return tree (TUPLE, "anim_repeat", (tree) bs[0]); }

  void anim_resync ();
  double anim_next ();
  rectangles anim_invalid ();
  void pre_display (renderer& ren);
};

anim_repeat_box_rep::anim_repeat_box_rep (path ip, box b, player pl):
  composite_anim_box_rep (ip, pl, b->anim_duration ()),
  current_it (0.0), last_it (0.0)
{
  insert (b, 0, 0);
  position ();
  finalize ();
}

void
anim_repeat_box_rep::anim_resync () {
  double t = anim_time ();
  double it= floor (t / duration);
  if (it != current_it) {
    bs[0]->anim_position (delay + it * duration);
    current_it= it;
  }
}

double
anim_repeat_box_rep::anim_next () {
  anim_resync ();
  double r= bs[0]->anim_next ();
  double t= anim_time ();
  if (pl->is_progressing ())
    r= min (r, pl->get_refresh_time ((current_it + 1.0) * duration - t));
  else
    r= min (r, pl->get_refresh_time (t - current_it * duration));
  return r;
}

rectangles
anim_repeat_box_rep::anim_invalid () {
  anim_resync ();
  rectangles rs= bs[0]->anim_invalid ();
  if (current_it != last_it)
    rs << rectangle (min (x1, x3), min (y1, y3), max (x2, x4), max (y2, y4));
  return rs;
}

void
anim_repeat_box_rep::pre_display (renderer& ren) {
  (void) ren;
  animated_flag= true;
  anim_resync ();
  last_it= current_it;
}

/******************************************************************************
* Special content effects
******************************************************************************/

struct anim_effect_box_rep: public composite_anim_box_rep {
  box    b;
  SI     old_clip_x1, old_clip_x2, old_clip_y1, old_clip_y2;
  double current_x;
  double last_x;

  anim_effect_box_rep (path ip, box b, player pl, int len);
  operator tree () { return tree (TUPLE, "anim_effect", (tree) b); }

  void anim_resync ();
  double anim_next ();
  rectangles anim_invalid ();

  void pre_display (renderer& ren);
  void post_display (renderer& ren);
  virtual void set_position (double t) = 0;
  virtual void set_clipping (renderer& ren, double t) = 0;
};

anim_effect_box_rep::anim_effect_box_rep (path ip, box b2, player pl, int len):
  composite_anim_box_rep (ip, pl, (double) len),
  b (b2), current_x (0.0), last_x (0.0)
{
  insert (b, 0, 0);
  position ();
  finalize ();
}

void
anim_effect_box_rep::anim_resync () {
  double t= anim_time ();
  current_x= max (0.0, min (1.0, t / duration));
}

double
anim_effect_box_rep::anim_next () {
  anim_resync ();
  double r= bs[0]->anim_next ();
  if (pl->is_progressing () && 1.0 > current_x)
    r= min (r, pl->get_refresh_time (0.0));
  if (!pl->is_progressing () && 0.0 < current_x)
    r= min (r, pl->get_refresh_time (0.0));
  return r;
}

rectangles
anim_effect_box_rep::anim_invalid () {
  rectangles rs= bs[0]->anim_invalid ();
  anim_resync ();
  if (current_x != last_x)
    rs << rectangle (min (x1, x3), min (y1, y3), max (x2, x4), max (y2, y4));
  return rs;
}

void
anim_effect_box_rep::pre_display (renderer& ren) {
  animated_flag= true;
  anim_resync ();
  set_position (current_x);
  ren->get_clipping (old_clip_x1, old_clip_y1, old_clip_x2, old_clip_y2);
  set_clipping (ren, current_x);
  last_x= current_x;
}

void
anim_effect_box_rep::post_display (renderer &ren) {
  ren->set_clipping
    (old_clip_x1, old_clip_y1, old_clip_x2, old_clip_y2, true);
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

struct sound_box_rep: public anim_box_rep {
  url    u;
  double last_played;

  sound_box_rep (path ip, player pl, url u2, SI h):
    anim_box_rep (ip, pl, 0.0), u (u2), last_played (-1.0) { y2= h; }
  operator tree () { return tree (TUPLE, "sound", u->t); }
  void display (renderer ren) { (void) ren; }

  void play_sound () {
    if (exists_in_path ("play"))
      system ("play", u, "&");
    else if (exists_in_path ("afplay"))
      system ("afplay", u, "&"); }
  void pre_display (renderer& ren) {
    (void) ren;
    animated_flag= true;
    double t= anim_time ();
    if (last_played < 0.0 && t >= 1000.0) last_played= t;
    if (last_played < 30.0 && t >= 30.0) play_sound ();
    last_played= t; }
  double anim_next () {
    double t= anim_time ();
    if (t >= 30.0) return 1.0e12;
    return pl->get_refresh_time (30.0 - t); }
  rectangles anim_invalid () {
    rectangles rs;
    double t= anim_time ();
    if (last_played < 30.0 && t >= 30.0)
      rs << rectangle (min (x1, x3), min (y1, y3), max (x2, x4), max (y2, y4));
    return rs; }
};

/******************************************************************************
* Animated gifs
******************************************************************************/

static hashmap<tree,tree> decomposed_gif ("");

static url
decompose_gif (url u) {
  if (!has_image_magick())
    return url_none ();
  if (!decomposed_gif->contains (u->t)) {
    url tmp= url_temp ("");
    url res= glue (tmp, "_%04d.gif");
    system (imagemagick_cmd () *" +adjoin -coalesce", u, res);
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
