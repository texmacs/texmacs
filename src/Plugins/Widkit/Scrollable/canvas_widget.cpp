
/******************************************************************************
* MODULE     : canvas_widget.cpp
* DESCRIPTION: A canvas widget with horizontal and vertical scrollbars.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "rectangles.hpp"
#include "window.hpp"
#include "message.hpp"
#include "Widkit/basic_widget.hpp"
#include "Widkit/Event/attribute_event.hpp"
#include "Widkit/scroll_widget.hpp"
#include "Widkit/layout.hpp"

SI get_dx (gravity grav, SI w);
SI get_dy (gravity grav, SI h);
gravity opposite (gravity grav);
wk_widget wrap_scroll_widget (wk_widget w);

/******************************************************************************
* Canvas widgets
******************************************************************************/

class canvas_widget_rep: public basic_widget_rep {
  //bool padding_flag; // should small canvasses be padded?
  bool request_focus; // request focus upon clicking in canvas
  SI ex1, ey1, ex2, ey2;
  SI last_w, last_h;
  bool show_scroll_bars;
  wk_widget hor; bool hor_active;
  wk_widget ver; bool ver_active;

public:
  canvas_widget_rep (wk_widget child, gravity grav, bool rf);
  operator tree ();
  void set_extents (SI Ex1, SI Ey1, SI Ex2, SI Ey2);

  bool handle_canvas_mouse (mouse_event ev);
  void handle_get_size (get_size_event ev);
  void handle_get_widget (get_widget_event ev);
  void handle_set_widget (set_widget_event ev);
  void handle_position (position_event ev);
  void handle_repaint (repaint_event ev);
  void handle_set_integer (set_integer_event ev);
  void handle_set_coord4 (set_coord4_event ev);
  void handle_set_string (set_string_event ev);
  bool handle (event ev);
};

/******************************************************************************
* Creation of canvas widgets
******************************************************************************/

canvas_widget_rep::canvas_widget_rep (wk_widget child, gravity grav, bool rf):
  basic_widget_rep (1),
  //padding_flag (false),
  request_focus (rf), show_scroll_bars (true)
{
  a[0] = tm_new<scrollable_widget_rep> (child, grav);
  hor  = tm_new<hor_scrollbar_widget_rep> (a[0]);
  ver  = tm_new<ver_scrollbar_widget_rep> (a[0]);
  a[0] << set_hor_bar (NULL); hor_active= false;
  a[0] << set_ver_bar (NULL); ver_active= false;
  ex1= ey1= ex2= ey2= last_w= last_h= 0;
}

canvas_widget_rep::operator tree () {
  return tree (TUPLE, "canvas", (tree) a[0]);
}

void
canvas_widget_rep::set_extents (SI Ex1, SI Ey1, SI Ex2, SI Ey2) {
  abs_outer_round (Ex1, Ey1, Ex2, Ey2);
  SI ew= Ex2 - Ex1, eh= Ey2 - Ey1;
  bool old_hor_active= hor_active, old_ver_active= ver_active;
  if ((ew<=(w-2*PIXEL)) && (eh<=(h-2*PIXEL))) {
    hor_active= false; ver_active= false; }
  else { hor_active= ew > (w-20*PIXEL); ver_active= eh > (h-20*PIXEL); }

  gravity grav= a[0]->grav;
  SI ww= w - (show_scroll_bars? (ver_active? 20*PIXEL: 2*PIXEL): 0);
  SI wh= h - (show_scroll_bars? (hor_active? 20*PIXEL: 2*PIXEL): 0);
  if (ew < ww) {
    SI chw= ww;
    //if (padding_flag) chw= ew;
    SI cxr= get_dx (grav, ww);
    SI cxc= Ex1 + get_dx (grav, chw);
    Ex1= cxc - cxr;
    Ex2= cxc - cxr + chw;
  }
  if (eh < wh) {
    SI chh= wh;
    //if (padding_flag) chh= eh;
    SI cyr= get_dy (grav, wh) + (wh - chh);
    SI cyc= Ey2 + get_dy (grav, chh);
    Ey1= cyc - cyr - chh;
    Ey2= cyc - cyr;
  }

  bool bars_changed=
    (old_hor_active != hor_active) || (old_ver_active != ver_active);
  bool visibility_changed=
    ((!show_scroll_bars) && (N(a)>1)) ||
    (show_scroll_bars && (N(a)==1) && (hor_active || ver_active));
  bool extents_changed=
    (Ex1 != ex1) || (Ey1 != ey1) || (Ex2 != ex2) || (Ey2 != ey2);
  ex1= Ex1; ey1= Ey1; ex2= Ex2; ey2= Ey2;

  if (bars_changed || visibility_changed) {
    if (hor_active) a[0] << set_hor_bar (hor);
    else a[0] << set_hor_bar (NULL);
    if (ver_active) a[0] << set_ver_bar (ver);
    else a[0] << set_ver_bar (NULL);
    a->resize(1);
    if (show_scroll_bars && hor_active) a << hor;
    if (show_scroll_bars && ver_active) a << ver;
    if (attached ()) {
      if (hor_active) hor << emit_attach_window (win);
      if (ver_active) ver << emit_attach_window (win);
      this << emit_reposition ();
    }
  }

  if (bars_changed || extents_changed) {
    a[0] << ::set_extents (ex1, ey1, ex2, ey2);
    if (attached ()) this << emit_invalidate_all ();
  }
}

/******************************************************************************
* Event handling
******************************************************************************/

bool
canvas_widget_rep::handle_canvas_mouse (mouse_event ev) {
  if (ev->type == "press-left" && request_focus)
    win->set_keyboard_focus (this);
  if (ev->type == "wheel")
    cout << "canvas wheel event\n";
  return basic_widget_rep::handle (ev);
}

void
canvas_widget_rep::handle_get_size (get_size_event ev) {
  if (ev->mode==-1) {
    ev->w= 32*PIXEL;
    ev->h= 32*PIXEL;
  }
  if (ev->mode== 1) gui_maximal_extents (ev->w, ev->h);
}

void
canvas_widget_rep::handle_get_widget (get_widget_event ev) {
  if (ev->which == "scrollable") ev->w= a[0]->a[0];
  else basic_widget_rep::handle_get_widget (ev);
}

void
canvas_widget_rep::handle_set_widget (set_widget_event ev) {
  if (ev->which == "scrollable") a[0]->a[0]= ev->w;
  else basic_widget_rep::handle_set_widget (ev);
}

void
canvas_widget_rep::handle_position (position_event ev) { (void) ev;
  if ((w != last_w) || (h != last_h)) {
    last_w= w; last_h= h;
    set_extents (ex1, ey1, ex2, ey2);
    if (attached ()) this << emit_invalidate_all ();
  }

  if (!show_scroll_bars)
    a[0] << emit_position (0, 0, w, h);
  else if (hor_active && ver_active) {
    a[0] << emit_position (PIXEL, -PIXEL, w-20*PIXEL, h-20*PIXEL);
    a[1] << emit_position (0, 16*PIXEL-h, w-18*PIXEL, 16*PIXEL);
    a[2] << emit_position (w-16*PIXEL, 0, 16*PIXEL, h-18*PIXEL);
  }
  else if (hor_active && (!ver_active)) {
    a[0] << emit_position (PIXEL, -PIXEL, w-2*PIXEL, h-20*PIXEL);
    a[1] << emit_position (0, 16*PIXEL-h, w, 16*PIXEL);
  }
  else if ((!hor_active) && ver_active) {
    a[0] << emit_position (PIXEL, -PIXEL, w-20*PIXEL, h-2*PIXEL);
    a[1] << emit_position (w-16*PIXEL, 0, 16*PIXEL, h);
  }
  else a[0] << emit_position (PIXEL, -PIXEL, w-2*PIXEL, h-2*PIXEL);
}

extern void indent ();

void
canvas_widget_rep::handle_repaint (repaint_event ev) { (void) ev;
  renderer ren= ev->win;
  if (!show_scroll_bars);
  else if (hor_active && ver_active) {
    layout_default (ren, w- 16*PIXEL, -h, w, -h+ 16*PIXEL);
    layout_default (ren, 0, -h+ 16*PIXEL, w, -h+ 18*PIXEL);
    layout_default (ren, w- 18*PIXEL, -h, w- 16*PIXEL, 0);
    layout_lower (ren, 0, -h+ 18*PIXEL, w- 18*PIXEL, 0);
  }
  else if (hor_active && (!ver_active)) {
    layout_default (ren, 0, -h+ 16*PIXEL, w, -h+ 18*PIXEL);
    layout_dark_outline (ren, 0, -h+ 15*PIXEL, w, 0);
    ren->set_pencil (pencil (layout_light (ren), PIXEL));
    ren->line (PIXEL, -h+ 18*PIXEL, w-2*PIXEL, -h+ 18*PIXEL);
  }
  else if ((!hor_active) && ver_active) {
    layout_default (ren, w- 18*PIXEL, -h, w- 16*PIXEL, 0);
    layout_dark_outline (ren, 0, -h, w- 15*PIXEL, 0);
    ren->set_pencil (pencil (layout_light (ren), PIXEL));
    ren->line (w- 19*PIXEL, -h+PIXEL, w- 19*PIXEL, -2*PIXEL);
  }
  else layout_dark_outline (ren, 0, -h, w, 0);
}

void
canvas_widget_rep::handle_set_integer (set_integer_event ev) {
  if (ev->which == "scrollbars") {
    if (((bool) ev->i) != show_scroll_bars) {
      show_scroll_bars= (bool) ev->i;
      set_extents (ex1, ey1, ex2, ey2);
      if (attached ()) this << emit_invalidate_all ();
    }
  }
  else a[0]->a[0] << ev;
}

void
canvas_widget_rep::handle_set_coord4 (set_coord4_event ev) {
  if (ev->which == "extents") set_extents (ev->c1, ev->c2, ev->c3, ev->c4);
  else a[0] << ev;
}

void
canvas_widget_rep::handle_set_string (set_string_event ev) {
  if (ev->which == "background") a[0] << ev;
  else a[0]->a[0] << ev;
}

bool
canvas_widget_rep::handle (event ev) {
  switch (ev->type) {
  case GET_SIZE_EVENT:
  case GET_WIDGET_EVENT:
  case SET_WIDGET_EVENT:
  case ATTACH_WINDOW_EVENT:
  case POSITION_EVENT:
  case UPDATE_EVENT:
  case INVALIDATE_EVENT:
    return basic_widget_rep::handle (ev);
  case MOUSE_EVENT: {
    mouse_event e (ev);
    return handle_canvas_mouse (e);
  }
  case REPAINT_EVENT:
  case FIND_CHILD_EVENT:
    return basic_widget_rep::handle (ev);
  case SET_INTEGER_EVENT: {
    set_integer_event e (ev);
    handle_set_integer (e);
    return true;
  }
  case SET_COORD4_EVENT: {
    set_coord4_event e (ev);
    handle_set_coord4 (e);
    return true;
  }
  case SET_STRING_EVENT: {
    set_string_event e (ev);
    handle_set_string (e);
    return true;
  }
  case GET_COORD4_EVENT:
  case SET_COORD2_EVENT:
  case GET_COORD2_EVENT:
    a[0] << ev;
    return true;
  default:
    a[0]->a[0] << ev;
    return true;
  }
}

/******************************************************************************
* Resize widgets
******************************************************************************/

class resize_widget_rep: public basic_widget_rep {
  int style;
  string minw, minh, defw, defh, maxw, maxh;
public:
  resize_widget_rep (wk_widget w, int style, string w1, string h1,
                     string w2, string h2, string w3, string j3);
  operator tree ();
  void handle_get_size (get_size_event ev);
  void handle_repaint (repaint_event ev);
};

resize_widget_rep::resize_widget_rep (wk_widget w, int style2,
                                      string w1, string h1,
                                      string w2, string h2,
                                      string w3, string h3):
  basic_widget_rep (1), style (style2), minw (w1), minh (h1),
  defw (w2), defh (h2), maxw (w3), maxh (h3) { a[0]= w; }

resize_widget_rep::operator tree () {
  return tree (TUPLE, "resize", (tree) a[0], defw, defh);
}

void
resize_widget_rep::handle_get_size (get_size_event ev) {
  string ww, hh;
  if (ev->mode == -1) { ww= minw; hh= minh; }
  else if (ev->mode == 1) { ww= maxw; hh= maxh; }
  else { ww= defw; hh= defh; }
  if (ww != "") ev->w= decode_length (ww, a[0], style);
  if (hh != "") ev->h= decode_length (hh, a[0], style);
  abs_round (ev->w, ev->h);
}

void
resize_widget_rep::handle_repaint (repaint_event ev) {
  renderer ren= ev->win;
  rectangles r1= rectangle (ev->x1, ev->y1, ev->x2, ev->y2);
  rectangles r2= rectangle (0, -h, w, 0);
  rectangles rs= r1 - r2;
  for (int i=0; i<N(rs); i++)
    layout_default (ren, rs[i]->x1, rs[i]->y1, rs[i]->x2, rs[i]->y2);
  //layout_default (ren, ev->x1, ev->y1, ev->x2, ev->y2);
  basic_widget_rep::handle_repaint (ev);
}

wk_widget
resize_widget (wk_widget w, int style, string w1, string h1,
               string w2, string h2, string w3, string h3) {
  //cout << "min: " << w1 << ", " << h1 << "\n";
  //cout << "def: " << w2 << ", " << h2 << "\n";
  //cout << "max: " << w3 << ", " << h3 << "\n";
  return tm_new<resize_widget_rep> (w, style, w1, h1, w2, h2, w3, h3);
}

wk_widget
resize_widget (wk_widget w, int style, string w1, string h1,
               string w2, string h2, string w3, string h3,
               string hp, string vp) {
  wk_widget cw= w;
  if (vp == "bottom") {
    cw= canvas_widget (wrap_scroll_widget (cw), south_west, false);
    SI widw, widh;
    w << get_size (widw, widh, -1);
    abs_round (widw, widh);
    cw << set_coord4 ("extents", 0, -widh, widw, 0);
    cw << set_scroll_pos (0, -widh);
  }
  return resize_widget (cw, style, w1, h1, w2, h2, w3, h3);
}

/******************************************************************************
* Wrap scroll widgets
******************************************************************************/

class wrap_scroll_widget_rep: public basic_widget_rep {
public:
  wrap_scroll_widget_rep (wk_widget w);
  operator tree ();
  void handle_repaint (repaint_event ev);
};

wrap_scroll_widget_rep::wrap_scroll_widget_rep (wk_widget w):
  basic_widget_rep (1) { a[0]= w; }

wrap_scroll_widget_rep::operator tree () {
  return tree (TUPLE, "wrap_scroll", (tree) a[0]);
}

void
wrap_scroll_widget_rep::handle_repaint (repaint_event ev) {
  renderer ren= ev->win;
  layout_default (ren, ev->x1, ev->y1, ev->x2, ev->y2);
  a[0] << emit_position (0, 0, w, h);
  basic_widget_rep::handle_repaint (ev);
}

wk_widget
wrap_scroll_widget (wk_widget w) {
  return tm_new<wrap_scroll_widget_rep> (w);
}

/******************************************************************************
* exported routines
******************************************************************************/

event
set_scrollable (wk_widget w) {
  return set_widget ("scrollable", w);
}

wk_widget
canvas_widget (wk_widget w, gravity grav, bool request_focus) {
  return tm_new<canvas_widget_rep> (w, grav, request_focus);
}

wk_widget
user_canvas_widget (wk_widget wid, int style) {
  wk_widget cv= canvas_widget (wrap_scroll_widget (wid));
  SI widw, widh;
  gui_maximal_extents (widw, widh);
  wid << get_size (widw, widh, -1);
  abs_round (widw, widh);
  cv << set_coord4 ("extents", 0, -widh, widw, 0);
  return cv;
}

wk_widget
vsplit_widget (wk_widget t, wk_widget b) {
  (void) t; (void) b;
  FAILED ("not yet implemented");
}
