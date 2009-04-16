
/******************************************************************************
* MODULE     : scrollbar_widget.cpp
* DESCRIPTION: Horizontal and vertical scrollbars
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Widkit/attribute_widget.hpp"
#include "Widkit/scroll_widget.hpp"
#include "Widkit/layout.hpp"

/******************************************************************************
* Routines for abstract scrollbars
******************************************************************************/

scrollbar_rep::scrollbar_rep (wk_widget ref2):
  scroll_widget_rep (0, south_west), ref (ref2.rep),
  sc_min(0), sc_max(0), sc_pos(0), before(0), after(0),
  factor (0.5), gripped (false), scrolling (false), increment (0) {}

void
scrollbar_rep::handle_set_coord1 (set_coord1_event ev) {
  if (ev->which == "scroll position") {
    sc_pos = ev->c1;
    sc_pos = min (sc_pos, sc_max);
    sc_pos = max (sc_pos, sc_min);
    abs_round (sc_pos);  
    this << emit_scroll (sc_pos, before, after);
    
    if (before+ after > sc_max- sc_min) {
      SI total= before+after;
      DI ext  = sc_max- sc_min; if (total==0) total=1;
      before  = (SI) ((before*ext)/total);
      after   = ((SI) ext)- before;
    }
    
    if (sc_pos- before < sc_min) before = sc_pos - sc_min;
    if (sc_pos+ after  > sc_max) after  = sc_max - sc_pos;
    
    if (attached ()) this << emit_invalidate_all ();
    return;
  }
  attribute_widget_rep::handle_set_coord1 (ev);
}

void
scrollbar_rep::handle_set_coord2 (set_coord2_event ev) {
  /*
  cout << "Scrollbar extents  " << (ev->c1/PIXEL) << ", "
       << (ev->c2/PIXEL) << "\n";
  cout << "Scrollbar position " << (sc_pos/PIXEL) << "\n";
  */
  if (ev->which == "extents") {
    sc_min= ev->c1;
    sc_max= ev->c2;
    this << emit_bar_scroll_to (sc_pos);
    return;
  }
  attribute_widget_rep::handle_set_coord2 (ev);
}

/******************************************************************************
* Routines for horizontal scrollbars
******************************************************************************/

hor_scrollbar_widget_rep::hor_scrollbar_widget_rep (wk_widget ref):
  scrollbar_rep (ref) {}

hor_scrollbar_widget_rep::operator tree () {
  return "horizontal scrollbar";
}

void
hor_scrollbar_widget_rep::handle_get_size (get_size_event ev) {
  if (ev->mode== 0) ev->h= 16*PIXEL;
  if (ev->mode==-1) {
    ev->w= 48*PIXEL;
    ev->h= 16*PIXEL;
  }
  if (ev->mode== 1) {
    gui_maximal_extents (ev->w, ev->h);
    ev->h= 16*PIXEL;
  }
}

void
hor_scrollbar_widget_rep::decode_position (SI& x1, SI& x2) {
  SI total = sc_max- sc_min; if (total==0) total=1;
  SI extra = (((h/PIXEL)*3)/4)*PIXEL + 3*PIXEL;
  SI min_w = min (w-2*extra, 12*PIXEL);
  DI real_w= w- 2*extra - min_w;
  SI bef   = (SI) ((before*real_w)/total);
  SI aft   = (SI) ((after*real_w)/total);
  SI p;

  if (bef+aft==0) aft=1;
  while (bef+aft< 4*PIXEL) {
    bef= aft= 2*PIXEL;
    p = (SI) (((sc_pos- sc_min)*real_w)/total);
    if (p<2*PIXEL) { bef=p; aft= 4*PIXEL-bef; }
    if (p>(real_w- 2*PIXEL)) { aft=real_w-p; bef= 4*PIXEL-aft; }
  }
  
  p = (SI) (((sc_pos- sc_min)*real_w)/total);
  x1= max (0, p-bef)+ extra;
  x2= min ((SI) real_w, p+aft)+ extra + min_w;
}

SI
hor_scrollbar_widget_rep::encode_position (SI x) {
  DI total  = sc_max- sc_min; if (total==0) total=1;
  SI extra  = (((h/PIXEL)*3)/4)*PIXEL + 3*PIXEL;
  SI min_w  = min (w-2*extra, 12*PIXEL);
  SI real_w = w- 2*extra - min_w;
  SI dec_x  = (SI) (((x - extra - (min_w>>1)) * total) / real_w);
  return dec_x+ sc_min;
}

void
hor_scrollbar_widget_rep::handle_repaint (repaint_event ev) { (void) ev;
  renderer ren= win->get_renderer ();
  SI X1, X2;
  decode_position (X1, X2);
  layout_dark (ren, 0, 0, w, h);
  layout_lower (ren, 0, 0, w, h); 
  layout_default (ren, X1, PIXEL, X2, h-PIXEL);
  layout_higher  (ren, X1, PIXEL, X2, h-PIXEL);

  SI aw= (((h/PIXEL)*3)/4)*PIXEL;
  SI ah= h-4*PIXEL;
  layout_left_arrow (ren, 2*PIXEL, 2*PIXEL, aw, ah);
  layout_right_arrow (ren, w- 2*PIXEL- aw, 2*PIXEL, aw, ah);
}

void
hor_scrollbar_widget_rep::handle_mouse (mouse_event ev) {
  string type= ev->type;
  SI     X   = ev->x;
  SI     aw  = (((h/PIXEL)*3)/4)*PIXEL + 3*PIXEL;
  SI     X1, X2;
  decode_position (X1, X2);

  if (type == "press-left") {
    SI ww; ref << get_width (ww);
    if (X < aw) {
      scrolling= true;
      increment= -5*PIXEL;
      this << emit_bar_scroll_to (sc_pos + increment);
      wk_grab_pointer (this);
      win->delayed_message (this, "scroll", 100);
    }
    else if (X >= (w-aw)) {
      scrolling= true;
      increment= 5*PIXEL;
      this << emit_bar_scroll_to (sc_pos + increment);
      wk_grab_pointer (this);
      win->delayed_message (this, "scroll", 100);
    }
    else if (X<X1) this << emit_bar_scroll_to (sc_pos- ww);
    else if (X>X2) this << emit_bar_scroll_to (sc_pos+ ww);
    else {
      gripped= true;
      wk_grab_pointer (this);
      factor= ((double) (X-X1))/((double) (X2-X1));
    }
  }

  if (type == "press-middle") {
    SI x= encode_position (X);
    this << emit_bar_scroll_to (x- ((after-before)>>1));
    wk_grab_pointer (this);
    factor= 0.5;
  }

  if (type == "press-right") {
    if (X<X1) this << emit_bar_scroll_to (sc_pos- 25*PIXEL);
    if (X>X2) this << emit_bar_scroll_to (sc_pos+ 25*PIXEL);
  }

  if ((type == "move") &&
      ((gripped && ev->pressed ("left")) || ev->pressed ("middle"))) {
    if (check_event (DRAG_EVENT)) return;
    SI x = encode_position (X);
    SI dx= (SI) ((after+before)*factor);
    this << emit_bar_scroll_to (x+ before- dx);
  }

  if (((type == "release-left") || (type == "release-middle")) &&
      (!ev->pressed ("left")) && (!ev->pressed ("middle"))) {
    gripped= scrolling= false;
    wk_ungrab_pointer (this);
  }
}

void
hor_scrollbar_widget_rep::handle_alarm (alarm_event ev) {
  if (scrolling && (ev->message == "scroll")) {
    this << emit_bar_scroll_to (sc_pos + increment);
    win->delayed_message (this, "scroll", 10);
  }
}

void
hor_scrollbar_widget_rep::handle_scroll (scroll_event ev) {
  if (ev->which != "this") WK_FAILED ("invalid scroll");
  ref << emit_hor_scroll (ev->c1, ev->c2, ev->c3);
}

/******************************************************************************
* Routines for vertical scrollbars
******************************************************************************/

ver_scrollbar_widget_rep::ver_scrollbar_widget_rep (wk_widget ref):
  scrollbar_rep (ref) {}

ver_scrollbar_widget_rep::operator tree () {
  return "vertical scrollbar";
}

void
ver_scrollbar_widget_rep::handle_get_size (get_size_event ev) {
  if (ev->mode== 0) ev->w= 16*PIXEL;
  if (ev->mode==-1) {
    ev->w= 16*PIXEL;
    ev->h= 8*PIXEL;
  }
  if (ev->mode== 1) {
    gui_maximal_extents (ev->w, ev->h);
    ev->w= 16*PIXEL;
  }
}

void
ver_scrollbar_widget_rep::decode_position (SI& y1, SI& y2) {
  SI total = sc_max- sc_min; if (total==0) total=1;
  SI extra = (((w/PIXEL)*3)/4)*PIXEL + 3*PIXEL;
  SI min_h = min (h-2*extra, 12*PIXEL);
  DI real_h= h- 2*extra - min_h;
  SI bef   = (SI) ((before*real_h)/total);
  SI aft   = (SI) ((after*real_h)/total);
  SI p;

  if (bef+aft==0) aft=1;
  while (bef+aft< 4*PIXEL) {
    bef= aft= 2*PIXEL;
    p = (SI) (((sc_pos- sc_min)*real_h)/total);
    if (p<2*PIXEL) { bef=p; aft= 4*PIXEL-bef; }
    if (p>(real_h- 2*PIXEL)) { aft=real_h-p; bef= 4*PIXEL-aft; }
  }
  
  p = (SI) (((sc_pos- sc_min)*real_h)/total);
  y1= max (0, p-bef)+ extra;
  y2= min ((SI) real_h, p+aft) + extra + min_h;
}

SI
ver_scrollbar_widget_rep::encode_position (SI y) {
  DI total  = sc_max- sc_min; if (total==0) total=1;
  SI extra  = (((w/PIXEL)*3)/4)*PIXEL + 3*PIXEL;
  SI min_h  = min (h-2*extra, 12*PIXEL);
  SI real_h = h - 2*extra - min_h;
  SI dec_y  = (SI) (((y - extra - (min_h>>1)) * total) / real_h);
  return dec_y+ sc_min;
}

void
ver_scrollbar_widget_rep::handle_repaint (repaint_event ev) { (void) ev;
  renderer ren= win->get_renderer ();
  SI Y1, Y2;
  decode_position (Y1, Y2);
  layout_dark (ren, 0, 0, w, h);
  layout_lower (ren, 0, 0, w, h); 
  layout_default (ren, PIXEL, Y1, w- PIXEL, Y2);
  layout_higher (ren, PIXEL, Y1, w- PIXEL, Y2);

  SI aw= w-4*PIXEL;
  SI ah= (((w/PIXEL)*3)/4)*PIXEL;
  layout_up_arrow (ren, 2*PIXEL, h- 2*PIXEL- ah, aw, ah);
  layout_down_arrow (ren, 2*PIXEL, 2*PIXEL, aw, ah);
}

void
ver_scrollbar_widget_rep::handle_mouse (mouse_event ev) {
  string type= ev->type;
  SI     Y   = ev->y;
  SI     ah  = (((w/PIXEL)*3)/4)*PIXEL + 3*PIXEL;
  SI     Y1, Y2;
  decode_position (Y1, Y2);

  if (type == "press-left") {
    SI hh; ref << get_height (hh);
    if (Y < ah) {
      scrolling= true;
      increment= -5*PIXEL;
      this << emit_bar_scroll_to (sc_pos + increment);
      wk_grab_pointer (this);
      win->delayed_message (this, "scroll", 100);
    }
    else if (Y >= (h-ah)) {
      scrolling= true;
      increment= 5*PIXEL;
      this << emit_bar_scroll_to (sc_pos + increment);
      wk_grab_pointer (this);
      win->delayed_message (this, "scroll", 100);
    }
    else if (Y<Y1) this << emit_bar_scroll_to (sc_pos- hh);
    else if (Y>Y2) this << emit_bar_scroll_to (sc_pos+ hh);
    else {
      gripped= true;
      wk_grab_pointer (this);
      factor= ((double) (Y-Y1))/((double) (Y2-Y1));
    }
  }

  if (type == "press-middle") {
    SI y= encode_position (Y);
    this << emit_bar_scroll_to (y- ((after-before)>>1));
    wk_grab_pointer (this);
    factor= 0.5;
  }

  if (type == "press-right") {
    if (Y < Y1) this << emit_bar_scroll_to (sc_pos- 25*PIXEL);
    if (Y > Y2) this << emit_bar_scroll_to (sc_pos+ 25*PIXEL);
  }

  if ((type == "move") &&
      ((gripped && ev->pressed ("left")) || ev->pressed ("middle"))) {
    if (check_event (DRAG_EVENT)) return;
    SI y = encode_position (Y);
    SI dy= (SI) ((after+before)*factor);
    this << emit_bar_scroll_to (y+ before- dy);
  }

  if (((type == "release-left") || (type == "release-middle")) &&
      (!ev->pressed ("left")) && (!ev->pressed ("middle"))) {
    gripped= scrolling= false;
    wk_ungrab_pointer (this);
  }
}

void
ver_scrollbar_widget_rep::handle_alarm (alarm_event ev) {
  if (scrolling && (ev->message == "scroll")) {
    this << emit_bar_scroll_to (sc_pos + increment);
    win->delayed_message (this, "scroll", 10);
  }
}

void
ver_scrollbar_widget_rep::handle_scroll (scroll_event ev) {
  if (ev->which != "this") WK_FAILED ("invalid scroll");
  ref << emit_ver_scroll (ev->c1, ev->c2, ev->c3);
}
