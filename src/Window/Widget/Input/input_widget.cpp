
/******************************************************************************
* MODULE     : input_widget.cpp
* DESCRIPTION: Input of data by the user in textual form
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Widget/attribute_widget.hpp"
#include "analyze.hpp"
#include "font.hpp"
#include "Widget/layout.hpp"

/******************************************************************************
* Input widgets
******************************************************************************/

class input_widget_rep: public attribute_widget_rep {
  string  s;           // the string being entered
  string  type;        // expected type of string
  array<string> def;   // default possible input values
  command call_back;   // routine called on <return> or <escape>
  int     def_cur;     // current choice between default possible values
  SI      dw, dh;      // border width and height
  int     pos;         // cursor position
  SI      scroll;      // how much scrolled to the left
  bool    got_focus;   // got keyboard focus
  bool    hilit;       // hilit on keyboard focus

public:
  input_widget_rep (display dis, command call_back);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_repaint (repaint_event ev);
  void handle_keypress (keypress_event ev);
  void handle_mouse (mouse_event ev);
  void handle_keyboard_focus (keyboard_focus_event ev);

  void handle_set_string (set_string_event ev);
  void handle_get_string (get_string_event ev);
};

/******************************************************************************
* Routines for input_widgets
******************************************************************************/

#define SHRINK 3

input_widget_rep::input_widget_rep (display dis, command call_back2):
  attribute_widget_rep (dis, south_west),
  s (""), type ("default"), def (), call_back (call_back2), def_cur (0),
  dw (2*PIXEL), dh (2*PIXEL), pos (N(s)), scroll (0),
  got_focus (false), hilit (false) { dw*=SHRINK; dh*= SHRINK; }

input_widget_rep::operator tree () {
  return tree (TUPLE, "input", s);
}

void
input_widget_rep::handle_get_size (get_size_event ev) {
  font fn= dis->default_font ();
  ev->h = (fn->y2- fn->y1+ 2*dh+ (SHRINK-1))/SHRINK;
  abs_round (ev->w, ev->h);
}

void
input_widget_rep::handle_repaint (repaint_event ev) { (void) ev;
  metric ex;
  font fn= dis->default_font ();
  fn->var_get_extents (s, ex);
  SI left= ex->x1, bottom= fn->y1, right= ex->x2;
  fn->var_get_extents (s (0, pos), ex);
  SI current= ex->x2- ex->x1;

  SI text_width= right-left;
  SI width= w*SHRINK- 2*dw, height= h*SHRINK;
  SI marge= width>>2;
  if ((current-scroll) > (width-marge)) scroll= current+ marge- width;
  if ((current-scroll) < marge) scroll= current- marge;
  if (scroll > (text_width- width)) scroll= text_width- width;
  if (scroll < 0) scroll= 0;
  left    += scroll;
  current -= scroll;

  if (got_focus && hilit) {
    layout_dark (win, 0, 0, w, h);
    layout_lower (win, 0, 0, w, h);
  }
  else layout_default (win, 0, 0, w, h);
  win->set_color (dis->black);
  win->set_shrinking_factor (SHRINK);
  fn ->var_draw (win, s, dw- left, dh- bottom);
  if (got_focus) {
    SI pixel= SHRINK*PIXEL;
    win->set_color (dis->red);
    win->line (current+ dw, dh,
	       current+ dw, height- pixel- dh);
    win->line (current+ dw- pixel, dh,
	       current+ dw+ pixel, dh);
    win->line (current+ dw- pixel, height- pixel- dh,
	       current+ dw+ pixel, height- pixel- dh);
  }
  win->set_shrinking_factor (1);
}

void
input_widget_rep::handle_keypress (keypress_event ev) {
  string key= ev->key;
  while ((N(key) >= 5) && (key(0,3) == "Mod") && (key[4] == '-') &&
	 (key[3] >= '1') && (key[3] <= '5')) key= key (5, N(key));
  if (key == "space") key= " ";

  if (key == "return") { s= quote (s); call_back (); }
  else if ((key == "escape") || (key == "C-c") ||
	   (key == "C-g")) { s= "cancel"; call_back (); }
  else if ((key == "left") || (key == "C-b")) { if (pos>0) pos--; }
  else if ((key == "right") || (key == "C-f")) { if (pos<N(s)) pos++; }
  else if ((key == "home") || (key == "C-a")) pos=0;
  else if ((key == "end") || (key == "C-e")) pos=N(s);
  else if ((key == "up") || (key == "C-p")) {
    if (N(def) > 0) {
      def_cur= (def_cur+1) % N(def);
      s      = copy (def[def_cur]);
      pos    = N(s);
    }
  }
  else if ((key == "down") || (key == "C-n")) {
    if (N(def) > 0) {
      def_cur= (def_cur+N(def)-1) % N(def);
      s      = copy (def[def_cur]);
      pos    = N(s);
    }
  }
  else if (key == "C-k") s= s (0, pos);
  else if ((key == "C-d") || (key == "delete")) {
    if ((pos<N(s)) && (N(s)>0))
      s= s (0, pos) * s (pos+1, N(s));
  }
  else if (key == "backspace") {
    if (pos>0) {
      pos--;
      s= s (0, pos) * s (pos+1, N(s));
    }
  }
  else {
    if (N(key)!=1) return;
    int i (key[0]);
    if ((i>=0) && (i<32)) return;
    s= s (0, pos) * key * s(pos, N(s));
    pos += N(key);
  }
  this << emit_invalidate_all ();
}

void
input_widget_rep::handle_mouse (mouse_event ev) {
  string type= ev->type;
  SI     x   = ev->x;
  font   fn  = dis->default_font ();

  if (type == "press-left") {
    metric ex;
    SI old= 0;
    for (pos=1; pos<=N(s); pos++) {
      fn->var_get_extents (s (0, pos), ex);
      if (((old+ ex->x2+ dw- ex->x1) >> 1) > (x*SHRINK+ scroll)) break;
      old= ex->x2+ dw- ex->x1;
    }
    pos--;
    win->set_keyboard_focus (this);
    this << emit_invalidate_all ();
  }

  if (type == "press-middle") {
    tree t= copy (dis->get_selection (widget (this), "primary"));
    if (is_tuple (t, "extern", 1)) {
      string ins= as_string (t[1]);
      s= s (0, pos) * ins * s(pos, N(s));
      pos += N(ins);
      this << emit_invalidate_all ();
    }
    else if (is_tuple (t, "texmacs", 3));
  }
}

void
input_widget_rep::handle_keyboard_focus (keyboard_focus_event ev) {
  got_focus= ev->flag;
  this << emit_invalidate_all ();
}

void
input_widget_rep::handle_set_string (set_string_event ev) {
  if (ev->which == "input") {
    s= copy (ev->s);
    pos= N(s);
    if (attached ()) this << emit_invalidate_all ();
  }
  else if (ev->which == "type") type= copy (ev->s);
  else if (ev->which == "default") def << copy (ev->s);
  else attribute_widget_rep::handle_set_string (ev);
}

void
input_widget_rep::handle_get_string (get_string_event ev) {
  if (ev->which == "input") ev->s= s;
  else attribute_widget_rep::handle_get_string (ev);
}

/******************************************************************************
* Interface
******************************************************************************/

event
set_input_string (string s) {
  return set_string ("input", s);
}

event
get_input_string (string& s) {
  return get_string ("input", s);
}

widget
input_text_widget (command call_back) {
  return new input_widget_rep (current_display (), call_back);
}
