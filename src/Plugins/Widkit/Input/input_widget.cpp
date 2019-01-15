
/******************************************************************************
* MODULE     : input_widget.cpp
* DESCRIPTION: Input of data by the user in textual form
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "analyze.hpp"
#include "font.hpp"
#include "file.hpp"
#include "window.hpp"
#include "Widkit/attribute_widget.hpp"
#include "Widkit/layout.hpp"
#include "scheme.hpp"
#include "message.hpp"

#ifdef OS_WIN32
#define URL_CONCATER  '\\'
#else
#define URL_CONCATER  '/'
#endif

/******************************************************************************
* Input widgets
******************************************************************************/

class input_widget_rep: public attribute_widget_rep {
  string  s;           // the string being entered
  string  draw_s;      // the string being displayed
  SI      text_h;      // text height
  string  type;        // expected type of string
  string  name;        // optional name of the input field
  string  serial;      // optional serial number of the input field
  array<string> def;   // default possible input values
  command call_back;   // routine called on <return> or <escape>
  int     style;       // style of widget
  bool    greyed;      // greyed input
  string  width;       // width of input field
  bool    persistent;  // don't complete after loss of focus
  bool    ok;          // input not canceled
  bool    done;        // call back has been called
  int     def_cur;     // current choice between default possible values
  SI      dw, dh;      // border width and height
  int     pos;         // cursor position
  SI      scroll;      // how much scrolled to the left
  bool    got_focus;   // got keyboard focus
  bool    hilit;       // hilit on keyboard focus
  array<string> tabs;  // tab completions
  int     tab_nr;      // currently visible tab-completion
  int     tab_pos;     // cursor position where tab was pressed

public:
  input_widget_rep (command call_back, int style, string width, bool persist);
  operator tree ();
  void update_draw_s ();
  void commit ();
  void cancel ();
  void set_type (string type);
  bool continuous ();

  void handle_get_size (get_size_event ev);
  void handle_repaint (repaint_event ev);
  void handle_keypress (keypress_event ev);
  void handle_mouse (mouse_event ev);
  void handle_keyboard_focus (keyboard_focus_event ev);
  void handle_keyboard_focus_on (keyboard_focus_on_event ev);

  void handle_set_string (set_string_event ev);
  void handle_get_string (get_string_event ev);
  void handle_get_coord2 (get_coord2_event ev);
  void handle_set_coord2 (set_coord2_event ev);
};

/******************************************************************************
* Routines for input_widgets
******************************************************************************/

#define SHRINK 3

input_widget_rep::input_widget_rep (command cb2, int st2, string w2, bool p2):
  attribute_widget_rep (south_west),
  s (""), draw_s (""),
  type ("default"), name ("default"), serial ("default"), def (),
  call_back (cb2), style (st2),
  greyed ((style & WIDGET_STYLE_INERT) != 0),
  width (w2), persistent (p2),
  ok (true), done (false), def_cur (0),
  dw (4*PIXEL), dh (2*PIXEL), pos (N(s)), scroll (0),
  got_focus (false), hilit (false)
{
  if ((style & WIDGET_STYLE_MINI) != 0) dh= 1.5 * PIXEL;
  dw *= SHRINK;
  dh *= SHRINK;
  font fn= get_default_styled_font (style);
  text_h = (fn->y2- fn->y1+ 2*dh+ (SHRINK-1))/SHRINK;
}

input_widget_rep::operator tree () {
  return tree (TUPLE, "input", s);
}

void
input_widget_rep::update_draw_s () {
  draw_s= s;
  if (type == "password") {
    draw_s= copy (s);
    for (int i=0; i<N(s); i++)
      draw_s[i]= '*';
  }
}

void
input_widget_rep::commit () {
  if (continuous ()) return;
  ok= true;
  done= true;
  call_back (list_object (object (s)));
}

void
input_widget_rep::cancel () {
  ok= false;
  done= true;
  call_back (list_object (object (false)));
}

void
input_widget_rep::set_type (string t) {
  int i= search_forwards (":", 0, t);
  if (i >= 0) {
    type= t (i+1, N(t));
    name= t (0, i);
    int j= search_forwards ("#", 0, name);
    if (j >= 0) {
      serial= name (j+1, N(name));
      name  = name (0, j);
    }
  }
  else type= t;
}

bool
input_widget_rep::continuous () {
  return
    starts (type, "search") ||
    starts (type, "replace-") ||
    starts (type, "spell") ||
    starts (serial, "form-");
}

void
input_widget_rep::handle_get_size (get_size_event ev) {
  SI ww = decode_length (width, this, style);
  SI dww= (SI) ((2*dw) / SHRINK);
  if (ends (width, "w") && is_double (width (0, N(width) - 1))) {
    font fn= get_default_styled_font (style);
    if (ev->mode == -1) ev->w= 0;
    else if (ev->mode == 0);
    else if (ev->mode == 1) ev->w= ww;
    ev->w= max (ev->w, (4*fn->wquad) / SHRINK + dww);
  }
  else if (ends (width, "em") && is_double (width (0, N(width) - 2)))
    ev->w= ww + dww;
  else if (ends (width, "px") && is_double (width (0, N(width) - 2)))
    ev->w= ww + dww;
  else if (ev->mode == 1)
    ev->w= ww;
  ev->h= text_h;
  abs_round (ev->w, ev->h);
}

void
input_widget_rep::handle_repaint (repaint_event ev) { (void) ev;
  renderer ren= ev->win;
  update_draw_s ();
  SI ecart= max (0, (h - (2*dh / SHRINK) - text_h) >> 1);

  metric ex;
  font fn= get_default_styled_font (style);
  fn->var_get_extents (draw_s, ex);
  SI left= ex->x1, bottom= fn->y1, right= ex->x2;
  fn->var_get_extents (draw_s (0, pos), ex);
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

  layout_default (ren, 0, 0, w, h);
  if (true) {
    SI yy= ((ecart + PIXEL/2) / PIXEL) * PIXEL;
    SI hh= ((h - 2*ecart + PIXEL/2) / PIXEL) * PIXEL;
    if (yy + hh + 2*PIXEL <= h) hh += 2 * PIXEL;
    else if (yy + hh + PIXEL <= h) hh += PIXEL;
    if (greyed) layout_default (ren, 0, yy, w, hh);
    else layout_pastel (ren, 0, yy, w, hh);
    layout_lower (ren, 0, yy, w, hh);
  }
  else if (got_focus && hilit) {
    layout_dark (ren, 0, 0, w, h);
    layout_lower (ren, 0, 0, w, h);
  }

  ren->set_shrinking_factor (SHRINK);
  SI pixel= SHRINK*PIXEL;
  ren->set_pencil (pencil (black, pixel));
  if (greyed) ren->set_pencil (pencil (dark_grey, pixel));
  ecart *= SHRINK;
  fn->var_draw (ren, draw_s, dw - left, dh - bottom + ecart);
  if (got_focus) {
    ren->set_pencil (pencil (red, pixel));
    ren->line (current + dw, dh + ecart,
	       current + dw, height - pixel - dh - ecart);
    ren->line (current + dw - pixel, dh + ecart,
	       current + dw + pixel, dh + ecart);
    ren->line (current + dw - pixel, height - pixel - dh - ecart,
	       current + dw + pixel, height - pixel - dh - ecart);
  }
  ren->set_shrinking_factor (1);
}

void
input_widget_rep::handle_keypress (keypress_event ev) {
  if (greyed) return;
  string key= ev->key;
  while ((N(key) >= 5) && (key(0,3) == "Mod") && (key[4] == '-') &&
	 (key[3] >= '1') && (key[3] <= '5')) key= key (5, N(key));
  if (key == "space") key= " ";
  if (key == "<") key= "<less>";
  if (key == ">") key= "<gtr>";

  /* tab-completion */
  if (continuous ());
  else if ((key == "tab" || key == "S-tab") && N(tabs) != 0) {
    int d = (key == "tab"? 1: N(tabs)-1);
    tab_nr= (tab_nr + d) % N(tabs);
    s     = s (0, tab_pos) * tabs[tab_nr];
    pos   = N(s);
    this << emit_invalidate_all ();
    return;
  }
  else if (key == "tab" || key == "S-tab") {
    if (pos != N(s)) return;
    tabs= copy (def);
    if (ends (type, "file") || type == "directory") {
      url search= url_here ();
      url dir= (ends (s, string (URL_CONCATER))? url (s): head (url (s)));
      if (type == "smart-file") search= url ("$TEXMACS_FILE_PATH");
      if (is_rooted (dir)) search= url_here ();
      if (is_none (dir)) dir= url_here ();
      tabs= file_completions (search, dir);
    }
    tabs= strip_completions (tabs, s);
    tabs= close_completions (tabs);
    if (N (tabs) == 0);
    else if (N (tabs) == 1) {
      s   = s * tabs[0];
      pos = N(s);
      tabs= array<string> (0);
    }
    else {
      tab_nr = 0;
      tab_pos= N(s);
      s      = s * tabs[0];
      pos    = N(s);
      beep ();
    }
    this << emit_invalidate_all ();
    return;
  }
  else {
    tabs   = array<string> (0);
    tab_nr = 0;
    tab_pos= 0;
  }

  /* other actions */
  if (continuous () &&
      (key == "return" ||
       key == "S-return" ||
       key == "home" ||
       key == "end" ||
       key == "up" ||
       key == "down" ||
       key == "pageup" ||
       key == "pagedown" ||
       key == "tab" ||
       key == "S-tab" ||
       key == "escape" ||
       (starts (type, "spell") && key >= "1" && key <= "9") ||
       (starts (type, "spell") && key == "+")));
  else if (key == "return") commit ();
  else if ((key == "escape") || (key == "C-c") ||
	   (key == "C-g")) cancel ();
  else if ((key == "left") || (key == "C-b")) {
    if (pos>0) tm_char_backwards (s, pos); }
  else if ((key == "right") || (key == "C-f")) {
    if (pos<N(s)) tm_char_forwards (s, pos); }
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
    if ((pos<N(s)) && (N(s)>0)) {
      int end= pos;
      tm_char_forwards (s, end);
      s= s (0, pos) * s (end, N(s));
    }
  }
  else if (key == "backspace" || key == "S-backspace") {
    if (pos>0) {
      int end= pos;
      tm_char_backwards (s, pos);
      s= s (0, pos) * s (end, N(s));
    }
  }
  else if (key == "C-backspace") {
    s= "";
    pos= 0;
  }
  else {
    if (starts (key, "<#"));
    else if (key == "<less>" || key == "<gtr>");
    else {
      if (N(key)!=1) return;
      int i (key[0]);
      if ((i>=0) && (i<32)) return;
    }
    s= s (0, pos) * key * s(pos, N(s));
    pos += N(key);
  }
  this << emit_invalidate_all ();
  if (continuous ())
    call_back (list_object (list_object (object (s), object (key))));
}

void
input_widget_rep::handle_mouse (mouse_event ev) {
  if (greyed) return;
  update_draw_s ();

  string type= ev->type;
  SI     x   = ev->x;
  font   fn  = get_default_styled_font (style);

  if (type == "press-left") {
    if (N(s)>0) {
      metric ex;
      SI old= 0;
      pos=0; tm_char_forwards (s, pos);
      for (; pos<=N(s); tm_char_forwards (s, pos)) {
	fn->var_get_extents (draw_s (0, pos), ex);
	if (((old+ ex->x2+ dw- ex->x1) >> 1) > (x*SHRINK+ scroll)) {
	  tm_char_backwards (s, pos);
	  break;
	}
	old= ex->x2+ dw- ex->x1;
	if (pos >= N(s)) break;
      }
    }
    win->set_keyboard_focus (this);
    this << emit_invalidate_all ();
  }

  if (type == "press-middle") {
    tree t; string sel;
    (void) get_selection ("primary", t, sel, "verbatim");
    if (is_tuple (t, "extern", 1)) {
      string ins= as_string (t[1]);
      ins= tm_encode (ins);
      s= s (0, pos) * ins * s(pos, N(s));
      pos += N(ins);
      this << emit_invalidate_all ();
    }
    else if (is_tuple (t, "texmacs", 3));
  }
}

void
input_widget_rep::handle_keyboard_focus (keyboard_focus_event ev) {
  if (got_focus && !ev->flag && !done && !persistent) {
    //if (type == "string") commit (); else cancel ();
    cancel ();
  }
  got_focus= ev->flag;
  if (attached ())
    this << emit_invalidate_all ();
}

void
input_widget_rep::handle_keyboard_focus_on (keyboard_focus_on_event ev) {
  if (ev->field == serial || ev->field == name || ev->field == type) {
    ev->done= true;
    send_keyboard_focus (abstract (this));
  }
}

void
input_widget_rep::handle_set_string (set_string_event ev) {
  if (ev->which == "input") {
    s= copy (ev->s);
    pos= N(s);
    ok= (ev->s != "#f");
    if (attached ()) this << emit_invalidate_all ();
  }
  else if (ev->which == "type") set_type (copy (ev->s));
  else if (ev->which == "default") def << copy (ev->s);
  else attribute_widget_rep::handle_set_string (ev);
}

void
input_widget_rep::handle_get_string (get_string_event ev) {
  if (ev->which == "input") {
    if (ok) ev->s= scm_quote (s);
    else ev->s= "#f";
  }
  else attribute_widget_rep::handle_get_string (ev);
}

void
input_widget_rep::handle_get_coord2 (get_coord2_event ev) {
  if (ev->which != "extra width") attribute_widget_rep::handle_get_coord2 (ev);
  else { ev->c1= 0; ev->c2= 0; }
}

void
input_widget_rep::handle_set_coord2 (set_coord2_event ev) {
  if (ev->which != "extra width") attribute_widget_rep::handle_set_coord2 (ev);
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

wk_widget
input_text_wk_widget (command call_back,
		      int style, string w, bool persistent)
{
  (void) style;
  return tm_new<input_widget_rep> (call_back, style, w, persistent);
}

wk_widget
input_text_wk_widget (command cb, string type, array<string> def,
		      int style, string w, bool persistent)
{
  (void) style;
  int i, n= N(def);
  wk_widget inp= input_text_wk_widget (cb, style, w, persistent);
  inp << set_string ("type", type);
  if (n>0) inp << set_string ("input", def[0]);
  for (i=0; i<n; i++) inp << set_string ("default", def[i]);
  return inp;
}
