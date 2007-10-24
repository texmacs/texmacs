
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

#include "analyze.hpp"
#include "font.hpp"
#include "file.hpp"
#include "Widkit/attribute_widget.hpp"
#include "Widkit/layout.hpp"

/******************************************************************************
* Tab-completion for file names
******************************************************************************/

#ifdef OS_WIN32
#define URL_CONCATER  '\\'
#else
#define URL_CONCATER  '/'
#endif

static void
file_completions_file (array<string>& a, url search, url u) {
  if (is_or (u)) {
    file_completions_file (a, search, u[1]);
    file_completions_file (a, search, u[2]);
  }
  else {
    url v= delta (search * url ("dummy"), u);
    if (is_none (v)) return;
    string s= as_string (v);
    if (is_directory (u)) s= s * string (URL_CONCATER);
    a << s;
  }
}

static void
file_completions_dir (array<string>& a, url search, url dir) {
  if (is_or (search)) {
    file_completions_dir (a, search[1], dir);
    file_completions_dir (a, search[2], dir);
  }
  else if (is_or (dir)) {
    file_completions_dir (a, search, dir[1]);
    file_completions_dir (a, search, dir[2]);
  }
  else {
    url u= search * dir * url_wildcard ("*");
    u= complete (u, "r");
    u= expand (u);
    file_completions_file (a, search, u);
  }
}

static array<string>
file_completions (url search, url dir) {
  array<string> a;
  file_completions_dir (a, search, dir);
  return a;
}

/******************************************************************************
* Input widgets
******************************************************************************/

class input_widget_rep: public attribute_widget_rep {
  string  s;           // the string being entered
  string  draw_s;      // the string being displayed
  string  type;        // expected type of string
  array<string> def;   // default possible input values
  command call_back;   // routine called on <return> or <escape>
  bool    ok;          // input not canceled
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
  input_widget_rep (command call_back);
  operator tree ();
  void update_draw_s ();

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

input_widget_rep::input_widget_rep (command call_back2):
  attribute_widget_rep (south_west),
  s (""), draw_s (""), type ("default"), def (), call_back (call_back2),
  ok (true), def_cur (0),
  dw (2*PIXEL), dh (2*PIXEL), pos (N(s)), scroll (0),
  got_focus (false), hilit (false) { dw*=SHRINK; dh*= SHRINK; }

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
input_widget_rep::handle_get_size (get_size_event ev) {
  SI dummy;
  font fn= the_display->default_font ();
  ev->h = (fn->y2- fn->y1+ 2*dh+ (SHRINK-1))/SHRINK;
  abs_round (ev->w, ev->h);
  if (ev->mode == 1) the_display->get_max_size (ev->w, dummy);
}

void
input_widget_rep::handle_repaint (repaint_event ev) { (void) ev;
  update_draw_s (); 

  metric ex;
  font fn= the_display->default_font ();
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

  if (got_focus && hilit) {
    layout_dark (win, 0, 0, w, h);
    layout_lower (win, 0, 0, w, h);
  }
  else layout_default (win, 0, 0, w, h);
  win->set_color (the_display->black);
  win->set_shrinking_factor (SHRINK);
  fn->var_draw (win, draw_s, dw- left, dh- bottom);
  if (got_focus) {
    SI pixel= SHRINK*PIXEL;
    win->set_color (the_display->red);
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

  /* tab-completion */
  if ((key == "tab" || key == "S-tab") && N(tabs) != 0) {
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
  if (key == "return") { ok= true; call_back (); }
  else if ((key == "escape") || (key == "C-c") ||
	   (key == "C-g")) { ok= false; call_back (); }
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
    else {
      if (N(key)!=1) return;
      int i (key[0]);
      if ((i>=0) && (i<32)) return;
    }
    s= s (0, pos) * key * s(pos, N(s));
    pos += N(key);
  }
  this << emit_invalidate_all ();
}

void
input_widget_rep::handle_mouse (mouse_event ev) {
  update_draw_s ();

  string type= ev->type;
  SI     x   = ev->x;
  font   fn  = the_display->default_font ();

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
    tree t=
      copy (the_display->get_selection (abstract (wk_widget (this)),
					"primary"));
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
    ok= (ev->s != "#f");
    if (attached ()) this << emit_invalidate_all ();
  }
  else if (ev->which == "type") type= copy (ev->s);
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
input_text_wk_widget (command call_back) {
  return new input_widget_rep (call_back);
}

wk_widget
input_text_wk_widget (command cb, string type, array<string> def) {
  int i, n= N(def);
  wk_widget inp= input_text_wk_widget (cb);
  inp << set_string ("type", type);
  if (n>0) inp << set_string ("input", def[0]);
  for (i=0; i<n; i++) inp << set_string ("default", def[i]);
  return inp;
}
