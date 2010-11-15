
/******************************************************************************
* MODULE     : texmacs_widget.cpp
* DESCRIPTION: Widget for the main TeXmacs window
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "window.hpp"
#include "Widkit/basic_widget.hpp"
#include "Widkit/Event/attribute_event.hpp"
#include "message.hpp"
#include "dictionary.hpp"

#define THIS (wk_widget (this))

/******************************************************************************
* The TeXmacs widget
******************************************************************************/

class texmacs_widget_rep: public basic_widget_rep {
protected:
  bool    footer_flag;  // footer visible ?
  command quit;         // called on destruction

protected:
  void set_left_footer (string s);
  void set_right_footer (string s);
  int  get_footer_mode ();
  void set_footer_mode (int which);
  bool get_footer_flag ();
  void set_footer_flag (bool on);

  void set_subwidget (wk_widget w, string which, wk_widget sw);
  bool get_subwidget_flag (wk_widget w);
  void set_subwidget_flag (wk_widget w, bool on);

public:
  texmacs_widget_rep (int mask, command quit);
  ~texmacs_widget_rep ();
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_set_widget (set_widget_event ev);
  void handle_get_widget (get_widget_event ev);
  void handle_set_integer (set_integer_event ev);
  void handle_set_string (set_string_event ev);
  void handle_get_string (get_string_event ev);
  void handle_set_coord2 (set_coord2_event ev);
  void handle_get_coord2 (get_coord2_event ev);
  void handle_set_coord4 (set_coord4_event ev);
  void handle_get_coord4 (get_coord4_event ev);
  void handle_keypress (keypress_event ev);
  void handle_mouse (mouse_event ev);
  void handle_keyboard_focus (keyboard_focus_event ev);
  void handle_resize (resize_event ev);
  void handle_destroy (destroy_event ev);

  bool handle (event ev);

  friend class tm_editor_rep;
  friend class texmacs_widget;
};

/******************************************************************************
* Subwidgets of main TeXmacs widgets
******************************************************************************/

static wk_widget
make_menu_bar () {
  array<wk_widget> M (2);
  array<string> M_name (2);
  M[0]= horizontal_array (array<wk_widget> (0));
  M[1]= glue_wk_widget (true, false);
  M_name[0]= "menu";

  array<wk_widget> V (2);
  array<string> V_name (2);
  V[0]= glue_wk_widget (true, false, 0, 2*PIXEL);
  V[1]= horizontal_array (M, M_name);
  V_name[1]= "bar";
  return optional_widget (vertical_list (V, V_name));
}

static wk_widget
make_icon_bar (bool on) {
  array<wk_widget> I (4);
  array<string> I_name (4);
  I[0]= glue_wk_widget (false, false, 3*PIXEL);
  I[1]= horizontal_array (array<wk_widget> (0));
  I[2]= glue_wk_widget (true, false);
  I[3]= glue_wk_widget (false, false, 3*PIXEL);
  I_name[1]= "icons";

  array<wk_widget> V (2);
  array<string> V_name (2);
  V[0]= separator_wk_widget (PIXEL, 2*PIXEL);
  V[1]= horizontal_array (I, I_name);
  V_name[1]= "bar";
  return optional_widget (vertical_list (V, V_name), on);
}

static wk_widget
make_header (int mask) {
  array<wk_widget> H (6);
  array<string> H_name (6);
  H[0]= make_menu_bar ();
  H[1]= make_icon_bar ((mask & 2) == 2);
  H[2]= make_icon_bar ((mask & 4) == 4);
  H[3]= make_icon_bar ((mask & 8) == 8);
  H[4]= make_icon_bar ((mask & 16) == 16);
  H[5]= glue_wk_widget (true, false, 0, 2*PIXEL);
  H_name[0]= "menu";
  H_name[1]= "main";
  H_name[2]= "mode";
  H_name[3]= "focus";
  H_name[4]= "user";
  return optional_widget (vertical_list (H, H_name), (mask & 1) == 1);
}

static wk_widget
make_footer (int mask) {
  array<wk_widget> F (3);
  array<string> F_name (3);
  F[0]= text_wk_widget (translate ("Welcome to TeXmacs!"));
  F[1]= glue_wk_widget (true, false);
  F[2]= text_wk_widget ("TeXmacs " TEXMACS_VERSION);
  F_name[0]= "left";
  F_name[1]= "middle";
  F_name[2]= "right";
  wk_widget ftr= horizontal_array (F, F_name, 1);
#ifdef OS_MACOS
  F << glue_wk_widget (false, false, 14*PIXEL, 0);
  F_name << string ("margin");
#endif

  array<wk_widget> I (3);
  array<string> I_name (3);
  I[0]= text_wk_widget (translate ("Input:"));
  I[1]= glue_wk_widget (true, false);
#ifdef OS_MACOS
  I[2]= glue_wk_widget (false, false, 18*PIXEL);
#else
  I[2]= glue_wk_widget (false, false, 2*PIXEL);
#endif
  I_name[0]= "left";
  I_name[1]= "middle";
  I_name[2]= "right";
  wk_widget iac= horizontal_list (I, I_name);
  //wk_widget iac= horizontal_array (I, I_name, 1);

  array<wk_widget> S (3);
  array<string> S_name (3);
  S[0]= ftr;
  S[1]= iac;
  S[2]= glue_wk_widget (false, false);
  S_name[0]= "default";
  S_name[1]= "interactive";
  return switch_widget (S, S_name, (mask & 32) == 32? 0: 2);
}

static wk_widget
make_texmacs_widget (int mask) {
  array<wk_widget> V (5);
  array<string> V_name (5);
  V[0]= make_header (mask);
  V[1]= canvas_widget (glue_wk_widget (), north_west, true);
  V[2]= glue_wk_widget (false, false, 0, PIXEL);
  V[3]= make_footer (mask);
  V[4]= glue_wk_widget (false, false, 0, PIXEL);
  V_name[0]= "header";
  V_name[1]= "canvas";
  V_name[3]= "footer";
  return vertical_list (V, V_name);
}

/******************************************************************************
* Constructor and destructor
******************************************************************************/

texmacs_widget_rep::texmacs_widget_rep (int mask, command quit2):
  basic_widget_rep (1), footer_flag (true), quit (quit2)
{
  a[0]= make_texmacs_widget (mask);
}

texmacs_widget_rep::~texmacs_widget_rep () {}

texmacs_widget_rep::operator tree () {
  return tree (TUPLE, "TeXmacs window", (tree) a[0]);
}

/******************************************************************************
* Accessors
******************************************************************************/

void
texmacs_widget_rep::set_subwidget (wk_widget w, string which, wk_widget sw) {
  SI ww1= 600*PIXEL, hh1=18*PIXEL, ww2=600*PIXEL, hh2=18*PIXEL;
  if (which == "icons") hh1= hh2= 18*PIXEL;
  w << get_size (ww1, hh1);
  w << set_widget (which, sw);
  w << get_size (ww2, hh2);
  if (attached ()) {
    if (hh1 == hh2) w << emit_update ();
    else THIS << emit_update ();
  }
}

bool
texmacs_widget_rep::get_subwidget_flag (wk_widget w) {
  int which;
  w << get_integer ("switch", which);
  return which == 0;
}

void
texmacs_widget_rep::set_subwidget_flag (wk_widget w, bool on) {
  if (get_subwidget_flag (w) != on) {
    w << set_integer ("switch", on? 0: 1);
    if (attached ()) THIS << emit_update ();
  }
}

/******************************************************************************
* Changing the footer
******************************************************************************/

void
texmacs_widget_rep::set_left_footer (string s) {
  wk_widget tw= text_wk_widget (s);
  set_subwidget (THIS ["footer"], "left", tw);
}

void
texmacs_widget_rep::set_right_footer (string s) {
  wk_widget tw= text_wk_widget (s);
  set_subwidget (THIS ["footer"], "right", tw);
}

int
texmacs_widget_rep::get_footer_mode () {
  int which;
  THIS ["footer"] << get_integer ("switch", which);
  return which;
}

void
texmacs_widget_rep::set_footer_mode (int new_mode) {
  int old_mode= get_footer_mode ();
  if (old_mode != new_mode) {
    wk_widget iac= THIS ["footer"] ["interactive"];
    if (old_mode == 1)
      send_keyboard_focus (abstract (THIS ["canvas"]));

    SI ww1= 600*PIXEL, hh1=18*PIXEL, ww2=600*PIXEL, hh2=18*PIXEL;
    THIS ["footer"] << get_size (ww1, hh1);
    THIS ["footer"] << set_integer ("switch", new_mode);
    THIS ["footer"] << get_size (ww2, hh2);
    if (attached ()) {
      if (hh1 == hh2) THIS ["footer"] << emit_update ();
      else THIS << emit_update ();
    }

    if (new_mode == 1) {
      iac << emit_update ();
      send_keyboard_focus (abstract (iac ["middle"]));
    }
  }
}

bool
texmacs_widget_rep::get_footer_flag () {
  return footer_flag;
}

void
texmacs_widget_rep::set_footer_flag (bool on) {
  footer_flag= on;
  if (get_footer_mode () != 1)
    set_footer_mode (on? 0: 2);
}

/******************************************************************************
* Handling standard events
******************************************************************************/

void
texmacs_widget_rep::handle_get_size (get_size_event ev) {
  if (ev->mode == 0) {
    ev->w= (800-24) * PIXEL; // (800-32) * PIXEL;
    ev->h= (600-28) * PIXEL;
  }
  else if (ev->mode == 1) gui_maximal_extents (ev->w, ev->h);
  else basic_widget_rep::handle_get_size (ev);
}

void
texmacs_widget_rep::handle_get_widget (get_widget_event ev) {
  a[0] << ev;
}

void
texmacs_widget_rep::handle_set_widget (set_widget_event ev) {
  if (ev->which == "menu bar")
    set_subwidget (THIS ["header"] ["menu"] ["bar"], "menu", ev->w);
  else if (ev->which == "main icons bar")
    set_subwidget (THIS ["header"] ["main"] ["bar"], "icons", ev->w);
  else if (ev->which == "mode icons bar")
    set_subwidget (THIS ["header"] ["mode"] ["bar"], "icons", ev->w);
  else if (ev->which == "focus icons bar")
    set_subwidget (THIS ["header"] ["focus"] ["bar"], "icons", ev->w);
  else if (ev->which == "user icons bar")
    set_subwidget (THIS ["header"] ["user"] ["bar"], "icons", ev->w);
  else if (ev->which == "interactive prompt")
    set_subwidget (THIS ["footer"] ["interactive"], "left", ev->w);
  else if (ev->which == "interactive input")
    set_subwidget (THIS ["footer"] ["interactive"], "middle", ev->w);
  else if (ev->which == "scrollable") {
    // FIXME: we manually take care of the keyboard focus
    // this should be done more systematically when changing subwidgets
    wk_widget old= THIS ["canvas"] ["scrollable"];
    bool focus= query_keyboard_focus (abstract (old));
    THIS ["canvas"] << set_widget ("scrollable", ev->w);
    THIS ["canvas"] << emit_attach_window (win);
    if (focus) send_keyboard_focus (abstract (THIS ["canvas"]));
    old << emit_attach_window (NULL);
    THIS ["canvas"] << emit_update ();
  }
  else a[0] << ev;
}

void
texmacs_widget_rep::handle_set_integer (set_integer_event ev) {
  if (ev->which == "shrinking factor")
    THIS ["canvas"] << set_integer ("shrinking factor", ev->i);
  else if (ev->which == "scrollbars")
    THIS ["canvas"] << set_integer ("scrollbars", ev->i);
  else WK_FAILED ("could not set integer attribute " * ev->which);
}

void
texmacs_widget_rep::handle_set_string (set_string_event ev) {
  if (ev->which == "window name") win->set_name (ev->s);
  else if (ev->which == "file");
  else if (ev->which == "header")
    set_subwidget_flag (THIS ["header"], ev->s == "on");
  else if (ev->which == "main icons")
    set_subwidget_flag (THIS ["header"] ["main"], ev->s == "on");
  else if (ev->which == "mode icons")
    set_subwidget_flag (THIS ["header"] ["mode"], ev->s == "on");
  else if (ev->which == "focus icons")
    set_subwidget_flag (THIS ["header"] ["focus"], ev->s == "on");
  else if (ev->which == "user icons")
    set_subwidget_flag (THIS ["header"] ["user"], ev->s == "on");
  else if (ev->which == "interactive mode")
    set_footer_mode (ev->s == "on"? 1: (footer_flag? 0: 2));
  else if (ev->which == "footer flag") set_footer_flag (ev->s == "on");
  else if (ev->which == "left footer") set_left_footer (ev->s);
  else if (ev->which == "right footer") set_right_footer (ev->s);
  else WK_FAILED ("could not set string attribute " * ev->which);
}

void
texmacs_widget_rep::handle_get_string (get_string_event ev) {
  if (ev->which == "header")
    ev->s= get_subwidget_flag (THIS ["header"])?
             string ("on"): string ("off");
  else if (ev->which == "main icons")
    ev->s= get_subwidget_flag (THIS ["header"] ["main"])?
             string ("on"): string ("off");
  else if (ev->which == "mode icons")
    ev->s= get_subwidget_flag (THIS ["header"] ["mode"])?
             string ("on"): string ("off");
  else if (ev->which == "focus icons")
    ev->s= get_subwidget_flag (THIS ["header"] ["focus"])?
             string ("on"): string ("off");
  else if (ev->which == "user icons")
    ev->s= get_subwidget_flag (THIS ["header"] ["user"])?
             string ("on"): string ("off");
  else if (ev->which == "interactive mode")
    ev->s= get_footer_mode () == 1? string ("on"): string ("off");
  else if (ev->which == "footer flag")
    ev->s= get_footer_flag ()? string ("on"): string ("off");
  else if (ev->which == "interactive input")
    THIS ["footer"] ["interactive"] ["middle"] << get_input_string (ev->s);
  else WK_FAILED ("could not set string attribute " * ev->which);
}

void
texmacs_widget_rep::handle_set_coord2 (set_coord2_event ev) {
  if (ev->which == "scroll position") THIS ["canvas"] << ev;
  else WK_FAILED ("could not set coord2 attribute " * ev->which);
}

void
texmacs_widget_rep::handle_get_coord2 (get_coord2_event ev) {
  if (ev->which == "scroll position") THIS ["canvas"] << ev;
  else WK_FAILED ("could not get coord2 attribute " * ev->which);
}

void
texmacs_widget_rep::handle_set_coord4 (set_coord4_event ev) {
  if (ev->which == "extents") THIS ["canvas"] << ev;
  else WK_FAILED ("could not set coord4 attribute " * ev->which);
}

void
texmacs_widget_rep::handle_get_coord4 (get_coord4_event ev) {
  if (ev->which == "extents") THIS ["canvas"] << ev;
  else if (ev->which == "visible") THIS ["canvas"] << ev;
  else WK_FAILED ("could not get coord4 attribute " * ev->which);
}

void
texmacs_widget_rep::handle_keypress (keypress_event ev) {
  if (get_footer_mode () == 1)
    THIS ["footer"] ["interactive"] ["middle"] << ev;
  else THIS ["canvas"] << ev;
}

void
texmacs_widget_rep::handle_mouse (mouse_event ev) {
  basic_widget_rep::handle_mouse (ev);
}

void
texmacs_widget_rep::handle_keyboard_focus (keyboard_focus_event ev) {
  if (get_footer_mode () == 1)
    THIS ["footer"] ["interactive"] ["middle"] << ev;
  else THIS ["canvas"] << ev;
}

void
texmacs_widget_rep::handle_resize (resize_event ev) {
  switch (get_footer_mode ()) {
  case 0: THIS ["footer"] ["middle"] << ev; break;
  case 1: THIS ["footer"] ["interactive"] ["middle"] << ev; break;
  }
  THIS ["canvas"] << ev;
}

void
texmacs_widget_rep::handle_destroy (destroy_event ev) {
  quit ();
}

/******************************************************************************
* Handling standard events
******************************************************************************/

bool
texmacs_widget_rep::handle (event ev) {
  // cout << "handle " << ((event) ev) << LF;
  switch (ev->type) {
  case SET_INTEGER_EVENT:
    handle_set_integer (ev);
    return true;
  case GET_STRING_EVENT:
    handle_get_string (ev);
    return true;
  case SET_STRING_EVENT:
    handle_set_string (ev);
    return true;
  case GET_COORD2_EVENT:
    handle_get_coord2 (ev);
    return true;
  case SET_COORD2_EVENT:
    handle_set_coord2 (ev);
    return true;
  case GET_COORD4_EVENT:
    handle_get_coord4 (ev);
    return true;
  case SET_COORD4_EVENT:
    handle_set_coord4 (ev);
    return true;
  default:
    return basic_widget_rep::handle (ev);
  }
}

/******************************************************************************
* exported routines
******************************************************************************/

wk_widget
texmacs_wk_widget (int mask, command quit) {
  return tm_new<texmacs_widget_rep> (mask, quit);
}
