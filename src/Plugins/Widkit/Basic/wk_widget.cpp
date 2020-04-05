
/******************************************************************************
* MODULE     : wk_widget.cpp
* DESCRIPTION: Definition of abstract native widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "window.hpp"
#include "Widkit/wk_widget.hpp"
#include "Widkit/Event/basic_event.hpp"
#include "analyze.hpp"
#include "font.hpp"

/******************************************************************************
* Widget construction and destruction
******************************************************************************/

extern int widget_count;
wk_widget_rep::wk_widget_rep (
  array<wk_widget> a2, array<string> n2, gravity grav2):
    win (NULL), ox (0), oy (0), w (0), h (0),
    grav (grav2), a (a2), name (n2) { TM_DEBUG(widget_count++); }

wk_widget_rep::~wk_widget_rep () { TM_DEBUG(widget_count--); }

/******************************************************************************
* Computing lower left and upper right widget coordinates
******************************************************************************/

SI
get_dx (gravity grav, SI w) {
  switch (grav) {
  case north_west: case west: case south_west:
    return 0;
  case north: case center: case south:
    return (w/(2*PIXEL))*PIXEL;
  case north_east: case east: case south_east:
    return w;
  }
  FAILED ("unknown gravity");
  return 0;
}

SI
get_dy (gravity grav, SI h) {
  switch (grav) {
  case north_west: case north: case  north_east:
    return 0;
  case west: case center: case east:
    return ((-h)/(2*PIXEL))*PIXEL;
  case south_west: case south: case south_east:
    return -h;
  }
  FAILED ("unknown gravity");
  return 0;
}

gravity
opposite (gravity grav) {
  switch (grav) {
  case north_west: return south_east;
  case north     : return south;
  case north_east: return south_west;
  case west      : return east;
  case center    : return center;
  case east      : return west;
  case south_west: return north_east;
  case south     : return north;
  case south_east: return north_west;
  }
  FAILED ("unknown gravity");
  return center;
}

SI
wk_widget_rep::x1 () {
  return ox- get_dx (grav, w);
}

SI
wk_widget_rep::y1 () {
  return oy- get_dy (grav, h)- h;
}

SI
wk_widget_rep::x2 () {
  return ox- get_dx (grav, w)+ w;
}

SI
wk_widget_rep::y2 () {
  return oy- get_dy (grav, h);
}

/******************************************************************************
* Finding and assigning children by name
******************************************************************************/

static string
get_first (string s) {
  int i;
  for (i=0; i<N(s); i++)
    if (s[i] == '|') return s (0, i);
  return s;
}

wk_widget
wk_widget::operator [] (string s) {
  string l= get_first (s);
  wk_widget w;
  (*this) << get_widget (l, w);
  if (l==s) return w;
  else return w [s (N(l)+1, N(s))];
}

/******************************************************************************
* Other routines
******************************************************************************/

bool
wk_widget_rep::attached () {
  return win != NULL;
}

bool
wk_widget_rep::is_window_widget () {
  return false;
}

void
wk_widget_rep::wk_error (string message) {
  widkit_error << "------------------------------------------------------------------------------\n";
  widkit_error << wk_widget (this);
  widkit_error << "------------------------------------------------------------------------------\n";
  widkit_error << message << "\n";
}

wk_widget
operator << (wk_widget w, event ev) {
  if (!w->handle (ev))
    widkit_warning << ((tree) ev) << " cannot be handled by\n" << w << "\n";
  return w;
}

static void
print_tree (tm_ostream& out, tree t, int tab) {
  int i;
  for (i=0; i<tab; i++) out << "| ";
  if (is_atomic (t)) out << as_string (t) << "\n";
  else {
    out << as_string (t[0]) << "\n";
    for (i=1; i<N(t); i++) print_tree (out, t[i], tab+1);
  }
}

tm_ostream&
wk_widget_rep::print (tm_ostream& out) {
  print_tree (out, operator tree (), 0);
  return out;  
}

tm_ostream&
operator << (tm_ostream& out, wk_widget w) {
  print_tree (out, (tree) w, 0);
  return out;
}

/******************************************************************************
* Pointer grabbing
******************************************************************************/

void
wk_grab_pointer (wk_widget w) {
  ASSERT (!is_nil (w) && w->win != NULL, "widget should be attached");
  w->win->set_mouse_grab (abstract (w), true);
}

void
wk_ungrab_pointer (wk_widget w) {
  ASSERT (!is_nil (w) && w->win != NULL, "widget should be attached");
  w->win->set_mouse_grab (abstract (w), false);
}

bool
wk_has_pointer_grab (wk_widget w) {
  return !is_nil (w) && w->win != NULL &&
    w->win->get_mouse_grab (abstract (w));
}

/******************************************************************************
* Length conversions
******************************************************************************/

#define SHRINK 3

SI
decode_length (string width, wk_widget wid, int style) {
  SI ex, ey;
  if (wid->win == NULL) gui_maximal_extents (ex, ey);
  else wid->win->get_size (ex, ey);

  double w_len;
  string w_unit;
  parse_length (width, w_len, w_unit);
  if (w_unit == "w") return (SI) (w_len * ex);
  else if (w_unit == "h") return (SI) (w_len * ey);
  else if (w_unit == "px") return (SI) (w_len * PIXEL);
  else if (w_unit == "em") {
    font fn= get_default_styled_font (style);
    return (SI) ((w_len * fn->wquad) / SHRINK);
  }
  else return ex;
}
