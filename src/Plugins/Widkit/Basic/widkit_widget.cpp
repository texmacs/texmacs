
/******************************************************************************
* MODULE     : widkit_widget.hpp
* DESCRIPTION: Definition of abstract native widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "window.hpp"
#include "Widkit/widkit_widget.hpp"

/******************************************************************************
* Widget construction and destruction
******************************************************************************/

extern int widget_count;
widkit_widget_rep::widkit_widget_rep (array<widget> a2, array<string> n2,
				      gravity grav2):
  win (NULL), ox (0), oy (0), w (0), h (0),
  grav (grav2), a (a2), name (n2) { DEBUG(widget_count++); }

widkit_widget_rep::~widkit_widget_rep () { DEBUG(widget_count--); }

/******************************************************************************
* Computing lower left and upper right widget coordinates
******************************************************************************/

SI
get_dx (gravity grav, int w) {
  switch (grav) {
  case north_west: case west: case south_west:
    return 0;
  case north: case center: case south:
    return (w/(2*PIXEL))*PIXEL;
  case north_east: case east: case south_east:
    return w;
  }
  fatal_error ("unknown gravity", "get_dx", "widget.cpp");
  return 0; // Because of bug in certain versions of g++
}

SI
get_dy (gravity grav, int h) {
  switch (grav) {
  case north_west: case north: case  north_east:
    return 0;
  case west: case center: case east:
    return ((-h)/(2*PIXEL))*PIXEL;
  case south_west: case south: case south_east:
    return -h;
  }
  fatal_error ("unknown gravity", "get_dy", "widget.cpp");
  return 0; // Because of bug in certain versions of g++
}

SI
widkit_widget_rep::x1 () {
  return ox- get_dx (grav, w);
}

SI
widkit_widget_rep::y1 () {
  return oy- get_dy (grav, h)- h;
}

SI
widkit_widget_rep::x2 () {
  return ox- get_dx (grav, w)+ w;
}

SI
widkit_widget_rep::y2 () {
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

widget
widget::operator [] (string s) {
  string l= get_first (s);
  widget w;
  (*this) << get_widget (l, w);
  if (l==s) return w;
  else return w [s (N(l)+1, N(s))];
}

/******************************************************************************
* Other routines
******************************************************************************/

bool
widkit_widget_rep::attached () {
  return win!=NULL;
}

void
widkit_widget_rep::fatal_error (string message, string in, string file_name) {
  cerr << "\n------------------------------------------------------------------------------\n";
  cerr << widget (this);
  cerr << "------------------------------------------------------------------------------\n";
  cerr << "Fatal error: " << message << "\n";
  if (in != "") cerr << "In function: '" << in << "'\n";
  if (file_name != "") cerr << "See file   : " << file_name << "\n";
  exit (1);
}

widget
operator << (widget w, event ev) {
  if (!w->handle (ev))
    cerr << "Warning: " << ((tree) ev)
	 << " cannot be handled by\n" << w << "\n";
  return w;
}

static void
print_tree (ostream& out, tree t, int tab) {
  int i;
  for (i=0; i<tab; i++) out << "| ";
  if (is_atomic (t)) out << as_string (t) << "\n";
  else {
    out << as_string (t[0]) << "\n";
    for (i=1; i<N(t); i++) print_tree (out, t[i], tab+1);
  }
}

ostream&
operator << (ostream& out, widget w) {
  print_tree (out, (tree) w, 0);
  return out;
}
