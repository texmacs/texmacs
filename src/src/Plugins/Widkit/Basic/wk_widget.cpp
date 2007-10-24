
/******************************************************************************
* MODULE     : wk_widget.hpp
* DESCRIPTION: Definition of abstract native widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "window.hpp"
#include "Widkit/wk_widget.hpp"

/******************************************************************************
* Widget construction and destruction
******************************************************************************/

extern int widget_count;
wk_widget_rep::wk_widget_rep (
  array<wk_widget> a2, array<string> n2, gravity grav2):
    win (NULL), ox (0), oy (0), w (0), h (0),
    grav (grav2), a (a2), name (n2) { DEBUG(widget_count++); }

wk_widget_rep::~wk_widget_rep () { DEBUG(widget_count--); }

/******************************************************************************
* Message passing
******************************************************************************/

void
wk_widget_rep::set_blackbox (string key, blackbox val) {
  int type_id= type_box (val);
  // cout << "Setting " << key << " := " << val << ", " << type_id << "\n";
  if (type_id == type_helper<int>::id)
    wk_widget (this) << ::set_integer (key, open_box<int> (val));
  else if (type_id == type_helper<string>::id)
    wk_widget (this) << ::set_string (key, open_box<string> (val));
  else if (type_id == type_helper<widget>::id)
    wk_widget (this) << ::set_widget (key, concrete (open_box<widget> (val)));
  else fatal_error ("cannot handle message type",
		    "wk_widget_rep::set_blackbox");
}

blackbox
wk_widget_rep::get_blackbox (string key, int type_id) {
  //cout << "Getting " << key << ", " << type_id << "\n";
  if (type_id == type_helper<int>::id) {
    int i;
    wk_widget (this) << ::get_integer (key, i);
    //cout << "Got " << key << " = " << i << "\n";
    return close_box<int> (i);
  }
  else if (type_id == type_helper<string>::id) {
    string s;
    wk_widget (this) << ::get_string (key, s);
    //cout << "Got " << key << " = " << s << "\n";
    return close_box<string> (s);
  }
  else if (type_id == type_helper<widget>::id) {
    wk_widget w;
    wk_widget (this) << ::get_widget (key, w);
    //cout << "Got " << key << " = " << w << "\n";
    return close_box<widget> (abstract (w));
  }
  else {
    fatal_error ("cannot handle message type",
		 "wk_widget_rep::get_blackbox");
    return blackbox ();
  }
}

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
  fatal_error ("unknown gravity", "get_dx", "wk_widget.cpp");
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
  fatal_error ("unknown gravity", "get_dy", "wk_widget.cpp");
  return 0; // Because of bug in certain versions of g++
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
  return win!=NULL;
}

void
wk_widget_rep::fatal_error (string message, string in, string file_name) {
  cerr << "\n------------------------------------------------------------------------------\n";
  cerr << wk_widget (this);
  cerr << "------------------------------------------------------------------------------\n";
  cerr << "Fatal error: " << message << "\n";
  if (in != "") cerr << "In function: '" << in << "'\n";
  if (file_name != "") cerr << "See file   : " << file_name << "\n";
  exit (1);
}

wk_widget
operator << (wk_widget w, event ev) {
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
operator << (ostream& out, wk_widget w) {
  print_tree (out, (tree) w, 0);
  return out;
}
