
/******************************************************************************
* MODULE     : refresh_widget.cpp
* DESCRIPTION: Widgets which are capable of being refreshed
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Widkit/attribute_widget.hpp"
#include "Widkit/layout.hpp"
#include "scheme.hpp"

widget make_menu_widget (object wid);

/******************************************************************************
* Refresh widgets
******************************************************************************/

class refresh_widget_rep: public basic_widget_rep {
  string tmwid;
  object curobj;
  widget cur;
  hashmap<object,widget> cache;
public:
  refresh_widget_rep (string tmwid);
  operator tree ();
  bool recompute ();
  void handle_refresh (refresh_event ev);
};

refresh_widget_rep::refresh_widget_rep (string tmwid2):
  basic_widget_rep (1), tmwid (tmwid2),
  curobj (false), cur (), cache (widget ()) {
    (void) recompute ();
    a[0]= concrete (cur); }

refresh_widget_rep::operator tree () {
  return tree (TUPLE, "refresh", tmwid);
}

bool
refresh_widget_rep::recompute () {
  string s= "'(vertical (link " * tmwid * "))";
  eval ("(lazy-initialize-force)");
  //cout << "Recompute " << tmwid << "\n";
  object xwid= call ("menu-expand", eval (s));
  //cout << "xwid= " << xwid << "\n";
  if (cache->contains (xwid)) {
    //if (curobj == xwid) cout << "Same " << s << "\n";
    if (curobj == xwid) return false;
    curobj= xwid;
    cur   = cache [xwid];
    //cout << "Cached " << s << "\n";
    return true;
  }
  else {
    curobj= xwid;
    //cout << "Compute " << s << "\n";
    object uwid= eval (s);
    //cout << "uwid= " << uwid << "\n";
    cur= make_menu_widget (uwid);
    //cout << "cur= " << cur << "\n";
    cache (xwid)= cur;
    return true;
  }
}

void
refresh_widget_rep::handle_refresh (refresh_event ev) { (void) ev;
  if (recompute ()) {
    SI ww1= a[0]->w, hh1= a[0]->h;
    SI ww2= a[0]->w, hh2= a[0]->h;
    a[0] << get_size (ww1, hh1);
    a[0]= concrete (cur);
    a[0] << get_size (ww1, hh1);
    if (attached ()) {
      if (!a[0]->attached () || a[0]->win != win)
        a[0] << emit_attach_window (win);
      if (ww1 == ww2 && hh1 == hh2) this << emit_update ();
      else concrete (this->win->get_widget ()) << emit_update ();
    }
  }
}

/******************************************************************************
* Refresh widget
******************************************************************************/

wk_widget
refresh_wk_widget (string tmwid) {
  return tm_new<refresh_widget_rep> (tmwid);
}
