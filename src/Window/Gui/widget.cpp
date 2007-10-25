
/******************************************************************************
* MODULE     : widget.cpp
* DESCRIPTION: Abstract widgets
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "widget.hpp"

/******************************************************************************
* The abstract widget_connection class
******************************************************************************/

class widget_connection_rep: public concrete_struct {
public:
  widget_rep* w1;  // widget which triggers the signal
  slot s1;         // corresponding slot
  widget_rep* w2;  // widget which receives the signal
  slot s2;         // corresponding slot

public:
  inline widget_connection_rep (widget_rep* w1b, slot s1b,
				widget_rep* w2b, slot s2b):
    w1 (w1b), s1 (s1b), w2 (w2b), s2 (s2b) {}

  friend class widget_connection;
};

class widget_connection {
public:
CONCRETE(widget_connection);
  inline widget_connection (widget_rep* w1, slot s1,
			    widget_rep* w2, slot s2):
    rep (new widget_connection_rep (w1, s1, w2, s2)) {}
  inline bool operator == (widget_connection con) {
    return rep->w1 == con->w1 && rep->s1 == con->s1 &&
           rep->w2 == con->w2 && rep->s2 == con->s2; }
  inline bool operator != (widget_connection con) {
    return rep->w1 != con->w1 || rep->s1 != con->s1 ||
           rep->w2 != con->w2 || rep->s2 != con->s2; }
};
CONCRETE_CODE(widget_connection);

/******************************************************************************
* Managing connections
******************************************************************************/

inline void
insert (list<widget_connection>& l, widget_connection con) {
  l= list<widget_connection> (con, l);
}

void
remove (list<widget_connection>& l, widget_connection con) {
  if (nil (l)) fatal_error ("removal not succeeded", "remove", "widget.cpp");
  else if (l->item == con) l= l->next;
  else remove (l->next, con);
}

widget_rep::widget_rep () {}

widget_rep::~widget_rep () {
  list<widget_connection> l= in;
  while (!nil (l)) {
    remove (l->item->w1->out, l->item);
    l= l->next;
  }
  l= out;
  while (!nil (l)) {
    remove (l->item->w2->in, l->item);
    l= l->next;
  }
  in = list<widget_connection> ();
  out= list<widget_connection> ();
}

void
widget_rep::connect (slot s, widget w2, slot s2) {
  widget_connection con (this, s, w2.rep, s2);
  insert (out, con);
  insert (w2->in, con);
}

void
widget_rep::deconnect (slot s, widget w2, slot s2) {
  widget_connection con (this, s, w2.rep, s2);
  remove (out, con);
  remove (w2->in, con);
}

/******************************************************************************
* Message passing
******************************************************************************/

void
widget_rep::send (slot s, blackbox val) {
  (void) s; (void) val;
  fatal_error ("No default implementation", "widget_rep::set_blackbox");
}

blackbox
widget_rep::query (slot s, int type_id) {
  (void) s; (void) type_id;
  fatal_error ("No default implementation", "widget_rep::set_blackbox");
  return blackbox ();
}

void
widget_rep::notify (slot s, int type_id) {
  blackbox val= query (s, type_id);
  list<widget_connection> l= out;
  while (!nil (l)) {
    l->item->w2->send (s, val);
    l= l->next;
  }  
}

/******************************************************************************
* Miscellaneous
******************************************************************************/

ostream&
widget_rep::print (ostream& out) {
  return out << "widget";
}
