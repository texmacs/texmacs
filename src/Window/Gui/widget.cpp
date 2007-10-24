
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
protected:
  widget_rep* w1;  // widget which triggers the signal
  string key1;     // name of the slot
  widget_rep* w2;  // widget which receives the signal
  string key2;     // name of the slot

public:
  inline widget_connection_rep (widget_rep* w1b, string key1b,
				widget_rep* w2b, string key2b):
    w1 (w1b), key1 (key1b), w2 (w2b), key2 (key2b) {}

  friend class widget_connection;
};

class widget_connection {
public:
CONCRETE(widget_connection);
  inline bool operator == (widget_connection con) {
    return rep->w1 == con->w1 && rep->key1 == con->key1 &&
           rep->w2 == con->w2 && rep->key2 == con->key2; }
  inline bool operator != (widget_connection con) {
    return rep->w1 != con->w1 || rep->key1 != con->key1 ||
           rep->w2 != con->w2 || rep->key2 != con->key2; }
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

widget_rep::connect (string key, widget w2, string key2) {
  widget_connection con (rep, key, w2.rep, key2);
  insert (out, con);
  insert (w2->in, con);
}

widget_rep::deconnect (string key, widget w2, string key2) {
  widget_connection con (rep, key, w2.rep, key2);
  remove (out, con);
  remove (w2->in, con);
}

/******************************************************************************
* Message passing
******************************************************************************/

void
widget_rep::set_blackbox (string key, blackbox val) {
  (void) key; (void) val;
  fatal_error ("No default implementation", "widget_rep::set_blackbox");
}

blackbox
widget_rep::get_blackbox (string key, int type_id) {
  (void) key; (void) type_id;
  fatal_error ("No default implementation", "widget_rep::set_blackbox");
}

void
widget_rep::changed (string key, int type_id) {
  blackbox val= get_blackbox (key, type_id);
  list<widget_connection> l= out;
  while (!nil (l)) {
    l->item->set_blackbox (key, val);
    l= l->next;
  }  
}
