
/******************************************************************************
* MODULE     : widget.cpp
* DESCRIPTION: Abstract widgets
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gui.hpp"
#include "message.hpp"
#include "font.hpp"
#include "window.hpp"

/******************************************************************************
* slot names, useful for debugging
******************************************************************************/

const char * 
slot_name (const slot s) { 
  const char * slot_names[]= {
    "SLOT_IDENTIFIER",
    "SLOT_WINDOW",
    "SLOT_RENDERER",
    "SLOT_VISIBILITY",
    "SLOT_FULL_SCREEN",
    "SLOT_NAME",
    "SLOT_MODIFIED",
    "SLOT_SIZE",
    "SLOT_POSITION",
    "SLOT_UPDATE",
    "SLOT_REFRESH",
    "SLOT_KEYBOARD",
    "SLOT_KEYBOARD_FOCUS",
    "SLOT_KEYBOARD_FOCUS_ON",
    "SLOT_MOUSE",
    "SLOT_MOUSE_GRAB",
    "SLOT_MOUSE_POINTER",
    "SLOT_INVALIDATE",
    "SLOT_INVALIDATE_ALL",
    "SLOT_REPAINT",
    "SLOT_DELAYED_MESSAGE",
    "SLOT_DESTROY",
    
    "SLOT_SHRINKING_FACTOR",
    "SLOT_EXTENTS",
    "SLOT_VISIBLE_PART",
    "SLOT_SCROLLBARS_VISIBILITY",
    "SLOT_SCROLL_POSITION",
    "SLOT_CANVAS",
    "SLOT_SCROLLABLE",
    "SLOT_CURSOR",
    
    "SLOT_HEADER_VISIBILITY",
    "SLOT_MAIN_MENU",
    "SLOT_MAIN_ICONS_VISIBILITY",
    "SLOT_MAIN_ICONS",
    "SLOT_MODE_ICONS_VISIBILITY",
    "SLOT_MODE_ICONS",
    "SLOT_FOCUS_ICONS_VISIBILITY",
    "SLOT_FOCUS_ICONS",
    "SLOT_USER_ICONS_VISIBILITY",
    "SLOT_USER_ICONS",
    "SLOT_SIDE_TOOLS_VISIBILITY",
    "SLOT_SIDE_TOOLS",
    "SLOT_BOTTOM_TOOLS_VISIBILITY",
    "SLOT_BOTTOM_TOOLS",
    "SLOT_FOOTER_VISIBILITY",
    "SLOT_LEFT_FOOTER",
    "SLOT_RIGHT_FOOTER",
    "SLOT_INTERACTIVE_MODE",
    "SLOT_INTERACTIVE_PROMPT",
    "SLOT_INTERACTIVE_INPUT",
    
    "SLOT_FORM_FIELD",
    "SLOT_STRING_INPUT",
    "SLOT_INPUT_TYPE",
    "SLOT_INPUT_PROPOSAL",
    "SLOT_FILE",
    "SLOT_DIRECTORY",

    "slot_id__LAST"
  };
  
  return slot_names[s.sid]; 
}

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
    rep (tm_new<widget_connection_rep> (w1, s1, w2, s2)) {}
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
  ASSERT (!is_nil (l), "removal not succeeded");
  if (l->item == con) l= l->next;
  else remove (l->next, con);
}

widget_rep::widget_rep () {}

widget_rep::~widget_rep () {
  list<widget_connection> l= in;
  while (!is_nil (l)) {
    remove (l->item->w1->out, l->item);
    l= l->next;
  }
  l= out;
  while (!is_nil (l)) {
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
  FAILED ("no default implementation");
}

blackbox
widget_rep::query (slot s, int type_id) {
  (void) s; (void) type_id;
  FAILED ("no default implementation");
  return blackbox ();
}

widget
widget_rep::read (slot s, blackbox index) {
  (void) s; (void) index;
  FAILED ("no default implementation");
  return widget ();
}

void
widget_rep::write (slot s, blackbox index, widget w) {
  (void) s; (void) index; (void) w;
  FAILED ("no default implementation");
}

void
widget_rep::notify (slot s, blackbox new_val) {
  list<widget_connection> l= out;
  while (!is_nil (l)) {
    l->item->w2->send (s, new_val);
    l= l->next;
  }  
}

/******************************************************************************
* Miscellaneous
******************************************************************************/

tm_ostream&
widget_rep::print (tm_ostream& out) {
  return out << "widget";
}

font
get_default_styled_font (int style) {
  bool tt  = (style & WIDGET_STYLE_MONOSPACED) != 0;
  bool mini= (style & WIDGET_STYLE_MINI) != 0;
  bool bold= (style & WIDGET_STYLE_BOLD) != 0;
  return get_default_font (tt, mini, bold);
}

#ifdef QTTEXMACS
bool use_side_tools= false;
#else
bool use_side_tools= false;
#endif

array<SI>
get_widget_size (widget w) {
  // FIXME: this does not work yet (nor under Qt, nor under X11)
  SI width, height;
  get_size (w, width, height);
  return array<SI> (width, height);
}

bool use_unified_toolbar= true;


void
tm_delete (widget_rep* ptr) {
  void *mem= ptr->derived_this ();
  ptr -> ~widget_rep ();
  fast_delete (mem);
}

