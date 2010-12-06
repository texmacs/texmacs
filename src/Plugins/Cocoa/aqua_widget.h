
/******************************************************************************
* MODULE     : aqua_widget.h
* DESCRIPTION: Aqua widget class
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef AQUA_WIDGET_H
#define AQUA_WIDGET_H

#include "widget.hpp"

#ifndef MAC_COCOA_H
typedef struct TeXmacs_NSView {
  void * isa;
}  NSView ; // hack to allow inclusion in pure C++ sources
//#else
//typedef NSView * NSViewPtr ; 
typedef void *TMMenuItem;
#else
@class TMMenuItem;
#endif


class aqua_widget_rep : public widget_rep {
public:
	aqua_widget_rep() : widget_rep () { };
	
	virtual widget plain_window_widget (string s); 
	virtual widget make_popup_widget (); 
	virtual widget popup_window_widget (string s); 
 
  virtual TMMenuItem *as_menuitem();

};




class aqua_view_widget_rep: public aqua_widget_rep {
public:	
	NSView *view;

public:
  aqua_view_widget_rep (NSView *v);
  ~aqua_view_widget_rep ();

    virtual void send (slot s, blackbox val);
    // send a message val to the slot s
  virtual blackbox query (slot s, int type_id);
    // obtain information of a given type from the slot s
  virtual widget read (slot s, blackbox index);
    // abstract read access (of type s) of a subwidget at position index
  virtual void write (slot s, blackbox index, widget w);
    // abstract write access (of type s) of a subwidget at position index
  virtual void notify (slot s, blackbox new_val);
    // notification of a change on a slot s which contains a state variable
//  virtual void connect (slot s, widget w2, slot s2);
    // connect a state slot s to another slot s2 of another widget w2
//  virtual void deconnect (slot s, widget w2, slot s2);
    // deconnect a state slot s from another slot s2 of another widget w2

	virtual widget plain_window_widget (string s); 

};

class aqua_widget {
public:
	ABSTRACT_NULL(aqua_widget);
  inline bool operator == (aqua_widget w) { return rep == w.rep; }
  inline bool operator != (aqua_widget w) { return rep != w.rep; }
};
ABSTRACT_NULL_CODE(aqua_widget);

inline widget abstract (aqua_widget w) { return widget (w.rep); }
inline aqua_widget concrete (widget w) { return aqua_widget ((aqua_widget_rep*) w.rep); }

extern widget the_keyboard_focus;

#endif // defined AQUA_WIDGET_H
