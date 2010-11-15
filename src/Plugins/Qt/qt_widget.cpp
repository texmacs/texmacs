
/******************************************************************************
* MODULE     : qt_widget.cpp
* DESCRIPTION: QT widget class
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "qt_widget.hpp"
#include "qt_simple_widget.hpp"
#include "qt_tm_widget.hpp"
#include "qt_utilities.hpp"


widget the_keyboard_focus (NULL);

widget
qt_widget_rep::plain_window_widget (string s) {
  (void) s;
  return widget ();
}

widget
qt_widget_rep::make_popup_widget () {
  return this;
}

widget
qt_widget_rep::popup_window_widget (string s) {
  (void) s;
  return widget ();
}




/******************************************************************************
* Global functions we export for the creation of widgets by TeXmacs
******************************************************************************/

// Window widgets creating functions
widget
plain_window_widget (widget w, string s) {
  // creates a decorated window with name s and contents w
  return concrete(w)->plain_window_widget (s);
}

widget
popup_window_widget (widget w, string s) {
  // creates an undecorated window with name s and contents w
  return concrete(w)->popup_window_widget (s);
}

void
destroy_window_widget (widget w) {
  // FIXME: Handle correcly
  // destroys a window as created by the above routines
  (void) w;

  // In the QT implementation explicitly destroying window widgets should not be necessary
  // since the widget itself destroy the Qt widget as soon as its destructor is called.
  // No memory leak should be caused by this trivial implementation.
}

/******************************************************************************
* Top-level widgets, typically given as an argument to plain_window_widget
* See also message.hpp for specific messages for these widgets
******************************************************************************/

widget
texmacs_widget (int mask, command quit) {
  // the main TeXmacs widget and a command which is called on exit
  // the mask variable indicates whether the menu, icon bars, status bar, etc.
  // are visible or not
  (void) mask; (void) quit; // FIXME: handle correctly mask and quit
  widget w= tm_new<qt_tm_widget_rep> (mask, quit);
  return w;
}

widget
popup_widget (widget w) {
  // a widget container which results w to be unmapped as soon as
  // the pointer quits the widget
  // used in edit_mouse.cpp to implement a contextual menu in the canvas
  return concrete(w)->make_popup_widget();
}

/******************************************************************************
*  Widgets which are not strictly required by TeXmacs
*  their implementation is void
******************************************************************************/

widget
empty_widget () {
  // an empty widget of size zero
  NOT_IMPLEMENTED;
  return widget();
}

widget
glue_widget (bool hx, bool vx, SI w, SI h) {
  //{ return widget(); }
  // an empty widget of minimal width w and height h and which is horizontally
  // resp. vertically extensible if hx resp. vx is true
  
  // glue_widget is used when detaching a canvas from the texmacs window
  // in view of attaching another one, e.g. when changing buffer.
  
  NOT_IMPLEMENTED;
  (void) hx; (void) vx; (void) w; (void) h;
  return tm_new<qt_view_widget_rep> (new QWidget ());
}

widget
glue_widget (tree col, bool hx, bool vx, SI w, SI h) {
  (void) col;
  return glue_widget (hx, vx, w, h);
}

widget
extend (widget w, array<widget> a) {
  (void) a;
  return w;
}

widget
wait_widget (SI width, SI height, string message) {
  // a widget of a specified width and height, displaying a wait message
  // this widget is only needed when using the X11 plugin
  (void) width; (void) height; (void) message;
  return widget();
}
