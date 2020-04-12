
/******************************************************************************
* MODULE     : qt_widget.hpp
* DESCRIPTION: QT widget class
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QT_WIDGET_HPP
#define QT_WIDGET_HPP

#include "fast_alloc.hpp"
#include "widget.hpp"
#include "message.hpp"
#include <QPointer>

class QWidget;
class QLayoutItem;
class QAction;
class QMenu;
class qt_widget;

/*! The base class of all TeXmacs widgets in the QT interface.

 Every TeXmacs Widget is an entity which can be manipulated from within scheme.
 The interface to this is provided by widget_rep, which we extend. The methods
 here declared must be implemented by the QT wrappers.
 
 We must distinguish between three stages of widget creation:
 
  1) Scheme widgets: these are scheme trees describing the widgets. These trees
     are the result of several macro expansions on the code given by the user in
     scheme (see under progs/kernel/gui) and are wholly handled by the TeXmacs
     core. The Qt side plays no role here.
  2) Parsed widgets: the C++ interpretation of the scheme widget. Merely a
     collection of qt_widget instances, without any (visible) QWidgets
     instantiated.
  3) Compiled widgets: The Qt rendering of the parsed widgets. The outermost
     widget receives a message to display which possibly translates to the
     instantiation of a QWidget using as_qwidget(). This call will in turn
     call this method for children widgets and so on down the chain.

 Additionally, we provide several methods to cope with the fact that TeXmacs
 expects widgets to behave in three different ways: as embedded widgets,
 as menus and as regular widgets, all of which are essentially different in QT.
 Hence the need to construct the QT widgets differently on a request basis via
 the four methods as_qaction(), as_qlayoutitem(), as_qwidget() and get_qmenu().
 Reimplementations of these should cope with the basic differences these
 concepts have in QT:
 
 * as_qaction() returns a QAction, mainly to be inserted into a QMenuBar. 
   QWidgets should render themselves in an adequate fashion (i.e. with small
   font, using popup buttons where appropriate, etc.)
 
 * as_qlayoutitem() usually does one of two things: either embedding a QWidget
   in a QLayouttItem, so as to add it to some QLayout, or creating a "pure"
   QLayoutItem.
 
 * as_qwidget() returns a regular QWidget, for example to be embedded in a
   standalone window.
 
 get_qmenu() returns the QMenu owned by the QAction returned by as_qaction().
 This method also promotes the object upon which it was invoked to owner of the
 QMenu. Again: calling wid->get_qmenu() creates the QMenu and leaves ownership
 of it to wid.
 
 In the first three cases a new instance of the QObject is created if neede
 and ownership is transferred to the caller. One reason why the underlying
 QWidget is NOT owned by the qt_widget is that several qt_widgets may have the
 same underlying QWidget (UPD: which?). Another are the problems due to delayed
 deletion of TeXmacs objects. The only exception to this rule are those
 qt_widgets whose compiled widgets are windows to whom we leave the
 responsibility of deletion. They are the outmost widget, typically a 
 QTMPlainWindow or a QTMWindow, who should be the parent of all the related
 QWidgets.
 
 Most of the UI items are implemented by qt_ui_element_rep, with some
 exceptions. Creation from the TeXmacs side is done using the global functions
 declared in Graphics/Gui/widget.hpp.
 */
class qt_widget;

class qt_widget_rep : public widget_rep {
protected:
  array<widget> children;
public:
  long                id;
  QPointer<QWidget> qwid;

  /*! A list of all supported widget types.
   FIXME: This enum breaks the basic inheritance rules, since we have to 
   update the base class each time we implement a new subclass. It's also some
   sort of bastardic and lame RTTI, which might be proof of bad design...
   But it comes handy in a few places right now ;)
   NOTE: please modify qt_widget_type_strings[] in type_as_string() accordingly!
   */
  enum types {
    none = 0,
    input_widget,    file_chooser,       window_widget,      view_widget,
    horizontal_menu, vertical_menu,      horizontal_list,    vertical_list,
    tile_menu,       minibar_menu,       menu_separator,     menu_group, 
    pulldown_button, pullright_button,   menu_button,        balloon_widget,
    text_widget,     xpm_widget,         toggle_widget,      enum_widget,
    choice_widget,   scrollable_widget,  hsplit_widget,      vsplit_widget,
    aligned_widget,  tabs_widget,        icon_tabs_widget,   wrapped_widget,
    refresh_widget,  refreshable_widget, glue_widget,        resize_widget,
    texmacs_widget,  simple_widget,      embedded_tm_widget, popup_widget,
    field_widget,    filtered_choice_widget, tree_view_widget
  } ;
  
  types type;
  
  qt_widget_rep (types _type=none, QWidget* _qwid=0);
  virtual ~qt_widget_rep ();
  virtual inline string get_nickname () { return "popup"; }
  
  virtual widget plain_window_widget (string name, command quit);
  virtual widget make_popup_widget ();
  virtual widget popup_window_widget (string s);

  void add_child (widget a);
  void add_children (array<widget> a);
  
  ////////////////////// Qt semantics of abstract texmacs widgets  
  
  virtual QAction*         as_qaction ();
  virtual QWidget*         as_qwidget ();
  virtual QLayoutItem*     as_qlayoutitem ();
  virtual QList<QAction*>* get_qactionlist();


  ////////////////////// Debugging
  
  string type_as_string() { 
    static const char* qt_widget_type_strings[] = {
      "none",
      "input_widget",       "file_chooser",       "window_widget",
      "view_widget",        "horizontal_menu",    "vertical_menu",
      "horizontal_list",    "vertical_list",      "tile_menu",
      "minibar_menu",       "menu_separator",     "menu_group",
      "pulldown_button",    "pullright_button",   "menu_button",
      "balloon_widget",     "text_widget",        "xpm_widget",
      "toggle_widget",      "enum_widget",        "choice_widget",
      "scrollable_widget",  "hsplit_widget",      "vsplit_widget",
      "aligned_widget",     "tabs_widget",        "icon_tabs_widget",
      "wrapped_widget",     "refresh_widget",     "refreshable_widget",
      "glue_widget",        "resize_widget",      "texmacs_widget",
      "simple_widget",      "embedded_tm_widget", "popup_widget",
      "field_widget",       "filtered_choice_widget", "tree_view_widget"
    };
    return string (qt_widget_type_strings[type]) * "\t id: " * as_string (id);
  }
  
  ////////////////////// Handling of TeXmacs' messages

    /// See widkit_wrapper.cpp for the reference list of slots. Based on the
    /// handlers invoked by wk_widget_rep::send(), query() etc. we can decide
    /// what slots must implement each qt_widget.
  virtual void send (slot s, blackbox val);
  
  virtual blackbox query (slot s, int type_id) {
    (void) type_id;
    if (DEBUG_QT)
      debug_qt << "qt_widget_rep::query(), unhandled " << slot_name (s) 
               << " for widget of type: " << type_as_string() << LF;
    return blackbox ();
  }
  
  virtual widget read (slot s, blackbox index) {
    (void) index;
    if (DEBUG_QT)
      debug_qt << "qt_widget_rep::read(), unhandled " << slot_name (s) 
               << " for widget of type: " << type_as_string() << LF;
    return widget ();
  }
  
  virtual void write (slot s, blackbox index, widget w) {
    (void) index; (void) w;
    if (DEBUG_QT)
      debug_qt << "qt_widget_rep::write(), unhandled " << slot_name (s) 
               << " for widget of type: " << type_as_string() << LF;
  }
  
  virtual void notify (slot s, blackbox new_val) {
    (void) new_val;
    if (DEBUG_QT)
      debug_qt << "qt_widget_rep::notify(), unhandled " << slot_name (s)
               << " for widget of type: " << type_as_string() << LF;
  }
};


/*! Reference counting mechanism.

 Like elsewhere in TeXmacs, this is a wrapper around its corresponding 
 qt_widget_rep which implements reference counting.
 See src/Kernel/Abstractions/basic.hpp
*/
class qt_widget {
public:
  ABSTRACT_NULL(qt_widget); // Automagically declared constructor, methods, etc.

  inline bool operator == (qt_widget w) { return rep == w.rep; }
  inline bool operator != (qt_widget w) { return rep != w.rep; }
};

// Automagically create definitions for the stuff declared inside qt_widget with
// the macro ABSTRACT_NULL(). See src/Kernel/Abstractions/basic.hpp
ABSTRACT_NULL_CODE(qt_widget);

  // Needed for the ntuples (see ntuple.h)
tm_ostream& operator << (tm_ostream& out, qt_widget w);

/*! Casting form qt_widget to widget */
inline widget abstract (qt_widget w) { return widget (w.rep); }

/*! Casting from widget to qt_widget */
inline qt_widget concrete (widget w) {
  return qt_widget (static_cast<qt_widget_rep*> (w.rep));
}

inline void tm_delete (qt_widget_rep* ptr) {
  void *mem= ptr->derived_this ();
  ptr -> ~qt_widget_rep ();
  fast_delete (mem);
}

#endif // defined QT_WIDGET_HPP
