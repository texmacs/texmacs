
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

#include "widget.hpp"
#include "message.hpp"

class QWidget;
class QLayoutItem;
class QAction;
class QMenu;

/*! The base class of all TeXmacs widgets in the QT interface.

 Every TeXmacs Widget is an entity which can be manipulated from within scheme.
 The interface to this is provided by widget_rep, which we extend. The methods
 here declared must be implemented by the QT wrappers.

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
 
 Most of the UI items are implemented by qt_ui_element_rep, with some
 exceptions. Creation from the TeXmacs side is done using the global (yuk!)
 functions declared in Graphics/Gui/widget.hpp.
 
 The underlying QWidget is NOT owned by the qt_widget, because several
 qt_widgets may have the same underlying QWidget, so we cannot delete it here.
 We leave the responsibility of that to the outmost widget, which will usually
 be a qt_window_widget and whose QWidget, typically a QTMPlainWindow or a
 QTMWindow, should be the parent of all the related QWidgets.
 */
class qt_widget_rep : public widget_rep {
public:
  long id;
  QWidget* qwid;
  
  /*! A list of all supported widget types.
   FIXME: This enum breaks the basic inheritance rules, since we have to 
   update the base class each time we implement a new subclass. It's also some
   sort of bastardic and lame RTTI, which might be proof of bad design...
   But it comes handy in a few places right now ;)
   */
  enum types {
    none = 0,
    input_widget,       file_chooser,      window_widget,   view_widget,
    horizontal_menu,    vertical_menu,     horizontal_list, vertical_list,
    tile_menu,          minibar_menu,      menu_separator,  menu_group, 
    pulldown_button,    pullright_button,  menu_button,     balloon_widget,
    text_widget,        xpm_widget,        toggle_widget,   enum_widget,
    choice_widget,      scrollable_widget, hsplit_widget,   vsplit_widget,
    aligned_widget,     tabs_widget,       wrapped_widget,  refresh_widget,
    glue_widget,        resize_widget,     texmacs_widget,  simple_widget,
    embedded_tm_widget, popup_widget
  } ;
  
  types type;
  
  qt_widget_rep(types _type=none, QWidget* _qwid=0);
  virtual ~qt_widget_rep ();
  
  virtual widget plain_window_widget (string title, command quit);
  virtual widget make_popup_widget ();
  virtual widget popup_window_widget (string s);

  
  ////////////////////// Qt semantics of abstract texmacs widgets  
  
  virtual QAction*         as_qaction ();
  virtual QWidget*         as_qwidget ();
  virtual QLayoutItem* as_qlayoutitem ();
  virtual QMenu*            get_qmenu ();
  

  ////////////////////// Debugging
  string type_as_string() { 
    static const char* qt_widget_type_strings[] = {
      "none",
      "input_widget",    "file_chooser",      "window_widget",   "view_widget",
      "horizontal_menu", "vertical_menu",     "horizontal_list", "vertical_list",
      "tile_menu",       "minibar_menu",      "menu_separator",  "menu_group", 
      "pulldown_button", "pullright_button",  "menu_button",     "balloon_widget",
      "text_widget",     "xpm_widget",        "toggle_widget",   "enum_widget",
      "choice_widget",   "scrollable_widget", "hsplit_widget",   "vsplit_widget", 
      "aligned_widget",  "tabs_widget",       "wrapped_widget",  "refresh_widget",
      "glue_widget",     "resize_widget",     "texmacs_widget",  "simple_widget",
      "embedded_tm_widget", "popup_widget"
    };
    return string(qt_widget_type_strings[type]) * "\t id: " * as_string(id);
  }
  
  ////////////////////// Handling of TeXmacs' messages
	
	/// This might be just temporary, in order to see which slots are unhandled
  
  virtual void send (slot s, blackbox val) {
    (void) val;
    if (DEBUG_QT)
      cout << "qt_widget_rep::send(), unhandled " << slot_name (s) 
           << " for widget of type: " << type_as_string() << LF;
  }
  
  virtual blackbox query (slot s, int type_id) {
		(void) type_id;
    if (DEBUG_QT)
      cout << "qt_widget_rep::write(), unhandled " << slot_name (s) 
           << " for widget of type: " << type_as_string() << LF;
		return blackbox ();
	}
  
  virtual widget read (slot s, blackbox index) {
		(void) index;
    if (DEBUG_QT)
      cout << "qt_widget_rep::read(), unhandled " << slot_name (s) 
           << " for widget of type: " << type_as_string() << LF;
		return widget ();
	}	
  
  virtual void write (slot s, blackbox index, widget w) {
		(void) index; (void) w;
    if (DEBUG_QT)
      cout << "qt_widget_rep::write(), unhandled " << slot_name (s) 
           << " for widget of type: " << type_as_string() << LF;
	}	
  
  virtual void notify (slot s, blackbox new_val) {
		(void) new_val;
    if (DEBUG_QT)
      cout << "qt_widget_rep::notify(), unhandled " << slot_name (s) 
           << " for widget of type: " << type_as_string() << LF;
	}
};


/*! Reference counting mechanism.

 Like elsewhere in TeXmacs, this is a wrapper around its corresponding qt_widget_rep 
 which implements reference counting. Please src/Kernel/Abstractions/basic.hpp
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


/*! casting form qt_widget to widget */
inline widget abstract (qt_widget w) { return widget (w.rep); }

/*! casting from widget to qt_widget */
inline qt_widget concrete (widget w) { return qt_widget (static_cast<qt_widget_rep*>(w.rep)); }

#endif // defined QT_WIDGET_HPP
