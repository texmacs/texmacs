
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
class QAction;
class QMenu;
class QLayoutItem;

/*! The base class of all TeXmacs widgets in the QT interface.
 * 
 * Ever TeXmacs Widget is an entity which can be manipulated from within scheme.
 * The interface to this is provided by widget_rep, which we extend. The methods
 * here declared must be implemented by the QT wrappers.
 *
 * Additionally, we provide several methods to cope with the fact that TeXmacs
 * expects widgets to behave in three different ways: as embedded widgets,
 * as menus and as regular widgets, all of which are essentially different in QT.
 * Hence the need to construct the QT widgets differently on a request basis via
 * the four methods as_qaction(), get_qmenu(), as_qlayoutmenu() and as_qwidget()
 * Reimplementations of these should cope with the basic differences these
 * concepts have in QT.
 *
 * Most of the UI items are implemented by qt_ui_element_rep, with some
 * exceptions.
 *
 * See \link src/Graphics/Gui/widget.hpp \endlink and the relevant sections of the
 * developer's guide as well.
 */
class qt_widget_rep : public widget_rep {
public:
  qt_widget_rep() : widget_rep () { };

  /*! Returns the widget as window.
   * 
   * Each TeXmacs widget can at some point be asked to present itself into a window.
   * The scheme-originating function window_create() expects this method in every
   * widget.
   * Widgets which are windows must declare themselves to be so via the QProperty
   * "texmacs_window_widget". This will be checked when processing messages from 
   * scheme asking to actually display the window. See qt_view_widget_rep::read()
   * and qt_window_widget_rep::qt_window_widget_rep().
   *
   *   \param title (Often?) a title for the window.
   *   \param quit Scheme closure to be executed upon close. 
   *   \return A pointer to this widget.
   */
  virtual widget plain_window_widget (string title, command quit);
  
  /*!  */
  virtual widget make_popup_widget ();
  
  /*!  */
  virtual widget popup_window_widget (string s);

  
  ////////////////////// Qt semantics of abstract texmacs widgets  
  
  virtual QAction* as_qaction ();
  
  /*! Returns the actual QWidget wrapped by this qt_widget_rep.
   * This is used to connect to signals, to change properties of the QWidget, etc.
   */
  virtual QWidget* as_qwidget () { return NULL; }
  
  /*!
   *
   * This method must not give ownership of the menu to the caller, thus
   * allowing menu caching at the TeXmacs level.
   * Called only by code which attaches root menus in the GUI elements.
   */
  virtual QMenu *get_qmenu() { return NULL; }

  
  /*!
   * Get a texmacs canvas (if available). 
   */
  virtual QWidget* get_canvas () { return NULL; }
  
  /*! Returns a QLayoutItem...
   */
  virtual QLayoutItem *as_qlayoutitem () { return NULL; }
  
  
  ////////////////////// Handling of TeXmacs' messages
  
  virtual void      send (slot s, blackbox val) {
    if (DEBUG_QT)
      cout << "qt_widget_rep::send(), unhandled slot: " << slot_name (s) << LF;
  }
  //virtual blackbox query (slot s, int type_id);
  //virtual widget    read (slot s, blackbox index);
  //virtual void     write (slot s, blackbox index, widget w);
  //virtual void    notify (slot s, blackbox new_val);
};


/*! Reference counting mechanism.
 *
 * Like elsewhere in TeXmacs, this is a wrapper around its corresponding qt_widget_rep 
 * which implements reference counting. Please src/Kernel/Abstractions/basic.hpp
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

/*! casting form qt_widget to widget */
inline widget abstract (qt_widget w) { return widget (w.rep); }

/*! casting from widget to qt_widget */
inline qt_widget concrete (widget w) { return qt_widget ((qt_widget_rep*) w.rep); }

#endif // defined QT_WIDGET_HPP
