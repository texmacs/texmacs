
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

class QWidget;
class QAction;
class QMenu;
class QLayoutItem;

/*! The base class of all TeXmacs widgets in the QT interface.
 * 
 * Ever TeXmacs Widget is an entity which can be manipulated from within scheme.
 * The interface to this is provided by widget_rep, which we extend. The methods
 * here declared must be implemented by the QT wrappers. We explictly FAIL() in
 * case not, to clearly signal to ourselves that there's a problem.
 *
 * See \link src/Graphics/Gui/widget.hpp \endlink and the relevant sections of the
 * developer's guide.
 */
class qt_widget_rep : public widget_rep {
public:
  qt_widget_rep() : widget_rep () { };

  /*! Returns the widget as window.
   * 
   * Each TeXmacs widget can at some point be asked to present itself into a window.
   * The scheme-originating function window_create() expects this method in every
   * widget.
   * Widgets which are windows must declare themselves to be via the QProperty
   * "texmacs_window_widget". This will be checked when processing messages from scheme
   * asking to actually display the window. See qt_view_widget_rep::read() and
   * qt_window_widget_rep::qt_window_widget_rep().
   *
   *   \param s (Often?) a title for the window.
   *   \param q 
   *   \return A pointer to this widget.
   */
  virtual widget plain_window_widget (string s, command q);
  
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

/*
 * Automagically create definitions for the stuff declared inside qt_widget with
 * the macro ABSTRACT_NULL(). See src/Kernel/Abstractions/basic.hpp
 */

ABSTRACT_NULL_CODE(qt_widget);

/*! casting form qt_widget to widget */
inline widget abstract (qt_widget w) { return widget (w.rep); }

/*! casting from widget to qt_widget */
inline qt_widget concrete (widget w) { return qt_widget ((qt_widget_rep*) w.rep); }

#endif // defined QT_WIDGET_HPP
