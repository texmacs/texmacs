
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

/*!
 * The basic implementation of an "abstract widget representation class" within
 * the QT toolkit, this does the real display/update/event work for the widget.
 * Any other GUI implementations using other toolkits (like Cocoa) must 
 * implement an analogous child of widget_rep.
 * See @link src/Graphics/Gui/widget.hpp and the relevant sections of the
 * developer's guide.
 */
class qt_widget_rep : public widget_rep {
public:
  qt_widget_rep() : widget_rep () { };

  /*!
   * (At least) @link window_create() @endlink expects this method in widgets
   * which implement windows. 
   * @param s (Often?) a title for the window.
   * @return A pointer to this widget.
   */
  virtual widget plain_window_widget (string s, command q);
  
  /*!
   *
   */
  virtual widget make_popup_widget ();
  
  /*!
   *
   */
  virtual widget popup_window_widget (string s);

  // Qt semantics of abstract texmacs widgets
  
  virtual QAction* as_qaction ();
  
  /*!
   *
   */
  virtual QWidget* as_qwidget () { return NULL ; }
  
  /*!
   * This method must not give ownership of the menu to the caller, thus
   * allowing menu caching at the TeXmacs level.
   * Called only by code which attaches root menus in the GUI elements.
   */
  virtual QMenu *get_qmenu() { return NULL; }

  
  /*!
   * Get a texmacs canvas (if available). 
   */
  virtual QWidget* get_canvas () { return NULL ; }
  
  /*!
   *
   */
  virtual QLayoutItem *as_qlayoutitem () { return NULL; }
  
};


/**
 * Wrapper around the qt_widget_rep widget representation class. It implements
 * reference counting. Please src/Kernel/Abstractions/basic.hpp
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

/**
 * casting form qt_widget to widget
 */

inline widget abstract (qt_widget w) { return widget (w.rep); }

/**
 * casting from widget to qt_widget
 */

inline qt_widget concrete (widget w) { return qt_widget ((qt_widget_rep*) w.rep); }

#endif // defined QT_WIDGET_HPP
