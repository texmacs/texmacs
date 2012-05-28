
/******************************************************************************
 * MODULE     : qt_tm_widget.hpp
 * DESCRIPTION: The main TeXmacs input widget and its embedded counterpart.
 * COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QT_TM_WIDGET_HPP
#define QT_TM_WIDGET_HPP

#include "list.hpp"

#include "qt_widget.hpp"
#include "qt_view_widget.hpp"

#include "QTMInteractiveInputHelper.hpp"
#include "QTMWidget.hpp"
#include "QTMScrollView.hpp"

#include <QMainWindow>
#include <QStackedWidget>
#include <QLayout>

class QLabel; 
class QToolBar;
class QTMInteractivePrompt;

/*! Models one main window with toolbars, an associated view, etc.
 
 The underlying QWidget is a QTMWindow, whose central widget is a 
 QStackedWidget holding the canvases for all the open buffers. Each canvas
 is of type QTMWidget and belongs to one qt_simple_widget_rep.
 */
class qt_tm_widget_rep: public qt_view_widget_rep {
public: 
  QLabel *rightLabel;
  QLabel *leftLabel;
  
  QToolBar *mainToolBar;
  QToolBar *modeToolBar;
  QToolBar *focusToolBar;
  QToolBar *userToolBar;
  QToolBar *sideToolBar;
  

#ifdef Q_WS_MAC
  QToolBar *dumbToolBar;
  
  QAction *modeToolBarAction;
  QAction *mainToolBarAction;
  
  QWidget *rulerWidget;
#endif
  
  QWidget *centralWidget;
  
  QTMInteractiveInputHelper helper;
  QTMInteractivePrompt *prompt;
  qt_widget int_prompt;
  qt_widget int_input;
  
  bool visibility[7];
  bool full_screen;
  
  command quit;
  
  widget main_menu_widget;
  widget main_icons_widget;
  widget mode_icons_widget;
  widget focus_icons_widget;
  widget user_icons_widget;
  widget side_tools_widget;
  widget waiting_main_menu_widget;
  
public:
  qt_tm_widget_rep (int mask, command _quit);
  ~qt_tm_widget_rep ();
  
  virtual widget plain_window_widget (string title, command quit);

  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget    read (slot s, blackbox index);
  virtual void     write (slot s, blackbox index, widget w);
    
  void set_full_screen (bool flag);
  void updateVisibility();
  void install_main_menu ();

    ////// Convenience methods to access our QWidgets
protected:
  QMainWindow* mainwindow () {
    return qobject_cast<QMainWindow*> (qwid); 
  }
  QStackedWidget* centralwidget () {
    return qwid->findChild<QStackedWidget*>("stacked widget");
  }
  QTMScrollView* scrollarea () {
    return qobject_cast<QTMScrollView*> (centralwidget()->currentWidget());
  }
  QTMWidget* canvas () {
    return qobject_cast<QTMWidget*> (scrollarea());
  }
};


//! List of widgets wanting to install their menu bar
extern list<qt_tm_widget_rep*> waiting_widgets;

//! Positive means the menu is busy.
extern int menu_count;


/*! A simple texmacs input widget.
 
 This is a stripped down version of qt_tm_widget_rep, whose underlying widget
 isn't a QTMWindow anymore, but a regular QWidget (right now a QStackedWidget)
 because it is intended to be embedded into a window.
 
 FIXME: the QStackedWidget is not needed for embedded texmacs widgets.
*/
class qt_tm_embedded_widget_rep: public qt_view_widget_rep {
public:
  command quit;
  
  qt_tm_embedded_widget_rep (command _quit);
  ~qt_tm_embedded_widget_rep ();

  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual void     write (slot s, blackbox index, widget w);
  
protected:
  QTMScrollView* scrollarea () {
    return qobject_cast<QTMScrollView*> (qwid);
  }
  QTMWidget* canvas () {
    return qobject_cast<QTMWidget*> (scrollarea());
  }
};

#endif // QT_TM_WIDGET_HPP
