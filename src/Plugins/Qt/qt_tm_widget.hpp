
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
#include "qt_simple_widget.hpp"
#include "qt_window_widget.hpp"

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
 
 The underlying QWidget is a QTMWindow, whose central widget is a QWidget
 holding the extra toolbars and the canvas for the open buffer. Each canvas
 is of type QTMWidget and belongs to one qt_simple_widget_rep.
 */
class qt_tm_widget_rep: public qt_window_widget_rep {

  /*
   enum { 
   header_visibility        =  1, // all toolbars
   main_toolbar_visibility  =  2, 
   mode_toolbar_visibility  =  4,
   focus_toolbar_visibility =  8,
   user_toolbar_visibility  = 16,
   footer_visibility        = 32,
   side_tools_0_visibility  = 64,
   bottom_tools_visibility  = 128
   } visibility_t;
   */
  QLabel*       rightLabel;
  QLabel*        leftLabel;
  QToolBar*    mainToolBar;
  QToolBar*    modeToolBar;
  QToolBar*   focusToolBar;
  QToolBar*    userToolBar;
  QDockWidget*   sideTools;
  QDockWidget* bottomTools;

#ifdef Q_OS_MAC
  QToolBar*      dumbToolBar;
  QAction* modeToolBarAction;
  QAction* mainToolBarAction;
  QWidget*       rulerWidget;
#endif

  QTMInteractiveInputHelper helper;
  QTMInteractivePrompt*     prompt;
  qt_widget int_prompt;
  qt_widget int_input;
  
  bool visibility[8];
  bool full_screen;
  
  qt_widget main_widget;
  qt_widget main_menu_widget;
  qt_widget waiting_main_menu_widget;
  qt_widget main_icons_widget;
  qt_widget mode_icons_widget;
  qt_widget focus_icons_widget;
  qt_widget user_icons_widget;
  qt_widget side_tools_widget;
  qt_widget bottom_tools_widget;
  qt_widget dock_window_widget;   // trick to return correct widget position

  
public:
  qt_tm_widget_rep (int mask, command _quit);
  ~qt_tm_widget_rep ();
  
  virtual widget plain_window_widget (string name, command quit);

  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget    read (slot s, blackbox index);
  virtual void     write (slot s, blackbox index, widget w);
    
  void set_full_screen (bool flag);
  void update_visibility();
  void install_main_menu ();
  static void tweak_iconbar_size (QSize& sz);

  friend class QTMInteractiveInputHelper;
  
protected:
  
      ////// Convenience methods to access our QWidgets
  
  QMainWindow* mainwindow () {
    return qobject_cast<QMainWindow*> (qwid); 
  }
  QWidget* centralwidget () {
    return mainwindow()->centralWidget();
  }
  QTMScrollView* scrollarea () {
    return qobject_cast<QTMScrollView*> (main_widget->qwid);
  }
  QTMWidget* canvas () {
    return qobject_cast<QTMWidget*> (main_widget->qwid);
  }
};


//! List of widgets wanting to install their menu bar
extern list<qt_tm_widget_rep*> waiting_widgets;

//! Positive means the menu is busy.
extern int menu_count;


/*! A simple texmacs input widget.
 
 This is a stripped down version of qt_tm_widget_rep, whose underlying widget
 isn't a QTMWindow anymore, but a regular QTMWidget because it is intended to be
 embedded somewhere else.

*/
class qt_tm_embedded_widget_rep: public qt_widget_rep {
  widget main_widget;

public:
  command quit;
  
  qt_tm_embedded_widget_rep (command _quit);

  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget    read (slot s, blackbox index);
  virtual void     write (slot s, blackbox index, widget w);
  
  virtual QWidget*         as_qwidget ();
  virtual QLayoutItem* as_qlayoutitem ();
};

#endif // QT_TM_WIDGET_HPP
