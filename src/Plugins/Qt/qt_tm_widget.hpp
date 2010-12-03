
/******************************************************************************
 * MODULE     : qt_tm_widget.hpp
 * DESCRIPTION: The main TeXmacs widget for the Qt GUI
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

class QLabel; class QToolBar;

/**
 * Main TeXmacs' widget. This is the interface between TeXmacs
 * and our QT implementation of this widget. The auxiliary function 
 * texmacs_widget() returns an instance of this class to be used within 
 * the rest of TeXmacs.
 */
class qt_tm_widget_rep: public qt_view_widget_rep {
public: 
  QLabel *rightLabel;
  QLabel *leftLabel;
  
  QToolBar *mainToolBar;
  QToolBar *modeToolBar;
  QToolBar *focusToolBar;
  QToolBar *userToolBar;
  

#ifdef Q_WS_MAC
  QToolBar *dumbToolBar;
  
  QAction *modeToolBarAction;
  QAction *mainToolBarAction;
  
  QWidget *rulerWidget;
#endif
  
  
  QWidget *centralWidget;
  
  QTMInteractiveInputHelper helper;
  
  qt_widget int_prompt;
  qt_widget int_input;
  
  bool visibility[6];
  bool full_screen;
  
  command quit;
  
  widget main_menu_widget;
  widget main_icons_widget;
  widget mode_icons_widget;
  widget focus_icons_widget;
  widget user_icons_widget;
  widget waiting_main_menu_widget;
  
public:
  qt_tm_widget_rep (int mask, command _quit);
  ~qt_tm_widget_rep ();
  
  virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget read (slot s, blackbox index);
  virtual void write (slot s, blackbox index, widget w);
  
  virtual widget plain_window_widget (string s);
  
  QMainWindow* tm_mainwindow () {
    return qobject_cast<QMainWindow*> (view); }
  QStackedWidget* tm_centralwidget () {
    return tm_mainwindow()->findChild<QStackedWidget*>("stacked widget"); }
  QTMScrollView* tm_scrollarea () {
    return qobject_cast<QTMScrollView*> (tm_centralwidget()->currentWidget()); }
  QTMWidget* tm_canvas () {
    return qobject_cast<QTMWidget*> (tm_scrollarea()); }
  
  void updateVisibility();
  void do_interactive_prompt ();
  void install_main_menu ();
  void set_full_screen (bool flag);
};


/**
 * List of widgets wanting to install their menu bar
 */
extern list<qt_tm_widget_rep*> waiting_widgets;

/**
 * Positive means the menu is busy.
 */
extern int menu_count;


#endif // QT_TM_WIDGET_HPP
