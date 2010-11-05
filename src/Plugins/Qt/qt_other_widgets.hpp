
/******************************************************************************
* MODULE     : qt_other_widgets.hpp
* DESCRIPTION: some QT widgets class declarations
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QT_OTHER_WIDGETS_HPP
#define QT_OTHER_WIDGETS_HPP

#include "qt_widget.hpp"
#include "QTMInteractiveInputHelper.hpp"
#include "QTMWidget.hpp"

#include <QLabel>
#include <QMainWindow>
#include <QStackedWidget>
#include <QAbstractScrollArea>
#include <QToolBar>


class qt_tm_widget_rep: public qt_view_widget_rep {
public: 
  QLabel *rightLabel;
  QLabel *leftLabel;

  QToolBar *mainToolBar;
  QToolBar *contextToolBar;
  QToolBar *userToolBar;
        
  QTMInteractiveInputHelper helper;
        
  qt_widget int_prompt;
  qt_widget int_input;

  bool visibility[5];

  command quit;
  
  widget main_menu_widget, main_icons_widget, context_icons_widget, user_icons_widget;
  widget waiting_main_menu_widget;

public:
  qt_tm_widget_rep (int mask, command _quit);
  ~qt_tm_widget_rep ();
        
  virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget read (slot s, blackbox index);
  virtual void write (slot s, blackbox index, widget w);
  // virtual void notify (slot s, blackbox new_val);    
  // virtual void connect (slot s, widget w2, slot s2);
  // virtual void deconnect (slot s, widget w2, slot s2);
  virtual widget plain_window_widget (string s);
        
  QMainWindow* tm_mainwindow () {
    return qobject_cast<QMainWindow*> (view); }
  QStackedWidget* tm_centralwidget () {
    return qobject_cast<QStackedWidget*> (tm_mainwindow()->centralWidget()); }
  QTMScrollView* tm_scrollarea () {
    return qobject_cast<QTMScrollView*> (tm_centralwidget()->currentWidget()); }
  QTMWidget* tm_canvas () {
    return qobject_cast<QTMWidget*> (tm_scrollarea()); }


  // void layout();
  void updateVisibility();
  void do_interactive_prompt ();
  void install_main_menu ();
};

class qt_window_widget_rep: public widget_rep {
public:

  QWidget *wid;

  qt_window_widget_rep (QWidget* _wid);
  ~qt_window_widget_rep ();

  virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget read (slot s, blackbox index);
  virtual void write (slot s, blackbox index, widget w);
  virtual void notify (slot s, blackbox new_val);
  // virtual void connect (slot s, widget w2, slot s2);
  // virtual void deconnect (slot s, widget w2, slot s2);
};

#endif // defined QT_OTHER_WIDGETS_HPP
