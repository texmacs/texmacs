
/******************************************************************************
* MODULE     : QTMMenuHelper.hpp
* DESCRIPTION: QT Texmacs menu helper classes
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMMENUHELPER_HPP
#define QTMMENUHELPER_HPP

#include "message.hpp"
#include "promise.hpp"
#include <QObject>
#include <QAction>
#include <QMenu>
#include <QWidgetAction>

#include "qt_gui.hpp"
#include "qt_basic_widgets.hpp"

class QTMCommand: public QObject {
  Q_OBJECT
  command cmd;
        
public:
  inline QTMCommand (command _cmd):
    cmd (_cmd) {  }

public slots:
  void apply();
};


class QTMKeyCommand: public QObject {
  Q_OBJECT
  string ks;
  
public:
  inline QTMKeyCommand (string _ks):
  ks (_ks) {  }
  
public slots:
  void apply();
};

class QTMLazyMenu: public QMenu {
  Q_OBJECT
  promise<widget> pm;

public:
  inline QTMLazyMenu (promise<widget> _pm):
    pm (_pm) {
      QObject::connect (this, SIGNAL (aboutToShow ()), this, SLOT (force ()));
    }

public slots:
  void force();
};

// this custom action frees its menu if it does not already have an owner.
class QTMAction : public QAction {
  Q_OBJECT
  
public:
  string str;
  
  QTMAction(QObject *parent = NULL);
  ~QTMAction();
  
public slots:
  void doRefresh();
  
};


class QTMInputTextWidgetHelper : public QObject {
  Q_OBJECT

  widget p_wid; 
  // we keep reference to the texmacs widget
  // which is always a qt_input_text_widget_rep
  
public:
  
  QList<QLineEdit*> views;

  QTMInputTextWidgetHelper ( qt_input_text_widget_rep*  _wid ) 
    : QObject(NULL), p_wid(abstract(_wid)) { }
  ~QTMInputTextWidgetHelper();

  qt_input_text_widget_rep* wid () 
    { return (qt_input_text_widget_rep*) p_wid.rep; }
  // useful cast
  
  void add (QLineEdit *);

public slots:
  void commit ();
  void leave ();
  void remove (QObject *);
  
};

class QTMWidgetAction : public QWidgetAction {
  Q_OBJECT
  
public:
  string str;
  QTMInputTextWidgetHelper *helper;
  
  QTMWidgetAction(QObject *parent = NULL);
  ~QTMWidgetAction();
  
  public slots:
  void doRefresh();
  
protected:
  QWidget * createWidget ( QWidget * parent );
  
};



#endif // QTMMENUHELPER_HPP
