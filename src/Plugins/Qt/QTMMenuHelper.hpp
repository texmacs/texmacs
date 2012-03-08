
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
#include <QLineEdit>

#include "qt_gui.hpp"
#include "qt_basic_widgets.hpp"


/*! Handles TeXmacs commands in the QT way.
 *
 * Most TeXmacs widgets accept one command_rep as an argument. This is a scheme
 * closure which will usually  be executed when the widget is closed, in the
 * case of dialogs, or activated in the case of checkboxes, combo boxes, etc.
 * This means connecting them to signals emmitted by our QWidgets and that's the
 * purpose of this wrapping class. Furthermore, this commands must be processed
 * in a separate queue. The slot apply() takes care of that.
 *
 * To use this class, one typically takes some given command "cmd" and does the
 * following:
 
    QTMCommand* qtmcmd = new QTMCommand(cmd);
    theQWidget->setParent(qtmcmd);
    QObject::connect(theQWidget, SIGNAL(somethingHappened()), qtmcmd, SLOT(apply()));
 
 * Since the slot in this class accepts no arguments, commands which require
 * access to the QWidget must be subclassed from command_rep to accept the
 * particular QWidget as a parameter. Then their invocation (which apply() will
 * call) must access it.
 *
 * An alternative would be to subclass QTMCommand to add slots accepting arguments
 * but making sure that the underlying command_rep is properly sent to the mentioned
 * queue.
 */
class QTMCommand: public QObject {
  Q_OBJECT
  command cmd;
        
public:
  inline QTMCommand (command _cmd):
    cmd (_cmd) {  }

public slots:
  void apply();
};


/*!
 *
 */
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

/*! This custom action frees its menu if it does not already have an owner. */
class QTMAction : public QAction {
  Q_OBJECT
  
public:
  string str;
  
  QTMAction(QObject *parent = NULL);
  ~QTMAction();
  
public slots:
  void doRefresh();
  
};

struct QLineEdit;
class QTMInputTextWidgetHelper : public QObject {
  Q_OBJECT

  widget p_wid; /*!< A reference to the tm widget, always a qt_input_text_widget_rep */

  bool done;
  
public:
  
  QList<QLineEdit*> views;

  QTMInputTextWidgetHelper ( qt_input_text_widget_rep*  _wid ) 
    : QObject(NULL), p_wid(abstract(_wid)), done(false) { }
  ~QTMInputTextWidgetHelper();

  qt_input_text_widget_rep* wid () 
    { return (qt_input_text_widget_rep*) p_wid.rep; }
  // useful cast
  
  void add (QLineEdit *);

public slots:
  void commit ();
  void leave ();
  void remove (QObject *);
  
  void doit ();
  
};

class QTMWidgetAction : public QWidgetAction {
  Q_OBJECT

  widget wid;
  
public:
  QTMWidgetAction(widget _wid, QObject *parent = NULL);
  ~QTMWidgetAction();
  
public slots:
  virtual void doRefresh();
  
protected:
  QWidget * createWidget ( QWidget * parent );
  
};



#endif // QTMMENUHELPER_HPP
