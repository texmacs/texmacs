
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
#include <QTabWidget>
#include <QLayout>
#include <QLineEdit>
#include <QComboBox>
#include <QTranslator>

#include "qt_gui.hpp"
#include "qt_dialogues.hpp"
#include "qt_ui_element.hpp"  // qt_refresh_widget_rep



/*! Handles TeXmacs commands in the QT way.

 Most TeXmacs widgets accept one command_rep as an argument. This is a scheme
 closure which will usually  be executed when the widget is closed, in the
 case of dialogs, or activated in the case of checkboxes, combo boxes, etc.
 This means connecting them to signals emmitted by our QWidgets and that's the
 purpose of this wrapping class. Furthermore, this commands must be processed
 in a separate queue. The slot apply() takes care of that.

 To use this class, one typically takes some given command "cmd" and does the
 following:
 
    QTMCommand* qtmcmd = new QTMCommand(theQWidget, cmd);
    QObject::connect(theQWidget, SIGNAL(somethingHappened()), qtmcmd, SLOT(apply()));
 
 Since the slot in this class accepts no arguments, commands which require
 access to the QWidget must be subclassed from command_rep to accept the
 particular QWidget as a parameter. Then their invocation (which apply() will
 call) must access it.

 An alternative would be to subclass QTMCommand to add slots accepting arguments
 but making sure that the underlying command_rep is properly sent to the mentioned
 queue.
*/
class QTMCommand: public QObject {
  Q_OBJECT

protected:
  command cmd;

public:
  QTMCommand (QObject* parent, command _cmd): QObject(parent), cmd (_cmd) {}

public slots:
  void apply();
};

/*! HACK: remove me!
 
 This special QTMCommand applies its underlying texmacs command immediately
 and upon destruction. This is needed to circumvent some strange behaviour
 where children QObjects are destroyed before the destroyed() signal is
 emmitted. 
 
 In particular, only qt_ui_widget_rep needs this for the wrapped_widget because
 it wants to connect the command to the destroyed() signal and the QTMCommand
 has already been deleted, the signal is disconnected and apply() never called.
 A nasty hack in the destructor applies the command anyway...
*/
class QTMOnDestroyCommand: public QTMCommand {
  Q_OBJECT

public:
  QTMOnDestroyCommand (QObject* parent, command _cmd): QTMCommand(parent, _cmd) {}
  ~QTMOnDestroyCommand () { apply (); }
public slots:
  void apply() {
    if (DEBUG_QT) 
      cout << "QTMOnDestroyCommand::apply()\n";
    // Immediately apply!!
    if (! is_nil(cmd)) cmd ();
  }
};


/*!
 *
 */
class QTMLazyMenu: public QMenu {
  Q_OBJECT
  promise<widget> pm;

public:
  QTMLazyMenu (promise<widget> _pm) : pm (_pm) {
      QObject::connect (this, SIGNAL (aboutToShow ()), this, SLOT (force ()));
  }

public slots:
  void force();
};


/*! The basic action for items in TeXmacs' menubars.
 
 This custom action frees its menu if it does not already have an owner: this is
 part of the memory policy explained in qt_menu_rep.

 A timer is used because the toolTips are not shown for items in the
 menu bar, since no mouse related events are ever sent to QActions placed
 there under MacOSX. We must use the signal hover() and add a timer to avoid
 instantly displaying the tooltip. This implies however ugly tradeoffs,
 see doShowToolTip().
 */
class QTMAction : public QAction {
  Q_OBJECT
  
  QTimer* _timer;
  QPoint    _pos;
public:
  string str;
  
  QTMAction(QObject *parent = NULL);
  ~QTMAction();

public slots:
  void doRefresh();
  void showToolTip();   //<! Force the display of the tooltip (starts a timer)

protected slots:
  void doShowToolTip();  // Actually show it.
};


/*! A customized QLineEdit with special keyboard handling and styling. */
class QTMLineEdit : public QLineEdit {
  Q_OBJECT

  bool completing;
public:
  string ww; // width of the parsed widget
  
  QTMLineEdit (QWidget *parent, string _ww, int style=0);
  virtual QSize	sizeHint () const ;
  
  bool event (QEvent* ev); 
  
protected:
  void keyPressEvent (QKeyEvent* ev);
  void  focusInEvent (QFocusEvent* ev);
};


/*! A class to keep a QLineEdit object and a qt_input_text_widget_rep object in
 sync.
 
 After certain events we store state information about the QLineEdit into the 
 qt_input_text_widget_rep: when the user has finished editing (i.e. has pressed
 enter), or has left the QLineEdit for instance.
 
 Actually we use this with a QTMLineEdit.
 */
class QTMInputTextWidgetHelper : public QObject {
  Q_OBJECT

  widget p_wid; /*!< A reference to the tm widget, always a qt_input_text_widget_rep */

  bool done;
  
public:
  
  QList<QLineEdit*> views;

  QTMInputTextWidgetHelper (qt_input_text_widget_rep*  _wid); 
  ~QTMInputTextWidgetHelper();

  qt_input_text_widget_rep* wid () { 
    return static_cast<qt_input_text_widget_rep*>(p_wid.rep); }
  // useful cast
  
  void add (QLineEdit *);

public slots:
  void commit ();
  void leave ();
  void remove (QObject *);

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


/*! A QTabWidget which resizes itself to the currently displayed page. */
class QTMTabWidget : public QTabWidget {
  Q_OBJECT
public:
  QTMTabWidget(QWidget* p=NULL);
public slots:
  void resizeOthers(int index);
};


/*! A container widget which redraws the widgets it owns. */
class QTMRefreshWidget : public QWidget {
  Q_OBJECT
  
  string tmwid;
  object curobj;
  widget cur;
  hashmap<object,widget> cache;
  
public:
  QTMRefreshWidget (string _tmwid);
  
  bool recompute ();
  
  public slots:
  void doRefresh ();  
};


/*! A mutilated QComboBox which fixes its size using texmacs lengths.
 
 To use just create the QWidget and call addItemsAndResize().
 */
class QTMComboBox : public QComboBox {
  Q_OBJECT
  
  QSize  calcSize;
  QSize   minSize;
public:
  QTMComboBox (QWidget* parent);
  
  void addItemsAndResize (const QStringList& texts, string ww, string h);
  bool event (QEvent* ev);

};


#endif // QTMMENUHELPER_HPP
