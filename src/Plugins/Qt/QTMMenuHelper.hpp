
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
#include "scheme.hpp"

#include "qt_gui.hpp"
#include "qt_dialogues.hpp"

#include <QTimer>
#include <QPoint>
#include <QStringListModel>
#include <QSortFilterProxyModel>
#include <QMenu>
#include <QAction>
#include <QWidgetAction>
#include <QLineEdit>
#include <QComboBox>
#include <QListView>
#include <QTreeView>
#include <QScrollArea>
#include <QTabWidget>
#include <QToolButton>

/*! Handles TeXmacs commands in the QT way.

 Most TeXmacs widgets accept one command_rep as an argument. This is a scheme
 closure which will usually  be executed when the widget is closed, in the
 case of dialogs, or activated in the case of checkboxes, combo boxes, etc.
 This means connecting them to signals emmitted by our QWidgets and that's the
 purpose of this wrapping class. Furthermore, this commands must be processed
 in a separate queue. The slot apply() takes care of that.

 To use this class, one typically takes some given command "cmd" and does the
 following:
 
    QTMCommand* qtmcmd = new QTMCommand (theQWidget, cmd);
    QObject::connect(theQWidget, SIGNAL (somethingHappened()), qtmcmd, SLOT (apply()));
 
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
  void apply ();
};


/*! A QMenu which builds its entries just before show()ing.
 
 The menu entries are given as a texmacs widget in form of a promise which is
 evaluated each time we force(). No caching internal to this object should be
 performed lest we break dynamic menus.
 
 If this is intended as a submenu of a QAction, we must attachTo() it: then,
 when the action is about to be destroyed() we are notified and remove ourselves
 
 If this is used as a menu for a button, then it suffices to set the parent
 accordingly in the constructor.
 */
class QTMLazyMenu: public QMenu {
  Q_OBJECT
  
  promise<widget> promise_widget;
  bool show_right;

public:
  QTMLazyMenu (promise<widget> _pm, QWidget* p = NULL, bool right = false);
  void attachTo (QAction* a);
  virtual void showEvent (QShowEvent* event);

public slots:
  void force ();

protected:
  void transferActions (QList<QAction*>* src);

protected slots:
  void destroy (QObject* obj=0);
};


/*! The basic action for items in TeXmacs' menus.
 
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
  string     str;
  
public:
  QTMAction (QObject *parent = NULL);
  ~QTMAction();

  void set_text (string s);
  
public slots:
  void doRefresh();
  void showToolTip();   //<! Force the display of the tooltip (starts a timer)

protected slots:
  void doShowToolTip();  // Actually show it.
};


/*!
 */
class QTMWidgetAction : public QWidgetAction {
  Q_OBJECT
  
  widget wid;
  
public:
  QTMWidgetAction (widget _wid, QObject* parent=NULL);
  
public slots:
  void doRefresh() { };
  
protected:
  virtual QWidget* createWidget (QWidget* parent);
};


/*! QTMTileAction is used to build a popup menu with a grid of buttons.
 
 In particular this is used to build the matrix of buttons in the color palette
 available at the toolbar. The corresponding texmacs widget using this class
 is a tile_menu (see qt_ui_element.cpp)
 */
class QTMTileAction: public QWidgetAction {
  Q_OBJECT

  QVector <QAction*> actions;
  int                   cols;
  
public:
  QTMTileAction (array<widget>& arr, int _cols, QObject* parent=NULL);
  virtual QWidget* createWidget (QWidget* parent);
};


/*!
 */
class QTMMinibarAction : public QWidgetAction {
  Q_OBJECT

  QVector <QAction*> actions;

public:
  QTMMinibarAction (array<widget>& arr, QObject* parent=NULL);
  virtual QWidget* createWidget (QWidget* parent);
};


/*! QTMMenuButton is a custom button appropriate for menus.
 
 We need to subclass QToolButton for two reasons:
    1) Custom appearance
    2) If used in QWidgetAction the menu does not disappear upon triggering the
       button. See QTBUG-10427 and TeXmacs bug #37719.
 */
class QTMMenuButton: public QToolButton {
  Q_OBJECT

public:
  QTMMenuButton (QWidget* parent = NULL);
 
  void mouseReleaseEvent (QMouseEvent* e);
  void mousePressEvent (QMouseEvent* e);
  void paintEvent (QPaintEvent* e);
};


/*!
 */
class QTMMenuWidget: public QWidget {
  Q_OBJECT

public:
  QTMMenuWidget (QWidget* parent = NULL);
  void paintEvent(QPaintEvent *event);
};


/*! A customized QLineEdit with special keyboard handling and styling. */
class QTMLineEdit : public QLineEdit {
  Q_OBJECT

  bool completing;
  string     type; // type of input field
  string     name; // optional name of input field
  string   serial; // optional serial number of input field
  string       ww; // width of the parsed widget
  command     cmd;
  int    last_key;
public:
  QTMLineEdit (QWidget *parent, string _type, string _ww,
               int style=0, command _cmd= command ());
  void set_type (string t);
  bool continuous ();
  virtual QSize	sizeHint () const ;
  virtual bool event (QEvent* ev);

signals:
  void focusOut (Qt::FocusReason reason);
  
protected:
  virtual void keyPressEvent (QKeyEvent* ev);
  virtual void focusInEvent (QFocusEvent* ev);
  virtual void focusOutEvent (QFocusEvent* ev);
};


/*! A class to keep a QTMLineEdit and a qt_input_text_widget_rep in sync.
 
 After certain events we store state information about the QLineEdit into the 
 qt_input_text_widget_rep:
 
  - When the user has finished editing (i.e. has pressed enter) we save the text
    from the QWidget in the texmacs widget and set a "modified" flag.
  - When the user leaves the QWidget we restore the text from the texmacs widget
    and in case there was a modification we execute the scheme command.
 
 Additionally and depending on user configuration we may always store the text
 when leaving or apply the command when pressing enter.
 
 @note On memory management: the QTMInputTextWidgetHelper is owned by the
       QTMLineEdit it is helping with.
*/
class QTMInputTextWidgetHelper : public QObject {
  Q_OBJECT
  
  qt_widget  p_wid;            //!< A pointer to a qt_input_text_widget_rep
  bool       can_autocommit;
  
public:
  QTMInputTextWidgetHelper (qt_widget _wid, bool _cac);
  
protected:
  qt_input_text_widget_rep* wid () { // useful cast
    return static_cast<qt_input_text_widget_rep*> (p_wid.rep); }

public slots:
  void commit ();
  void leave (Qt::FocusReason reason);
};


/*! A class to keep a QComboBox and a qt_field_widget_rep in sync.
 
 After certain events we store state information about the QComboBox into the 
 qt_input_text_widget_rep: when the user has finished editing (i.e. has pressed
 enter), or has left the QComboBox for instance.
 
 @note On memory management: the QTMFieldWidgetHelper is owned by the
       QComboBox it is helping with.
*/
class QTMFieldWidgetHelper : public QObject {
  Q_OBJECT
  
  qt_widget wid;
  bool     done;
  
public:
  QTMFieldWidgetHelper (qt_widget _wid, QComboBox* parent);
  QTMFieldWidgetHelper (qt_widget _wid, QLineEdit* parent);

public slots:
  void commit (const QString& qst);
};


/*! A QTabWidget which resizes itself to the currently displayed page. */
class QTMTabWidget : public QTabWidget {
  Q_OBJECT

public:
  QTMTabWidget (QWidget* p = NULL);

public slots:
  void resizeOthers(int index);
};


/*! A container widget which redraws the widgets it owns. */
class QTMRefreshWidget : public QWidget {
  Q_OBJECT
  
  string strwid;
  string   kind;
  object curobj;
  widget    cur;
  qt_widget tmwid;
  QWidget*   qwid;
  hashmap<object,widget> cache;
  
public:
  QTMRefreshWidget (qt_widget _tmwid, string _strwid, string _kind);

  bool recompute (string what);
    //static void deleteLayout (QLayout*);

public slots:
  void doRefresh (string kind);
};


/*! A container widget which redraws the widgets it owns. */
class QTMRefreshableWidget : public QWidget {
  Q_OBJECT
  
  object     prom;
  string     kind;
  object   curobj;
  widget      cur;
  qt_widget tmwid;
  QWidget*   qwid;
  
public:
  QTMRefreshableWidget (qt_widget _tmwid, object _prom, string _kind);

  bool recompute (string what);
    //static void deleteLayout (QLayout*);

public slots:
  void doRefresh (string kind);
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


/*! A QListView with a sorting proxy model. */
class QTMListView : public QListView {
  Q_OBJECT

  friend class QTMScrollArea;

  QStringListModel*      stringModel;
  QSortFilterProxyModel* filterModel;

public:
  QTMListView (const command& cmd, const QStringList& vals, const QStringList&,
               bool multiple, bool scroll = false, bool filtered = false,
               QWidget* parent = NULL);
  
  QSortFilterProxyModel* filter() const { return filterModel; }
  bool isScrollable() const {
    return (verticalScrollBarPolicy() != Qt::ScrollBarAlwaysOff) &&
           (horizontalScrollBarPolicy() != Qt::ScrollBarAlwaysOff);
  }

signals:
  void selectionChanged (const QItemSelection& c);

protected slots:
  virtual void selectionChanged (const QItemSelection& c, const QItemSelection& p);
};


class QTMTreeModel;

/*! A simple wrapper around a QTreeView.
 
 This class keeps a pointer to the tree it's displaying, in order to keep it
 alive. It also instantiates (if necessary) the QTMTreeModel it needs to sync 
 with the data. This model is property of a qt_tree_observer, which will delete
 it when the tree is.
 */
class QTMTreeView : public QTreeView {
  Q_OBJECT

  tree      _t;
  command _cmd;
  
  QTMTreeView (const QTMTreeView&);
  QTMTreeView& operator= (const QTMTreeView&);
  
public:
  QTMTreeView (command cmd, tree data, const tree& roles, QWidget* parent=0);
  QTMTreeModel* tmModel() const;
  
protected:
  virtual void currentChanged (const QModelIndex& cur, const QModelIndex& pre);
  
private slots:
  void callOnChange (const QModelIndex& index, bool mouse=true);
};


/*! A QScrollArea which automatically scrolls to selected items in QListWidgets.
 
 This is needed because of our implementation of choice_widget which disables
 the default scrollbars in QListWidget. Instead we add an explicit QScrollArea
 which then has to be scrolled manually, for instance when the user navigates 
 with the cursor keys. To this end we use the slot scrollToSelection(), 
 connected to the signal currentIndexChanged() of each QListWidget contained in
 the QScrollArea.
 
 Furthermore, we must implement showEvent() because scrolling at the time of
 widget compilation in qt_ui_element_rep::as_qwidget() before the widget
 is shown results in an unsufficient scroll performed (by an amount roughly the
 size of the viewport).
 */
class QTMScrollArea : public QScrollArea {
  Q_OBJECT
  
  QList<QTMListView*> listViews;
  
  typedef QList<QTMListView*>::iterator ListViewsIterator;

public:
  QTMScrollArea (QWidget* p = NULL) : QScrollArea(p) { };
  void setWidgetAndConnect (QWidget* w);

protected:
  virtual void showEvent (QShowEvent* ev);

public slots:
  void scrollToSelection (const QItemSelection& selected);
};

#endif // QTMMENUHELPER_HPP
