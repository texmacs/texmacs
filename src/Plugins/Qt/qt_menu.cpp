
/******************************************************************************
* MODULE     : qt_menu.cpp
* DESCRIPTION: QT menu proxies
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "qt_menu.hpp"
#include "qt_utilities.hpp"
#include "qt_renderer.hpp"
#include "qt_simple_widget.hpp"
#include "qt_basic_widgets.hpp"
#include "QTMMenuHelper.hpp"
#include "QTMStyle.hpp"
#include "analyze.hpp"
#include "widget.hpp"
#include "message.hpp"
#include "promise.hpp"
//#import "TMView.h"
#include <QtGui>


#include "QTMGuiHelper.hpp"
#include "qt_gui.hpp"

// REMARK on memory management.
// the hierarchy of a QMenu has parents correctly set to the proper supermenu
// this guarantees that deletion of the root menu correclty deletes all the tree below it.
// the root menu itself (without parent QObject) is "owned" by the associate qt_menu_rep instance
// and it is deallocated by it. This ensures correct memory management between TeXmacs and Qt
// since qt_menu_rep is sometimes cached at TeXmacs level.
// this also means that when we install some menu in the GUI (in the main menu or in the toolbar)
// we should just add actions and not reroot the qt parent hierarchy since even if the menu will
// be eventually removed from the GUI it has some chance to still be cached in the TeXmacs side
// conventions are as follows:
// - the method as_qaction passes the ownership of the action and the eventual submenu to the caller 
//   responsibility. When creating menu hierachies (eg. via the scheme interface) you should use this method
//   to retrieve the relevant Qt objects.
// - the method qt_menu_rep::get_menu preserves the onwership of the menu to the called qt_menu_rep
//   (to guarantee correct caching). when installing menus in the gui you should use this method.
// Submenus belongs implicitly to the parent QTMAction since it controls if the menu has an explicit parent
// and in negative case delete the submenu. All submenus in the menu hierarchy should have an empty parent widget
// and be attached to some QTMAction. This guarantees correct memory management.



QTMAction::QTMAction(QObject *parent) : QAction(parent) { 
  QObject::connect(the_gui->gui_helper, SIGNAL(refresh()), this, SLOT(doRefresh()));
}

QTMAction::~QTMAction() { 
  QObject::disconnect(the_gui->gui_helper, 0, this, 0);
  if (menu() && !(menu()->parent())) delete menu(); 
}


void 
QTMAction::doRefresh() {
  if (N(str)) {
    string t= tm_var_encode (str);
    if (t == "Help") t= "Help ";
    setText(to_qstring (t));
  }
}


void 
QTMCommand::apply()  {
  if (!is_nil(cmd)) { the_gui->process_command(cmd); }
}


void 
QTMKeyCommand::apply()  {
  if (N(ks)) { 
    QTMWidget *w = qobject_cast<QTMWidget*>(qApp->focusWidget());
    if (w && w->tm_widget()) {
      if (DEBUG_QT)
        cout << "shortcut: " << ks << LF;
      the_gui -> process_keypress (w->tm_widget(), ks, texmacs_time());
    }
  }
}


/*******************************************************************************
* Default action is empty.
*******************************************************************************/

QAction *qt_widget_rep::as_qaction() {
  QAction *a = new QTMAction (NULL); 
  //  a->setSeparator(true);
  a->setEnabled(false);
  return a;
};


/******************************************************************************/

class qt_menu_rep: public qt_widget_rep {
public:
  QAction  *item;
  qt_menu_rep (QAction* _item) : item (_item ? _item : new QTMAction (NULL)) {  }
  ~qt_menu_rep () { 
    delete item; // the submenu is usually also deleted since item is a QTMAction
  }

  virtual QMenu *get_qmenu() { return (item ? item->menu() : NULL); }
  // get_menu doest not give ownership of the menu to the caller
  // this allow menu caching at the TeXmacs level
  // get_qmenu is called only by code which attach root menus in the GUI elements
  
  virtual void send (slot s, blackbox val);
  virtual widget make_popup_widget ();
  virtual widget popup_window_widget (string s);
  virtual widget plain_window_widget (string s);
  virtual QAction* as_qaction ();
};

QAction*
qt_menu_rep::as_qaction() {
  // FIXME: the convention is that as_qaction give ownership of
  // the action to the caller. However in this case we do not want
  // to replicate the action so we must be sure to be called only once.
  if (!item) cout << "THIS MUST NOT HAPPEN TWICE" << LF;
  QAction *ret = item;
  item = NULL;
  return ret;
}

widget
qt_menu_rep::make_popup_widget () {
  return this;
}

widget
qt_menu_rep::popup_window_widget (string s) {
  item->menu()->setWindowTitle (to_qstring (s));
  return this;
}

widget
qt_menu_rep::plain_window_widget (string s) {
  item->menu()->setWindowTitle (to_qstring (s));
  return this;
}

void
qt_menu_rep::send (slot s, blackbox val) {
  if (DEBUG_QT)
    cout << "qt_menu_rep::send " << slot_name(s) << LF;
  switch (s) {
  case SLOT_POSITION:
    ASSERT (type_box (val) == type_helper<coord2>::id, "type mismatch");
    break;
  case SLOT_VISIBILITY:
    {   
      check_type<bool> (val, "SLOT_VISIBILITY");
      bool flag = open_box<bool> (val);
      if (flag)
        item->menu()->show();
      (void) flag;
    }   
    break;
  case SLOT_MOUSE_GRAB:
    {   
      check_type<bool> (val, "SLOT_MOUSE_GRAB");
      bool flag = open_box<bool> (val);
      (void) flag;
      // [NSMenu popUpContextMenu:[item submenu] withEvent:[NSApp currentEvent] forView:( (qt_view_widget_rep*)(the_keyboard_focus.rep))->view ];
      if (item->menu ())
        item->menu()->exec (QCursor::pos ());
    }   
    break;
  default:
    FAILED ("cannot handle slot type");
  }
}



/******************************************************************************
 * Auxiliary classes
 ******************************************************************************/

// we use this class to properly initialize style options
// for our QWidgets which have to blend into QMenus
// see #QTBUG-1993.
// see #QTBUG-7707.

class QTMAuxMenu: public QMenu {
public:
  QTMAuxMenu (): QMenu() {}
  
  void myInitStyleOption (QStyleOptionMenuItem *option) const {
    QAction action (NULL);
    initStyleOption(option,&action);
  }
};


// we need to subclass QToolButton for two reasons
// 1) custom appearence
// 2) if used in QWidgetAction the menu do not disappear upon triggering the
//    button. See QTBUG-10427.

// QTMMenuButton is a custom button appropriate for menus

class QTMMenuButton: public QToolButton {
  QStyleOptionMenuItem option;
public:
  QTMMenuButton (QWidget* parent = 0): QToolButton(parent) {
    QTMAuxMenu m;
    m.myInitStyleOption (&option);
    setAttribute (Qt::WA_Hover);
  }  
  void mouseReleaseEvent (QMouseEvent *event);
  void mousePressEvent (QMouseEvent *event);
  void paintEvent (QPaintEvent *event);
};

void
QTMMenuButton::mousePressEvent (QMouseEvent* event) {
  // this one triggers the action and toggles the button
  QToolButton::mousePressEvent (event);
  // this one forwards the event to the parent
  // (which eventually is the menu)
  QWidget::mousePressEvent (event);
}

void
QTMMenuButton::mouseReleaseEvent (QMouseEvent* event) {
  // this one triggers the action and untoggles the button
  QToolButton::mouseReleaseEvent (event);
  // this one forwards the event to the parent
  // (which eventually is the menu which then close itself)
  QWidget::mouseReleaseEvent (event);
}

void
QTMMenuButton::paintEvent (QPaintEvent* event) {
  (void) event;
  QPainter p (this);
  
  // initialize the options
  QStyleOptionToolButton buttonOpt;
  initStyleOption (&buttonOpt);
  QRect r = rect ();
  option.rect = r;
  option.state = QStyle::State_Enabled |
    ( buttonOpt.state & QStyle::State_MouseOver ? 
      QStyle::State_Selected : QStyle::State_None ); 
  // draw the control background as a menu item
  style () -> drawControl (QStyle::CE_MenuItem, &option, &p, this); 
  // draw the icon with a bit of inset.
  r.adjust (2,2,-2,-2);
  defaultAction ()-> icon ().paint (&p, r);
}


class QTMMenuWidget: public QWidget {
  QStyleOptionMenuItem option;
public:
  QTMMenuWidget (QWidget* parent = 0): QWidget(parent) {
    QTMAuxMenu m;
    m.myInitStyleOption (&option);
  }
  void paintEvent(QPaintEvent *event);
};

void
QTMMenuWidget::paintEvent(QPaintEvent* event) {
  (void) event;
  QPainter p (this);
  option.rect = rect ();
  //QRect r = rect ();
  style()->drawControl (QStyle::CE_MenuEmptyArea, &option, &p, this);
  QWidget::paintEvent(event);
}

class QTMTileAction: public QWidgetAction {
  QVector <QAction*> actions;
  int cols;
public:
  QTMTileAction (QWidget* parent, array<widget>& arr, int _cols)
  : QWidgetAction (parent), cols (_cols)
  {
    actions.reserve(N(arr));
    for(int i = 0; i < N(arr); i++) {
      if (is_nil(arr[i])) break;
      QAction *act = concrete(arr[i])->as_qaction();
      act->setParent(this);
      actions.append(act);
    };
  }
  QWidget* createWidget(QWidget* parent);
  // virtual void activate (ActionEvent event) {
  //   cout << "TRIG\n"; QWidgetAction::activate (event); }
  void paintEvent(QPaintEvent *event);
};


// FIXME: QTMTileAction::createWidget is called twice:
// the first time when the action is added to the menu,
// the second when from the menu it is transferred to the toolbar.
// This is weird since the first widget does not ever use
// the widget so it results in a waste of time.

QWidget*
QTMTileAction::createWidget(QWidget* parent) {
  if (DEBUG_QT) 
    cout << "QTMTileAction::createWidget\n";
  QWidget* wid= new QTMMenuWidget (parent);
  QGridLayout* l= new QGridLayout (wid);
 // wid->setAutoFillBackground(true);
 // wid->setBackgroundRole(QPalette::Base);
  wid->setLayout (l);
  l->setSizeConstraint (QLayout::SetFixedSize);
  l->setHorizontalSpacing (2);
  l->setVerticalSpacing (2);
  l->setContentsMargins (4, 0, 4, 0);
  int row= 0, col= 0;
  for (int i=0; i < actions.count(); i++) {
    QAction* sa= actions[i];
    QToolButton* tb= new QTMMenuButton (wid);
    tb->setDefaultAction (sa);
    QObject::connect(tb, SIGNAL(released()), this, SLOT(trigger()));
    //  tb->setStyle (qtmstyle ());
    l->addWidget (tb, row, col);
    col++;
    if (col >= cols) { col = 0; row++; }
  }
  return wid;
}

class QTMMinibarAction : public QWidgetAction {
  QVector <QAction*> actions;
public:
  QTMMinibarAction (QWidget* parent, array<widget>& arr)
  : QWidgetAction (parent)
  {
    actions.reserve(N(arr));
    for(int i = 0; i < N(arr); i++) {
      if (is_nil(arr[i])) break;
      QAction *act = concrete(arr[i])->as_qaction();
      act->setParent(this);
      actions.append(act);
    };
  }
  QWidget* createWidget(QWidget* parent);
  // virtual void activate (ActionEvent event) {
  //   cout << "TRIG\n"; QWidgetAction::activate (event); }
};

// FIXME: QTMMinibarAction::createWidget is called twice:
// the first time when the action is added to the menu,
// the second when from the menu it is transferred to the toolbar.
// This is weird since the first widget does not ever use
// the widget so it results in a waste of time.

QWidget*
QTMMinibarAction::createWidget(QWidget* parent) {
  if (DEBUG_QT) cout << "QTMMinibarAction::createWidget\n";
  QWidget* wid= new QWidget (parent);
  QBoxLayout* l= new QBoxLayout (QBoxLayout::LeftToRight, wid);
  wid->setLayout (l);
  //  l->setSizeConstraint (QLayout::SetFixedSize);
  l->setContentsMargins (0, 0, 0, 0);
  l->setSpacing(0);
  for (int i=0; i < actions.count(); i++) {
    QAction* sa= actions[i];
    if (QWidgetAction * wa = qobject_cast<QWidgetAction*>(sa)) {
      QWidget *w = wa->requestWidget(wid);
      l->addWidget(w);
    } else if ((sa->text().isNull())&&(sa->icon().isNull())) {
      l->addSpacing(8);
    } else {
      QToolButton *tb = new QToolButton(wid);
      
      //HACK: texmacs does not use the checked state of the action
      // if the action is checkable then it means that it should be
      // checked
      sa->setChecked(sa->isCheckable());
      
      tb->setDefaultAction(sa);
      tb->setAutoRaise(true);
      tb->setPopupMode (QToolButton::InstantPopup);
      tb->setStyle(qtmstyle());
      tb->setIconSize(QSize(12,12));
      QFont f = tb->font();
      f.setPixelSize(10);
      tb->setFont(f);
      l->addWidget(tb);
    }
  }
  return wid;
}

QPixmap
impress (simple_widget_rep* wid) {
  if (wid) {
    int width, height;
    wid->handle_get_size_hint (width, height);
    QSize s = QSize (width/PIXEL, height/PIXEL);
    QPixmap pxm(s);
    //cout << "impress (" << s.width() << "," << s.height() << ")\n";
    pxm.fill (Qt::transparent);
    {
      qt_renderer_rep *ren = the_qt_renderer();
      ren->begin (static_cast<QPaintDevice*>(&pxm));
      wid->set_current_renderer(the_qt_renderer());
      rectangle r = rectangle (0, 0, s.width(), s.height());
      ren->set_origin(0,0);
      ren->encode (r->x1, r->y1);
      ren->encode (r->x2, r->y2);
      ren->set_clipping (r->x1, r->y2, r->x2, r->y1);
      {
        // we do not want to be interrupted here...
        extern bool disable_check_event;
        bool cache = disable_check_event;
        disable_check_event= true;
        wid->handle_repaint (r->x1, r->y2, r->x2, r->y1);
        disable_check_event= cache;
      }
      ren->end();
      wid->set_current_renderer(NULL);
    }
    return pxm;
  }
  else {
    // return arbitrary image...
    QPixmap pxm (10, 10);
    return pxm;
  }
}

QAction*
simple_widget_rep::as_qaction () {
  QAction* a= new QTMAction (NULL);
  QPixmap pxm (impress (this));
  QIcon icon (pxm);
  a->setIcon (icon);
  return a;
}

void
rerootActions (QWidget* dest, QWidget* src) {
  QList<QAction *> list = dest->actions();
  while (!list.isEmpty()) {
    QAction* a= list.takeFirst();
    dest->removeAction (a);
    //    delete a;
    a->deleteLater();
  }
  list = src->actions();
  while (!list.isEmpty()) {
    QAction* a= list.takeFirst();
    dest->addAction (a);
    a->setParent (dest);
  }
}

void
QTMLazyMenu::force () {
  if (DEBUG_QT)  
    cout << "Force lazy menu" << LF;
  widget w= pm ();
  QMenu *menu2 = concrete(w)->get_qmenu();
  rerootActions (this, menu2);
}

