
/******************************************************************************
* MODULE     : qt_menu.h
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
#include <QtGlobal>
#include <QPointer>
#include <QBitmap>
#include <QGridLayout>
#include <QToolButton>
#include <QWidgetAction>
#include <QEvent>
#include <QStyleOptionMenuItem>
#include <QKeySequence>


#include "QTMGuiHelper.hpp"
#include "qt_gui.hpp"

// REMARK on memory management.
// the hierarchy of a QMenu has parents correctly set to the proper supermenu
// this guarantees that deletion of the root menu correclty deletes all the tree below it.
// the root menu itself (without parent QObject) is "owned" by the associate qt_menu_rep instance
// and it is deallocated by it. This ensure correct memory management between TeXmacs and Qt
// since qt_menu_rep is sometimes cached at TeXmacs level.
// this also mean that when we install some menu in the GUI (in the main menu or in the toolbar)
// we should just add actions and not reroot the qt parent hierarchy since even if the menu will
// be eventually removed from the GUI it has some chance to still be cached in the TeXmacs side
// conventions are as follows:
// - the method as_qaction passes the ownership of the action and the eventual submenu to the caller 
//   responsibility. When creating menu hierachy (eg. via the scheme interface) you should use this method
//   to retrieve the relevant Qt objects.
// - the method qt_menu_rep::get_menu preserve the onwership of the menu to the called qt_menu_rep
//   (to guarantee correct caching). when installing menus in the gui you should use this method.
// Submenus belongs implicilty to the parent QTMAction since it controls if the menu has an explicit parent
// and in negative case delete the submenu. All submenus in the menu hierarchy should have empty parent widget
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

QAction *qt_widget_rep::as_qaction() { return new QTMAction (NULL); };

/******************************************************************************/

class qt_menu_rep: public qt_widget_rep {
public:
  QAction  *item;
  qt_menu_rep (QAction* _item) : item (_item ? _item : new QTMAction (NULL)) {  }
  ~qt_menu_rep () { 
    delete item; // the submenu is usually also deleted since item is a QTMAction
  }

  QMenu *get_qmenu() { return (item ? item->menu() : NULL); }
  // get_menu doest not give ownership of the menu to the caller
  // this allow menu caching at the TeXmacs level
  // get_qmenu is called only by code which attach root menus in the GUI elements
  
  virtual void send (slot s, blackbox val);
  virtual widget make_popup_widget ();
  virtual widget popup_window_widget (string s);
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
* Widgets for the construction of menus
******************************************************************************/

widget
horizontal_menu (array<widget> arr) {
  // a horizontal menu made up of the widgets in a
  QAction* act= new QTMAction (NULL);
  act->setText("Menu");
  QMenu* m= new QMenu ();
  for (int i = 0; i < N(arr); i++) {
    if (is_nil (arr[i])) break;
    QAction* a= concrete (arr[i]) -> as_qaction ();
    m->addAction (a);
    a->setParent (m);
  }
  act->setMenu (m);
  return tm_new<qt_menu_rep> (act);     
}

widget
vertical_menu (array<widget> a) {
  // a vertical menu made up of the widgets in a
  return horizontal_menu (a);
}

#if 1
class QTMAuxMenu: public QMenu {
public:
  QTMAuxMenu (): QMenu() {}
  void myInitStyleOption (QStyleOptionMenuItem *option, const QAction *action) const {
    initStyleOption(option,action);
  }
};
#endif

class QTMToolButton: public QToolButton {
public:
  QTMToolButton (QWidget* parent = 0): QToolButton(parent) {}
  void mouseReleaseEvent(QMouseEvent *event);
  void mousePressEvent(QMouseEvent *event);
  void paintEvent(QPaintEvent *event);
};

void
QTMToolButton::mousePressEvent (QMouseEvent* event) {
  // this one triggers the action and toggles the button
  QToolButton::mousePressEvent (event);
  // this one forwards the event to the parent
  // (which eventually is the menu)
  QWidget::mousePressEvent (event);
}

void
QTMToolButton::mouseReleaseEvent (QMouseEvent* event) {
  // this one triggers the action and untoggles the button
  QToolButton::mouseReleaseEvent (event);
  // this one forwards the event to the parent
  // (which eventually is the menu which then close itself)
  QWidget::mouseReleaseEvent (event);
}

void
QTMToolButton::paintEvent(QPaintEvent* event) {
  (void) event;
  QPainter p (this);
#if 1
  QStyleOptionMenuItem option;
  QAction *action = defaultAction ();
  QTMAuxMenu m;
  m.myInitStyleOption (&option, action);
  option.rect = rect ();
  QRect r = rect ();
  style()->drawControl (QStyle::CE_MenuItem, &option, &p, this);
#else
  defaultAction()->icon().paint (&p, rect ());
#endif
}

class QTMTileAction: public QWidgetAction {
  QVector <QAction*> actions;
  int cols;
public:
  QTMTileAction (QWidget* parent, array<widget>& arr, int _cols):
    QWidgetAction (parent), cols (_cols)
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

// FIXME: QTMTileAction::createWidget is called twice:
// the first time when the action is added to the menu,
// the second when from the menu it is transferred to the toolbar.
// This is weird since the first widget does not ever use
// the widget so it results in a waste of time.

QWidget*
QTMTileAction::createWidget(QWidget* parent) {
  if (DEBUG_QT) cout << "QTMTileAction::createWidget\n";
  QWidget* wid= new QWidget (parent);
  QGridLayout* l= new QGridLayout (wid);
  wid->setLayout (l);
  l->setSizeConstraint (QLayout::SetFixedSize);
  l->setHorizontalSpacing (0);
  l->setVerticalSpacing (0);
  l->setContentsMargins (0, 0, 0, 0);
  int row= 0, col= 0;
  for (int i=0; i < actions.count(); i++) {
    QAction* sa= actions[i];
    QToolButton* tb= new QTMToolButton (wid);
    tb->setDefaultAction (sa);
    l->addWidget (tb, row, col);
    col++;
    if (col >= cols) { col = 0; row++; }
  }
  return wid;
}

widget
tile_menu (array<widget> a, int cols) {
  // a menu rendered as a table of cols columns wide & made up of widgets in a
  (void) cols;
#if 1
  QWidgetAction* act= new QTMTileAction (NULL, a, cols);
  return tm_new<qt_menu_rep> (act);
#else
  return horizontal_menu (a);
#endif
}

widget
menu_separator (bool vertical) {
  // a horizontal or vertical menu separator
  (void) vertical;
  QAction* a= new QTMAction (NULL);
  a->setSeparator (true);
  return tm_new<qt_menu_rep> (a);
}

widget
menu_group (string name) {
  // a menu group; the name should be greyed and centered
  QAction* a= new QTMAction (NULL);
  a->setText(to_qstring(tm_var_encode ((name))));
  a->setEnabled (false);
  return tm_new<qt_menu_rep> (a);
}

widget
pulldown_button (widget w, promise<widget> pw) {
  // a button w with a lazy pulldown menu pw
  QAction* a= concrete (w) -> as_qaction ();
  QTMLazyMenu* lm= new QTMLazyMenu (pw);
  QMenu *old_menu = a->menu();
  a->setMenu (lm);
  if (old_menu) {
    cout << "this should not happen\n";
    delete old_menu;
  }
  return tm_new<qt_menu_rep> (a);
}

widget
pullright_button (widget w, promise<widget> pw) {
  // a button w with a lazy pullright menu pw
  return pulldown_button (w, pw);
}

QAction*
qt_text_widget_rep::as_qaction () {
  QTMAction* a= new QTMAction (NULL);
  string t= tm_var_encode (str);
  if (t == "Help") t= "Help ";
  a->setText(to_qstring (t));
  a->str = str;
  return a;
}

QAction*
qt_image_widget_rep::as_qaction () {
  QAction* a= new QTMAction (NULL);
  QPixmap* img= the_qt_renderer () -> xpm_image (image);
  QIcon icon (*img);
  a->setIcon (icon);
  return a;
}

QAction*
qt_balloon_widget_rep::as_qaction() {
  QAction* a= concrete(text)->as_qaction();
  a->setToolTip (to_qstring (((qt_text_widget_rep*) hint.rep) -> str));
  return a;
}

string
conv_sub (string ks) {
  string r(ks);
#ifdef Q_WS_MAC
  r = replace (r, "S-", "Shift+");
  r = replace (r, "C-", "Meta+");
  r = replace (r, "A-", "Alt+");
  r = replace (r, "M-", "Ctrl+");
  //r = replace (r, "K-", "");
  r = replace (r, " ", ",");
#else
  r = replace (r, "S-", "Shift+");
  r = replace (r, "C-", "Ctrl+");
  r = replace (r, "A-", "Alt+");
  r = replace (r, "M-", "Meta+");
  //r = replace (r, "K-", "");
  r = replace (r, " ", ",");
#endif
  if (N(r) == 1 || (N(r) > 2 && r[N(r)-2] == '+')) {
    if (is_locase (r[N(r)-1]))
      r= r (0, N(r)-1) * upcase_all (r (N(r)-1, N(r)));
    else if (is_upcase (r[N(r)-1]))
      r= r (0, N(r)-1) * "Shift+" * upcase_all (r (N(r)-1, N(r)));
  }
  return r;
}

string
conv (string s) {
  int i=0, k;
  string r;
  for (k=0; k<=N(s); k++)
    if (k == N(s) || s[k] == ' ') {
      r << conv_sub (s (i, k));
      i= k;
    }
  return r;
}

widget
menu_button (widget w, command cmd, string pre, string ks,
	     bool ok, bool pressed) {
  // a command button with an optional prefix (o, * or v) and
  // keyboard shortcut; if ok does not hold, then the button is greyed
  QAction* a= NULL;
  a= concrete(w)->as_qaction();
#ifdef Q_WS_MAC
  if (search_forwards (" ", ks) != -1) ks= "";
#endif
  if (N(ks) > 0) {
    string qtks = conv (ks);
    QKeySequence qks (to_qstring (qtks));
    if (DEBUG_QT)
      cout << "ks: " << ks << " " << qks.toString().toAscii().data() << "\n";
    a->setShortcut (qks);
    QTMKeyCommand* c= new QTMKeyCommand (ks);
    c->setParent (a);
    QObject::connect (a, SIGNAL (triggered ()), c, SLOT (apply ()),
                      Qt::QueuedConnection);    
  } else {
    QTMCommand* c= new QTMCommand (cmd.rep);
    c->setParent (a);
    QObject::connect (a, SIGNAL (triggered ()), c, SLOT (apply ()),
                      Qt::QueuedConnection);    
  }
  // FIXME: implement complete prefix handling
  a->setEnabled (ok? true: false);
  a->setCheckable (pre != ""? true: false);
  a->setChecked (pre != ""? true: false);
  if (pre == "v") {}
  else if (pre == "*") {}
    // [mi setOnStateImage:[NSImage imageNamed:@"TMStarMenuBullet"]];
  else if (pre == "o") {}
  return tm_new<qt_menu_rep> (a);
}

widget
balloon_widget (widget w, widget help)  {
  // given a button widget w, specify a help balloon which should be displayed
  // when the user leaves the mouse pointer on the button for a small while
  return tm_new<qt_balloon_widget_rep> (w, help);
}

widget
text_widget (string s, color col, bool tsp) {
  // a text widget with a given color and transparency
  return tm_new<qt_text_widget_rep> (s, col, tsp);
}

widget
xpm_widget (url file_name) {
  // return widget ();
  // a widget with an X pixmap icon
  return tm_new<qt_image_widget_rep> (file_name);
}

QMenu*
to_qmenu(widget w) {
  //FIXME: the static cast is not sure in general, how we can test for
  //       the right widget type?
  QMenu *m = static_cast<qt_menu_rep*>(w.rep)->get_qmenu();
  return m;
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
      wid->handle_repaint (r->x1, r->y2, r->x2, r->y1);
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
  QMenu *menu2 = to_qmenu(w);
  rerootActions (this, menu2);
}
