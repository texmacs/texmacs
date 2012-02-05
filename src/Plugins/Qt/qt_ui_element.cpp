/******************************************************************************
 * MODULE     : qt_ui_element.cpp
 * DESCRIPTION: User interface proxies
 * COPYRIGHT  : (C) 2010  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "qt_ui_element.hpp"
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
#include "qt_window_widget.hpp"
#include "qt_menu.hpp"


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




QTMWidgetAction::QTMWidgetAction (widget _wid, QObject *parent)
: QWidgetAction (parent), wid (_wid) { 
  QObject::connect (the_gui->gui_helper, SIGNAL(refresh()), this, SLOT(doRefresh()));
}

QTMWidgetAction::~QTMWidgetAction() {
}


void 
QTMWidgetAction::doRefresh() {
#if 0
  if (N(str)) {
    string t= tm_var_encode (str);
    if (t == "Help") t= "Help ";
    setText (to_qstring (t));
  }
#endif
}

QWidget * 
QTMWidgetAction::createWidget ( QWidget * parent ) {
  QWidget *qw = concrete(wid)->as_qwidget();
  qw->setParent(parent);
  return qw;
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
      //  tb->setIconSize(QSize(12,12));
      QFont f = tb->font();
      f.setPixelSize(10);
      tb->setFont(f);
      l->addWidget(tb);
    }
  }
  return wid;
}



static string
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

static string
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


qt_ui_element_rep::~qt_ui_element_rep()
{
  if (cachedAction) delete cachedAction;
}

widget 
qt_ui_element_rep::make_popup_widget () {
  return tm_new<qt_menu_rep>(as_qaction());
}

widget 
qt_ui_element_rep::popup_window_widget (string s)  {
  return concrete(make_popup_widget())->popup_window_widget(s);
}




class qt_plain_widget_rep: public qt_view_widget_rep {
  
public:
  qt_plain_widget_rep (QWidget *v) : qt_view_widget_rep(v) {};
  ~qt_plain_widget_rep () {};
  
  virtual void send (slot s, blackbox val);
//  virtual blackbox query (slot s, int type_id);
//  virtual widget read (slot s, blackbox index);
//  virtual void write (slot s, blackbox index, widget w);
//  virtual void notify (slot s, blackbox new_val);
  
 // virtual widget plain_window_widget (string s);
 // void set_current_renderer(basic_renderer _r) { current_renderer = _r;  }
 // basic_renderer get_current_renderer() {  return current_renderer; }
 // virtual QWidget* as_qwidget () { return view ; };
  
};


void
qt_plain_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT)
    cout << "qt_plain_widget_rep::send " << slot_name(s) << LF;
  switch (s) {
    case SLOT_POSITION:
      ASSERT (type_box (val) == type_helper<coord2>::id, "type mismatch");
      NOT_IMPLEMENTED;
      break;
    case SLOT_VISIBILITY:
    {   
      check_type<bool> (val, "SLOT_VISIBILITY");
      bool flag = open_box<bool> (val);
      if (flag)
        view->show();
      else 
        view->hide();
    }   
      break;
    default:
      qt_view_widget_rep::send (s,val);
  }
}


widget 
qt_ui_element_rep::plain_window_widget (string s)  {
  QLayoutItem *li = as_qlayoutitem();
  QWidget *w;
  if (li->widget()) 
    w = li->widget();
  else if (li->layout()) {
    w = new QWidget();
    w->setLayout(li->layout());
  } else {
    w = new QWidget();
  }
  w->setWindowTitle (to_qstring (s));
  return tm_new<qt_plain_widget_rep>(w);
//  concrete(make_popup_widget())->plain_window_widget(s);
}

QMenu *
qt_ui_element_rep::get_qmenu() {
  if (!cachedAction) {
    cachedAction = as_qaction();
  }
  return (cachedAction ? cachedAction->menu() : NULL);
}


QAction* 
qt_ui_element_rep::as_qaction () {
  switch (type) {
    case horizontal_menu:
    case vertical_menu:
    case horizontal_list:
    case vertical_list:
    {
      typedef array<widget> T;
      array<widget> arr = open_box<T> (load);
      
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
      return act;
    }
      break;
      
    case tile_menu: 
    {
      typedef pair<array<widget>, int> T;
      T x = open_box<T>(load);
      array<widget> a = x.x1;
      int cols = x.x2;
      
      // a menu rendered as a table of cols columns wide & made up of widgets in a
      QWidgetAction* act= new QTMTileAction (NULL, a, cols);
      return act;
    }
      break;
      
    case minibar_menu: 
    {
      typedef array<widget> T;
      array<widget> arr = open_box<T> (load);

      QWidgetAction* act= new QTMMinibarAction (NULL, arr);
      return act;
    }
      break;
      
    case menu_separator: 
    {
      typedef bool T;
      bool vertical = open_box<T> (load);
      // a horizontal or vertical menu separator
      (void) vertical;
      QAction* a= new QTMAction (NULL);
      a->setSeparator (true);
      return a;
    }
      break;
      
    case menu_group: 
    {
      typedef pair<string, int> T;
      T x = open_box<T>(load);
      string name = x.x1;
      int style = x.x2;
      
      (void) style;
      // a menu group; the name should be greyed and centered
      QAction* a= new QTMAction (NULL);
      a->setText(to_qstring(tm_var_encode ((name))));
      a->setEnabled (false);
      if (style == WIDGET_STYLE_MINI) {
        QFont f = a->font();
        f.setPointSize(10);
        a->setFont(f);
      }  
      return a;
    }
      break;
      
    case pulldown_button:
    case pullright_button:
    {
      typedef pair<widget, promise<widget> > T;
      T x = open_box<T>(load);
      widget w = x.x1;
      promise<widget> pw = x.x2;
      
      // a button w with a lazy pulldown menu pw
      QAction* a= concrete (w) -> as_qaction ();
      QTMLazyMenu* lm= new QTMLazyMenu (pw);
      QMenu *old_menu = a->menu();
      a->setMenu (lm);
      a->setEnabled(true);
      if (old_menu) {
        cout << "this should not happen\n";
        delete old_menu;
      }
      return a;
    }
      break;
      
    case menu_button:
    {
      typedef quintuple<widget, command, string, string, int> T;
      T x = open_box<T>(load);
      widget w = x.x1;
      command cmd = x.x2;
      string pre = x.x3;
      string ks = x.x4;
      int style = x.x5;
      
      // a command button with an optional prefix (o, * or v) and
      // keyboard shortcut; if ok does not hold, then the button is greyed
      bool ok= (style & WIDGET_STYLE_INERT) == 0;
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
      
      bool check = (pre != "") || (style & WIDGET_STYLE_PRESSED);
      
      a->setCheckable (check? true: false);
      a->setChecked (check? true: false);
      if (pre == "v") {}
      else if (pre == "*") {}
      // [mi setOnStateImage:[NSImage imageNamed:@"TMStarMenuBullet"]];
      else if (pre == "o") {}
      return a;
    }
      break;
      
    case balloon_widget:
    {
      typedef pair<widget, widget> T;
      T x = open_box<T>(load);
      widget text = x.x1;
      widget help = x.x2;
      
      // given a button widget w, specify a help balloon which should be displayed
      // when the user leaves the mouse pointer on the button for a small while
      QAction* a= concrete(text)->as_qaction();
      {
        typedef quartet<string, int, color, bool> T1;
        T1 x = open_box<T1>(static_cast<qt_ui_element_rep*>(help.rep)->load);
        string str = x.x1;
        a->setToolTip (to_qstring (str));
      }
      return a;
    }
      break;
      
    case text_widget:
    {
      typedef quartet<string, int, color, bool> T;
      T x = open_box<T>(load);
      string str = x.x1;
      int style = x.x2;
      //color col = x.x3;
      //bool tsp = x.x4;
      
      // a text widget with a given color and transparency

      QTMAction* a= new QTMAction (NULL);
      string t= tm_var_encode (str);
      if (t == "Help") t= "Help ";
      a->setText(to_qstring (t));
      a->str = str;
      if (style == WIDGET_STYLE_MINI) {
        QFont f = a->font();
        f.setPointSize(10);
        a->setFont(f);
      }
      return a;
    }
      break;
      
    case xpm_widget:
    {
      url image = open_box<url>(load);

      // return widget ();
      // a widget with an X pixmap icon
      QAction* a= new QTMAction (NULL);
      QPixmap* img= the_qt_renderer () -> xpm_image (image);
      QIcon icon (*img);
      a->setIcon (icon);
      return a;
    }
      break;
      
    default:
      ;
  }
  
  return NULL;
}


class QTMUIButton: public QToolButton {
public:
  QTMUIButton (QWidget* parent = 0): QToolButton(parent) {}
  void paintEvent(QPaintEvent *event);
};


void
QTMUIButton::paintEvent(QPaintEvent* event) {
  (void) event;
  QPainter p (this);
  defaultAction()->icon().paint (&p, rect ());
}

QLayoutItem *
qt_ui_element_rep::as_qlayoutitem () {
  switch (type) {
    case horizontal_menu:
    case vertical_menu:
    case horizontal_list:
    case vertical_list:
    {
      typedef array<widget> T;
      array<widget> arr = open_box<T> (load);
      
      // a horizontal/vertical menu made up of the widgets in a
      QLayout *l;
      if ((type == horizontal_list) || (type==horizontal_menu))
        l =  new QHBoxLayout();
      else
        l =  new QVBoxLayout();

      for (int i = 0; i < N(arr); i++) {
        if (is_nil (arr[i])) break;
        QLayoutItem* li= concrete (arr[i]) -> as_qlayoutitem ();
        if (li) l->addItem(li); // ownership transferred
      }
      return l;
    }
      break;
      
    case tile_menu: 
    {
      typedef pair<array<widget>, int> T;
      T x = open_box<T>(load);
      array<widget> a = x.x1;
      int cols = x.x2;
      
      // a menu rendered as a table of cols columns wide & made up of widgets in a
      
      QGridLayout* l= new QGridLayout ();
      l->setSizeConstraint (QLayout::SetFixedSize);
      l->setHorizontalSpacing (2);
      l->setVerticalSpacing (2);
      l->setContentsMargins (4, 0, 4, 0);
      int row= 0, col= 0;
      for (int i=0; i < N(a); i++) {
        QLayoutItem *li = concrete(a[i])->as_qlayoutitem();
        l->addItem(li, row, col);
        col++;
        if (col >= cols) { col = 0; row++; }
      }
      return l;
    }
      break;
      
    case minibar_menu: 
    {
      typedef array<widget> T;
      array<widget> arr = open_box<T> (load);
      QBoxLayout* l= new QBoxLayout (QBoxLayout::LeftToRight);
      l->setContentsMargins (0, 0, 0, 0);
      l->setSpacing(0);
      for (int i=0; i < N(arr); i++) {
        QLayoutItem *li = concrete(arr[i])->as_qlayoutitem();
        l->addItem(li);
      }
      return l;
    }
      break;
      
    case menu_separator: 
    {
      typedef bool T;
      bool vertical = open_box<T> (load);
      // a horizontal or vertical menu separator
      (void) vertical;
      //FIXME: implement h/v
      return new QSpacerItem(1,1);
    }
      break;
      
    case menu_group: 
    {
      typedef pair<string, int> T;
      T x = open_box<T>(load);
      string name = x.x1;
      int style = x.x2;
      
      (void) style;
      // a menu group; the name should be greyed and centered
      return NULL;
    }
      break;
      
    case pulldown_button:
    case pullright_button:
    case menu_button:
    case text_widget:
    case xpm_widget:
    {
      QWidget *w = this->as_qwidget();
      return new QWidgetItem(w);
    }
      break;
      
    case balloon_widget:
    {
      typedef pair<widget, widget> T;
      T x = open_box<T>(load);
      widget text = x.x1;
      widget help = x.x2;
      
      // given a button widget w, specify a help balloon which should be displayed
      // when the user leaves the mouse pointer on the button for a small while
      QLayoutItem* li= concrete(text)->as_qlayoutitem();
      if (li->widget())
      {
        typedef quartet<string, int, color, bool> T1;
        T1 x = open_box<T1>(static_cast<qt_ui_element_rep*>(help.rep)->load);
        string str = x.x1;
        li->widget()->setToolTip (to_qstring (str));
      }
      return li;
    }
      break;
      
      
    default:
      ;
  }
  
  return NULL;
}

QWidget *
qt_ui_element_rep::as_qwidget () {
  switch (type) {
    case horizontal_menu:
    case vertical_menu:
    case horizontal_list:
    case vertical_list:
    case tile_menu: 
    case minibar_menu: 
    {
      QLayoutItem *li = this->as_qlayoutitem();
      QWidget *w = new QWidget();
      if (QLayout *l = li->layout()) {
        w->setLayout(l);
      } else {
        cout << "qt_ui_element_rep::as_qwidget : invalid situation" << LF;
      }
      delete li;
      return w;
    }
      break;
      
    case menu_separator: 
    case menu_group: 
    {
      return NULL;
    }
      break;
      
    case pulldown_button:
    case pullright_button:
    {
      typedef pair<widget, promise<widget> > T;
      T x = open_box<T>(load);
      widget w = x.x1;
      promise<widget> pw = x.x2;
      
      // a button w with a lazy pulldown menu pw
      
      QAction* a= concrete (this) -> as_qaction ();
      QToolButton *b = new QTMUIButton();
      a->setParent(b);
      b->setDefaultAction(a);
      return b;
    }
      break;
      
    case menu_button:
    {
      typedef quintuple<widget, command, string, string, int> T;
      T x = open_box<T>(load);
      widget w = x.x1;
      command cmd = x.x2;
      string pre = x.x3;
      string ks = x.x4;
      int style = x.x5;
      
      
      // a command button with an optional prefix (o, * or v) and
      // keyboard shortcut; if ok does not hold, then the button is greyed
      QAction* a= concrete(this)->as_qaction();
      QToolButton *b = (style & WIDGET_STYLE_BUTTON) ? new QToolButton() : new QTMUIButton();
      b->setDefaultAction(a);
      a->setParent(b);
      return b;
    }
      break;
      
    case balloon_widget:
    {
      typedef pair<widget, widget> T;
      T x = open_box<T>(load);
      widget text = x.x1;
      widget help = x.x2;
      
      // given a button widget w, specify a help balloon which should be displayed
      // when the user leaves the mouse pointer on the button for a small while
      QWidget* w= concrete(text)->as_qwidget();
      if (w)
      {
        typedef quartet<string, int, color, bool> T1;
        T1 x = open_box<T1>(static_cast<qt_ui_element_rep*>(help.rep)->load);
        string str = x.x1;
        w->setToolTip (to_qstring (str));
      }
      return w;
    }
      break;
      
    case text_widget:
    {
      typedef quartet<string, int, color, bool> T;
      T x = open_box<T>(load);
      string str = x.x1;
      int style = x.x2;
      //color col = x.x3;
      //bool tsp = x.x4;
      //str= "<font color=#f00>" * str * "</font>";
      
      // a text widget with a given color and transparency
      QLabel *w = new QLabel();
#if 0
      //FIXME: implement refresh when changing language
      QTMAction* a= new QTMAction (NULL);
      //a->str = str;
#endif
      string t= tm_var_encode (str);
      if (t == "Help") t= "Help ";
      //w->setTextFormat(Qt::RichText);
      w->setText(to_qstring (t));
      if (style == WIDGET_STYLE_MINI) {
        QFont f = w->font();
        f.setPointSize(10);
        w->setFont(f);
      }
      return w;
    }
      break;
      
    case xpm_widget:
    {
      url image = open_box<url>(load);
      
      // return widget ();
      // a widget with an X pixmap icon
      QLabel* l= new QLabel (NULL);
      QPixmap* img= the_qt_renderer () -> xpm_image (image);
      QIcon icon (*img);
      l->setPixmap (*img);
      return l;
    }
      break;
      
    default:
      ;
  }
  
  return NULL;
}




/******************************************************************************
 * Widgets for the construction of menus and dialogs
 ******************************************************************************/

// TeXmacs interface

widget horizontal_menu (array<widget> arr) { return qt_ui_element_rep::create (qt_ui_element_rep::horizontal_menu, arr); }
widget vertical_menu (array<widget> arr)  { return qt_ui_element_rep::create (qt_ui_element_rep::vertical_menu, arr); }
widget horizontal_list (array<widget> arr) { return qt_ui_element_rep::create (qt_ui_element_rep::horizontal_list, arr); }
widget vertical_list (array<widget> arr) { return qt_ui_element_rep::create (qt_ui_element_rep::vertical_list, arr); }
widget aligned_widget (array<widget> lhs, array<widget> rhs, SI, SI, SI, SI) {
  // FIXME: to be implemented in a clean way
  array<widget> a;
  for (int i=0; i<min(N(lhs),N(rhs)); i++) a << lhs[i] << rhs[i];
  return tile_menu (a, 2); }
widget tabs_widget (array<widget> tabs, array<widget> bodies) {
  (void) tabs; (void) bodies; ERROR ("not yet implemented"); }
widget tile_menu (array<widget> a, int cols) { return qt_ui_element_rep::create (qt_ui_element_rep::tile_menu, a, cols); }
widget minibar_menu (array<widget> arr) { return qt_ui_element_rep::create (qt_ui_element_rep::minibar_menu, arr); }
widget menu_separator (bool vertical) { return qt_ui_element_rep::create (qt_ui_element_rep::menu_separator, vertical); }
widget menu_group (string name, int style) { return qt_ui_element_rep::create (qt_ui_element_rep::menu_group , name, style); }
widget pulldown_button (widget w, promise<widget> pw) { return qt_ui_element_rep::create (qt_ui_element_rep::pulldown_button, w, pw); }
widget pullright_button (widget w, promise<widget> pw) { return qt_ui_element_rep::create (qt_ui_element_rep::pullright_button, w, pw); }
widget menu_button (widget w, command cmd, string pre, string ks, int style) { return qt_ui_element_rep::create (qt_ui_element_rep::menu_button, w, cmd, pre, ks, style); }
widget balloon_widget (widget w, widget help) { return qt_ui_element_rep::create (qt_ui_element_rep::balloon_widget, w, help); }
widget text_widget (string s, int style, color col, bool tsp) { return qt_ui_element_rep::create (qt_ui_element_rep::text_widget, s, style, col, tsp); }
widget xpm_widget (url file_name) { return qt_ui_element_rep::create (qt_ui_element_rep::xpm_widget, file_name); }

