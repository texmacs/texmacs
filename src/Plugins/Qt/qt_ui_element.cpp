/******************************************************************************
 * MODULE     : qt_ui_element.cpp
 * DESCRIPTION: User interface proxies
 * COPYRIGHT  : (C) 2010  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "qt_utilities.hpp"
#include "qt_renderer.hpp"

#include "qt_widget.hpp"
#include "qt_ui_element.hpp"
#include "qt_simple_widget.hpp"
#include "qt_window_widget.hpp"
#include "qt_menu.hpp"

#include "analyze.hpp"
#include "widget.hpp"
#include "message.hpp"
#include "promise.hpp"
#include "scheme.hpp"

#include <QtGui>
#include "QTMWindow.hpp"
#include "QTMGuiHelper.hpp"
#include "QTMMenuHelper.hpp"
#include "QTMStyle.hpp"

#include "qt_gui.hpp"

#include "../../Style/Evaluate/evaluate_main.hpp" // required for as_length(string)


/******************************************************************************
 * Auxiliary classes
 ******************************************************************************/

/*!
 * We use this class to properly initialize style options for our QWidgets
 * which have to blend into QMenus
 *   see #QTBUG-1993.
 *   see #QTBUG-7707.
 */
class QTMAuxMenu: public QMenu {
public:
  QTMAuxMenu (): QMenu() {}
  
  void myInitStyleOption (QStyleOptionMenuItem *option) const {
    QAction action (NULL);
    initStyleOption(option,&action);
  }
};


/*! QTMMenuButton is a custom button appropriate for menus
 
  We need to subclass QToolButton for two reasons
   1) custom appearence
   2) if used in QWidgetAction the menu does not disappear upon triggering the
      button. See QTBUG-10427.
 */
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


/*! A QToolButton which draws itself using its defaultAction()'s icon. */
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
QTMWidgetAction::createWidget (QWidget * parent) {
  QWidget* qw = concrete(wid)->as_qwidget();
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

/*!
  FIXME: QTMTileAction::createWidget is called twice:
  the first time when the action is added to the menu,
  the second when from the menu it is transferred to the toolbar.
  This is weird since the first widget does not ever use
  the widget so it results in a waste of time.
 */
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

/*!
  FIXME: QTMMinibarAction::createWidget is called twice:
  the first time when the action is added to the menu,
  the second when from the menu it is transferred to the toolbar.
  This is weird since the first widget does not ever use
  the widget so it results in a waste of time. 
 */
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


/******************************************************************************
 * Ad-hoc command_rep derivates for different UI elements in qt_ui_element_rep
 ******************************************************************************/

/*! Ad-hoc command to be used to simulate keypresses
 * 
 * \sa qt_ui_element, , qt_ui_element_rep::as_qaction
 */

class qt_key_command_rep: public command_rep {
  string ks; 
  
public:
  qt_key_command_rep(string ks_) : ks(ks_) { }
  
  void apply () {
    if (N(ks)) { 
      QTMWidget *w = qobject_cast<QTMWidget*>(qApp->focusWidget());
      if (w && w->tm_widget()) {
        if (DEBUG_QT) cout << "shortcut: " << ks << LF;
        the_gui->process_keypress (w->tm_widget(), ks, texmacs_time());
      }
    }
  }
  
  tm_ostream& print (tm_ostream& out) { return out << "qt_key_command_rep"; }
};


/*! Ad-hoc command to be used with toggle widgets.
 * The command associated with a qt_ui_element::toggle_widget has as a parameter the state
 * of the QCheckBox. Since it is assumed everywhere else that commands injected into
 * the gui's queue accept no parameters, and changes would be too big, we choose to
 * encapsulate the original command in a new one which will execute the first with 
 * its argument.
 * \sa qt_ui_element, , qt_ui_element_rep::as_qwidget, qt_ui_element_rep::toggle_widget
 */
class qt_toggle_command_rep: public command_rep {
  QPointer<QCheckBox> qwid;
  command cmd; 
  
public:
  qt_toggle_command_rep(QCheckBox* w, command c) : qwid(w), cmd(c) { }
  void apply () { if (qwid) cmd (list_object (object (qwid->isChecked()))); }

  tm_ostream& print (tm_ostream& out) { return out << "Toggle"; }
};

/*! Ad-hoc command to be used with enum widgets.
 * The command associated with a qt_ui_element::enum_widget has one parameter. For the
 * reason to be of this class, see \sa qt_toggle_command_rep .
 * \sa qt_ui_element, , qt_ui_element_rep::as_qwidget, qt_ui_element_rep::enum_widget
 */
class qt_enum_command_rep: public command_rep {
  QPointer<QComboBox> qwid;
  command cmd; 
  
public:
  qt_enum_command_rep(QComboBox* w, command c) : qwid(w), cmd(c) {}
  void apply () { 
    if (qwid)
      cmd (list_object (object (from_qstring(qwid->currentText()))));
  }
  
  tm_ostream& print (tm_ostream& out) { return out << "Enum"; }
};

/*! Ad-hoc command to be used with choice widgets.
 * The command associated with a qt_ui_element::choice_widget has one parameter. (a
 * list of selected items).
 * For the reason to be of this class, see \sa qt_toggle_command_rep.
 * \sa qt_ui_element, , qt_ui_element_rep::as_qwidget, qt_ui_element_rep::choice_widget
 */
class qt_choice_command_rep: public command_rep {
  QPointer<QListWidget> qwid;
  command cmd;
  bool multiple;  //<! Whether multiple choices are allowed in the widget.
  
public:
  qt_choice_command_rep(QListWidget* w, command c, bool m) : qwid(w), cmd(c), multiple(m) {}
  void apply () { 
    if (qwid) {
      QList<QListWidgetItem*> items = qwid->selectedItems();
      array<string> selected;
      for(int i = 0; i < items.size(); ++i)
        selected << from_qstring (items[i]->text());
      object l= null_object ();
      if(multiple)
        for (int i = N(selected)-1; i >= 0; --i)
          l= cons (selected[i], l);
      else if(N(selected)>0)  //Do not return a list with the item if only one
        l= selected[0];
      cmd (list_object (l));
    }
  }
  
  tm_ostream& print (tm_ostream& out) { return out << "Choice"; }
};


/******************************************************************************
 * glue widget
 ******************************************************************************/

QPixmap 
qt_glue_widget_rep::render () {
  QSize s = to_qsize(w, h);
  QPixmap pxm(s);
    //cout << "glue (" << s.width() << "," << s.height() << ")\n";
  pxm.fill (Qt::transparent);
  QPaintDevice *pd = static_cast<QPaintDevice*>(&pxm);
  
  if (pd && !pxm.isNull()) {
    qt_renderer_rep *ren = the_qt_renderer();
    ren->begin (pd);
    rectangle r = rectangle (0, 0, s.width(), s.height());
    ren->set_origin(0,0);
    ren->encode (r->x1, r->y1);
    ren->encode (r->x2, r->y2);
    ren->set_clipping (r->x1, r->y2, r->x2, r->y1);
    
    if (col == "") {
        // do nothing
    } else {
      if (is_atomic (col)) {
        color c= named_color (col->label);
        ren->set_background (c);
        ren->set_color (c);
        ren->fill (r->x1, r->y2, r->x2, r->y1);
      } else {
        ren->set_shrinking_factor (5);
        int  old_a;
        tree old_bg= ren->get_background_pattern (old_a);
        ren->set_background_pattern (col);
        ren->clear_pattern (5*r->x1, 5*r->y2, 5*r->x2, 5*r->y1);
        ren->set_background_pattern (old_bg, old_a);
        ren->set_shrinking_factor (1);
      }
    }
    ren->end();
  }
  
  return pxm;  
}

QAction *
qt_glue_widget_rep::as_qaction() {
  QAction *a= new QTMAction();
  a->setText(to_qstring(as_string(col)));
  QIcon icon;
#if 0
  tree old_col = col;
  icon.addPixmap(render(), QIcon::Active, QIcon::On);
  col = "";
  icon.addPixmap(render(), QIcon::Normal, QIcon::On);
  col = old_col;
#else
  icon.addPixmap (render ());
#endif
  a->setIcon (icon);  
  a->setEnabled(false);
  return a;
}

QWidget *
qt_glue_widget_rep::as_qwidget() {
  QLabel* w = new QLabel();
  w->setText (to_qstring (as_string (col)));
  QIcon icon;
  w->setPixmap (render ());  
    //  w->setEnabled(false);
  qwid = w;
  return qwid;
}


/******************************************************************************
 * qt_ui_element_rep
 ******************************************************************************/

qt_ui_element_rep::qt_ui_element_rep (types _type, blackbox _load)
  : qt_widget_rep(_type), load(_load), cachedAction(NULL)  { }

qt_ui_element_rep::~qt_ui_element_rep() {
  if (cachedAction) delete cachedAction;
}

/*! Returns the ui element as a popup widget.
 If the widget is of type vertical_menu, it is understood that the popup widget
 must be of the standard OS dependent type implemented by qt_menu_rep using 
 QMenu.
 */
widget 
qt_ui_element_rep::make_popup_widget () {
  if (type == qt_widget_rep::vertical_menu)
    return tm_new<qt_menu_rep>(as_qaction());
  else
    return qt_widget_rep::make_popup_widget();
}

QMenu *
qt_ui_element_rep::get_qmenu() {
  if (!cachedAction) {
    cachedAction = as_qaction();
  }
  return (cachedAction ? cachedAction->menu() : NULL);
}


/*! For the refresh_widget
 * FIXME? Is this really used?
 */
qt_ui_element_rep::operator tree () {
  if (type == refresh_widget) {
    return tree (TUPLE, "refresh", open_box<string> (load));
  } else {
    return tree();
  }
}

QAction* 
qt_ui_element_rep::as_qaction () {
  //if (DEBUG_QT)
  //cout << "as_qaction: " << type_as_string() << LF;
  
  switch (type) {
    case vertical_menu:
    case horizontal_menu:
    case vertical_list:
    {
        // a vertical menu made up of the widgets in arr
      typedef array<widget> T;
      array<widget> arr = open_box<T> (load);
      
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
    
    case horizontal_list:       
    {
        // an horizontal list made up of the widgets in arr
      typedef array<widget> T;
      array<widget> arr = open_box<T> (load);
      return new QTMMinibarAction (NULL, arr);
    }
      break;
      
    case aligned_widget:       
    {
        //  a table with two columns FIXME!!!!
      
      typedef triple<array<widget>, array<widget>, coord4 > T;
      T x = open_box<T>(load);
      array<widget> lhs = x.x1;
      array<widget> rhs = x.x2;

      if (N(lhs) != N(rhs)) FAILED("aligned_widget: N(lhs) != N(rhs) ");
      
      array<widget> wids(N(lhs)*3);
      for (int i=0; i < N(lhs); ++i) {
        wids[3*i]   = lhs[i];
        wids[3*i+1] = rhs[i];
        wids[3*i+2] = ::glue_widget(false, true, 1, 1);
      }

      return new QTMMinibarAction (NULL, wids);
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

    case glue_widget:
    {
      QTMAction* a = new QTMAction();
      a->setEnabled(false);
      return a;
    }
      break;

    case menu_group: 
    {
      typedef pair<string, int> T;
      T x = open_box<T>(load);
      string name = x.x1;
      int   style = x.x2; 
        // FIXME? use to_qstylesheet(style) together with QWidgetAction?
      
      // a menu group; the name should be greyed and centered
      QAction* a= new QTMAction (NULL);
      a->setText(to_qstring(tm_var_encode ((name))));
      a->setEnabled (false);
      a->setFont(to_qfont(style, a->font())); 
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
        if (DEBUG_QT) 
          cout << "qt_ui_element_rep::as_qaction(), this should not happen\n";
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
      QTMCommand* c;
      if (N(ks) > 0) {
        QKeySequence qks = to_qkeysequence (ks);
        if (DEBUG_QT)
          cout << "ks: " << ks << " " << qks.toString().toAscii().data() << "\n";
        a->setShortcut (qks);
        command key_cmd = tm_new<qt_key_command_rep>(ks);
        c= new QTMCommand (a, key_cmd);
      } else {
        c= new QTMCommand (a, cmd);
      }
        
      // NOTE: this used to be a Qt::QueuedConnection, but the slot would not
      // be called if in a contextual menu
      QObject::connect (a, SIGNAL (triggered ()), c, SLOT (apply ()));    
  
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
        // HACK: force displaying of the tooltip (needed for items in the QMenuBar)
        QObject::connect(a, SIGNAL(hovered()), a, SLOT(showToolTip()));
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
      if (t == "Help") t= "Help "; // HACK to avoid MacOS autodetection of the Help menu?
      a->setText(to_qstring (t));
      a->str = str;
      a->setFont(to_qfont(style, a->font()));
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


QLayoutItem *
qt_ui_element_rep::as_qlayoutitem () {
  if (DEBUG_QT)
    cout << "as_qlayoutitem: " << type_as_string() << LF;

  switch (type) {
    case horizontal_menu:
    case vertical_menu:
    case horizontal_list:
    case vertical_list:
    {
      typedef array<widget> T;
      T arr = open_box<T> (load);
      
      // a horizontal/vertical menu/widget made up of the widgets in arr
      QLayout *l;
      if ((type == horizontal_list) || (type==horizontal_menu))
        l = new QHBoxLayout();
      else
        l = new QVBoxLayout();

      l->setSpacing(0);

      if (N(arr) > 0 && concrete(arr[0]).rep && concrete(arr[0]).rep->type == tabs_widget)  // HACK!
        l->setContentsMargins(0, 6, 0, 0);
      else
        l->setContentsMargins(0, 0, 0, 0);

      for (int i = 0; i < N(arr); i++) {
        if (is_nil (arr[i])) break;
        QLayoutItem* li = concrete (arr[i])->as_qlayoutitem ();
        if (li) l->addItem(li); // ownership transferred
      }

      return l;
    }

    // a menu rendered as a table of 'cols' columns wide & made up of widgets in
    // the array 'a'
    case tile_menu: 
    {
      typedef array<widget> T1;
      typedef pair<T1, int> T;
      T  x     = open_box<T>(load);
      T1 a     = x.x1;
      int cols = x.x2;
            
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

    //  a table with two columns
    case aligned_widget:       
    {
      typedef array<widget> T2;
      typedef triple<T2, T2, coord4 > T;
      T      x = open_box<T>(load);
      T2   lhs = x.x1;
      T2   rhs = x.x2;
      coord4 y = x.x3;

      // FIXME: lpad and rpad ignored.
      SI hsep = y.x1; SI vsep = y.x2; SI lpad = y.x3; SI rpad = y.x4;
      (void) lpad; (void) rpad;
     
      if (N(lhs) != N(rhs)) FAILED("aligned_widget: N(lhs) != N(rhs) ");
      
        /* FIXME? From the docs:
         If the QGridLayout is not the top-level layout (i.e. does not manage 
         all of the widget's area and children), you must add it to its parent 
         layout when you create it, but before you do anything with it. 
         */
      QGridLayout* l= new QGridLayout ();
      l->setSizeConstraint (QLayout::SetMinimumSize);
      l->setHorizontalSpacing (6+hsep/PIXEL);
      l->setVerticalSpacing (2+vsep/PIXEL);
        // Columns with a higher stretch factor take more of the available space.
        //l->setColumnStretch(0, 10);
        //l->setColumnStretch(1, 1);
      for (int i=0; i < N(lhs); i++) {
        QLayoutItem* lli = concrete(lhs[i])->as_qlayoutitem();
        QLayoutItem* rli = concrete(rhs[i])->as_qlayoutitem();
        if (lli) l->addItem(lli, i, 0, 1, 1, Qt::AlignRight);
        if (rli) l->addItem(rli, i, 1, 1, 1, Qt::AlignLeft);
      }
      return l;
       /*
        // Test:
      QFormLayout* l= new QFormLayout ();
      l->setSizeConstraint (QLayout::SetMinimumSize);
      l->setHorizontalSpacing (6+hsep/PIXEL);
      l->setVerticalSpacing (2+vsep/PIXEL);
      l->setRowWrapPolicy(QFormLayout::DontWrapRows);
      l->setFieldGrowthPolicy(QFormLayout::FieldsStayAtSizeHint);
      l->setFormAlignment(Qt::AlignHCenter | Qt::AlignTop);
      l->setLabelAlignment(Qt::AlignLeft);
      for (int i=0; i < N(lhs); i++) {
        QWidget* lw = concrete(lhs[i])->as_qwidget();
        QWidget* rw = concrete(rhs[i])->as_qwidget();
        if (lw && rw) l->addRow (lw, rw);
      }
      return l;
       */
    }
      
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
      
    case menu_separator: 
    {
      typedef bool T;
      T vertical = open_box<T> (load);
      QSizePolicy::Policy hpolicy = vertical ? QSizePolicy::Fixed 
                                             : QSizePolicy::Preferred;
      QSizePolicy::Policy vpolicy = vertical ? QSizePolicy::Preferred 
                                             : QSizePolicy::Fixed;
      return new QSpacerItem (1, 1, hpolicy, vpolicy);
    }
      
    case menu_group:
    {
      return NULL;
    }

    case pulldown_button:
    case pullright_button:
    case menu_button:
    case text_widget:
    case xpm_widget:
    case toggle_widget:
    case enum_widget:
    case choice_widget:
    case scrollable_widget:
    case hsplit_widget:
    case vsplit_widget:
    case tabs_widget:
    case wrapped_widget:
    case resize_widget:
    case refresh_widget:
    case balloon_widget:
    {
      QWidgetItem* wi = new QWidgetItem(this->as_qwidget());
      return wi;
    }
      break;

      // Used only for non-colored glue widgets (skips qt_glue_widget_rep)
    case glue_widget:
    {
      typedef quartet<bool, bool, SI, SI> T;
      T x = open_box<T>(load);

      /* This should be right, but actually breaks lots of stuff... ?!?
      QSizePolicy::Policy hpolicy = x.x1 ? QSizePolicy::Minimum 
                                         : QSizePolicy::Fixed;
      QSizePolicy::Policy vpolicy = x.x2 ? QSizePolicy::Minimum
                                         : QSizePolicy::Fixed;
       */
      QSizePolicy::Policy hpolicy = x.x1 ? QSizePolicy::Expanding 
                                         : QSizePolicy::Minimum;
      QSizePolicy::Policy vpolicy = x.x2 ? QSizePolicy::Expanding
                                         : QSizePolicy::Minimum;
      
      return new QSpacerItem (x.x3, x.x4, hpolicy, vpolicy);
    }
      break;
    default:
      ;
  }
  
  return NULL;
}

/*
 Because our policy is that the returned QWidget is owned by the caller, we
 must be careful and any QObjects we construct here must have the returned 
 QWidget as parent.
*/
QWidget *
qt_ui_element_rep::as_qwidget () {
  if (DEBUG_QT)
    cout << "as_qwidget: " << type_as_string() << LF;

  switch (type) {
    case horizontal_menu:
    case vertical_menu:
    case horizontal_list:
    case vertical_list:
    case tile_menu: 
    case minibar_menu: 
    case aligned_widget: 
    {
        // note that the QLayout is the same object as the QLayoutItem 
        // so no need to free the layoutitem
      QLayout* l = this->as_qlayoutitem()->layout();
      QWidget *w = new QWidget();
      if (l)
        w->setLayout(l);
      else if (DEBUG_QT)
          // should we create a default layout?
        cout << "qt_ui_element_rep::as_qwidget() : invalid situation" << LF;
      qwid = w;
    }
      break;
      
    case resize_widget:
    {
      typedef triple <string, string, string> T1;
      typedef quartet <widget, int, T1, T1 > T;
      T x = open_box<T>(load);

      qt_widget wid = concrete(x.x1);
      QString sheet = to_qstylesheet(x.x2);
      T1         y1 = x.x3;
      T1         y2 = x.x4;
      
      qwid = wid->as_qwidget();
      qwid->setStyleSheet(sheet);
      
      QSize minSize = qt_decode_length(y1.x1, y2.x1, qwid->minimumSizeHint(), qwid->fontMetrics());
      QSize defSize = qt_decode_length(y1.x2, y2.x2, qwid->minimumSizeHint(), qwid->fontMetrics());
      QSize maxSize = qt_decode_length(y1.x3, y2.x3, qwid->minimumSizeHint(), qwid->fontMetrics());

      if (minSize == defSize && defSize == maxSize) {        
        qwid->setFixedSize(defSize);        
        qwid->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
      } else {
        qwid->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Ignored);
        qwid->setMinimumSize(minSize);
        qwid->setMaximumSize(maxSize);
        qwid->resize(defSize);
      }
    }
      break;
      
    case menu_separator: 
    case menu_group:
    case glue_widget:
    {
      qwid = new QWidget();
    }
      break;
      
    case pulldown_button:
    case pullright_button:
    {
      typedef promise<widget> T1;
      typedef pair<widget, T1> T;
      T      x = open_box<T>(load);
      widget w = x.x1;
      T1    pw = x.x2;
      
      // a button w with a lazy pulldown menu pw
      
      QAction* a= concrete(this)->as_qaction ();
      QToolButton *b = new QTMUIButton();
      a->setParent(b);
      b->setDefaultAction(a);
      qwid = b;
    }
      break;
      
        // a command button with an optional prefix (o, * or v) and (sometimes) 
        // keyboard shortcut
    case menu_button:
    {
      typedef quintuple<widget, command, string, string, int> T;
      T x = open_box<T>(load);
      widget    w = x.x1; // the button's contents: xpm_widget, text_widget, ...?
      command cmd = x.x2;
      string  pre = x.x3;
      string   ks = x.x4;
      int   style = x.x5;
      
      QWidget* qw = concrete(w)->as_qwidget();  // will be discarded at the end

      if (concrete(w)->type == xpm_widget) {  // Toolbar button
        QAction*     a = as_qaction();        // Create key shortcuts and actions
        QTMUIButton* b = new QTMUIButton();
        b->setDefaultAction(a);
        a->setParent(b);
        qwid = b;
      } else { // text_widget
        QPushButton*     b = new QPushButton();
        QTMCommand* qtmcmd = new QTMCommand(b, cmd);
        QObject::connect (b, SIGNAL (clicked ()), qtmcmd, SLOT (apply ()));
        if (qw && concrete(w)->type == text_widget)
          b->setText(static_cast<QLabel*>(qw)->text());
        b->setEnabled (! (style & WIDGET_STYLE_INERT));
        b->setFlat (! (style & WIDGET_STYLE_BUTTON));
        qwid = b;
      }

      delete qw;
    }
      break;
      
      // given a button widget w, specify a help balloon which should be displayed
      // when the user leaves the mouse pointer on the button for a small while
    case balloon_widget:
    {
      typedef pair<widget, widget> T;
      T         x = open_box<T>(load);
      widget text = x.x1;
      widget help = x.x2;

      QWidget* w= concrete(text)->as_qwidget();
      if (w) {
        typedef quartet<string, int, color, bool> T1;
        T1 x = open_box<T1>(static_cast<qt_ui_element_rep*>(help.rep)->load);
        string str = x.x1;
        w->setToolTip (to_qstring (str));
      }
      qwid = w;
    }
      break;
      
      // a text widget with a given color and transparency
    case text_widget:
    {
      typedef quartet<string, int, color, bool> T;
      T           x = open_box<T>(load);
      string    str = x.x1;
      QString style = to_qstylesheet(x.x2, x.x3);

      QLabel* w = new QLabel();
#if 0
      //FIXME: implement refresh when changing language
      QTMAction* a= new QTMAction (NULL);
      //a->str = str;
#endif
      string t= tm_var_encode (str);
      if (t == "Help") t= "Help ";
      //w->setTextFormat(Qt::RichText);
      w->setText(to_qstring (t));
      w->setStyleSheet(style);
      w->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
      // Workaround too small sizeHint() when the text has letters with descent:
      w->setMinimumHeight(w->fontMetrics().height());
      qwid = w;
    }
      break;
      
      // a widget with an X pixmap icon
    case xpm_widget:
    {
      url image = open_box<url>(load);
      QLabel* l= new QLabel (NULL);
      QPixmap* img= the_qt_renderer () -> xpm_image (image);
      QIcon icon (*img);
      l->setPixmap (*img);
      qwid = l;
    }
      break;
      
    case toggle_widget:
    { 
      typedef triple<command, bool, int > T;
      T           x = open_box<T>(load);
      command   cmd = x.x1;
      bool    check = x.x2;
      QString style = to_qstylesheet(x.x3);
      
      QCheckBox* w  = new QCheckBox (NULL);  
      w->setCheckState(check ? Qt::Checked : Qt::Unchecked);
      w->setStyleSheet(style);
      
      command tcmd = tm_new<qt_toggle_command_rep> (w, cmd);
      QTMCommand* c = new QTMCommand (w, tcmd);
      QObject::connect (w, SIGNAL (stateChanged(int)), c, SLOT (apply()));

      qwid = w;
    }
      break;
      
    case enum_widget:
    {
      typedef quintuple<command, array<string>, string, int, string> T;
      T                x = open_box<T>(load);
      command        cmd = x.x1;
      QStringList values = to_qstringlist(x.x2);
      QString      value = to_qstring(x.x3);
      QString      style = to_qstylesheet(x.x4);
            
      QTMComboBox* w = new QTMComboBox(NULL);
      w->setEditable(value.isEmpty() || values.last().isEmpty());  // weird convention?!
      if (values.last().isEmpty())
        values.removeLast();
      
      w->addItemsAndResize(values, x.x5, "");
      int index = w->findText(value, Qt::MatchFixedString | Qt::MatchCaseSensitive);
      if (index != -1)
        w->setCurrentIndex(index);
   
      w->setStyleSheet(style);
      
      command ecmd = tm_new<qt_enum_command_rep> (w, cmd);
      QTMCommand* c = new QTMCommand (w, ecmd);
      // NOTE: with QueuedConnections, the slots are sometimes not invoked.
      QObject::connect (w, SIGNAL (currentIndexChanged(int)), c, SLOT (apply()));
      
      qwid = w;
    }
      break;
      
      // select one or multiple values from a list
    case choice_widget:
    {
      typedef quartet<command, array<string>, array<string>, bool > T;
      T                x = open_box<T>(load);
      command        cmd = x.x1;
      QStringList  items = to_qstringlist(x.x2);
      QStringList chosen = to_qstringlist(x.x3);
      bool  multiple_sel = x.x4;
      
      QListWidget* w = new QListWidget();
      w->addItems(items);

      if (multiple_sel)
        w->setSelectionMode(QAbstractItemView::ExtendedSelection);  // Support CTRL and SHIFT multiple selections.
      else
        w->setSelectionMode(QAbstractItemView::SingleSelection);
      
      for (int i = 0; i < items.size(); ++i) {
        QListWidgetItem* item = w->item(i);
        item->setSelected(chosen.contains(item->text(), Qt::CaseSensitive));  // Qt::CaseSensitive is the default anyway
      }
      
      w->setMinimumWidth(w->sizeHintForColumn(0));
      w->setMinimumHeight(w->sizeHintForRow(0)*items.count());
      w->setSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding);
      w->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
      w->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
      w->setFrameStyle(QFrame::NoFrame);
        //w->setFocusPolicy(Qt::NoFocus);
      
      command ecmd = tm_new<qt_choice_command_rep> (w, cmd, multiple_sel);
      QTMCommand* qcmd = new QTMCommand (w, ecmd);
      QObject::connect (w, SIGNAL (itemSelectionChanged()), qcmd, SLOT (apply()));
      
      qwid = w;      
    }
      break;
      
    case scrollable_widget:
    {
      typedef pair<widget, int> T;
      T           x = open_box<T>(load);
      qt_widget wid = concrete(x.x1);
      QString style = to_qstylesheet(x.x2);
            
      QScrollArea* scroll = new QScrollArea();
      scroll->setStyleSheet(style);
      scroll->setWidget(wid->as_qwidget());
      scroll->setWidgetResizable(true);
      
        // FIXME????
        // "Note that You must add the layout of widget before you call this function; 
        //  if you add it later, the widget will not be visible - regardless of when you show() the scroll area.
        //  In this case, you can also not show() the widget later."
      qwid = scroll;
    }
      break;
      
    case hsplit_widget:
    case vsplit_widget:
    {
      typedef pair<widget, widget> T;
      T          x = open_box<T>(load);
      qt_widget w1 = concrete(x.x1);
      qt_widget w2 = concrete(x.x2);
      
      QWidget* qw1 = w1->as_qwidget();
      QWidget* qw2 = w2->as_qwidget();
      QSplitter* split = new QSplitter();
      split->setOrientation(type == hsplit_widget ? Qt::Horizontal 
                                                  : Qt::Vertical);
      split->addWidget(qw1);
      split->addWidget(qw2);
      
      qwid = split;
    }
      break;
      
    case tabs_widget:
    {
      typedef array<widget> T1;
      typedef pair<T1, T1> T;
      T       x = open_box<T>(load);
      T1   tabs = x.x1;
      T1 bodies = x.x2;
      
      QTMTabWidget* tw = new QTMTabWidget ();
      
      int i;
      for (i = 0; i < N(tabs); i++) {
        if (is_nil (tabs[i])) break;
        QWidget* prelabel = concrete (tabs[i])->as_qwidget();
        QLabel*     label = qobject_cast<QLabel*> (prelabel);
        QWidget*     body = concrete (bodies[i])->as_qwidget();
        tw->addTab(body, label ? label->text() : "");
        delete prelabel;
      }

      if (i>0) tw->resizeOthers(0);   // Force the automatic resizing

      qwid = tw;
    }
      break;
      
    case wrapped_widget:
    {
      typedef pair<widget, command> T;
      T         x = open_box<T>(load);
      widget    w = x.x1;
      command cmd = x.x2;
      
      QWidget* qw = concrete(w)->as_qwidget();
      QTMOnDestroyCommand* c = new QTMOnDestroyCommand (qw, cmd);
				// See QTMOnDestroyCommand for an explanation of why it exists
      QObject::connect (qw, SIGNAL (destroyed ()), c, SLOT (apply ()));
      qwid = qw;
    }
      break;
      
    case refresh_widget:
    {
      string tmwid = open_box<string> (load);
      qwid = new QTMRefreshWidget (tmwid);
    }
      break;
      
    default:
      qwid = NULL;
  }

  return qwid;
}
