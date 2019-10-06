
/******************************************************************************
* MODULE     : QTMMenuHelper.cpp
* DESCRIPTION: QT Texmacs menu helper classes
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "analyze.hpp"

#include "qt_gui.hpp"
#include "qt_utilities.hpp"
#include "qt_window_widget.hpp"
#include "qt_ui_element.hpp"    // qt_choice_command_rep
#include "qt_picture.hpp"       // xpm_image
#include "qt_tm_widget.hpp"     // tweak_iconbar_size
#include "QTMMenuHelper.hpp"
#include "QTMGuiHelper.hpp"
#include "QTMStyle.hpp"
#include "QTMApplication.hpp"
#include "QTMTreeModel.hpp"

#include <QToolTip>
#include <QCompleter>
#include <QKeyEvent>

/******************************************************************************
 * QTMCommand
 ******************************************************************************/

/*! Queues the object's command into the main queue. */
void 
QTMCommand::apply()  {
BEGIN_SLOT
  if (!is_nil (cmd)) {
    the_gui->process_command (cmd);
    if (DEBUG_QT) {
      debug_qt << "QTMCommand::apply() (delayed)\n";
      /* FIXME: this sometimes crashes:
         cmd->print(debug_qt);
         debug_qt << "\n";
      */
    }
  }
END_SLOT
}

/******************************************************************************
 * QTMAction
 ******************************************************************************/

QTMAction::QTMAction (QObject *parent) : QAction (parent) {
  QObject::connect (the_gui->gui_helper, SIGNAL (refresh()),
                    this,                  SLOT (doRefresh()));
  _timer = new QTimer (this);
  QObject::connect (_timer, SIGNAL (timeout()),
                    this,     SLOT (doShowToolTip()));
}

QTMAction::~QTMAction() { 
  if (menu() && !menu()->parent()) delete menu();
}

void
QTMAction::set_text (string s) {
  if (N(s)) {
      // FIXME: this will only work if the system language is English!
    if (s == "Help" || s == "Edit" || s == "View" ||
        s == "Preferences...")
      s = s * " ";
    s= replace (s, "&", "&&");
    str = s;
    setText (to_qstring (s));
  }
}

void 
QTMAction::doRefresh() {
BEGIN_SLOT
  set_text (str);
END_SLOT
}

void
QTMAction::showToolTip() {
BEGIN_SLOT
  _timer->start (500);   // Restarts the timer if already running
  _pos = QCursor::pos();
END_SLOT
}

/*
 This is the best I could come up with: under MacOSX menu items receive no
 mouse events, nor are they QWidgets whose geometry we can query. As far as I
 know, it is not possible to know whether the menu item currently under the
 cursor is this particular one, so in order to avoid displaying outdated
 toolTips (because the user moved fast over items) we compute distances.
 This is obviously wrong, and will behave weirdly under certain resolutions,
 for given menu item sizes, etc. Also, one typically moves for a while 
 horizontally over the first item in an extensible menu, so once the user
 stops, the distance is bigger than the given constant and no tooltip is
 displayed.
 */

void
QTMAction::doShowToolTip() {
BEGIN_SLOT
  static int step = QApplication::font().pointSize();
  _timer->stop();
  if ((QCursor::pos() - _pos).manhattanLength() < step)  // Hideous HACK
    QToolTip::showText (QCursor::pos(), toolTip());
  else
    QToolTip::hideText();
END_SLOT
}

/******************************************************************************
 * QTMWidgetAction
 ******************************************************************************/


QTMWidgetAction::QTMWidgetAction (widget _wid, QObject *parent)
: QWidgetAction (parent), wid (_wid) {
  QObject::connect (the_gui->gui_helper, SIGNAL (refresh()),
                    this,                  SLOT (doRefresh()));
}

QWidget *
QTMWidgetAction::createWidget (QWidget * parent) {
  QWidget* qw = concrete (wid)->as_qwidget();
  qw->setParent (parent);
  return qw;
}


/******************************************************************************
 * QTMTileAction
 ******************************************************************************/

QTMTileAction::QTMTileAction (array<widget>& arr, int _cols, QObject* parent)
: QWidgetAction (parent), cols (_cols) {
  actions.reserve (N (arr));
  for (int i = 0; i < N (arr); i++) {
    if (is_nil (arr[i])) break;
    QAction* act = concrete (arr[i])->as_qaction();
    act->setParent (this);
    actions.append (act);
  };
}

QWidget*
QTMTileAction::createWidget (QWidget* parent)
{
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "QTMTileAction::createWidget\n";
  QWidget* wid= new QTMMenuWidget (parent);
  QGridLayout* l= new QGridLayout (wid);
    // wid->setAutoFillBackground (true);
    // wid->setBackgroundRole (QPalette::Base);
  wid->setLayout (l);
  l->setSizeConstraint (QLayout::SetFixedSize);
  l->setHorizontalSpacing (2);
  l->setVerticalSpacing (2);
  l->setContentsMargins (4, 0, 4, 0);
  int row = 0, col = 0;
  for (int    i = 0; i < actions.count(); i++) {
    QAction* sa = actions[i];
    QToolButton* tb= new QTMMenuButton (wid);
    tb->setDefaultAction (sa);
    QObject::connect (tb, SIGNAL (released()), this, SLOT (trigger()));
      //  tb->setStyle (qtmstyle ());
    l->addWidget (tb, row, col);
    col++;
    if (col >= cols) { col = 0; row++; }
  }
  return wid;
}


/******************************************************************************
 * QTMMinibarAction
 ******************************************************************************/

QTMMinibarAction::QTMMinibarAction (array<widget>& arr, QObject* parent)
: QWidgetAction (parent)
{
  actions.reserve (N (arr));
  for (int i = 0; i < N (arr); i++) {
    if (is_nil (arr[i])) break;
    QAction* act = concrete (arr[i])->as_qaction();
    act->setParent (this);
    actions.append (act);
  };
}

QWidget*
QTMMinibarAction::createWidget (QWidget* parent) {
  static QImage* pxm = xpm_image ("tm_add.xpm"); // See qt_tm_widget.cpp 
  QSize sz = pxm ? pxm->size() : QSize (16, 16);
  qt_tm_widget_rep::tweak_iconbar_size (sz);
  
  if (DEBUG_QT_WIDGETS) debug_widgets << "QTMMinibarAction::createWidget\n";
  QWidget* wid= new QWidget (parent);
  QBoxLayout* l= new QBoxLayout (QBoxLayout::LeftToRight, wid);
  wid->setLayout (l);
    //  l->setSizeConstraint (QLayout::SetFixedSize);
  l->setContentsMargins (0, 0, 0, 0);
  l->setSpacing (0);
  for (int i=0; i < actions.count(); i++) {
    QAction* sa= actions[i];
    if (QWidgetAction * wa = qobject_cast<QWidgetAction*> (sa)) {
      QWidget *w = wa->requestWidget (wid);
      l->addWidget(w);
    } else if (sa->text().isNull() && sa->icon().isNull()) {
      l->addSpacing(8);
    } else {
      QToolButton *tb = new QToolButton (wid);
      
        //HACK: texmacs does not use the checked state of the action
        // if the action is checkable then it means that it should be checked
      sa->setChecked (sa->isCheckable());
      
      tb->setDefaultAction (sa);
      tb->setAutoRaise (true);
      tb->setPopupMode (QToolButton::InstantPopup);
      tb->setStyle (qtmstyle());
      tb->setIconSize (sz);
      QFont f = tb->font();
      int fs = as_int (get_preference ("gui:mini-fontsize", QTM_MINI_FONTSIZE));
      f.setPointSize (qt_zoom (fs > 0 ? fs : QTM_MINI_FONTSIZE));
      tb->setFont(f);
      l->addWidget (tb);
    }
  }
  return wid;
}

/******************************************************************************
 * QTMMenuButton
 ******************************************************************************/

QTMMenuButton::QTMMenuButton (QWidget* parent) : QToolButton (parent) {
  setAttribute (Qt::WA_Hover);
}

void
QTMMenuButton::mousePressEvent (QMouseEvent* e) {
    // this one triggers the action and toggles the button
  QToolButton::mousePressEvent (e);
    // this one forwards the event to the parent
    // (which eventually is the menu)
  QWidget::mousePressEvent (e);
}

void
QTMMenuButton::mouseReleaseEvent (QMouseEvent* e) {
    // this one triggers the action and untoggles the button
  QToolButton::mouseReleaseEvent (e);
    // this one forwards the event to the parent
    // (which eventually is the menu which then closes itself)
  QWidget::mouseReleaseEvent (e);
}

void
QTMMenuButton::paintEvent (QPaintEvent* e) {
  (void) e;
  
    // initialize the options
  QStyleOptionToolButton opt;
  initStyleOption (&opt);

  QPainter p (this);
  QStyleOptionToolButton option;
  QRect r = rect();
  option.rect = r;
  option.state = QStyle::State_Enabled | (opt.state & QStyle::State_MouseOver
                                          ? QStyle::State_Selected
                                          : QStyle::State_None);
    // draw the control background as a menu item
  style()->drawControl (QStyle::CE_MenuItem, &option, &p, this);
    // draw the icon with a bit of inset.
  r.adjust (2, 2, -2, -2);
  defaultAction()->icon().paint (&p, r);
}

/******************************************************************************
 * QTMMenuWidget
 ******************************************************************************/

QTMMenuWidget::QTMMenuWidget (QWidget* parent) : QWidget (parent) {
}

void
QTMMenuWidget::paintEvent(QPaintEvent* e) {
  QPainter p (this);
  QStyleOptionMenuItem option;
  option.rect = rect();
  style()->drawControl (QStyle::CE_MenuEmptyArea, &option, &p, this);
  QWidget::paintEvent (e);
}

/******************************************************************************
 * QTMLazyMenu
 ******************************************************************************/

QTMLazyMenu::QTMLazyMenu (promise<widget> _pm, QWidget* p, bool right)
: QMenu (p), promise_widget (_pm), show_right (right) {
  QObject::connect (this, SIGNAL (aboutToShow ()), this, SLOT (force ()));
}

void
QTMLazyMenu::showEvent (QShowEvent* e)
{
  if (show_right && parentWidget()) {
    QPoint p = pos();
    p.rx() += parentWidget()->width();
    p.ry() -= parentWidget()->height();
    move (p);
  }
  QMenu::showEvent (e);
}

/*! Sets the QTMLazyMenu as the menu for the QAction and makes its destruction
 depend on that of the latter. */
void
QTMLazyMenu::attachTo (QAction* a) {
  QObject::connect (a,  SIGNAL (destroyed (QObject*)),
                    this, SLOT (destroy (QObject*)));
  a->setMenu (this);
}

void
QTMLazyMenu::transferActions (QList<QAction*>* from) {
  if (from == NULL) return;
  QList<QAction*> list = actions();
  while (!list.isEmpty()) {
    QAction* a = list.takeFirst();
    removeAction (a);
  }
  while (!from->isEmpty()) {
    QAction* a = from->takeFirst();
    addAction (a);
  }
}

void
QTMLazyMenu::force () {
BEGIN_SLOT
  QList<QAction*>* list = concrete (promise_widget())->get_qactionlist();
  transferActions (list);
END_SLOT
}

void
QTMLazyMenu::destroy (QObject* obj) {
BEGIN_SLOT
  (void) obj;
  deleteLater();
END_SLOT
}

/******************************************************************************
 * QTMInputTextWidgetHelper
 ******************************************************************************/

QTMInputTextWidgetHelper::QTMInputTextWidgetHelper (qt_widget _wid)
: QObject (), p_wid (_wid) {
  QTMLineEdit* le = qobject_cast<QTMLineEdit*>(wid()->qwid);
  setParent(le);
  ASSERT (le != NULL, "QTMInputTextWidgetHelper: expecting valid QTMLineEdit");
  QObject::connect (le, SIGNAL (returnPressed ()), this, SLOT (commit ()));
  QObject::connect (le, SIGNAL (focusOut (Qt::FocusReason)),
                    this, SLOT (leave (Qt::FocusReason)));
}

/*! Executed when the enter key is pressed. */
void
QTMInputTextWidgetHelper::commit () {
BEGIN_SLOT
  if (sender() != wid()->qwid) return;
  wid()->commit(true);
END_SLOT
}

/*! Executed after commit of the input field (enter) and when losing focus */
void
QTMInputTextWidgetHelper::leave (Qt::FocusReason reason) {
BEGIN_SLOT
  if (sender() != wid()->qwid) return;
  wid()->commit((reason != Qt::OtherFocusReason &&
                 get_preference ("gui:line-input:autocommit") == "#t"));
END_SLOT
}

/******************************************************************************
 * QTMFieldWidgetHelper
 ******************************************************************************/

QTMFieldWidgetHelper::QTMFieldWidgetHelper (qt_widget _wid, QComboBox* cb)
: QObject (cb), wid (_wid), done (false) {
  ASSERT (cb != NULL, "QTMFieldWidgetHelper: expecting valid QComboBox");
  QObject::connect (cb, SIGNAL (editTextChanged (const QString&)),
                    this, SLOT (commit (const QString&)));
}
QTMFieldWidgetHelper::QTMFieldWidgetHelper (qt_widget _wid, QLineEdit* cb)
: QObject (cb), wid (_wid), done (false) {
  ASSERT (cb != NULL, "QTMFieldWidgetHelper: expecting valid QLineEdit");
  QObject::connect (cb, SIGNAL (textChanged (const QString&)),
                    this, SLOT (commit (const QString&)));
}

void
QTMFieldWidgetHelper::commit (const QString& qst) {
BEGIN_SLOT
  static_cast<qt_field_widget_rep*> (wid.rep)->input =
      scm_quote (from_qstring (qst));
END_SLOT
}

/******************************************************************************
 * QTMLineEdit
 ******************************************************************************/

QTMLineEdit::QTMLineEdit (QWidget* parent, string _type, string _ww,
                          int style, command _cmd)
  : QLineEdit (parent), completing (false),
    type ("default"), name ("default"), serial ("default"),
    ww (_ww), cmd (_cmd), last_key (0) {
  set_type (_type);
  if (type == "password") setEchoMode(QLineEdit::Password);
  if (style & WIDGET_STYLE_MINI) {
    setStyle (qtmstyle());
      // FIXME: we should remove this and let the scheme code decide.
    QPalette pal (palette());
    pal.setColor (QPalette::Base, QColor (252, 252, 248));
    pal.setColor (QPalette::WindowText, Qt::black);
    setPalette (pal);
  }
  
    // just to be sure we don't capture the wrong keys in keyPressEvent
  setCompleter (0);

  qt_apply_tm_style (this, style);
}

void
QTMLineEdit::set_type (string t) {
  int i= search_forwards (":", 0, t);
  if (i >= 0) {
    type= t (i+1, N(t));
    name= t (0, i);
    int j= search_forwards ("#", 0, name);
    if (j >= 0) {
      serial= name (j+1, N(name));
      name  = name (0, j);
    }
  }
  else type= t;
}

bool
QTMLineEdit::continuous () {
  return
    starts (type, "search") ||
    starts (type, "replace-") ||
    starts (type, "spell") ||
    starts (serial, "form-");
}

/*
 We need to reimplement the main event handler because we need the tab key for
 completions. (See Qt docs for QWidget::event())
 */
bool
QTMLineEdit::event (QEvent* ev) {
  if (ev->type() == QEvent::KeyPress)  // Handle ALL keys
    keyPressEvent (static_cast<QKeyEvent*> (ev));
  else
    return QWidget::event (ev);
  return true;
}

extern hashmap<int,string> qtkeymap;
void initkeymap ();

/*
 FIXME: This is a hideous mess...
 */
void
QTMLineEdit::keyPressEvent (QKeyEvent* ev)
{
  QCompleter* c = completer();
  
  last_key = (ev->key() == Qt::Key_Tab && ev->modifiers() & Qt::ShiftModifier)
            ? Qt::Key_Backtab
            : ev->key();

  if (continuous ()) {
    initkeymap ();
    if ((last_key != Qt::Key_Tab || type == "replace-what") &&
        (last_key != Qt::Key_Backtab || type == "replace-by") &&
        last_key != Qt::Key_Down &&
        last_key != Qt::Key_Up &&        
        last_key != Qt::Key_Enter &&
        last_key != Qt::Key_Return &&
        last_key != Qt::Key_Escape &&
        (!starts (type, "spell") || last_key < 49 || last_key >= 58) &&
        (!starts (type, "spell") || last_key != 43) &&
        (ev->modifiers() & Qt::ControlModifier) == 0 &&
        (ev->modifiers() & Qt::MetaModifier) == 0)
      QLineEdit::keyPressEvent (ev);
    string key= "none";
    string s  = from_qstring (text());
    if (last_key >= 32 && last_key <= 126) {
      key= string ((char) last_key);
      if (is_upcase (key[0]))
        if ((ev->modifiers() & Qt::ShiftModifier) == 0)
          key[0]= (int) (key[0] + ((int) 'a') - ((int) 'A'));
    }
    if (qtkeymap->contains (last_key)) key= qtkeymap[last_key];
    if ((ev->modifiers() & Qt::ShiftModifier) && N(key) > 1) key= "S-" * key;
#ifdef Q_WS_MAC
    if (ev->modifiers() & Qt::ControlModifier) key= "C-" * key;
    if (ev->modifiers() & Qt::AltModifier) key= "none";
    if (ev->modifiers() & Qt::MetaModifier) key= "M-" * key;
#else
    if (ev->modifiers() & Qt::ControlModifier) key= "M-" * key;
    if (ev->modifiers() & Qt::AltModifier) key= "A-" * key;
    if (ev->modifiers() & Qt::MetaModifier) key= "C-" * key;
#endif
    cmd (list_object (list_object (object (s), object (key))));
    return;
  }
  else if (c) {
    int row = 0;
    switch (last_key) {
      case Qt::Key_Down:
        completing = true;
        setCursorPosition (0);
        c->complete();
      case Qt::Key_Tab:
      {
//        cout << "Completing= " << completing << LF;
//        cout << "hasSelectedText= " << hasSelectedText() << LF;
//        cout << "CursorPosition= " << cursorPosition() << LF;
//        cout << "SelectionStart= " << selectionStart() << LF;
        
        if (completing) {
//          cout << "CompletionCount= " << c->completionCount() << LF;
          if (c->completionCount() > 1) {
            if (! c->setCurrentRow (c->currentRow() + 1))
              c->setCurrentRow (0);    // cycle
          } else {
            completing = false;
            setCursorPosition (text().length());
            c->setCompletionPrefix ("");
              //c->popup()->hide();
          }
          if (hasSelectedText())
            setCursorPosition (selectionStart());
          if (c->currentCompletion() != "") {
            int pos = cursorPosition();
            setText (c->currentCompletion ());
            setSelection (pos, text().length());
          } else {
            completing = false;
            setSelection (0, text(). length());
            c->setCompletionPrefix ("");
          }
        } else {
          QString prefix;
          if (hasSelectedText())
            prefix = text().left (selectionStart());
          else
            prefix = text().left (cursorPosition());
          c->setCompletionPrefix (prefix);
//          cout << "prefix= " << from_qstring (prefix) << LF;
//          cout << "CompletionCount= " << c->completionCount() << LF;
          
            // If there are no completions, go to the end of the line or
            // send the key up for tab navigation
          if (c->completionCount() == 0 ||
              (c->completionCount() == 1 && c->currentCompletion() == text())) {
            if (c->popup() && c->popup()->isVisible()) {
              setCursorPosition (text().length());
              c->popup()->hide();
            } else if (cursorPosition() == text().length()) {
              QLineEdit::keyPressEvent (ev);
            } else {
              setCursorPosition (text().length());
            }
            return;
          }

          completing = true;
            // hack: advance one completion (needed after tab navigation)
          if (c->currentCompletion() == text()) {
            clear();
            if (! c->setCurrentRow (c->currentRow() + 1))
              c->setCurrentRow (0);    // cycle
          }
          c->complete();
        }
        ev->accept();
      }
        return;
          // This is different on purpose: when "back-completing" suggested text
          // we want to display the previous entry to the one suggested
      case Qt::Key_Up:
        completing = true;
        setCursorPosition (0);
        c->complete();
      case Qt::Key_Backtab:
      {
        if (!completing) {
          if (hasSelectedText())
            c->setCompletionPrefix (text().left (selectionStart()));
          else
            c->setCompletionPrefix (text().left (cursorPosition()));
            // If there are no completions, go to the end of the line or
            // send the key up for tab navigation
          if (c->completionCount() == 0 ||
              (c->completionCount() == 1 && c->currentCompletion() == text())) {
            if (c->popup() && c->popup()->isVisible()) {
              setCursorPosition (text().length());
              c->popup()->hide();
            } else if (cursorPosition() == text().length()) {
              QLineEdit::keyPressEvent (ev);
            } else {
              setCursorPosition (text().length());
            }
            return;
          }
          completing = true;
          c->complete();
        } else {
          row = c->currentRow();
          if (! c->setCurrentRow (row - 1))
            c->setCurrentRow (c->completionCount() - 1);    // cycle
          if (c->currentCompletion() != "") {
            int pos;
            if (hasSelectedText())
              pos = selectionStart();
            else
              pos = cursorPosition();
            setText (c->currentCompletion ());
            setSelection (pos, text().length());
          }
        }
        ev->accept();
      }
        return;
      case Qt::Key_Enter:
      case Qt::Key_Return:
        if (c->popup() && c->popup()->isVisible()) {
          setCursorPosition (text().length());
          c->popup()->hide();
        } else if (completing) {
          completing = false;
          setText (c->currentCompletion());
          setCursorPosition (text().length());
          c->setCompletionPrefix ("");
        } else {
          completing = false;
          c->setCompletionPrefix ("");
          QLineEdit::keyPressEvent (ev);
          return;
        }
        ev->accept();
        return;
      case Qt::Key_Escape:
        if (completing && c->completionMode() == QCompleter::PopupCompletion) {
          if (c->popup()) c->popup()->hide();
          completing = false;
        } else {
          emit editingFinished();
          ev->accept();
          if (parentWidget())        // HACK to return focus to the main editor widget
            parentWidget()->setFocus ();
        }
        c->setCompletionPrefix ("");
        return;
      default:
        completing = false;
        c->setCompletionPrefix ("");
        QLineEdit::keyPressEvent (ev);
        return;
    }
  } else {
    QLineEdit::keyPressEvent (ev);
  }
}

QSize
QTMLineEdit::sizeHint () const {
  return qt_decode_length (ww, "", QLineEdit::sizeHint(), fontMetrics());
}

void 
QTMLineEdit::focusInEvent (QFocusEvent* ev)
{
  setCursorPosition (text().size());
    // selectAll ();
  QLineEdit::focusInEvent (ev);
}

void
QTMLineEdit::focusOutEvent (QFocusEvent* ev)
{
  if (!continuous ()) {
    Qt::FocusReason reason =
      (last_key != Qt::Key_Escape) ? ev->reason() : Qt::OtherFocusReason;
    emit focusOut (reason);
  }
  QLineEdit::focusOutEvent (ev);
}


/******************************************************************************
 * QTMTabWidget
 ******************************************************************************/

QTMTabWidget::QTMTabWidget (QWidget *p) : QTabWidget(p) {
  QObject::connect (this, SIGNAL (currentChanged (int)), this, SLOT (resizeOthers (int)));
}

/*! Resizes the widget to the size of the tab given by the index.
 
 In particular, we must tell all parent widgets to adjustSize() as well as
 possibly resize the window: qt_window_widget_rep's constructor sets a fixed
 size for windows which do not contain variable size resize_widgets. In this 
 case we must update the fixed size to reflect the change of tab.
 */
void
QTMTabWidget::resizeOthers (int current) {
BEGIN_SLOT
  for (int i = 0; i < count(); ++i) {
    if (i != current)
      widget(i)->setSizePolicy (QSizePolicy::Ignored, QSizePolicy::Ignored);
    else
      widget(i)->setSizePolicy (QSizePolicy::Minimum, QSizePolicy::Minimum);
  }
  
    // FIXME? this could loop indefinitely if parents are cyclic.
  QWidget* p = this;
  while (p != window()) {
    p->adjustSize();
    p = p->parentWidget();
  }
  p->adjustSize();

  if (window()->minimumSize()!=QSize (0,0) && 
      window()->maximumSize() != QSize (QWIDGETSIZE_MAX, QWIDGETSIZE_MAX))
    window()->setFixedSize (window()->sizeHint());
END_SLOT
}

/******************************************************************************
 * QTMRefreshWidget
 ******************************************************************************/

widget make_menu_widget (object wid);
extern bool menu_caching;

QTMRefreshWidget::QTMRefreshWidget (qt_widget _tmwid, string _strwid, string _kind)
: QWidget (), strwid (_strwid), kind (_kind),
  curobj (false), cur (), tmwid (_tmwid), qwid (NULL), cache (widget ())
{   
  QObject::connect (the_gui->gui_helper, SIGNAL (tmSlotRefresh (string)),
                   this, SLOT (doRefresh (string)));
  QVBoxLayout* l = new QVBoxLayout (this);
  l->setContentsMargins (0, 0, 0, 0);
  l->setMargin (0);
  setLayout (l);
  
  doRefresh ("init");
}

bool
QTMRefreshWidget::recompute (string what) {
  if (what != "init" && kind != "any" && kind != what) return false;
  string s = "'(vertical (link " * strwid * "))";
  eval ("(lazy-initialize-force)");
  object xwid = call ("menu-expand", eval (s));
  
  if (cache->contains (xwid)) {
    if (curobj == xwid) return false;
    curobj = xwid;
    cur    = cache [xwid];
    return true;
  } else {
    curobj = xwid;
    object uwid = eval (s);
    cur = make_menu_widget (uwid);
    tmwid->add_child (cur); // FIXME?! Is this ok? what when we refresh?
    if (menu_caching) cache (xwid) = cur;
    return true;
  }
}

/*
void
QTMRefreshWidget::deleteLayout (QLayout* l) {
  if (!l)
    return;

  QLayoutItem* item;
  while ((item = l->takeAt(0)) != 0) {
    if (item->widget()) {
        //qDebug() << "Deleting widget: " << item->widget();
      l->removeWidget (item->widget());
      item->widget()->setParent (NULL);
      delete item->widget();
    }	else if (item->layout()) {
        //qDebug() << "Deleting layout: " << item->layout();
      item->layout()->setParent (NULL);
      deleteLayout (item->layout());
    }
  }

  delete l;
}
*/

void
QTMRefreshWidget::doRefresh (string kind) {
BEGIN_SLOT
  if (recompute (kind)) {
    if (qwid) qwid->setParent (NULL);
    delete qwid;
    qwid = concrete (cur)->as_qwidget();
    qwid->setParent (this);

    delete layout()->takeAt(0);
    layout()->addWidget (qwid);
    update();
    
      // Tell the window to fix its size to the new one if we had it fixed to
      // begin with (this is indicated by minimum and maximum sizes set to 
      // values other than the default)
    if (window()->minimumSize() != QSize (0,0) &&
        window()->maximumSize() != QSize (QWIDGETSIZE_MAX, QWIDGETSIZE_MAX))
      window()->setFixedSize (window()->sizeHint());  
  }
END_SLOT
}


/******************************************************************************
 * QTMRefreshableWidget
 ******************************************************************************/

QTMRefreshableWidget::QTMRefreshableWidget (qt_widget _tmwid, object _prom, string _kind)
: QWidget (), prom (_prom), kind (_kind),
  curobj (false), cur (), tmwid (_tmwid), qwid (NULL)
{   
  QObject::connect (the_gui->gui_helper, SIGNAL (tmSlotRefresh (string)),
                   this, SLOT (doRefresh (string)));
  QVBoxLayout* l = new QVBoxLayout (this);
  l->setContentsMargins (0, 0, 0, 0);
  l->setMargin (0);
  setLayout (l);
  
  doRefresh ("init");
}

bool
QTMRefreshableWidget::recompute (string what) {
  if (what != "init" && kind != "any" && kind != what) return false;
  eval ("(lazy-initialize-force)");
  object xwid = call (prom);
  if (curobj == xwid) return false;
  if (!is_widget (xwid)) return false;
  curobj= xwid;
  cur= as_widget (xwid);
  tmwid->add_child (cur); // FIXME?! Is this ok? what when we refresh?
  return true;
}

/*
void
QTMRefreshableWidget::deleteLayout (QLayout* l) {
  if (!l)
    return;

  QLayoutItem* item;
  while ((item = l->takeAt(0)) != 0) {
    if (item->widget()) {
        //qDebug() << "Deleting widget: " << item->widget();
      l->removeWidget (item->widget());
      item->widget()->setParent (NULL);
      delete item->widget();
    }	else if (item->layout()) {
        //qDebug() << "Deleting layout: " << item->layout();
      item->layout()->setParent (NULL);
      deleteLayout (item->layout());
    }
  }

  delete l;
}
*/

void
QTMRefreshableWidget::doRefresh (string kind) {
BEGIN_SLOT
  if (recompute (kind)) {
    if (qwid) qwid->setParent (NULL);
    delete qwid;
    qwid = concrete (cur)->as_qwidget();
    qwid->setParent (this);

    delete layout()->takeAt(0);
    layout()->addWidget (qwid);
    update();
    
      // Tell the window to fix its size to the new one if we had it fixed to
      // begin with (this is indicated by minimum and maximum sizes set to 
      // values other than the default)
    if (window()->minimumSize() != QSize (0,0) &&
        window()->maximumSize() != QSize (QWIDGETSIZE_MAX, QWIDGETSIZE_MAX))
      window()->setFixedSize (window()->sizeHint());  
  }
END_SLOT
}


/******************************************************************************
 * QTMComboBox
 ******************************************************************************/

QTMComboBox::QTMComboBox (QWidget* parent) : QComboBox (parent) {
    ///// Obtain the minimum vertical size
  QComboBox cb;
  cb.setSizeAdjustPolicy (AdjustToContents);
  cb.addItem ("");
  minSize = cb.sizeHint();  // we'll just keep the height
  
    ///// Add width of the arrow button
  QStyleOptionComboBox opt;
  opt.initFrom (&cb);
  opt.activeSubControls = QStyle::SC_ComboBoxArrow;
  QRect r = style()->subControlRect (QStyle::CC_ComboBox, &opt,
                                     QStyle::SC_ComboBoxArrow, &cb);
  int max_w= (int) floor (40 * retina_scale);
  minSize.setWidth (min (r.width(), max_w));
}

/*! Add items and fix the ComboBox size using texmacs length units.
 
 Relative sizes are set based on the minimum bounding box in which any item of
 the list fits. Absolute sizes are set independently of the size of items in 
 the list.
 
 The QComboBox' minimum height is the original minimumSizeHint().
 */
void
QTMComboBox::addItemsAndResize (const QStringList& texts, string ww, string hh) {
  QComboBox::addItems (texts);
  
    ///// Calculate the minimal contents size:
  calcSize = QApplication::globalStrut ();
  const QFontMetrics& fm = fontMetrics ();
  
  for (int i = 0; i < count(); ++i) {
    QRect br = fm.boundingRect (itemText(i));
    calcSize.setWidth (qMax (calcSize.width(), br.width()));
    calcSize.setHeight (qMax (calcSize.height(), br.height()));
  }
  calcSize = qt_decode_length (ww, hh, calcSize, fm);
  
    ///// Add minimum constraints and fix size
  calcSize.setHeight (qMax (calcSize.height(), minSize.height()));
  calcSize.rwidth() += minSize.width();
  
  setFixedSize (calcSize);
}

/*
 We need to reimplement the main event handler because we need the tab key for
 completions. (See Qt docs for QWidget::event())
 */
bool
QTMComboBox::event (QEvent* ev) {
  if (ev->type() == QEvent::KeyPress && isEditable()) {       // Handle ALL keys
    QKeyEvent* k = static_cast<QKeyEvent*> (ev);
    if (k->key() == Qt::Key_Up || k->key() == Qt::Key_Down)
      showPopup();
    else if (k->key() != Qt::Key_Escape) // HACK: QTMLineEdit won't need this
      lineEdit()->event (ev);             // but we do.
    else
      return false;
  } else
    return QComboBox::event (ev);

  return true;
}


/******************************************************************************
 * QTMScrollArea
 ******************************************************************************/

/*! Sets the widget for the scrollarea and looks for QListViews.
 
 This is needed to correctly update the scrollbars when the user navigates with
 the keys through items in a QListView contained in the QTMScrollArea.
 It also scrolls the viewport to the position of selected items in QListWidgets.
 */
void
QTMScrollArea::setWidgetAndConnect (QWidget* w) {
  setWidget (w);
 
  listViews = w->findChildren<QTMListView*>();
  for (ListViewsIterator it = listViews.begin(); it != listViews.end(); ++it) {
    if (! (*it)->isScrollable())
      QObject::connect (*it, SIGNAL (selectionChanged (const QItemSelection&)),
                        this,  SLOT (scrollToSelection (const QItemSelection&)));
  }
}

/*! Scrolls the area to a given index in a QTMListView. */
void
QTMScrollArea::scrollToSelection (const QItemSelection& sel) {
BEGIN_SLOT
  if (sel.isEmpty())
    return;

  QTMListView* lw = qobject_cast<QTMListView*> (sender());
  if (lw) {
    QRect r = lw->visualRect (sel.indexes().last());
    QRect g = lw->geometry();
    int   x = r.x() + g.x();
    int   y = r.y() + g.y();
    
    if (! viewport()->geometry().contains (x, y))
      ensureVisible (x, y, r.width(), r.height());
  }
END_SLOT
}

/*! Work around a problem with scrolling before the widget is shown.
 
 Calling ensureVisible() before the widget is shown scrolls the viewport by an
 insufficient amount. See the comments to QTMScrollArea.
 */
void
QTMScrollArea::showEvent (QShowEvent* ev) {
  for (ListViewsIterator it = listViews.begin(); it != listViews.end(); ++it) {
    QItemSelection sel = (*it)->selectionModel()->selection();
    (*it)->selectionChanged (sel, sel);
  }
  QScrollArea::showEvent (ev);
}

/******************************************************************************
 * QTMListView
 ******************************************************************************/

QTMListView::QTMListView (const command& cmd,
                          const QStringList& strings,
                          const QStringList& selections,
                          bool multiple,
                          bool scroll,
                          bool filtered,
                          QWidget* parent)
: QListView (parent) {
  
  stringModel = new QStringListModel (strings, this);
  filterModel = new QSortFilterProxyModel (this);
  
  filterModel->setSourceModel (stringModel);
    //filterModel->setDynamicSortFilter (true);
  filterModel->setFilterCaseSensitivity (Qt::CaseSensitive);
  
  setModel (filterModel);
  
  setSelectionMode (multiple ? ExtendedSelection : SingleSelection);
  setEditTriggers (NoEditTriggers);

    // NOTE: using selectionModel()->select(item, QItemSelection::SelectCurrent)
    // doesn't update the selection but overwrites it, so we explicitly define
    // our QItemSelection and use merge()
  QItemSelection sel;
  for (int i = 0; i < model()->rowCount(); ++i) {
    QModelIndex item = model()->index (i, 0);
    if (selections.contains (model()->data (item, Qt::DisplayRole).toString(),
                             Qt::CaseSensitive))
      sel.merge (QItemSelection(item, item), QItemSelectionModel::Select);
  }
  selectionModel()->select (sel, QItemSelectionModel::Select);
  
  if (!scroll) {
    setMinimumWidth (sizeHintForColumn(0));
    setMinimumHeight (sizeHintForRow(0) * model()->rowCount());
    setHorizontalScrollBarPolicy (Qt::ScrollBarAlwaysOff);
    setVerticalScrollBarPolicy (Qt::ScrollBarAlwaysOff);
    setFrameStyle (QFrame::NoFrame);
  }

  setUniformItemSizes (true);
  setSizePolicy (QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding);

  command     ecmd = tm_new<qt_choice_command_rep> (this, cmd, multiple, filtered);
  QTMCommand* qcmd = new QTMCommand (this, ecmd);
  QObject::connect (selectionModel(),
                    SIGNAL (selectionChanged (const QItemSelection&, const QItemSelection&)),
                    qcmd,
                    SLOT (apply()));
}

/*! Reimplemented from QListView.
 
 We simply emit another signal, mainly to notify QTMScrollArea that we have a new
 selection for those cases were we don't have our own scrollbars.
 */
void
QTMListView::selectionChanged (const QItemSelection& c, const QItemSelection& p) {
BEGIN_SLOT
  QListView::selectionChanged (c, p);
  emit selectionChanged (c);
END_SLOT
}

/******************************************************************************
 * QTMTreeView
 ******************************************************************************/

QTMTreeView::QTMTreeView (command cmd, tree data, const tree& roles, QWidget* p)
: QTreeView (p), _t (data), _cmd (cmd) {
  setModel (QTMTreeModel::instance (_t, roles));
  setUniformRowHeights (true);  // assuming we display only text.
  setHeaderHidden (true);       // for now...
  QObject::connect (this, SIGNAL (pressed (const QModelIndex&)),
                    this,   SLOT (callOnChange (const QModelIndex&)));
}

void
QTMTreeView::currentChanged (const QModelIndex& curr, const QModelIndex& prev) {
  (void) prev;
  if (selectedIndexes().contains(curr))
    callOnChange (curr, false);
}

void
QTMTreeView::callOnChange (const QModelIndex& index, bool mouse) {
BEGIN_SLOT
  object arguments = mouse ? list_object ((int)QApplication::mouseButtons())
                           : list_object (-1);
    
    // docs state the index is valid, no need to check
  QVariant d = tmModel()->data (index, QTMTreeModel::CommandRole);
    // If there's no CommandRole, we return the subtree by default
  if (!d.isValid() || !d.canConvert (QVariant::String))
    arguments = cons (tmModel()->item_from_index (index), arguments);
  else
    arguments = cons (from_qstring (d.toString()), arguments);
  int cnt = QTMTreeModel::TMUserRole;
  d = tmModel()->data (index, cnt);
  while (d.isValid() && d.canConvert (QVariant::String)) {
    arguments = cons (from_qstring (d.toString()), arguments);
    d = tmModel()->data (index, ++cnt);
  }
  _cmd (arguments);
END_SLOT
}

inline QTMTreeModel*
QTMTreeView::tmModel() const {
  return static_cast<QTMTreeModel*> (model());
}

