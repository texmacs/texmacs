
/******************************************************************************
* MODULE     : QTMMenuHelper.cpp
* DESCRIPTION: QT Texmacs menu helper classes
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMMenuHelper.hpp"
#include "QTMGuiHelper.hpp"
#include "QTMStyle.hpp"
#include "qt_gui.hpp"
#include "qt_utilities.hpp"
#include "qt_window_widget.hpp"
#include "qt_ui_element.hpp"  // qt_choice_command_rep
#include "analyze.hpp"

#include <QtGui>

/******************************************************************************
 * QTMCommand
 ******************************************************************************/

/*! Queues the object's command into the main queue. */
void 
QTMCommand::apply()  {
  if (DEBUG_QT) 
    cout << "QTMCommand::apply() (delayed)\n";
  if (!is_nil(cmd)) { the_gui->process_command(cmd); }
}


/******************************************************************************
 * QTMAction
 ******************************************************************************/

QTMAction::QTMAction(QObject *parent) : QAction(parent) { 
  QObject::connect(the_gui->gui_helper, SIGNAL(refresh()), this, SLOT(doRefresh()));
  _timer = new QTimer(this);
  QObject::connect(_timer, SIGNAL(timeout()), this, SLOT(doShowToolTip()));
  
}

QTMAction::~QTMAction() { 
  if (menu() && !(menu()->parent())) delete menu(); 
}

void
QTMAction::set_text (string s) {
  if (N(s)) {
    if (s == "Help" || s == "Edit")
      s = s * " ";
    str = s;
    setText (to_qstring (s));
  }
}

void 
QTMAction::doRefresh() {
  set_text (str);
}

void
QTMAction::showToolTip()
{
  _timer->start(500);   // Restarts the timer if already running
  _pos = QCursor::pos();
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
  static int step = QApplication::font().pointSize();
  _timer->stop();
  if((QCursor::pos() - _pos).manhattanLength() < step)  // Hideous HACK
    QToolTip::showText(QCursor::pos(), toolTip());
  else
    QToolTip::hideText();
}


/******************************************************************************
 * QTMWidgetAction
 ******************************************************************************/


QTMWidgetAction::QTMWidgetAction (widget _wid, QObject *parent)
: QWidgetAction (parent), wid (_wid) {
  QObject::connect (the_gui->gui_helper, SIGNAL(refresh()), this, SLOT(doRefresh()));
}

QWidget *
QTMWidgetAction::createWidget (QWidget * parent) {
  QWidget* qw = concrete(wid)->as_qwidget();
  qw->setParent(parent);
  return qw;
}


/******************************************************************************
 * QTMTileAction
 ******************************************************************************/

QTMTileAction::QTMTileAction (QWidget* parent, array<widget>& arr, int _cols)
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

/*!
 FIXME: QTMTileAction::createWidget is called twice:
 the first time when the action is added to the menu,
 the second when from the menu it is transferred to the toolbar.
 This is weird since the first widget does not ever use
 the widget so it results in a waste of time.
 */
QWidget*
QTMTileAction::createWidget(QWidget* parent)
{
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


/******************************************************************************
 * QTMMinibarAction
 ******************************************************************************/

QTMMinibarAction::QTMMinibarAction (QWidget* parent, array<widget>& arr)
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

/*!
 FIXME: QTMMinibarAction::createWidget is called twice:
 the first time when the action is added to the menu, the second when from the
 menu it is transferred to the toolbar. This is weird since the first widget
 does not ever use the widget so it results in a waste of time.
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
        // if the action is checkable then it means that it should be checked
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
 * QTMLazyMenu
 ******************************************************************************/

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


/******************************************************************************
 * QTMInputTextWidgetHelper
 ******************************************************************************/

QTMInputTextWidgetHelper::QTMInputTextWidgetHelper (qt_input_text_widget_rep* _wid) 
: QObject(NULL), p_wid(abstract(_wid)), done(false) { }

/*! Destructor.
 Removes reference to the helper in the texmacs widget. If needed the texmacs
 widget is automatically deleted.
 */
QTMInputTextWidgetHelper::~QTMInputTextWidgetHelper() {
  wid()->helper = NULL;
}

void
QTMInputTextWidgetHelper::apply () {
  if (done) return;
  done = true;
  the_gui->process_command (wid()->cmd, wid()->ok
                            ? list_object (object (wid()->input))
                            : list_object (object (false)));
}

/*! Executed when the enter key is pressed. */
void
QTMInputTextWidgetHelper::commit () {
  QTMLineEdit* le = qobject_cast <QTMLineEdit*> (sender());
  if (!le) return;

  done         = false;
  wid()->ok    = true;
  wid()->input = from_qstring (le->text());

    // HACK: restore focus to the main editor widget
  widget_rep* win = qt_window_widget_rep::widget_from_qwidget (le);
  if (win && concrete(win)->type == qt_widget_rep::texmacs_widget)
    concrete(get_canvas(win))->qwid->setFocus();

  if (win) apply();    // This is 0 inside a dialog => no command
}

/*! Executed after commit of the input field (enter) and when losing focus */
void
QTMInputTextWidgetHelper::leave () {
  QTMLineEdit* le = qobject_cast <QTMLineEdit*> (sender());
  if (!le) return;

  if (get_preference ("gui:line-input:autocommit") == "#t") {
    done         = false;
    wid()->ok    = true;
    wid()->input = from_qstring (le->text());
  } else {
    le->setText (to_qstring (wid()->input));
  }

  widget_rep* win = qt_window_widget_rep::widget_from_qwidget(le);
  if (win) apply();    // This is 0 inside a dialog => no command
}

void
QTMInputTextWidgetHelper::remove (QObject* obj) {
  views.removeAll (qobject_cast<QTMLineEdit*> (obj));
  if (views.count () == 0)
    deleteLater();
}

void
QTMInputTextWidgetHelper::add (QObject* obj) {
  QTMLineEdit* le = qobject_cast<QTMLineEdit*> (obj);
  if (le && !views.contains (le)) {
    QObject::connect (le, SIGNAL (destroyed (QObject*)), this, SLOT (remove (QObject*)));
    QObject::connect (le, SIGNAL (returnPressed ()),     this, SLOT (commit ()));
    QObject::connect (le, SIGNAL (focusOut ()),          this, SLOT (leave ()));
    views << le;
  }
}


/******************************************************************************
 * QTMFieldWidgetHelper
 ******************************************************************************/

QTMFieldWidgetHelper::QTMFieldWidgetHelper (qt_field_widget _wid) 
  : QObject(NULL), wid(_wid), done(false) { }

/*! Destructor.
 Removes reference to the helper in the texmacs widget. Deletion of the texmacs
 widget is automagic.
 */
QTMFieldWidgetHelper::~QTMFieldWidgetHelper() {
  wid->helper = NULL;
}

void
QTMFieldWidgetHelper::commit (const QString& qst) {
  wid->input = scm_quote (from_qstring (qst));
}

void
QTMFieldWidgetHelper::remove (QObject* obj) {
  views.removeAll (qobject_cast<QTMComboBox*> (obj));
  if (views.count () == 0)
    deleteLater();
}

void
QTMFieldWidgetHelper::add (QObject* obj) {
  QComboBox* cb = qobject_cast<QComboBox*> (obj);
  if (obj && !views.contains (cb)) {
    QObject::connect (cb, SIGNAL (destroyed (QObject*)), this, SLOT (remove (QObject*)));
    QObject::connect (cb, SIGNAL (editTextChanged (const QString&)), this, SLOT (commit (const QString&)));
    views << cb;
  }
}


/******************************************************************************
 * QTMLineEdit
 ******************************************************************************/

QTMLineEdit::QTMLineEdit (QWidget* parent, string _ww, int style)
: QLineEdit (parent), completing (false), ww (_ww) {
  if (style & WIDGET_STYLE_MINI) {
    setStyle (qtmstyle());
      // FIXME: we should remove this and let the scheme code decide.
    QPalette pal (palette());
    pal.setColor (QPalette::Base, QColor (252, 252, 248));
    setPalette (pal);
  }
  
    // just to be sure we don't capture the wrong keys in keyPressEvent
  setCompleter(0);

  setStyleSheet (to_qstylesheet (style)); 
}

/*
 We need to reimplement the main event handler because we need the tab key for
 completions. (See Qt docs for QWidget::event())
 */
bool
QTMLineEdit::event (QEvent* ev) {
  if (ev->type() == QEvent::KeyPress)  // Handle ALL keys
    keyPressEvent(static_cast<QKeyEvent*> (ev));
  else
    return QWidget::event (ev);

  return true;
}

/*
 FIXME: This is a hideous mess...
 */
void 
QTMLineEdit::keyPressEvent(QKeyEvent* ev)
{
  QCompleter* c = completer();

  int key = (ev->key() == Qt::Key_Tab && ev->modifiers() & Qt::ShiftModifier)
            ? Qt::Key_Backtab
            : ev->key();
  
  if (c) {
    int row = 0;
    switch (key) {
      case Qt::Key_Down:
        completing = true;
        c->complete();
      case Qt::Key_Tab:
      {
        if (completing) {
          if (c->completionCount() > 1) {
            if(! c->setCurrentRow (c->currentRow() + 1))
              c->setCurrentRow (0);    // cycle
          } else {
            setCursorPosition (text().length());
            completing = false;
              //c->popup()->hide();
          }
        } else {
          if (hasSelectedText())
            c->setCompletionPrefix (text().left (selectionStart()));
          else
            c->setCompletionPrefix (text().left (cursorPosition()));

            // If there are no completions, send the key up for tab navigation
          if (c->completionCount() == 0 ||
              (c->completionCount() == 1 && c->currentCompletion() == text())) {
            QLineEdit::keyPressEvent (ev);
            return;
          }
          
          completing = true;
          c->complete();
        }
        if (hasSelectedText())
          setCursorPosition (selectionStart());
        if (c->currentCompletion() != "") {
          int pos = cursorPosition();
          setText (c->currentCompletion ());
          setSelection (pos, text().length() - 1);
        } else {
          completing = false;
          setSelection (0, text(). length() - 1);
        }

        ev->accept();
      }
        return;
          // This is different on purpose: when "back-completing" suggested text
          // we want to display the previous entry to the one suggested
      case Qt::Key_Up:
      case Qt::Key_Backtab:
      {
        completing = true;
        c->complete();
        row = c->currentRow();
        if(! c->setCurrentRow (row - 1))
          c->setCurrentRow (c->completionCount() - 1);    // cycle
        if (c->currentCompletion() != "") {
          int pos;
          if (hasSelectedText())
            pos = selectionStart();
          else
            pos = cursorPosition();
          setText (c->currentCompletion ());
          setSelection (pos, text().length() - 1);
        } else {
            // TODO: blink somehow
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
          setText (c->currentCompletion());
          setCursorPosition (text().length());
        } else {
          emit returnPressed();
        }
        completing = false;
        ev->accept();
        return;
      case Qt::Key_Escape:
        if (completing) {
          if (c->popup()) c->popup()->hide();
          completing = false;
        } else {
          emit editingFinished();
          ev->accept();
          if (parentWidget())        // HACK to return focus to the main editor widget
            parentWidget()->setFocus();
        }
        return;
      default:
        completing = false;
        QLineEdit::keyPressEvent (ev);
        return;
    }
  } else {
    QLineEdit::keyPressEvent (ev);
  }
}

QSize
QTMLineEdit::sizeHint () const {
  return qt_decode_length(ww, "", QLineEdit::sizeHint(), fontMetrics());
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
  emit focusOut();
  QLineEdit::focusOutEvent (ev);
}



/******************************************************************************
 * QTMTabWidget
 ******************************************************************************/

QTMTabWidget::QTMTabWidget(QWidget *p) : QTabWidget(p) {
  QObject::connect(this, SIGNAL(currentChanged(int)), this, SLOT(resizeOthers(int)));
}

/*! Resizes the widget to the size of the tab given by the index.
 
 In particular, we must tell all parent widgets to adjustSize() as well as
 possibly resize the window: qt_window_widget_rep's constructor sets a fixed
 size for windows which do not contain variable size resize_widgets. In this 
 case we must update the fixed size to reflect the change of tab.
 */
void
QTMTabWidget::resizeOthers(int current) {
  for(int i = 0; i < count(); ++i) {
    if (i != current)
      widget(i)->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Ignored);
    else
      widget(i)->setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Minimum);
  }
  
    // FIXME? this could loop indefinitely if parents are cyclic.
  QWidget* p = this;
  while (p != window()) {
    p->adjustSize();
    p = p->parentWidget();
  }

  if (window()->minimumSize()!=QSize(0,0) && 
      window()->maximumSize() != QSize(QWIDGETSIZE_MAX, QWIDGETSIZE_MAX))
    window()->setFixedSize(window()->sizeHint());
}



/******************************************************************************
 * QTMRefreshWidget
 ******************************************************************************/

widget make_menu_widget (object wid);

QTMRefreshWidget::QTMRefreshWidget (string _tmwid)
: QWidget (), tmwid (_tmwid), curobj (false), cur (), qwid(NULL),
  cache (widget ())
{   
  QObject::connect(the_gui->gui_helper, SIGNAL(tmSlotRefresh()), 
                   this, SLOT(doRefresh()));
  QVBoxLayout* l = new QVBoxLayout (this);
  l->setContentsMargins (0, 0, 0, 0);
  l->setMargin (0);
  setLayout (l);
  
  doRefresh();
}

bool
QTMRefreshWidget::recompute () {
  string s = "'(vertical (link " * tmwid * "))";
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
    cache (xwid) = cur;
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
QTMRefreshWidget::doRefresh() {
  if (recompute()) {
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
}


/******************************************************************************
 * QTMComboBox
 ******************************************************************************/

QTMComboBox::QTMComboBox (QWidget* parent) : QComboBox (parent) {
    ///// Obtain the minimum vertical size
  QComboBox cb;
  cb.setSizeAdjustPolicy(AdjustToContents);
  cb.addItem("");
  minSize = cb.sizeHint();  // we'll just keep the height
  
    ///// Add width of the arrow button
  QStyleOptionComboBox opt;
  opt.initFrom(&cb);
  opt.activeSubControls = QStyle::SC_ComboBoxArrow;
  QRect r = style()->subControlRect (QStyle::CC_ComboBox, &opt,
                                     QStyle::SC_ComboBoxArrow, &cb);
  minSize.setWidth(r.width());
}

/*! Add items and fix the ComboBox size using texmacs length units.
 
 Relative sizes are set based on the minimum bounding box in which any item of
 the list fits. Absolute sizes are set independently of the size of items in 
 the list.
 
 The QComboBox' minimum height is the original minimumSizeHint().
 */
void
QTMComboBox::addItemsAndResize (const QStringList& texts, string ww, string hh) {
  QComboBox::addItems(texts);
  
    ///// Calculate the minimal contents size:
  calcSize = QApplication::globalStrut ();
  const QFontMetrics& fm = fontMetrics ();
  
  for (int i = 0; i < count(); ++i) {
    QRect br = fm.boundingRect(itemText(i));
    calcSize.setWidth (qMax (calcSize.width(), br.width()));
    calcSize.setHeight (qMax (calcSize.height(), br.height()));
  }
  calcSize = qt_decode_length (ww, hh, calcSize, fm);
  
    ///// Add minimum constraints and fix size
  calcSize.setHeight (qMax (calcSize.height(), minSize.height()));
  calcSize.rwidth() += minSize.width();
  
  setFixedSize(calcSize);
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
      lineEdit()->event(ev);             // but we do.
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
QTMScrollArea::setWidgetAndConnect (QWidget* w)
{
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
QTMScrollArea::scrollToSelection (const QItemSelection& sel)
{
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
}

/*! Work around a problem with scrolling before the widget is shown.
 
 Calling ensureVisible() before the widget is shown scrolls the viewport by an
 insufficient amount. See the comments to QTMScrollArea.
 */
void
QTMScrollArea::showEvent (QShowEvent* ev)
{
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
  
  for (int i = 0; i < model()->rowCount(); ++i) {
    QModelIndex item = model()->index (i, 0);
    if (selections.contains (model()->data(item, Qt::DisplayRole).toString(), Qt::CaseSensitive))
      selectionModel()->select (item, QItemSelectionModel::SelectCurrent);
  }

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
  QListView::selectionChanged (c, p);
  emit selectionChanged (c);
}
