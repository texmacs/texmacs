
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
QTMAction::doRefresh() {
  if (N(str)) {
    if (str == "Help") str= "Help ";
    setText (to_qstring (str));
  }
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
QTMInputTextWidgetHelper::commit () {
  QLineEdit *le = qobject_cast <QLineEdit*> (sender());
  if (le) {
    done         = false;
    wid()->ok    = true;
    wid()->input = from_qstring (le->text());
  }
}

/*! Executed after commit of the input field (enter) and when losing focus */
void
QTMInputTextWidgetHelper::leave () {
  QLineEdit* le = qobject_cast <QLineEdit*> (sender());  
  if (!le) return;
 
    // reset the text according to the texmacs widget
  le->setText (to_qstring (wid()->input));
  
    // HACK: restore focus to the main editor widget
  widget win = qt_window_widget_rep::widget_from_qwidget(le);
  if (concrete(win)->type == qt_widget_rep::texmacs_widget)
    concrete(get_canvas(win))->qwid->setFocus();
  
    // process the scheme command
  if (done) return;
  done = true;
  the_gui->process_command (wid()->cmd, wid()->ok
                            ? list_object (object (wid()->input))
                            : list_object (object (false)));
}

void
QTMInputTextWidgetHelper::remove (QObject* obj) {
  views.removeAll (qobject_cast<QLineEdit*> (obj));
  if (views.count () == 0)
    deleteLater();
}

void
QTMInputTextWidgetHelper::add (QLineEdit* le) {
  if (!views.contains (le)) {
    QObject::connect (le, SIGNAL (destroyed (QObject*)), this, SLOT (remove (QObject*)));
    QObject::connect (le, SIGNAL (returnPressed ()),     this, SLOT (commit ()));
    QObject::connect (le, SIGNAL (editingFinished ()),   this, SLOT (leave ()));
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
QTMFieldWidgetHelper::add (QComboBox* cb) {
  if (!views.contains (cb)) {
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
    keyPressEvent((QKeyEvent*)ev);
  else
    return QWidget::event (ev);

  return true;
}

void 
QTMLineEdit::keyPressEvent(QKeyEvent* ev)
{
  QCompleter* c = completer();
  
  if (c) {
    int row = 0;
    switch (ev->key()) {
      case Qt::Key_Up:
        completing = true;
        row = c->currentRow();
        c->setCompletionPrefix (QString ());        // reset completion
        if(! c->setCurrentRow (row-1))
          c->setCurrentRow(c->completionCount()-1); // cycle
        setText (c->currentCompletion ());
        ev->accept();
        return;
      case Qt::Key_Down:
        completing = true;
        row = c->currentRow();
        c->setCompletionPrefix (QString ());        // reset completion
        if (! c->setCurrentRow (row+1))
          c->setCurrentRow(0);                      // cycle
        setText (c->currentCompletion ());
        ev->accept();
        return;
        
        // Either complete the suggested text or advance in the list of
        // completions.
      case Qt::Key_Tab:
      {
        if (completing) {
          if (c->completionCount() > 1) {
            if(! c->setCurrentRow (c->currentRow()-1))
              c->setCurrentRow(c->completionCount()-1); // cycle
          } else {
            setCursorPosition(text().length());
          }
        } else {
          if (hasSelectedText())
            c->setCompletionPrefix(text().left(selectionStart()));
          else
            c->setCompletionPrefix(text().left(cursorPosition()));
          completing = true;          
        }
        if (hasSelectedText())
          setCursorPosition(selectionStart());
        int pos = cursorPosition();
        setText (c->currentCompletion ());
        setSelection (pos, text().length()-1);
        ev->accept();
      }
        return;
          // This is different on purpose: when "back-completing" suggested text
          // we want to display the previous entry to the one suggested
      case Qt::Key_Backtab:
      {
        completing = true;
        row = c->currentRow();
        if(! c->setCurrentRow (row+1))
          c->setCurrentRow(0);                      // cycle
        int pos;
        if (hasSelectedText())
          pos = selectionStart();
        else
          pos = cursorPosition();
        setText (c->currentCompletion ());
        setSelection (pos, text().length()-1);
        ev->accept();
      }
        return;
      case Qt::Key_Shift:   // need to ignore this to correctly get shift-tabs
        if (completing) return;
      default:
        completing = false;
        break;
    }
  }

  if (ev->key() == Qt::Key_Escape) {
    emit editingFinished();
    ev->accept();
    if (parentWidget())        // HACK to return focus to the main editor widget
      parentWidget()->setFocus();
  } else {
    QLineEdit::keyPressEvent(ev);
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
: QWidget (), tmwid (_tmwid), curobj (false), cur (), cache (widget ()) 
{   
  QObject::connect(the_gui->gui_helper, SIGNAL(tmSlotRefresh()), 
                   this, SLOT(doRefresh()));
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

void 
QTMRefreshWidget::doRefresh() {
  if (recompute()) {
    if (layout()) {
      QLayoutItem* item;
      while ((item = layout()->takeAt(0)) != 0) {		
        if (item->widget()) {
          layout()->removeWidget(item->widget());
          delete item->widget();
        }	
        delete item;
      }
      delete layout();
    }
    setLayout(concrete(cur)->as_qlayoutitem()->layout());
    
      // Tell the window to fix its size to the new one if we had it fixed to
      // begin with (this is indicated by minimum and maximum sizes set to 
      // values other than the default)
    if (window()->minimumSize() != QSize(0,0) && 
        window()->maximumSize() != QSize(QWIDGETSIZE_MAX, QWIDGETSIZE_MAX))
      window()->setFixedSize(window()->sizeHint());  
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