
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

#include "analyze.hpp"

#include <QtGui>

//////////////////////////////// QTMCommand ////////////////////////////////////

/*! Queues the object's command into the main queue. */
void 
QTMCommand::apply()  {
  if (DEBUG_QT) 
    cout << "QTMCommand::apply() (delayed)\n";
  if (!is_nil(cmd)) { the_gui->process_command(cmd); }
}


///////////////////////////////// QTMAction ////////////////////////////////////

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
    string t= tm_var_encode (str);
    if (t == "Help") t= "Help ";
    setText(to_qstring (t));
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


/////////////////////////////// QTMLazyMenu ////////////////////////////////////

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


////////////////////////// QTMInputTextWidgetHelper ////////////////////////////


QTMInputTextWidgetHelper::~QTMInputTextWidgetHelper() {
  //cout << "deleting" << LF;
  // remove refernce to helper in the texmacs widget
  wid()->helper = NULL;
  // if needed the texmacs widget is automatically deleted
}


void
QTMInputTextWidgetHelper::commit () {
  QLineEdit *le = qobject_cast <QLineEdit*> (sender());
  if (le) {
    //    le -> setFrame(false);
    wid()->ok = true;
    done = false;
    wid () -> text = from_qstring (le -> text());
  }
}

void
QTMInputTextWidgetHelper::leave () {
  // this is executed after commit
  // and when losing focus
  QLineEdit *le = qobject_cast <QLineEdit*> (sender());  
  if (le) {
    // reset the text according to the texmacs widget
    le -> setText (to_qstring (wid () -> text));
    //ok = false;
    QTimer::singleShot (0, this, SLOT (doit ()));
  }
}

void
QTMInputTextWidgetHelper::remove (QObject *obj) {
  views.removeAll (qobject_cast<QLineEdit*> (obj));
  if (views.count () == 0) {
    // no more view, free the helper 
    deleteLater();
  }
}

void
QTMInputTextWidgetHelper::add(QLineEdit *obj) {
  if (!views.contains (obj)) {
    QObject::connect (obj, SIGNAL(destroyed (QObject*)), this, SLOT(remove (QObject*)));
    views << obj;
  }
}

void
QTMInputTextWidgetHelper::doit () {
  if (done) return;
  done = true;
#if 0
  if (wid()->ok) 
    cout << "Committing: " << wid () -> text << LF;
  else 
    cout << "Leaving with text: " << wid () -> text << LF;
#endif
#if 0
  wid () -> cmd (wid()->ok ? list_object (object (wid() -> text)) : 
                 list_object (object (false)));
#else
  the_gui -> process_command(wid()->cmd, wid()->ok ? list_object (object (wid() -> text)) : 
                             list_object (object (false)));
#endif
}


  ////////////////////////// QTMLineEdit ////////////////////////////


QTMLineEdit::QTMLineEdit (QWidget* parent, string _ww, int style)
: QLineEdit (parent), ww (_ww) {
  if (style & WIDGET_STYLE_MINI) {
    setStyle (qtmstyle());
      // FIXME: we should remove this and let the scheme code decide.
    QPalette pal (palette());
    pal.setColor (QPalette::Base, QColor (252, 252, 248));
    setPalette (pal);      
  }

  setStyleSheet (to_qstylesheet (style)); 
}

void 
QTMLineEdit::keyPressEvent(QKeyEvent *event)
{
  QCompleter* c = completer();
  
  if (c) c->setCompletionPrefix (QString ());      // reset completion
  
  if (event->key() == Qt::Key_Up) {                // move back in history
    if (c) {
      int row = c->currentRow ();
      c->setCurrentRow (row-1);
      setText (c->currentCompletion ());
    }
    event->accept();
  } else if (event->key() == Qt::Key_Down) {       // move forward in history
    if (c) {
      int row = c->currentRow ();
      c->setCurrentRow (row+1);
      setText (c->currentCompletion ());
    }
    event->accept();
  } else if (event->key() == Qt::Key_Escape) {     // exit editing
    emit editingFinished();
    event->accept();
  } else {
    QLineEdit::keyPressEvent(event);
  }
}

QSize
QTMLineEdit::sizeHint () const {
  return qt_decode_length(ww, "", minimumSizeHint(), fontMetrics());
}

void 
QTMLineEdit::focusInEvent (QFocusEvent *event)
{
  setCursorPosition (text().size());
    //  selectAll ();
  QLineEdit::focusInEvent (event);
}


//////////////////////////////// QTMTabWidget /////////////////////////////////


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
