
/******************************************************************************
* MODULE     : QTMMenuHelper.hpp
* DESCRIPTION: QT Texmacs menu helper class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMMenuHelper.hpp"

#include "qt_gui.hpp"
#include "qt_utilities.hpp"
#include "Scheme/object.hpp"


//////////////////////////////// QTMCommand ////////////////////////////////////


/*! Queues the object's command into the main queue. */
void 
QTMCommand::apply()  {
  if (!is_nil(cmd)) { the_gui->process_command(cmd); }
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


//////////////////////////////// QTMTabWidget /////////////////////////////////


QTMTabWidget::QTMTabWidget(QWidget *p) : QTabWidget(p) {
  QObject::connect(this, SIGNAL(currentChanged(int)), this, SLOT(resizeOthers(int)));
}

void
QTMTabWidget::resizeOthers(int index) {
  for(int i = 0; i < count(); ++i) {
    if (i != index)
      widget(i)->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Ignored);
      else
        widget(i)->setSizePolicy(QSizePolicy::MinimumExpanding, 
                                 QSizePolicy::MinimumExpanding);
        }

  if (layout())
    layout()->activate();
  window()->resize(minimumSizeHint());
}