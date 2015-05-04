
/******************************************************************************
* MODULE     : QTMGuiHelper.cpp
* DESCRIPTION: QT Gui helper class.
*              Infrastructure for delayed menu installation 
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMGuiHelper.hpp"
#include "qt_tm_widget.hpp"
#include "qt_utilities.hpp"
#include "scheme.hpp"
#include "iterator.hpp"
#include <QFileOpenEvent>

void
QTMGuiHelper::doUpdate () {
  // cout << "UPDATE " << texmacs_time () << LF;
  try {
    gui->update();
  }
  catch (string s) {
    the_exception= s;
  }
}

void
QTMGuiHelper::doRefresh () {
  try {
    emit refresh();
  }
  catch (string s) {
    the_exception= s;
  }
}

bool
QTMGuiHelper::eventFilter (QObject *obj, QEvent *event) {
  if (event->type() == QEvent::FileOpen) {
    QFileOpenEvent* openEvent = static_cast<QFileOpenEvent *>(event);
    const char* s = openEvent->file().toUtf8().constData();
      //qDebug ("File Open Event %s", s);
    call ("load-buffer", object (url_system (s)), eval (":new-window"));
    return true;
  } else {
      // standard event processing
    return QObject::eventFilter (obj, event);
  }
}

void
QTMGuiHelper::doWriteSocketNotification (int socket) {
  try {
    if (DEBUG_QT)
      debug_qt << "WRITE SOCKET NOTIFICATION " << socket << " "
	       << texmacs_time () << LF;
    iterator<socket_notifier> it = iterate (gui->write_notifiers);
    while (it->busy ()) {
      socket_notifier sn= it->next ();
      if (sn->fd == socket) {
        //sn->notify();
	the_gui->process_socket_notification (sn);
	the_gui->enable_notifier (sn, false);
      }
    }
  }
  catch (string s) {
    the_exception= s;
  }
}

void
QTMGuiHelper::doReadSocketNotification (int socket) {
  try {
    if (DEBUG_QT)
      debug_qt << "READ SOCKET NOTIFICATION " << socket << " "
	       << texmacs_time () << LF;
    iterator<socket_notifier> it = iterate (gui->read_notifiers);
    while (it->busy ()) {
      socket_notifier sn= it->next ();
      if (sn->fd == socket) {
        //sn->notify();
	the_gui->process_socket_notification (sn);
	the_gui->enable_notifier (sn, false);
      }
    }
  }
  catch (string s) {
    the_exception= s;
  }
}

void
QTMGuiHelper::aboutToShowMainMenu() {
  //cout << "Show :" << menu_count << LF;
  try {
    menu_count++;
  }
  catch (string s) {
    the_exception= s;
  }
}

void 
QTMGuiHelper::aboutToHideMainMenu() {
  try {
    menu_count--;
    //cout << "Hide :" << menu_count << " " << N(waiting_widgets) <<  LF;
    if (menu_count <= 0) {
      menu_count = 0;
      QTimer::singleShot (0, the_gui->gui_helper, SLOT (doPopWaitingWidgets ()));
    }
  }
  catch (string s) {
    the_exception= s;
  }
}

void 
QTMGuiHelper::doPopWaitingWidgets() {
  try {
    if (!is_nil (waiting_widgets)) {
      if (DEBUG_QT)
	debug_qt << "Installing postponed menu" << LF;
      waiting_widgets->item->install_main_menu();
      waiting_widgets = waiting_widgets->next;
    }
  }
  catch (string s) {
    the_exception= s;
  }
}

void
QTMGuiHelper::emitTmSlotRefresh (string kind) {
  try {
    emit tmSlotRefresh (kind);
  }
  catch (string s) {
    the_exception= s;
  }
}
