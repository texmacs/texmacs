
/******************************************************************************
* MODULE     : QTMGuiHelper.hpp
* DESCRIPTION: QT Gui helper class. Infrastructure for delayed menu installation 
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMGUIHELPER_HPP
#define QTMGUIHELPER_HPP

#include "qt_gui.hpp"
#include "qt_ui_element.hpp"  // qt_refresh_widget_rep
#include <QObject>
#include <QComboBox>
#include <QTranslator>

/*!
 */
class QTMGuiHelper : public QObject {
  Q_OBJECT
  qt_gui_rep* gui;

public:
  inline QTMGuiHelper (qt_gui_rep *_gui) : QObject (), gui (_gui) {}

protected:
  bool eventFilter (QObject *obj, QEvent *event);
  
public slots:
  void doUpdate ();
  void doRefresh ();
  void doReadSocketNotification (int socket);  
  void doWriteSocketNotification (int socket);  
  
  void aboutToShowMainMenu ();
  void aboutToHideMainMenu ();
  void doPopWaitingWidgets ();
 
  void emitTmSlotRefresh ();

signals:
  void refresh ();
  void tmSlotRefresh ();   //!< qt_widgets which need to refresh connect here.
};


/*! A container widget which redraws the widgets it owns. */
class QTMRefreshWidget : public QWidget {
  Q_OBJECT
  
  string tmwid;
  object curobj;
  widget cur;
  hashmap<object,widget> cache;
  
public:
  QTMRefreshWidget (string _tmwid);
  
  bool recompute ();
  
public slots:
  void doRefresh ();  
};


/*! A mutilated QComboBox which fixes its size using texmacs lengths.

 To use just create the QWidget and call addItemsAndResize().
 */
class QTMComboBox : public QComboBox {
  Q_OBJECT
  
  QSize calcSize;
  QSize minSize;
public:
  QTMComboBox (QWidget* parent);

  void addItemsAndResize (const QStringList& texts, string ww, string h);
};

#endif // QTMGUIHELPER_HPP
