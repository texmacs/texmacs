
/******************************************************************************
* MODULE     : QTMInteractiveInputHelper.hpp
* DESCRIPTION: QT Interactive Input Helper class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMINTERACTIVEINPUTHELPER_HPP
#define QTMINTERACTIVEINPUTHELPER_HPP

#include <QObject>

class qt_tm_widget_rep;

class QTMInteractiveInputHelper : public QObject {
  Q_OBJECT
  qt_tm_widget_rep* wid;

public:
  inline QTMInteractiveInputHelper (qt_tm_widget_rep* _wid):
    QObject(), wid(_wid) {}

public slots:
  void commit(int result);
};

#endif // QTMINTERACTIVEINPUTHELPER_HPP
