
/******************************************************************************
* MODULE     : QTMMenuHelper.hpp
* DESCRIPTION: QT Texmacs menu helper classes
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMMENUHELPER_HPP
#define QTMMENUHELPER_HPP

#include "message.hpp"
#include "promise.hpp"
#include <QObject>
#include <QAction>
#include <QMenu>

class QTMCommand: public QObject {
  Q_OBJECT
  command_rep* cmd;
	
public:
  inline QTMCommand (command_rep *_cmd):
    cmd (_cmd) { INC_COUNT_NULL(cmd); }
  inline ~QTMCommand () {
    DEC_COUNT_NULL (cmd); }

public slots:
  inline void apply() {
    if (cmd) cmd->apply(); }
};

class QTMLazyMenu: public QMenu {
  Q_OBJECT
  promise_rep<widget> *pm;
  bool forced;

public:
  inline QTMLazyMenu (promise_rep<widget>* _pm):
    pm (_pm), forced (false) {
      INC_COUNT_NULL (pm); 
      QObject::connect (this, SIGNAL (aboutToShow ()), this, SLOT (force ()));
    }
  inline ~QTMLazyMenu() { DEC_COUNT_NULL(pm); }

public slots:
  void force();
};

#endif // QTMMENUHELPER_HPP
