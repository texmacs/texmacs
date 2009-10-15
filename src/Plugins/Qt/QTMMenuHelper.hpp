
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

#include "gui.hpp"

class QTMCommand: public QObject {
  Q_OBJECT
  command cmd;
        
public:
  inline QTMCommand (command _cmd):
    cmd (_cmd) {  }

public slots:
  inline void apply() {
    if (!is_nil(cmd)) { cmd->apply();  needs_update(); }
  }
};

class QTMLazyMenu: public QMenu {
  Q_OBJECT
  promise<widget> pm;

public:
  inline QTMLazyMenu (promise<widget> _pm):
    pm (_pm) {
      QObject::connect (this, SIGNAL (aboutToShow ()), this, SLOT (force ()));
    }

public slots:
  void force();
};

#endif // QTMMENUHELPER_HPP
