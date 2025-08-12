
/******************************************************************************
* MODULE     : android_server_log.cpp
* DESCRIPTION: Android server log facilities
* COPYRIGHT  : (C) 2025 Robin Wils
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/
#include "server_log.hpp"
#include "string.hpp"

#include <QDebug>

void
server_log_start () {
}

void
server_log_stop () {
}

void
server_log_write (int level, string m) {
  c_string _msg(m);
  QString msg = QString::fromUtf8(_msg, N(m));

  switch (level) {
    case log_emergency:
    case log_alert:
    case log_critical:
    case log_error:
      qCritical() << msg << LF;
    case log_warning:
      qWarning() << msg << LF;
    case log_debug:
      qDebug() << msg << LF;
    default:
      qInfo() << msg << LF;
  }
}
