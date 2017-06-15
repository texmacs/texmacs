
/******************************************************************************
* MODULE     : qt_sys_utils.cpp
* DESCRIPTION: external command launcher
* COPYRIGHT  : (C) 2009  David MICHEL
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "qt_sys_utils.hpp"
#include "basic.hpp"
#include "string.hpp"

#include <QProcess>
#include <QString>

int
qt_system (string cmd) {
  string result;
  return qt_system (cmd, result);
}

int
qt_system (string cmd, string& result) {
  QProcess proc;

  proc.setProcessChannelMode (QProcess::MergedChannels);
  c_string _cmd (cmd);
#ifdef OS_MINGW
  QString qcmd = QString::fromLocal8Bit (_cmd);
#else
  QString qcmd = "sh -c \"";
  qcmd += _cmd;
  qcmd += "\"";
#endif
  proc.start (qcmd);
  if (! proc.waitForStarted ()) {
    if (DEBUG_STD) debug_shell << "System: failed to launch command\n";
    return 1;
  }
  proc.closeWriteChannel ();
  if (! proc.waitForFinished (-1)) {
    if (DEBUG_STD) debug_shell << "System: waiting for too long\n";
    return 1;
  }
  result = proc.readAll ().constData ();
  if (DEBUG_STD) debug_shell << result;
  return proc.exitCode ();
}

