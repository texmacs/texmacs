
/******************************************************************************
* MODULE     : qt_sys_utils.cpp
* DESCRIPTION: external command launcher
* COPYRIGHT  : (C) 2009, 2016  David MICHEL, Denis Raux
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

static void
ReadOutputs(QProcess& p, string& o, string& e) {
  if (p.processChannelMode() == QProcess::MergedChannels)
    o = p.readAll ().constData ();
  else {
    o = p.readAllStandardOutput ().constData ();
    e = p.readAllStandardError ().constData ();
  }
  if (DEBUG_STD) debug_shell << o << e;
}

static int
qt_system (QProcess& proc, string& cmd, string& cmdout, string& cmderr) {
  c_string _cmd (cmd);
#ifdef OS_MINGW
  QString qcmd = QString::fromUtf8 (_cmd);
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
  if (! proc.waitForFinished ()) { //default time 30000ms
    if (DEBUG_STD) debug_shell << "System: waiting for too long\n";
    ReadOutputs (proc, cmdout, cmderr);
    proc.close ();
    return 2;
  }
  ReadOutputs (proc, cmdout, cmderr);
  return proc.exitCode ();
}

int
qt_system (string cmd) {
  string result;
  return qt_system (cmd, result);
}

int
qt_system (string cmd, string& cmdout, string& cmderr) {
  QProcess proc;
  return qt_system (proc, cmd, cmdout, cmderr);
}

int
qt_system (string cmd, string& result) {
  QProcess proc;
  string dummy;	
  proc.setProcessChannelMode (QProcess::MergedChannels);
  return qt_system (proc, cmd, result, dummy);
}
