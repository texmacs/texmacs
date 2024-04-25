
/******************************************************************************
* MODULE     : qt_pipe_link.cpp
* DESCRIPTION: QT TeXmacs links
* COPYRIGHT  : (C) 2009 David MICHEL
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_link.hpp"
#include "qt_utilities.hpp"
#include "qt_gui.hpp"
#include "QTMPipeLink.hpp"
#include <QByteArray>

static string
debug_io_string (QByteArray s) {
  int i, n= s.size ();
  string r;
  for (i=0; i<n; i++) {
    unsigned char c= (unsigned char) s[i];
    if (c == DATA_BEGIN) r << "[BEGIN]";
    else if (c == DATA_END) r << "[END]";
    else if (c == DATA_ABORT) r << "[ABORT]";
    else if (c == DATA_COMMAND) r << "[COMMAND]";
    else if (c == DATA_ESCAPE) r << "[ESCAPE]";
    else r << s[i];
  }
  return r;
}

void
QTMPipeLink::readErrOut () {
BEGIN_SLOT
  feedBuf (QProcess::StandardError);
  feedBuf (QProcess::StandardOutput);
END_SLOT
}

QTMPipeLink::QTMPipeLink (string cmd2) : cmd (cmd2), outbuf (""), errbuf ("") {}

QTMPipeLink::~QTMPipeLink () {
  killProcess (1000);
}

bool
QTMPipeLink::launchCmd () {
  if (state () != QProcess::NotRunning) killProcess (1000);
  //FIXME: is UTF8 the right encoding here?
  startCommand (utf8_to_qstring (cmd));
  bool r= waitForStarted ();
  if (r) {
    connect (this, SIGNAL(readyReadStandardOutput ()), SLOT(readErrOut ()));
    connect (this, SIGNAL(readyReadStandardError ()), SLOT(readErrOut ()));
  }
  return r;
}

int
QTMPipeLink::writeStdin (string s) {
  c_string _s (s);
  if (DEBUG_IO) debug_io << "[INPUT]" << debug_io_string ((char*)_s);
  int err= QIODevice::write (_s, N(s));
  return err;
}

void
QTMPipeLink::feedBuf (ProcessChannel channel) {
  setReadChannel (channel);
  QByteArray tempout = QIODevice::readAll ();
  if (channel == QProcess::StandardOutput) outbuf << tempout.constData ();
  else errbuf << tempout.constData ();
  if (DEBUG_IO)
    debug_io << "[OUTPUT " << channel << "]" << debug_io_string (tempout.constData ()) << "\n";
}

bool
QTMPipeLink::listenChannel (ProcessChannel channel, int msecs) {
  setReadChannel (channel);
  return waitForReadyRead (msecs);
}

void
QTMPipeLink::killProcess (int msecs) {
  disconnect (SIGNAL(readyReadStandardOutput ()), this, SLOT(readErrOut ()));
  disconnect (SIGNAL(readyReadStandardError ()), this, SLOT(readErrOut ()));
#ifdef OS_MINGW
  (void) msecs;
  close ();
#else
  terminate ();
  if (! waitForFinished (msecs)) kill ();
#endif
}

