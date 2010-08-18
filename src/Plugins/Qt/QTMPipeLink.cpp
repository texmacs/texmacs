
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
    else if (c == DATA_COMMAND) r << "[COMMAND]";
    else if (c == DATA_ESCAPE) r << "[ESCAPE]";
    else r << s[i];
  }
  return r;
}

void
QTMPipeLink::readErrOut () {
  feedBuf (QProcess::StandardError);
  feedBuf (QProcess::StandardOutput);
}

QTMPipeLink::QTMPipeLink (string cmd2) : cmd (cmd2), outbuf (""), errbuf ("") {}

QTMPipeLink::~QTMPipeLink () {
  killProcess ();
}

bool
QTMPipeLink::launchCmd () {
  if (state () != QProcess::NotRunning) killProcess ();
  char* _cmd = as_charp (cmd);
  QProcess::start (_cmd);
  tm_delete_array (_cmd);
  bool r= waitForStarted ();
  if (r) {
    connect (this, SIGNAL(readyReadStandardOutput ()), SLOT(readErrOut ()));
    connect (this, SIGNAL(readyReadStandardError ()), SLOT(readErrOut ()));
  }
  return r;
}

int
QTMPipeLink::writeStdin (string s) {
  if (DEBUG_IO) cout << "[INPUT]" << s;
  char* _s= as_charp (s);
  int err= QIODevice::write (_s, N(s));
  tm_delete_array (_s);
  return err;
}

void
QTMPipeLink::feedBuf (ProcessChannel channel) {
  setReadChannel (channel);
  QByteArray tempout = QIODevice::readAll ();
  if (channel == QProcess::StandardOutput) outbuf << tempout.constData ();
  else errbuf << tempout.constData ();
  if (DEBUG_IO) cout << channel << " " << debug_io_string (tempout.constData ()) << "\n";
}

bool
QTMPipeLink::listenChannel (ProcessChannel channel, int msecs) {
  setReadChannel (channel);
  return waitForReadyRead (msecs);
}

void
QTMPipeLink::killProcess () {
  disconnect (SIGNAL(readyReadStandardOutput ()), this, SLOT(readErrOut ()));
  disconnect (SIGNAL(readyReadStandardError ()), this, SLOT(readErrOut ()));
#if defined(__MINGW__) || defined(__MINGW32__)
  close ();
#else
  terminate ();
  if (! waitForFinished ()) kill ();
#endif
}

