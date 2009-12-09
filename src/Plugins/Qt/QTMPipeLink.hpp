
/******************************************************************************
* MODULE     : QTMPipeLink.hpp
* DESCRIPTION: QT TeXmacs links - header file
* COPYRIGHT  : (C) 2009 David MICHEL
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTM_PIPE_LINK_H
#define QTM_PIPE_LINK_H

#include "string.hpp"
#include "command.hpp"
#include <QProcess>

class QTMPipeLink : public QProcess {
  Q_OBJECT

public slots:
  void readErrOut ();

public:
  string cmd;
  string outbuf;
  string errbuf;
  command* feed_cmd;

  QTMPipeLink (string);
  ~QTMPipeLink ();

  inline void setOutbuf(string out) { outbuf= out; }
  inline void setErrbuf (string err) { errbuf= err; }
  inline string& getOutbuf() { return outbuf; }
  inline string& getErrbuf () { return errbuf; }

  bool launchCmd ();
  int  writeStdin (string s);
  void feedBuf (ProcessChannel);
  bool listenChannel (ProcessChannel, int msecs);
  void killProcess ();
};

#endif // QTM_PIPE_LINK

