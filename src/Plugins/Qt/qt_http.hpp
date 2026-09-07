
/******************************************************************************
* MODULE     : qt_http.hpp
* DESCRIPTION: HTTP requests
* COPYRIGHT  : (C) 2026  Gregoire Lecerf
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QT_HTTP_HPP
#define QT_HTTP_HPP

#include "string.hpp"
#include "array.hpp"
#include "scheme.hpp"
#include "qt_utilities.hpp"

#include <QNetworkReply>

tree qt_http_from_json (string s, int mode= 0);

// post raw data
int qt_http_post (string& ret, string url, array<string> headers_attr,
		  string data);
bool qt_async_http_post (string url, array<string> headers_attr,
			 string data, object callback);

// post json tree
int qt_http_post (string& ret, string url, array<string> headers_attr,
		  tree t);
bool qt_async_http_post (string url, array<string> headers_attr,
			 tree t, object callback);
bool qt_async_http_post (string url, array<string> headers_attr,
			 tree t, int& status, string& outbuf,
			 string& errbuf, bool& kill);

// post query
int qt_http_post (string& ret, string url, array<string> headers_attr,
		  array<string> attr);
bool qt_async_http_post (string url, array<string> headers_attr,
			 array<string> attr, object callback);

// async handler
class QTMHTTPHandler: public QObject {
  Q_OBJECT
  QNetworkReply* reply;
  int* status;
  string* outbuf;
  string* errbuf;
  bool* kill;
public:
  QTMHTTPHandler (QNetworkReply* nr, int* st, string* o, string* e, bool* k,
		  QObject* parent= NULL) :
    QObject (parent), reply (nr), status (st),
    outbuf (o), errbuf (e), kill (k) {}
  ~QTMHTTPHandler () {
    reply->deleteLater ();
    reply= NULL; }
public slots:
  void onFinished ();
};

// async handler for scheme callback
class QTMHTTPHandler_callback: public QObject {
  Q_OBJECT
  QNetworkReply* reply;
  object callback;
public:
  QTMHTTPHandler_callback (QNetworkReply* nr, object cb,
			   QObject* parent= NULL) :
    QObject (parent), reply (nr), callback (cb) {}
  ~QTMHTTPHandler_callback () {
    reply->deleteLater ();
    reply= NULL; }
public slots:
  void onFinished ();
};

#endif  // QT_HTTP_HPP
