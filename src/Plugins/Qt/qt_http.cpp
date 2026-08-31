
/******************************************************************************
* MODULE     : qt_http.cpp
* DESCRIPTION: HTTP requests
* COPYRIGHT  : (C) 2026  Gregoire Lecerf
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "qt_http.hpp"
#include "convert.hpp"
#include "qt_utilities.hpp"

#if QT_VERSION >= 0x060000

#include <QNetworkAccessManager>
#include <QNetworkRequest>
#include <QNetworkReply>
#include <QEventLoop>
#include <QUrlQuery>
#include <QFile>
#include <QJsonValue>
#include <QJsonObject>
#include <QJsonArray>
#include <QJsonDocument>

// Use a single manager in order to share connections several times
static QNetworkAccessManager*
get_manager () {
  static QNetworkAccessManager* manager= new QNetworkAccessManager ();
  static bool first= true;
  if (first) {
#if QT_VERSION >= QT_VERSION_CHECK(5, 15, 0)
    manager->setTransferTimeout (60000); // 60s
#endif
    first= false;
  }
  return manager;
}

// post raw data
static int
qt_http_post (string& ret, string url, array<string> headers_attr,
	      const char* data, long long n) {
  if (DEBUG_IO)
    debug_io << "qt_http_post" << LF
	     << headers_attr << LF
	     << string (data, n) << LF;
  ret= "";
  QUrl qurl (utf8_to_qstring (url));
  if (!qurl.isValid ()) {
    io_error << "qt_http_post, invalid URL: " << url << LF;
    return -1;
  }
  QNetworkRequest request (qurl);
  for (int i= 0; i+1 < N(headers_attr); i += 2) {
    string name= headers_attr[i];
    string value= headers_attr[i+1];
    request.setRawHeader (QByteArray (&name[0], N(name)),
			  QByteArray (&value[0], N(value)));
  }
  QNetworkReply* reply=
    get_manager ()->post (request, QByteArray (data, n));
  if (reply == NULL) {
    io_error << "qt_http_post, cannot connect to " << url << LF;
    return -2;
  }
  QEventLoop loop;
  QObject::connect (reply, &QNetworkReply::finished,
		    &loop, &QEventLoop::quit);
  loop.exec();
  if (reply->error () != QNetworkReply::NoError) {
    io_error << "qt_http_post, in reply from " << url << ": "
	     << from_qstring_utf8 (reply->errorString ()) << LF;
    return -3;
  }
  ret= from_qstring_utf8 (QString (reply->readAll ()));
  delete reply;
  if (DEBUG_IO)
    debug_io << "qt_http_post, replied: " << ret << LF;
  return N(ret);
}

// post raw data
int
qt_http_post (string& ret, string url, array<string> headers_attr,
	      string data) {
  return qt_http_post (ret, url, headers_attr, &data[0], N(data));
}

// conversion from TeXmacs Json
static QJsonValue
tree_to_qjson (tree t) {
  if (is_func (t, ATTR)) {
    QJsonObject o;
    for (int i= 0; i+1 < N(t); i += 2) {
      if (!is_string (t[i]))
	io_error << "tree_to_qjson, invalid key name: " << t[i] << LF;
      o [utf8_to_qstring (as_string (t[i]))]= tree_to_qjson (t[i+1]);
    }
    return o;
  }
  if (is_func (t, TUPLE)) {
    QJsonArray a;
    for (int i= 0; i < N(t); i++)
      a.prepend (tree_to_qjson (t[i]));
    return a;
  }
  else if (t == tree () || is_compound (t, "json-null")) {
    return QJsonValue (); }
  else if (is_compound (t, "json-boolean", 1) && is_atomic (t[0])) {
    return as_bool (t[0]); }
  else if (is_compound (t, "json-number", 1) && is_atomic (t[0])) {
    return is_int (t[0]) ? as_int (t[0]) : as_double (t[0]); } 
  if (is_atomic (t)) {
    if (is_bool (t)) return as_bool (t);
    if (is_int (t)) return as_int (t);
    if (is_double (t)) return as_double (t);
    if (is_string (t)) return utf8_to_qstring (as_string (t));
  }
  io_error << "tree_to_qjson, invalid json tree: " << t << LF;
  return QJsonValue ();
}

static QByteArray
qjson_to_bytes (const QJsonValue& v) {
  QJsonDocument d;
  if (v.isArray ()) d= QJsonDocument (v.toArray ());
  else if (v.isObject ()) d= QJsonDocument (v.toObject ());
  return d.toJson ();
}

// conversion from TeXmacs Json
static tree
qjson_to_tree (const QJsonValue& j, int mode) {
  if (j.isNull ())
    return json_null (mode);
  if (j.isBool ())
    return json_boolean (j.toBool () ? "true": "false", mode);
  if (j.isDouble ())
    return json_number (as_string (j.toDouble ()), mode);
  if (j.isString ())
    return as_tree (from_qstring_utf8 (j.toString ()));
  if (j.isArray ()) {
    QJsonArray a= j.toArray ();
    tree t (TUPLE);
    for (int i= 0; i < a.size (); i++)
      t << qjson_to_tree (a[i], mode);
    return t;
  }
  if (j.isObject ()) {
    QJsonObject o= j.toObject ();
    tree t (ATTR);
    for (QJsonObject::const_iterator it= o.begin (); it != o.end (); it++) {
      t << from_qstring_utf8 (it.key ());
      t << qjson_to_tree (it.value (), mode);
    }
    return t;
  }
  QByteArray a= qjson_to_bytes (j);
  io_error << "qjson_to_tree, invalid QJsonValue: "
	   << string (a.constData (), a.size ()) << LF;
  return tree ();
}

// http post json data
int
qt_http_post (string& ret, string url, array<string> headers_attr, tree t) {
  QJsonValue v= tree_to_qjson (t);
  QByteArray a= qjson_to_bytes (v);
  return qt_http_post (ret, url, headers_attr, a.constData (), a.size ());
}

// http post query data
int
qt_http_post (string& ret, string url, array<string> headers_attr,
	      array<string> attr) {
  QUrlQuery q;
  for (int i= 0; i+1 < N(attr); i += 2) {
    QString tmp;
    if (ends (attr[i], "@")) {
      string key= attr[i](0, N(attr[i])-1);
      QFile f (utf8_to_qstring (attr[i+1]));
      if (f.open (QIODevice::ReadOnly | QIODevice::Text)) {
	QTextStream in (&f);
	tmp= in.readAll();
	f.close();
      }
      else
	io_error << "qt_http_post, cannot open file " << attr[i+1] << LF;
      q.addQueryItem (utf8_to_qstring (key), tmp);
    }
    else
      q.addQueryItem (utf8_to_qstring (attr[i]),
		      utf8_to_qstring (attr[i+1]));
  }
  QByteArray data= q.toString (QUrl::FullyEncoded).toUtf8 ();
  return qt_http_post (ret, url, headers_attr,
		       data.constData (), data.size ());
}

tree
qt_http_from_json (string s) { 
  QJsonDocument d= QJsonDocument::fromJson (QByteArray (&s[0], N(s)));
  QJsonValue j;
  const int mode= JSON_NULL | JSON_BOOLEAN | JSON_NUMBER;
  if (d.isEmpty () || d.isNull ())
    return json_null (mode);
  if (d.isArray ()) j= d.array ();
  if (d.isObject ()) j= d.object ();
  return qjson_to_tree (j, mode);
}

// Asynchroneous variant

void
QTMHTTPHandler::onFinished () {
  if (reply == NULL) call (callback, string (""));
  if (reply->error() != QNetworkReply::NoError)
    io_error << "http_post from "
	     << from_qstring_utf8 (reply->url ().host ()) << ": "
	     << from_qstring_utf8 (reply->errorString ()) << LF;
  else {
    string buffer= from_qstring_utf8 (QString (reply->readAll ()));
    call (callback, buffer);
  }
  reply->close ();
  this->deleteLater ();
}

// Asynchroneous post of raw data
static bool
qt_async_http_post (string url, array<string> headers_attr,
		    const char* data, long long n, object callback) {
  if (DEBUG_IO)
    debug_io << "qt_async_http_post" << LF
	     << headers_attr << LF
	     << string (data, n) << LF;
  QUrl qurl (utf8_to_qstring (url));
  if (!qurl.isValid ()) {
    io_error << "qt_async_http_post, invalid URL: " << url << LF;
    return true;
  }
  QNetworkRequest request (qurl);
  for (int i= 0; i+1 < N(headers_attr); i += 2) {
    string name= headers_attr[i];
    string value= headers_attr[i+1];
    request.setRawHeader (QByteArray (&name[0], N(name)),
			  QByteArray (&value[0], N(value)));
  }
  QNetworkReply* reply=
    get_manager ()->post (request, QByteArray (data, n));
  if (reply == NULL) {
    io_error << "qt_async_http_post, cannot connect to " << url << LF;
    return true;
  }
  QTMHTTPHandler* h= new QTMHTTPHandler (reply, callback);
  QObject::connect (reply, &QNetworkReply::finished,
		    h, &QTMHTTPHandler::onFinished);
  return h == NULL;
}

// Asynchroneous post of raw data
bool
qt_async_http_post (string url, array<string> headers_attr,
		    string data, object callback) {
  return qt_async_http_post (url, headers_attr, &data[0], N(data),
			     callback);
}

// Asynchroneous post of json data
bool
qt_async_http_post (string url, array<string> headers_attr,
		    tree data, object callback) {
  QJsonValue v= tree_to_qjson (data);
  QByteArray a= qjson_to_bytes (v);
  return qt_async_http_post (url, headers_attr, a.constData (), a.size (),
			     callback);
}

// Asynchroneous post of queries
bool
qt_async_http_post (string url, array<string> headers_attr,
		    array<string> attr, object callback) {
  QUrlQuery q;
  for (int i= 0; i+1 < N(attr); i += 2) {
    QString tmp;
    if (ends (attr[i], "@")) {
      string key= attr[i](0, N(attr[i])-1);
      QFile f (utf8_to_qstring (attr[i+1]));
      if (f.open (QIODevice::ReadOnly | QIODevice::Text)) {
	QTextStream in (&f);
	tmp= in.readAll();
	f.close();
      }
      else
	io_error << "qt_async_http_post, cannot open file "
		 << attr[i+1] << LF;
      q.addQueryItem (utf8_to_qstring (key), tmp);
    }
    else
      q.addQueryItem (utf8_to_qstring (attr[i]),
		      utf8_to_qstring (attr[i+1]));
  }
  QByteArray data= q.toString (QUrl::FullyEncoded).toUtf8 ();
  return qt_async_http_post (url, headers_attr,
			     data.constData (), data.size (), callback);
}

#else // QT_VERSION < 0x060000
tree
qt_http_from_json (string s, int mode) {
  (void) s; (void) mode;
  return tree (); }

int
qt_http_post (string& ret, string url, array<string> headers_attr,
	      string data) {
  (void) url; (void) headers_attr; (void) data;
  ret= "";
  return -1; }

int
qt_http_post (string& ret, string url, array<string> headers_attr, tree t) {
  (void) url; (void) headers_attr; (void) t;
  ret= "";
  return -1; }

int
qt_http_post (string& ret, string url, array<string> headers_attr,
	      array<string> attr) {
  (void) url; (void) headers_attr; (void) attr;
  ret= "";
  return -1; }

bool
qt_async_http_post (string url, array<string> headers_attr,
		    string data, object callback) {
  (void) url; (void) headers_attr; (void) data; (void) callback;
  return true; }

bool
qt_async_http_post (string url, array<string> headers_attr,
		    tree data, object callback) {
  (void) url; (void) headers_attr; (void) data; (void) callback;
  return true; }

bool
qt_async_http_post (string url, array<string> headers_attr,
		    array<string> attr, object callback) {
  (void) url; (void) headers_attr; (void) attr; (void) callback;
  return true; }

void
QTMHTTPHandler::onFinished () {}
#endif // QT_VERSION >= 0x060000
