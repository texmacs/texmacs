#include "QTMApplication.hpp"
#include <QTimer>
#include <QFile>
#if QT_VERSION >= 0x060000
#include <QFileSystemWatcher>
#endif
#include "file.hpp"
#include "qt_utilities.hpp"

QTMApplication::QTMApplication (int& argc, char** argv) :
  QApplication (argc, argv), mOnscreenKeyboard(nullptr), mCssWatcher(nullptr) {
  mCssWatcher = new QFileSystemWatcher(this);
  connect(mCssWatcher, &QFileSystemWatcher::fileChanged, 
          this, &QTMApplication::onCssFileChanged);
}

void QTMApplication::load() {
  mUseTabWindow = true;
  mPixmapManagerInitialized = false;

  init_theme ();

  mOnscreenKeyboard = new QTMOnscreenKeyboard();
  mOnscreenKeyboard->hide();

  if (mUseTabWindow) new QTMMainTabWindow();

}
  

void QTMApplication::init_theme () {
#if defined(OS_MINGW64)
  setStyle(QStyleFactory::create("Windows"));
#endif    
  string theme= get_user_preference ("gui theme", "default");
  if (theme != "default" && theme != "light" && theme != "dark") {
    std_warning << "Invalid GUI theme preference: " << theme << ", falling back to default." << LF;
    theme = "default";
  }
  if (theme == "default") 
    theme = get_default_theme ();
  if (theme == "light")
    tm_style_sheet= "$TEXMACS_PATH/misc/themes/qt6-light.css";
  else if (theme == "dark")
    tm_style_sheet= "$TEXMACS_PATH/misc/themes/qt6-dark.css";

  
  string density= get_user_preference ("gui density", "default");
  if (density == "default")
    density = "normal";
  if (density == "normal")
    tm_style_density= "$TEXMACS_PATH/misc/themes/standard-density-normal.css";
  else if (density == "compact")
    tm_style_density= "$TEXMACS_PATH/misc/themes/standard-density-compact.css";
  else if (density == "large")
    tm_style_density= "$TEXMACS_PATH/misc/themes/standard-density-large.css";

  
  if (mCssWatcher != nullptr) {
    mCssWatcher->removePaths (mCssWatcher->files ());
    QString qcss_path= utf8_to_qstring (concretize (url_system (tm_style_sheet)));
    mCssWatcher->addPath(qcss_path);
    QString qdensity_path= utf8_to_qstring (concretize (url_system (tm_style_density)));
    mCssWatcher->addPath(qdensity_path);
  }
  
  init_palette (this);
  init_style_sheet (this);
}

void QTMApplication::set_window_icon (string icon_path) {
  url icon_url= url_system (get_env ("TEXMACS_PATH") * icon_path);
  if (exists (icon_url)) {
    const c_string _icon (as_string (icon_url));
    setWindowIcon (QIcon ((const char*) _icon));
  }
  else {
    std_warning << "Could not find TeXmacs icon file: " << as_string (icon_url) << LF;
  }
}

bool QTMApplication::notify (QObject* receiver, QEvent* event)
{
  try {
    return QApplication::notify (receiver, event);
  }
  catch (string s) {
    //c_string cs (s);
    //tm_failure (cs);
    //qt_error << "Thrown " << s << LF;
    the_exception= s;
  }
  return false;
}

void QTMApplication::notify_preference (string var) {
  if (var == "gui theme" || var == "gui density") {
    init_theme ();
    emit themeChanged();
  }
}

void qt_notify_preference (string var) {
  QTMApplication* app= static_cast<QTMApplication*> (QApplication::instance ());
  if (app) app->notify_preference (var);
}


void QTMApplication::onCssFileChanged(const QString &path) {
  (void) path;
  init_theme();
  emit themeChanged();
}

void QTMApplication::toggleOnScreenKeyboardVisibility() {
  if (mOnscreenKeyboard) {
    if (mOnscreenKeyboard->isVisible()) {
      mOnscreenKeyboard->hide();
    } else {
      mOnscreenKeyboard->show();
    }
  }
}

/******************************************************************************
* Single instance and messages
******************************************************************************/

QTMLocalServer::QTMLocalServer (): QObject (NULL) {
  server= NULL;
  alive= false;
  lock_file= NULL;
#if defined(Q_OS_MAC)
  return;
#endif
  if (headless_mode) return;
  url f= "$TEXMACS_HOME_PATH/system/tmp/localserver.lock";
  FILE* lock_file= texmacs_fopen (concretize (f), "w", false);
  if (lock_file == NULL) {
    //qCritical () << "QTMLocalServer failed to open lock file";
    return;
  }
  texmacs_lock_file (lock_file, true);
  if (lock_file == NULL) {
    //qCritical () << "QTMLocalServer failed to grab lock file";
    return;
  }
  server= new QLocalServer (this);
  connect (server, &QLocalServer::newConnection, this,
	   &QTMLocalServer::onNewConnection);
  if (!QLocalServer::removeServer ("org.texmacs.local")) {
    //qCritical () << "QTMLocalServer failed to grab org.texmacs.local";
    return;
  }      
  if (!server->listen ("org.texmacs.local")) {
    //qCritical () << "QTMLocalServer failed to start: "
    //             << server->errorString ();
    return;
  }
  //qDebug () << "QTMLocalServer is running...";
  alive= true;
}

QTMLocalServer::~QTMLocalServer () {
  if (server) {
    server->close ();
    server->deleteLater ();
  }
  if (lock_file)
    texmacs_fclose (lock_file);     
}

bool
QTMLocalServer::is_alive () { return alive; }

static bool
message_complete (string s) { // same format as tm_link
  int start= 0;
  int i, n= N(s);
  for (i=start; i<n; i++)
    if (s[i] == '\n') break;
  if (i == n) return false;
  return (n - (i+1)) >= as_int (s (start, i));
}

static string
message_receive (string& s) {
  int start= 0;
  int i, n= N(s);
  for (i=start; i<n; i++)
    if (s[i] == '\n') break;
  if (i == n) return "";
  int l= as_int (s (start, i++));
  string r= s (i, i+l);
  s= s (i+l, n);
  return r;
}

static void
apply_command (string s) {
  // make sure that commands are not arbitrary for security purpose
  object t= string_to_object (s);
  if (is_list (t) && !is_null (t) && !is_null (cdr(t)) &&
      is_symbol (car(t)) && as_symbol (car(t)) == "load-buffer" &&
      is_string (cadr(t)))
    eval (t);
}

void
QTMLocalServer::onNewConnection () {
  QLocalSocket *client= server->nextPendingConnection ();
  if (!client) return;
  // qDebug () << "New local client connected";
  connect (client, &QLocalSocket::readyRead, this,
	   &QTMLocalServer::readClientData);
  connect (client, &QLocalSocket::disconnected, this,
	   &QTMLocalServer::onClientDisconnected);
}

void
QTMLocalServer::readClientData () {
  QLocalSocket* client= qobject_cast<QLocalSocket*> (sender ());
  if (!client) return;
  QByteArray data= client->readAll ();
  if (received.contains (client))
    received[client] += QString::fromUtf8 (data);
  else
    received[client]= QString::fromUtf8 (data);
  // qDebug () << "Local server received: " << received[client];
  string msg= from_qstring_utf8 (received[client]);
  if (message_complete (msg))
    apply_command (message_receive (msg));
}

void
QTMLocalServer::onClientDisconnected () {
  QLocalSocket* client= qobject_cast<QLocalSocket*> (sender ());
  if (!client) return;
  QString cmd;
  if (received.contains (client))
    received.remove (client);
}

bool
send_to_single_instance (string s) {
  QLocalSocket socket;
  socket.connectToServer ("org.texmacs.local");
  if (!socket.waitForConnected (1000)) // 1s
    return false;
  string msg= (as_string (N (s)) * "\n") * s; // same as tm_link
  int failed= 0;
  while (msg != "") {
    if (failed > 20) break; // wait for at most 10s in total
    long long n= socket.write (&(msg[0]), N(msg));
    socket.flush ();
    if (n < 0) break;
    msg= msg (n, N(msg));
    if (n == 0) {
      socket.waitForBytesWritten (500); // ms
      failed++;
    }
  }
  socket.close ();
  return msg == "";
}