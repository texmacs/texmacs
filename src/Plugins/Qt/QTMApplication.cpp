#include "QTMApplication.hpp"
#include <QTimer>
#include <QFile>
#if QT_VERSION >= 0x060000
#include <QFileSystemWatcher>
#endif
#include "qt_utilities.hpp"

QTMApplication::QTMApplication (int& argc, char** argv) :
  QApplication (argc, argv), mOnscreenKeyboard(nullptr) {
#if QT_VERSION >= 0x060000
    mCssWatcher = new QFileSystemWatcher(this);
    connect(mCssWatcher, &QFileSystemWatcher::fileChanged, 
            this, &QTMApplication::onCssFileChanged);
#endif
}

void QTMApplication::load() {
  mUseTabWindow = get_user_preference ("enable tab") == "on";
#ifdef OS_ANDROID
  mUseTabWindow = true;
#endif

#if QT_VERSION >= 0x060000
  mUseNewToolbar = get_user_preference ("new toolbar") != "off";
#else
  mUseNewToolbar = false;
#endif

#if QT_VERSION >= 0x060000
  mPixmapManagerInitialized = false;
#endif

  init_theme ();

#if QT_VERSION >= 0x050000
  if (mUseTabWindow) new QTMMainTabWindow();
#endif

#ifdef OS_ANDROID
  mOnscreenKeyboard = new QTMOnscreenKeyboard();
  if (mUseTabWindow && QTMMainTabWindow::topTabWindow() != nullptr) {
    QTMMainTabWindow::topTabWindow()->attachOnscreenKeyboard(mOnscreenKeyboard);
  } else {
    mOnscreenKeyboard->show();
  }
#endif

}
  

void QTMApplication::init_theme () {
#if defined(OS_MINGW64) && QT_VERSION >= 0x060000
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
    tm_style_sheet= "$TEXMACS_PATH/misc/themes/standard-light.css";
  else if (theme == "dark")
    tm_style_sheet= "$TEXMACS_PATH/misc/themes/standard-dark.css";

  
  QString qcss_path= utf8_to_qstring (concretize (url_system (tm_style_sheet)));
  mCssWatcher->addPath(qcss_path);
  
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
  (void) var;
#if QT_VERSION >= 0x060000
  if (var == "gui theme") {
    init_theme ();
    emit themeChanged();
  }
#endif
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