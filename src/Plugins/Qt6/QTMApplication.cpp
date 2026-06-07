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
  set_user_preference ("enable tab", "on");
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