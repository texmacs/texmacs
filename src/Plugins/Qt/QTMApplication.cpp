#include "QTMApplication.hpp"

  
QTMApplication::QTMApplication (int& argc, char** argv) :
  QApplication (argc, argv) { }

void QTMApplication::load() {
  #ifdef OS_ANDROID
  mUseTabWindow = true;
#else
  mUseTabWindow = get_user_preference ("enable tab") == "on";
#endif
  mWaitDialog = new QTMWaitDialog ();

#if QT_VERSION >= 0x060000
  mPixmapManagerInitialized = false;
#endif

  init_theme ();

  if (mUseTabWindow) new QTMMainTabWindow();
}
  

void QTMApplication::init_theme () {
#if defined(OS_MINGW64) && QT_VERSION >= 0x060000
  setStyle(QStyleFactory::create("Windows"));
#endif    
  string theme= get_user_preference ("gui theme", "default");
  if (theme == "default") 
    theme = get_default_theme ();
  if (theme == "light")
    tm_style_sheet= "$TEXMACS_PATH/misc/themes/standard-light.css";
  else if (theme == "dark")
    tm_style_sheet= "$TEXMACS_PATH/misc/themes/standard-dark.css";
  else if (theme != "")
    tm_style_sheet= theme;

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

void
texmacs_qt_wait_handler (string message, string arg, int level) {
  tmapp()->waitDialog().setActive(true);
  if (N(message)) {
    if (arg != "") message = message * " " * arg * "...";
    tmapp()->waitDialog().pushMessage (message);
  } else {
    tmapp()->waitDialog().popMessage ();
  }
}

void QTMApplication::installWaitHandler() {
  set_wait_handler (texmacs_qt_wait_handler);
}