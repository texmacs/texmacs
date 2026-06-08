
/******************************************************************************
* MODULE     : QTMIconManager.cpp
* DESCRIPTION: A Qt6 utility class to manage icons
* COPYRIGHT  : (C) 2024 Liza Belos, 2025 Gregoire Lecerf
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QApplication>

#if QT_VERSION >= 0x060000
#include <QIconEngine>
#include <QTimer>

#include "QTMIconManager.hpp"
#include "QTMApplication.hpp"
#include "qt_picture.hpp"
#include "qt_utilities.hpp"

bool may_transform (url file_name, const QImage& pm);

static bool
load_svg (url file_name, QIcon& icon) {
  url sub= QTMIconManager::is_dark_mode () ?
    url ("dark") : url ("light");
  url res= file_name;
  if (!is_rooted (file_name)) {
    res= resolve (url ("$TEXMACS_PIXMAP_PATH") * sub * file_name |
		  url ("$TEXMACS_PIXMAP_PATH") * file_name);
    if (is_none (res)) return false;
  }
  icon= QIcon (to_qstring (concretize (res)));
  if (QTMIconManager::is_dark_mode () &&
      tail (head (res)) != url (sub)) {
    QImage image= icon.pixmap (512).toImage ();
    if (may_transform (file_name, image)) {
      invert_colors (image);
      saturate (image);
      QPixmap pixmap= QPixmap::fromImage (image);
      icon= QIcon (pixmap);
    }
  }
  return !icon.isNull ();
}

static bool
load_pixmap (url file_name, QIcon& icon, double dpr) {
  url sub= QTMIconManager::is_dark_mode () ?
    url ("dark") : url ("light");
  url res= file_name;
  int possible_dpr= ceil (dpr);
  if (!is_rooted (file_name)) {
    string tag= "";
    string suf= suffix (file_name);
    url name= N(suf) == 0 ? file_name : unglue (file_name, N(suf)+1);
    if (possible_dpr == 2 || possible_dpr == 4)
      tag= "_x" * as_string (possible_dpr);
    url name_png= glue (name, tag * ".png");
    url name_xpm= glue (name, tag * ".xpm");
    res= resolve (url ("$TEXMACS_PIXMAP_PATH") * sub * name_png |
		  url ("$TEXMACS_PIXMAP_PATH") * sub * name_xpm |
		  url ("$TEXMACS_PIXMAP_PATH") * name_png |
		  url ("$TEXMACS_PIXMAP_PATH") * name_xpm);
    if (is_none (res)) return false;
  }
  QPixmap pm= QPixmap (to_qstring (concretize (res)));
  pm.setDevicePixelRatio (possible_dpr);
  if (QTMIconManager::is_dark_mode () &&
      tail (head (res)) != url (sub)) {
    QImage image= pm.toImage();
    if (may_transform (file_name, image)) {
      invert_colors (image);
      saturate (image);
      pm= QPixmap::fromImage (image);
    }
  }
  icon= QIcon (pm);
  return !icon.isNull ();
}

static bool
load_pixmap (url file_name, QIcon& icon) {
  return load_pixmap (file_name, icon, 4.0) ||
         load_pixmap (file_name, icon, 2.0) ||
         load_pixmap (file_name, icon, 1.0);
}

class QTMThemeIconEngine : public QIconEngine {
public:
  QTMThemeIconEngine (url file_name) : m_file_name(file_name) {}

  QIcon &getCurrentIcon () {
    if (QTMIconManager::is_dark_mode ()) {
      if (m_dark_icon.isNull ()) m_dark_icon = resolveIcon ();
      return m_dark_icon;
    } else {
      if (m_light_icon.isNull ()) m_light_icon = resolveIcon ();
      return m_light_icon;
    }
  }

  void paint (QPainter *painter, const QRect &rect, QIcon::Mode mode, QIcon::State state) override {
    getCurrentIcon().paint (painter, rect, Qt::AlignCenter, mode, state);
  }

  QPixmap pixmap (const QSize &size, QIcon::Mode mode, QIcon::State state) override {
    return getCurrentIcon().pixmap (size, mode, state);
  }

  QSize actualSize (const QSize &size, QIcon::Mode mode, QIcon::State state) override {
    return getCurrentIcon().actualSize (size, mode, state);
  }

  QIconEngine *clone () const override {
    QTMThemeIconEngine* engine = new QTMThemeIconEngine (m_file_name);
    engine->m_light_icon = m_light_icon;
    engine->m_dark_icon = m_dark_icon;
    return engine;
  }

private:
  url m_file_name;
  QIcon m_light_icon;
  QIcon m_dark_icon;

  QIcon resolveIcon () {
    QIcon icon;
    string suf= suffix (m_file_name);
    url name= N(suf) == 0 ? m_file_name : unglue (m_file_name, N(suf)+1);
    if (load_svg (glue (name, ".svg"), icon) ||
        load_pixmap (m_file_name, icon)) {
      return icon;
    }
    return icon;
  }
};

QIcon
QTMIconManager::getIcon (url file_name) {
  QString qfile_name= to_qstring (as_string (file_name));
  
  if (engine_cache.contains (qfile_name))
    return engine_cache[qfile_name];
    
  QIcon icon (new QTMThemeIconEngine (file_name));
  engine_cache[qfile_name]= icon;
  
  return icon;
}

void QTMIconManager::setLabelPixmap(QPointer<QLabel> label, url file_name, int width, int height) {
  if (get_env ("TEXMACS_PIXMAP_PATH") == "") {
    QTimer::singleShot (1, [this, label, file_name, width, height]() {
      setLabelPixmap(label, file_name, width, height);
    });
  }
  if (label) {
    QIcon icon= this->getIcon (file_name);
    if (width > 0 && height > 0) {
      QPixmap pixmap= icon.pixmap (width, height);
      label->setPixmap (pixmap);
    } else {
      label->setPixmap (icon.pixmap (icon.availableSizes().last()));
    }
  }
}

void QTMIconManager::setPushButtonIcon(QPointer<QPushButton> button, url file_name) {
  if (button) {
    QIcon icon= this->getIcon (file_name);
    button->setIcon (icon);
  }
}

void QTMIconManager::setActionIcon(QPointer<QAction> action, url file_name) {
  if (action) {
    QIcon icon= this->getIcon (file_name);
    action->setIcon (icon);
  }
}

void QTMIconManager::setWindowIcon(QPointer<QMainWindow> window, url file_name) {
  if (get_env ("TEXMACS_PIXMAP_PATH") == "") {
    QTimer::singleShot (1000, [this, window, file_name]() {
      setWindowIcon(window, file_name);
    });
  }
  if (window) {
    QIcon icon= this->getIcon (file_name);
    window->setWindowIcon (icon);
  }
}

void QTMIconManager::setTabIcon(QPointer<QTabWidget> tabwidget, int index, url file_name) {
  if (get_env ("TEXMACS_PIXMAP_PATH") == "") {
    QTimer::singleShot (1, [this, tabwidget, index, file_name]() {
      setTabIcon(tabwidget, index, file_name);
    });
  }
  if (tabwidget) {
    QIcon icon= this->getIcon (file_name);
    tabwidget->setTabIcon (index, icon);
  }
}

void QTMIconManager::setTabIcon(QPointer<QTMResponsiveTabWidget> tabbar, int index, url file_name) {
  if (get_env ("TEXMACS_PIXMAP_PATH") == "") {
    QTimer::singleShot (1, [this, tabbar, index, file_name]() {
      setTabIcon(tabbar, index, file_name);
    });
  }
  if (tabbar) {
    QIcon icon= this->getIcon (file_name);
    tabbar->setTabIcon (index, icon);
  }
}

void QTMIconManager::setToolButtonIcon(QPointer<QToolButton> button, url file_name) {
  if (get_env ("TEXMACS_PIXMAP_PATH") == "") {
    QTimer::singleShot (1, [this, button, file_name]() {
      setToolButtonIcon(button, file_name);
    });
  }
  if (button) {
    QIcon icon= this->getIcon (file_name);
    button->setIcon (icon);
  }
}

#endif // QT_VERSION >= 0x060000
